/* Copyright (C) 2025 [Gobierno de Espana]
 * This file is part of "Autofirma".
 * "Autofirma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.KeyStoreUtilities;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox accedidos
 *  v&iacute;a NSS en el que se tratan de forma unificada los m&oacute;dulos internos y externos.
 *  Se a&ntilde;de tambi&eacute;n el almac&eacute;n de claves del sistema operativo que se est&eacute; utilizando */
public class MozillaUnifiedWithOSKeyStoreManager extends MozillaUnifiedKeyStoreManager {

	/** Crea un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox. */
	public MozillaUnifiedWithOSKeyStoreManager() {
		setKeyStoreType(AOKeyStore.MOZ_UNI_WITH_OS);
	}

	/** Inicializa la clase gestora de almacenes de claves. */
	@Override
	public void init(final AOKeyStore type,
			               final InputStream store,
			               final PasswordCallback pssCallBack,
			               final Object[] params,
			               final boolean forceReset) {

		LOGGER.info("Inicializamos el almacen de tipo: " + type); //$NON-NLS-1$

		this.passwordCallback = pssCallBack;
		this.configParams = params != null ? params.clone() : null;

		// Vaciamos el listado de almacenes agregados ya que esta llamada puede realizarse como
		// parte de una operacion de refresco del almacen
		removeAll();

		Object parentComponent = null;

		if (this.configParams != null && this.configParams.length > 0) {
			parentComponent = this.configParams[0];
		}

		// Agregamos el almacen interno de Mozilla
		if (!Boolean.getBoolean(ONLY_PKCS11) && !Boolean.parseBoolean(System.getenv(ONLY_PKCS11_ENV))) {
			// Primero anadimos el almacen principal NSS
			final AOKeyStoreManager ksm = getNssKeyStoreManager();
			try {
				ksm.init(type, store, pssCallBack, this.configParams, forceReset);
			}
			catch(final Exception e) {
				LOGGER.severe(
					"No se ha podido cargar NSS, se continuara con los almacenes externos: " + e //$NON-NLS-1$
				);
			}
			setKeyStore(ksm.getKeyStore());
			addKeyStoreManager(ksm);
		}

		//Agregamos el almacen del SO si aplica
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			LOGGER.info("Agregamos las entradas del almacen de claves de Windows"); //$NON-NLS-1$
			try {
				final AOKeyStoreManager osKeystore = AOKeyStoreManagerFactory.getWindowsMyCapiKeyStoreManager(true);
				addKeyStoreManager(osKeystore);
			} catch (final Exception e) {
				LOGGER.severe("No se ha podido agregar el almacen de Windows: " + e); //$NON-NLS-1$
			}
		} else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			LOGGER.info("Agregamos las entradas del almacen de claves del llavero de macOS"); //$NON-NLS-1$
			try {
				final AOKeyStoreManager osKeystore = AOKeyStoreManagerFactory.getMacOSXKeyStoreManager(
						AOKeyStore.APPLE, null, true, parentComponent);
				addKeyStoreManager(osKeystore);
			} catch (final Exception e) {
				LOGGER.severe("No se ha podido agregar el almacen de MacOS: " + e); //$NON-NLS-1$
			}
		}

		// Intentamos ahora agregar los almacenes externos preferentes ajenos a los
		// dispositivos de seguridad configurados en Firefox haciendo uso del controlador Java
		boolean excludePreferredKeyStores = false;
		if (forceReset || !this.initialized) {
			try {
				this.preferredKsAdded = KeyStoreUtilities.addPreferredKeyStoreManagers(this, parentComponent);
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Se cancelo el uso del driver Java: " + e); //$NON-NLS-1$
				// En caso de haber detectado una tarjeta preferida pero haberse cancelado su uso,
				// permitiremos utilizar el resto de modulos a excepcion de los PKCS#11 que tambien
				// controlen las tarjetas preferidas, ya que se supone que no se desean utilizar
				this.preferredKsAdded = false;
				excludePreferredKeyStores = true;
			}
		}

		// Si se pudo agregar algun almacen preferente entendemos que se desean usar y no cargamos los
		// configurados en Firefox. Si no, iniciamos los almacenes externos. DNIe nunca se cargara como
		// almacen externo. En el caso de las tarjetas CERES, si no se pudo cargar a traves del
		// controlador JAVA, se intentara cargar a traves del PKCS#11 si estaba configurado en Firefox
		if (!this.preferredKsAdded) {
			final Map<String, String> externalStores = getExternalStores(excludePreferredKeyStores);

			if (externalStores.size() > 0) {
				final StringBuilder logStr = new StringBuilder(
					"Encontrados los siguientes modulos PKCS#11 externos instalados en Mozilla / Firefox: " //$NON-NLS-1$
				);
				for (final String key : externalStores.keySet()) {
					logStr.append("'"); //$NON-NLS-1$
					logStr.append(externalStores.get(key));
					logStr.append("' "); //$NON-NLS-1$
				}
				LOGGER.info(logStr.toString());
			}
			else {
				LOGGER.info("No se han encontrado modulos PKCS#11 externos instalados en Firefox"); //$NON-NLS-1$
			}

			for (final String descr : externalStores.keySet()) {
				final AOKeyStoreManager tmpKsm = new AOKeyStoreManager();
				try {
					internalInitStore(tmpKsm, descr, parentComponent, forceReset, externalStores.get(descr));
				}
				catch (final AOCancelledOperationException ex) {
					LOGGER.warning(
						"Se cancelo el acceso al almacen externo  '" + descr + "', se continuara con el siguiente: " + ex //$NON-NLS-1$ //$NON-NLS-2$
					);
					continue;
				}
				catch (final Exception ex) {
					// En ciertos sistemas Linux fallan las inicializaciones la primera vez por culpa de PC/SC, reintentamos
					if (!Platform.OS.LINUX.equals(Platform.getOS())) {
						LOGGER.warning("No se ha podido inicializar el PKCS#11 '" + descr + "': " + ex); //$NON-NLS-1$ //$NON-NLS-2$
						continue;
					}
					try {
						internalInitStore(tmpKsm, descr, parentComponent, forceReset, externalStores.get(descr));
					}
					catch (final AOCancelledOperationException exc) {
						LOGGER.warning("Se cancelo el acceso al almacen externo  '" + descr + "', se continuara con el siguiente: " + exc); //$NON-NLS-1$ //$NON-NLS-2$
						continue;
					}
					catch(final Exception e) {
						LOGGER.warning(
							"No se ha podido inicializar el PKCS#11 '" + descr + "' tras haberlo intentado dos veces: " + ex + ", " + e //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
						);
						continue;
					}
				}
				addKeyStoreManager(tmpKsm);

				LOGGER.info(
					"El almacen externo '" + descr + "' ha podido inicializarse, se anadiran sus entradas y se detiene la carga del resto de almacenes" //$NON-NLS-1$ //$NON-NLS-2$
				);
				break;
			}
		}


		if (lacksKeyStores()) {
			LOGGER.warning(
				"No se ha podido inicializar ningun almacen, interno o externo, de Mozilla, ni los almacenes preferentes" //$NON-NLS-1$
			);
		}

		setKeyStoreType(type);

		this.initialized = true;
	}

	@Override
	public void refresh() throws IOException {
		init(AOKeyStore.MOZ_UNI_WITH_OS, null, this.passwordCallback, this.configParams, true);
	}

}
