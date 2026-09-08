/* Copyright (C) 2025 [Gobierno de Espana]
 * This file is part of "Autofirma".
 * "Autofirma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.KeyStoreUtilities;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

import javax.security.auth.callback.PasswordCallback;
import java.io.InputStream;
import java.util.Map;
import java.util.logging.Level;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox accedidos
 *  v&iacute;a NSS en el que se tratan de forma unificada los m&oacute;dulos internos y externos.
 *  Se a&ntilde;de tambi&eacute;n el almac&eacute;n de claves del sistema operativo que se est&eacute; utilizando */
public class MozillaUnifiedWithOSKeyStoreManager extends MozillaUnifiedKeyStoreManager {

	/** Crea un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox. */
	public MozillaUnifiedWithOSKeyStoreManager() {
		setType(AOKeyStore.MOZ_UNI_WITH_OS);
	}

	@Override
	public void init(final AOKeyStore type,
			               final InputStream store,
			               final PasswordCallback pssCallBack,
			               final Object[] params,
			               final boolean forceReset) {

		LOGGER.info("Inicializamos el almacen de tipo: " + type); //$NON-NLS-1$

		this.passwordCallback = pssCallBack;
		this.configParams = params != null ? params.clone() : null;

		// Vaciamos el listado de almacenes agregados, ya que esta llamada puede realizarse como
		// parte de una operacion de refresco del almacen
		removeAll();

		Object parentComponent = null;

		if (this.configParams != null && this.configParams.length > 0) {
			parentComponent = this.configParams[0];
		}

		// Salvo que se indique que solo se carguen los PKCS#11, primero agregamos el almacen interno de Mozilla
		if (!Boolean.getBoolean(ONLY_PKCS11) && !Boolean.parseBoolean(System.getenv(ONLY_PKCS11_ENV))) {
			LOGGER.info("Agregamos las entradas del almacen NSS de Firefox"); //$NON-NLS-1$
			final AOKeyStoreManager ksm = getNssKeyStoreManager();
			try {
				ksm.init(type, store, this.passwordCallback, this.configParams, forceReset);
			}
			catch(final Exception e) {
				LOGGER.severe(
					"No se ha podido cargar NSS, se continuara con los almacenes externos: " + e //$NON-NLS-1$
				);
			}
			setKeyStore(ksm.getKeyStore());
			addKeyStoreManager(ksm);
		}

		// Agregamos el almacen del SO si aplica
		// Como almacenes secundarios que son, evitamos el reseteo si ya estaban cargados. Esto evita problemas con el
		// PKCS#11 del DNIe
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			LOGGER.info("Agregamos las entradas del almacen de claves de Windows"); //$NON-NLS-1$
			try {
				final AOKeyStoreManager osKeystore = AOKeyStoreManagerFactory.getAOKeyStoreManager(
						AOKeyStore.WINDOWS, null, null, this.passwordCallback, parentComponent, false);
				addKeyStoreManager(osKeystore);
			} catch (final Exception e) {
				LOGGER.severe("No se ha podido agregar el almacen de Windows: " + e); //$NON-NLS-1$
			}
		} else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			LOGGER.info("Agregamos las entradas del almacen de claves del llavero de macOS"); //$NON-NLS-1$
			try {
				final AOKeyStoreManager osKeystore = AOKeyStoreManagerFactory.getAOKeyStoreManager(
						AOKeyStore.APPLE, null, null, null, parentComponent, false);
				addKeyStoreManager(osKeystore);
			} catch (final Exception e) {
				LOGGER.severe("No se ha podido agregar el almacen de MacOS: " + e); //$NON-NLS-1$
			}
		}

		// Intentamos ahora agregar los almacenes de JMulticard. Estos almacenes siempre se agregan con maxima prioridad
		boolean excludeJMulticardKeyStores = false;
		if (forceReset || !this.initialized) {
			try {
				this.smartcardLoadedByJMulticard = KeyStoreUtilities.addJMulticardKeyStoreManagers(this, parentComponent, forceReset);
				setSmartCardAdded(this.smartcardLoadedByJMulticard);
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Se cancelo el uso del driver Java: " + e); //$NON-NLS-1$
				this.smartcardLoadedByJMulticard = false;
				// En caso de haya cancelado expresamente el uso de una tarjeta que se haya encontrado insertada,
				// lo marcamos para que, al cargar el resto de modulos mediante PKCS#11 se omitan aquellos que se
				// controlen desde JMulticard y que el usuario ha indicado que no desea utilizar
				excludeJMulticardKeyStores = true;
			}
		}

		// Si se cargo alguna tarjeta con JMulticard entendemos que se desean usar y no cargamos ningun otro
		// almacen externo. Si no, trataremos de cargar el DNI a traves de su PKCS#11.
		if (!this.smartcardLoadedByJMulticard) {
			try {
				AOKeyStoreManager pkcs11DnieKsm = getDNIePKCS11KeyStoreManager(parentComponent, forceReset);
				addKeyStoreManager(0, pkcs11DnieKsm);
				setSmartCardAdded(true);
				this.dnieLoadedBypkcs11 = true;
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Se cancelo el uso del PKCS#11 del DNIe: " + e); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING,
						"No se ha podido cargar el DNIe a traves de su PKCS#11, se continuara con el resto de almacenes externos", e); //$NON-NLS-1$
			}

			// Si no se ha cargado el DNIe, cargamos los PKCS#11 externos declarados en el propio Firefox
			if (!this.dnieLoadedBypkcs11) {
				final Map<String, String> externalStores = getExternalStores(excludeJMulticardKeyStores);

				if (!externalStores.isEmpty()) {
					final StringBuilder logStr = new StringBuilder(
							"Encontrados los siguientes modulos PKCS#11 externos instalados en Mozilla / Firefox: " //$NON-NLS-1$
					);
					for (final String key : externalStores.keySet()) {
						logStr.append("'"); //$NON-NLS-1$
						logStr.append(externalStores.get(key));
						logStr.append("' "); //$NON-NLS-1$
					}
					LOGGER.info(logStr.toString());
				} else {
					LOGGER.info("No se han encontrado modulos PKCS#11 externos instalados en Firefox"); //$NON-NLS-1$
				}

				for (final String descr : externalStores.keySet()) {
					final AOKeyStoreManager tmpKsm;
					try {
						tmpKsm = initExternalStore(externalStores.get(descr), descr, parentComponent, forceReset);
					}
					catch (final AOCancelledOperationException ex) {
						LOGGER.warning(
								"Se cancelo el acceso al almacen externo  '" + descr + "', se continuara con el siguiente: " + ex //$NON-NLS-1$ //$NON-NLS-2$
						);
						continue;
					}
					catch (final Exception ex) {
						LOGGER.warning("No se ha podido inicializar el PKCS#11 '" + descr + "': " + ex); //$NON-NLS-1$ //$NON-NLS-2$
						continue;
					}
					// Agregamos el almacen con maxima prioridad para que, dado un mismo certificado, se utilice el de
					// tarjeta en lugar del certificado software. Esto no afecta al uso de JMulticard, ya que si se ha
					// cargado un almacen de este tipo, no se cargan los PKCS#11 externos
					addKeyStoreManager(0, tmpKsm);

					LOGGER.info(
							"El almacen externo '" + descr + "' ha podido inicializarse, se anadiran sus entradas y se detiene la carga del resto de almacenes" //$NON-NLS-1$ //$NON-NLS-2$
					);
					break;
				}
			}
		}

		if (lacksKeyStores()) {
			LOGGER.warning(
				"No se ha podido inicializar ningun almacen, interno o externo, de Mozilla, ni los almacenes preferentes" //$NON-NLS-1$
			);
		}

		setType(type);

		this.initialized = true;
	}

	@Override
	public void refresh() {
		if (getParentComponent() != null) {
			if (this.passwordCallback != null && this.passwordCallback instanceof UIPasswordCallback) {
				((UIPasswordCallback) this.passwordCallback).setParent(getParentComponent());
			}
			if (this.configParams == null || this.configParams.length == 0) {
				this.configParams = new Object[1];
			}
			this.configParams[0] = getParentComponent();
		}
		init(AOKeyStore.MOZ_UNI_WITH_OS, null, this.passwordCallback, this.configParams, true);
	}

}
