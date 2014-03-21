/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.InputStream;
import java.security.KeyStore;
import java.security.Provider;
import java.util.Map;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox accedidos
 *  v&iacute;a NSS en el que se tratan de forma unificada los m&oacute;dulos internos y externos. */
public final class MozillaUnifiedKeyStoreManager extends AOKeyStoreManager {

	private static Provider nssProvider = null;

	/** Componente padre sobre el que montar los di&aacute;logos modales. */
	private Object parentComponent = null;

	/** PasswordCallback establecido de forma externa para el acceso al
	 * almac&eacute;n. */
	private PasswordCallback externallPasswordCallback = null;

	/** Construye un gestor unificado de almac&eacute;n de claves y certificados Mozilla (NSS). */
	public MozillaUnifiedKeyStoreManager() {
		setKeyStoreType(AOKeyStore.MOZ_UNI);
	}

	/** Inicializa la clase gestora de almacenes de claves.
	 * @throws AOKeyStoreManagerException
	 *         Si no puede inicializarse ning&uacute;n almac&eacute;n de
	 *         claves, ni el NSS interno, ni ning&uacute;n PKCS#11 externo
	 *         definido en SecMod */
	@Override
	public void init(final AOKeyStore type, final InputStream store, final PasswordCallback pssCallBack, final Object[] params) throws AOKeyStoreManagerException {

		// Se ha detectado que en algunas versiones de Java/OpenJDK, al solicitar un proveedor
		// de seguridad comprobar su existencia, puede afectar negativamente a que este proveedor
		// se cargue en un futuro, asi que guardamos una copia local del proveedor para hacer
		// estas comprobaciones
		final Provider p = getNssProvider();

		KeyStore keyStore = null;

		if (p != null) {
			try {
				keyStore = KeyStore.getInstance("PKCS11", p); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido obtener el KeyStore PKCS#11 NSS del proveedor SunPKCS11, se continuara con los almacenes externos: " + e); //$NON-NLS-1$
				keyStore = null;
			}
		}
		if (keyStore != null) {
			try {
				keyStore.load(null, new char[0]);
			}
			catch (final Exception e) {
				try {
					keyStore.load(null, this.externallPasswordCallback != null
						? this.externallPasswordCallback.getPassword()
							: new UIPasswordCallback(FirefoxKeyStoreMessages.getString("MozillaUnifiedKeyStoreManager.0"), //$NON-NLS-1$
								this.parentComponent).getPassword());
				}
				catch (final AOCancelledOperationException e1) {
					keyStore = null;
					throw e1;
				}
				catch (final Exception e2) {
					LOGGER.warning("No se ha podido abrir el almacen PKCS#11 NSS del proveedor SunPKCS11, se continuara con los almacenes externos: " + e2); //$NON-NLS-1$
					keyStore = null;
				}
			}

		}

		if (keyStore != null) {
			addKeyStore(keyStore);
		}

		// Vamos ahora con los almacenes externos, que se limpian antes de usarse quitando DNIe (porque se usa
		// el controlador Java) y anadiendo modulos conocidos si se encuentran en el sistema.
		final Map<String, String> externalStores = ExternalStoresHelper.cleanExternalStores(
			MozillaKeyStoreUtilities.getMozillaPKCS11Modules()
		);

		if (externalStores.size() > 0) {
			final StringBuilder logStr = new StringBuilder("Encontrados los siguientes modulos PKCS#11 externos instalados en Mozilla / Firefox: "); //$NON-NLS-1$
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

		KeyStore tmpStore;
		for (final String descr : externalStores.keySet()) {
			try {
				final AOKeyStoreManager tmpKsm = new AOKeyStoreManager();
				tmpKsm.init(
					AOKeyStore.PKCS11,
					null,
					new UIPasswordCallback(
						FirefoxKeyStoreMessages.getString("MozillaUnifiedKeyStoreManager.1") + " " + MozillaKeyStoreUtilities.getMozModuleName(descr.toString()), //$NON-NLS-1$ //$NON-NLS-2$
						this.parentComponent
					),
					new String[] {
						externalStores.get(descr), descr.toString()
					}
				);
				addKeyStores(tmpKsm.getKeyStores());
			}
			catch (final AOCancelledOperationException ex) {
				LOGGER.warning("Se cancelo el acceso al almacen externo  '" + descr + "', se continuara con el siguiente: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}
			catch (final Exception ex) {
				LOGGER.severe("No se ha podido inicializar el PKCS#11 '" + descr + "': " + ex); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}

			LOGGER.info("El almacen externo '" + descr + "' ha podido inicializarse, se anadiran sus entradas"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// Anadimos el controlador Java del DNIe **SIEMPRE**
		try {
			tmpStore = AOKeyStoreManagerFactory.getAOKeyStoreManager(
				AOKeyStore.DNIEJAVA,
				null,
				null,
				null,
				this.parentComponent
			).getKeyStores().get(0);
			LOGGER.info("El DNIe 100% Java ha podido inicializarse, se anadiran sus entradas"); //$NON-NLS-1$
			addKeyStore(tmpStore);
		}
		catch (final AOCancelledOperationException ex) {
			LOGGER.warning("Se cancelo el acceso al almacen DNIe 100% Java: " + ex); //$NON-NLS-1$
		}
		catch (final Exception ex) {
			LOGGER.warning("No se ha podido inicializar el controlador DNIe 100% Java: " + ex); //$NON-NLS-1$
		}

		if (lacksKeyStores()) {
			throw new AOKeyStoreManagerException("No se ha podido inicializar ningun almacen, interno o externo, de Firefox"); //$NON-NLS-1$
		}

	}

	/** Establece la interfaz de entrada de la contrase&ntilde;a del
	 * almac&eacute;n interno de Firefox. Si no se indica o se establece a <code>null</code> se utilizar&aacute; el por defecto.
	 * @param externallPC Interfaz de entrada de contrase&ntilde;a. */
	public void setPasswordCallback(final PasswordCallback externallPC) {
		this.externallPasswordCallback = externallPC;
	}

	/** Establece el componente padre sobre el que mostrar los di&aacute;logos
	 * modales para la inserci&oacute;n de contrase&ntilde;as.
	 * @param parent
	 *        Componente padre. */
	public void setParentComponent(final Object parent) {
		this.parentComponent = parent;
	}

	/** Carga e instala el proveedor de seguridad para el acceso al almac&eacute;n de NSS. Si
	 * ya estaba cargado, lo recupera directamente.
	 * @return Proveedor para el acceso a NSS. */
	private static Provider getNssProvider() {

		if (nssProvider != null) {
			return nssProvider;
		}

		try {
			nssProvider = MozillaKeyStoreUtilities.loadNSS();
		}
		catch (final Exception e) {
			LOGGER.severe("Error inicializando el proveedor NSS: " + e); //$NON-NLS-1$
			nssProvider = null;
		}

		return nssProvider;
	}
}
