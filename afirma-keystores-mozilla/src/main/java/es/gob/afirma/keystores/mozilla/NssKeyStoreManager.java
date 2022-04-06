/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.InputStream;
import java.security.KeyStore;
import java.security.Provider;
import java.security.Security;
import java.util.logging.Level;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

/** Almac&eacute;n de claves y certificados basado en NSS.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class NssKeyStoreManager extends AOKeyStoreManager {

	private static Provider nssProvider = null;

	private final boolean useSharedNss;

	/** Componente padre sobre el que montar los di&aacute;logos modales. */
	private final Object parentComponent;

	/** Construye un gestor de almac&eacute;n de claves y certificados NSS.
	 * @param parent Elemento padre para la modalidad.
	 * @param sharedNss Si se indica <code>true</code> se usa el directorio de NSS compartido (de sistema), si por
	 *                  el contrario se indica <code>false</code> se usa el NSS espec&iacute;fico de Mozilla. */
	public NssKeyStoreManager(final Object parent, final boolean sharedNss) {
		setKeyStoreType(AOKeyStore.MOZ_UNI);
		this.parentComponent = parent;
		this.useSharedNss = sharedNss;
	}

	/** Inicializa la clase gestora de almacenes de claves. */
	@Override
	public void init(final AOKeyStore type,
			         final InputStream store,
			         final PasswordCallback pssCallBack,
			         final Object[] params,
			         final boolean forceReset) {

		// Se ha detectado que en algunas versiones de Java/OpenJDK, al solicitar un proveedor
		// de seguridad comprobar su existencia, puede afectar negativamente a que este proveedor
		// se cargue en un futuro, asi que guardamos una copia local del proveedor para hacer
		// estas comprobaciones
		// getNssProvider() hace toda la inicializacion de NSS como PKCS#11 especial en Java
		final Provider p = getNssProvider(this.useSharedNss, forceReset);

		KeyStore keyStore = null;
		if (p != null) {
			try {
				keyStore = KeyStore.getInstance("PKCS11", p); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido obtener el KeyStore PKCS#11 NSS del proveedor SunPKCS11: " + e); //$NON-NLS-1$
				keyStore = null;
			}
		}

		if (keyStore != null) {
			try {
				keyStore.load(null, new char[0]);
			}
			catch (final Exception e) {
				LOGGER.info(
					"No se ha podido abrir el almacen sin contrasena, se intentara proporcionando una : " + e //$NON-NLS-1$
				);
				try {
					keyStore.load(null, pssCallBack != null
						? pssCallBack.getPassword()
							: new UIPasswordCallback(FirefoxKeyStoreMessages.getString("MozillaUnifiedKeyStoreManager.0"), //$NON-NLS-1$
								this.parentComponent).getPassword());
				}
				catch (final AOCancelledOperationException e1) {
					keyStore = null;
					throw e1;
				}
				catch (final Exception e2) {
					LOGGER.warning(
						"No se ha podido abrir el almacen PKCS#11 NSS del proveedor SunPKCS11: " + e2 //$NON-NLS-1$
					);
					keyStore = null;
				}
			}
		}

		if (keyStore != null) {
			setKeyStore(keyStore);
		}

	}

	/** Carga e instala el proveedor de seguridad para el acceso al almac&eacute;n de NSS.
	 * @param useSharedNss Si se indica <code>true</code> se usa el directorio de NSS compartido (de sistema), si por
	 *                     el contrario se indica <code>false</code> se usa el NSS espec&iacute;fico de Mozilla.
	 * @param forceReset Si se indica <code>true</code> no se reutiliza el proveedor y siempre se crea uno
	 *                   nuevo. Si se indica <code>false</code> y ya hab&iacute; un proveedor cargado, lo recupera
	 *                   directamente, reutiliz&aacute;ndolo.
	 * @return Proveedor para el acceso a NSS. */
	private static Provider getNssProvider(final boolean useSharedNss, final boolean forceReset) {
		if (nssProvider != null) {
			if (forceReset) {
				Security.removeProvider(nssProvider.getName());
			}
			else {
				return (Provider) nssProvider.clone();
			}
		}
		try {
			nssProvider = MozillaKeyStoreUtilities.loadNSS(useSharedNss);
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error obteniendo el proveedor NSS: " + e, e); //$NON-NLS-1$
			nssProvider = null;
		}
		return nssProvider != null ? (Provider) nssProvider.clone() : null;
	}
}
