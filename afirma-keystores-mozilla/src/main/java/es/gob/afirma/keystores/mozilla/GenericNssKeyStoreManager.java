/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

import javax.security.auth.callback.PasswordCallback;
import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.security.KeyStore;
import java.security.Provider;
import java.security.Security;
import java.util.logging.Level;

/**
 * Almac&eacute;n de claves NSS gen&eacute;rico al que se le debe indicar la ruta
 * del almac&oacute;n.
 */
public final class GenericNssKeyStoreManager extends AOKeyStoreManager {

	private Provider nssProvider = null;

	private String nssKeystorePath;

	/** Componente padre sobre el que montar los di&aacute;logos modales. */
	private Object parentComponent = null;

	/**
	 * Construye un gestor de almac&eacute;n de claves y certificados NSS.
	 * @param ksType Tipo de almac&eacute;n de claves.
	 * @param keystoreDir Directorio del almac&eacute;n de claves NSS.
	 */
	public GenericNssKeyStoreManager(AOKeyStore ksType, final File keystoreDir) {
		setType(ksType);
		if (keystoreDir == null) {
			throw new IllegalArgumentException("El directorio del almacén NSS no puede ser nulo"); //$NON-NLS-1$
		}
		this.nssKeystorePath = keystoreDir.getAbsolutePath();
	}

	/**
	 * Construye un gestor de almac&eacute;n de claves y certificados NSS.
	 * @param ksType Tipo de almac&eacute;n de claves.
	 * @param nssKeystorePath Ruta absoluta del directorio del almac&eacute;n de claves NSS.
	 */
	public GenericNssKeyStoreManager(AOKeyStore ksType, final String nssKeystorePath) {
		setType(ksType);
		if (nssKeystorePath == null) {
			throw new IllegalArgumentException("El directorio del almacén NSS no puede ser nulo"); //$NON-NLS-1$
		}
		this.nssKeystorePath = nssKeystorePath;
	}

	@Override
	public void init(final AOKeyStore type,
			         final InputStream store,
			         final PasswordCallback pssCallBack,
			         final Object[] params,
			         final boolean forceReset) {

		if (params != null && params.length > 0 && params[0] instanceof Component) {
			this.parentComponent = (Component) params[0];
		}

		// Se ha detectado que en algunas versiones de Java/OpenJDK, al solicitar un proveedor
		// de seguridad para comprobar su existencia, puede afectar negativamente a que este proveedor
		// se cargue en un futuro, asi que guardamos una copia local del proveedor para hacer estas comprobaciones
		// getNssProvider() hace toda la inicializacion de NSS como PKCS#11 especial en Java
		final Provider p = getNssProvider(type, forceReset);

		KeyStore keyStore = null;
		if (p != null) {
			try {
				keyStore = KeyStore.getInstance("PKCS11", p); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido obtener el KeyStore PKCS#11 NSS del proveedor SunPKCS11: " + e); //$NON-NLS-1$
			}
		}

		if (keyStore != null) {
			try {
				keyStore.load(null, new char[0]);
			}
			catch (final Exception e) {
				LOGGER.info(
					"No se ha podido abrir el almacen sin contrasena, se intentara proporcionando una: " + e //$NON-NLS-1$
				);
				try {
					keyStore.load(null, pssCallBack != null
						? pssCallBack.getPassword()
							: new UIPasswordCallback(FirefoxKeyStoreMessages.getString("MozillaUnifiedKeyStoreManager.0"), //$NON-NLS-1$
								this.parentComponent).getPassword());
				}
				catch (final AOCancelledOperationException e1) {
					throw e1;
				}
				catch (final Exception e2) {
					LOGGER.log(Level.WARNING,"No se ha podido abrir el almacen NSS", e2); //$NON-NLS-1$
					keyStore = null;
				}
			}
		}

		if (keyStore != null) {
			setKeyStore(keyStore);
		}

	}

	/**
     * Carga e instala el proveedor de seguridad para el acceso al almac&eacute;n de NSS. Despu&eacute;s de cargarlo, se
     * devuelve una copia del mismo para que pueda ser usado sin afectar al proveedor original.
     * @param ksType Tipo de almac&eacute;n de claves para darle prioridad al uso de sus bibliotecas (opcional).
	 * @param forceReset Si se indica <code>true</code> no se reutiliza el proveedor y siempre se crea uno
	 *                   nuevo. Si se indica <code>false</code> y ya hab&iacute; un proveedor cargado, lo recupera
	 *                   directamente, reutiliz&aacute;ndolo.
	 * @return Copia del proveedor para el acceso a NSS.
     */
	private Provider getNssProvider(final AOKeyStore ksType, final boolean forceReset) {
		if (nssProvider != null) {
			if (forceReset) {
				Security.removeProvider(nssProvider.getName());
			}
			else {
				return (Provider) nssProvider.clone();
			}
		}
		try {
			nssProvider = loadNSS(ksType);
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error obteniendo el proveedor NSS: " + e, e); //$NON-NLS-1$
			nssProvider = null;
		}
		return nssProvider != null ? (Provider) nssProvider.clone() : null;
	}

	private Provider loadNSS(final AOKeyStore ksType) throws IOException,
			AOException,
			InstantiationException,
			IllegalAccessException,
			IllegalArgumentException,
			InvocationTargetException,
			NoSuchMethodException,
			SecurityException,
			ClassNotFoundException {

		String ksName = cleanName(getType().getName());

		final String nssDirectory = MozillaKeyStoreUtilities.getSystemNSSLibDir(ksType);

		LOGGER.info("Directorio de bibliotecas NSS: " + nssDirectory); //$NON-NLS-1$

		LOGGER.info("Ruta del almacen NSS: " + LoggerUtil.getCleanUserHomePath(this.nssKeystorePath)); //$NON-NLS-1$

		return MozillaKeyStoreUtilities.loadNSSProvider(ksName, nssDirectory, this.nssKeystorePath);
	}

	/**
	 * Limpia un nombre de almac&eacute;n para que pueda ser usado como nombre de proveedor de seguridad.
	 * @param name Nombre del almac&eacute;n.
	 * @return Nombre del almac&eacute;n limpio.
	 */
	private static String cleanName(String name) {
		return name.replaceAll("[^a-zA-Z0-9]", "_"); //$NON-NLS-1$ //$NON-NLS-2$
	}
}
