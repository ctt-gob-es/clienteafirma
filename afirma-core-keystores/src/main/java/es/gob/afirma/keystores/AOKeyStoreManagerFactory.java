/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.jmulticard.ui.DialogBuilder;

import javax.security.auth.callback.PasswordCallback;
import java.awt.*;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Obtiene clases de tipo AOKeyStoreManager seg&uacute;n se necesiten,
 * proporcionando adem&aacute;s ciertos m&eacute;todos de utilidad. Contiene
 * fragmentos de las clases <code>com.sun.deploy.config.UnixConfig</code> y <code>com.sun.deploy.config.WinConfig</code>
 * @version 0.4
 */
public final class AOKeyStoreManagerFactory {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final static Map<String, AOKeyStoreManager> cachedKeystores = new HashMap<>();

	private AOKeyStoreManagerFactory() {
        // No permitimos la instanciacion
    }

    /** Variable de entorno de ejecuci&acute;n que si se establece a <code>true</code> indica que
     * nunca debe reutilizarse un almac&eacute;n de claves ya creado. Si no se establece o se
     * establece a <code>false</code>, se reutilizan las instancias existentes siempre que sea
     * posible, por eficiencia. */
    public static final String FORCE_STORE_RESET = "es.gob.afirma.keystores.ForceReset"; //$NON-NLS-1$

    /** Obtiene el <code>KeyStoreManager</code> del tipo indicado.
     * @param store Almac&eacute;n de claves
     * @param lib Biblioteca del KeyStore (solo para KeyStoreManager de tipo PKCS#11) o fichero de almac&eacute;n de claves (para
     *            PKCS#12, Java KeyStore, JCE KeyStore, X.509, llavero de Mac OS X [opcional] y PKCS#7)
     * @param description Descripci&oacute;n del KeyStoreManager que se desea obtener,
     *                    necesario para obtener el n&uacute;mero de z&oacute;calo de los modulos PKCS#11 obtenidos del Secmod de Mozilla / Firefox.
     *                    Debe seguir el formato definido en el m&eacute;todo <code>toString()</code> de la clase <code>sun.security.pkcs11.Secmod.Module</code>
     * @param pssCallback <i>Callback</i> que solicita la contrase&ntilde;a del repositorio que deseamos recuperar.
     * @param parentComponent Componente padre sobre el que mostrar los di&aacute;logos (normalmente un <code>java.awt.Comonent</code>)
     *                        modales de ser necesario.
     * @return <code>KeyStoreManager</code> del tipo indicado
     * @throws AOCancelledOperationException Cuando el usuario cancela el proceso (por ejemplo, al introducir la contrase&ntilde;a)
     * @throws KeystoreAlternativeException Cuando ocurre cualquier otro problema durante el proceso
     * @throws IOException Cuando la contrase&ntilde;a del almac&eacute;n es incorrecta.
     * @throws es.gob.afirma.core.InvalidOSException Cuando se pide un almac&eacute;n &uacute;nicamente disponible para
     *                                               un sistema operativo distinto del actual
     * @throws es.gob.afirma.core.MissingLibraryException Cuando no se localice una biblioteca necesaria para el
     *                                                    uso del almac&eacute;n. */
	public static AOKeyStoreManager getAOKeyStoreManager(final AOKeyStore store,
                                                         final String lib,
                                                         final String description,
                                                         final PasswordCallback pssCallback,
                                                         final Object parentComponent,
														 boolean forceReset)
			throws KeystoreAlternativeException, IOException {

		// Se usa try-catch para capturar errores de permisos de lectura de variables
		try {
			boolean forceResetConfigBySystem = Boolean.getBoolean(FORCE_STORE_RESET);
			if (forceResetConfigBySystem) {
				forceReset = true;
			}
		}
		catch (final Exception e) {
			LOGGER.warning(
					"No se ha podido leer la variable '" + FORCE_STORE_RESET + "', no se forzara expresamente: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		// Cargamos el almacen correspondiente
		switch (store) {

			// Almacen PKCS#12, en cualquier sistema operativo
			case PKCS12:
				return getPkcs12KeyStoreManager(lib, pssCallback, forceReset, parentComponent);

			// Almacen JKS, en cualquier sistema operativo
			case JAVA:
				return getJavaKeyStoreManager(lib, pssCallback, forceReset, parentComponent);

			// Fichero P7, X509, JCEKS o CaseExactKS en cualquier sistema operativo
			case SINGLE:
			case JAVACE:
			case JCEKS:
				return getFileKeyStoreManager(store, lib, pssCallback, forceReset, parentComponent);

			// Token PKCS#11, en cualquier sistema operativo
			case PKCS11:
				return getPkcs11KeyStoreManager(lib, description, pssCallback, forceReset, parentComponent);

			case PKCS11_DNIE:
				return getPkcs11DnieKeyStoreManager(forceReset, parentComponent);

			// Almacen de certificados de Windows
			case WINDOWS:
				return getWindowsMyCapiKeyStoreManager(forceReset);

			// Almacen de certificados de Windows unificado
			case WINDOWS_UNI:
				return getWindowsMyCapiUnifiedKeyStoreManager(forceReset);

			// Libreta de direcciones de Windows
			case WINADDRESSBOOK:
			case WINCA:
				return getWindowsAddressBookKeyStoreManager(store, forceReset);

			// Almacen de Mozilla que muestra tanto los certificados del almacen los de
			// los dispositivos externos configuramos.
			case MOZ_UNI:
				return getMozillaUnifiedKeyStoreManager(pssCallback, forceReset, parentComponent);

			// Almacen de Mozilla que muestra tanto los certificados del almacen, los del almacen del SO
			// y los dispositivos externos configuramos.
			case MOZ_UNI_WITH_OS:
				return getMozillaUnifiedWithOSKeyStoreManager(pssCallback, forceReset, parentComponent);

			// Almacen de tipo Mozilla (almacen NSS junto con modulos PKCS#11) con los certificados de Chrome, Chromium o Brave.
			case NSS_CHROME:
			case NSS_CHROMIUM:
			case NSS_BRAVE:
				return getMozillaLikeStoreManager(store, pssCallback, forceReset, parentComponent);

			// Almacen NSS compartido (de sistema) que muestra tanto los certificados del almacen
			// como los de los dispositivos externos configuramos.
			case SHARED_NSS:
				return getSharedNssKeyStoreManager(pssCallback, forceReset, parentComponent);

			// Apple Safari sobre Mac OS X.
			case APPLE:
				return getMacOSXKeyStoreManager(store, lib, forceReset, parentComponent);

			// Modulo de JMulticard
			case DNIEJAVA:
			case CERES_430:
			case CERES:
			case SMARTCAFE:
				return loadJMulticardKeystoreManager(store, pssCallback, forceReset, parentComponent);

			default:
				throw new KeystoreAlternativeException(
						getAlternateKeyStoreType(store),
						"La plataforma de navegador '"  //$NON-NLS-1$
								+ store.getName()
								+ "' mas sistema operativo '" //$NON-NLS-1$
								+ Platform.getOS()
								+ "' no esta soportada", //$NON-NLS-1$
						KeyStoreErrorCode.Request.UNSUPPORTED_KEYSTORE
				);
		}
	}

	private static String selectKeyStoreFile(final AOKeyStore ksType, final String[] exts,
	                                                        final String desc,
	                                                        final String lib,
	                                                        final Object parentComponent) {

		String keystorePath = null;
        if (lib != null && !lib.isEmpty() && new File(lib).exists()) {
            keystorePath = lib;
        }
		else {
            keystorePath = AOUIFactory.getLoadFiles(
        		KeyStoreMessages.getString("AOKeyStoreManagerFactory.4", ksType.getName()), //$NON-NLS-1$ //$NON-NLS-2$
        		null,
        		null,
        		exts,
        		desc,
        		false,
        		false,
        		null,
        		parentComponent
    		)[0].getAbsolutePath();
            if (keystorePath == null) {
                throw new AOCancelledOperationException("No se ha seleccionado el almacen de certificados"); //$NON-NLS-1$
            }
        }

		return keystorePath;
    }

	private static AOKeyStoreManager getPkcs12KeyStoreManager(final String lib,
	                                                          final PasswordCallback pssCallback,
	                                                          final boolean forceReset,
	                                                          final Object parentComponent)
			throws IOException, KeystoreAlternativeException {

		// Seleccionamos el fichero PKCS#12 que se quiera cargar
		String libPath = selectKeyStoreFile(
				AOKeyStore.PKCS12,
				new String[]{
						"pfx", "p12" //$NON-NLS-1$ //$NON-NLS-2$
				},
				KeyStoreMessages.getString("AOKeyStoreManagerFactory.0"), //$NON-NLS-1$
				lib,
				parentComponent
		);

		// Comprobamos si ya estaba en cache el almacen de ese fichero y lo devolvemos si es asi
		Pkcs12KeyStoreManager ksm = (Pkcs12KeyStoreManager) loadFromCache(AOKeyStore.PKCS12, libPath, forceReset);
		if (ksm != null) {
			return ksm;
		}

		// Si el almacen no estaba en cache, lo cargamos
		ksm = new Pkcs12KeyStoreManager();
		ksm.setKeyStoreFile(libPath);
		try (InputStream ksInputStream = new FileInputStream(libPath)) {
			ksm.init(AOKeyStore.PKCS12, ksInputStream, pssCallback, null, forceReset);
		}
		catch (final AOException e) {
			throw new KeystoreAlternativeException(
					AOKeyStore.PKCS12,
					"No se ha podido abrir el almacen de tipo PKCS#12 con el fichero " + lib, //$NON-NLS-1$
					e,
					KeyStoreErrorCode.Internal.LOADING_PKCS12_KEYSTORE_ERROR
			);
		}

		// Guardamos el KeyStoreManager en cache para que no se vuelva a cargar
		saveInCache(AOKeyStore.PKCS12, libPath, ksm);

		return ksm;
	}

    private static AOKeyStoreManager getJavaKeyStoreManager(final String lib,
    														final PasswordCallback pssCallback,
    														final boolean forceReset,
    														final Object parentComponent) throws IOException,
    															                                 KeystoreAlternativeException {
		// Seleccionamos el fichero JKS que se quiera cargar
		String libPath = selectKeyStoreFile(
				AOKeyStore.JAVA,
				new String[]{
						"jks" //$NON-NLS-1$ //$NON-NLS-2$
				},
				KeyStoreMessages.getString("AOKeyStoreManagerFactory.1"), //$NON-NLS-1$
				lib,
				parentComponent
		);

		// Comprobamos si ya estaba en cache el almacen de ese fichero y lo devolvemos si es asi
		JavaKeyStoreManager ksm = (JavaKeyStoreManager) loadFromCache(AOKeyStore.JAVA, libPath, forceReset);
		if (ksm != null) {
			return ksm;
		}

		// Si el almacen no estaba en cache, lo cargamos
		ksm = new JavaKeyStoreManager();
		ksm.setKeyStoreFile(libPath);
		try (InputStream ksInputStream = new FileInputStream(libPath)) {
			ksm.init(AOKeyStore.JAVA, ksInputStream, pssCallback, null, forceReset);
		}
		catch (final AOException e) {
			throw new KeystoreAlternativeException(
					AOKeyStore.JAVA,
					"No se ha podido abrir el almacen de tipo Java con el fichero " + lib, //$NON-NLS-1$
					e,
					KeyStoreErrorCode.Internal.LOADING_JAVA_KEYSTORE_ERROR
			);
		}

		// Guardamos el KeyStoreManager en cache para que no se vuelva a cargar
		saveInCache(AOKeyStore.JAVA, libPath, ksm);

		return ksm;
    }

	private static AOKeyStoreManager loadJMulticardKeystoreManager(
			AOKeyStore store,
			final PasswordCallback pssCallback,
			final boolean forceReset,
			final Object parentComponent)
			throws KeystoreAlternativeException, IOException {

		// Comprobamos si ya estaba en cache el almacen de ese fichero y lo devolvemos si es asi
		AOKeyStoreManager ksm = loadFromCache(store, forceReset);
		if (ksm != null) {
			return ksm;
		}

		// Cargamos el almacen a traves de JMulticard
		ksm = new AOKeyStoreManager();
		try {
			ksm.init(store, null, pssCallback, new Object[] { parentComponent }, forceReset);
		}
		catch (final AOKeyStoreManagerException e) {
			throw new KeystoreAlternativeException(
					AOKeyStore.PKCS12,
					"Error al inicializar JMulticard para la carga de " + store.getName(), //$NON-NLS-1$
					e,
					KeyStoreErrorCode.Internal.LOADING_JMULTICARD_KEYSTORE_ERROR
			);
		}

		// Guardamos el KeyStoreManager en cache para que no se vuelva a cargar
		saveInCache(store, ksm);

		return ksm;
	}

	/**
	 * Carga un certificado o almacen de confianza como si fuese un alamacen de claves.
	 * @param store Tipo de almac&eacute;n de claves a cargar.
	 * @param lib Fichero del almac&eacute;n.
	 * @param pssCallback <i>Callback</i> que solicita la contrase&ntilde;a del almac&eacute;n.
	 * @param forceReset Indica si se debe forzar al reinicio del almac&eacute;n si ya estaba iniciado.
	 * @param parentComponent Componente padre sobre el que mostrar los di&aacute;logos modales de ser necesario.
	 * @return <code>AOKeyStoreManager</code> del tipo indicado.
	 * @throws IOException Si hay problemas en la lectura de datos.
	 * @throws KeystoreAlternativeException Indica un tipo alternativo de almac&eacute;n si no es posible cargar este.
	 */
	private static AOKeyStoreManager getFileKeyStoreManager(final AOKeyStore store,
	                                                        final String lib,
	                                                        final PasswordCallback pssCallback,
	                                                        final boolean forceReset,
	                                                        final Object parentComponent) throws IOException,
			KeystoreAlternativeException {

		String desc = null;
		String[] exts = null;
		if (store == AOKeyStore.SINGLE) {
			exts = new String[]{
					"cer", "p7b" //$NON-NLS-1$ //$NON-NLS-2$
			};
			desc = KeyStoreMessages.getString("AOKeyStoreManagerFactory.2"); //$NON-NLS-1$
		} else if (store == AOKeyStore.JCEKS || store == AOKeyStore.JAVACE) {
			exts = new String[]{
					"jceks", "jks", "jce" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			};
			desc = KeyStoreMessages.getString("AOKeyStoreManagerFactory.3"); //$NON-NLS-1$
		}

		// Seleccionamos el fichero de almacen que se quiera cargar
		String libPath = selectKeyStoreFile(store, exts, desc, lib, parentComponent);

		// Comprobamos si ya estaba en cache el almacen de ese fichero y lo devolvemos si es asi
		AOKeyStoreManager ksm = loadFromCache(store, libPath, forceReset);
		if (ksm != null) {
			return ksm;
		}

		// Si el almacen no estaba en cache, lo cargamos
		ksm = new AOKeyStoreManager();
		try (
				final InputStream is = new FileInputStream(libPath);
		) {
			ksm.init(store, is, pssCallback, null, forceReset);
		}
		catch (final AOException e) {
			throw new KeystoreAlternativeException(
					getAlternateKeyStoreType(store),
					"No se ha podido abrir el almacen de tipo " + store.getName(), //$NON-NLS-1$
					e,
					KeyStoreErrorCode.Internal.LOADING_FILE_CERTSTORE_ERROR
			);
		}

		// Guardamos el KeyStoreManager en cache para que no se vuelva a cargar
		saveInCache(store, libPath, ksm);

		return ksm;
	}

    private static AOKeyStoreManager getPkcs11KeyStoreManager(final String lib,
                                                              final String description,
                                                              final PasswordCallback pssCallback,
                                                              final boolean forceReset,
                                                              final Object parentComponent) throws IOException,
                                                                                                   KeystoreAlternativeException {
        String p11Lib = null;
        if (lib != null && !lib.isEmpty()) {
            p11Lib = lib;
        }
        if (p11Lib != null && !new File(p11Lib).isFile()) {
        	throw new IOException("La biblioteca '" + p11Lib + "' no existe"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        if (p11Lib == null) {
            final String[] exts;
            String extsDesc = KeyStoreMessages.getString("AOKeyStoreManagerFactory.6"); //$NON-NLS-1$
            if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
                exts = new String[] { "dll" }; //$NON-NLS-1$
                extsDesc = extsDesc + " (*.dll)"; //$NON-NLS-1$
            }
            else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
                exts = new String[] { "so", "dylib" }; //$NON-NLS-1$ //$NON-NLS-2$
                extsDesc = extsDesc + " (*.dylib, *.so)"; //$NON-NLS-1$
            }
            else {
                exts = new String[] { "so" }; //$NON-NLS-1$
                extsDesc = extsDesc + " (*.so)"; //$NON-NLS-1$
            }
            p11Lib = AOUIFactory.getLoadFiles(
	             KeyStoreMessages.getString("AOKeyStoreManagerFactory.7"),  //$NON-NLS-1$
	             null,
	             null,
	             exts,
	             extsDesc,
	             false,
	             false,
	             null,
	             parentComponent
            )[0].getAbsolutePath();
        }
        if (p11Lib == null) {
            throw new AOCancelledOperationException("No se ha seleccionado el controlador PKCS#11"); //$NON-NLS-1$
        }

		// Tratamos de recuperar el KeyStoreManager de la cache y lo devolvemos si estaba cargado
		AOKeyStoreManager ksm = loadFromCache(AOKeyStore.PKCS11, p11Lib, forceReset);
		if (ksm != null) {
			return ksm;
		}

		// Si no estaba cargado, lo cargamos
		ksm = new Pkcs11KeyStoreManager();
        try {
            ksm.init(
        		AOKeyStore.PKCS11,
        		null,
        		pssCallback,
        		new String[] {
                    p11Lib, description
        		},
        		forceReset
    		);
        }
		catch (AOCancelledOperationException e) {
			throw e;
		}
        catch (final Exception e) {

			// En ciertos sistemas Linux fallan las inicializaciones la primera vez por culpa de PC/SC, asi que, si no
			// es Linux, lanzamos la excepcion, y si es Linux, reintentamos
			if (Platform.OS.LINUX.equals(Platform.getOS())) {
				LOGGER.warning("Ha fallado el primer intento de inicializacion del modulo PKCS#11, se reintentara: " + e); //$NON-NLS-1$
				try {
					ksm.init(
							AOKeyStore.PKCS11,
							null,
							pssCallback,
							new String[] {
									p11Lib, description
							},
							forceReset
					);
				}
				catch (Exception e1) {
					LOGGER.warning("Ha fallado el segundo intento de inicializacion del modulo PKCS#11: " + e); //$NON-NLS-1$
				}
			}

            throw new KeystoreAlternativeException(
                 getAlternateKeyStoreType(AOKeyStore.PKCS11),
                 "Error al inicializar el modulo PKCS#11: " + e, //$NON-NLS-1$
                 e,
                 KeyStoreErrorCode.Internal.LOADING_PKCS11_KEYSTORE_ERROR
            );
        }

		// Guardamos el KeyStoreManager en cache para que no se vuelva a cargar
		saveInCache(AOKeyStore.PKCS11, p11Lib, ksm);

        return ksm;
    }

	private static AOKeyStoreManager getPkcs11DnieKeyStoreManager(
			final boolean forceReset, final Object parent) throws IOException {

		AOKeyStoreManager ksm = loadFromCache(AOKeyStore.PKCS11_DNIE, forceReset);
		if (ksm != null) {
			return ksm;
		}

		ksm = new DNIePKCS11KeyStoreManager();
		ksm.setParentComponent(parent);

		Component parentComponent = parent instanceof Component ? (Component) parent : null;
		PasswordCallback pc = DialogBuilder.getDefaultDniePasswordCallback(parentComponent);
		try {
			ksm.init(AOKeyStore.PKCS11_DNIE, null, pc, null, forceReset);
		}
		catch (final AOException e) {
			throw new IOException("Error al inicializar el almacen del DNIe mediante su PKCS#11", e); //$NON-NLS-1$
		}

		// Guardamos el KeyStoreManager en cache para que no se vuelva a cargar
		saveInCache(AOKeyStore.PKCS11_DNIE, ksm);

		return ksm;
	}

    private static AOKeyStoreManager getWindowsAddressBookKeyStoreManager(final AOKeyStore store,
    		                                                              final boolean forceReset) throws IOException,
                                                                                                  KeystoreAlternativeException {

		 if (!Platform.OS.WINDOWS.equals(Platform.getOS())) {
			 throw new KeystoreAlternativeException(
					 getAlternateKeyStoreType(store),
					 "El almacen " + store.getName() + " solo esta disponible en sistemas operativos Windows", //$NON-NLS-1$ //$NON-NLS-2$
					 KeyStoreErrorCode.Request.UNSUPPORTED_KEYSTORE
			 );
		 }

		// Tratamos de recuperar el KeyStoreManager de la cache y lo devolvemos si estaba cargado
		AOKeyStoreManager ksm = loadFromCache(store, forceReset);
		if (ksm != null) {
			return ksm;
		}

    	ksm = new AOKeyStoreManager();
        try {
            ksm.init(store, null, NullPasswordCallback.getInstance(), null, forceReset);
        }
        catch (final AOException e) {
            throw new KeystoreAlternativeException(
                 getAlternateKeyStoreType(store),
                 "Error al inicializar el almacen " + store.getName(), //$NON-NLS-1$
                 e,
                 KeyStoreErrorCode.Internal.LOADING_PUBLIC_WINDOWS_KEYSTORE_ERROR
            );
        }

		// Guardamos en cache el KeyStoreManager para que no se vuelva a cargar
		saveInCache(store, ksm);

        return ksm;
    }

    public static AOKeyStoreManager getWindowsMyCapiKeyStoreManager(final boolean forceReset)
			throws KeystoreAlternativeException, IOException {

		if (Platform.getOS() != Platform.OS.WINDOWS) {
			throw new KeystoreAlternativeException(
					getAlternateKeyStoreType(AOKeyStore.WINDOWS),
					"El almacen WINDOWS solo esta disponible en sistemas operativos Windows", //$NON-NLS-1$
					KeyStoreErrorCode.Request.UNSUPPORTED_KEYSTORE
			);
		}

		// Tratamos de recuperar el KeyStoreManager de la cache y lo devolvemos si estaba cargado
		AOKeyStoreManager ksm = loadFromCache(AOKeyStore.WINDOWS, forceReset);
		if (ksm != null) {
			return ksm;
		}

		// Cargamos el KeyStoreManager de Windows
		ksm = new CAPIKeyStoreManager();
		try {
			ksm.init(AOKeyStore.WINDOWS, null, null, null, forceReset);
		}
		catch (final AOKeyStoreManagerException | IOException e) {
			throw new KeystoreAlternativeException(
                 getAlternateKeyStoreType(AOKeyStore.WINDOWS),
                 "Error al obtener almacen WINDOWS: " + e, //$NON-NLS-1$
                 e,
                 KeyStoreErrorCode.Internal.LOADING_WINDOWS_KEYSTORE_ERROR
             );
		}

		// Guardamos en cache el KeyStoreManager para que no se vuelva a cargar
		saveInCache(AOKeyStore.WINDOWS, ksm);

		return ksm;
    }

	public static AOKeyStoreManager getWindowsMyCapiUnifiedKeyStoreManager(final boolean forceReset) throws KeystoreAlternativeException,
			IOException {

		if (Platform.getOS() != Platform.OS.WINDOWS) {
			throw new KeystoreAlternativeException(
					getAlternateKeyStoreType(AOKeyStore.WINDOWS_UNI),
					"El almacen WINDOWS Unificado solo esta disponible en sistemas operativos Windows", //$NON-NLS-1$
					KeyStoreErrorCode.Request.UNSUPPORTED_KEYSTORE
			);
		}

		// Tratamos de recuperar el KeyStoreManager de la cache y lo devolvemos si estaba cargado
		AOKeyStoreManager ksm = loadFromCache(AOKeyStore.WINDOWS_UNI, forceReset);
		if (ksm != null) {
			return ksm;
		}

		// Cargamos el KeyStoreManager de Windows unificado
		ksm = new CAPIUnifiedKeyStoreManager();
		try {
			ksm.init(AOKeyStore.WINDOWS_UNI, null, null, null, forceReset);
		}
		catch (final AOKeyStoreManagerException e) {
			throw new KeystoreAlternativeException(
					getAlternateKeyStoreType(AOKeyStore.WINDOWS),
					"Error al obtener almacen WINDOWS: " + e, //$NON-NLS-1$
					e,
					KeyStoreErrorCode.Internal.LOADING_WINDOWS_KEYSTORE_ERROR
			);
		}

		// Guardamos en cache el KeyStoreManager para que no se vuelva a cargar
		saveInCache(AOKeyStore.WINDOWS_UNI, ksm);

		return ksm;
	}

	private static AOKeyStoreManager getSharedNssKeyStoreManager(final PasswordCallback pssCallback,
	                                                             final boolean forceReset,
	                                                             final Object parentComponent)
			throws KeystoreAlternativeException, IOException {

		if (!Platform.OS.LINUX.equals(Platform.getOS())) {
			throw new KeystoreAlternativeException(
					getAlternateKeyStoreType(AOKeyStore.SHARED_NSS),
					"El almacen " + AOKeyStore.SHARED_NSS.getName() + " solo esta disponible en sistemas operativos Linux", //$NON-NLS-1$ //$NON-NLS-2$
					KeyStoreErrorCode.Request.UNSUPPORTED_KEYSTORE
			);
		}

		return getNssKeyStoreManager(
				AOKeyStore.SHARED_NSS,
				"es.gob.afirma.keystores.mozilla.shared.SharedNssKeyStoreManager",  //$NON-NLS-1$
				pssCallback,
				forceReset,
				parentComponent
		);
	}

    private static AOKeyStoreManager getMozillaUnifiedKeyStoreManager(final PasswordCallback pssCallback,
    		                                                                  final boolean forceReset,
                                                                              final Object parentComponent)
			throws KeystoreAlternativeException, IOException {

		return getNssKeyStoreManager(
				AOKeyStore.MOZ_UNI,
				"es.gob.afirma.keystores.mozilla.MozillaUnifiedKeyStoreManager",  //$NON-NLS-1$
				pssCallback,
				forceReset,
				parentComponent
		);
    }

	private static AOKeyStoreManager getMozillaUnifiedWithOSKeyStoreManager(final PasswordCallback pssCallback,
	                                                                        final boolean forceReset,
	                                                                        final Object parentComponent)
			throws KeystoreAlternativeException, IOException {

		return getNssKeyStoreManager(
				AOKeyStore.MOZ_UNI_WITH_OS,
				"es.gob.afirma.keystores.mozilla.MozillaUnifiedWithOSKeyStoreManager",  //$NON-NLS-1$
				pssCallback,
				forceReset,
				parentComponent
		);
	}


	public static AOKeyStoreManager getNssKeyStoreManager(final AOKeyStore store,
	                                                       final String KsmClassName,
	                                                       final PasswordCallback pssCallback,
	                                                       final boolean forceReset,
	                                                       final Object parentComponent)
			throws KeystoreAlternativeException, IOException {

		// Tratamos de recuperar el KeyStoreManager de la cache y lo devolvemos si estaba cargado
		AOKeyStoreManager ksm = loadFromCache(store, forceReset);
		if (ksm != null) {
			return ksm;
		}

		try {
			ksm = (AOKeyStoreManager) Class.forName(KsmClassName).getConstructor().newInstance();
		}
		catch(final Exception e) {
			throw new KeystoreAlternativeException(
					getAlternateKeyStoreType(store),
					"Error al obtener dinamicamente el almacen NSS: " + e, //$NON-NLS-1$
					e,
					KeyStoreErrorCode.Internal.LOADING_MOZILLA_KEYSTORE_ERROR
			);
		}
		try {
			// Proporcionamos el componente padre como parametro
			ksm.init(store, null, pssCallback, new Object[] { parentComponent }, forceReset);
		}
		catch (final AOException e) {
			throw new KeystoreAlternativeException(
					getAlternateKeyStoreType(store),
					"Error al inicializar el almacen NSS: " + e, //$NON-NLS-1$
					e,
					KeyStoreErrorCode.Internal.LOADING_MOZILLA_KEYSTORE_ERROR
			);
		}

		// Guardamos en cache el KeyStoreManager para que no se vuelva a cargar
		saveInCache(store, ksm);

		return ksm;
	}

    private static AOKeyStoreManager getMacOSXKeyStoreManager(final AOKeyStore store,
    		                                                          final String lib,
    		                                                          final boolean forceReset,
    		                                                          final Object parentComponent)
			throws IOException, KeystoreAlternativeException {

		if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
			throw new KeystoreAlternativeException(
					getAlternateKeyStoreType(store),
					"El almacen " + store.getName() + " solo esta disponible en sistemas operativos macOS", //$NON-NLS-1$ //$NON-NLS-2$
					KeyStoreErrorCode.Request.UNSUPPORTED_KEYSTORE
			);
		}

		// Tratamos de recuperar el KeyStoreManager de la cache y lo devolvemos si estaba cargado
		AOKeyStoreManager ksm = loadFromCache(store, forceReset);
		if (ksm != null) {
			return ksm;
		}

		// Cargamos el KeyStoreManager de macOS
    	ksm = new AppleKeyStoreManager();
        // En macOS podemos inicializar un KeyChain en un fichero particular o el por defecto del sistema
        try (
    		final InputStream is = lib == null || lib.isEmpty() ? null : new FileInputStream(lib);
		) {
            ksm.init(
                 store,
                 is,
        		 NullPasswordCallback.getInstance(),
                 null,
                 forceReset
            );
        }
        catch (final AOException e) {
            throw new KeystoreAlternativeException(
        		getAlternateKeyStoreType(store), "Error al inicializar el Llavero de Mac OS X", //$NON-NLS-1$
        		e,
                KeyStoreErrorCode.Internal.LOADING_APPLE_KEYSTORE_ERROR
    		);
        }
        final AggregatedKeyStoreManager aksm = new AggregatedKeyStoreManager(ksm);
        try {
    		KeyStoreUtilities.addJMulticardKeyStoreManagers(aksm, parentComponent, forceReset);
    	}
    	catch (final AOCancelledOperationException e) {
    		LOGGER.info("Se cancelo el uso del driver Java: " + e); //$NON-NLS-1$
    	}

		// Guardamos en cache el KeyStoreManager para que no se vuelva a cargar
		saveInCache(store, ksm);

        return aksm;
    }

	private static AOKeyStoreManager getMozillaLikeStoreManager(final AOKeyStore store,
	                                                               final PasswordCallback pssCallback,
	                                                               final boolean forceReset,
	                                                               final Object parentComponent)
            throws KeystoreAlternativeException, IOException {

		if (!Platform.OS.LINUX.equals(Platform.getOS())) {
			throw new KeystoreAlternativeException(
					getAlternateKeyStoreType(store),
					"El almacen " + store.getName() + " solo esta disponible en sistemas operativos Linux", //$NON-NLS-1$ //$NON-NLS-2$
					KeyStoreErrorCode.Request.UNSUPPORTED_KEYSTORE
			);
		}


		// Tratamos de recuperar el KeyStoreManager de la cache y lo devolvemos si estaba cargado
		AOKeyStoreManager ksm = loadFromCache(store, forceReset);
		if (ksm != null) {
			return ksm;
		}

		// Cargamos el KeyStoreManager de NSS
		try {
			Constructor<?> constructor = Class
					.forName("es.gob.afirma.keystores.mozilla.MozillaLikeKeyStoreManager")
					.getConstructor(AOKeyStore.class);
			ksm = (AOKeyStoreManager) constructor.newInstance(store);
		}
		catch(final Exception e) {
			throw new KeystoreAlternativeException(
					getAlternateKeyStoreType(store),
					"Error al obtener dinamicamente el almacen NSS de tipo " + store, //$NON-NLS-1$
					e,
					KeyStoreErrorCode.Internal.LOADING_MOZILLA_KEYSTORE_ERROR
			);
		}
		try {
			// Proporcionamos el componente padre como parametro
			ksm.init(store, null, pssCallback, new Object[] { parentComponent }, forceReset);
		}
		catch (final Exception e) {
			throw new KeystoreAlternativeException(
					getAlternateKeyStoreType(store),
					"Error al inicializar el almacen NSS: " + e, //$NON-NLS-1$
					e,
					KeyStoreErrorCode.Internal.LOADING_MOZILLA_KEYSTORE_ERROR
			);
		}

		// Guardamos en cache el KeyStoreManager para que no se vuelva a cargar
		saveInCache(store, ksm);

		return ksm;
	}

    /** Devuelve el almac&eacute;n de claves alternativo al actual m&aacute;s apropiado para usar
     * cuando falla la carga de este &uacute;ltimo.
     * @param currentStore Almac&eacute;n de claves actual
     * @return <code>AOKeyStore</code> alternativo o <code>null</code> si no hay alternativo */
    private static AOKeyStore getAlternateKeyStoreType(final AOKeyStore currentStore) {
        if (AOKeyStore.PKCS12.equals(currentStore)) {
            return null;
        }
        if (Platform.OS.WINDOWS.equals(Platform.getOS()) && !AOKeyStore.WINDOWS.equals(currentStore)) {
            return AOKeyStore.WINDOWS;
        }
		if (Platform.OS.LINUX.equals(Platform.getOS()) && !AOKeyStore.SHARED_NSS.equals(currentStore)) {
			return AOKeyStore.SHARED_NSS;
		}
        if (Platform.OS.MACOSX.equals(Platform.getOS()) && !AOKeyStore.APPLE.equals(currentStore)) {
            return AOKeyStore.APPLE;
        }
        return AOKeyStore.PKCS12;
    }

	private static AOKeyStoreManager loadFromCache(AOKeyStore store, boolean forceReset) throws IOException {
		return loadFromCache(store, null, forceReset);
	}

	private static AOKeyStoreManager loadFromCache(AOKeyStore store, String lib, boolean forceReset) throws IOException {
		String cacheName = composeCacheName(store, lib);
		if (cachedKeystores.containsKey(cacheName)) {
			final AOKeyStoreManager ksm = cachedKeystores.get(cacheName);
			if (forceReset) {
				ksm.refresh();
			}
			return ksm;
		}
		return null;
	}

	private static void saveInCache(AOKeyStore store, AOKeyStoreManager ksm) {
		saveInCache(store, null, ksm);
	}

	private static void saveInCache(AOKeyStore store, String lib, AOKeyStoreManager ksm) {
		String cacheName = composeCacheName(store, lib);
		 cachedKeystores.put(cacheName, ksm);
	}

	/**
	 * Compone un nombre de cach&eacute; para el almac&eacute;n de claves indicado y la biblioteca indicada.
	 * @param store Tipo de almac&eacute;n de claves.
	 * @param lib Biblioteca o direcci&oacute;n del almac&eacute;n (solo cuando el tipo de almac&eacute;n lo requiere).
	 * @return Nombre de cache&eacute;.
	 */
	private static String composeCacheName(AOKeyStore store, String lib) {
		return store.getName() + (lib != null ? ":" + lib : ""); //$NON-NLS-1$ //$NON-NLS-2$
	}
}