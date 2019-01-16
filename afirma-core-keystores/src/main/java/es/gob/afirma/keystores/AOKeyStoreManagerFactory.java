/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;

/** Obtiene clases de tipo AOKeyStoreManager seg&uacute;n se necesiten,
 * proporcionando adem&aacute;s ciertos m&eacute;todos de utilidad. Contiene
 * fragmentos de las clases <code>com.sun.deploy.config.UnixConfig</code> y <code>com.sun.deploy.config.WinConfig</code>
 * @version 0.3 */
public final class AOKeyStoreManagerFactory {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

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
     * @throws AOKeystoreAlternativeException Cuando ocurre cualquier otro problema durante el proceso
     * @throws IOException Cuando la contrase&ntilde;a del almac&eacute;n es incorrecta.
     * @throws es.gob.afirma.core.InvalidOSException Cuando se pide un almac&eacute;n &uacute;nicamente disponible para
     *                                               un sistema operativo distinto del actual
     * @throws es.gob.afirma.core.MissingLibraryException Cuando no se localice una biblioteca necesaria para el
     *                                                    uso del almac&eacute;n. */
    public static AggregatedKeyStoreManager getAOKeyStoreManager(final AOKeyStore store,
                                                                 final String lib,
                                                                 final String description,
                                                                 final PasswordCallback pssCallback,
                                                                 final Object parentComponent) throws AOKeystoreAlternativeException,
                                                                                                      IOException {
    	boolean forceReset;
    	// Se usa try-catch para capturar errores de permisos de lectura de variables
    	try {
    		forceReset = Boolean.getBoolean(FORCE_STORE_RESET);
    	}
    	catch(final Exception e) {
    		forceReset = false;
    		LOGGER.warning(
				"No se ha podido leer la variable '" + FORCE_STORE_RESET + "', se usara 'false' como valor por defecto: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
    	}
    	if (forceReset) {
    		LOGGER.info(
				"No se mantendran los almacenes de claves precargados (es.gob.afirma.keystores.ForceReset=true)" //$NON-NLS-1$
			);
    	}

    	// Almacen PKCS#12, en cualquier sistema operativo
    	if (AOKeyStore.PKCS12.equals(store)) {
    		return new AggregatedKeyStoreManager(getPkcs12KeyStoreManager(lib, pssCallback, forceReset, parentComponent));
    	}

    	// Almacen JKS, en cualquier sistema operativo
    	if (AOKeyStore.JAVA.equals(store)) {
    		return new AggregatedKeyStoreManager(getJavaKeyStoreManager(lib, pssCallback, forceReset, parentComponent));
        }

    	// Fichero P7, X509, JCEKS o CaseExactKS en cualquier sistema operativo
        if (AOKeyStore.SINGLE.equals(store) ||
    		AOKeyStore.JAVACE.equals(store) ||
    		AOKeyStore.JCEKS.equals(store)) {
        		return new AggregatedKeyStoreManager(getFileKeyStoreManager(store, lib, pssCallback, forceReset, parentComponent));
        }

        // Token PKCS#11, en cualquier sistema operativo
        if (AOKeyStore.PKCS11.equals(store)) {
        	final AOKeyStoreManager ksm = getPkcs11KeyStoreManager(
				lib,
				description,
				pssCallback,
				forceReset,
				parentComponent
			);
        	final AggregatedKeyStoreManager aksm = new AggregatedKeyStoreManager(
    			ksm
			);
        	aksm.setKeyStore(
    			ksm.getKeyStore()
			);
        	return aksm;
        }

        // Almacen de certificados de Windows
        if (Platform.getOS().equals(Platform.OS.WINDOWS) && AOKeyStore.WINDOWS.equals(store)) {
        	return new AggregatedKeyStoreManager(getWindowsMyCapiKeyStoreManager(forceReset));
        }

        // Libreta de direcciones de Windows
        if (Platform.getOS().equals(Platform.OS.WINDOWS) && (AOKeyStore.WINADDRESSBOOK.equals(store) || AOKeyStore.WINCA.equals(store))) {
        	return new AggregatedKeyStoreManager(getWindowsAddressBookKeyStoreManager(store, forceReset));
        }

        // Almacen de Mozilla que muestra tanto los certificados del almacemo los de
        // los dispositivos externos configuramos.
        if (AOKeyStore.MOZ_UNI.equals(store)) {
        	return getMozillaUnifiedKeyStoreManager(pssCallback, forceReset, parentComponent);
        }

        // Almacen NSS compartido (de sistema) que muestra tanto los certificados del almacen
        // como los de los dispositivos externos configuramos.
        if (Platform.getOS().equals(Platform.OS.LINUX) && AOKeyStore.SHARED_NSS.equals(store)) {
        	return getSharedNssKeyStoreManager(pssCallback, forceReset, parentComponent);
        }

        // Apple Safari sobre Mac OS X.
        if (Platform.getOS().equals(Platform.OS.MACOSX) && AOKeyStore.APPLE.equals(store)) {
        	return getMacOSXKeyStoreManager(store, lib, forceReset, parentComponent);
        }

        // Driver Java para DNIe
        if (AOKeyStore.DNIEJAVA.equals(store)) {
        	return new AggregatedKeyStoreManager(getDnieJavaKeyStoreManager(pssCallback, forceReset, parentComponent));
        }

        // Driver Java para tarjetas CERES 4.30 y superiores
        if (AOKeyStore.CERES_430.equals(store)) {
        	return new AggregatedKeyStoreManager(getCeres430JavaKeyStoreManager(pssCallback, forceReset, parentComponent));
        }

    	// Driver Java para CERES
        if (AOKeyStore.CERES.equals(store)) {
        	return new AggregatedKeyStoreManager(getCeresJavaKeyStoreManager(pssCallback, forceReset, parentComponent));
        }

        if (AOKeyStore.SMARTCAFE.equals(store)) {
        	return new AggregatedKeyStoreManager(getSmartCafeJavaKeyStoreManager(pssCallback, forceReset, parentComponent));
        }

        throw new AOKeystoreAlternativeException(
             getAlternateKeyStoreType(store),
             "La plataforma de navegador '"  //$NON-NLS-1$
               + store.getName()
               + "' mas sistema operativo '" //$NON-NLS-1$
               + Platform.getOS()
               + "' no esta soportada" //$NON-NLS-1$
        );
    }

    private static AOKeyStoreManager addFileKeyStoreManager(final AOKeyStoreManager ksm,
    		                                                final String[] exts,
    		                                                final String desc,
    		                                                final String lib,
  		                                                    final PasswordCallback pssCallback,
  		                                                    final boolean forceReset,
  		                                                    final Object parentComponent) throws AOKeystoreAlternativeException, IOException {
        String storeFilename = null;
        if (lib != null && !lib.isEmpty() && new File(lib).exists()) {
            storeFilename = lib;
        }
        if (storeFilename == null) {
            storeFilename = AOUIFactory.getLoadFiles(
        		KeyStoreMessages.getString("AOKeyStoreManagerFactory.4") + " " + ksm.getType().getName(), //$NON-NLS-1$ //$NON-NLS-2$
        		null,
        		null,
        		exts,
        		desc,
        		false,
        		false,
        		null,
        		parentComponent
    		)[0].getAbsolutePath();
            if (storeFilename == null) {
                throw new AOCancelledOperationException("No se ha seleccionado el almacen de certificados"); //$NON-NLS-1$
            }
        }

        try (
    		final InputStream is = new FileInputStream(storeFilename);
		) {
            ksm.init(null, is, pssCallback, null, forceReset);
        }
        catch (final AOException e) {
            throw new AOKeystoreAlternativeException(
        	   AOKeyStore.JAVA,
               "No se ha podido abrir el almacen de tipo " + ksm.getType().getName() + " para el fichero " + lib, //$NON-NLS-1$ //$NON-NLS-2$
               e
            );
        }
        return ksm;
    }

    private static AOKeyStoreManager getPkcs12KeyStoreManager(final String lib,
    		                                                  final PasswordCallback pssCallback,
    		                                                  final boolean forceReset,
    		                                                  final Object parentComponent) throws IOException,
    		                                                  						               AOKeystoreAlternativeException {
    	final FileKeyStoreManager p12Ksm = new Pkcs12KeyStoreManager();
    	p12Ksm.setKeyStoreFile(lib);

    	return addFileKeyStoreManager(
    			p12Ksm,
			new String[] {
                "pfx", "p12" //$NON-NLS-1$ //$NON-NLS-2$
            },
            KeyStoreMessages.getString("AOKeyStoreManagerFactory.0"), //$NON-NLS-1$
			lib,
			pssCallback,
			forceReset,
			parentComponent
		);
	}

    private static AOKeyStoreManager getJavaKeyStoreManager(final String lib,
    														final PasswordCallback pssCallback,
    														final boolean forceReset,
    														final Object parentComponent) throws IOException,
    															                                 AOKeystoreAlternativeException {
    	final FileKeyStoreManager javaKsm = new JavaKeyStoreManager();
    	javaKsm.setKeyStoreFile(lib);

    	return addFileKeyStoreManager(
    		javaKsm,
			new String[] {
                "jks" //$NON-NLS-1$
            },
            KeyStoreMessages.getString("AOKeyStoreManagerFactory.1"), //$NON-NLS-1$
			lib,
			pssCallback,
			forceReset,
			parentComponent
		);
    }

	private static AOKeyStoreManager getSmartCafeJavaKeyStoreManager(final PasswordCallback pssCallback,
			                                                     final boolean forceReset,
			                                                     final Object parentComponent) throws AOKeystoreAlternativeException,
	                                                                                                  IOException {
		final AOKeyStoreManager ksm = new AOKeyStoreManager();
		try {
			ksm.init(AOKeyStore.SMARTCAFE, null, pssCallback, new Object[] { parentComponent }, forceReset);
		}
		catch (final AOKeyStoreManagerException e) {
			throw new AOKeystoreAlternativeException(getAlternateKeyStoreType(AOKeyStore.PKCS12),
				"Error al inicializar el modulo G&D SmartCafe 100% Java: " + e, e //$NON-NLS-1$
			);
		}
		ksm.setPreferred(true);
		return ksm;
	}

	private static AOKeyStoreManager getCeresJavaKeyStoreManager(final PasswordCallback pssCallback,
																	final boolean forceReset,
																	final Object parentComponent) throws AOKeystoreAlternativeException,
	IOException {
		final AOKeyStoreManager ksm = new AOKeyStoreManager();
		try {
			ksm.init(AOKeyStore.CERES, null, pssCallback, new Object[] { parentComponent }, forceReset);
		}
		catch (final AOKeyStoreManagerException e) {
			throw new AOKeystoreAlternativeException(
					getAlternateKeyStoreType(AOKeyStore.PKCS12),
					"Error al inicializar el modulo CERES 100% Java: " + e, //$NON-NLS-1$
					e
					);
		}
		ksm.setPreferred(true);
		return ksm;
	}

	private static AOKeyStoreManager getCeres430JavaKeyStoreManager(final PasswordCallback pssCallback,
																	final boolean forceReset,
																	final Object parentComponent) throws AOKeystoreAlternativeException,
																										IOException {
		final AOKeyStoreManager ksm = new AOKeyStoreManager();
		try {
			ksm.init(AOKeyStore.CERES_430, null, pssCallback, new Object[] { parentComponent }, forceReset);
		}
		catch (final AOKeyStoreManagerException e) {
			throw new AOKeystoreAlternativeException(
					getAlternateKeyStoreType(AOKeyStore.PKCS12),
					"Error al inicializar el modulo CERES 430 100% Java: " + e, //$NON-NLS-1$
					e
					);
		}
		ksm.setPreferred(true);
		return ksm;
	}

	private static AOKeyStoreManager getDnieJavaKeyStoreManager(final PasswordCallback pssCallback,
																final boolean forceReset,
    	                                                        final Object parentComponent) throws AOKeystoreAlternativeException,
    																							     IOException {
    	final AOKeyStoreManager ksm = new AOKeyStoreManager();
    	try {
    		// Proporcionamos el componente padre como parametro
    		ksm.init(AOKeyStore.DNIEJAVA, null, pssCallback, new Object[] { parentComponent }, forceReset);
    	}
    	catch (final AOKeyStoreManagerException e) {
    	   throw new AOKeystoreAlternativeException(
                getAlternateKeyStoreType(AOKeyStore.PKCS12),
                "Error al inicializar el modulo DNIe 100% Java: " + e, //$NON-NLS-1$
                e
           );
		}
    	ksm.setPreferred(true);
    	return ksm;
    }

    private static AOKeyStoreManager getFileKeyStoreManager(final AOKeyStore store,
                                                            final String lib,
                                                            final PasswordCallback pssCallback,
                                                            final boolean forceReset,
                                                            final Object parentComponent) throws IOException,
                                                                                                 AOKeystoreAlternativeException {
    	final AOKeyStoreManager ksm = new AOKeyStoreManager();
        String storeFilename = null;
        if (lib != null && !lib.isEmpty() && new File(lib).exists()) {
            storeFilename = lib;
        }
        else {
            String desc = null;
            String[] exts = null;
            if (store == AOKeyStore.SINGLE) {
                exts = new String[] {
                        "cer", "p7b" //$NON-NLS-1$ //$NON-NLS-2$
                };
                desc = KeyStoreMessages.getString("AOKeyStoreManagerFactory.2"); //$NON-NLS-1$
            }
            else if (store == AOKeyStore.JCEKS || store == AOKeyStore.JAVACE) {
                exts = new String[] {
                        "jceks", "jks", "jce" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                };
                desc = KeyStoreMessages.getString("AOKeyStoreManagerFactory.3"); //$NON-NLS-1$
            }
            storeFilename = AOUIFactory.getLoadFiles(
        		KeyStoreMessages.getString("AOKeyStoreManagerFactory.4") + " " + store.getName(), //$NON-NLS-1$ //$NON-NLS-2$
        		null,
        		null,
        		exts,
        		desc,
        		false,
        		false,
        		null,
        		parentComponent
    		)[0].getAbsolutePath();
            if (storeFilename == null) {
                throw new AOCancelledOperationException("No se ha seleccionado el almacen de certificados"); //$NON-NLS-1$
            }
        }

        try (
    		final InputStream is = new FileInputStream(storeFilename);
		) {
            ksm.init(store, is, pssCallback, null, forceReset);
        }
        catch (final AOException e) {
            throw new AOKeystoreAlternativeException(
               getAlternateKeyStoreType(store),
               "No se ha podido abrir el almacen de tipo " + store.getName(), //$NON-NLS-1$
               e
            );
        }
        return ksm;
    }

    private static AOKeyStoreManager getPkcs11KeyStoreManager(final String lib,
                                                              final String description,
                                                              final PasswordCallback pssCallback,
                                                              final boolean forceReset,
                                                              final Object parentComponent) throws IOException,
                                                                                                   AOKeystoreAlternativeException {
    	final AOKeyStoreManager ksm = new AOKeyStoreManager();
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
        catch (final AOException e) {
            throw new AOKeystoreAlternativeException(
                 getAlternateKeyStoreType(AOKeyStore.PKCS11),
                 "Error al inicializar el modulo PKCS#11: " + e, //$NON-NLS-1$
                 e
            );
        }
        return ksm;
    }

    private static AOKeyStoreManager getWindowsAddressBookKeyStoreManager(final AOKeyStore store,
    		                                                              final boolean forceReset) throws IOException,
                                                                                                  AOKeystoreAlternativeException {
    	final AOKeyStoreManager ksm = new AOKeyStoreManager();
        try {
            ksm.init(store, null, NullPasswordCallback.getInstance(), null, forceReset);
        }
        catch (final AOException e) {
            throw new AOKeystoreAlternativeException(
                 getAlternateKeyStoreType(store),
                 "Error al inicializar el almacen " + store.getName(), //$NON-NLS-1$
                 e
            );
        }
        return ksm;
    }

    private static AOKeyStoreManager getWindowsMyCapiKeyStoreManager(final boolean forceReset) throws AOKeystoreAlternativeException,
    		                                                                                          IOException {
    	final AggregatedKeyStoreManager ksmCapi = new CAPIUnifiedKeyStoreManager();
		try {
			ksmCapi.init(AOKeyStore.WINDOWS, null, null, null, forceReset);
		}
		catch (final AOKeyStoreManagerException e) {
			throw new AOKeystoreAlternativeException(
                 getAlternateKeyStoreType(AOKeyStore.WINDOWS),
                 "Error al obtener almacen WINDOWS: " + e, //$NON-NLS-1$
                 e
             );
		}

		return ksmCapi;
    }

    private static AggregatedKeyStoreManager getNssKeyStoreManager(final String KsmClassName,
    		                                                       final PasswordCallback pssCallback,
    		                                                       final boolean forceReset,
    		                                                       final Object parentComponent) throws AOKeystoreAlternativeException,
                                                                                                        IOException {
    	final AggregatedKeyStoreManager ksmUni;
    	try {
    		ksmUni = (AggregatedKeyStoreManager) Class.forName(KsmClassName).getConstructor().newInstance();
    	}
    	catch(final Exception e) {
    		throw new AOKeystoreAlternativeException(
				getAlternateKeyStoreType(AOKeyStore.MOZ_UNI),
				"Error al obtener dinamicamente el almacen NSS: " + e, //$NON-NLS-1$
				e
			);
    	}
    	try {
    		// Proporcionamos el componente padre como parametro
    		ksmUni.init(AOKeyStore.SHARED_NSS, null, pssCallback, new Object[] { parentComponent }, forceReset);
    	}
    	catch (final AOException e) {
    		throw new AOKeystoreAlternativeException(
				getAlternateKeyStoreType(AOKeyStore.MOZ_UNI),
				"Error al inicializar el almacen NSS: " + e, //$NON-NLS-1$
				e
			);
    	}
    	return ksmUni;
    }

    private static AggregatedKeyStoreManager getSharedNssKeyStoreManager(final PasswordCallback pssCallback,
                                                                         final boolean forceReset,
                                                                         final Object parentComponent) throws AOKeystoreAlternativeException,
                                                                                                              IOException {
    	return getNssKeyStoreManager(
			"es.gob.afirma.keystores.mozilla.shared.SharedNssKeyStoreManager",  //$NON-NLS-1$
			pssCallback,
			forceReset,
			parentComponent
		);
    }

    private static AggregatedKeyStoreManager mozillaKeyStoreManager = null;
    private static AggregatedKeyStoreManager getMozillaUnifiedKeyStoreManager(final PasswordCallback pssCallback,
    		                                                                  final boolean forceReset,
                                                                              final Object parentComponent) throws AOKeystoreAlternativeException,
    		                                                                                                       IOException {
    	AggregatedKeyStoreManager ksm = mozillaKeyStoreManager;
    	if (ksm == null) {
    		ksm = getNssKeyStoreManager(
    	    		"es.gob.afirma.keystores.mozilla.MozillaUnifiedKeyStoreManager",  //$NON-NLS-1$
    	    		pssCallback,
    	    		forceReset,
    	    		parentComponent
    			);

    		if (!containsDnieJavaKeyStoreManager(ksm)) {
    			mozillaKeyStoreManager = ksm;
    		}
    	}
        return ksm;
    }

    private static boolean containsDnieJavaKeyStoreManager(final AggregatedKeyStoreManager aggregatedKsm) {

    	for (final AOKeyStoreManager ksm : aggregatedKsm.getKeyStoreManagers()) {
    		if (ksm.getType() == AOKeyStore.DNIEJAVA ||
    				ksm instanceof AggregatedKeyStoreManager && containsDnieJavaKeyStoreManager((AggregatedKeyStoreManager) ksm)) {
    			return true;
    		}
    	}
    	return false;
    }

    private static AggregatedKeyStoreManager getMacOSXKeyStoreManager(final AOKeyStore store,
    		                                                          final String lib,
    		                                                          final boolean forceReset,
    		                                                          final Object parentComponent) throws IOException,
                                                                                                           AOKeystoreAlternativeException {
    	final AOKeyStoreManager ksm = new AppleKeyStoreManager();
        // En Mac OS X podemos inicializar un KeyChain en un fichero particular o el por defecto del sistema
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
            throw new AOKeystoreAlternativeException(
        		getAlternateKeyStoreType(store), "Error al inicializar el Llavero de Mac OS X", e //$NON-NLS-1$
    		);
        }
        final AggregatedKeyStoreManager aksm = new AggregatedKeyStoreManager(ksm);
        try {
    		KeyStoreUtilities.addPreferredKeyStoreManagers(aksm, parentComponent);
    	}
    	catch (final AOCancelledOperationException e) {
    		LOGGER.info("Se cancelo el uso del driver Java: " + e); //$NON-NLS-1$
    	}

        return aksm;
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
        if (Platform.OS.MACOSX.equals(Platform.getOS()) && !AOKeyStore.APPLE.equals(currentStore)) {
            return AOKeyStore.APPLE;
        }
        return AOKeyStore.PKCS12;
    }
}