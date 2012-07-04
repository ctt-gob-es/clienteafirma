/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.main.common;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.InvalidOSException;
import es.gob.afirma.core.MissingLibraryException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.dnie.DnieUnifiedKeyStoreManager;
import es.gob.afirma.keystores.main.callbacks.NullPasswordCallback;

/** Obtiene clases de tipo AOKeyStoreManager seg&uacute;n se necesiten,
 * proporcionando adem&aacute;s ciertos m&eacute;todos de utilidad. Contiene
 * fragmentos de las clases <code>com.sun.deploy.config.UnixConfig</code> y <code>com.sun.deploy.config.WinConfig</code>
 * @version 0.3 */
public final class AOKeyStoreManagerFactory {

    private AOKeyStoreManagerFactory() {
        // No permitimos la instanciacion
    }

    /** Obtiene el <code>KeyStoreManager</code> del tipo indicado.
     * @param store
     *        Almac&eacute;n de claves
     * @param lib
     *        Biblioteca del KeyStore (solo para KeyStoreManager de tipo
     *        PKCS#11) o fichero de almac&eacute;n de claves (para
     *        PKCS#12, Java KeyStore, JCE KeyStore, X.509, llavero de Mac OS X [opcional] y PKCS#7)
     * @param description
     *        Descripci&oacute;n del KeyStoreManager que se desea obtener,
     *        necesario para obtener el n&uacute;mero de z&oacute;calo de
     *        los modulos PKCS#11 obtenidos del Secmod de Mozilla / Firefox.
     *        Debe seguir el formato definido en el m&eacute;todo <code>toString()</code> de la clase <code>sun.security.pkcs11.Secmod.Module</code>
     * @param pssCallback
     *        <i>Callback</i> que solicita la password del repositorio que deseamos
     *        recuperar.
     * @param parentComponent
     *        Componente padre sobre el que mostrar los di&aacute;logos (normalmente un <code>java.awt.Comonent</code>)
     *        modales de ser necesario.
     * @return KeyStoreManager del tipo indicado
     * @throws AOCancelledOperationException
     *         Cuando el usuario cancela el proceso (por ejemplo, al
     *         introducir la contrase&ntilde;a)
     * @throws AOKeystoreAlternativeException
     *         Cuando ocurre cualquier otro problema durante el proceso
     * @throws IOException
     *         Cuando la contrase&ntilde;a del almac&eacute;n es incorrecta.
     * @throws InvalidOSException Cuando se pide un almac&eacute; &uacute;nicamente disponible para
     *                            un sistema operativo distinto del actual
     * @throws MissingLibraryException Cuando no se localice una biblioteca necesaria para el
     * uso del almac&eacute;n.
     */
    public static AOKeyStoreManager getAOKeyStoreManager(final AOKeyStore store,
                                                         final String lib,
                                                         final String description,
                                                         final PasswordCallback pssCallback,
                                                         final Object parentComponent) throws AOKeystoreAlternativeException,
                                                                                              IOException, MissingLibraryException, InvalidOSException {

        // Fichero P7, X509, P12/PFX o Java JKS, en cualquier sistema operativo
        if (AOKeyStore.PKCS12.equals(store) ||
    		AOKeyStore.JAVA.equals(store)   ||
    		AOKeyStore.SINGLE.equals(store) ||
    		AOKeyStore.JAVACE.equals(store) ||
    		AOKeyStore.JCEKS.equals(store)) {
        		return getFileKeyStoreManager(store, lib, pssCallback, parentComponent);
        }

        // Token PKCS#11, en cualquier sistema operativo
        else if (AOKeyStore.PKCS11.equals(store)) {
        	return getPkcs11KeyStoreManager(store, lib, description, pssCallback, parentComponent);
        }

        // Almacen de certificados personales de Windows. Le agregamos el gestor de DNIe para
        // que agregue los certificados mediante el controlador Java del DNIe si se encuentra la
        // biblioteca y hay un DNIe insertado
        else if (Platform.getOS().equals(Platform.OS.WINDOWS) &&
                AOKeyStore.WINDOWS.equals(store)) {
        	return getWindowsCapiKeyStoreManager(store);
        }

        // Almacen de certificados de Windows (distintos a los personales)
        else if (Platform.getOS().equals(Platform.OS.WINDOWS) &&
                (AOKeyStore.WINROOT.equals(store) ||
        		 AOKeyStore.WINADDRESSBOOK.equals(store) ||
        		 AOKeyStore.WINCA.equals(store))) {
        	return getWindowsCapiKeyStoreManager(store);
        }

        else if (AOKeyStore.DNIE.equals(store)) {
        	return getDniePkcs11KeyStoreManager(store, pssCallback);
        }

        // Almacen de Mozilla que muestra tanto los certificados del almacen interno como los de
        // los dispositivos externos configuramos. A esto, le agregamos el gestor de DNIe para
        // que agregue los certificados de este mediante el controlador Java del DNIe si se encuentra
        // la biblioteca y hay un DNIe insertado
        else if (AOKeyStore.MOZ_UNI.equals(store)) {
        	if (Platform.OS.MACOSX.equals(Platform.getOS())) {
        		return new DnieUnifiedKeyStoreManager(getMozillaUnifiedKeyStoreManager(store, pssCallback), parentComponent);
        	}
        	return getMozillaUnifiedKeyStoreManager(store, pssCallback);
        }

        // Apple Safari sobre Mac OS X. Le agregamos el gestor de DNIe para que agregue los
        // certificados mediante el controlador Java del DNIe si se encuentra la biblioteca y
        // hay un DNIe insertado
        else if (Platform.getOS().equals(Platform.OS.MACOSX) && AOKeyStore.APPLE.equals(store)) {
        	return new DnieUnifiedKeyStoreManager(getMacOSXKeyStoreManager(store, lib), parentComponent);
        }

        else if (AOKeyStore.DNIEJAVA.equals(store)) {
        	return getDnieJavaKeyStoreManager(store, pssCallback, parentComponent);
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

    private static AOKeyStoreManager getDnieJavaKeyStoreManager(final AOKeyStore store,
    		                                                    final PasswordCallback pssCallback,
    	                                                         final Object parentComponent)
    															throws AOKeystoreAlternativeException,
    															IOException {
    	final AOKeyStoreManager ksm = new AOKeyStoreManager();
    	try {
    		// Proporcionamos el componente padre como parametro
    		ksm.init(store, null, pssCallback, new Object[] { parentComponent });
    	}
    	catch (final IOException e) {
    		throw e;
		}
    	catch(final Exception e) {
    		throw new AOKeystoreAlternativeException(
                getAlternateKeyStoreType(store),
                "Error al inicializar el modulo DNIe 100% Java: " + e, //$NON-NLS-1$
                e
           );
    	}
    	return ksm;
    }

    private static AOKeyStoreManager getFileKeyStoreManager(final AOKeyStore store,
                                                            final String lib,
                                                            final PasswordCallback pssCallback,
                                                            final Object parentComponent) throws MissingLibraryException,
                                                                                                 InvalidOSException,
                                                                                                 IOException,
                                                                                                 AOKeystoreAlternativeException {
    	final AOKeyStoreManager ksm = new AOKeyStoreManager();
        String storeFilename = null;
        if (lib != null && !"".equals(lib) && new File(lib).exists()) { //$NON-NLS-1$
            storeFilename = lib;
        }
        if (storeFilename == null) {
            String desc = null;
            String[] exts = null;
            if (store == AOKeyStore.PKCS12) {
                exts = new String[] {
                        "pfx", "p12" //$NON-NLS-1$ //$NON-NLS-2$
                };
                desc = KeyStoreMessages.getString("AOKeyStoreManagerFactory.0"); //$NON-NLS-1$
            }
            if (store == AOKeyStore.JAVA) {
                exts = new String[] {
                    "jks" //$NON-NLS-1$
                };
                desc = KeyStoreMessages.getString("AOKeyStoreManagerFactory.1"); //$NON-NLS-1$
            }
            if (store == AOKeyStore.SINGLE) {
                exts = new String[] {
                        "cer", "p7b" //$NON-NLS-1$ //$NON-NLS-2$
                };
                desc = KeyStoreMessages.getString("AOKeyStoreManagerFactory.2"); //$NON-NLS-1$
            }
            if (store == AOKeyStore.JCEKS) {
                exts = new String[] {
                        "jceks", "jks" //$NON-NLS-1$ //$NON-NLS-2$
                };
                desc = KeyStoreMessages.getString("AOKeyStoreManagerFactory.3"); //$NON-NLS-1$
            }
            storeFilename = AOUIFactory.getLoadFileName(KeyStoreMessages.getString("AOKeyStoreManagerFactory.4") + " " + store.getName(), exts, desc, parentComponent); //$NON-NLS-1$ //$NON-NLS-2$
            if (storeFilename == null) {
                throw new AOCancelledOperationException("No se ha seleccionado el almacen de certificados"); //$NON-NLS-1$
            }
        }

        final InputStream is;
        try {
            is = new FileInputStream(storeFilename);
            ksm.init(store, is, pssCallback, null);
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

    private static AOKeyStoreManager getPkcs11KeyStoreManager(final AOKeyStore store,
                                                              final String lib,
                                                              final String description,
                                                              final PasswordCallback pssCallback,
                                                              final Object parentComponent) throws MissingLibraryException,
                                                                                                   InvalidOSException,
                                                                                                   IOException,
                                                                                                   AOKeystoreAlternativeException {
    	final AOKeyStoreManager ksm = new AOKeyStoreManager();
        String p11Lib = null;
        if (lib != null && !"".equals(lib) && new File(lib).exists()) { //$NON-NLS-1$
            p11Lib = lib;
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
            p11Lib = AOUIFactory.getLoadFileName(
                 KeyStoreMessages.getString("AOKeyStoreManagerFactory.7"),  //$NON-NLS-1$
                 exts,
                 extsDesc,
                 parentComponent
            );
        }
        if (p11Lib == null) {
            throw new AOCancelledOperationException("No se ha seleccionado el controlador PKCS#11"); //$NON-NLS-1$
        }
        try {
            ksm.init(store, null, pssCallback, new String[] {
                    p11Lib, description
            });
        }
        catch (final AOException e) {
            throw new AOKeystoreAlternativeException(
                 getAlternateKeyStoreType(store),
                 "Error al inicializar el modulo PKCS#11", //$NON-NLS-1$
                 e
            );
        }
        return ksm;
    }

    private static AOKeyStoreManager getWindowsCapiKeyStoreManager(final AOKeyStore store) throws MissingLibraryException,
                                                                                                  InvalidOSException,
                                                                                                  IOException,
                                                                                                  AOKeystoreAlternativeException {
    	final AOKeyStoreManager ksm = new AOKeyStoreManager();
        try {
            ksm.init(store, null, new NullPasswordCallback(), null);
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

    private static AOKeyStoreManager getDniePkcs11KeyStoreManager(final AOKeyStore store,
                                                                  final PasswordCallback pssCallback) throws MissingLibraryException,
                                                                                                             InvalidOSException,
                                                                                                             IOException,
                                                                                                             AOKeystoreAlternativeException {
    	final AOKeyStoreManager ksm = new AOKeyStoreManager();
    	try {
            ksm.init(
        		store,
        		null,
        		(!(pssCallback instanceof NullPasswordCallback)) ? pssCallback : null,
        		null
    		);
        }
        catch (final AOException e) {
            throw new AOKeystoreAlternativeException(
                 getAlternateKeyStoreType(store),
                 "Error al inicializar el PKCS#11 del DNIe", //$NON-NLS-1$
                 e
            );
        }
        return ksm;
    }

    private static AOKeyStoreManager getMozillaUnifiedKeyStoreManager(final AOKeyStore store,
    		                                                          final PasswordCallback pssCallback) throws AOKeystoreAlternativeException,
    		                                                                                                     MissingLibraryException,
    		                                                                                                     InvalidOSException,
    		                                                                                                     IOException {
        final AOKeyStoreManager ksmUni;
        try {
            ksmUni = (AOKeyStoreManager) AOUtil.classForName("es.gob.afirma.keystores.mozilla.MozillaUnifiedKeyStoreManager").newInstance(); //$NON-NLS-1$
        }
        catch(final Exception e) {
            throw new AOKeystoreAlternativeException(
                 getAlternateKeyStoreType(store),
                 "Error al obtener dinamicamente el almacen NSS unificado de Mozilla Firefox", //$NON-NLS-1$
                 e
             );
        }
        try {
            ksmUni.init(AOKeyStore.MOZ_UNI, null, pssCallback, null);
        }
        catch (final AOException e) {
            throw new AOKeystoreAlternativeException(
                getAlternateKeyStoreType(store),
                "Error al inicializar el almacen NSS unificado de Mozilla Firefox", //$NON-NLS-1$
                e
            );
        }
        return ksmUni;
    }

    private static AOKeyStoreManager getMacOSXKeyStoreManager(final AOKeyStore store, final String lib) throws MissingLibraryException,
                                                                                                        InvalidOSException,
                                                                                                        FileNotFoundException,
                                                                                                        IOException,
                                                                                                        AOKeystoreAlternativeException {
    	final AOKeyStoreManager ksm = new AOKeyStoreManager();
        // En Mac OS X podemos inicializar un KeyChain en un fichero particular o el "defecto del sistema"
        try {
            ksm.init(
                 store,
                 (lib == null || "".equals(lib)) ? null : new FileInputStream(lib),  //$NON-NLS-1$
                 new NullPasswordCallback(),
                 null
            );
        }
        catch (final AOException e) {
            throw new AOKeystoreAlternativeException(getAlternateKeyStoreType(store), "Error al inicializar el Llavero de Mac OS X", e); //$NON-NLS-1$
        }
        return ksm;
    }

    /** @return <code>AOKeyStore</code> alternativo o <code>null</code> si no hay alternativo */
    private static AOKeyStore getAlternateKeyStoreType(final AOKeyStore currentStore) {
        if (AOKeyStore.PKCS12.equals(currentStore)) {
            return null;
        }
        if (Platform.OS.WINDOWS.equals(Platform.getOS()) && (!AOKeyStore.WINDOWS.equals(currentStore))) {
            return AOKeyStore.WINDOWS;
        }
        if (Platform.OS.MACOSX.equals(Platform.getOS()) && (!AOKeyStore.APPLE.equals(currentStore))) {
            return AOKeyStore.APPLE;
        }
        return AOKeyStore.PKCS12;
    }

}