/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone;

import java.awt.Component;
import java.io.File;
import java.util.logging.Logger;

import es.gob.afirma.callbacks.NullPasswordCallback;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOKeyStoreManagerException;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.AOKeystoreAlternativeException;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.standalone.ui.DNIePasswordCallback;

/** Gestor simple de <code>KeyStores</code>. Obtiene o un <code>KeyStore</code> de DNIe
 * v&iacute;a PKCS#11 o el <code>KeyStore</code> por defecto del sistema operativo
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SimpleKeyStoreManager {

    private SimpleKeyStoreManager() {}

    private static String getPKCS11DNIeLib() throws AOKeyStoreManagerException {
        if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
            final String lib = AOUtil.getSystemLibDir();
            if (new File(lib + "\\UsrPkcs11.dll").exists()) return lib + "\\UsrPkcs11.dll";  //$NON-NLS-1$ //$NON-NLS-2$
            // if (new File(lib + "\\AutBioPkcs11.dll").exists()) lib = lib + "\\AutBioPkcs11.dll";
            if (new File(lib + "\\opensc-pkcs11.dll").exists()) return lib + "\\opensc-pkcs11.dll";  //$NON-NLS-1$//$NON-NLS-2$
            throw new AOKeyStoreManagerException("No hay controlador PKCS#11 de DNIe instalado en este sistema Windows"); //$NON-NLS-1$
        }
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            if (new File("/Library/OpenSC/lib/libopensc-dnie.dylib").exists()) return "/Library/OpenSC/lib/libopensc-dnie.dylib";  //$NON-NLS-1$//$NON-NLS-2$
            if (new File("/Library/OpenSC/lib/opensc-pkcs11.so").exists()) return "/Library/OpenSC/lib/opensc-pkcs11.so"; //$NON-NLS-1$ //$NON-NLS-2$
            if (new File("/Library/OpenSC/lib/libopensc-dnie.1.0.3.dylib").exists()) return "/Library/OpenSC/lib/libopensc-dnie.1.0.3.dylib";  //$NON-NLS-1$//$NON-NLS-2$
            if (new File("/usr/lib/opensc-pkcs11.so").exists()) return "/usr/lib/opensc-pkcs11.so";  //$NON-NLS-1$//$NON-NLS-2$
            throw new AOKeyStoreManagerException("No hay controlador PKCS#11 de DNIe instalado en este sistema Mac OS X"); //$NON-NLS-1$
        }
        if (new File("/usr/local/lib/libopensc-dnie.so").exists()) return "/usr/local/lib/libopensc-dnie.so"; //$NON-NLS-1$ //$NON-NLS-2$
        if (new File("/usr/lib/libopensc-dnie.so").exists()) return "/usr/lib/libopensc-dnie.so"; //$NON-NLS-1$ //$NON-NLS-2$
        if (new File("/lib/libopensc-dnie.so").exists()) return "/lib/libopensc-dnie.so"; //$NON-NLS-1$ //$NON-NLS-2$
        if (new File("/usr/lib/opensc-pkcs11.so").exists()) return "/usr/lib/opensc-pkcs11.so";  //$NON-NLS-1$//$NON-NLS-2$
        if (new File("/lib/opensc-pkcs11.so").exists()) return "/lib/opensc-pkcs11.so";  //$NON-NLS-1$//$NON-NLS-2$
        if (new File("/usr/local/lib/opensc-pkcs11.so").exists()) return "/usr/local/lib/opensc-pkcs11.so"; //$NON-NLS-1$ //$NON-NLS-2$
        throw new AOKeyStoreManagerException("No hay controlador PKCS#11 de DNIe instalado en este sistema"); //$NON-NLS-1$
    }

    /** Obtiene un <code>KeyStore</code>.
     * @param dnie <code>true</code> si desea obtenerse un <code>KeyStore</code> para DNIe v&iacute;a PKCS#11, <code>false</code> si desea obtenerse
     *        el <code>KeyStore</code> por defecto del sistema operativo
     * @param parent Componente padre para la modalidad
     * @return <code>KeyStore</code> apropiado
     * @throws AOKeyStoreManagerException Si ocurre cualquier problema durante la obtenci&oacute;n del <code>KeyStore</code> */
    public static AOKeyStoreManager getKeyStore(final boolean dnie, final Component parent) throws AOKeyStoreManagerException {

        if (dnie) {
            final String lib = getPKCS11DNIeLib();
            try {
                return AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.PKCS11,
                     lib,
                     "DNIe", //$NON-NLS-1$
                     new DNIePasswordCallback(parent),
                     parent
                 );
            }
            catch(final AOCancelledOperationException e) {}
            catch(final AOKeystoreAlternativeException e) {}
            catch(final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("No se ha podido inicializar el controlador PKCS#11 del DNIe (" + lib + "): " + e);  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
            }
        }

        if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
            try {
                return AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.WINDOWS, null, null, new NullPasswordCallback(), parent);
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido inicializar SunMSCAPI", e); //$NON-NLS-1$
            }
        }

        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            try {
                return AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.APPLE, null, null, new NullPasswordCallback(), parent);
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido incializar el Llavero de Mac OS X", e); //$NON-NLS-1$
            }
        }

        return null;
    }

}
