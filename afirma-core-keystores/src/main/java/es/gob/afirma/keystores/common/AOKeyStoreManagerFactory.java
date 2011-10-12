/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.keystores.common;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.InvalidKeyException;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;

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
     *        BR_PKCS11) o fichero de almac&eacute;n de claves (para
     *        PKCS#12, Java KeyStore, JCE KeyStore, X.509 y PKCS#7)
     * @param description
     *        Descripci&oacute;n del KeyStoreManager que se desea obtener,
     *        necesario para obtener el n&uacute;mero de z&oacute;calo de
     *        los modulos PKCS#11 obtenidos del Secmod de Mozilla / Firefox.
     *        Debe seguir el formato definido en el m&eacute;todo <code>toString()</code> de la clase <code>sun.security.pkcs11.Secmod.Module</code>
     * @param pssCallback
     *        Callback que solicita la password del repositorio que deseamos
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
     * @throws InvalidKeyException
     *         Cuando la contrase&ntilde;a del almac&eacute;n es incorrecta.
     */
    public static AOKeyStoreManager getAOKeyStoreManager(final AOKeyStore store,
                                                         final String lib,
                                                         final String description,
                                                         final PasswordCallback pssCallback,
                                                         final Object parentComponent) throws AOKeystoreAlternativeException, 
                                                                                              InvalidKeyException {

        final AOKeyStoreManager ksm = new AOKeyStoreManager();

        // Fichero P7, X509, P12/PFX o Java JKS, en cualquier sistema operativo
        if (store == AOKeyStore.PKCS12 || store == AOKeyStore.JAVA
            || store == AOKeyStore.SINGLE
            || store == AOKeyStore.JAVACE
            || store == AOKeyStore.JCEKS) {
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

                storeFilename = AOUIFactory.getLoadFileName(KeyStoreMessages.getString("AOKeyStoreManagerFactory.4") + " " + store.getDescription(), exts, desc, parentComponent); //$NON-NLS-1$ //$NON-NLS-2$
                if (storeFilename == null) {
                    throw new AOCancelledOperationException("No se ha seleccionado el almacen de certificados"); //$NON-NLS-1$
                }
            }
            final InputStream is;
            try {
                is = new FileInputStream(storeFilename);
                ksm.init(store, is, pssCallback, null);
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final IOException e) {
                throw new InvalidKeyException("La contrasena del almacen es incorrecta: " + e); //$NON-NLS-1$
            }
            catch (final Exception e) {
                throw new AOKeystoreAlternativeException(getAlternateKeyStoreType(store),
                                                         "No se ha podido abrir el almacen de tipo " + store.getDescription(), //$NON-NLS-1$
                                                         e);
            }
            return ksm;
        }
        // Token PKCS#11, en cualquier sistema operativo
        else if (store == AOKeyStore.PKCS11) {
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
                p11Lib = AOUIFactory.getLoadFileName(KeyStoreMessages.getString("AOKeyStoreManagerFactory.7"), exts, extsDesc, parentComponent); //$NON-NLS-1$
            }
            if (p11Lib == null) {
                throw new AOCancelledOperationException("No se ha seleccionado el controlador PKCS#11"); //$NON-NLS-1$
            }
            try {
                ksm.init(store, null, pssCallback, new String[] {
                        p11Lib, description
                });
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOKeystoreAlternativeException(getAlternateKeyStoreType(store), "Error al inicializar el modulo PKCS#11", e); //$NON-NLS-1$
            }
            return ksm;
        }

        // Internet Explorer en Windows (descartamos Internet Explorer en
        // Solaris, HP-UX o Mac OS X)
        // o Google Chrome en Windows, que tambien usa el almacen de CAPI
        else if (Platform.getOS().equals(Platform.OS.WINDOWS) &&
                (store == AOKeyStore.WINDOWS || store == AOKeyStore.WINROOT
                        || store == AOKeyStore.WINADDRESSBOOK || store == AOKeyStore.WINCA)) {
            try {
                ksm.init(store, null, new NullPasswordCallback(), null);
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOKeystoreAlternativeException(getAlternateKeyStoreType(store),
                                                         "Error al inicializar el almacen " + store.getDescription(), //$NON-NLS-1$
                                                         e);
            }
            return ksm;
        }

        else if (store == AOKeyStore.MOZ_UNI) {
            final AOKeyStoreManager ksmUni;
            try {
                ksmUni = (AOKeyStoreManager) AOUtil.classForName("es.gob.afirma.keystores.mozilla.MozillaUnifiedKeyStoreManager").newInstance(); //$NON-NLS-1$
            }
            catch(final Exception e) {
                throw new AOKeystoreAlternativeException(
                     getAlternateKeyStoreType(store),
                     "Error al obteniendo dinamicamente el almacen NSS unificado de Mozilla Firefox", //$NON-NLS-1$
                     e
                 );
            }
            try {
                ksmUni.init(AOKeyStore.MOZ_UNI, null, pssCallback, null);
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOKeystoreAlternativeException(getAlternateKeyStoreType(store),
                        "Error al inicializar el almacen NSS unificado de Mozilla Firefox", //$NON-NLS-1$
                        e);
            }
            return ksmUni;
        }

        // Apple Safari sobre Mac OS X
        // Identificacion del sistema operativo segun (anadiendo mayusculas
        // donde se necesitaba)
        // http://developer.apple.com/technotes/tn2002/tn2110.html
        else if (Platform.getOS().equals(Platform.OS.MACOSX) && store == AOKeyStore.APPLE) {
            // En Mac OS X podemos inicializar un KeyChain en un fichero particular o el "defecto del sistema"
            try {
                ksm.init(
                     store, 
                     (lib == null || "".equals(lib)) ? null : new FileInputStream(lib),  //$NON-NLS-1$
                     new NullPasswordCallback(), 
                     null
                );
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOKeystoreAlternativeException(getAlternateKeyStoreType(store), "Error al inicializar el Llavero de Mac OS X", e); //$NON-NLS-1$
            }
            return ksm;
        }

        throw new AOKeystoreAlternativeException(getAlternateKeyStoreType(store), "La plataforma de navegador '" + store.getDescription() //$NON-NLS-1$
                                                                                  + "' mas sistema operativo '" //$NON-NLS-1$
                                                                                  + Platform.getOS()
                                                                                  + "' no esta soportada"); //$NON-NLS-1$
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
