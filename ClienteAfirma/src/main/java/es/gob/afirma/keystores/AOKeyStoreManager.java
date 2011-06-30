/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.keystores;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.security.Provider;
import java.security.Security;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.List;
import java.util.Vector;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.callbacks.NullPasswordCallback;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOCertificateKeyException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOKeyStoreManagerException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.Platform;

/** Clase gestora de claves y certificados. B&aacute;sicamente se encarga de
 * crear KeyStores de distintos tipos, utilizando el proveedor JCA apropiado
 * para cada caso
 * @version 0.3 */
public class AOKeyStoreManager {

    /** Instancia del provider de NSS. S&oacute;lo se permite una instancia de
     * esta clase, as&iacute; que la cacheamos. */
    protected Provider nssProvider = null;

    /** Instancia del provider CAPI de Sun. Aunque se permite m&aacute;s de una
     * instancia de este provider, lo cacheamos para evitar problemas. */
    private static Provider sunMSCAPIProvider = null;

    /** Almac&eacute;n de claves. */
    private KeyStore ks;

    /** Tipo de almac&eacute;n. */
    private AOConstants.AOKeyStore ksType;

    /** Devuelve el tipo de almac&eacute;n de claves.
     * @return Tipo de almac&eacute;n de claves */
    public AOConstants.AOKeyStore getType() {
        return ksType;
    }

    /** Obtiene un almac&eacute;n de claves ya inicializado. Se encarga
     * tambi&eacute;n de a&ntilde;adir o retirar los <i>Provider</i> necesarios
     * para operar con dicho almac&eacute;n
     * @param type
     *        Tipo del almac&eacute;n de claves
     * @param store
     *        Flujo para la lectura directa del almac&acute;n de claves
     *        (solo para los almacenes en disco)
     * @param pssCallBack
     *        CallBack encargado de recuperar la contrase¬ntilde;a del
     *        Keystore
     * @param params
     *        Par&aacute;metros adicionales
     * @return Almac&eacute;n de claves solicitado (<b>ya inicializado</b>,
     *         pueden ser varios en el caso de Mozilla, el interno y los
     *         externos)
     * @throws AOException
     *         Cuando ocurre cualquier problema durante la
     *         inicializaci&oacute;n */
    public Vector<KeyStore> init(AOConstants.AOKeyStore type, final InputStream store, PasswordCallback pssCallBack, final Object[] params) throws AOException {

        Logger.getLogger("es.gob.afirma").info("Inicializamos el almacen de tipo: " + type);

        final Vector<KeyStore> ret = new Vector<KeyStore>(1);

        if (type == null) {
            Logger.getLogger("es.gob.afirma").severe("Se ha solicitado inicializar un AOKeyStore nulo, se intentara inicializar un PKCS#12");
            type = AOConstants.AOKeyStore.PKCS12;
        }
        ksType = type;

        if (pssCallBack == null) pssCallBack = new NullPasswordCallback();

        if (type == AOConstants.AOKeyStore.SINGLE) {

            if (store == null) throw new AOException("Es necesario proporcionar el fichero X.509 o PKCS#7");

            final Provider pkcs7Provider = new SingleCertKeyStoreProvider();
            Security.addProvider(pkcs7Provider);

            try {
                ks = KeyStore.getInstance(type.getName(), pkcs7Provider);
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido obtener el almacen " + type.getName() + " solicitado", e);
            }

            try {
                ks.load(store, pssCallBack.getPassword());
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido abrir el almacen " + type.getName() + " solicitado", e);
            }
            ret.add(ks);
            try {
                store.close();
            }
            catch (final Exception e) {}
            return ret;

        }

        else if (type == AOConstants.AOKeyStore.JAVA || type == AOConstants.AOKeyStore.JAVACE || type == AOConstants.AOKeyStore.JCEKS) {
            // Suponemos que el proveedor SunJSSE esta instalado. Hay que tener
            // cuidado con esto
            // si alguna vez se usa JSS, que a veces lo retira
            if (store == null) throw new AOException("Es necesario proporcionar el fichero KeyStore");

            try {
                ks = KeyStore.getInstance(type.getName());
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido obtener el almacen de nombre '" + type.getName() + "'", e);
            }

            // TODO: Revisar si el KeyStore de Java requiere contrasena
            try {
                ks.load(store, pssCallBack.getPassword());
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido abrir el almacen " + type.getName() + " solicitado.", e);
            }
            ret.add(ks);
            try {
                store.close();
            }
            catch (final Exception e) {}
            return ret;
        }

        else if (type == AOConstants.AOKeyStore.PKCS12) {

            // Suponemos que el proveedor SunJSSE esta instalado. Hay que tener
            // cuidado con esto
            // si alguna vez se usa JSS, que a veces lo retira

            if (store == null) throw new AOException("Es necesario proporcionar el fichero PKCS12 / PFX");

            try {
                ks = KeyStore.getInstance(type.getName());
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido obtener el almacen de nombre '" + type.getName() + "'", e);
            }
            try {
                ks.load(store, pssCallBack.getPassword());
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido abrir el almacen " + type.getName() + " solicitado.", e);
            }
            ret.add(ks);
            try {
                store.close();
            }
            catch (final Exception e) {}
            return ret;

        }

        // else if (type == AOConstants.AOKeyStore.WINDEPLOY) {
        // if (!Platform.getOS().equals(Platform.OS.WINDOWS)) {
        // throw new AOKeyStoreManagerException(
        // "No se puede obtener el KeyStore de Windows en un sistema no Windows:"
        // +
        // Platform.getOS()
        // );
        // }
        // try {
        // Security.addProvider((Provider)Class.forName("com.sun.deploy.security.MSCryptoProvider").newInstance());
        // }
        // catch(final Exception e) {
        // Logger.getLogger("es.gob.afirma").warning(
        // "No se ha podido instanciar el proveedor 'com.sun.deploy.security.MSCryptoProvider': "
        // + e
        // );
        // }
        // try {
        // ks = KeyStore.getInstance(type.getName());
        // }
        // catch(final Exception e) {
        // throw new AOKeyStoreManagerException(
        // "No se ha podido obtener el KeyStore de nombre '" + type.getName() +
        // "' del Provider 'com.sun.deploy.security.MSCryptoProvider'", e
        // );
        // }
        // try {
        // ks.load(null, null);
        // }
        // catch(final Exception e) {
        // throw new AOException(
        // "Error inicializando el almacen de claves y certificados del proveedor com.sun.deploy.security.MSCryptoProvider",
        // e
        // );
        // }
        //
        // ret.add(ks);
        // return ret;
        //
        // }

        else if (type == AOConstants.AOKeyStore.WINDOWS || type == AOConstants.AOKeyStore.WINROOT) {

            if (!Platform.getOS().equals(Platform.OS.WINDOWS)) {
                throw new AOKeyStoreManagerException("No se puede obtener el KeyStore de Windows en un sistema no Windows:" + Platform.getOS());
            }

            // Si no se ha agregado el proveedor CAPI de Sun, lo anadimos
            // En java 6 viene instalado de serie, pero no pasa nada por
            // reinstalarlo
            if (sunMSCAPIProvider == null) {
                try {
                    sunMSCAPIProvider = (Provider) Class.forName("sun.security.mscapi.SunMSCAPI").newInstance();
                    Security.insertProviderAt(sunMSCAPIProvider, 1);
                }
                catch (final Exception e) {
                    Logger.getLogger("es.gob.afirma").warning("No se ha podido instanciar el proveedor 'sun.security.mscapi.SunMSCAPI': " + e);
                }
            }

            // Inicializamos
            try {
                ks = KeyStore.getInstance(type.getName());
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido obtener el KeyStore de nombre '" + type.getName()
                                                     + "' del Provider 'sun.security.mscapi.SunMSCAPI'", e);
            }

            Logger.getLogger("es.gob.afirma").info("Cargando KeyStore de Windows.");
            try {
                ks.load(store, pssCallBack.getPassword());
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido inicializar el KeyStore de nombre '" + type.getName()
                                                     + "' del Provider 'sun.security.mscapi.SunMSCAPI'", e);
            }

            // Tratamos los alias repetidos, situacion problematica afectada por
            // el bug
            // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6483657
            // Este solo se da con SunMSCAPI
            try {
                KeyStoreUtilities.cleanCAPIDuplicateAliases(ks);
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("No se han podido tratar los alias duplicados: " + e);
            }

            ret.add(ks);
            return ret;
        }

        else if (type == AOConstants.AOKeyStore.WINCA || type == AOConstants.AOKeyStore.WINADDRESSBOOK) {

            if (!Platform.getOS().equals(Platform.OS.WINDOWS)) {
                throw new AOKeyStoreManagerException("No se puede obtener el KeyStore ADDRESSBOOK de Windows en un sistema no Windows:" + Platform.getOS());
            }

            // Nos aseguramos de que SunMSCAPI este cargado, para que la DLL
            // sunmscapi.dll tambien lo este
            if (Security.getProvider("SunMSCAPI") == null) {
                try {
                    Security.addProvider((Provider) Class.forName("sun.security.mscapi.SunMSCAPI").newInstance());
                }
                catch (final Exception e) {
                    throw new AOKeyStoreManagerException("No se ha podido obtener el KeyStore de nombre '" + type.getName()
                                                         + "' por no poderse cargar el Provider 'sun.security.mscapi.SunMSCAPI'", e);
                }
            }
            Provider p = Security.getProvider("SunMSCAPIAddressBook");
            if (p == null) {
                try {
                    p = (Provider) Class.forName("es.gob.afirma.keystores.SunMSCAPIAddressBook").newInstance();
                }
                catch (final Exception e) {
                    throw new AOException("Error obteniendo el almacen '" + type.getName()
                                          + "' por no poder instanciarse el proveedor SunMSCAPIAddressBook", e);
                }
                Security.addProvider(p);
            }

            try {
                ks = KeyStore.getInstance(type.getName(), p);
            }
            catch (final Exception e) {
                throw new AOException("Error obteniendo el almacen '" + type.getName() + "' del proveedor SunMSCAPIAddressBook", e);
            }

            try {
                ks.load(null, null);
            }
            catch (final Exception e) {
                throw new AOException("Error inicializando el almacen de claves y certificados del proveedor SunMSCAPIAddressBook", e);
            }

            ret.add(ks);
            return ret;
        }

        else if (type == AOConstants.AOKeyStore.PKCS11) {
            // En el "params" debemos traer los parametros:
            // [0] - p11lib: Biblioteca PKCS#11, debe estar en el Path (Windows)
            // o en el
            // LD_LIBRARY_PATH (UNIX, Linux, Mac OS X)
            // [1] -desc: Descripcion del token PKCS#11 (opcional)
            // [2] -slot: Numero de lector de tarjeta (Sistema Operativo)
            // [OPCIONAL]

            // Anadimos el proveedor PKCS11 de Sun
            if (params == null || params.length < 2) throw new AOException("No se puede acceder al KeyStore PKCS#11 si no se especifica la biblioteca");
            final String p11lib;
            if (params[0] != null) p11lib = params[0].toString();
            else throw new NullPointerException("No se puede acceder al KeyStore PKCS#11 si se especifica una biblioteca nula");

            // Numero de lector
            Integer slot = null;
            if (params.length >= 3) {
                if (params[2] instanceof Integer) slot = (Integer) params[2];
            }

            // Agregamos un nombre a cada PKCS#11 para asegurarnos de no se
            // agregan mas de una vez como provider.
            // Si ya se cargo el PKCS#11 anteriormente, se volvera a instanciar.
            final String p11ProviderName = new File(p11lib).getName().replace('.', '_').replace(' ', '_');
            Provider p11Provider = Security.getProvider("SunPKCS11-" + p11ProviderName);

            if (p11Provider == null) {

                Constructor<?> sunPKCS11Contructor;
                try {
                    sunPKCS11Contructor = Class.forName("sun.security.pkcs11.SunPKCS11").getConstructor(InputStream.class);
                }
                catch (Exception e) {
                    throw new AOKeyStoreManagerException("No se ha podido construir un proveedor de tipo 'sun.security.pkcs11.SunPKCS11'", e);
                }

                byte[] config = KeyStoreUtilities.createPKCS11ConfigFile(p11lib, p11ProviderName, slot).getBytes();
                try {
                    p11Provider = (Provider) sunPKCS11Contructor.newInstance( // TODO:
                                                                              // InvocationTargetException
                    new ByteArrayInputStream(config));
                }
                catch (final Exception e) {
                    // El PKCS#11 del DNIe a veces falla a la primera pero va
                    // correctamente a la segunda
                    // asi que reintentamos una vez mas
                    try {
                        p11Provider = (Provider) sunPKCS11Contructor.newInstance(new ByteArrayInputStream(config));
                    }
                    catch (final Exception ex) {
                        throw new AOException("Error instanciando el proveedor SunPKCS11 para la la biblioteca " + p11lib, ex);
                    }
                }
                try {
                    Security.addProvider(p11Provider);
                }
                catch (final Exception e) {
                    throw new AOException("Error instalando el proveedor SunPKCS11 para la la biblioteca " + p11lib, e);
                }
            }
            else {
                Logger.getLogger("es.gob.afirma")
                      .info("El proveedor PKCS#11 solicitado ya estaba instanciado, se reutilizara esa instancia: " + p11Provider.getName());
            }

            try {
                ks = KeyStore.getInstance(type.getName(), p11Provider);
            }
            catch (final Exception e) {
                Security.removeProvider(p11Provider.getName());
                p11Provider = null;
                throw new AOKeyStoreManagerException("No se ha podido obtener el KeyStore de nombre '" + type.getName()
                                                     + "' del Provider 'sun.security.pkcs11.SunPKCS11'", e);
            }

            try {
                ks.load(store, pssCallBack.getPassword());
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                Security.removeProvider(p11Provider.getName());
                p11Provider = null;
                throw new AOKeyStoreManagerException("No se ha podido inicializar el KeyStore de nombre '" + type.getName()
                                                     + "' del Provider 'sun.security.pkcs11.SunPKCS11'", e);
            }
            ret.add(ks);
            return ret;
        }

        else if (type == AOConstants.AOKeyStore.MOZILLA) {

            // Usamos NSS

            // En el "params" debemos traer los parametros:
            // - mozillaProfileDir: Directorio de configuracion de Mozilla /
            // Firefox
            // - NSSLibDir: Directorio con las librerias de NSS
            // Se ha detectado que es necesario disponder de algunas librerias
            // de NSS en el directorio
            // de Mozilla / Firefox.

            // Anadimos el proveedor PKCS11 de Sun
            if (params == null || params.length < 2) throw new AOException("No se puede acceder al KeyStore PKCS#11 de NSS si no se especifica el directorio del perfil de " + "usuario y la ubicacion de las bibliotecas");
            final String mozillaProfileDir = params[0].toString();
            final String nssLibDir = params[1].toString();

            // Logger.getLogger("es.gob.afirma").info("Se usaran las bibliotecas NSS del siguiente directorio: "+NSSLibDir);

            nssProvider = Security.getProvider("SunPKCS11-NSSCrypto-AFirma");

            if (nssProvider == null) {
                MozillaKeyStoreUtilities.loadNSSDependencies(nssLibDir);
                try {
                    // nssProvider = new sun.security.pkcs11.SunPKCS11(new
                    // ByteArrayInputStream(KeyStoreUtilities.createPKCS11NSSConfigFile(mozillaProfileDir,
                    // NSSLibDir).getBytes()));
                    Constructor<?> sunPKCS11Constructor = Class.forName("sun.security.pkcs11.SunPKCS11").getConstructor(InputStream.class);
                    nssProvider =
                            (Provider) sunPKCS11Constructor.newInstance(new ByteArrayInputStream(MozillaKeyStoreUtilities.createPKCS11NSSConfigFile(mozillaProfileDir,
                                                                                                                                                    nssLibDir)
                                                                                                                         .getBytes()));
                }
                catch (final Exception e) {
                    throw new AOException("No se ha podido instanciar la clase SunPKCS11, se abortara la operacion", e);
                }
                Security.addProvider(nssProvider);
            }

            try {
                ks = KeyStore.getInstance(type.getName(), nssProvider);
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido obtener el KeyStore de nombre '" + type.getName()
                                                     + "' del Provider 'sun.security.pkcs11.SunPKCS11'", e);
            }

            try {
                ks.load(store, pssCallBack.getPassword());
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido inicializar el KeyStore de nombre '" + type.getName()
                                                     + "' del Provider 'sun.security.pkcs11.SunPKCS11'", e);
            }
            ret.add(ks);
            return ret;
        }

        else if (type == AOConstants.AOKeyStore.APPLE) {

            // Anadimos el proveedor de Apple
            try {
                Security.insertProviderAt((Provider) Class.forName("com.apple.crypto.provider.Apple").newInstance(), 0);
            }
            catch (final Exception e) {
                throw new AOException("No se ha podido instanciar el proveedor 'com.apple.crypto.provider.Apple'", e);
            }

            // Inicializamos
            try {
                ks = KeyStore.getInstance(type.getName());
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido obtener el KeyStore de nombre '" + type.getName()
                                                     + "' del Provider 'com.apple.crypto.provider.Apple'", e);
            }

            try {
                ks.load(store, pssCallBack.getPassword());
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOKeyStoreManagerException("No se ha podido inicializar el KeyStore de nombre '" + type.getName()
                                                     + "' del Provider 'com.apple.crypto.provider.Apple'", e);
            }
            ret.add(ks);
            return ret;
        }

        throw new UnsupportedOperationException("Tipo de almacen no soportado");
    }

    /** Obtiene la clave privada de un certificado.
     * @param alias
     *        Alias del certificado
     * @param pssCallback
     *        <i>CallBback</i> para obtener la contrase&ntilde;a del
     *        certificado que contiene la clave
     * @return Clave privada del certificado correspondiente al alias
     * @throws AOCancelledOperationException
     *         Cuando el usuario cancela el proceso antes de que finalice
     * @throws AOCertificateKeyException
     *         Cuando ocurren errores obteniendo la clave privada del
     *         certificado */
    public KeyStore.PrivateKeyEntry getKeyEntry(final String alias, PasswordCallback pssCallback) throws AOCancelledOperationException,
                                                                                                 AOCertificateKeyException {

        if (pssCallback == null) pssCallback = new NullPasswordCallback();

        if (ks == null) throw new NullPointerException("Se han pedido claves a un almacen no inicializado");

        final KeyStore.PrivateKeyEntry keyEntry;

        // El llavero de Mac OS X no responde al getKeyEntry(), solo al
        // getKey(), pero
        // obligartoriamente hay que proporcionarle una cadena de texto no vacia
        // y no nula
        // como contrasena. Esta cadena puede contener cualquier texto, no se
        // comprueba.
        // Esta cadena de texto debe contener unicamente caracteres ASCII.
        if ("KeychainStore".equals(ks.getType())) {
            try {
                Logger.getLogger("es.gob.afirma").info("Detectado almacen Llavero de Mac OS X, se trataran directamente las claves privadas");
                Certificate[] certChain = ks.getCertificateChain(alias);
                if (certChain == null) {
                    Logger.getLogger("es.gob.afirma").warning("El certificado " + alias
                                                              + " no tiene su cadena completa de confianza "
                                                              + "instalada en el Llavero de Mac OS X, se insertara solo este certificado");
                    certChain = new Certificate[] {
                        ks.getCertificate(alias)
                    };
                }

                keyEntry = new KeyStore.PrivateKeyEntry((PrivateKey) ks.getKey(alias, "dummy".toCharArray()), certChain);
            }
            catch (final Exception e) {
                throw new AOCertificateKeyException("Error intentando obtener la clave privada en Mac OS X", e);
            }
        }
        else {
            try {
                keyEntry = (KeyStore.PrivateKeyEntry) ks.getEntry(alias, new KeyStore.PasswordProtection(pssCallback.getPassword()));
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOCertificateKeyException("Error intentando obtener la clave privada", e);
            }
        }

        return keyEntry;
    }

    /** Obtiene el certificado correspondiente a una clave privada.
     * @param privateKeyEntry
     *        Clave privada del certificado
     * @return Certificado cuya clave privada es la indicada */
    public X509Certificate getCertificate(final KeyStore.PrivateKeyEntry privateKeyEntry) {
        final Certificate cert = privateKeyEntry.getCertificate();
        if (cert instanceof X509Certificate) return (X509Certificate) cert;
        Logger.getLogger("es.gob.afirma").severe("El certificado solicitado no era de tipo X509Certificate, se devolvera null");
        return null;
    }

    /** Obtiene un certificado del keystore activo a partir de su alias.
     * @param alias
     *        Alias del certificado.
     * @return El certificado o {@code null} si no se pudo recuperar. */
    public X509Certificate getCertificate(final String alias) {
        if (alias == null) {
            Logger.getLogger("es.gob.afirma").warning("El alias del certificado es nulo, se devolvera null");
            return null;
        }

        if (this.ks == null) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido recuperar el certificado con alias '" + alias
                                                      + "' porque el KeyStore no estaba inicializado, se devolvera null");
            return null;
        }

        Certificate cert = null;
        try {
            cert = ks.getCertificate(alias);
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido recuperar el certificado con alias '" + alias + "', se devolvera null: " + e);
            return null;
        }
        if (cert == null) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido recuperar el certificado con alias '" + alias + "', se devolvera null");
            return null;
        }
        if (cert instanceof X509Certificate) return (X509Certificate) cert;

        Logger.getLogger("es.gob.afirma").warning("El certificado con alias '" + alias + "' no es de tipo X509Certificate, se devolvera null");
        return null;

    }

    /** Obtiene la cadena de certificaci&oacute;n correspondiente a una clave
     * privada.
     * @param privateKeyEntry
     *        Clave privada del certificado
     * @return Certificados de la cadena de certificaci&oacute;n. */
    public X509Certificate[] getCertificateChain(final KeyStore.PrivateKeyEntry privateKeyEntry) {
        final Certificate[] certs = privateKeyEntry.getCertificateChain();
        if (certs != null && (certs instanceof X509Certificate[])) return (X509Certificate[]) certs;
        final Certificate cert = privateKeyEntry.getCertificate();
        if (cert instanceof X509Certificate) return new X509Certificate[] {
            (X509Certificate) cert
        };
        Logger.getLogger("es.gob.afirma").severe("No se ha podido obtener la cadena de certificados, se devolvera una cadena vacia");
        return new X509Certificate[0];
    }

    /** Obtiene la cadena de certificaci&oacute;n de un certificado del keystore
     * activo a partir de su alias.
     * @param alias
     *        Alias del certificado.
     * @return Certificados de la cadena de certificaci&oacute;n o {@code null} si no se pudo recuperar. */
    public X509Certificate[] getCertificateChain(final String alias) {
        if (ks == null) {
            Logger.getLogger("es.gob.afirma")
                  .warning("El KeyStore actual no esta inicializado, por lo que no se pudo recuperar el certificado para el alias '" + alias + "'");
            return null;
        }
        try {
            final Certificate[] certs = ks.getCertificateChain(alias);
            if (certs != null && (certs instanceof X509Certificate[])) return (X509Certificate[]) certs;
            final Certificate cert = ks.getCertificate(alias);
            if (cert instanceof X509Certificate) return new X509Certificate[] {
                (X509Certificate) cert
            };
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Error al obtener la cadena de certificados para el alias '" + alias
                                                     + "', se devolvera una cadena vacia: "
                                                     + e);
            return new X509Certificate[0];
        }
        Logger.getLogger("es.gob.afirma").severe("No se ha podido obtener la cadena de certificados para el alias '" + alias
                                                 + "', se devolvera una cadena vacia");
        return new X509Certificate[0];
    }

    /** Obtiene todos los alias de los certificados del almac&eacute;n actual.
     * @return Todos los alias encontrados en el almac&eacute;n actual */
    public String[] getAliases() {

        if (ks == null) throw new NullPointerException("Se han pedido los alias de un almacen no inicializado");

        Logger.getLogger("es.gob.afirma").info("Solicitando los alias al KeyStore (" + ks.getProvider() + ")");

        final Enumeration<String> aliases;
        try {
            aliases = ks.aliases();
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma")
                  .severe("Error intentando obtener los alias del almacen de claves, se devolvera " + "una enumeracion vacia: " + e);
            return new String[0];
        }

        String currAlias;
        final Vector<String> v = new Vector<String>();

        Logger.getLogger("es.gob.afirma").info("Componiendo el vector de alias");

        for (; aliases.hasMoreElements();) {
            currAlias = aliases.nextElement().toString();
            v.add(currAlias);
        }

        return v.toArray(new String[0]);

    }

    /** Devuelve el <code>keyStore</code> en uso.
     * @return Almac&eacute;n de claves (<code>KeyStore</code>) actual */
    public List<KeyStore> getKeyStores() {
        final List<KeyStore> ret = new Vector<KeyStore>(1);
        ret.add(ks);
        return ret;
    }

    /** Recupera el repositorio que posea la descripci&oacute;n indicada. Si no
     * existe un keystore con la descripci&oacute;n indicada, se devuleve <code>null</code>.
     * @param description
     *        Descripci&oacute;n del repositorio que se desea recuperar.
     * @return KeyStore Repositorio de certificados. */
    public static AOKeyStore getKeyStore(final String description) {
        AOKeyStore keystore = null;
        for (AOKeyStore tempKs : AOKeyStore.values()) {
            if (tempKs.getDescription().equals(description)) {
                return tempKs;
            }
        }
        return keystore;
    }

    @Override
    public String toString() {
        final StringBuilder ret = new StringBuilder("Gestor de almacenes de claves");
        if (ksType != null) {
            String tmpStr = ksType.getDescription();
            if (tmpStr != null) {
                ret.append(" de tipo ");
                ret.append(tmpStr);
            }
            tmpStr = ksType.getName();
            if (tmpStr != null) {
                ret.append(" con nombre ");
                ret.append(tmpStr);
            }
            ret.append(" de clase ");
            ret.append(ksType.toString());
        }
        return ret.toString();
    }
}
