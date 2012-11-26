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

import java.awt.Component;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;
import java.security.UnrecoverableEntryException;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Logger;

import javax.crypto.BadPaddingException;
import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.InvalidOSException;
import es.gob.afirma.core.MissingLibraryException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.main.callbacks.UIPasswordCallback;

/** Clase gestora de claves y certificados. B&aacute;sicamente se encarga de
 * crear KeyStores de distintos tipos, utilizando el proveedor JCA apropiado
 * para cada caso
 * @version 0.3 */
public class AOKeyStoreManager {

    protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Instancia del provider de NSS. S&oacute;lo se permite una instancia de
     * esta clase, as&iacute; que la cacheamos. */
    private Provider nssProvider = null;

    protected Provider getNSSProvider() {
        return this.nssProvider;
    }

    protected void setNSSProvider(final Provider p) {
        this.nssProvider = p;
    }

    /** Instancia del provider CAPI de Sun. Aunque se permite m&aacute;s de una
     * instancia de este provider, lo cacheamos para evitar problemas. */
    private static Provider sunMSCAPIProvider = null;

    /** Almac&eacute;n de claves. */
    private KeyStore ks;
    protected void setKeyStore(final KeyStore k) {
    	this.ks = k;
    }
    protected KeyStore getKeyStore() {
    	return this.ks;
    }

    /** Tipo de almac&eacute;n. */
    private AOKeyStore ksType;

    /** Devuelve el tipo de almac&eacute;n de claves.
     * @return Tipo de almac&eacute;n de claves */
    public AOKeyStore getType() {
        return this.ksType;
    }

    /**
     * Inicializa un almac&eacute;n PKCS#11.
     * @param pssCallBack Callback para la recuperaci&oacute;n de la
     *        contrase&ntilde;a del almac&eacute;n.
     * @param params Parametros adicionales para la configuraci&oacute;n del
     *        almac&eacute;n.
     * @return Array con los almacenes configurados.
     * @throws AOKeyStoreManagerException Cuando ocurre un error durante la inicializaci&oacute;n.
     * @throws IOException Cuando se indique una contrase&ntilde;a incorrecta para la
     *         apertura del almac&eacute;n.
     * @throws es.gob.afirma.keystores.main.common.MissingSunPKCS11Exception Si no se encuentra la biblioteca SunPKCS11 */
    private List<KeyStore> initPKCS11(final PasswordCallback pssCallBack,
    		                          final Object[] params) throws AOKeyStoreManagerException,
    		                                                        IOException {
        // En el "params" debemos traer los parametros:
        // [0] - p11lib: Biblioteca PKCS#11, debe estar en el Path (Windows) o en el LD_LIBRARY_PATH (UNIX, Linux, Mac OS X)
        // [1] -desc: Descripcion del token PKCS#11 (opcional)
        // [2] -slot: Numero de lector de tarjeta (Sistema Operativo) [OPCIONAL]

        // Anadimos el proveedor PKCS11 de Sun
        if (params == null || params.length < 2) {
            throw new IOException("No se puede acceder al KeyStore PKCS#11 si no se especifica la biblioteca"); //$NON-NLS-1$
        }
        final String p11lib;
        if (params[0] != null) {
            p11lib = params[0].toString();
        }
        else {
            throw new IllegalArgumentException("No se puede acceder al KeyStore PKCS#11 si se especifica una biblioteca nula"); //$NON-NLS-1$
        }

        // Numero de lector
        Integer slot = null;
        if (params.length >= 3 && params[2] instanceof Integer) {
            slot = (Integer) params[2];
        }

        // Agregamos un nombre a cada PKCS#11 para asegurarnos de no se
        // agregan mas de una vez como provider.
        // Si ya se cargo el PKCS#11 anteriormente, se volvera a instanciar.
        final String p11ProviderName = new File(p11lib).getName().replace('.', '_').replace(' ', '_');
        Provider p11Provider = Security.getProvider("SunPKCS11-" + p11ProviderName); //$NON-NLS-1$

        if (p11Provider == null) {

            Constructor<?> sunPKCS11Contructor;
            try {
                sunPKCS11Contructor = AOUtil.classForName("sun.security.pkcs11.SunPKCS11").getConstructor(InputStream.class); //$NON-NLS-1$
            }
            catch (final Exception e) {
                throw new MissingSunPKCS11Exception(e);
            }

            final byte[] config = KeyStoreUtilities.createPKCS11ConfigFile(p11lib, p11ProviderName, slot).getBytes();
            try {
                p11Provider = (Provider) sunPKCS11Contructor.newInstance(new ByteArrayInputStream(config));
            }
            catch (final Exception e) {
                // El PKCS#11 del DNIe a veces falla a la primera pero va
                // correctamente a la segunda
                // asi que reintentamos una vez mas
                try {
                    p11Provider = (Provider) sunPKCS11Contructor.newInstance(new ByteArrayInputStream(config));
                }
                catch (final Exception ex) {
                    throw new AOKeyStoreManagerException("No se ha podido instanciar el proveedor SunPKCS11 para la la biblioteca " + p11lib, ex); //$NON-NLS-1$
                }
            }
            Security.addProvider(p11Provider);
        }
        else {
            LOGGER.info("El proveedor SunPKCS11 solicitado ya estaba instanciado, se reutilizara esa instancia: " + p11Provider.getName()); //$NON-NLS-1$
        }

        try {
            this.ks = KeyStore.getInstance(this.ksType.getProviderName(), p11Provider);
        }
        catch (final Exception e) {
            Security.removeProvider(p11Provider.getName());
            p11Provider = null;
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen PKCS#11", e); //$NON-NLS-1$
        }

        try {
            this.ks.load(null, pssCallBack != null ? pssCallBack.getPassword() : null);
        }
        catch (final IOException e) {
            if (e.getCause() instanceof UnrecoverableKeyException ||
                    e.getCause() instanceof BadPaddingException) {
                throw new IOException("Contrasena invalida: " + e, e); //$NON-NLS-1$
            }
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen PKCS#11 solicitado", e); //$NON-NLS-1$
        }
        catch (final CertificateException e) {
            Security.removeProvider(p11Provider.getName());
            p11Provider = null;
            throw new AOKeyStoreManagerException("No se han podido cargar los certificados del almacen PKCS#11 solicitado", e); //$NON-NLS-1$
        }
        catch (final NoSuchAlgorithmException e) {
            Security.removeProvider(p11Provider.getName());
            p11Provider = null;
            throw new AOKeyStoreManagerException("No se ha podido verificar la integridad del almacen PKCS#11 solicitado", e); //$NON-NLS-1$
		}
        final List<KeyStore> ret = new ArrayList<KeyStore>(1);
        ret.add(this.ks);
        return ret;
    }

    private List<KeyStore> initSingle(final InputStream store,
            					      final PasswordCallback pssCallBack) throws AOKeyStoreManagerException,
                                                                                 IOException {
        if (store == null) {
            throw new AOKeyStoreManagerException("Es necesario proporcionar el fichero X.509 o PKCS#7"); //$NON-NLS-1$
        }

        final Provider pkcs7Provider;
        try {
            pkcs7Provider = (Provider) AOUtil.classForName("es.gob.afirma.keystores.single.SingleCertKeyStoreProvider").newInstance(); //$NON-NLS-1$
        }
        catch(final Exception e) {
            throw new MissingLibraryException("No se ha podido instanciar el proveedor SingleCertKeyStoreProvider: " + e, e); //$NON-NLS-1$
        }
        Security.addProvider(pkcs7Provider);

        try {
            this.ks = KeyStore.getInstance(this.ksType.getProviderName(), pkcs7Provider);
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen PKCS#7 / X.509", e); //$NON-NLS-1$
        }

        try {
            this.ks.load(store, pssCallBack != null ? pssCallBack.getPassword() : null);
        }
        catch (final IOException e) {
            if (e.getCause() instanceof UnrecoverableKeyException ||
                    e.getCause() instanceof BadPaddingException) {
                throw new IOException("Contrasena invalida: " + e, e); //$NON-NLS-1$
            }
            throw new AOKeyStoreManagerException("No se ha podido abrir el almacen PKCS#7 / X.509 solicitado", e); //$NON-NLS-1$
        }
        catch (final CertificateException e) {
            throw new AOKeyStoreManagerException("No se han podido cargar los certificados del almacen PKCS#7 / X.509 solicitado", e); //$NON-NLS-1$
        }
        catch (final NoSuchAlgorithmException e) {
        	throw new AOKeyStoreManagerException("No se ha podido verificar la integridad del almacen PKCS#7 / X.509 solicitado", e); //$NON-NLS-1$
		}
        final List<KeyStore> ret = new ArrayList<KeyStore>(1);
        ret.add(this.ks);
        try {
            store.close();
        }
        catch (final Exception e) {
            // Ignoramos errores en el cierre
        }
        return ret;


    }

    private List<KeyStore> initJava(final InputStream store,
                            final PasswordCallback pssCallBack) throws AOKeyStoreManagerException,
                                                                       IOException {
        // Suponemos que el proveedor SunJSSE esta instalado. Hay que tener
        // cuidado con esto
        // si alguna vez se usa JSS, que a veces lo retira
        if (store == null) {
            throw new IOException("Es necesario proporcionar el fichero KeyStore"); //$NON-NLS-1$
        }

        try {
            this.ks = KeyStore.getInstance(this.ksType.getProviderName());
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen JavaKeyStore", e); //$NON-NLS-1$
        }

        // TODO: Revisar si el KeyStore de Java requiere contrasena
        try {
            this.ks.load(store, pssCallBack != null ? pssCallBack.getPassword() : null);
        }
        catch (final IOException e) {
            if (e.getCause() instanceof UnrecoverableKeyException ||
                    e.getCause() instanceof BadPaddingException) {
                throw new IOException("Contrasena invalida: " + e, e); //$NON-NLS-1$
            }
        }
        catch (final CertificateException e) {
            throw new AOKeyStoreManagerException("No se han podido cargar los certificados del almacen JavaKeyStore solicitado", e); //$NON-NLS-1$
        }
        catch (final NoSuchAlgorithmException e) {
            throw new AOKeyStoreManagerException("No se ha podido verificar la integridad del almacen JavaKeyStore solicitado", e); //$NON-NLS-1$
		}
        final List<KeyStore> ret = new ArrayList<KeyStore>(1);
        ret.add(this.ks);
        try {
            store.close();
        }
        catch (final Exception e) {
         // Ignoramos errores en el cierre
        }
        return ret;

    }

    private List<KeyStore> initCAPI() throws AOKeyStoreManagerException,
                                             IOException {
        if (!Platform.getOS().equals(Platform.OS.WINDOWS)) {
            throw new InvalidOSException("Microsoft Windows"); //$NON-NLS-1$
        }

        // Si no se ha agregado el proveedor CAPI de Sun, lo anadimos
        // En java 6 viene instalado de serie, pero no pasa nada por
        // reinstalarlo
        if (sunMSCAPIProvider == null && Security.getProvider("SunMSCAPI") == null) { //$NON-NLS-1$
            try {
                sunMSCAPIProvider = (Provider) AOUtil.classForName("sun.security.mscapi.SunMSCAPI").newInstance(); //$NON-NLS-1$
                Security.addProvider(sunMSCAPIProvider);
            }
            catch(final Exception e) {
            	throw new MissingSunMSCAPIException(e);
            }
        }

        // Inicializamos
        try {
            this.ks = KeyStore.getInstance(this.ksType.getProviderName());
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen SunMSCAPI.MY", e); //$NON-NLS-1$
        }

        LOGGER.info("Cargando KeyStore de Windows"); //$NON-NLS-1$
        try {
            this.ks.load(null, null);
        }
        catch (final CertificateException e) {
            throw new AOKeyStoreManagerException("No se han podido cargar los certificados del almacen SunMSCAPI.MY", e); //$NON-NLS-1$
        }
        catch (final NoSuchAlgorithmException e) {
        	throw new AOKeyStoreManagerException("No se ha podido verificar la integridad del almacen SunMSCAPI.MY", e); //$NON-NLS-1$
		}

        // Tratamos los alias repetidos, situacion problematica afectada por el bug
        // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6483657
        // Este solo se da con SunMSCAPI
        try {
            KeyStoreUtilities.cleanCAPIDuplicateAliases(this.ks);
        }
        catch (final Exception e) {
            LOGGER.warning("No se han podido tratar los alias duplicados: " + e); //$NON-NLS-1$
        }

        final List<KeyStore> ret = new ArrayList<KeyStore>(1);
        ret.add(this.ks);
        return ret;

    }

    private List<KeyStore> initCAPIAddressBook() throws AOKeyStoreManagerException {
        if (!Platform.getOS().equals(Platform.OS.WINDOWS)) {
            throw new InvalidOSException("Microsoft Windows"); //$NON-NLS-1$
        }

        // Nos aseguramos de que SunMSCAPI este cargado, para que la DLL
        // sunmscapi.dll tambien lo este
        if (Security.getProvider("SunMSCAPI") == null) { //$NON-NLS-1$
            try {
                Security.addProvider((Provider) AOUtil.classForName("sun.security.mscapi.SunMSCAPI").newInstance()); //$NON-NLS-1$
            }
            catch (final Exception e) {
                throw new MissingSunMSCAPIException(e);
            }
        }
        Provider p = Security.getProvider("MSCAPIAddressBook"); //$NON-NLS-1$
        if (p == null) {
            try {
                p = (Provider) AOUtil.classForName("es.gob.afirma.keystores.capiaddressbook.MSCAPIAddressBook").newInstance(); //$NON-NLS-1$
            }
            catch (final Exception e) {
                throw new MissingLibraryException("No se ha podido instanciar el proveedor MSCAPIAddressBook", e); //$NON-NLS-1$
            }
            Security.addProvider(p);
        }

        try {
            this.ks = KeyStore.getInstance(this.ksType.getProviderName(), p);
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen MSCAPIAddressBook.ADDRESSBOOK", e);  //$NON-NLS-1$
        }

        try {
            this.ks.load(null, null);
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido abrir el almacen MSCAPIAddressBook.ADDRESSBOOK", e); //$NON-NLS-1$
        }

        final List<KeyStore> ret = new ArrayList<KeyStore>(1);
        ret.add(this.ks);
        return ret;
    }

    private List<KeyStore> initApple(final InputStream store) throws AOKeyStoreManagerException,
                                          							 IOException {
    	if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
    		throw new InvalidOSException("Apple Mac OS X"); //$NON-NLS-1$
    	}

        // Inicializamos
        try {
            this.ks = KeyStore.getInstance(this.ksType.getProviderName());
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen Apple.KeychainStore", e); //$NON-NLS-1$
        }

        try {
            this.ks.load(store, null);
        }
        catch (final CertificateException e) {
            throw new AOKeyStoreManagerException("No se han podido cargar los certificados del almacen Apple.KeychainStore", e); //$NON-NLS-1$
        }
        catch (final NoSuchAlgorithmException e) {
            throw new AOKeyStoreManagerException("No se ha podido verificar la integridad del almacen Apple.KeychainStore", e); //$NON-NLS-1$
		}
        final List<KeyStore> ret = new ArrayList<KeyStore>(1);
        ret.add(this.ks);
        return ret;
    }

    private List<KeyStore> initDnieJava(final PasswordCallback pssCallBack, final Object parentComponent) throws AOKeyStoreManagerException, IOException {
    	final Provider p;
    	if (Security.getProvider(AOKeyStore.DNIEJAVA.getProviderName()) == null) {
    		try {
    			p = (Provider) AOUtil.classForName("es.gob.jmulticard.jse.provider.DnieProvider").newInstance(); //$NON-NLS-1$
    			Security.addProvider(p);
    		}
    		catch (final Exception e) {
    			throw new AOKeyStoreManagerException(
					"No se ha podido instanciar e instalar el proveedor 100% Java para DNIe de Afirma: " + e, //$NON-NLS-1$
					e
				);
    		}
    	}

    	try {
    		final Class<?> managerClass = AOUtil.classForName("es.gob.jmulticard.ui.passwordcallback.PasswordCallbackManager"); //$NON-NLS-1$
    		final Method setDialogOwnerFrameMethod = managerClass.getMethod("setDialogOwner", Component.class); //$NON-NLS-1$
    		setDialogOwnerFrameMethod.invoke(null, parentComponent);
    	}
    	catch (final Exception e) {
    		LOGGER.warning("No se ha podido establecer el componente padre para los dialogos del almacen: " + e); //$NON-NLS-1$
    	}

        // Inicializamos
        try {
            this.ks = KeyStore.getInstance(this.ksType.getProviderName());
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen DNIe 100% Java: " + e, e); //$NON-NLS-1$
        }

        LOGGER.info("Cargando KeyStore DNIe 100% Java"); //$NON-NLS-1$
        try {
			this.ks.load(null, pssCallBack == null ? null : pssCallBack.getPassword());
		}
        catch (final NoSuchAlgorithmException e) {
        	throw new AOKeyStoreManagerException("Error de algoritmo al obtener el almacen DNIe 100% Java: " + e, e);  //$NON-NLS-1$
		}
        catch (final CertificateException e) {
			throw new AOKeyStoreManagerException("Error de certificado al obtener el almacen DNIe 100% Java: " + e, e);  //$NON-NLS-1$
		}

        final List<KeyStore> ret = new ArrayList<KeyStore>(1);
        ret.add(this.ks);
        return ret;
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
     *        CallBack encargado de recuperar la contrasenya del
     *        Keystore
     * @param params
     *        Par&aacute;metros adicionales (dependen del tipo de almac&eacute;n)
     * @return Almac&eacute;n de claves solicitado (<b>ya inicializado</b>,
     *         pueden ser varios en el caso de Mozilla, el interno y los
     *         externos)
     * @throws AOKeyStoreManagerException
     *         Cuando ocurre cualquier problema durante la inicializaci&oacute;n
     * @throws IOException
     *         Se ha insertado una contrase&ntilde;a incorrecta para la apertura del
     *         almac&eacute;n de certificados.
     * @throws es.gob.afirma.core.MissingLibraryException Cuando faltan bibliotecas necesarias para la inicializaci&oacute;n
     * @throws es.gob.afirma.core.InvalidOSException Cuando se pide un almac&eacute;n disponible solo en un sistema operativo
     *                            distinto al actual */
    public List<KeyStore> init(final AOKeyStore type,
    		                   final InputStream store,
    		                   final PasswordCallback pssCallBack,
    		                   final Object[] params) throws AOKeyStoreManagerException,
    		                                                 IOException {
        if (type == null) {
            throw new IllegalArgumentException("Se ha solicitado inicializar un AOKeyStore nulo"); //$NON-NLS-1$
        }
        LOGGER.info("Inicializamos el almacen de tipo: " + type); //$NON-NLS-1$

        this.ksType = type;

        if (this.ksType.equals(AOKeyStore.SINGLE)) {
        	return initSingle(store, pssCallBack);
        }

        if (this.ksType.equals(AOKeyStore.DNIEJAVA)) {
        	return initDnieJava(pssCallBack, params != null && params.length > 0 ? params[0] : null);
        }

        else if (this.ksType.equals(AOKeyStore.JAVA) || this.ksType.equals(AOKeyStore.JAVACE) || this.ksType.equals(AOKeyStore.JCEKS)) {
        	return initJava(store, pssCallBack);
        }

        else if (this.ksType.equals(AOKeyStore.WINDOWS) || this.ksType.equals(AOKeyStore.WINROOT)) {
        	return initCAPI();
        }

        else if (this.ksType.equals(AOKeyStore.WINCA) || this.ksType.equals(AOKeyStore.WINADDRESSBOOK)) {
        	return initCAPIAddressBook();
        }

        else if (this.ksType.equals(AOKeyStore.PKCS11)) {
            // En el "params" debemos traer los parametros:
            // [0] - p11lib: Biblioteca PKCS#11, debe estar en el Path (Windows) o en el LD_LIBRARY_PATH (UNIX, Linux, Mac OS X)
            // [1] -desc: Descripcion del token PKCS#11 (opcional)
            // [2] -slot: Numero de lector de tarjeta (Sistema Operativo) [OPCIONAL]
            return initPKCS11(pssCallBack, params);
        }

        else if (this.ksType.equals(AOKeyStore.APPLE)) { // Por ahora no anadimos soporte a llaveros en ficheros sueltos
        	return initApple(store);
        }

        else if (this.ksType.equals(AOKeyStore.DNIE)) {
            return initPKCS11(pssCallBack != null ? pssCallBack : new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStoreManager.0"), null), new String[] { KeyStoreUtilities.getPKCS11DNIeLib(), "DNIe-Afirma" });  //$NON-NLS-1$//$NON-NLS-2$
        }

        throw new UnsupportedOperationException("Tipo de almacen no soportado"); //$NON-NLS-1$
    }

    /** Obtiene la clave privada de un certificado.
     * @param alias
     *        Alias del certificado
     * @param pssCallback
     *        <i>CallBback</i> para obtener la contrase&ntilde;a del
     *        certificado que contiene la clave
     * @return Clave privada del certificado correspondiente al alias
     * @throws KeyStoreException
     * 		   Cuando ocurren errores en el tratamiento del almac&eacute;n de claves
     * @throws NoSuchAlgorithmException
     * 		   Cuando no se puede identificar el algoritmo para la recuperaci&oacute;n de la clave.
     * @throws UnrecoverableEntryException
     * 		   Si la contrase&ntilde;a proporcionada no es v&aacute;lida para obtener la clave privada
     * @throws es.gob.afirma.core.AOCancelledOperationException
     * 		   Cuando el usuario cancela el proceso antes de que finalice
     */
    public KeyStore.PrivateKeyEntry getKeyEntry(final String alias,
    		                                    final PasswordCallback pssCallback) throws KeyStoreException,
    		                                                                               NoSuchAlgorithmException,
    		                                                                               UnrecoverableEntryException {
        if (this.ks == null) {
            throw new IllegalStateException("Se han pedido claves a un almacen no inicializado"); //$NON-NLS-1$
        }
        return (KeyStore.PrivateKeyEntry) this.ks.getEntry(alias, pssCallback != null ? new KeyStore.PasswordProtection(pssCallback.getPassword()) : null);
    }

    /** Obtiene el certificado correspondiente a una clave privada.
     * @param privateKeyEntry
     *        Clave privada del certificado
     * @return Certificado cuya clave privada es la indicada */
    public static X509Certificate getCertificate(final KeyStore.PrivateKeyEntry privateKeyEntry) {
        return (X509Certificate) privateKeyEntry.getCertificate();
    }

    /** Obtiene un certificado del keystore activo a partir de su alias.
     * @param alias
     *        Alias del certificado.
     * @return El certificado o {@code null} si no se pudo recuperar. */
    public X509Certificate getCertificate(final String alias) {
        if (alias == null) {
            LOGGER.warning("El alias del certificado es nulo, se devolvera null"); //$NON-NLS-1$
            return null;
        }

        if (this.ks == null) {
            LOGGER.warning("No se ha podido recuperar el certificado con alias '" + alias //$NON-NLS-1$
                                                      + "' porque el KeyStore no estaba inicializado, se devolvera null"); //$NON-NLS-1$
            return null;
        }

        Certificate cert = null;
        try {
            cert = this.ks.getCertificate(alias);
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido recuperar el certificado con alias '" + alias + "', se devolvera null: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
        }
        if (cert == null) {
            LOGGER.warning("No se ha podido recuperar el certificado con alias '" + alias + "', se devolvera null"); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
        }
        return (X509Certificate) cert;

    }

    /** Obtiene la cadena de certificaci&oacute;n correspondiente a una clave
     * privada.
     * @param privateKeyEntry
     *        Clave privada del certificado
     * @return Certificados de la cadena de certificaci&oacute;n. */
    public static X509Certificate[] getCertificateChain(final KeyStore.PrivateKeyEntry privateKeyEntry) {
        return (X509Certificate[]) privateKeyEntry.getCertificateChain();
    }

    /** Obtiene la cadena de certificaci&oacute;n de un certificado del keystore
     * activo a partir de su alias.
     * @param alias
     *        Alias del certificado.
     * @return Certificados de la cadena de certificaci&oacute;n o {@code null} si no se pudo recuperar. */
    public X509Certificate[] getCertificateChain(final String alias) {
        if (this.ks == null) {
            LOGGER.warning("El KeyStore actual no esta inicializado, por lo que no se pudo recuperar el certificado para el alias '" + alias + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
        }
        try {
            return (X509Certificate[]) this.ks.getCertificateChain(alias);
        }
        catch (final Exception e) {
            LOGGER.severe(
              "Error al obtener la cadena de certificados para el alias '" + alias //$NON-NLS-1$
                 + "', se devolvera una cadena vacia: " //$NON-NLS-1$
                 + e
            );
            return new X509Certificate[0];
        }
    }

    /** Obtiene todos los alias de los certificados del almac&eacute;n actual.
     * @return Todos los alias encontrados en el almac&eacute;n actual */
    public String[] getAliases() {

        if (this.ks == null) {
            throw new IllegalStateException("Se han pedido los alias de un almacen no inicializado"); //$NON-NLS-1$
        }

        LOGGER.info("Solicitando los alias al KeyStore (" + this.ks.getProvider() + ")"); //$NON-NLS-1$ //$NON-NLS-2$

        final Enumeration<String> aliases;
        try {
            aliases = this.ks.aliases();
        }
        catch (final Exception e) {
            LOGGER.severe("Error intentando obtener los alias del almacen de claves, se devolvera " + "una enumeracion vacia: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return new String[0];
        }

        String currAlias;
        final List<String> v = new ArrayList<String>();

        LOGGER.info("Componiendo el vector de alias"); //$NON-NLS-1$

        while (aliases.hasMoreElements()) {
            currAlias = aliases.nextElement().toString();
            v.add(currAlias);

            LOGGER.info("Alias: " + currAlias);	//TODO: Traza para ayudar a localizar los problemas relacionados con CLAUER //$NON-NLS-1$
        }

        return v.toArray(new String[0]);

    }

    /** Devuelve el <code>keyStore</code> en uso.
     * @return Almac&eacute;n de claves (<code>KeyStore</code>) actual */
    public List<KeyStore> getKeyStores() {
        final List<KeyStore> ret = new ArrayList<KeyStore>(1);
        ret.add(this.ks);
        return ret;
    }

    @Override
    public String toString() {
        final StringBuilder ret = new StringBuilder("Gestor de almacenes de claves"); //$NON-NLS-1$
        if (this.ksType != null) {
            String tmpStr = this.ksType.getName();
            if (tmpStr != null) {
                ret.append(" de tipo "); //$NON-NLS-1$
                ret.append(tmpStr);
            }
            tmpStr = this.ksType.getName();
            if (tmpStr != null) {
                ret.append(" con nombre "); //$NON-NLS-1$
                ret.append(tmpStr);
            }
            ret.append(" de clase "); //$NON-NLS-1$
            ret.append(this.ksType.toString());
        }
        return ret.toString();
    }
}
