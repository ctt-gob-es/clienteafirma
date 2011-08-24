/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.keystores.mozilla;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.Provider;
import java.security.Security;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;
import es.gob.afirma.keystores.common.AOKeyStore;
import es.gob.afirma.keystores.common.AOKeyStoreManager;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de
 * Firefox accedidos v&iacute;a NSS en el que se tratan de forma
 * unificada los m&oacute;dulos internos y externos. */
public final class MozillaUnifiedKeyStoreManager extends AOKeyStoreManager {

    private Hashtable<String, KeyStore> storesByAlias;

    private final Vector<KeyStore> kss = new Vector<KeyStore>();

    /** Componente padre sobre el que montar los di&aacute;logos modales. */
    private Object parentComponent = null;

    /** PasswordCallback establecido de forma externa para el acceso al
     * almac&eacute;n. */
    private PasswordCallback externallPasswordCallback = null;

    /** Inicializa la clase gestora de almacenes de claves.
     * @return Almac&eacute;n de claves de Firefox correspondiente
     *         &uacute;nicamente el m&oacute;dulo interno principal
     * @throws AOException
     *         Si no puede inicializarse ning&uacute;n almac&eacute;n de
     *         claves, ni el NSS interno, ni ning&uacute;n PKCS#11 externo
     *         definido en SecMod */
    public Vector<KeyStore> init() throws AOException {

        // Por si el proveedor estubiese ya instalado por una ejecucion anterior
        // intentamos obtenerlo directamente
        this.nssProvider = Security.getProvider("SunPKCS11-NSSCrypto-AFirma"); //$NON-NLS-1$
        try {
            if (this.nssProvider == null) {
                LOGGER.info("Inicializando almacen unificado de Firefox (NSS + modulos PKCS#11)"); //$NON-NLS-1$

                final String nssDirectory = MozillaKeyStoreUtilities.getSystemNSSLibDir();
                final String p11NSSConfigFile =
                        MozillaKeyStoreUtilities.createPKCS11NSSConfigFile(MozillaKeyStoreUtilities.getMozillaUserProfileDirectory(), nssDirectory);

                // Cargamos las dependencias necesarias para la correcta carga
                // del almacen
                MozillaKeyStoreUtilities.loadNSSDependencies(nssDirectory);

                LOGGER.info("Configuracion de NSS para SunPKCS11:\n" + p11NSSConfigFile); //$NON-NLS-1$

                this.nssProvider =
                        (Provider) Class.forName("sun.security.pkcs11.SunPKCS11") //$NON-NLS-1$
                                        .getConstructor(InputStream.class)
                                        .newInstance(new ByteArrayInputStream(p11NSSConfigFile.getBytes()));

                Security.addProvider(this.nssProvider);
                LOGGER.info("Proveedor PKCS#11 para Firefox anadido"); //$NON-NLS-1$
            }
        }
        catch (final Exception e) {
            LOGGER.severe("Error inicializando NSS, se continuara con los almacenes externos de Firefox, pero los certificados del almacen interno no estaran disponibles: " + e); //$NON-NLS-1$
        }

        Enumeration<String> tmpAlias = new Vector<String>(0).elements();
        this.storesByAlias = new Hashtable<String, KeyStore>();

        KeyStore ks = null;

        if (this.nssProvider != null) {
            try {
                ks = KeyStore.getInstance("PKCS11", this.nssProvider); //$NON-NLS-1$
            }
            catch (final Exception e) {
                LOGGER.warning("No se ha podido obtener el KeyStore PKCS#11 NSS del proveedor SunPKCS11, se continuara con los almacenes externos: " + e); //$NON-NLS-1$
                ks = null;
            }
        }
        if (ks != null) {
            try {
                ks.load(null, new char[0]);
            }
            catch (final Exception e) {
                try {
                    ks.load(null, (this.externallPasswordCallback != null
                                                                    ? this.externallPasswordCallback.getPassword()
                                                                    : new UIPasswordCallback(FirefoxKeyStoreMessages.getString("MozillaUnifiedKeyStoreManager.0"), //$NON-NLS-1$
                                                                                             this.parentComponent).getPassword()));
                }
                catch (final AOCancelledOperationException e1) {
                    ks = null;
                    throw e1;
                }
                catch (final Exception e2) {
                    LOGGER.warning("No se ha podido abrir el almacen PKCS#11 NSS del proveedor SunPKCS11, se continuara con los almacenes externos: " + e2); //$NON-NLS-1$
                    ks = null;
                }
            }

            if (ks != null) {
                try {
                    tmpAlias = ks.aliases();
                }
                catch (final Exception e) {
                    LOGGER.warning("El almacen interno de Firefox no devolvio certificados, se continuara con los externos: " + e); //$NON-NLS-1$
                    ks = null;
                }
                while (tmpAlias.hasMoreElements()) {
                    this.storesByAlias.put(tmpAlias.nextElement().toString(), ks);
                }
            }
        }

        if (ks != null) {
            this.kss.add(ks);
        }

        // Vamos ahora con los almacenes externos
        final Hashtable<String, String> externalStores = MozillaKeyStoreUtilities.getMozillaPKCS11Modules();

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

        KeyStore tmpStore = null;
        Object descr;
        for (final Enumeration<String> e = externalStores.keys(); e.hasMoreElements();) {
            descr = e.nextElement();
            try {
                tmpStore =
                        new AOKeyStoreManager().init(AOKeyStore.PKCS11,
                                                     null,
                                                     new UIPasswordCallback(FirefoxKeyStoreMessages.getString("MozillaUnifiedKeyStoreManager.1") + " " + MozillaKeyStoreUtilities.getMozModuleName(descr.toString()), //$NON-NLS-1$ //$NON-NLS-2$
                                                                            this.parentComponent),
                                                     new String[] {
                                                             externalStores.get(descr), descr.toString()
                                                     })
                                               .get(0);
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

            if (ks == null) {
                ks = tmpStore;
            }

            tmpAlias = new Vector<String>(0).elements();
            try {
                tmpAlias = tmpStore.aliases();
            }
            catch (final Exception ex) {
                LOGGER.warning("Se encontro un error obteniendo los alias del almacen externo '" + descr + "', se continuara con el siguiente: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
                continue;
            }
            String alias;
            while (tmpAlias.hasMoreElements()) {
                alias = tmpAlias.nextElement().toString();
                this.storesByAlias.put(alias, tmpStore);
                LOGGER.info("Anadida la entrada '" + alias + "' del almacen externo '" + descr + "'"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            }
            this.kss.add(tmpStore);
        }

        if (this.kss.isEmpty()) {
            throw new AOException("No se ha podido inicializar ningun almacen, interno o externo, de Firefox"); //$NON-NLS-1$
        }

        return this.kss;
    }

    /** Establece la interfaz de entrada de la contrase&ntilde;a del
     * almac&eacute;n interno de Firefox. Si no se indica o se establece a <code>null</code> se utilizar&aacute; el por defecto.
     * @param externallPC
     *        Interfaz de entrada de contrase&ntilde;a. */
    public void setPasswordCallback(final PasswordCallback externallPC) {
        this.externallPasswordCallback = externallPC;
    }

    @Override
    public String[] getAliases() {
        if (this.kss == null) {
            LOGGER.warning("Se han pedido los alias de un almacen sin inicializar, se intentara inicializar primero"); //$NON-NLS-1$
            try {
                init();
            }
            catch (final Exception e) {
                LOGGER.severe("No se ha podido inicializar el almacen, se devolvera una lista de alias vacia: " + e); //$NON-NLS-1$
                return new String[0];
            }
        }
        final String[] tmpAlias = new String[this.storesByAlias.size()];
        int i = 0;
        for (final Enumeration<String> e = this.storesByAlias.keys(); e.hasMoreElements();) {
            tmpAlias[i] = e.nextElement().toString();
            i++;
        }
        return tmpAlias.clone();
    }

    @Override
    public KeyStore.PrivateKeyEntry getKeyEntry(final String alias, PasswordCallback pssCallback) throws AOCancelledOperationException {

        final KeyStore tmpStore = this.storesByAlias.get(alias);
        if (tmpStore == null) {
            throw new IllegalStateException("No hay ningun almacen de Firefox que contenga un certificado con el alias '" + alias + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        final KeyStore.PrivateKeyEntry keyEntry;
        try {
            keyEntry = (KeyStore.PrivateKeyEntry) tmpStore.getEntry(alias, new KeyStore.PasswordProtection((pssCallback != null) ? pssCallback.getPassword() : null));
        }
        catch (final AOCancelledOperationException e) {
            throw e;
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido obtener la clave privada del certicado '" + alias + "', se devolvera null: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
        }
        return keyEntry;
    }

    @Override
    public Vector<KeyStore> getKeyStores() {
        return this.kss;
    }

    @Override
    public String toString() {
        return "Almacen de claves de tipo Firefox unificado"; //$NON-NLS-1$
    }

    /** Obtiene un certificado del keystore activo a partir de su alias.
     * @param alias
     *        Alias del certificado.
     * @return Certificado. */
    @Override
    public X509Certificate getCertificate(final String alias) {
        if (this.kss == null) {
            LOGGER.warning("El KeyStore actual no esta inicializado, por lo que no se pudo recuperar el certificado '" + alias + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
        }
        for (final KeyStore ks : this.kss) {
            try {
                if (ks.containsAlias(alias)) {
                    return (X509Certificate) ks.getCertificate(alias);
                }
            }
            catch (final Exception e) {
                LOGGER.info("El KeyStore '" + ks + "' no contenia o no pudo recuperar el certificado '" + alias + "', se probara con el siguiente: " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            }
        }
        LOGGER.warning("Ningun KeyStore de Firefox contenia el certificado '" + alias + "', se devolvera null"); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
    }

    /** Establece el componente padre sobre el que mostrar los di&aacute;logos
     * modales para la inserci&oacute;n de contrase&ntilde;as.
     * @param parent
     *        Componente padre. */
    public void setParentComponent(final Object parent) {
        this.parentComponent = parent;
    }
}
