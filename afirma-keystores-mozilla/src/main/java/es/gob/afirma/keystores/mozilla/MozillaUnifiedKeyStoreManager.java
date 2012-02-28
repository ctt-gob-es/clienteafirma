/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.Provider;
import java.security.Security;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.main.callbacks.UIPasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerException;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de
 * Firefox accedidos v&iacute;a NSS en el que se tratan de forma
 * unificada los m&oacute;dulos internos y externos. */
public final class MozillaUnifiedKeyStoreManager extends AOKeyStoreManager {

    private Map<String, KeyStore> storesByAlias;

    private final List<KeyStore> kss = new ArrayList<KeyStore>();

    /** Componente padre sobre el que montar los di&aacute;logos modales. */
    private Object parentComponent = null;

    /** PasswordCallback establecido de forma externa para el acceso al
     * almac&eacute;n. */
    private PasswordCallback externallPasswordCallback = null;

    /** Inicializa la clase gestora de almacenes de claves.
     * @return Almac&eacute;n de claves de Firefox correspondiente
     *         &uacute;nicamente el m&oacute;dulo interno principal
     * @throws AOKeyStoreManagerException
     *         Si no puede inicializarse ning&uacute;n almac&eacute;n de
     *         claves, ni el NSS interno, ni ning&uacute;n PKCS#11 externo
     *         definido en SecMod */
    @Override
    public List<KeyStore> init(final AOKeyStore type, final InputStream store, final PasswordCallback pssCallBack, final Object[] params) throws AOKeyStoreManagerException {

        // Por si el proveedor estubiese ya instalado por una ejecucion anterior
        // intentamos obtenerlo directamente
        setNSSProvider(Security.getProvider("SunPKCS11-NSSCrypto-AFirma")); //$NON-NLS-1$
        try {
            if (getNSSProvider() == null) {
                LOGGER.info("Inicializando almacen unificado de Firefox (NSS + modulos PKCS#11)"); //$NON-NLS-1$

                final String nssDirectory = MozillaKeyStoreUtilities.getSystemNSSLibDir();
                final String p11NSSConfigFile =
                    MozillaKeyStoreUtilities.createPKCS11NSSConfigFile(
                		MozillaKeyStoreUtilities.getMozillaUserProfileDirectory(),
                		nssDirectory
            		);

                // Cargamos las dependencias necesarias para la correcta carga
                // del almacen (en Mac se crean enlaces simbolicos)
                if (Platform.OS.MACOSX.equals(Platform.getOS())) {
                    MozillaKeyStoreUtilities.configureMacNSS(nssDirectory);
                }
                else {
                    MozillaKeyStoreUtilities.loadNSSDependencies(nssDirectory);
                }

                LOGGER.info("Configuracion de NSS para SunPKCS11:\n" + p11NSSConfigFile); //$NON-NLS-1$

                setNSSProvider(
                        (Provider) AOUtil.classForName("sun.security.pkcs11.SunPKCS11") //$NON-NLS-1$
                                        .getConstructor(InputStream.class)
                                        .newInstance(new ByteArrayInputStream(p11NSSConfigFile.getBytes()))
                );

                Security.addProvider(getNSSProvider());
                LOGGER.info("Proveedor PKCS#11 para Firefox anadido"); //$NON-NLS-1$
            }
        }
        catch (final Exception e) {
            LOGGER.severe("Error inicializando NSS, se continuara con los almacenes externos de Firefox, pero los certificados del almacen interno no estaran disponibles: " + e); //$NON-NLS-1$
        }

        Enumeration<String> tmpAlias = new Vector<String>(0).elements();
        this.storesByAlias = new Hashtable<String, KeyStore>();

        KeyStore ks = null;

        if (getNSSProvider() != null) {
            try {
                ks = KeyStore.getInstance("PKCS11", getNSSProvider()); //$NON-NLS-1$
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
        final Map<String, String> externalStores = MozillaKeyStoreUtilities.getMozillaPKCS11Modules();

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
        for (final String descr : externalStores.keySet()) {
            try {
                tmpStore = new AOKeyStoreManager().init(
                    AOKeyStore.PKCS11,
                    null,
                    new UIPasswordCallback(
                       FirefoxKeyStoreMessages.getString("MozillaUnifiedKeyStoreManager.1") + " " + MozillaKeyStoreUtilities.getMozModuleName(descr.toString()), //$NON-NLS-1$ //$NON-NLS-2$
                       this.parentComponent
                    ),
                    new String[] {
                       externalStores.get(descr), descr.toString()
                    }).get(0);
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
            throw new AOKeyStoreManagerException("No se ha podido inicializar ningun almacen, interno o externo, de Firefox"); //$NON-NLS-1$
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

    /** {@inheritDoc} */
    @Override
    public String[] getAliases() {
        if (this.kss == null) {
            LOGGER.warning("Se han pedido los alias de un almacen sin inicializar, se intentara inicializar primero"); //$NON-NLS-1$
            try {
                init(null, null, null, null);
            }
            catch (final Exception e) {
                LOGGER.severe("No se ha podido inicializar el almacen, se devolvera una lista de alias vacia: " + e); //$NON-NLS-1$
                return new String[0];
            }
        }
        final String[] tmpAlias = new String[this.storesByAlias.size()];
        int i = 0;
        for (final String al : this.storesByAlias.keySet()) {
            tmpAlias[i] = al;
            i++;
        }
        return tmpAlias.clone();
    }

    /** {@inheritDoc} */
    @Override
    public KeyStore.PrivateKeyEntry getKeyEntry(final String alias, final PasswordCallback pssCallback) {

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

    /** {@inheritDoc} */
    @Override
    public List<KeyStore> getKeyStores() {
        return this.kss;
    }

    /** {@inheritDoc} */
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
