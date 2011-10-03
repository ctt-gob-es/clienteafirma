/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.ciphers;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.Key;
import java.security.KeyStore;
import java.util.Enumeration;
import java.util.Vector;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Platform;


/** Utilidades para el manejo de claves de cifrado en el almac&eacute;n privado
 * de AFirma. */
public final class AOCipherKeyStoreHelper {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Almac&eacute;n de claves de cifrado. */
    private KeyStore ks;

    /** Interfaz para la inserci&oacute;n de la contrase&ntilde;a del
     * almac&eacute;n. */
    private final char[] pss;

    /** Almacena una clave en el almac&eacute;n privado de AFirma.
     * @param alias
     *        Alias con el que se almacenar&aacute; la clave
     * @param key
     *        Clave a almacenar
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso de
     *         almacenado */
    public void storeKey(final String alias, final Key key) throws AOException {
        if (this.ks == null) {
            throw new AOException("No se puede almacenar una clave en un almacen no inicializado"); //$NON-NLS-1$
        }
        try {
            this.ks.setKeyEntry(alias, key, this.pss, null);
        }
        catch (final Exception e) {
            throw new AOException("Error almacenando la clave en el almacen", e); //$NON-NLS-1$
        }
        try {
            this.ks.store(new BufferedOutputStream(new FileOutputStream(new File(getCipherKeystore()))), this.pss);
        }
        catch (final Exception e) {
            throw new AOException("Error guardando el almacen de claves", e); //$NON-NLS-1$
        }
    }

    /** Obtiene los alias de todas las claves del almac&eacute;n privado de
     * AFIrma.
     * @return Alias de todas las claves del almac&eacute;n */
    public String[] getAliases() {
        final Enumeration<String> aliases;
        try {
            aliases = this.ks.aliases();
        }
        catch (final Exception e) {
            LOGGER.severe("Error obteniendo los alias del almacen, se devolvera una lista vacia: " + e); //$NON-NLS-1$
            return new String[0];
        }
        final Vector<String> tmpRet = new Vector<String>();
        while (aliases.hasMoreElements()) {
            tmpRet.add(aliases.nextElement().toString());
        }
        return tmpRet.toArray(new String[0]);
    }

    /** Crea, si no existe ya, el almac&eacute;n de claves de cifrado en el
     * directorio del usuario activo.
     * @throws AOException
     *         Cuando se produce un error al crear el almac&eacute;n. */
    private void createCipherKeyStore() throws AOException {
        if (this.ks == null) {
            try {
                this.ks = KeyStore.getInstance("JCEKS"); //$NON-NLS-1$
            }
            catch (final Exception e) {
                throw new AOException("Error obteniendo una instancia de KeyStore JCE", e); //$NON-NLS-1$
            }
        }
        if (new File(getCipherKeystore()).exists()) {
            throw new AOException("Se ha pedido crear un almacen de claves, pero ya existia uno (" + getCipherKeystore() + ")"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        try {
            this.ks.load(null, this.pss);
        }
        catch (final Exception e) {
            throw new AOException("Error creando un KeyStore vacio", e); //$NON-NLS-1$
        }
        try {
            this.ks.store(new FileOutputStream(new File(getCipherKeystore())), this.pss);
        }
        catch (final Exception e) {
            throw new AOException("Error guardando en disco el KeyStore vacio", e); //$NON-NLS-1$
        }
        if (!new File(getCipherKeystore()).exists()) {
            throw new AOException("Se creo el KeyStore sin errores, pero este no aparece en el disco"); //$NON-NLS-1$
        }
    }

    /** Carga el almac&eacute;n privado de claves de cifrado de AFirma.
     * @throws AOException
     *         Cuando ocurre cualquier problema durante la carga.
     * @throws IOException
     *         Cuando se inserta una clave incorrecta. */
    public void loadCipherKeyStore() throws AOException, IOException {
        if (this.ks == null) {
            try {
                this.ks = KeyStore.getInstance("JCEKS"); //$NON-NLS-1$
            }
            catch (final Exception e) {
                throw new AOException("Error al instalanciar un almacen de claves JCEKS", e); //$NON-NLS-1$
            }
        }
        if (!new File(getCipherKeystore()).exists()) {
            LOGGER.warning("El almacen no existe, se creara uno nuevo"); //$NON-NLS-1$
            createCipherKeyStore();
        }
        final InputStream ksIs;
        try {
            ksIs = new FileInputStream(new File(getCipherKeystore()));
        }
        catch (final IOException e) {
            throw new AOException("Error al cargar el almacen de claves de cifrado", e); //$NON-NLS-1$
        }
        try {
            this.ks.load(new BufferedInputStream(ksIs), this.pss);
        }
        catch (final Exception e) {
            throw new AOException("Error al cargar el almacen de claves de cifrado", e); //$NON-NLS-1$
        }
        finally {
            try {
                ksIs.close();
            }
            catch (final Exception e) {
                // Ignoramos los errores en el cierre
            }
        }
    }

    /** Obtiene una clave del almac&eacute;n.
     * @param alias
     *        Alias de la clave solicitada
     * @return Clave de cifrado del alam&eacute;n privado de claves de cifrado
     *         de AFirma cuyo alias coincide con el proporcionado
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    public Key getKey(final String alias) throws AOException {
        try {
            return this.ks.getKey(alias, this.pss);
        }
        catch (final Exception e) {
            throw new AOException("Error recuperando la contrasena con alias '" + alias + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /** Crea un <code>AOCipherKeyStoreHelper</code>.
     * @param p
     *        Contrase&ntilde;a del almac&eacute;n de claves
     * @throws AOException
     *         Cuando ocurre cualquier problema durante la carga del
     *         almac&eacute;n.
     * @throws IOException
     *         Cuando la contrase&ntilde;a es incorrecta. */
    public AOCipherKeyStoreHelper(final char[] p) throws AOException, IOException {
        if (p == null) {
            throw new IllegalArgumentException("Se necesita una contrasena para instanciar la clase"); //$NON-NLS-1$
        }
        this.pss = p.clone();
        loadCipherKeyStore();
    }

    /** Indica si el almac&eacute;n privado de claves de cifrado de AFirma ha
     * sido ya creado.
     * @return <code>true</code> si el almac&eacute;n ya existe, <code>false</code> en caso contrario */
    public static boolean storeExists() {
        final File keystore = new File(getCipherKeystore());
        return keystore.exists() && keystore.isFile();
    }

    /** Elimina el almac&eacute;n de claves del usuario. No se realiza ninguna
     * consulta de verificaci&oacute;n de la orden.
     * @return Devuelve <code>true</code> si el keystore exist&iacute;a y se
     *         borr&oacute; correctamente. */
    public static boolean removeStore() {
        final File storeFile = new File(getCipherKeystore());
        return storeFile.exists() && storeFile.delete();
    }
    
    /** Obtiene la ruta absoluta del fichero de almac&eacute;n de las claves de
     * cifrado.
     * @return Ruta absoluta del fichero. */
    public final static String getCipherKeystore() {
        return Platform.getUserHome() + File.separator + "ciphkeys.jceks"; //$NON-NLS-1$
    }
}
