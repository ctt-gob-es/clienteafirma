/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.applet;

import java.awt.Component;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.security.Key;
import java.security.NoSuchAlgorithmException;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

/** Manejador de las funcionalidades de cifrado del Cliente @firma. */
public final class CipherManager {

    /** Componente sobre el que se mostrar&aacute;n los di6aaclogos modales. */
    private Component parent = null;

    private AOCipherConfig cipherConfig = null;

    /** Alias de la clave de cifrado que deseamos utilizar cuando el modo de
     * clave es 'KEYSTORE'. */
    private String cipherKeyAlias = null;

    /** Contrase&ntilde;a del almac&eacute;n de claves de cifrado. */
    private char[] cipherKeystorePass = null;

    /** Indica si se debe dar la posibilidad de que el usuario almacene una clave
     * generada autom&aacute;ticamente en su repositorio de claves. */
    private boolean useCipherKeyStore = true;

    /** Modo de generaci&oacute;n de clave */
    private String keyMode = AOConstants.DEFAULT_KEY_MODE;

    /** Clave para el cifrado y desencriptado en base 64 */
    private String cipherB64Key = null;

    /** Contrase&ntilde;a para el cifrado de datos. */
    private char[] cipherPassword = null;

    /** Datos planos que van a ser cifrados o que han sido descifrados */
    private byte[] plainData = null;

    /** Datos cifrados o preparados para ser descifrados */
    private byte[] cipheredData = null;

    /** Ruta a un fichero de entrada de datos. */
    private URI fileUri = null;

    /** Indica si el contenido del fichero introducido est&aacute; codificado en
     * Base 64. */
    private boolean fileBase64 = false;

    /** Construye el objeto. */
    public CipherManager() {
        this.cipherConfig = new AOCipherConfig(AOCipherAlgorithm.getDefault(), null, null);
    }

    /** Construye el objeto estableciendo un componente padre sobre el que
     * mostrar los di&aacute;logos modales.
     * @param parent
     *        Componente padre. */
    public CipherManager(final Component parent) {
        this();
        this.parent = parent;
    }

    /** Reinicia el componente a su configuraci&oacute;n por defecto. */
    public void initialize() {
        cipherConfig = new AOCipherConfig(AOCipherAlgorithm.getDefault(), null, null);
        cipherKeyAlias = null;
        cipherKeystorePass = null;
        useCipherKeyStore = true;
        keyMode = AOConstants.DEFAULT_KEY_MODE;
        cipherB64Key = null;
        cipherPassword = null;
        plainData = null;
        cipheredData = null;
        fileUri = null;
        fileBase64 = false;
    }

    /** Recupera la ruta de fichero configurada.
     * @return Ruta de fichero. */
    public URI getFileUri() {
        return fileUri;
    }

    /** Indica si el contenido del fichero configurado est&aacute; en base 64.
     * @return Devuelve {@code true} si el contenido del fichero es base 64, {@code false} en caso contrario. */
    public boolean isFileBase64() {
        return fileBase64;
    }

    /** Establece el fichero a utilizar.
     * @param fileUri
     *        Ruta del fichero.
     * @param fileBase64
     *        Indica si el contenido del fichero es base 64. */
    public void setFileUri(final URI fileUri, final boolean fileBase64) {
        this.fileUri = fileUri;
        this.fileBase64 = fileBase64;
    }

    /** Recupera el algoritmo de cifrado configurado.
     * @return Algoritmo de cifrado. */
    public AOCipherAlgorithm getCipherAlgorithm() {
        return cipherConfig.getAlgorithm();
    }

    /** Recupera la configuraci&oacute;n de cifrado: algoritmo/modo/padding. El
     * modo y el padding s&oacute;lo aparecer&aacute;n si ambos est&aacute;n
     * configurados.
     * @return Configuraci&olacute;n de cifrado. */
    public String getCipherAlgorithmConfiguration() {
        return cipherConfig.toString();
    }

    /** Configura el algoritmo de cifrado.
     * @param cipAlgo
     *        Algoritmo de cifrado. */
    public void setCipherAlgorithm(final AOCipherAlgorithm cipAlgo) {
        this.cipherConfig.setAlgorithm(cipAlgo == null ? AOCipherAlgorithm.getDefault() : cipAlgo);
    }

    /** Establece la configuraci&oacute;n de cifrado a partir de una cadena que
     * siga uno de los siguientes patrones: <list>
     * <ul>
     * Algoritmo/ModoBloque/Padding
     * </ul>
     * <ul>
     * Algoritmo
     * </ul>
     * </list> Si se introduce un nulo, se configurar&aacute; el algoritmo de
     * cifrado por defecto.<br/>
     * Si s&oacute;lo se especifica el algoritmo de cifrado, se tomar&aacute; el
     * modo y el padding configurados por defecto para ese algoritmo.
     * @param config
     *        Configuraci&oacute;n de cifrado.
     * @throws NoSuchAlgorithmException
     *         Cuando el algoritmo de la configuraci&oacute;n no est&aacute;
     *         soportado. */
    public void setCipherConfig(final String config) throws NoSuchAlgorithmException {
        // Si se introduce null o cadena vacia, eliminamos la configuracion
        // actual
        if (config == null || config.length() == 0) {
            this.setCipherConfig((AOCipherConfig) null);
        }
        else {
            this.cipherConfig = AOCipherConfig.parse(config);
        }
    }

    /** Establece la configuraci&oacute;n de cifrado. Si se introduce un nulo, se
     * configurar&aacute; el algoritmo de cifrado por defecto.<br/>
     * @param config
     *        Configuraci&oacute;n de cifrado. */
    public void setCipherConfig(final AOCipherConfig config) {

        if (config == null) {
            this.cipherConfig.setAlgorithm(AOCipherAlgorithm.getDefault());
            this.cipherConfig.setBlockMode(null);
            this.cipherConfig.setPadding(null);
        }
        else {
            this.cipherConfig = config;
        }
    }

    /** Recupera la configuraci&oacute;n de cifrado.
     * @return Configuraci&oacute;n de cifrado. */
    public AOCipherConfig getCipherConfig() {
        return this.cipherConfig;
    }

    /** Recupera el modo de bloque configurado para el cifrado.
     * @return Modo de bloque. */
    public AOCipherBlockMode getCipherBlockMode() {
        return this.cipherConfig.getBlockMode();
    }

    /** Establece el modo de bloque para el cifrado.
     * @param cipBlockMode
     *        Modo de bloque. */
    public void setCipherBlockMode(final AOCipherBlockMode cipBlockMode) {
        this.cipherConfig.setBlockMode(cipBlockMode);
    }

    /** Recupera el formato de padding para el cifrado.
     * @return Formato de padding. */
    public AOCipherPadding getCipherPadding() {
        return this.cipherConfig.getPadding();
    }

    /** Establece el formato de padding para el cifrado.
     * @param cipPadding
     *        Padding para el cifrado. */
    public void setCipherPadding(final AOCipherPadding cipPadding) {
        this.cipherConfig.setPadding(cipPadding);
    }

    /** Recupera el alias configurado del almac&eacute;n de claves de cifrado.
     * @return Alias configurado. */
    public String getCipherKeyAlias() {
        return cipherKeyAlias;
    }

    /** Establece el alias de la clave del almac&eacute;n de claves de cifrado
     * que se desea utilizar.
     * @param cipherKeyAlias
     *        Alias de la clave. */
    public void setCipherKeyAlias(final String cipherKeyAlias) {
        this.cipherKeyAlias = cipherKeyAlias;
    }

    /** Recupera la contrase&ntilde;a configurada para el almac&eacute;n de
     * claves.
     * @return Contrase&ntilda;a configurada. */
    public char[] getCipherKeystorePass() {
        return cipherKeystorePass.clone();
    }

    /** Establece la contrase&ntilde;a para la apertura del almac&eacute;n de
     * claves.
     * @param cipherKeystorePass
     *        Contrase&nmtilde;a del almac&eacute;n. */
    public void setCipherKeystorePass(final char[] cipherKeystorePass) {
        this.cipherKeystorePass = cipherKeystorePass.clone();
    }

    /** Indica si est&aacute; habilitado el almac&eacute;n de claves de cifrado
     * para el guardado de claves.
     * @return Devuelve {@code true} si est&aacute;a habilitado, {@code false} en caso contrario. */
    public boolean isUseCipherKeyStore() {
        return useCipherKeyStore;
    }

    /** Establece si debe utilizarse el almac&eacute;n de claves de cifrado para
     * el guardado de las nuevas claves generadas.
     * @param useCipherKeyStore
     *        Indica si debe usarse el almac&eacute;n. */
    public void setUseCipherKeyStore(final boolean useCipherKeyStore) {
        this.useCipherKeyStore = useCipherKeyStore;
    }

    /** Recupera el modo de clave configurado.
     * @return Modo de clave. */
    public String getKeyMode() {
        return keyMode;
    }

    /** Establece el modo de clave. Si se introduce un nulo, se
     * establecer&aacute;a el modo por defecto.
     * @param keyMode
     *        Modo de clave. */
    public void setKeyMode(final String keyMode) {
        this.keyMode = (keyMode == null ? AOConstants.DEFAULT_KEY_MODE : keyMode);
    }

    /** Recupera la clave de cifrado.
     * @return Clave de cifrado en base 64. */
    public String getCipherB64Key() {
        return cipherB64Key;
    }

    /** Establece la clave de cifrado.
     * @param cipherB64Key
     *        Clave de cifrado en base 64. */
    public void setCipherB64Key(final String cipherB64Key) {
        this.cipherB64Key = cipherB64Key;
    }

    /** recupera la contrase&ntilde;a de cifrado.
     * @return Contrasen&tilde;a de cifrado. */
    public char[] getCipherPassword() {
        return cipherPassword.clone();
    }

    /** Establece la contrase&ntilde;a de cifrado.
     * @param cipherPassword
     *        Contrase&ntilde;a de cifrado. */
    public void setCipherPassword(final char[] cipherPassword) {
        this.cipherPassword = cipherPassword.clone();
    }

    /** Indica si la contrase&ntilde;a introducida es v&aacute;lida para ser
     * usada para el cifrado de datos. Una contrase&ntilde;a nula se considera
     * no v&aacute;lida, pero si lo es una contrase&ntilde;a vac&iacute;a.
     * @param password
     *        Contrase&ntilde;a que queremos evaluar.
     * @return Devuelve {@code true} si la contrase&ntilde;a es v&aacute;lida. */
    public static boolean isValidPassword(final String password) {
        if (password == null) {
            return false;
        }
        for (final char c : password.toCharArray()) {
            if (c < 32 || c > 126) {
                return false;
            }
        }
        return true;
    }

    /** Recupera los datos planos para cifrado.
     * @return Datos para cifrar. */
    public byte[] getPlainData() {
        return plainData.clone();
    }

    /** Establece los datos planos para cifrar.
     * @param plainData
     *        Datos para cifrar. */
    public void setPlainData(final byte[] plainData) {
        this.plainData = plainData.clone();
    }

    /** Recupera los dato cifrados.
     * @return Datos cifrados. */
    public byte[] getCipheredData() {
        return cipheredData.clone();
    }

    /** Recupera en base 64 los datos cifrados.
     * @return Datos cifrados en base 64. */
    public String getCipheredDataB64Encoded() {
        return (cipheredData == null ? null : StringUtils.newStringUtf8(Base64.encodeBase64(cipheredData, false)));
    }

    /** Establece los datos cifrados para descifrar.
     * @param cipheredData
     *        Datos cifrados. */
    public void setCipheredData(final byte[] cipheredData) {
        this.cipheredData = cipheredData.clone();
    }

    /** Establece los datos cifrados para descifrar.
     * @param cipheredDataB64
     *        Datos cifrados en base 64. */
    public void setCipheredData(final String cipheredDataB64) {
        this.cipheredData = (cipheredDataB64 == null ? null : Base64.decodeBase64(cipheredDataB64));
    }

    /** Cifra los datos con la configuraci&oacute;n establecida.
     * @throws AOCancelledOperationException
     *         Operaci&oacute;n cancelada por el usuario.
     * @throws IllegalArgumentException
     *         Modo de clave no soportado.
     * @throws IOException
     *         No se han podido leer los datos a cifrar.
     * @throws NoSuchAlgorithmException
     *         Algoritmo de cifrado no soportado.
     * @throws AOException
     *         Ocurri&oacute; un error durante el proceso de cifrado. */
    public void cipherData() throws IOException, NoSuchAlgorithmException, AOException {

        byte[] dataToCipher = null;
        if (plainData != null) {
            dataToCipher = plainData;
        }
        else {

            // Fichero de entrada
            if (fileUri == null) {
                final String fileName = AOUIFactory.getLoadFileName(null, null, parent);
                try {
                    fileUri = AOUtil.createURI(fileName);
                }
                catch (final Exception e) {
                    throw new IOException("Se ha proporcionado un nombre de fichero no valido: " + e);
                }
            }

            // En este punto, tenemos la URI de los datos de entrada
            final InputStream is = AOUtil.loadFile(fileUri, parent, true, fileBase64);
            dataToCipher = AOUtil.getDataFromInputStream(is);
            try {
                is.close();
            }
            catch (final Exception e) {}
        }

        cipherData(dataToCipher);
    }

    /** Cifra los datos indicados aplicando la configuraci&oacute;n de cifrado
     * actual y establece el resultado en la configuraci&oacute;n del cliente.
     * @param dataToCipher
     *        Datos que se desean cifrar. en caso contrario.
     * @throws NoSuchAlgorithmException
     *         Algoritmo de cifrado no soportado.
     * @throws AOCancelledOperationException
     *         Operaci&oacute;n cancelada por el usuario.
     * @throws IllegalArgumentException
     *         Modo de clave no soportado.
     * @throws AOException
     *         Error durante el proceso de cifrado. */
    public void cipherData(final byte[] dataToCipher) throws NoSuchAlgorithmException, AOException {

        if (dataToCipher == null) {
            throw new IllegalArgumentException("Los datos a crifar no pueden ser nulos");
        }

        // Ya tenemos el stream con los datos, vamos a ver que Cipher uso
        final AOCipher cipher = new AOSunJCECipher();
        final Key cipherKey = getConfiguredKey(cipher, cipherConfig);

        // realizamos la operacion de cifrado
        cipheredData = cipher.cipher(dataToCipher, cipherConfig, cipherKey);
    }

    /** Obtiene una clave compatible para una configuraci&oacute;n de cifrado. La
     * clave puede haber sido insertada por el usuario, extra&iacute;da del
     * almac&eacute;n de claves...
     * @return Clave para el cifrado.
     * @throws NoSuchAlgorithmException
     *         Algoritmo de cifrado no soportado.
     * @throws AOException
     *         Ocurri&oacute; un error al obtener la clave. */
    public Key getConfiguredKey() throws NoSuchAlgorithmException, AOException {
        return this.getConfiguredKey(new AOSunJCECipher(), cipherConfig);
    }

    /** Obtiene una clave compatible para una configuraci&oacute;n de cifrado. La
     * clave puede haber sido insertada por el usuario, extra&iacute;da del
     * almac&eacute;n de claves...
     * @param cipher
     *        Manejador del proveedor de funciones de cifrado.
     * @param config
     *        Configuraci&oacute;n de cifrado.
     * @return Clave para el cifrado.
     * @throws NoSuchAlgorithmException
     *         Algoritmo de cifrado no soportado.
     * @throws AOCancelledOperationException
     *         Operaci&oacute;n cancelada por el usuario.
     * @throws IllegalArgumentException
     *         Modo de clave no soportado.
     * @throws AOException
     *         Ocurri&oacute; un error al obtener la clave. */
    private Key getConfiguredKey(final AOCipher cipher, final AOCipherConfig config) throws NoSuchAlgorithmException, AOException {

        Key cipherKey;

        // Tomamos o generamos la clave, segun nos indique el modo de clave.
        if (keyMode.equals(AOConstants.KEY_MODE_GENERATEKEY)) {
            cipherKey = cipher.generateKey(config);
            cipherB64Key = Base64.encodeBase64String(cipherKey.getEncoded());

            // Si se permite el almacenamiento de las claves, le damos la
            // posibilidad al usuario
            // Si se selecciona "Si" se almacenara la clave, si se selecciona
            // "No" no se almacenara
            // y si se selecciona "Cancelar" o se cierra el dialogo, se
            // cancelara toda la operacion
            // de cifrado.
            if (useCipherKeyStore) {
                try {
                    saveCipherKey(config, cipherKey);
                }
                catch (final AOMaxAttemptsExceededException e) {
                    JOptionPane.showMessageDialog(parent, AppletMessages.getString("SignApplet.43"), //$NON-NLS-1$
                                                  AppletMessages.getString("SignApplet.156"), //$NON-NLS-1$
                                                  JOptionPane.ERROR_MESSAGE);
                }
            }
        }
        else if (keyMode.equals(AOConstants.KEY_MODE_USERINPUT)) {

            /*
             * Cuando se selecciono introducir una clave: - Si el usuario la ha
             * introducido, se usa la clave configurada. - Si no la ha
             * introducido, se comprueba si se permite el uso del almacen de
             * claves de cifrado: - Si se permite, se comprueba que existe el
             * almacen - Si existe, se muestra el dialogo para acceder a el - Si
             * no existe, se indica que no se ha introducido ninguna clave para
             * el cifrado - Si no se permite, se indica que no se ha introducido
             * ninguna clave para el cifrado
             */
            if (cipherB64Key != null) {
                cipherKey = cipher.decodeKey(cipherB64Key, config, null);
            }
            else if (useCipherKeyStore && AOCipherKeyStoreHelper.storeExists()) {
                try {
                    cipherKey = getKeyFromCipherKeyStore();
                }
                catch (final AOCancelledOperationException e) {
                    throw e;
                }
                catch (final Exception e) {
                    throw new AOException("Error al extraer una clave del almacen de clave de cifrado", e);
                }
            }
            else {
                throw new AOException("No se ha establecido una clave de cifrado");
            }
        }
        else if (keyMode.equals(AOConstants.KEY_MODE_PASSWORD)) {
            if (cipherPassword == null || cipherPassword.length == 0) {
                cipherPassword = AOUIFactory.getPassword(AppletMessages.getString("SignApplet.414"), AOConstants.ACCEPTED_CHARS, true, parent); //$NON-NLS-1$
            }
            cipherKey = cipher.decodePassphrase(cipherPassword, config, null);
        }
        else {
            throw new IllegalArgumentException("Modo de clave no soportado");
        }

        return cipherKey;
    }

    /** Descifra los datos con la configuraci&oacute;n establecida.
     * @throws AOCancelledOperationException
     *         Operaci&oacute;n cancelada por el usuario.
     * @throws IOException
     *         Si se han pueden leer los datos a cifrar.
     * @throws NoSuchAlgorithmException
     *         Si el algoritmo de cifrado no est&aacute; soportado.
     * @throws AOException
     *         Si ocurre alg&uacute; error durante el proceso de cifrado.
     * @throws AOInvalidKeyException
     *         Si la clave de descifrado no es v&aacute;lida */
    public void decipherData() throws IOException, AOException, AOInvalidKeyException {

        byte[] dataToDecipher = null;
        if (cipheredData != null) {
            dataToDecipher = cipheredData;
        }
        else {

            // Si no hay una informacion cofrada establecida, la tratamos de
            // leer desde fichero
            if (fileUri == null) {
                final String fileName = AOUIFactory.getLoadFileName(null, null, parent);
                try {
                    fileUri = AOUtil.createURI(fileName);
                }
                catch (final Exception e) {
                    throw new IOException("Se ha proporcionado un nombre de fichero no valido: " + e);
                }
            }

            // En este punto, tenemos la URI de los datos de entrada
            final InputStream is = AOUtil.loadFile(fileUri, parent, true, fileBase64);
            dataToDecipher = AOUtil.getDataFromInputStream(is);
            try {
                is.close();
            }
            catch (final Exception e) {}
        }

        decipherData(dataToDecipher);
    }

    /** Descifra los datos indicados aplicando la configuraci&oacute;n de cifrado
     * actual y establece el resultado en la configuraci&oacute;n del cliente.
     * @param dataToDecipher
     *        Datos que se desean descifrar.
     * @throws AOCancelledOperationException
     *         Cuando el usuario cancela la operaci&oacute;n.
     * @throws AOException
     *         Cuando ocurre un error durante el desencriptado.
     * @throws AOInvalidKeyException
     *         Cuando se proporciona una clave incorrecta. */
    public void decipherData(final byte[] dataToDecipher) throws AOException, AOInvalidKeyException {

        // Si no esta establecido el algoritmo de cifrado usamos el por
        // defecto, pero solo para esta ocasion
        final AOCipher decipher = new AOSunJCECipher();
        Key decipherKey = null;

        // Si el modo de clave es por password, generamos la clave a partir de
        // el.
        // En caso contrario, requeriremos que nos den la clave
        if (keyMode.equals(AOConstants.KEY_MODE_PASSWORD)) {
            if (cipherPassword == null || cipherPassword.length == 0) {
                cipherPassword = AOUIFactory.getPassword(AppletMessages.getString("SignApplet.414"), parent); //$NON-NLS-1$
            }
            decipherKey = decipher.decodePassphrase(cipherPassword, cipherConfig, null);
        }
        else {
            // En el caso de trabajar con claves, si no se indico cual debe
            // usarse,
            // se ofrecera al usuario la posibilidad de tomar una del almacen de
            // claves
            // de cifrado. Si no existe el almacen, se le indica que es
            // obligatorio
            // introducir la clave.
            if (cipherB64Key == null) {
                if (AOCipherKeyStoreHelper.storeExists()) {
                    try {
                        decipherKey = getKeyFromCipherKeyStore();
                    }
                    catch (final AOCancelledOperationException e) {
                        throw e;
                    }
                    catch (final AOException e) {
                        throw e;
                    }
                    catch (final Exception e) {
                        throw new AOException("Error al extraer una clave del almacen de clave de cifrado", e); //$NON-NLS-1$
                    }
                }
                else {
                    throw new AOException("No se ha indicado la clave para el descifrado de datos y no hay almacen de claves"); //$NON-NLS-1$
                }
            }
            else {
                decipherKey = decipher.decodeKey(cipherB64Key, cipherConfig, null);
            }
        }

        // Realizamos la operacion de desencriptado
        plainData = decipher.decipher(dataToDecipher, cipherConfig, decipherKey);
    }

    /** Obtiene una clave de cifrado a partir de una clave del almac&eacute;n del
     * usuario. Si no se ha establecido la contrase&ntilde;a del almac&eacute;n
     * y/o no se ha seleccionado un alias, se solicita al usuario mediante una
     * ventana modal.
     * @return Clave de cifrado/descifrado.
     * @throws AOException
     *         Ocurri&oacute; un error durate el proceso de
     *         configuraci&oacute;n. */
    private Key getKeyFromCipherKeyStore() throws AOException {
        // Si el almacen no existe devolvemos un error
        if (!AOCipherKeyStoreHelper.storeExists()) {
            throw new AOException("No existe un almacen de claves de cifrado asociado al usuario"); //$NON-NLS-1$
        }
        // Abrimos el Almacen de claves de cifrado preguntandole al usuario la
        // clave si no
        // la indico
        AOCipherKeyStoreHelper cKs = null;
        try {
            cKs =
                    new AOCipherKeyStoreHelper(cipherKeystorePass != null
                                                                         ? cipherKeystorePass
                                                                         : AOUIFactory.getPassword(AppletMessages.getString("SignApplet.52"), parent) //$NON-NLS-1$
                    );
        }
        catch (final AOCancelledOperationException e) {
            throw e;
        }
        catch (final Exception e) {
            throw new AOException("Error al abrir el repositorio de claves del usuario", e); //$NON-NLS-1$
        }

        // Si no se establecio el alias de la clave de cifrado, se la pedimos al
        // usuario
        String alias = null;
        if (cipherKeyAlias == null) {
            try {
                alias = AOUIManager.showCertSelectionDialog(cKs.getAliases(), // aliases
                                                            null, // KeyStores
                                                            parent, // parent
                                                            true, // comprobar claves privadas
                                                            true, // comprobar validez
                                                            true // mostrar caducados
                );
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOException("Error seleccionar la clave de cifrado", e); //$NON-NLS-1$
            }
        }
        else {
            alias = cipherKeyAlias;
        }

        return cKs.getKey(alias);
    }

    /** Guarda una clave de cifrado en el repositorio de claves del usuario.
     * @param config
     *        Configuraci&oacute;n de la clave que se desea almacenar.
     * @param cipherKey
     *        Clave de cifrado que se desea almacenar. <code>false</code>
     * @throws AOCancelledOperationException
     *         Operacion cancelada por el usuario.
     * @throws AOMaxAttemptsExceededException
     *         Cuando se supera el n&uacute;mero m&aacute;ximo de intentos
     *         fallidos al almac&eacute;n.
     * @throws AOException
     *         Error al almacenar la clave de cifrado. */
    private void saveCipherKey(final AOCipherConfig config, final Key cipherKey) throws AOMaxAttemptsExceededException, AOException {
        // Preguntamos si se desea almacenar en el almacen de claves de cifrado
        // y si se acepta y no existe este almacen, lo creamos
        final int selectedOption = JOptionPane.showConfirmDialog(parent, AppletMessages.getString("SignApplet.40"), //$NON-NLS-1$
                                                           AppletMessages.getString("SignApplet.41"), //$NON-NLS-1$
                                                           JOptionPane.YES_NO_CANCEL_OPTION);

        // Si se pulsa Cancelar o se cierra el dialogo, se cancela toda la
        // operacion de cifrado
        if (selectedOption == JOptionPane.CANCEL_OPTION || selectedOption == JOptionPane.CLOSED_OPTION) {
            throw new AOCancelledOperationException("Se ha cancelado el guardado de la clave de cifrado"); //$NON-NLS-1$
        }
        else if (selectedOption == JOptionPane.YES_OPTION) {

            // Controlamos un maximo de 3 intentos para abrir el almacen cuando
            // no se establecio la contrasena
            AOCipherKeyStoreHelper cKs = null;
            if (cipherKeystorePass != null) {
                try {
                    cKs = new AOCipherKeyStoreHelper(cipherKeystorePass);
                }
                catch (final IOException e) {
                    throw new AOException("La contrasena del almacen de claves de cifrado no es valida", e);
                }
            }
            else {
                int numTries = 0;
                do {
                    numTries++;
                    try {
                        cKs = new AOCipherKeyStoreHelper(new UIPasswordCallback(AppletMessages.getString("SignApplet.42"), parent).getPassword());
                    }
                    catch (final IOException e) {
                        if (numTries >= 3) {
                            throw new AOMaxAttemptsExceededException("Se ha sobrepasado el numero maximo de intentos en la insercion de la clave del almacen");
                        }
                    }
                } while (cKs == null);
            }

            String alias = cipherKeyAlias;
            if (alias == null) {
                try {
                    alias = JOptionPane.showInputDialog(parent, AppletMessages.getString("SignApplet.46"), //$NON-NLS-1$
                                                        AppletMessages.getString("SignApplet.47"), //$NON-NLS-1$
                                                        JOptionPane.QUESTION_MESSAGE);
                }
                catch (final Exception e) {
                    throw new AOException("Error al almacenar la clave de cifrado, la clave quedara sin almacenar");
                }
                alias += " (" + config.toString() + ")"; //$NON-NLS-1$ //$NON-NLS-2$
            }

            cKs.storeKey(alias, cipherKey);
        }
    }
}
