/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.awt.Component;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.security.Key;
import java.security.KeyException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;

import javax.swing.JOptionPane;

import es.gob.afirma.ciphers.AOCipherConstants;
import es.gob.afirma.ciphers.AOCipherKeyStoreHelper;
import es.gob.afirma.ciphers.jce.AOSunJCECipher;
import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipher;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

/** Manejador de las funcionalidades de cifrado del Cliente @firma. */
final class CipherManager {

    /** Caracteres ASCII validos para la contrase&ntilde;a de cifrado. */
	private static final String ACCEPTED_CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"; //$NON-NLS-1$

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
    private String keyMode = AOCipherConstants.DEFAULT_KEY_MODE;

    /** Clave para el cifrado y desencriptado en base 64 */
    private byte[] cipherKeyEncoded = null;

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
    CipherManager() {
        this.cipherConfig = new AOCipherConfig(AOCipherAlgorithm.getDefault(), null, null);
    }

    /** Construye el objeto estableciendo un componente padre sobre el que
     * mostrar los di&aacute;logos modales.
     * @param parent
     *        Componente padre. */
    CipherManager(final Component parent) {
        this();
        this.parent = parent;
    }

    /** Reinicia el componente a su configuraci&oacute;n por defecto. */
    void initialize() {
        this.cipherConfig = new AOCipherConfig(AOCipherAlgorithm.getDefault(), null, null);
        this.cipherKeyAlias = null;
        this.cipherKeystorePass = null;
        this.useCipherKeyStore = true;
        this.keyMode = AOCipherConstants.DEFAULT_KEY_MODE;
        this.cipherKeyEncoded = null;
        this.cipherPassword = null;
        this.plainData = null;
        this.cipheredData = null;
        this.fileUri = null;
        this.fileBase64 = false;
    }

    /** Recupera la ruta de fichero configurada.
     * @return Ruta de fichero. */
    URI getFileUri() {
        return this.fileUri;
    }

    /** Indica si el contenido del fichero configurado est&aacute; en base 64.
     * @return Devuelve {@code true} si el contenido del fichero es base 64, {@code false} en caso contrario. */
    boolean isFileBase64() {
        return this.fileBase64;
    }

    /** Establece el fichero a utilizar.
     * @param fileUri
     *        Ruta del fichero.
     * @param fileBase64
     *        Indica si el contenido del fichero es base 64. */
    void setFileUri(final URI fileUri, final boolean fileBase64) {
        this.fileUri = fileUri;
        this.fileBase64 = fileBase64;
    }

    /** Recupera el algoritmo de cifrado configurado.
     * @return Algoritmo de cifrado. */
    AOCipherAlgorithm getCipherAlgorithm() {
        return this.cipherConfig.getAlgorithm();
    }

    /** Configura el algoritmo de cifrado.
     * @param cipAlgo
     *        Algoritmo de cifrado. */
    void setCipherAlgorithm(final AOCipherAlgorithm cipAlgo) {
        this.cipherConfig = new AOCipherConfig(cipAlgo, null, null);
    }

    /** Establece la configuraci&oacute;n de cifrado a partir de una cadena que
     * siga uno de los siguientes patrones:
     * <ul>
     *  <li>Algoritmo/ModoBloque/Padding</li>
     *  <li>Algoritmo</li>
     * </ul>
     * Si se introduce un nulo, se configurar&aacute; el algoritmo de
     * cifrado por defecto.<br>
     * Si s&oacute;lo se especifica el algoritmo de cifrado, se tomar&aacute; el
     * modo y el padding configurados por defecto para ese algoritmo.
     * @param config Configuraci&oacute;n de cifrado.
     * @throws NoSuchAlgorithmException Cuando el algoritmo de la configuraci&oacute;n no est&aacute;
     *                                  soportado. */
    void setCipherConfig(final String config) throws NoSuchAlgorithmException {
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
     * configurar&aacute; el algoritmo de cifrado por defecto.
     * @param config Configuraci&oacute;n de cifrado. */
    void setCipherConfig(final AOCipherConfig config) {

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
    AOCipherConfig getCipherConfig() {
        return this.cipherConfig;
    }

    /** Recupera el alias configurado del almac&eacute;n de claves de cifrado.
     * @return Alias configurado. */
    String getCipherKeyAlias() {
        return this.cipherKeyAlias;
    }

    /** Establece el alias de la clave del almac&eacute;n de claves de cifrado
     * que se desea utilizar.
     * @param cipherKeyAlias
     *        Alias de la clave. */
    void setCipherKeyAlias(final String cipherKeyAlias) {
        this.cipherKeyAlias = cipherKeyAlias;
    }

    /** Recupera la contrase&ntilde;a configurada para el almac&eacute;n de
     * claves.
     * @return Contrase&ntilde;a configurada. */
    char[] getCipherKeystorePass() {
        return this.cipherKeystorePass == null ? null : this.cipherKeystorePass.clone();
    }

    /** Establece la contrase&ntilde;a para la apertura del almac&eacute;n de
     * claves.
     * @param cipherKeystorePass Contrase&ntilde;a del almac&eacute;n. */
    void setCipherKeystorePass(final char[] cipherKeystorePass) {
        this.cipherKeystorePass = cipherKeystorePass == null ? null : cipherKeystorePass.clone();
    }

    /** Indica si est&aacute; habilitado el almac&eacute;n de claves de cifrado
     * para el guardado de claves.
     * @return Devuelve {@code true} si est&aacute;a habilitado, {@code false} en caso contrario. */
    boolean isUseCipherKeyStore() {
        return this.useCipherKeyStore;
    }

    /** Establece si debe utilizarse el almac&eacute;n de claves de cifrado para
     * el guardado de las nuevas claves generadas.
     * @param useCipherKeyStore
     *        Indica si debe usarse el almac&eacute;n. */
    void setUseCipherKeyStore(final boolean useCipherKeyStore) {
        this.useCipherKeyStore = useCipherKeyStore;
    }

    /** Recupera el modo de clave configurado.
     * @return Modo de clave. */
    String getKeyMode() {
        return this.keyMode;
    }

    /** Establece el modo de clave. Si se introduce un nulo, se
     * establecer&aacute;a el modo por defecto.
     * @param keyMode
     *        Modo de clave. */
    void setKeyMode(final String keyMode) {
        this.keyMode = keyMode == null ? AOCipherConstants.DEFAULT_KEY_MODE : keyMode;
    }

    /** Recupera la clave de cifrado.
     * @return Clave de cifrado. */
    byte[] getCipherKey() {
        return this.cipherKeyEncoded;
    }

    /** Establece la clave de cifrado.
     * @param cipherKey
     *        Clave de cifrado */
    void setCipherKey(final byte[] cipherKey) {
        this.cipherKeyEncoded = cipherKey == null ? null : cipherKey.clone();
    }

    /** recupera la contrase&ntilde;a de cifrado.
     * @return Contrasen&tilde;a de cifrado. */
    char[] getCipherPassword() {
        return this.cipherPassword == null ? null : this.cipherPassword.clone();
    }

    /** Establece la contrase&ntilde;a de cifrado.
     * @param cipherPassword
     *        Contrase&ntilde;a de cifrado. */
    void setCipherPassword(final char[] cipherPassword) {
        this.cipherPassword = cipherPassword == null ? null : cipherPassword.clone();
    }

    /** Indica si la contrase&ntilde;a introducida es v&aacute;lida para ser
     * usada para el cifrado de datos. Una contrase&ntilde;a nula se considera
     * no v&aacute;lida, pero si lo es una contrase&ntilde;a vac&iacute;a.
     * @param password
     *        Contrase&ntilde;a que queremos evaluar.
     * @return Devuelve {@code true} si la contrase&ntilde;a es v&aacute;lida. */
    static boolean isValidPassword(final String password) {
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
    byte[] getPlainData() {
        return this.plainData == null ? null : this.plainData.clone();
    }

    /** Establece los datos planos para cifrar.
     * @param plainData
     *        Datos para cifrar. */
    void setPlainData(final byte[] plainData) {
    	this.plainData = plainData == null ? null : plainData.clone();
    }

    /** Recupera los dato cifrados.
     * @return Datos cifrados. */
    byte[] getCipheredData() {
        return this.cipheredData == null ? null : this.cipheredData.clone();
    }

    /** Establece los datos cifrados para descifrar.
     * @param cipheredData
     *        Datos cifrados. */
    void setCipheredData(final byte[] cipheredData) {
        this.cipheredData = cipheredData == null ? null : cipheredData.clone();
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
     *         Ocurri&oacute; un error durante el proceso de cifrado.
     * @throws KeyException Cuando la clave de cifrado no sea compatible con el
     * algoritmo de firma configurado.
     */
    void cipherData() throws IOException, NoSuchAlgorithmException, AOException, KeyException {

        byte[] dataToCipher = null;
        if (this.plainData != null) {
            dataToCipher = this.plainData;
        }
        else {

            // Fichero de entrada
            if (this.fileUri == null) {
                final String fileName = AOUIFactory.getLoadFiles(null, null, null, null, null, false, false, null, this.parent)[0].getAbsolutePath();
                try {
                    this.fileUri = AOUtil.createURI(fileName);
                }
                catch (final Exception e) {
                    throw new IOException("Se ha proporcionado un nombre de fichero no valido: " + e, e); //$NON-NLS-1$
                }
            }

            // En este punto, tenemos la URI de los datos de entrada
            final InputStream is = AOUtil.loadFile(this.fileUri);
            dataToCipher = AOUtil.getDataFromInputStream(is);
            is.close();
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
     *         Error durante el proceso de cifrado.
     * @throws KeyException
     *         Cuando se indica una clave no v&aacute;lida para el cifrado.
     */
    void cipherData(final byte[] dataToCipher) throws NoSuchAlgorithmException, AOException, KeyException {

        if (dataToCipher == null) {
            throw new IllegalArgumentException("Los datos a crifar no pueden ser nulos"); //$NON-NLS-1$
        }

        // Ya tenemos el stream con los datos, vamos a ver que Cipher uso
        final AOCipher cipher = new AOSunJCECipher();
        final Key cipherKey;
		try {
			cipherKey = getConfiguredKey(cipher, this.cipherConfig);
		}
		catch (final CertificateException e) {
			throw new AOException("Error en un certificado del almacen de cifrado: " + e, e); //$NON-NLS-1$
		}
		catch (final KeyStoreException e) {
			throw new AOException("Error en el almacen de cifrado: " + e, e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("Error al acceder al almacen de cifrado: " + e, e); //$NON-NLS-1$
		}

        // realizamos la operacion de cifrado
        this.cipheredData = cipher.cipher(dataToCipher, this.cipherConfig, cipherKey);
    }

    /** Obtiene una clave compatible para una configuraci&oacute;n de cifrado. La
     * clave puede haber sido insertada por el usuario, extra&iacute;da del
     * almac&eacute;n de claves...
     * @return Clave para el cifrado.
     * @throws NoSuchAlgorithmException
     *         Algoritmo de cifrado no soportado.
     * @throws AOException
     *         Ocurri&oacute; un error al obtener la clave.
     * @throws KeyException Cuando se produce un error al generar la clave.
     */
    Key getConfiguredKey() throws NoSuchAlgorithmException, AOException, KeyException {
        try {
			return this.getConfiguredKey(new AOSunJCECipher(), this.cipherConfig);
		}
        catch (final CertificateException e) {
        	throw new AOException("Error en un certificado del almacen de cifrado: " + e, e); //$NON-NLS-1$
		}
        catch (final KeyStoreException e) {
        	throw new AOException("Error en el almacen de cifrado: " + e, e); //$NON-NLS-1$
		}
        catch (final IOException e) {
        	throw new AOException("Error al acceder al almacen de cifrado: " + e, e); //$NON-NLS-1$
		}
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
     *         Ocurri&oacute; un error al obtener la clave.
     * @throws KeyException Cuando se produce un error al generar la clave.
     * @throws IOException Si hay problemas en el tratamiento de datos.
     * @throws KeyStoreException Si hay problemas con el almac&eacute;n de claves.
     * @throws CertificateException Si hay problemas en el tratamiento de los certificados. */
    private Key getConfiguredKey(final AOCipher cipher, final AOCipherConfig config) throws NoSuchAlgorithmException,
                                                                                            AOException,
                                                                                            KeyException,
                                                                                            CertificateException,
                                                                                            KeyStoreException,
                                                                                            IOException {
        Key cipherKey;

        // Tomamos o generamos la clave, segun nos indique el modo de clave.
        if (this.keyMode.equals(AOCipherConstants.KEY_MODE_GENERATEKEY)) {
            cipherKey = cipher.generateKey(config);
            this.cipherKeyEncoded = cipherKey.getEncoded();

            // Si se permite el almacenamiento de las claves, le damos la
            // posibilidad al usuario
            // Si se selecciona "Si" se almacenara la clave, si se selecciona
            // "No" no se almacenara
            // y si se selecciona "Cancelar" o se cierra el dialogo, se
            // cancelara toda la operacion
            // de cifrado.
            if (this.useCipherKeyStore) {
                try {
                    saveCipherKey(config, cipherKey);
                }
                catch (final AOMaxAttemptsExceededException e) {
                	AOUIFactory.showMessageDialog(
            			this.parent,
            			AppletMessages.getString("SignApplet.43"), //$NON-NLS-1$
                        AppletMessages.getString("SignApplet.156"), //$NON-NLS-1$
                        JOptionPane.ERROR_MESSAGE
                    );
                }
            }
        }
        else if (this.keyMode.equals(AOCipherConstants.KEY_MODE_USERINPUT)) {

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
            if (this.cipherKeyEncoded != null) {
                cipherKey = cipher.decodeKey(this.cipherKeyEncoded, config, null);
            }
            else if (this.useCipherKeyStore && AOCipherKeyStoreHelper.storeExists()) {
                cipherKey = getKeyFromCipherKeyStore();
            }
            else {
                throw new AOException("No se ha establecido una clave de cifrado"); //$NON-NLS-1$
            }
        }
        else if (this.keyMode.equals(AOCipherConstants.KEY_MODE_STRING)) {
            if (this.cipherPassword == null || this.cipherPassword.length == 0) {
                this.cipherPassword = AOUIFactory.getPassword(AppletMessages.getString("SignApplet.414"), null, ACCEPTED_CHARS, true, this.parent); //$NON-NLS-1$
            }
            cipherKey = cipher.decodePassphrase(this.cipherPassword, config, null);
        }
        else {
            throw new IllegalArgumentException("Modo de clave no soportado"); //$NON-NLS-1$
        }

        return cipherKey;
    }

    /** Descifra los datos con la configuraci&oacute;n establecida.
     * @throws AOCancelledOperationException
     *         Operaci&oacute;n cancelada por el usuario.
     * @throws IOException
     *         Si no se pueden leer los datos a descifrar.
     * @throws AOException
     *         Si ocurre alg&uacute; error durante el proceso de descifrado.
     * @throws KeyException Cuando la clave para el descifrado no sea correcta.
     */
    void decipherData() throws IOException, AOException, KeyException {

        byte[] dataToDecipher = null;
        if (this.cipheredData != null) {
            dataToDecipher = this.cipheredData;
        }
        else {

            // Si no hay una informacion cofrada establecida, la tratamos de
            // leer desde fichero
            if (this.fileUri == null) {
                final String fileName = AOUIFactory.getLoadFiles(null, null, null, null, null, false, false, null, this.parent)[0].getAbsolutePath();
                try {
                    this.fileUri = AOUtil.createURI(fileName);
                }
                catch (final Exception e) {
                    throw new IOException("Se ha proporcionado un nombre de fichero no valido: " + e, e); //$NON-NLS-1$
                }
            }

            // En este punto, tenemos la URI de los datos de entrada
            final InputStream is =  AOUtil.loadFile(this.fileUri);
            if (this.fileBase64) {
                dataToDecipher = Base64.decode(
                        new String(AOUtil.getDataFromInputStream(is)));
            }
            else {
                dataToDecipher = AOUtil.getDataFromInputStream(is);
            }

            is.close();
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
     * @throws KeyException
     *         Cuando la clave no sea correcta.
     */
    void decipherData(final byte[] dataToDecipher) throws AOException, KeyException {

        // Si no esta establecido el algoritmo de cifrado usamos el por
        // defecto, pero solo para esta ocasion
        final AOCipher decipher = new AOSunJCECipher();
        Key decipherKey = null;

        // Si el modo de clave es por password, generamos la clave a partir de
        // el.
        // En caso contrario, requeriremos que nos den la clave
        if (this.keyMode.equals(AOCipherConstants.KEY_MODE_STRING)) {
            if (this.cipherPassword == null || this.cipherPassword.length == 0) {
                this.cipherPassword = AOUIFactory.getPassword(AppletMessages.getString("SignApplet.414"), this.parent); //$NON-NLS-1$
            }
            decipherKey = decipher.decodePassphrase(this.cipherPassword, this.cipherConfig, null);
        }
        else {
            // En el caso de trabajar con claves, si no se indico cual debe usarse,
            // se ofrecera al usuario la posibilidad de tomar una del almacen de claves
            // de cifrado. Si no existe el almacen, se le indica que es obligatorio
            // introducir la clave.
            if (this.cipherKeyEncoded == null) {
                if (AOCipherKeyStoreHelper.storeExists()) {
                    try {
						decipherKey = getKeyFromCipherKeyStore();
					}
                    catch (final NoSuchAlgorithmException e) {
						throw new AOException("Error en el algoritmo de cifrado: " + e, e); //$NON-NLS-1$
					}
                    catch (final CertificateException e) {
                    	throw new AOException("Error en un certificado del almacen de cifrado: " + e, e); //$NON-NLS-1$
					}
                    catch (final KeyStoreException e) {
                    	throw new AOException("Error en el almacen de cifrado: " + e, e); //$NON-NLS-1$
					}
                    catch (final IOException e) {
                    	throw new AOException("Error al acceder al almacen de cifrado: " + e, e); //$NON-NLS-1$
					}
                }
                else {
                    throw new AOException("No se ha indicado la clave para el descifrado de datos y no hay almacen de claves"); //$NON-NLS-1$
                }
            }
            else {
                decipherKey = decipher.decodeKey(this.cipherKeyEncoded, this.cipherConfig, null);
            }
        }

        // Realizamos la operacion de desencriptado
        this.plainData = decipher.decipher(dataToDecipher, this.cipherConfig, decipherKey);
    }

    /** Obtiene una clave de cifrado a partir de una clave del almac&eacute;n del
     * usuario. Si no se ha establecido la contrase&ntilde;a del almac&eacute;n
     * y/o no se ha seleccionado un alias, se solicita al usuario mediante una
     * ventana modal.
     * @return Clave de cifrado/descifrado.
     * @throws IOException Si hay problemas en el tratamiento de datos.
     * @throws KeyStoreException Si hay problemas con el almac&eacute;n de claves.
     * @throws CertificateException Si hay problemas en el tratamiento de los certificados.
     * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario.
     * @throws AOException En caso de cualquier otro error. */
    private Key getKeyFromCipherKeyStore() throws AOException,
                                                  NoSuchAlgorithmException,
                                                  CertificateException,
                                                  KeyStoreException,
                                                  IOException {
        // Si el almacen no existe devolvemos un error
        if (!AOCipherKeyStoreHelper.storeExists()) {
            throw new AOException("No existe un almacen de claves de cifrado asociado al usuario"); //$NON-NLS-1$
        }
        // Abrimos el Almacen de claves de cifrado preguntandole al usuario la
        // clave si no
        // la indico
        final AOCipherKeyStoreHelper cKs = new AOCipherKeyStoreHelper(
        		this.cipherKeystorePass != null
                	? this.cipherKeystorePass
                    	: AOUIFactory.getPassword(AppletMessages.getString("SignApplet.52"), this.parent) //$NON-NLS-1$
        );

        // Si no se establecio el alias de la clave de cifrado, se la pedimos al
        // usuario
        String alias = null;
        if (this.cipherKeyAlias == null) {
            try {
            	// Le pedimos el alias de la clave de cifrado al usuario
                alias = (String) AOUIFactory.showInputDialog(
                		this.parent,
                		AppletMessages.getString("CipherManager.0"), //$NON-NLS-1$
                		AppletMessages.getString("CipherManager.1"), //$NON-NLS-1$
                		JOptionPane.PLAIN_MESSAGE,
                		null,
                		cKs.getAliases(),
                		cKs.getAliases()[0]);
            }
            catch (final AOCancelledOperationException e) {
                throw e;
            }
            catch (final Exception e) {
                throw new AOException("Error seleccionar la clave de cifrado", e); //$NON-NLS-1$
            }
        }
        else {
            alias = this.cipherKeyAlias;
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
     * @throws es.gob.afirma.applet.AOMaxAttemptsExceededException
     *         Cuando se supera el n&uacute;mero m&aacute;ximo de intentos
     *         fallidos al almac&eacute;n.
     * @throws AOException
     *         Error al almacenar la clave de cifrado. */
    private void saveCipherKey(final AOCipherConfig config, final Key cipherKey) throws AOException {
        // Preguntamos si se desea almacenar en el almacen de claves de cifrado
        // y si se acepta y no existe este almacen, lo creamos
    	final int selectedOption = AOUIFactory.showConfirmDialog(
			this.parent,
			AppletMessages.getString("SignApplet.40"), //$NON-NLS-1$
			AppletMessages.getString("SignApplet.41"), //$NON-NLS-1$
			AOUIFactory.OK_CANCEL_OPTION,
			AOUIFactory.QUESTION_MESSAGE
		);

        if (selectedOption == AOUIFactory.OK_OPTION) {

            // Controlamos un maximo de 3 intentos para abrir el almacen cuando
            // no se establecio la contrasena
            AOCipherKeyStoreHelper cKs = null;
            try {
            	if (this.cipherKeystorePass != null) {
            		try {
            			cKs = new AOCipherKeyStoreHelper(this.cipherKeystorePass);
            		}
            		catch (final IOException e) {
            			throw new AOException("La contrasena del almacen de claves de cifrado no es valida", e); //$NON-NLS-1$
            		}
            	}
            	else {
            		int numTries = 0;
            		do {
            			numTries++;
            			try {
            				cKs = new AOCipherKeyStoreHelper(new UIPasswordCallback(AppletMessages.getString("SignApplet.42"), this.parent).getPassword()); //$NON-NLS-1$
            			}
            			catch (final IOException e) {
            				if (numTries >= 3) {
            					throw new AOMaxAttemptsExceededException(
        							"Se ha sobrepasado el numero maximo de intentos en la insercion de la clave del almacen", //$NON-NLS-1$
        							e
    							);
            				}
            			}
            		} while (cKs == null);
            	}
            } catch (final Exception e) {
            	throw new AOException("No se ha podido cargar el almacen de claves del usuario", e); //$NON-NLS-1$
            }
            String alias = this.cipherKeyAlias;
            if (alias == null) {
                try {
                	alias = AOUIFactory.showInputDialog(
            			this.parent,
            			AppletMessages.getString("SignApplet.46"), //$NON-NLS-1$
            			AppletMessages.getString("SignApplet.47"), //$NON-NLS-1$
            			AOUIFactory.QUESTION_MESSAGE,
            			null,
            			null,
            			null
        			).toString();
                }
                catch (final Exception e) {
                    throw new AOException("Error al almacenar la clave de cifrado, la clave quedara sin almacenar: " + e, e); //$NON-NLS-1$
                }
                alias += " (" + config.toString() + ")"; //$NON-NLS-1$ //$NON-NLS-2$
            }

            cKs.storeKey(alias, cipherKey);
        }
        else {
        	throw new AOCancelledOperationException("Se ha cancelado el guardado de la clave de cifrado"); //$NON-NLS-1$
        }
    }
}
