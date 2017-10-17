/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

/** Contiene los puntos de entrada de las funcionalidades accesorias y de
 * utilidad del Cliente AFirma para su uso como Applet Java. <br>
 * <br>
 * Contains AFirma cryptographic functionalities access points, for their use as
 * Applet Java. */
interface EntryPointsUtil {

    /** Devuelve el contenido de un fichero (especificado previamente con
     * setFileURI) en base 64. Si se produce alg&uacute;n error durante la
     * operaci&oacute;n se devolver&aacute; <code>null</code>. <br>
     * <br>
     * Returns the contents of a file (previously specified with setFileURI) in
     * base 64. Errors ocurred during the operation display <code>null</code>
     * @param showProgress
     *        Mostrar o no ventana de progreso de la lectura <br>
     *        Show / Hide Read Progress window
     * @return El contenido del fichero codificado en base 64. <br>
     *         File contents, codified in base 64. */
    String getFileBase64Encoded(boolean showProgress);

    /** Devuelve el contenido de un fichero en base 64. Si se produce
     * alg&uacute;n error durante la operaci&oacute;n se devolver&aacute; {@code null}. <br>
     * Se muestra un di&aacute;logo al usuario para que consienta la operaci&oacute;n. Si no lo hace,
     * se aborta la operaci&oacute;n (devolvi&eacute;ndose {@code null} y se establece un error.
     * <br>
     * Returns the contents of a file in base 64. If fileUri parameter has been
     * previously set (with setFileUri), modifications are kept. If errors ocurr
     * during operation <code>null</code> is returned.
     * @param fileUri
     *        URL al fichero de entrada <br>
     *        URL to input file
     * @param showProgress
     *        Mostrar o no ventana de progreso de la lectura <br>
     *        Show / Hide Read Progress window
     * @return El contenido del fichero codificado en base 64 <br>
     *         File contents, codified in base 64. */
    String getFileBase64Encoded(String fileUri, boolean showProgress);

    /** Devuelve el hash de un fichero (especificado previamente con setFileURI)
     * codificado en base 64. El hash se calcula mediante el algoritmo de hash
     * establecido por defecto en el cliente. Si se produce alg&uacute; error
     * durante la operaci&oacute;n se devolver&aacute; <code>null</code>. <br>
     * <br>
     * Returns a file's hash (as previously specified with setFileURI), codified
     * in base 64. Hash is calculated with the client's default hash algorithm.
     * If an error occurs during the operation, <code>null</code> is returned.
     * @return El hash del fichero, en base 64. <br>
     *         File's Hash, in base 64. */
    String getFileHashBase64Encoded();

    /** Devuelve el contenido de un fichero como String. Se muestra un di&aacute;logo al usuario
     * para que consienta la operaci&oacute;n. Si no lo hace, se aborta la operaci&oacute;n
     * y se establece un error.<br>
     * Returns a file's contents as a string.
     * @param url
     *        URL del fichero a leer. <br>
     *        File's URL to be read.
     * @return El contenido del fichero como java.lang.String <br>
     *         File contents as java.lang.String
     * @deprecated Utilizar <code>getFileBase64Encoded(String, boolean)</code>.
     */
    @Deprecated
	String getTextFileContent(String url);

    /** Devuelve el contenido de un fichero como String codificado con el juego de
     * caracteres indicado. Se muestra un di&aacute;logo al usuario para que consienta
     * la operaci&oacute;n. Si no lo hace, se aborta la operaci&oacute;n y se establece
     * un error.<br>
     * Returns a file's contents as a string encode with the charset established.
     * @param url
     *        URL del fichero a leer. <br>
     *        File's URL to be read.
     * @param charset
     *        Juego de caracteres. <br>
     *        Charset.
     * @return El contenido del fichero como java.lang.String <br>
     *         File contents as java.lang.String
     * @deprecated Utilizar <code>getFileBase64Encoded(String, boolean)</code>.
     */
    @Deprecated
	String getTextFileContent(String url, String charset);

    /** Devuelve verdadero si el applet ha quedado en estado de error tras la
     * ultima operaci&oacute;n. <br>
     * <br>
     * Returns true if applet is left in error state after last operation.
     * @return true si la ultima ejecuci&oacute;n no se pudo completar
     *         correctamente, false en caso contrario <br>
     *         true if last execution could not be correctly completed.
     *         Otherwise, false. */
    boolean isError();

    /** Devuelve el mensaje de error, si el Applet est&aacute; en estado de
     * error. Si no esta en estado de error, se devuelve cadena vac&iacute;a. <br>
     * <br>
     * Returns error message, if applet is in error state. If not in error
     * state, returns empty string.
     * @return Mensaje de error <br>
     *         Error Message */
    String getErrorMessage();

    /** Establece si se debe informar de los errores al usuario
     * gr&aacute;ficamente cu&aacute;ndo se producen. Debe evitarse el uso de
     * esta funci&oacute;n. En su lugar, es preferible mostrar los mensajes
     * mediante JavaScript, por defecto no se mostrar&aacute;n <br>
     * <br>
     * Sets whether errors should be displayed graphically as they occur. Use of
     * this function should be avoided in favor of Java Script. By default,
     * errors are not displayed.
     * @param showErrors
     *        <code>true</code> si se debe informar al usuario de los
     *        errores mediante un di&aacute;logo gr&aacute;fico, <code>false</code> en caso contrario <br>
     *        <code>true</code> if errors must be displayed using a
     *        graphical dialog. Else, <code>false</code>. */
    void setShowErrors(boolean showErrors);

    /** Indica si se deben mostrar los certificados caducados o aun no activos en
     * el listado de certificados disponibles para firma. <br>
     * <br>
     * Indicates whether void or not yet active certificates should be displayed
     * in the list of certificates available to be signed.
     * @param showExpiratedCerts
     *        Indica si se deben mostrar los certificados expirados o aun no
     *        v&aacute;lidos. <br>
     *        Indicates whether void or not yet valid certificates should be
     *        displayed. */
    void setShowExpiratedCertificates(boolean showExpiratedCerts);

    /** Devuelve <code>true</code> si el applet ha sido inicializado. <br>
     * Returns <code>true</code> if applet has been initialized.
     * @return <code>true</code> si el Applet ha sido inicializado <br>
     *         <code>true</code> if applet has been initialized. */
    boolean isInitialized();

    /** Reinicia todos los par&aacute;metros del cliente de firma
     * (par&aacute;metros de entrada a su valor por defecto y los de salida a
     * nulo) y elimina el estado de error. Es recomendable el uso de este
     * m&eacute;todo cada vez que se realice una operaci&oacute;n compleja
     * (firma, multifirma, cifrado, sobre digital,...) y posteriormente se
     * almacenen los datos. <br>
     * <br>
     * Restarts all signature client parameters (input parameters are set to
     * their default value, and output parameters are set to null) and error
     * state is removed. This method is desirable for complex operations (e.g.,
     * signature, multi-signature, cyphering, digital enveloping) and later data
     * storage. */
    void initialize();

    /** Decodifica un texto en base 64. Si se produce alg&uacute;n error se
     * devuelve <code>null</code>. <br>
     * <br>
     * Decodes a text in base 64. If an error occurs, <code>null</code> is
     * returned.
     * @param b64
     *        Texto en base 64. <br>
     *        Text in base 64.
     * @return Texto decodificado. <br>
     *         Decoded text.
     * @deprecated Utilizar <code>getTextFromBase64(String b64, String charsetName)</code>*/
    @Deprecated
	String getTextFromBase64(String b64);

    /** Decodifica un texto en base 64. Si se produce alg&uacute;n error se
     * devuelve <code>null</code>. <br>
     * <br>
     * Decodes a text in base 64. If an error occurs, <code>null</code> is
     * returned.
     * @param b64
     *        Texto en base 64. <br>
     *        Text in base 64.
     * @param charsetName
     *        Juego de caracteres. <br>
     *        Charset.
     * @return Texto decodificado. <br>
     *         Decoded text.*/
    String getTextFromBase64(String b64, String charsetName);

    /** Codifica un texto plano a base 64. Si se produce alg&uacute;n error se
     * devuelve <code>null</code>. <br>
     * <br>
     * Encodes plain text to base 64. If an error occurs, <code>null</code> is
     * returned.
     * @param plainText
     *        Texto plano. <br>
     *        Plain Text.
     * @return Texto codificado. <br>
     *         Encoded Text.
     * @deprecated Utilizar <code>getBase64FromText(String plainText, String charsetName)</code>*/
    @Deprecated
    String getBase64FromText(String plainText);

    /** Codifica un texto plano a base 64. Si se produce alg&uacute;n error se
     * devuelve <code>null</code>. <br>
     * <br>
     * Encodes plain text to base 64. If an error occurs, <code>null</code> is
     * returned.
     * @param plainText
     *        Texto plano. <br>
     *        Plain Text.
     * @param charsetName
     *        Juego de caracteres. Si se especifica <code>null</code> se usar&aacute; el por defecto excepto en los casos de textos que
     *        representen un XML bien formado, casos en los que se detectar&aacute; la docificaci&oacute;n del XML y se usar&aacute; esta.
     *        Si especifica un juego de caracteres distinto de <code>null</code> codificando un fichero XML el resultado puede ser la
     *        introducci&oacute;n de caracteres no soportados por la definici&oacute;n de ese XML y por lo tanto en un XML mal formado. Use
     *        siempre <code>null</code> cuando codifique ficheros XML a Base64.
     *        <br>
     *        Charset. If <code>null</code> is specified the default charset will be used but when the text is a well-formed XML, where the
     *        XML encodinf will be used. When converting XML files to base64 use allways <code>null</code> as the charser value, as using
     *        a specify charse can lead to encoding problems and to a non-valid XML.
     * @return Texto codificado. <br>
     *         Encoded Text. */
    String getBase64FromText(String plainText, String charsetName);

    /** Recupera la versi&oacute;n del cliente de firma.<br>
     * El formato de la versi&oacute;n ser&aacute; siempre:<br>
     * <code>X.Y.Z Descripci&oacute;n</code><br>
     * En donde <code>X</code>, <code>Y</code> y <code>Z</code> son la
     * versi&oacute;n, subversi&oacute;n y construcci&oacute;n del cliente y
     * debe tener uno o m&aacute;s d&iacute;gitos; y <code>Descripci&oacute;n</code> es un texto libre opcional que puede
     * completar la identificaci&oacute;n de la versi&oacute;n del cliente. <br>
     * <br>
     * Returns signature client's version.<br>
     * Version shall always adopt this format:<br>
     * <code>X.Y.Z Descripci&oacute;n</code><br>
     * Where <code>X</code>, <code>Y</code> and <code>Z</code> are the client's
     * version, sub-version, and build and must contain one or more digits. <code>Descripci&oacute;n</code> is an optional free text used to
     * supplement client's version identification.
     * @return Versi&oacute;n del cliente. <br>
     *         Client Version. */
    String getVersion();

    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero
     * del que se recuperar&aacute; su ruta completa. Si no se selecciona
     * ning&uacute;n fichero, se devuelve <code>null</code>. <br>
     * <br>
     * Displays a modal dialog for the selection of a file, from which the whole
     * route will be returned. If no file is selected, <code>null</code> is
     * returned.
     * @param title
     *        T&iacute;tulo del di&aacute;logo modal. <br>
     *        Modal dialog's title.
     * @param exts
     *        Extensiones de b&uacute;squeda (separadas por {@link es.gob.afirma.applet.SignApplet#STRING_SEPARATOR
     *        STRING_SEPARATOR}) <br>
     *        Search extensions (separated by {@link es.gob.afirma.applet.SignApplet#STRING_SEPARATOR
     *        STRING_SEPARATOR})
     * @param description
     *        Descripcion del tipo de ficheros buscado. <br>
     *        Description of the search files' type.
     * @return Ruta completa del fichero seleccionado. <br>
     *         Complete route to the selected file. */
    String loadFilePath(String title, String exts, String description);

    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de un
     * directorio. Si no se selecciona ning&uacute;n directorio, se devuelve <code>null</code>. <br>
     * <br>
     * Displays a modal dialog for the selection of a directory. If no directory
     * is selected, <code>null</code> is returned.
     * @return Ruta completa del directorio seleccionado. <br>
     *         Complete route for the selected directory. */
    String selectDirectory();

}
