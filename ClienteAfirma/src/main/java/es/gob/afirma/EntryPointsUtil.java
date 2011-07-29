/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma;

/** Contiene los puntos de entrada de las funcionalidades accesorias y de
 * utilidad del Cliente AFirma para su uso como Applet Java. <br>
 * <br>
 * Contains AFirma cryptographic functionalities access points, for their use as
 * Applet Java. */
public interface EntryPointsUtil {

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
     *         File contents, codified in base 64.
     * @see #getFileBase64Encoded(String, boolean) */
    String getFileBase64Encoded(boolean showProgress);

    /** Devuelve el contenido de un fichero en base 64. Si se produce
     * alg&uacute;n error durante la operaci&oacute;n se devolver&aacute; <code>null</code>. <br>
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
     *         File contents, codified in base 64.
     * @see #getFileBase64Encoded(boolean) */
    String getFileBase64Encoded(String fileUri, boolean showProgress);

    /** Devuelve el hash de un fichero (especificado previamente con setFileURI)
     * codificado en base 64. El hash se calcula mediante el algoritmo de hash
     * establecido por defecto en el cliente. Si se produce alg&uacute; error
     * durante la operaci&oacute;n se devolver&aacute; <code>null</code>. <br>
     * <br>
     * Returns a file's hash (as previously specified with setFileURI), codified
     * in base 64. Hash is calculated with the client's default hash algorithm.
     * If an error occurs during the operation, <code>null</code> is returned.
     * @param showProgress
     *        Mostrar o no ventana de progreso de la lectura del fichero <br>
     *        Show / Hide File Read Progress window
     * @return El hash del fichero, en base 64. <br>
     *         File's Hash, in base 64. */
    String getFileHashBase64Encoded(boolean showProgress);

    /** Devuelve el contenido de un fichero como String.<br>
     * Returns a file's contents as a string.
     * @param url
     *        URL del fichero a leer. <br>
     *        File's URL to be read.
     * @return El contenido del fichero como java.lang.String <br>
     *         File contents as java.lang.String */
    String getTextFileContent(String url);

    /** Devuelve verdadero si el applet ha quedado en estado de error tras la
     * última operaci&oacute;n. <br>
     * <br>
     * Returns true if applet is left in error state after last operation.
     * @return true si la última ejecuci&oacute;n no se pudo completar
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

    /** Si showHashMessage=true, se muestra el mensaje informativo sobre el hash
     * de los datos a firmar. Si showHashMessage=false, no se muestra el
     * mensaje. <br>
     * <br>
     * If showHashMessage=true, a message is displayed, regarding the hash of
     * data to be signed. If showHashMessage=false, it isn't show the message.
     * @param showHashMessage
     *        Indica si se debe mostrar el nmensaje. <br>
     *        Indicates whether void or not yet the message should be
     *        displayed.
     * @deprecated El applet no debe mostrar nunca al usuario el hash de los
     *             datos a firmar. <br>
     *             Applet must never display the hash of data to be signed. */
    @Deprecated
    void setShowHashMessage(boolean showHashMessage);

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
     *         Decoded text. */
    String getTextFromBase64(String b64);

    /** Codifica un texto plano a base 64. Si se produce alg&uacute;n error se
     * devuelve <code>null</code>. <br>
     * <br>
     * Encodes plain text to base 64. If an error occurs, <code>null</code> is
     * returned.
     * @param plainText
     *        Texto plano. <br>
     *        Plain Text.
     * @return Texto codificado. <br>
     *         Encoded Text. */
    String getBase64FromText(String plainText);

    /** Recupera la versi&oacute;n del cliente de firma.<br/>
     * El formato de la versi&oacute;n ser&aacute; siempre:<br/>
     * <code>X.Y.Z Descripci&oacute;n</code><br/>
     * En donde <code>X</code>, <code>Y</code> y <code>Z</code> son la
     * versi&oacute;n, subversi&oacute;n y construcci&oacute;n del cliente y
     * debe tener uno o m&aacute;s d&iacute;gitos; y <code>Descripci&oacute;n</code> es un texto libre opcional que puede
     * completar la identificaci&oacute;n de la versi&oacute;n del cliente. <br>
     * <br>
     * Returns signature client's version.<br/>
     * Version shall always adopt this format:<br/>
     * <code>X.Y.Z Descripci&oacute;n</code><br/>
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
     *        Extensiones de b&uacute;squeda (separadas por {@link es.gob.afirma.cliente.SignApplet#STRING_SEPARATOR
     *        STRING_SEPARATOR}) <br>
     *        Search extensions (separated by {@link es.gob.afirma.cliente.SignApplet#STRING_SEPARATOR
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

    /** Cambia la localizaci&oacute;n del Cliente. <br>
     * <br>
     * Change the current locale.
     * @param locale
     *        C&oacute;digo de la localizaci&oacute;n (por ejemplo: en, en_GB, es, it...). Por defecto, la del sistema operativo.<br>
     *        Locale code (p.e. en, en_GB, es, it...) */
    void setLocale(String locale); 

    // /**
    // * Recupera un fragmento de datos en base 64.
    // * @return Fragmento base 64 de los datos.
    // */
    // String getTrunkBase64Encoded ();
    //
    // /**
    // * Informa si restan fragmentos de datos por recuperar.
    // * @return Devuelve {@code true} si quedan datos por recuperar, {@code
    // false} en caso contrario.
    // */
    // boolean hasMoreData();

}
