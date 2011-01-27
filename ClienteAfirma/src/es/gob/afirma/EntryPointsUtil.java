/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma;

/**
 * Contiene los puntos de entrada de las funcionalidades accesorias y de utilidad del
 * Cliente AFirma para su uso como Applet Java.
 */
public interface EntryPointsUtil {

    /**
     * Devuelve el contenido de un fichero (especificado previamente con setFileURI)
     * en base 64.  Si se produce alg&uacute;n error durante la operaci&oacute;n se
     * devolver&aacute; <code>null</code>.
     * @param showProgress
     *            Mostrar o no ventana de progreso de la lectura
     * @return El contenido del fichero codificado en base 64.
     * @see #getFileBase64Encoded(String, boolean)
     */
    public String getFileBase64Encoded(boolean showProgress);
    
    /**
     * Devuelve el contenido de un fichero en base 64. Si se hab&iacute;a establecido previamente
     * el par&aacute;metro fileUri (con setFileUri) este queda modificado. Si se produce alg&uacute;n
     * error durante la operaci&oacute;n se devolver&aacute; <code>null</code>.
     * @param fileUri 
     *            URL al fichero de entrada
     * @param showProgress
     *            Mostrar o no ventana de progreso de la lectura
     * @return El contenido del fichero codificado en base 64
     * @see #getFileBase64Encoded(boolean)
     */
    public String getFileBase64Encoded(String fileUri, boolean showProgress);
    
    /**
     * Devuelve el hash de un fichero (especificado previamente con setFileURI)
     * codificado en base 64. El hash se calcula mediante el algoritmo de hash
     * establecido por defecto en el cliente. Si se produce alg&uacute; error durante
     * la operaci&oacute;n se devolver&aacute; <code>null</code>.
     * 
     * @param showProgress
     *            Mostrar o no ventana de progreso de la lectura del fichero
     * @return El hash del fichero, en base 64 (el hash calculado sobre el
     *         fichero en su codificaci&oacute;n original)
     */
    public String getFileHashBase64Encoded(boolean showProgress);

    /**
     * Devuelve el contenido de un fichero como String.<br>
     * 
     * @param url URL del fichero a leer (si est&aacute; en local, debe empezar por file:///)
     * @return El contenido del fichero como java.lang.String
     */
    public String getTextFileContent(String url);
        
    /**
     * Devuelve verdadero si el applet ha quedado en estado de error tras la
     * última operaci&oacute;n. 
     * @return true si la última ejecuci&oacute;n no se pudo completar correctamente, false en caso contrario
     */
    public boolean isError();
    
    /**
     * Devuelve el mensaje de error, si el Applet est&aacute; en estado de error.
     * Si no esta en estado de error, se devuelve cadena vac&iacute;a.
     * @return Mensaje de error
     */
    public String getErrorMessage();

    /**
     * Establece si se debe informar de los errores al usuario gr&aacute;ficamente cu&aacute;ndo se producen.
     * Debe evitarse su uso y mostrar los mensajes mediante JavaScript, por defecto no se mostrar&aacute;n 
     * @param showErrors <code>true</code> si se debe informar al usuario de los errores mediante un di&aacute;logo
     *                   gr&aacute;fico, <code>false</code> en caso contrario
     */
	public void setShowErrors(boolean showErrors);
    
    /**
     * Originalmente si showHashMessage=true, se mostraba el mensaje informativo sobre el hash de los
     * datos a firmar. Si showHashMessage=false, no se mostraba el mensaje.
     * Actualmente, este m&eacute;todo no realiza ninguna acci&oacute;n y muestra por pantalla un mensaje
     * advirtiendo que se ha eliminado la funcionalidad. 
     * @param showHashMessage No tiene efecto
     * @deprecated El applet no debe mostrar nunca al usuario el hash de los datos a firmar. 
     */
    @Deprecated
	public void setShowHashMessage(boolean showHashMessage);
    
    /**
     * Indica si se deben mostrar los certificados caducados o aun no activos en el listado de
     * certificados disponibles para firma. 
     * @param showExpiratedCerts Indica si se deben mostrar los certificados expirados o aun no
     * v&aacute;lidos. 
     */
    public void setShowExpiratedCertificates(boolean showExpiratedCerts);
    
    /**
     * Devuelve <code>true</code> si el applet ha sido inicializado. 
     * @return <code>true</code> si el Applet ha sido inicializado
     */
    public boolean isInitialized();
    
    /**
     * Reinicia todos los par&aacute;metros del cliente de firma (par&aacute;metros de entrada a su valor por
     * defecto y los de salida a nulo) y elimina el estado de error. Es recomendable el uso de este
     * m&eacute;todo cada vez que se realice una operaci&oacute;n compleja (firma, multifirma, cifrado, sobre digital,...)
     * y posteriormente se almacenen los datos. 
     */
    public void initialize();
    
    /**
     * Decodifica un texto en base 64. Si se produce alg&uacute;n error se devuelve <code>null</code>. 
     * @param b64 Texto en base 64.
     * @return Texto decodificado.
     */
    public String getTextFromBase64(String b64);
    
    /**
     * Codifica un texto plano a base 64. Si se produce alg&uacute;n error se devuelve <code>null</code>. 
     * @param plainText Texto plano.
     * @return Texto codificado.
     */
    public String getBase64FromText(String plainText);
    
    /**
     * Recupera la versi&oacute;n del cliente de firma. Esta es la versi&oacute;n del n&uacute;cleo
     * del cliente y no tiene que coincidir con la version de los plugins que integre.<br/>
     * El formato de la versi&oacute;n ser&aacute; siempre:<br/>
     * <code>X.Y.Z Descripci&oacute;n</code><br/>
     * En donde <code>X</code>, <code>X</code> y <code>X</code> son la versi&oacute;n, subversi&oacute;n
     * y contrucci&oacute;n del cliente y debe tener uno o m&aacute;s d&iacute;gitos; y 
     * <code>Descripci&oacute;n</code> es un texto libre opcional que puede completar la identificaci&oacute;n
     * de la versi&oacute;n del cliente.
     * @return Versi&oacute;n del cliente.
     */
    public String getVersion();

    /**
     * Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero del que se
     * recuperar&aacute; su ruta completa. Si no se selecciona ning&uacute;n fichero,
     * se devuelve <code>null</code>.
     * @param title T&iacute;tulo del di&aacute;logo modal.
     * @param exts Extensiones de b&uacute;squeda (separadas por
     * {@link es.gob.afirma.cliente.SignApplet#STRING_SEPARATOR STRING_SEPARATOR})
     * @param description Descripcion del tipo de ficheros buscado.
     * @return Ruta completa del fichero seleccionado.
     */
    public String loadFilePath(String title, String exts, String description);
    
    /**
     * Muestra un di&aacute;logo modal para la selecci&oacute;n de un directorio del que se
     * recuperar&aacute; su ruta completa. Si no se selecciona ning&uacute;n directorio,
     * se devuelve <code>null</code>.
     * @return Ruta completa del directorio seleccionado.
     */
    public String selectDirectory();
}
