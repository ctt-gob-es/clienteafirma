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
 * M&eacute;todos necesarios para el Applet que act&uacute;a como instalador del cliente de firma
 * y sus extensiones.
 */
public interface InstaladorClienteEntryPoints {

    /**
     * Establece la URL desde la cual se bajar&aacute;n los ficheros para instalar.
     * @param url URL desde la cual se bajar&aacute;n los ficheros para instalar
     * @return <code>true</code> si se ha establecido correctamente, <code>false</code> si la URL est&aacute; mal formada
     */
    public boolean setBaseDownloadURL(String url);

    /**
     * Indica si el Applet ya esta iniciado.
     * @return <code>true</code> si est&aacute; inciado, <code>false</code> en caso contrario.
     */
    public boolean isIniciado();

    /**
     * Comprueba si est&aacute; instalada en el sistema alguna de las construcciones del
     * cliente de firma.
     * @return Devuelve <code>true</code> si el cliente est&aacute; instalado correctamente,
     * <code>false</code> en caso contrario.
     */
    public boolean isInstalado();

    /**
     * Comprueba si la contrucci&oacute;n instalada del cliente es igual o superior a la
     * indicada. En el caso de no encontrarse instalado el cliente en el sistema del usuario
     * el m&eacute;todo devolver&aacute; <code>false</code>.<br/>
     * Las distintas construcciones disponibles del cliente son: 'LITE', 'MEDIA' y 'COMPLETA'.
     * Cada una de estas construcciones ampl&iacute;a las capacidades de la anterior.
     * @param build Construcci&oacute;n m&iacute;nima exigida. 
     * @return Devuelve <code>true</code> si se encuentra instalada una contrucci&oacute;n igual
     * o superior a la solicitada, <code>false</code> en caso contrario.
     */
    public boolean isInstalado(final String build);

    /**
     * Indica si es obligatoria la instalaci&oacute;n de componentes en el sistema del usuario para
     * el uso del cliente en el entorno actual Sistema Operativo/Navegador Web/Versión de
     * Java/Arquitectura. También se comprueba que la versi&oacute;n de Java sea superior a la 1.6u12
     * a partir de la cual es posible el despliegue JNLP.
     * @return Si es necesaria o no instalaci&oacute;n.
     */
    public boolean isInstallationNeeded();
    
    /**
     * Comprueba que la contrucci&oacute;n instalada del cliente sea la m&aacute;s actualizada
     * disponible en el servidor.
     * @return Devuelve <code>true</code> si la contrucci&oacute;n est&aacute; actualizada,
     * <code>false</code> en caso contrario.
     */
    public boolean isActualizado();

    /**
     * Realiza una actualizaci&oacute;n de la construcci&oacute;n instalada del sistema del
     * cliente. Por ejemplo, si el usuario tiene instalada la construcci&oacute;n 'MEDIA' del
     * cliente se descargar&aacute; e instalar&aacute; la construcci&oacute;n 'MEDIA' m&aacute;s
     * reciente.<br/>
     * Si el cliente no se encontraba instalado, se instalar&aacute; la construcci&oacute;n LITE
     * del mismo.
     * @return <code>true</code> si la actualizaci&oacute;n se realiza correctamente,
     * <code>false</code> en caso contrario.
     */
    public boolean actualizar();

    /**
     * Realiza una actualizaci&oacute;n de la construcci&oacute;n instalada del sistema del
     * cliente y ejecuta un m&eacute;todo JavaScript al finalizar. 
     * Por ejemplo, si el usuario tiene instalada la construcci&oacute;n 'MEDIA' del
     * cliente se descargar&aacute; e instalar&aacute; la construcci&oacute;n 'MEDIA' m&aacute;s
     * reciente.<br/>
     * Si el cliente no se encontraba instalado, se instalar&aacute; la construcci&oacute;n LITE
     * del mismo.
     * @param jsMethodName M&eacute;todo JavaScript al que hay que notificar cuando termine la actualizaci&oacute;n
     * @param jsMethodParams Par&aacute;metros del m&eacute;todo JavaScript al que hay que notificar cuando termine la actualizaci&oacute;n
     * @return <code>true</code> si la actualizaci&oacute;n se realiza correctamente,
     * <code>false</code> en caso contrario.
     */
    public boolean actualizar(String jsMethodName, Object jsMethodParams);
    
    /**
     * Instala la construcci&oacute;n 'LITE' (la que incorpora las capacidades m&iacute;nimas)
     * del cliente de firma, incluyendo las partes nativas y clases Java necesarias.
     * @return Directorio de instalaci&oacute;n o <code>null</code> si ocurri&oacute; un error
     * durante la instalaci&oacute;n. 
     */
    public String instalar();

    /**
     * Instala la contrucci&oacute;n indicada del cliente de firma. Este m&eacute;todo instala
     * siempre la construcci&oacute;n en cuesti&oacute;n por lo que es aconsejable la previa
     * comprobaci&oacute;n de la versi&oacute;n mediante el m&eacute;todo
     * {@link #isInstalado(String)} para comprobar que no se encontrase ya instalada en el
     * sistema e {@link #isActualizado()} para comprobar que est&aacute; actualizada.<br/>
     * Los identificadores de contrucci&oacute;n aceptados son: 'LITE', 'MEDIA', 'COMPLETA'.
     * @param build Contrucci&oacute;n que se desea instalar.
     * @return Directorio de instalaci&oacute;n o <code>null</code> si ocurri&oacute; un error
     * durante la instalaci&oacute;n. 
     */
    public String instalar(final String build);

    /**
     * Instala la contrucci&oacute;n indicada del cliente de firma y ejecuta
     * un m&eacute;todo JavaScript al finalizar. Este m&eacute;todo instala
     * siempre la construcci&oacute;n en cuesti&oacute;n por lo que es aconsejable la previa
     * comprobaci&oacute;n de la versi&oacute;n mediante el m&eacute;todo
     * {@link #isInstalado(String)} para comprobar que no se encontrase ya instalada en el
     * sistema e {@link #isActualizado()} para comprobar que est&aacute; actualizada.<br/>
     * Los identificadores de contrucci&oacute;n aceptados son: 'LITE', 'MEDIA', 'COMPLETA'.
     * @param build Contrucci&oacute;n que se desea instalar.
     * @param jsMethodName M&eacute;todo JavaScript a ejecutar al terminar la
     *                     instalaci&oacute;n (puede ser nulo)
     * @param jsMethodParams Par&aacute;metros del m&eacute;todo JavaScript
     * @return Directorio de instalaci&oacute;n o <code>null</code> si ocurri&oacute; un error
     * durante la instalaci&oacute;n. 
     */
	public String instalar(final String build, final String jsMethodName, final Object jsMethodParams);

	/**
	 * Instala el n&uacute;cleo de una construcci&oacute;n concreta del cliente.
   * Devuelve el directorio de instalaci&oacute;n o {@code null} si se produce alg&uacute;n error.
   * @param build Contrucci&oacute;n que se desea instalar.
   * @return Directorio de instalaci&oacute;n.
	 */
	public String installCore(final String build);
	
	/**
   * Instala las dependencias que necesita el sistema para una construcci&oacute;n concreta.
   * Devuelve el directorio de instalaci&oacute;n o {@code null} si se produce alg&uacute;n error.
   * @param build Contrucci&oacute;n que se desea instalar.
   * @return Directorio de instalaci&oacute;n.
   */
  public String installDependencies(final String build);
	
  /**
   * Instala las dependencias de la contrucci&oacute;n indicada del cliente de firma y ejecuta
   * un m&eacute;todo JavaScript al finalizar.<br/>
   * Los identificadores de contrucci&oacute;n aceptados son: 'LITE', 'MEDIA', 'COMPLETA'.
   * @param build Contrucci&oacute;n de la que se desean instalar las dependencias.
   * @param jsMethodName M&eacute;todo JavaScript a ejecutar al terminar la
   *                     instalaci&oacute;n (puede ser nulo).
   * @param jsMethodParams Par&aacute;metros del m&eacute;todo JavaScript.
   * @return Directorio de instalaci&oacute;n. 
   */
  public String installDependencies(final String build, final String jsMethodName, final Object jsMethodParams);
  
    /**
     * Devuelve la lista de jars necesarios para la ejecuci&oacute;n del Cliente de Firma.
     * @return Lista de jars necesarios para la ejecuci&oacute;n del Cliente de Firma, separados por comas
     * @deprecated Las comas pueden ser parte del nombre de fichero en casi todos los sistemas operativos, no
     *             tiene utilidad pr&aacute;ctica exponer los archivos Java del cliente
     */
    @Deprecated
    public String getAllJars();

    /**
     * Devuelve la ruta al directorio local en que se ha instalado (o se
     * instalar&aacute;) el Cliente de Firma.
     * @return Ruta al directorio de instalaci&oacute;n del Cliente de Firma
     */
    public String getInstallationDirectory();

    /**
     * Desinstala localmente el Cliente de Firma.
     * @return Devuelve <code>true</code> si el cliente se desinstal&oacute;n por completo, <code>false</code>
     * si existieron fichero que no se pudieron eliminar.
     */
    public boolean desinstalar();

    /**
     * Desinstala las versiones 2.4 y anteriores del cliente que se encuentren instaladas en
     * el sistema.
     * @return Devuelve <code>true</code> si la desinstal&oacute;n finaliz&oacute; correctamente
     * <code></code> en caso contrario.
     */
    public boolean desinstalarAntiguas();
    
    /**
     * Recupera la versi&oacute;n del BootLoader.
     * @return Versi&oacute;n del BootLoader.
     */
    public String getVersion();

    /**
     * Recupera la version de un cliente instalado en el sistema.<br/> 
     * La versi&oacute;n se indica mediante una cadena del tipo "X.Y.Z BUILD", en donde
     * <code>X</code>, <code>Y</code> y <code>Z</code> indican la versi&oacute;n de la
     * construcci&oacute;n instalada del cliente y BUILD es el tipo de construcci&oacute;n
     * instalada ('LITE', 'MEDIA' y 'COMPLETA').<br/>
     * En caso de no estar instalado el cliente en el sistema se devuelve la cadena "0.0.0".
     * @return Cadena descriptiva de la versi&oacute;n.
     */
    public String getClientVersion();
}
