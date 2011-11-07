/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.miniapplet;

import java.io.IOException;
import java.security.PrivilegedActionException;

import es.gob.afirma.core.AOFormatFileException;


/** Contiene los puntos de entrada de las funcionalidades criptogr&aacute;ficas
 * del Mini-Applet del Cliente AFirma.
 * El Applet acepta como par&aacute;metros de entrada (desde el HTML):
 * <dl>
 *  <dt>keystore</dt>
 *  <dd>Indica el almac&eacute;n de claves y certificados que se debe usar, aceptando los siguientes valores:
 *   <ul>
 *    <li><b>DNIE</b>: DNI Electr&oacute;nico. Es necesario que un controlador PKCS#11 de DNIe est&eacute; instalado en el sistema (ya sea el <i>oficial</i> u OpenDNIe).</li>
 *    <li><b>WINDOWS</b>: Repositorio de Microsoft Windows (CAPI). &Uacute;nicamente compatible con sistemas Windows.</li>
 *    <li><b>APPLE</b>: Repositorio de Apple Mac OS X. &Uacute;nicamente compatible con sistemas Apple Mac OS X.</li>
 *    <li><b>PKCS11</b>: Repositorio de tipo PKCS#11 controlador por una libreria de sistema.</li>
 *    <li><b>PKCS12</b>: Repositorios en disco en formato PKCS#12 o PFX (Personal File Exchange).</li>
 *    <li><b>JKS</b>: Repositorios en disco en formato JKS (Java KeyStore).</li>
 *    <li><b>JAVACE</b>: Repositorios en disco en formato Java KeyStore Exact Case.</li>
 *    <li><b>MOZ_UNI</b>: Repositorio de Mozilla Firefox. Necesita que el repositorio exista y que Firefox o las bibliotecas NSS est&eacute;n instalados en el sistema.</li>
 *    <li>Es posible tambi&eacute;n indicar un <i>UserAgent</i> de un navegador Web, realizando entonces el Applet una selecci&oacute;n inteligente.
 *   </ul>
 *   En caso de necesitarse un fichero externo (biblioteca, almac&eacute;n en archivo, etc.) o una contrase&ntilde;a, estos se solicitan al usuario mediante di&aacute;logos gr&aacute;ficos
 *  </dd>
 * </dl>
 */
public interface MiniAfirma {

    /**
     * Firma unos datos seg&uacute;n la configuracion proporcionada.
     * La configuraci&oacute;n que se puede proporcionar es el algoritmo,
     * el formato de firma y par&aacute;metros adicionales del formato particular.
     * Estos par&aacute;metrosd extra se indicar&aacute;n como un listado de cadenas
     * con la forma {code CLAVE=VALOR}, en donde {@code CLAVE} es el nombre de la
     * propiedad y {@code VALOR} el valor asignada a esta. Para utilizar el valor
     * por defecto de una propiedad se dejar&aacute; de indicar esta en el listado de
     * par&aacute;metros.
     * @param data Datos a firmar en Base64.
     * @param algorithm Algoritmo de firma.
     * @param format Formato de firma.
     * @param extraParams Par&aacute;metros adicionales para configurar la operac&oacute;n.
     * @return Firma electr&oacute;nica resultante en Base64.
     * @throws IOException Cuando se produce un error durante la firma electr&oacute;nica.
     * @throws AOFormatFileException Cuando se indica un formato de firma no soportado.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     */
    String sign(String data, String algorithm, String format, String[] extraParams) throws IOException, AOFormatFileException, PrivilegedActionException;

    
    /**
     * Realiza la firma paralela (cofirma) de unos datos. La cofirma de una firma requiere
     * que los datos est&eacute;n contenidos en la firma original o que se indiquen de
     * forma externa. Si no se proporcionasen los datos, &uacute;nicamente se realizar&aacute; la cofirma
     * si el algoritmo de firma indicado conincide con el de la firma ya existente.
     * Tanto si la firma est&aacute; contenida en los datos (por ejemplo, en OOXML, ODF, PDF, etc.) como si los datos est&aacute;n contenidos en la firma,
     * el par&aacute;metro <i>data</i> debe establecerse a <code>null</code> (s&oacute;lo se establece si firma y datos se
     * encuentran en erchivos independientes)
     * @param sign Firma electr&oacute;nica en Base64.
     * @param data Datos en Base64 que se desean cofirmar.
     * @param algorithm Algoritmo de firma.
     * @param format Formato de firma.
     * @param extraParams Par&aacute;metros adicionales para configurar la operac&oacute;n.
     * @return Cofirma resultante en Base64.
	 * @throws IOException Cuando se produce un error durante la cofirma electr&oacute;nica.
     * @throws AOFormatFileException Cuando se indica un formato de firma no soportado o no
     * se puede identificar el formato de la firma.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     */
    String coSign(String sign, String data, String algorithm, String format, String[] extraParams) throws IOException, AOFormatFileException, PrivilegedActionException;

    
    /**
     * Realiza una firma en cascada (Contrafirma) sobre una firma. Se contrafirman todos los
     * nodos hoja salvo que mediante <i>extraParams</i> se indique el par&aacute;metro
     * "{@code target=tree}", en cuyo caso se contrafirmar&aacute;n todos los nodos del
     * &aacute;rbol.
     * @param sign Firma electr&oacute;nica que se desea contrafirmar.
     * @param algorithm Algoritmo de firma.
     * @param format Formato de firma.
     * @param extraParams Par&aacute;metros adicionales para configurar la operaci&oacute;n.
     * @return Contrafirma resultante en Base64.
     * @throws IOException Cuando se produzca algun error durante la operaci&oacute;n.
     * @throws AOFormatFileException Cuando se indica un formato de firma no soportado o no
     * se puede identificar el formato de la firma.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     */
    String counterSign(String sign, String algorithm, String format, String[] extraParams) throws IOException, AOFormatFileException, PrivilegedActionException;
    
    
    /** Devuelve la estructura de firmantes de una firma electr&oacute;nica. Los
     * firmantes se separan por '\n' y comienzan por tantos '\t' como el nivel
     * en el que est&aacute;n.<br>
     * <br>
     * Por ejemplo:
     * <ul>
     * <li>Firma en paralelo: "firmante1\nfirmante2\nfirmante3"</li>
     * <li>Firma en cascada: "firmante1\n\tfirmante2\n\t\tfirmante3"</li>
     * <li>Firma 'compleja': "firmante1\n\tfirmante2\n\tfirmante3\n\t\tfirmante4\nfirmante5"</li>
     * </ul>
     * Si no se reconoce el formato de firma, se devuelve {@code null}.
     * @param signB64 Firma en Base64.
     * @return &Aacute;rbol de firmantes.
     * @throws IOException Cuabndo se produzca un error al decodificar el base64.
     * @throws AOFormatFileException Cuando se indica un formato de firma no soportado.
     * @throws NullPointerException Cuando se introduce un par&aacute;metro nulo.
     */
    String getSignersStructure(String signB64) throws IOException, AOFormatFileException ;

    
    /**
     * Muestra un di&aacute;logo modal que permite al usuario seleccionar
     * el directorio y el nombre de fichero para el guardado de datos.
     * @param data Datos en Base64 que se desean guardar
     * @param title T&iacute;tulo para el di&aacute;logo.
     * @param fileName Nombre que se debe proponer al usuario en el di&aacute;logo para
     * guardar el fichero. Puede ser nulo.
     * @param extension Extensi&oacute;n del fichero a guardar (que habitualmente indica el tipo)
     * @param description Descripci&oacute;n del tipo de fichero que se desea guardar.
     * @return {@code true} en caso de guardarse correctamente, {@code false} en caso
     * contrario.
     * @throws IOException Cuando ocurre alg&uacute;n error en el guardado del fichero.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     */
    boolean saveDataToFile(String data, String title, String fileName, String extension, String description) throws IOException, PrivilegedActionException;

    
    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero del
     * que se devolver&aacute; el contenido en Base64. Si el usuario cancela la operaci&oacute;n
     * de selecci&oacute;n del fichero se devuelve {@code null}.
     * @param title T&iacute;tulo para el di&aacute;logo.
     * @param extensions Extensiones de b&uacute;squeda.
     * @param description Descripci&oacute;n del tipo de fichero que se desea cargar.
     * @return El contenido del fichero codificado en Base64.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura del fichero.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     */
    String getFileContent(String title, String extensions, String description) throws IOException, PrivilegedActionException;


    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero
     * del que se recuperar&aacute; su ruta absoluta. Si no se selecciona
     * ning&uacute;n fichero, se devuelve {@code null}.
     * @param title T&iacute;tulo para el di&aacute;logo.
     * @param extensions Extensiones de b&uacute;squeda.
     * @param description Descripci&oacute;n del tipo de fichero que se desea cargar.
     * @return Nombre del fichero seleccionado.
     * @throws IOException Cuando se produce un error al seleccionar el fichero.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     */
    String loadFilePath(String title, String extensions, String description) throws IOException, PrivilegedActionException;
    
    
    /** Decodifica un texto en Base64. Si se introducen datos nulos se
     * devuelve {@code null}.
     * @param data Datos en Base64.
     * @param charset Juego de caracteres a utilizar en el texto de salida.
     *                El nombre de este debe seguir las convenciones de la <a href="http://ietf.org/rfc/rfc2278.txt">RFC2278</a>, 
     *                soport&aacute;ndose al menos los siguientes:
     *                <ul>
     *                 <li>US-ASCII</li>
     *                 <li>ISO-8859-1</li>
     *                 <li>UTF-8</li>
     *                 <li>UTF-16BE</li>
     *                 <li>UTF-16LE</li>
     *                 <li>UTF-16</li>
     *                </ul>  
     * @return Texto decodificado.
     * @throws IOException 
     */
    String getTextFromBase64(String data, String charset) throws IOException;

    
    /** Codifica un texto plano a Base64. Si se introduce un texto nulo se
     * devuelve {@code null}.
     * @param plainText Texto plano.
     * @param charset Juego de caracteres del texto.
     * 				  El nombre de este debe seguir las convenciones de la <a href="http://ietf.org/rfc/rfc2278.txt">RFC2278</a>, 
     *                soport&aacute;ndose al menos los siguientes:
     *                <ul>
     *                 <li>US-ASCII</li>
     *                 <li>ISO-8859-1</li>
     *                 <li>UTF-8</li>
     *                 <li>UTF-16BE</li>
     *                 <li>UTF-16LE</li>
     *                 <li>UTF-16</li>
     *                </ul>  
     * @return Texto codificado en Base64.
     * @throws IOException Cuando se indica una codificaci&oacute;n no v&aacute;lida.
     */
    public String getBase64FromText(String plainText, String charset) throws IOException;
}
