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

import javax.jnlp.UnavailableServiceException;

import es.gob.afirma.core.AOCancelledOperationException;


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
     * @param data Datos a firmar en Base64.
     * @param algorithm Algoritmo de firma.
     * @param format Formato de firma.
     * @param extraParams Par&aacute;metros adicionales para configurar la operac&oacute;n.
     * @return Firma electr&oacute;nica resultante en Base64.
     */
    String sign(String data, String algorithm, String format, String extraParams);

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
     */
    String coSign(String sign, String data, String algorithm, String format, String extraParams);

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
     * @param sign Firma en Base64.
     * @return &Aacute;rbol de firmantes.
     */
    String getSignersStructure(String sign);

    /**
     * Realiza una firma en cascada (Contrafirma) sobre una firma. SI no se indica lo contrario mediante <i>extraParams</i>
     * se contrafirman todos los nodos hoja
     * @param sign Firma electr&oacute;nica que se desea contrafirmar.
     * @param algorithm Algoritmo de firma.
     * @param format Formato de firma.
     * @param extraParams Par&aacute;metros adicionales para configurar la operaci&oacute;n.
     * @return Contrafirma resultante en Base64.
     */
    String counterSign(String sign, String algorithm, String format, String extraParams);

    /**
     * Muestra un di&aacute;logo modal que permite al usuario seleccionar
     * el directorio y el nombre de fichero para el guardado de datos. Se usar&aacute; las funcionalidades
     * de JNLP siempre que sea posible
     * @param data Datos en Base64 que se desean guardar
     * @param fileName Nombre que se debe proponer al usuario en el di&aacute;logo para guardar el fichero. Puede ser nulo
     * @param extension Extensi&oacute;n del fichero a guardar (que habitualmente indica el tipo)
     * @return {@code true} en caso de guardarse correctamente, {@code false} en caso
     * contrario.
     */
    boolean saveDataToFile(String data, String fileName, String extension);

    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero del
     * que se devolver&aacute; el contenido en Base64. Si ocurre un error durante
     * la operaci&oacute;n se devuelve {@code null}. Se usar&aacute; las funcionalidades
     * de JNLP siempre que sea posible
     * @return El contenido del fichero codificado en Base64.
     * @throws IOException 
     * @throws AOCancelledOperationException 
     * @throws UnavailableServiceException 
     * @throws Exception 
     */
    String getFileContent() throws AOCancelledOperationException, IOException, UnavailableServiceException, Exception;

    /** Decodifica un texto en Base64. Si se produce alg&uacute;n error se
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
     */
    String getTextFromBase64(String data, String charset) throws IOException;

    /** Codifica un texto plano a Base64. Si se produce alg&uacute;n error se
     * devuelve <code>null</code>.
     * @param plainText Texto plano.
     * @return Texto codificado en Base64.
     */
    String getBase64FromText(String plainText);

    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero
     * del que se recuperar&aacute; su ruta completa. Si no se selecciona
     * ning&uacute;n fichero, se devuelve <code>null</code>. Se usar&aacute; las funcionalidades
     * de JNLP siempre que sea posible
     * @param title T&iacute;tulo del di&aacute;logo modal.
     * @param exts Extensiones de b&uacute;squeda.
     * @param description Descripcion del tipo de ficheros buscado.
     * @return Ruta completa del fichero seleccionado. Se devuleve la ruta absoluta, pero no la can&oacute;nica
     */
    String loadFilePath(String title, String exts, String description) throws IOException, UnavailableServiceException;
    
}
