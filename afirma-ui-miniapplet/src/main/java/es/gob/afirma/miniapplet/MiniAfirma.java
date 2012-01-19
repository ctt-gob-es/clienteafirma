/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.miniapplet;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
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
 * @version 1.01
 */
interface MiniAfirma {

    /** Firma unos datos seg&uacute;n la configuracion proporcionada.
     * La configuraci&oacute;n que se puede proporcionar es el algoritmo,
     * el formato de firma y par&aacute;metros adicionales del formato particular.
     * Estos par&aacute;metros extra se indicar&aacute;n como una cadena de
     * m&uacute;ltiples l&iacute;neas con la forma {@code CLAVE=VALOR}, en donde
     * {@code CLAVE} es el nombre de la propiedad y {@code VALOR} el valor asignado
     * a esta. Para utilizar el valor por defecto de una propiedad se dejar&aacute;
     * de indicar esta en el listado depar&aacute;metros.
     * @param data Datos a firmar en Base64.
     * @param algorithm Algoritmo de firma.
     * @param format Formato de firma.
     * @param extraParams Par&aacute;metros adicionales para configurar la operac&oacute;n.
     * @return Firma electr&oacute;nica resultante en Base64.
     * @throws IOException Cuando se produce un error durante la firma electr&oacute;nica.
     * @throws AOFormatFileException Cuando se indica un formato de firma no soportado.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     */
    String sign(String data, String algorithm, String format, String extraParams) throws AOFormatFileException, PrivilegedActionException, IOException;

    /** Realiza la firma paralela (cofirma) de unos datos. La cofirma de una firma requiere
     * que los datos est&eacute;n contenidos en la firma original o que se indiquen de
     * forma externa. Si no se proporcionasen los datos, &uacute;nicamente se realizar&aacute;
     * la cofirma si el algoritmo de firma indicado conincide con el de la firma ya existente.
     * Tanto si la firma est&aacute; contenida en los datos (por ejemplo, en OOXML, ODF, PDF,
     * etc.) como si los datos est&aacute;n contenidos en la firma, el par&aacute;metro
     * <i>data</i> debe establecerse a <code>null</code> (s&oacute;lo se establece si firma y
     * datos se encuentran en erchivos independientes).
     * Es posible configurar la multifirma generada por medio de una serie de par&aacute;metros
     * extra propios de cada formato de firma (consultar la documentaci&oacute;n de cada formato
     * particular). Estos par&aacute;metros extra se indicar&aacute;n como una cadena de
     * m&uacute;ltiples l&iacute;neas con la forma {@code CLAVE=VALOR}, en donde
     * {@code CLAVE} es el nombre de la propiedad y {@code VALOR} el valor asignado
     * a esta. Para utilizar el valor por defecto de una propiedad se dejar&aacute;
     * de indicar esta en el listado depar&aacute;metros.
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
    String coSign(String sign, String data, String algorithm, String format, String extraParams) throws AOFormatFileException, PrivilegedActionException, IOException;

    /** Realiza una firma en cascada (Contrafirma) sobre una firma. Se contrafirman todos los
     * nodos hoja salvo que mediante {@code extraParams} se indique el par&aacute;metro
     * "{@code target=tree}", en cuyo caso se contrafirmar&aacute;n todos los nodos del
     * &aacute;rbol.
     * Los par&aacute;metro adicionales permiten configurar m&uacute;ltiples opciones de la
     * contrafirma. Consulte la documentaci&oacute;n de cada formato de firma particular para
     * conocer cu&aacute;les puede utilizar. Estos par&aacute;metros extra se indicar&aacute;n
     * como una cadena de m&uacute;ltiples l&iacute;neas con la forma {@code CLAVE=VALOR}, en
     * donde {@code CLAVE} es el nombre de la propiedad y {@code VALOR} el valor asignado a
     * esta. Para utilizar el valor por defecto de una propiedad se dejar&aacute; de indicar
     * esta en el listado depar&aacute;metros.
     * @param sign Firma electr&oacute;nica que se desea contrafirmar.
     * @param algorithm Algoritmo de firma.
     * @param format Formato de firma.
     * @param extraParams Par&aacute;metros adicionales para configurar la operaci&oacute;n.
     * @return Contrafirma resultante en Base64.
     * @throws IOException Cuando se produce algun error durante la operaci&oacute;n.
     * @throws AOFormatFileException Cuando se indica un formato de firma no soportado o no
     * se puede identificar el formato de la firma.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     */
    String counterSign(String sign, String algorithm, String format, String extraParams) throws AOFormatFileException, PrivilegedActionException, IOException;

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
     * @throws IOException Cuabndo se produce un error al decodificar el base64.
     * @throws AOFormatFileException Cuando se indica un formato de firma no soportado.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     * @throws NullPointerException Cuando se introduce un par&aacute;metro nulo.
     */
    String getSignersStructure(String signB64) throws IOException, PrivilegedActionException, AOFormatFileException;

    /** Muestra un di&aacute;logo modal que permite al usuario seleccionar
     * el directorio y el nombre de fichero para el guardado de datos.
     * @param data Datos en Base64 que se desean guardar
     * @param title T&iacute;tulo para el di&aacute;logo.
     * @param fileName Nombre que se debe proponer al usuario en el di&aacute;logo para
     * guardar el fichero. Puede ser nulo.
     * @param extension Extensi&oacute;n del fichero a guardar. Ejemplos, "csig", "pdf", "xsig"...
     * @param description Descripci&oacute;n del tipo de fichero que se desea guardar.
     * @return {@code true} en caso de guardarse correctamente, {@code false} en caso
     * contrario.
     * @throws IOException Cuando ocurre alg&uacute;n error en el guardado del fichero.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     */
    boolean saveDataToFile(String data, String title, String fileName, String extension, String description) throws PrivilegedActionException, IOException;

    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero del
     * que se devolver&aacute; el contenido en Base64. Si el usuario cancela la operaci&oacute;n
     * de selecci&oacute;n del fichero se devuelve {@code null}.
     * @param title T&iacute;tulo para el di&aacute;logo.
     * @param extensions Extensiones de b&uacute;squeda separadas por comas (',').
     * Por ejemplo: "pdf,xml,doc".
     * @param description Descripci&oacute;n del tipo de fichero que se desea cargar.
     * @return El contenido del fichero codificado en Base64.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     * @deprecated Sustituir por <code>getFileNameContentBase64(String, String, String)</code>.
     */
    @Deprecated
    String getFileContent(String title, String extensions, String description) throws PrivilegedActionException;

    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero del
     * que se devolver&aacute; su nombre y su contenido en formato texto unicode. El
     * resultado devuelto es una cadena con el nombre y el contenido separados por el
     * car&aacute;cter '|'. Si el usuario cancela la operaci&oacute;n de selecci&oacute;n
     * del fichero se devuelve {@code null}.
     * @param title T&iacute;tulo para el di&aacute;logo.
     * @param extensions Extensiones de b&uacute;squeda separadas por comas (',').
     * @param description Descripci&oacute;n del tipo de fichero que se desea cargar.
     * @return El nombre del fichero y su contenido en unicode.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     * @deprecated Los ficheros deben cargarse en base64 (getFileNameContentBase64) y despu&eacute;s
	 * convertirse a texto con la codificaci&oacute;n que se desee (getTextFromBase64).  */
    @Deprecated
    String getFileNameContentText(final String title, final String extensions, final String description) throws PrivilegedActionException;

    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de m&uacute;ltiples ficheros de los
     * que se devolver&aacute; sus nombres y sus contenidos en formato texto unicode. El
     * resultado devuelto es un array en el que cada elemento contiene, por cada fichero seleccionado,
     * su nombre y su contenido separados por el car&aacute;cter '|'.
     * Si el usuario cancela la operaci&oacute;n de selecci&oacute;n
     * del fichero se devuelve {@code null}.
     * @param title T&iacute;tulo para el di&aacute;logo.
     * @param extensions Extensiones de b&uacute;squeda.
     * @param description Descripci&oacute;n del tipo de fichero que se desea cargar.
     * @return Array con los nombres del ficheros y sus contenidos en unicode.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     * @deprecated Los ficheros deben cargarse en base64 (getMultiFileNameContentBase64) y
     * despu&eacute;s convertirse a texto con la codificaci&oacute;n que se desee (getTextFromBase64). */
    @Deprecated
    String[] getMultiFileNameContentText(final String title, final String extensions, final String description) throws PrivilegedActionException;

    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero del
     * que se devolver&aacute; su nombre y su contenido en Base64. El resultado
     * devuelto es una cadena con el nombre y el contenido separados por el
     * car&aacute;cter '|'. Si el usuario cancela la operaci&oacute;n de selecci&oacute;n
     * del fichero se devuelve {@code null}.
     * @param title T&iacute;tulo para el di&aacute;logo.
     * @param extensions Extensiones de b&uacute;squeda separadas por comas (',').
     * @param description Descripci&oacute;n del tipo de fichero que se desea cargar.
     * @return El nombre del fichero y su contenido en base64.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura del fichero.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     */
    String getFileNameContentBase64(final String title, final String extensions, final String description) throws IOException, PrivilegedActionException;

    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de m&uacute;ltiples ficheros de los
     * que se devolver&aacute; sus nombres y sus contenidos en Base64. El
     * resultado devuelto es un array en el que cada elemento contiene, por cada fichero seleccionado,
     * su nombre y su contenido (en Base64) separados por el car&aacute;cter '|'.
     * Si el usuario cancela la operaci&oacute;n de selecci&oacute;n
     * del fichero se devuelve {@code null}.
     * @param title T&iacute;tulo para el di&aacute;logo.
     * @param extensions Extensiones de b&uacute;squeda.
     * @param description Descripci&oacute;n del tipo de fichero que se desea cargar.
     * @return Array con los nombres del ficheros y sus contenidos en Base64.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura del fichero.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad. */
    String[] getMultiFileNameContentBase64(final String title, final String extensions,final String description) throws IOException, PrivilegedActionException;

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
     * @throws IOException Cuando se indica una codificaci&oacute;n no v&aacute;lida.
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
     * @throws UnsupportedEncodingException Cuando se indica una codificaci&oacute;n no v&aacute;lida.
     */
    String getBase64FromText(String plainText, String charset) throws UnsupportedEncodingException;

    /**
     * Recupera el error producido durante la &uacute;ltima operaci&oacute;n
     * realizada por el applet. El texto obtenido consiste en la cadena de
     * excepciones cualificadas que produjeron el error, separadas por la cadena ": "
     * y seguidas igualmente por ": " y el mensaje del error. Por ejemplo:
     * <p>{@code es.gob.afirma.keystores.main.common.AOCertificatesNotFoundException:
     * El almacen no contenia entradas validas}</p>
     * Si no se produjo ning&uacute;n error durante la
     * operaci&oacute;n, se devolver&aacute; {@code null}.
     * @return Mensaje de error.
     */
    String getErrorMessage();
}
