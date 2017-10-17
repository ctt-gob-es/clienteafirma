/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.miniapplet;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.PrivilegedActionException;
import java.security.cert.CertificateEncodingException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.ExtraParamsProcessor.IncompatiblePolicyException;

/** Contiene los puntos de entrada de las funcionalidades criptogr&aacute;ficas
 * del MiniApplet del Cliente AFirma.
 * El MiniApplet acepta como par&aacute;metros de entrada (desde el HTML en el momento de la carga):
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
 *   En caso de necesitarse un fichero externo (biblioteca, almac&eacute;n en archivo, etc.) o una contrase&ntilde;a, estos se
 *   solicitan al usuario mediante di&aacute;logos gr&aacute;ficos
 *  </dd>
 * </dl>
 * @version 1.02 */
interface MiniAfirma {

    /** Firma unos datos seg&uacute;n la configuracion proporcionada.
     * Los datos se deber&aacute;n haber establecido previamente mediante el uso
     * reiterado del m&eacute;todo {@code addData(String)}.
     * La configuraci&oacute;n que se puede proporcionar es el algoritmo,
     * el formato de firma y par&aacute;metros adicionales del formato particular.
     * Estos par&aacute;metros extra se indicar&aacute;n como una cadena de
     * m&uacute;ltiples l&iacute;neas con la forma {@code CLAVE=VALOR}, en donde
     * {@code CLAVE} es el nombre de la propiedad y {@code VALOR} el valor asignado
     * a esta. Para utilizar el valor por defecto de una propiedad se dejar&aacute;
     * de indicar esta en el listado de par&aacute;metros.
     * @param algorithm Algoritmo de firma.
     * @param format Formato de firma.
     * @param extraParams Par&aacute;metros adicionales para configurar la operac&oacute;n.
     * @return Firma electr&oacute;nica resultante en Base64. Si el tama&ntilde;o del resultado es demasiado grande ser&aacute;
     *         necesario realizar llamadas adicionales a <code>getRemainingData()</code> y concatenar las respuestas hasta
     *         que este &uacute;ltimo m&eacute;todo devuelva '%%EOF%%'
     * @throws IOException Cuando se produce un error durante la firma electr&oacute;nica.
     * @throws es.gob.afirma.core.AOFormatFileException Cuando se indica un formato de firma no soportado.
     * @throws es.gob.afirma.core.InvalidLibraryException Cuando se detecta una versi&oacute;n no v&aacute;lida de una biblioteca.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     * @throws es.gob.afirma.core.MissingLibraryException Cuando no se encuentra una biblioteca necesaria para la operaci&oacute;n.
     * @throws AOException Cuando se produce un error desconocido.
     * @throws CertificateEncodingException Cuando no se puede codificar el certificado usado para la firma.
     * @throws IncompatiblePolicyException Si se pide una pol&iacute;tica de firma concreta (por nombre, no indicando los par&aacute;metros
     *                                     individualmente) incompatible con el formato de firma indicado. */
    String sign(String algorithm, String format, String extraParams) throws PrivilegedActionException,
                                                                            IOException,
                                                                            AOException,
                                                                            CertificateEncodingException,
                                                                            IncompatiblePolicyException;

    /** Realiza la firma paralela (cofirma) de unos datos. La cofirma de una firma requiere
     * que los datos est&eacute;n contenidos en la firma original o que se indiquen como
     * par&aacute;metro de esta funci&oacute;n. Si no se proporcionasen los datos,
     * &uacute;nicamente se realizar&aacute; la cofirma si el algoritmo de firma indicado
     * conincide con el de la firma ya existente.<br>
     * La firmar se deber&aacute; haber establecido previamente mediante el uso
     * reiterado del m&eacute;todo {@code addData(String)}.
     * Tanto si la firma est&aacute; contenida en los datos (por ejemplo, en OOXML, ODF, PDF,
     * etc.) como si los datos est&aacute;n contenidos en la firma, el par&aacute;metro
     * <i>data</i> debe establecerse a {@code null}. S&oacute;lo se establece si firma y
     * datos se encuentran en archivos independientes.
     * Es posible configurar la multifirma generada por medio de una serie de par&aacute;metros
     * extra propios de cada formato de firma (consultar la documentaci&oacute;n de cada formato
     * particular). Estos par&aacute;metros extra se indicar&aacute;n como una cadena de
     * m&uacute;ltiples l&iacute;neas con la forma {@code CLAVE=VALOR}, en donde
     * {@code CLAVE} es el nombre de la propiedad y {@code VALOR} el valor asignado
     * a esta. Para utilizar el valor por defecto de una propiedad se dejar&aacute;
     * de indicar esta en el listado de par&aacute;metros.
     * @param data Datos en Base64 que se firmaron originalmente o {@code null} si no son necesarios.
     * @param algorithm Algoritmo de firma.
     * @param format Formato de firma.
     * @param extraParams Par&aacute;metros adicionales para configurar la operac&oacute;n.
     * @return Cofirma resultante en Base64. Si el tama&ntilde;o del resultado es demasiado grande ser&aacute;
     *         necesario realizar llamadas adicionales a <code>getRemainingData()</code> y concatenar las respuestas hasta
     *         que este &uacute;ltimo m&eacute;todo devuelva '%%EOF%%'
	 * @throws IOException Cuando se produce un error durante la cofirma electr&oacute;nica.
     * @throws es.gob.afirma.core.AOFormatFileException Cuando se indica un formato de firma no soportado o no
     * se puede identificar el formato de la firma.
     * @throws es.gob.afirma.core.InvalidLibraryException Cuando se detecta una versi&oacute;n no v&aacute;lida de una biblioteca.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     * @throws es.gob.afirma.core.MissingLibraryException Cuando no se encuentra una biblioteca necesaria para la operaci&oacute;n.
     * @throws AOException Cuando se produce un error desconocido.
     * @throws CertificateEncodingException Cuando no se puede codificar el certificado usado para la firma.
     * @throws IncompatiblePolicyException Si se pide una pol&iacute;tica de firma concreta (por nombre, no indicando los par&aacute;metros
     *                                     individualmente) incompatible con el formato de firma indicado. */
    String coSign(String data, String algorithm, String format, String extraParams) throws PrivilegedActionException,
                                                                                           IOException,
                                                                                           AOException,
                                                                                           CertificateEncodingException,
                                                                                           IncompatiblePolicyException;

    /** Realiza una firma en cascada (Contrafirma) sobre una firma. La firma se deber&aacute;
     * haber establecido previamente mediante el uso reiterado del m&eacute;todo
     * {@code addData(String)}.<br>
     * Se contrafirman todos los nodos hoja salvo que mediante {@code extraParams} se indique
     * el par&aacute;metro "{@code target=tree}", en cuyo caso se contrafirmar&aacute;n todos
     * los nodos del &aacute;rbol.<br>
     * Los par&aacute;metro adicionales permiten configurar m&uacute;ltiples opciones de la
     * contrafirma. Consulte la documentaci&oacute;n de cada formato de firma particular para
     * conocer cu&aacute;les puede utilizar. Estos par&aacute;metros extra se indicar&aacute;n
     * como una cadena de m&uacute;ltiples l&iacute;neas con la forma {@code CLAVE=VALOR}, en
     * donde {@code CLAVE} es el nombre de la propiedad y {@code VALOR} el valor asignado a
     * esta. Para utilizar el valor por defecto de una propiedad se dejar&aacute; de indicar
     * esta en el listado de par&aacute;metros.
     * @param algorithm Algoritmo de firma.
     * @param format Formato de firma.
     * @param extraParams Par&aacute;metros adicionales para configurar la operaci&oacute;n.
     * @return Contrafirma resultante en Base64. Si el tama&ntilde;o del resultado es demasiado grande ser&aacute;
     *         necesario realizar llamadas adicionales a <code>getRemainingData()</code> y concatenar las respuestas hasta
     *         que este &uacute;ltimo m&eacute;todo devuelva '%%EOF%%'
     * @throws IOException Cuando se produce algun error durante la operaci&oacute;n.
     * @throws es.gob.afirma.core.AOFormatFileException Cuando se indica un formato de firma no soportado o no
     * se puede identificar el formato de la firma.
     * @throws es.gob.afirma.core.InvalidLibraryException Cuando se detecta una versi&oacute;n no v&aacute;lida de una biblioteca.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     * @throws es.gob.afirma.core.MissingLibraryException Cuando no se encuentra una biblioteca necesaria para la operaci&oacute;n.
     * @throws AOException Cuando se produce un error desconocido.
     * @throws CertificateEncodingException Cuando no se puede codificar el certificado usado para la firma.
     * @throws IncompatiblePolicyException Si se pide una pol&iacute;tica de firma concreta (por nombre, no indicando los par&aacute;metros
     *                                     individualmente) incompatible con el formato de firma indicado. */
    String counterSign(String algorithm, String format, String extraParams) throws PrivilegedActionException,
                                                                                   IOException,
                                                                                   AOException,
                                                                                   CertificateEncodingException,
                                                                                   IncompatiblePolicyException;

    /** Firma/multifirma unos datos seg&uacute;n la configuracion proporcionada y
     * guarda el resultado en disco.
     * Los datos se deber&aacute;n haber establecido previamente mediante el uso
     * reiterado del m&eacute;todo {@code addData(String)}.
     * La configuraci&oacute;n que se puede proporcionar es el algoritmo,
     * el formato de firma y par&aacute;metros adicionales del formato particular.
     * Estos par&aacute;metros extra se indicar&aacute;n como una cadena de
     * m&uacute;ltiples l&iacute;neas con la forma {@code CLAVE=VALOR}, en donde
     * {@code CLAVE} es el nombre de la propiedad y {@code VALOR} el valor asignado
     * a esta. Para utilizar el valor por defecto de una propiedad se dejar&aacute;
     * de indicar esta en el listado de par&aacute;metros.
     * @param op Operaci&oacute;n criptogr&aacute;fica a realizar (sign, cosign, countersign)
     * @param algorithm Algoritmo de firma.
     * @param format Formato de firma.
     * @param extraParams Par&aacute;metros adicionales para configurar la operac&oacute;n.
     * @param fileName Nombre propuesto para el fichero de salida.
     * @return Firma electr&oacute;nica resultante en Base64. Si el tama&ntilde;o del resultado es demasiado grande ser&aacute;
     *         necesario realizar llamadas adicionales a <code>getRemainingData()</code> y concatenar las respuestas hasta
     *         que este &uacute;ltimo m&eacute;todo devuelva '%%EOF%%'
     * @throws IOException Cuando se produce un error durante la firma electr&oacute;nica.
     * @throws es.gob.afirma.core.AOFormatFileException Cuando se indica un formato de firma no soportado.
     * @throws es.gob.afirma.core.InvalidLibraryException Cuando se detecta una versi&oacute;n no v&aacute;lida de una biblioteca.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     * @throws es.gob.afirma.core.MissingLibraryException Cuando no se encuentra una biblioteca necesaria para la operaci&oacute;n.
     * @throws AOException Cuando se produce un error desconocido.
     * @throws CertificateEncodingException Cuando no se puede codificar el certificado usado para la firma.
     * @throws IncompatiblePolicyException Si se pide una pol&iacute;tica de firma concreta (por nombre, no indicando los par&aacute;metros
     *                                     individualmente) incompatible con el formato de firma indicado. */
    String signAndSaveToFile(String op, String algorithm, String format, String extraParams, String fileName) throws PrivilegedActionException,
																									    IOException,
																									    AOException,
																									    CertificateEncodingException,
																									    IncompatiblePolicyException;

    /** Muestra un di&aacute;logo modal que permite al usuario seleccionar
     * el directorio y el nombre de fichero para el guardado de datos.
     * @param title T&iacute;tulo para el di&aacute;logo.
     * @param fileName Nombre que se debe proponer al usuario en el di&aacute;logo para
     *                 guardar el fichero. Puede ser nulo.
     * @param extension Extensi&oacute;n del fichero a guardar. Ejemplos, "csig", "pdf", "xsig"...
     * @param description Descripci&oacute;n del tipo de fichero que se desea guardar.
     * @return {@code true} en caso de guardarse correctamente, {@code false} en caso
     *         contrario.
     * @throws IOException Cuando ocurre alg&uacute;n error en el guardado del fichero.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad. */
    boolean saveDataToFile(String title, String fileName, String extension, String description) throws PrivilegedActionException, IOException;

    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero del
     * que se devolver&aacute; su nombre y su contenido en Base64. El resultado
     * devuelto es una cadena con el nombre y el contenido separados por el
     * car&aacute;cter '|'. Si el usuario cancela la operaci&oacute;n de selecci&oacute;n
     * del fichero se devuelve {@code null}.
     * @param title T&iacute;tulo para el di&aacute;logo.
     * @param extensions Extensiones de b&uacute;squeda separadas por comas (',').
     * @param description Descripci&oacute;n del tipo de fichero que se desea cargar.
     * @param filePath Ruta del fichero seleccionado por defecto.
     * @return El nombre del fichero y su contenido en base64. Si el tama&ntilde;o del resultado es demasiado grande ser&aacute;
     *         necesario realizar llamadas adicionales a <code>getRemainingData()</code> y concatenar las respuestas hasta
     *         que este &uacute;ltimo m&eacute;todo devuelva '%%EOF%%'
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura del fichero.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad. */
    String getFileNameContentBase64(final String title, final String extensions, final String description, final String filePath) throws IOException, PrivilegedActionException;

    /** Muestra un di&aacute;logo modal para la selecci&oacute;n de m&uacute;ltiples ficheros, de los
     * que se devolver&aacute; sus nombres y sus contenidos en Base64.
     * El resultado devuelto es un array en el que cada elemento contiene, por cada fichero seleccionado,
     * su nombre y su contenido (en Base64) separados por el car&aacute;cter '|'.
     * Si el usuario cancela la operaci&oacute;n de selecci&oacute;n
     * del fichero se devuelve {@code null}.
     * @param title T&iacute;tulo para el di&aacute;logo.
     * @param extensions Extensiones de b&uacute;squeda.
     * @param description Descripci&oacute;n del tipo de fichero que se desea cargar.
     * @param filePath Ruta del fichero seleccionado por defecto.
     * @return Array con los nombres del ficheros y sus contenidos en Base64.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura del fichero.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad. */
    String[] getMultiFileNameContentBase64(final String title, final String extensions, final String description, final String filePath) throws IOException, PrivilegedActionException;

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
     *                Adicionalmente, en vez del nombre de la codificaci&oacute;n, se pueden usar dos identificadores especiales:
     *                <dl>
     *                 <dt>default</dt>
     *                  <dd>Se utilizar&aacute; la codificaci&oacute;n por defecto del sistema. Es equivalente a usar <code>null</code>.</dd>
     *                 <dt>auto</dt>
     *                  <dd>Se realizar&aacute; una autodetecci&oacute;n de la codificaci&oacute;n del texto. Esta autodetecci&oacute;n no tiene una fiabilidad del 100%, y en ciertas ocasiones puede devolver una codificaci&oacute;n inapropiada.</dd>
     *                </dl>
     * @return Texto decodificado. Si el tama&ntilde;o del resultado es demasiado grande ser&aacute;
     *         necesario realizar llamadas adicionales a <code>getRemainingData()</code> y concatenar las respuestas hasta
     *         que este &uacute;ltimo m&eacute;todo devuelva '%%EOF%%'
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
     *                Adicionalmente, en vez del nombre de la codificaci&oacute;n, se puede usar un identificador especial:
     *                <dl>
     *                 <dt>default</dt>
     *                  <dd>Se utilizar&aacute; la codificaci&oacute;n por defecto del sistema. Es equivalente a usar <code>null</code>.</dd>
     *                </dl>
     * @return Texto codificado en Base64.
     * @throws UnsupportedEncodingException Cuando se indica una codificaci&oacute;n no v&aacute;lida.
     */
    String getBase64FromText(String plainText, String charset) throws UnsupportedEncodingException;

    /** Recupera el error producido durante la &uacute;ltima operaci&oacute;n
     * realizada por el applet. El texto obtenido ser&aacute; el declarado por el
     * error producido. En caso de no haber ning&uacute;n mensaje de error, se
     * utilizar&aacute; el texto completo declarado por este. Com&uacute;nmente,
     * el tipo cualificado del error seguido del texto del mismo.Por ejemplo:
     * <p>{@code es.gob.afirma.keystores.AOCertificatesNotFoundException:
     * El almacen no contenia entradas validas}</p>
     * Si no se produjo ning&uacute;n error durante la
     * operaci&oacute;n, se devolver&aacute; {@code null}.
     * @return Mensaje de error. */
    String getErrorMessage();

    /** Recupera el tipo de error producido durante la &uacute;ltima operaci&oacute;n
     * realizada por el applet. El texto obtenido consiste en el nombre de la
     * excepci&oacute;n que genero el error. Si no se produjo ning&uacute;n error durante
     * la operaci&oacute;n, se devolver&aacute; {@code null}.
     * @return Tipo de error. */
    String getErrorType();

    /** Imprime en el logger el texto "MiniApplet cargado y en ejecuci&oacute;n". El uso de
	 * este m&eacute;todo permite determinar si el applet se encuentra inicializado.
     * @return El texto "MiniApplet cargado y en ejecuci&oacute;n" */
	String echo();

	/** Obtiene los datos restantes que no se entregaron por exceso de tama&ntilde;o en cualquiera de los m&eacute;todos
	 *  p&uacute;blicos del Applet.
	 * @return Datos restantes que no se entregaron por exceso de tama&ntilde;o, en formato Base64. Este Base64 puede estar
	 *         incompleto, y debe llamarse de forma continuada hasta que se obtenga la cadena '%%EOF%%' que indica que no hay
	 *         m&aacute;s datos restantes. El resultado de la operaci&oacute;n ser&aacute; la concatenaci&oacute;n directa
	 *         (sin decodificaci&oacute;n previa) de las cadenas resultantes de las llamadas a este m&eacute;todo (excluyendo
	 *         la &uacute;tima que devuelve %%EOF%%), la cual ya sera susceptible de ser descodificada en Base64.
	 * @throws IOException En caso de problemas en el tratamiento de los datos */
	String getRemainingData() throws IOException;

	/** A&ntilde;ade datos mediante porciones de un total codificado en Base64.
	 * Para establecer una cantidad grande de datos debe trocearse el Base64 de origen en porciones de menor
	 * tama&ntilde;o y realizarse varias llamadas a este m&eacute;todo hasta que se haya establecido el total. Las porciones
	 * establecidas se concatenar&aacute;n como texto, descodificandose porteriormente el resultado de las concatenaciones
	 * como un Base64 &uacute;nico.
	 * Si la cantidad de datos es peque&ntilde;a, puede establecerla la totalidad con una &uacute;nica llamada
	 * proporcionando una cadena Base64 completa.
	 * Los datos acumulados se reinicializan a cadena vac&iacute;a tras una operaci&oacute;n de firma, cofirma,
	 * contrafirma o guardado.
	 * @param data Datos o porci&oacute;n de estos a procesar. Si se proporciona <code>null</code> se reinicializa
	 *             el acumulado (a cadena vac&iacute;a) */
	void addData(String data);

	/** Obtiene el registro (<i>log</i>), en formato XML, acumulado desde la carga inicial del Applet.
	 * @return <i>log</i> en formato XML del Applet */
	String getCurrentLog();

	/** Procesa un lote de firmas, que se realizar&aacute;n siempre de forma tri&aacute;sica.
	 * Los lotes deben proporcionase definidos en un fichero XML con el siguiente esquema XSD:
	 * <pre>
	 * &lt;xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema"&gt;
  	 * &lt;xs:element name="signbatch"&gt;
	 *     &lt;xs:complexType&gt;
	 *       &lt;xs:sequence&gt;
	 *         &lt;xs:element name="singlesign" maxOccurs="unbounded" minOccurs="1"&gt;
	 *           &lt;xs:complexType&gt;
	 *             &lt;xs:sequence&gt;
	 *               &lt;xs:element type="xs:string" name="datasource"/&gt;
	 *               &lt;xs:element name="format"&gt;
	 *                 &lt;xs:simpleType&gt;
	 *                   &lt;xs:restriction base="xs:string"&gt;
	 *                     &lt;xs:enumeration value="XAdES"/&gt;
	 *                     &lt;xs:enumeration value="CAdES"/&gt;
	 *                     &lt;xs:enumeration value="PAdES"/&gt;
	 *                   &lt;/xs:restriction&gt;
	 *                 &lt;/xs:simpleType&gt;
	 *               &lt;/xs:element&gt;
	 *               &lt;xs:element name="suboperation"&gt;
	 *                 &lt;xs:simpleType&gt;
	 *                   &lt;xs:restriction base="xs:string"&gt;
	 *                     &lt;xs:enumeration value="sign"/&gt;
	 *                     &lt;xs:enumeration value="cosign"/&gt;
	 *                   &lt;/xs:restriction&gt;
	 *                 &lt;/xs:simpleType&gt;
	 *               &lt;/xs:element&gt;
	 *               &lt;xs:element name="extraparams"&gt;
	 *                 &lt;xs:simpleType&gt;
	 *                  &lt;xs:restriction  base="xs:base64Binary" /&gt;
	 *                 &lt;/xs:simpleType&gt;
	 *               &lt;/xs:element&gt;
	 *               &lt;xs:element name="signsaver"&gt;
	 *                 &lt;xs:complexType&gt;
	 *                   &lt;xs:sequence&gt;
	 *                     &lt;xs:element type="xs:string" name="class"/&gt;
	 *                     &lt;xs:element name="config"&gt;
	 *                       &lt;xs:simpleType&gt;
	 *                         &lt;xs:restriction  base="xs:base64Binary" /&gt;
	 *                       &lt;/xs:simpleType&gt;
	 *                     &lt;/xs:element&gt;
	 *                   &lt;/xs:sequence&gt;
	 *                 &lt;/xs:complexType&gt;
	 *               &lt;/xs:element&gt;
	 *             &lt;/xs:sequence&gt;
	 *             &lt;xs:attribute type="xs:string" name="id" use="required"/&gt;
	 *           &lt;/xs:complexType&gt;
	 *         &lt;/xs:element&gt;
	 *       &lt;/xs:sequence&gt;
	 *       &lt;xs:attribute type="xs:string" name="stoponerror" use="optional"/&gt;
	 *       &lt;xs:attribute type="xs:string" name="algorithm" use="required"&gt;
	 *         &lt;xs:simpleType&gt;
	 *           &lt;xs:restriction base="xs:string"&gt;
	 *             &lt;xs:enumeration value="SHA1withRSA"/&gt;
	 *             &lt;xs:enumeration value="SHA256withRSA"/&gt;
	 *             &lt;xs:enumeration value="SHA384withRSA"/&gt;
	 *             &lt;xs:enumeration value="SHA512withRSA"/&gt;
	 *           &lt;/xs:restriction&gt;
	 *         &lt;/xs:simpleType&gt;
	 *       &lt;xs:attribute&gt;
	 *     &lt;/xs:complexType&gt;
	 *   &lt;/xs:element&gt;
	 * &lt;/xs:schema&gt;
	 * </pre>
	 * Un ejemplo de definici&oacute;n XML de lote de firmas podr&iacute;a ser
	 * este (ejemplo con dos firmas en el lote):
	 * <pre>
	 * &lt;?xml version="1.0" encoding="UTF-8" ?&gt;
	 * &lt;signbatch stoponerror="true" algorithm="SHA1withRSA"&gt;
	 *  &lt;singlesign id="f8526f7b-d30a-4720-9e35-fe3494217944"&gt;
	 *   &lt;datasource&gt;http://google.com&lt;/datasource&gt;
	 *   &lt;format&gt;XAdES&lt;/format&gt;
	 *   &lt;suboperation&gt;sign&lt;/suboperation&gt;
	 *   &lt;extraparams&gt;Iw0KI1RodSBBdW[...]QNCg==&lt;/extraparams&gt;
	 *   &lt;signsaver&gt;
	 *    &lt;class&gt;es.gob.afirma.signers.batch.SignSaverFile&lt;/class&gt;
	 *    &lt;config&gt;Iw0KI1RodSBBdWcgMT[...]wNCg==&lt;/config&gt;
	 *   &lt;/signsaver&gt;
	 *  &lt;/singlesign&gt;
	 *  &lt;singlesign id="0e9cc5de-63ee-45ee-ae02-4a6591ab9a46"&gt;
	 *   &lt;datasource&gt;SG9sYSBNdW5kbw==&lt;/datasource&gt;
	 *   &lt;format&gt;CAdES&lt;/format&gt;
	 *   &lt;suboperation&gt;sign&lt;/suboperation&gt;
	 *   &lt;extraparams&gt;Iw0KI1RodSBBdWc[...]NCg==&lt;/extraparams&gt;
	 *   &lt;signsaver&gt;
	 *    &lt;class&gt;es.gob.afirma.signers.batch.SignSaverFile&lt;/class&gt;
	 *    &lt;config&gt;Iw0KI1RodSBBdWcgMTM[...]Cg==&lt;/config&gt;
	 *   &lt;/signsaver&gt;
	 *  &lt;/singlesign&gt;
	 * &lt;/signbatch&gt;
	 * </pre>
	 * @param batchB64 XML de definici&oacute;n del lote de firmas.
	 * @param batchPresignerUrl URL del servicio remoto de preproceso de lotes de firma.
	 * @param batchPostSignerUrl URL del servicio remoto de postproceso de lotes de firma.
	 * @param extraParams Par&aacute;metros adicionales para es establecimiento de filtros de
	 *                    certificados (se ignorar&aacute;n todos los par&aacute;metros
	 *                    proporcionados que no est&eacute;n relacionados con los filtros).
	 * @return Registro del resultado general del proceso por lote, en un XML con este esquema:
	 * <pre>
	 * &lt;xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema"&gt;
	 *  &lt;xs:element name="signs"&gt;
	 *    &lt;xs:complexType&gt;
	 *      &lt;xs:sequence&gt;
	 *        &lt;xs:element name="sign" maxOccurs="unbounded" minOccurs="1"&gt;
	 *          &lt;xs:complexType&gt;
	 *            &lt;xs:sequence&gt;
	 *              &lt;xs:element name="result"&gt;
	 *                &lt;xs:simpleType&gt;
	 *                  &lt;xs:restriction base="xs:string"&gt;
	 *                    &lt;xs:enumeration value="OK"/&gt;
	 *                    &lt;xs:enumeration value="KO"/&gt;
	 *                    &lt;xs:enumeration value="NP"/&gt;
	 *                  &lt;/xs:restriction&gt;
	 *                &lt;/xs:simpleType&gt;
	 *              &lt;/xs:element&gt;
	 *              &lt;xs:element type="xs:string" name="reason" minOccurs="0"/&gt;
	 *            &lt;/xs:sequence&gt;
	 *            &lt;xs:attribute type="xs:string" name="id" use="required"/&gt;
	 *          &lt;/xs:complexType&gt;
	 *        &lt;/xs:element&gt;
	 *      &lt;/xs:sequence&gt;
	 *    &lt;/xs:complexType&gt;
	 *  &lt;/xs:element&gt;
	 * &lt;/xs:schema&gt;
	 * </pre>
	 * @throws IOException Si hay problemas de tratamiento de datos o de conectividad de red.
	 * @throws AOException En cualquier otro error.
	 * @throws PrivilegedActionException Cuando ocurre un error de seguridad, t&iacute;picamente en
	 *                                   la obtenci&oacute;n de la clave y el certificado de firma. */
	String signBatch(final String batchB64,
                     final String batchPresignerUrl,
                     final String batchPostSignerUrl,
                     final String extraParams) throws IOException, AOException,
                                                                   PrivilegedActionException;

	/**
	 * Selecciona y recupera un certificado del almac&eacute;n actual.
	 * Permite la configuraci&oacute;n de filtros de certificados mediante el uso de
	 * par&aacute;metros extra. Estos par&aacute;metros se indicar&aacute;n como una
	 * cadena de m&uacute;ltiples l&iacute;neas con la forma {@code CLAVE=VALOR}, en
	 * donde {@code CLAVE} es el nombre de la propiedad y {@code VALOR} el valor asignado
     * a esta.
	 * @param extraParams Propiedades para la configuraci&oacute;n de los filtro de
	 * certificados.
	 * @return Certitificado seleccionado codificado en base 64.
	 * @throws AOException Cuando ocurre un error grave durante la selecci&oacute;n del certificado.
	 * @throws CertificateEncodingException Cuando no se haya podido decodificar el certificado.
	 * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
	 */
	String selectCertificate(final String extraParams) throws AOException, CertificateEncodingException, PrivilegedActionException;

	/** Fija el firmante que se establezca para ser reutilizado (sin intervenci&oacute;n del usuario) en todas
	 * las operaciones posteriores hasta que se desactive esta opci&oacute;n.
	 * En las operaciones de firmante fijado por un filtro, en las que no se muestra ning&uacute;n di&aacute;logo de
	 * selecci&oacute;n de certificado, controla la aparcici&oacute;n del di&aacute;logo de solicitud de confirmaci&oacute;n de
	 * firma.
	 * @param sticky Si se establece a <code>true</code>, el firmante que se seleccione tras este establecimiento se
	 *               reutilizar&aacute; para todas las operaciones posteriores, hasta que se restablezca a <code>false</code>.
	 *               Si se establece al <code>false</code> se borra el firmante fijado si lo hubiese, por lo que se preguntar&aacute;
	 *               al usuario de nuevo con un di&aacute;logo de selecci&oacute;n de certificado la pr&oacute;xima vez que se
	 *               necesite (o un di&aacute;logo de confirmaci&oacute;n de firma si el certificado se establece un&iacute;vocamente
	 *               mediante un filtro. */
    void setStickySignatory(boolean sticky);

	/**
	 * Establece el almac&eacute;n de certificados que se debe utilizar en las subsiguientes
	 * operaciones con certificados.
	 * @param ksType Identificador del almac&eacute;n de certificados.
	 * @throws AOException Cuando se indica un tipo de almac&eacute;n no v&aacute;lido.
	 */
	void setKeyStore(String ksType) throws AOException;
}
