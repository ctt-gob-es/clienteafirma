/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.security.cert.X509Certificate;

/** Contiene los puntos de entrada de las funcionalidades criptogr&aacute;ficas
 * del Cliente AFirma para su uso como Applet Java. <br>
 * <br>
 * Contains AFirma cryptographic functionalities access points, for their use as
 * Applet Java. */
interface EntryPointsCrypto {

    /** Realiza el proceso de firma digital.<br>
     * <br>
     * <b>Par&aacute;metros de entrada:</b><br>
     * <dl>
     * <dt>Para la selecci&oacute;n de certificado (opcional, excluyentes), si no se especifica ninguno, se muestra una ventana al usuario para que
     * escojaentre todos los certificados que tenga instalados</dt>
     * <dd>
     * <ul>
     * <li>mandatoryCertCondition</li>
     * <li>certFilter</li>
     * </ul>
     * </dd>
     * <dt>Para la selecci&oacute;n de datos de entrada(opcional, excluyentes): Si no se especifica ninguno, se muestra una ventana al usuario para
     * que escoja un fichero a firmar</dt>
     * <dd>
     * <ul>
     * <li>data</li>
     * <li>hash</li>
     * <li>hashes (firma masiva)</li>
     * <li>fileUri</li>
     * </ul>
     * </dd>
     * <dt>Para la firma digital(opcionales): Si no se especifican, se toman los valores por defecto</dt>
     * <dd>
     * <ul>
     * <li>signatureAlgorithm</li>
     * <li>signatureFormat</li>
     * </ul>
     * </dd>
     * <dt>Par&aacute;metros de salida:</dt>
     * <dd>
     * <dl>
     * <dt>Si se produjo un error <code>(isError()==true)</code>:</dt>
     * <dd>errorMessage</dd>
     * <dt>Si no se produjo <code>(isError()==false)</code>:</dt>
     * <dd>
     * <ul>
     * <li>signatureBase64Encoded</li>
     * <li>fileUsedPath (si se han le&iacute;do los datos a firmar de un fichero)</li>
     * <li>signCertificateBase64Encoded</li>
     * </ul>
     * </dd>
     * </dl>
     * </dd>
     * </dl>
     * <br>
     * <br>
     * Performs the digital signature process.<br>
     * <br>
     * <b>Input parameters:</b><br>
     * <dl>
     * <dt>For the selection of certificates (optional, exclusive), if none is specified, a window is displayed prompting the user to choose among
     * already installed certificates.</dt>
     * <dd>
     * <ul>
     * <li>mandatoryCertCondition</li>
     * <li>certFilter</li>
     * </ul>
     * </dd>
     * <dt>For the selection of input data (optional, exclusive): if none is specified, a window is displayed, prompting the user to select the file
     * to be signed.</dt>
     * <dd>
     * <ul>
     * <li>data</li>
     * <li>hash</li>
     * <li>hashes (massive signature)</li>
     * <li>fileUri</li>
     * </ul>
     * </dd>
     * <dt>For electronic signature (optional): If none are specified, default values are used.</dt>
     * <dd>
     * <ul>
     * <li>signatureAlgorithm</li>
     * <li>signatureFormat</li>
     * </ul>
     * </dd>
     * <dt>Output parameters:</dt>
     * <dd>
     * <dl>
     * <dt>If an error occurs <code>(isError()==true)</code>:</dt>
     * <dd>errorMessage</dd>
     * <dt>If no error occurs <code>(isError()==false)</code>:</dt>
     * <dd>
     * <ul>
     * <li>signatureBase64Encoded</li>
     * <li>fileUsedPath (if data to be signed has been read from a file)</li>
     * <li>signCertificateBase64Encoded</li>
     * </ul>
     * </dd>
     * </dl>
     * </dd>
     * </dl>
     * @return <code>true</code> si se ha ejecutado correctamente, <code>false</code> en caso contrario <br>
     *         <code>true</code> if execution is correct, else <code>false</code>. */
    boolean sign();

    /** Firma los datos proporcionados en base 64. <br>
     * <br>
     * Signs provided data in base 64.
     * @param b64data
     *        Datos a firmar, en formato Base64. <br>
     *        Data to be signed, in Base64 format.
     * @return <code>true</code> si &eacute;xito, <code>false</code> si
     *         ocurri&oacute; alg&uacute;n fallo <br>
     *         <code>true</code> if successful, <code>false</code> in case of
     *         failure. */
    boolean signData(String b64data);

    /** A&ntilde;ade un atributo firmado al formato de firma seleccionado. Este
     * formato debe reconocer el OID especificado, siendo el atributo {@code value} su valor como cadena de texto. <br>
     * <br>
     * Adds a signed attribute to the selected signature format. This format
     * must recognize the specified OID. Attribute {@code value} is its value as
     * a text string.
     * @param oid
     *        Object Identifier. Identificador del objeto a introducir. <br>
     *        Object Identifier
     * @param value
     *        Valor asignado <br>
     *        Assigned value
     * @return true si se ha realizado correctamente. <br>
     *         true if successful. */
    boolean addSignedAttribute(String oid, String value);

    /** Elimina el atributo con el OID indicado antes de realizar la firma. <br>
     * <br>
     * Eliminates the attribute with the indicated OID before signing.
     * @param oid
     *        Object Identifier.
     * @return true si se ha eliminado correctamente. False si no exist&iacute;a
     *         o ha ocurrido alg&uacute;n error. <br>
     *         true if it has been successfully eliminated. False if
     *         nonexistent, or an error occurred. */
    boolean removeSignedAttribute(String oid);

    /** A&ntilde;ade un atributo no firmado al formato de firma seleccionado. <br>
     * <br>
     * Adds an unsigned attribute to the selected signature format.
     * @param oid
     *        Object Identifier. Identificador del atributo a introducir.
     * @param value
     *        Valor asignado <br>
     *        Assigned value
     * @return true si se ha realizado correctamente. <br>
     *         true if successful. */
    boolean addUnsignedAttribute(String oid, String value);

    /** Elimina el atributo con el OID indicado antes de realizar la firma. <br>
     * Eliminates the attribute with the indicated OID before signing.
     * @param oid
     *        Object Identifier.
     * @param value
     *        Valor especifico a borrar. <br>
     *        Specific value to be eliminated.
     * @return true si se ha eliminado correctamente. False si no exist&iacute;a
     *         o ha ocurrido alg&uacute;n error. <br>
     *         true if successfully eliminated. False if nonexistent, or error
     *         occurred. */
    boolean removeUnsignedAttribute(String oid, String value);

    /** Agrega una propiedad adicional a la configuraci&oacute;n de firma. <br>
     * <br>
     * Includes an additional property to the signature configuration.
     * @param key
     *        Propiedad que se desea establecer. <br>
     *        Property to be included.
     * @param value
     *        Valor que se desea establecer a la propiedad. <br>
     *        Value for the property to be included. */
    void addExtraParam(String key, String value);

    /** Elimina una propiedad adicional de la configuraci&oacute;n de firma. <br>
     * <br>
     * Eliminates an additional property from the signature configuration.
     * @param key
     *        propiedad que se desea eliminar. <br>
     *        property to be eliminated. */
    void removeExtraParam(String key);

    /** Agrega una nueva transformaci&oacute;n XML a la configuraci&oacute;n del
     * cliente. Esta transformaci&oacute;n ser&aacute; tenida en cuenta por
     * aquellos formatos de firma que permitan configurar las transformaciones
     * XML que deben aplicarse sobre los datos.<br>
     * Este m&eacute;todo puede utilizarse varias veces para establecer un
     * conjunto de transformaciones que se apliquen a las firmas de forma
     * ordenada. Existen varios tipos de transformaciones, dentro de los cuales
     * algunos tienen subtipos. Los tipos y subtipos reglados son:
     * <ul>
     * <li><b>http://www.w3.org/TR/1999/REC-xpath-19991116</b>: Transformaci&oacute;n de tipo XPATH.</li>
     * <li><b>http://www.w3.org/2002/06/xmldsig-filter2</b>: Transformaci&oacute;n de tipo XPATH2.<br>
     * Subtipos:
     * <ul>
     * <li>subtract</li>
     * <li>intersect</li>
     * <li>union</li>
     * </ul>
     * </li>
     * <li><b>http://www.w3.org/TR/1999/REC-xslt-19991116</b>: Transformaci&oacute;n de tipo XSLT.</li>
     * <li><b>http://www.w3.org/2000/09/xmldsig#base64</b>: Transformaci&oacute;n de tipo BASE64.</li>
     * <li><b>http://www.w3.org/2000/09/xmldsig#enveloped-signature</b>: Transformaci&oacute;n de tipo ENVELOPED.</li>
     * </ul>
     * En el caso en que una transformaci&oacute;n no tenga subtipo, se
     * indicar&aacute; {@code null}. Las transformaciones de tipo BASE64 y
     * ENVELOPED no necesitan cuerpo (se usar&aacute; {@code null}) <br>
     * <br>
     * Includes a new XML transformation to the client's configuration. This
     * transformation will be considered by those signature formats that allow
     * the configuration of XML transformations to be applied on the data.<br>
     * This method may be used several times in order to establish that a set of
     * transformations be applied to the signatures following a pre-arranged
     * scheme. There are several types of transformations. In turn, some types
     * have subtypes. Established types and subtypes are: <br>
     * <ul>
     * <li><b>http://www.w3.org/TR/1999/REC-xpath-19991116</b>: Type XPATH transformation.</li>
     * <li><b>http://www.w3.org/2002/06/xmldsig-filter2</b>: Type XPATH2 transformation.<br>
     * Subtypes:
     * <ul>
     * <li>subtract</li>
     * <li>intersect</li>
     * <li>union</li>
     * </ul>
     * </li>
     * <li><b>http://www.w3.org/TR/1999/REC-xslt-19991116</b>: Type XSLT transformation.</li>
     * <li><b>http://www.w3.org/2000/09/xmldsig#base64</b>: Type BASE64 transformation.</li>
     * <li><b>http://www.w3.org/2000/09/xmldsig#enveloped-signature</b>: Type ENVELOPED transformation.</li>
     * </ul>
     * Should a transformation have no subtype,{@code null} will be indicated.
     * Transformations of type BASE64 and ENVELOPED do not need a body ( {@code null} shall be used)
     * @param type
     *        Tipo de transformaci&oacute;n. <br>
     *        Type of transformation
     * @param subtype
     *        Subtipo de transformaci&oacute;n (opcional). <br>
     *        Subtype of transformation (optional)
     * @param body
     *        Cuerpo de la transformaci&oacute;n. <br>
     *        Transformation body. */
    void addXMLTransform(String type, String subtype, String body);

    /** Elimina todas las transformaciones XML configuradas en el cliente. <br>
     * <br>
     * Eliminates all XML transformations configured in the client. */
    void resetXMLTransforms();

    /** Realiza el proceso de co-firmado (firma en paralelo). Los
     * par&aacute;metros de operaci&oacute;n que pueden establecerse y los
     * m&eacute;todos para ello son:<br>
     * <ul>
     *  <li>
     *   <b>Datos a firmar (excluyentes): </b>
     *   <ul>
     *    <li><code>setData(String)</code></li>
     *    <li><code>setFileuri(String)</code></li>
     *   </ul>
     *  </li>
     *  <li>
     *   <b>Firma original que se desea cofirmar (excluyentes): </b>
     *   <ul>
     *    <li><code>setElectronicSignature(String)</code></li>
     *    <li><code>setElectronicSignatureFile(String)</code></li>
     *   </ul>
     *  </li>
     *  <li>
     *   <b>Certificado de firma (excluyentes): </b></li> <li><b>Configuraci&oacute;n de firma: </b>
     *   <ul>
     *    <li><code>setSignatureAlgorithm(String)</code></li>
     *    <li><code>setSignatureFormat(String)</code></li>
     *    <li><code>setSignatureMode(String)</code></li>
     *   </ul>
     *  </li>
     * </ul> Los par&aacute;mnetros establecidos como resultado de esta
     * operaci&oacute;n son:
     * <ul>
     *  <li>Firma codificada en base 64 (<code>getSignatureBase64Encoded()</code>)</li>
     *  <li>Firma en modo texto (<code>getSignatureText()</code>)</li>
     *  <li>Ruta al fichero firmado si no se introdujeron los datos directamente (<code>getFileUsedPath()</code>)</li>
     *  <li>Certificado de firma en base 64 (<code>getSignCertificateBase64Encoded()</code>)</li>
     * </ul>
     * En caso de error, la descripci&oacute;n del mismo puede recuperarse
     * mediante el m&eacute;todo getErrorMessage() del cliente. <br>
     * <br>
     * Performs a parallel cosignature process. Available operative parameters
     * and methods are:<br>
     * <ul>
     *  <li>
     *   <b>Data to be signed (exclusive): </b>
     *   <ul>
     *    <li><code>setData(String)</code></li>
     *    <li><code>setFileuri(String)</code></li>
     *   </ul>
     *  </li>
     *  <li>
     *   <b>Original signature to be cosigned (exclusive): </b>
     *   <ul>
     *    <li><code>setElectronicSignature(String)</code></li>
     *    <li><code>setElectronicSignatureFile(String)</code></li>
     *   </ul>
     *  </li>
     *  <li>
     *   <b>Signature certificate (exclusive): </b></li> <li><b>Signature configuration: </b>
     *   <ul>
     *    <li><code>setSignatureAlgorithm(String)</code></li>
     *    <li><code>setSignatureFormat(String)</code></li>
     *    <li><code>setSignatureMode(String)</code></li>
     *   </ul>
     *  </li>
     * </ul> Parameters established as result of this operation are:
     * <ul>
     *  <li>Encoded signature in base 64 (<code>getSignatureBase64Encoded()</code>)</li>
     *  <li>Signature as plain text (<code>getSignatureText()</code>)</li>
     *  <li>Route to the signed file, if data has not been entered directly (<code>getFileUsedPath()</code>)</li>
     *  <li>Signature certificate in base 64 (<code>getSignCertificateBase64Encoded()</code>)</li>
     * </ul>
     * If error occurs, its description can be retrieved by means of client's
     * getErrorMessage() method. <br>
     * <br>
     * @return <code>true</code> si la operaci&oacute;n ha finalizado
     *         correctamente, <code>false</code> en caso contrario. <br>
     *         <code>true</code> if the operation ended successfully. Else, <code>false</code>. */
    boolean coSign();

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
     * La firma puede ser establecida mediante <code>setElectronicSignature(String)</code> o <code>setElectronicSignatureFile(String)</code>. El
     * an&aacute;lisis de la
     * firma se realiza primeramente por medio del manejador del formato de
     * firma establecido con <code>setSignatureFormat(String)</code> y, en caso de
     * que este manejador no soporte la firma introducida, se buscar&aacute;
     * entre el resto de manejadores el m&aacute;s adecuado para manejar la
     * firma. Si no se encuentra ninguno, se devuelve <code>null</code>. <br>
     * <br>
     * Returns an electronic signature's signer structure. Signers are separated
     * by '\n' and start by as many '\t' as is the level they are in.<br>
     * <br>
     * Example:
     * <ul>
     * <li>Parallel Signature: "signer1\nsigner2\nsigner3"</li>
     * <li>Cascade Signature: "signer1\n\tsigner2\n\t\tsigner3"</li>
     * <li>'Complex' Signature: "signer1\n\tsigner2\n\tsigner3\n\t\tsigner4\nsigner5"</li>
     * </ul>
     * Signature may be set with <code>setElectronicSignature(String)</code> or <code>setElectronicSignatureFile(String)</code>. Signature analysis is
     * performed first by means of the Signature Format Administrator
     * established with <code>setSignatureFormat(String)</code> and, in case it
     * provides no support for the intoduced signature, a suitable administrator
     * shall be sought. If none is found, <code>null</code> is returned.
     * @return Una cadena en la que se representa la estructura firmas (en
     *         paralelo y cascada). <br>
     *         A string representing signature structure (parallel and cascade) */
    String getSignersStructure();

    /** Realiza el proceso de contra-firmado (firma en cascada). Contra-firma
     * todos los nodos de un o varios firmantes.<br>
     * <br>
     * <b>Par&aacute;metros de entrada:</b><br>
     * - Para la selecci&oacute;n de certificado (opcional, excluyentes): Si no
     * se especifica ninguno, se muestra una ventana al usuario para que escoja
     * entre todos los certificados que tenga instalados<br>
     *
     * <pre>
     * -mandatoryCertCondition
     * </pre>
     *
     * <pre>
     * -certFilter
     * </pre>
     *
     * - Para la firma digital(opcionales): Si no se especifican, se toman los
     * valores por defecto <br>
     *
     * <pre>
     * -signatureAlgorithm
     * </pre>
     *
     * <pre>
     * -signatureFormat
     * </pre>
     *
     * - Firma electr&oacute;nica de entrada(opcional, excluyentes):
     *
     * <pre>
     * -electronicSignature
     * </pre>
     *
     * <pre>
     * -electronicSignatureFile
     * </pre>
     *
     * - Firmantes a contra-firmar:
     *
     * <pre>
     * - signersToCounterSign (nombres de firmantes devueltos por getSignersStructure() separados por '\n' )
     * </pre>
     *
     * <br>
     * <b>Par&aacute;metros de salida:</b><br>
     * - Si se produjo un error (isError()==true):<br>
     *
     * <pre>
     * -errorMessage
     * </pre>
     *
     * - Si no se produjo (isError()==false)<br>
     *
     * <pre>
     * -signatureBase64Encoded
     * </pre>
     *
     * <pre>
     * - fileUsedPath (si se han le&iacute;do los datos a firmar de un fichero)
     * </pre>
     *
     * <pre>
     * -signCertificateBase64Encoded
     * </pre>
     *
     * <br>
     * Performs the counter-signature process (cascade signature). Counter-signs
     * all nodes from one or more signers.<br>
     * <br>
     * <b>Input parameters:</b><br>
     * - Certificate selection (optional, exclusive): if unspecified, a window
     * is displayed, prompting the user to choose among installed certificates<br>
     *
     * <pre>
     * -mandatoryCertCondition
     * </pre>
     *
     * <pre>
     * -certFilter
     * </pre>
     *
     * - For electronic signature (optional): If unspecified, default values are
     * used<br>
     *
     * <pre>
     * -signatureAlgorithm
     * </pre>
     *
     * <pre>
     * -signatureFormat
     * </pre>
     *
     * - Input Electronic Signature (optional, exclusive):
     *
     * <pre>
     * -electronicSignature
     * </pre>
     *
     * <pre>
     * -electronicSignatureFile
     * </pre>
     *
     * - Signers to counter-sign:
     *
     * <pre>
     * - signersToCounterSign (signers names returned by getSignersStructure() separated by '\n' )
     * </pre>
     *
     * <br>
     * <b>Output parameters:</b><br>
     * - If error occurs (isError()==true):<br>
     *
     * <pre>
     * -errorMessage
     * </pre>
     *
     * - No error occurs (isError()==false)<br>
     *
     * <pre>
     * -signatureBase64Encoded
     * </pre>
     *
     * <pre>
     * - fileUsedPath (if data to be signed has been read from a file)
     * </pre>
     *
     * <pre>
     * -signCertificateBase64Encoded
     * </pre>
     * @return {@code true} si se ha ejecutado correctamente <br>
     *         {@code true} if successful. */
    boolean counterSignSigners();

    /** Realiza el proceso de contra-firmado (firma en cascada). Contra-firma los
     * nodos con determinados &iacute;ndices.<br>
     * <br>
     * <b>Par&aacute;metros de entrada:</b><br>
     * - Para la selecci&oacute;n de certificado (opcional, excluyentes): Si no
     * se especifica ninguno, se muestra una ventana al usuario para que escoja
     * entre todos los certificados que tenga instalados<br>
     *
     * <pre>
     * -mandatoryCertCondition
     * </pre>
     *
     * <pre>
     * -certFilter
     * </pre>
     *
     * - Para la firma digital(opcionales): Si no se especifican, se toman los
     * valores por defecto <br>
     *
     * <pre>
     * -signatureAlgorithm
     * </pre>
     *
     * <pre>
     * -signatureFormat
     * </pre>
     *
     * - Firma electr&oacute;nica de entrada(opcional, excluyentes):
     *
     * <pre>
     * -electronicSignature
     * </pre>
     *
     * <pre>
     * -electronicSignatureFile
     * </pre>
     *
     * - Firmantes a contra-firmar:
     *
     * <pre>
     * - signersToCounterSign (indices de nodos de getSignersStructure() separados por '\n')
     * </pre>
     *
     * <br>
     * <b>Par&aacute;metros de salida:</b><br>
     * - Si se produjo un error (isError()==true):<br>
     *
     * <pre>
     * -errorMessage
     * </pre>
     *
     * - Si no se produjo (isError()==false)<br>
     *
     * <pre>
     * -signatureBase64Encoded
     * </pre>
     *
     * <pre>
     * - fileUsedPath (si se han le&iacute;do los datos a firmar de un fichero)
     * </pre>
     *
     * <pre>
     * -signCertificateBase64Encoded
     * </pre>
     *
     * <br>
     * <br>
     * Performs the counter-signature process (cascade signature). Counter-signs
     * nodes with specific indexes.<br>
     * <br>
     * <b>Input parameters:</b><br>
     * - For certificate selection (optional, exclusive): If unspecified, a
     * window is displayed prompting user to choose among all installed
     * certificates<br>
     *
     * <pre>
     * -mandatoryCertCondition
     * </pre>
     *
     * <pre>
     * -certFilter
     * </pre>
     *
     * - For electronic signature (optional): If unspecified, default values are
     * used. <br>
     *
     * <pre>
     * -signatureAlgorithm
     * </pre>
     *
     * <pre>
     * -signatureFormat
     * </pre>
     *
     * - Input Electronic Signature (optional, exclusive):
     *
     * <pre>
     * -electronicSignature
     * </pre>
     *
     * <pre>
     * -electronicSignatureFile
     * </pre>
     *
     * - Signers to counter-sign:
     *
     * <pre>
     * - signersToCounterSign (Node index from  getSignersStructure() separated by '\n')
     * </pre>
     *
     * <br>
     * <b>Output parameters:</b><br>
     * - If error occurs (isError()==true):<br>
     *
     * <pre>
     * -errorMessage
     * </pre>
     *
     * - No error occurs (isError()==false)<br>
     *
     * <pre>
     * -signatureBase64Encoded
     * </pre>
     *
     * <pre>
     * - fileUsedPath (if data to be signed has been read from a file)
     * </pre>
     *
     * <pre>
     * -signCertificateBase64Encoded
     * </pre>
     *
     * <br>
     * <br>
     * @return Devuelve {@code true} si la operaci&oacute;n finaliz&oacute;
     *         correctamente, false en caso contrario. <br>
     *         Returns {@code true} if operation ends successfully. Else,
     *         false. */
    boolean counterSignIndexes();

    /** Realiza el proceso de contra-firmado (firma en cascada). Contra-firma
     * todos los nodos del &aacute;rbol. <br>
     * <b>Par&aacute;metros de entrada:</b><br>
     * - Para la selecci&oacute;n de certificado (opcional, excluyentes): Si no
     * se especifica ninguno, se muestra una ventana al usuario para que escoja
     * entre todos los certificados que tenga instalados<br>
     *
     * <pre>
     * -mandatoryCertCondition
     * </pre>
     *
     * <pre>
     * -certFilter
     * </pre>
     *
     * - Para la firma digital(opcionales): Si no se especifican, se toman los
     * valores por defecto <br>
     *
     * <pre>
     * -signatureAlgorithm
     * </pre>
     *
     * <pre>
     * -signatureFormat
     * </pre>
     *
     * - Firma electr&oacute;nica de entrada(opcional, excluyentes):
     *
     * <pre>
     * -electronicSignature
     * </pre>
     *
     * <pre>
     * -electronicSignatureFile
     * </pre>
     *
     * <br>
     * <b>Par&aacute;metros de salida:</b><br>
     * - Si se produjo un error (isError()==true):<br>
     *
     * <pre>
     * -errorMessage
     * </pre>
     *
     * - Si no se produjo (isError()==false)<br>
     *
     * <pre>
     * -signatureBase64Encoded
     * </pre>
     *
     * <pre>
     * - fileUsedPath (si se han le&iacute;do los datos a firmar de un fichero)
     * </pre>
     *
     * <pre>
     * -signCertificateBase64Encoded
     * </pre>
     *
     * <br>
     * Performs the counter-signature process (cascade signature). Counter-signs
     * all nodes in tree. <br>
     * <b>Input parameters:</b><br>
     * - For certificate selection (optional, exclusive): If unspecified, a
     * window is displayed prompting user to choose among all installed
     * certificates<br>
     *
     * <pre>
     * -mandatoryCertCondition
     * </pre>
     *
     * <pre>
     * -certFilter
     * </pre>
     *
     * - For electronic signature (optional): If unspecified, default values are
     * used. <br>
     *
     * <pre>
     * -signatureAlgorithm
     * </pre>
     *
     * <pre>
     * -signatureFormat
     * </pre>
     *
     * - Input Electronic Signature (optional, exclusive):
     *
     * <pre>
     * -electronicSignature
     * </pre>
     *
     * <pre>
     * -electronicSignatureFile
     * </pre>
     *
     * <br>
     * <b>Output parameters:</b><br>
     * - If error occurs (isError()==true):<br>
     *
     * <pre>
     * -errorMessage
     * </pre>
     *
     * - No error occurs (isError()==false)<br>
     *
     * <pre>
     * -signatureBase64Encoded
     * </pre>
     *
     * <pre>
     * - fileUsedPath (if data to be signed has been read from a file)
     * </pre>
     *
     * <pre>
     * -signCertificateBase64Encoded
     * </pre>
     *
     * <br>
     * @return true si se ha ejecutado correctamente <br>
     *         true if succesful. */
    boolean counterSignTree();

    /** Realiza el proceso de contra-firmado (firma en cascada). Contra-firma las
     * hojas del &aacute;rbol.<br>
     * <br>
     * <b>Par&aacute;metros de entrada:</b><br>
     * - Para la selecci&oacute;n de certificado (opcional, excluyentes): Si no
     * se especifica ninguno, se muestra una ventana al usuario para que escoja
     * entre todos los certificados que tenga instalados<br>
     *
     * <pre>
     * -mandatoryCertCondition
     * </pre>
     *
     * <pre>
     * -certFilter
     * </pre>
     *
     * - Para la firma digital(opcionales): Si no se especifican, se toman los
     * valores por defecto <br>
     *
     * <pre>
     * -signatureAlgorithm
     * </pre>
     *
     * <pre>
     * -signatureFormat
     * </pre>
     *
     * - Firma electr&oacute;nica de entrada(opcional, excluyentes):
     *
     * <pre>
     * -electronicSignature
     * </pre>
     *
     * <pre>
     * -electronicSignatureFile
     * </pre>
     *
     * <br>
     * <b>Par&aacute;metros de salida:</b><br>
     * - Si se produjo un error (isError()==true):<br>
     *
     * <pre>
     * -errorMessage
     * </pre>
     *
     * - Si no se produjo (isError()==false)<br>
     *
     * <pre>
     * -signatureBase64Encoded
     * </pre>
     *
     * <pre>
     * - fileUsedPath (si se han le&iacute;do los datos a firmar de un fichero)
     * </pre>
     *
     * <pre>
     * -signCertificateBase64Encoded
     * </pre>
     *
     * <br>
     * Performs counter-signature process (cascade signature). Counter-signs
     * tree leaves.<br>
     * <br>
     * <b>Input parameters:</b><br>
     * - For certificate selection (optional, exclusive): If unspecified, a
     * window is displayed prompting user to choose among all installed
     * certificates<br>
     *
     * <pre>
     * -mandatoryCertCondition
     * </pre>
     *
     * <pre>
     * -certFilter
     * </pre>
     *
     * - For electronic signature (optional): If unspecified, default values are
     * used <br>
     *
     * <pre>
     * -signatureAlgorithm
     * </pre>
     *
     * <pre>
     * -signatureFormat
     * </pre>
     *
     * - Input Electronic signature (optional, exclusive):
     *
     * <pre>
     * -electronicSignature
     * </pre>
     *
     * <pre>
     * -electronicSignatureFile
     * </pre>
     *
     * <br>
     * <b>Output parameters:</b><br>
     * - If error occurs (isError()==true):<br>
     *
     * <pre>
     * -errorMessage
     * </pre>
     *
     * - No error occurs (isError()==false)<br>
     *
     * <pre>
     * -signatureBase64Encoded
     * </pre>
     *
     * <pre>
     * - fileUsedPath (if data to be signed has been read from a file)
     * </pre>
     *
     * <pre>
     * -signCertificateBase64Encoded
     * </pre>
     *
     * <br>
     * @return true si se ha ejecutado correctamente <br>
     *         true if succesful. */
    boolean counterSignLeafs();

    /** Guarda una firma electr&oacute;nica generada anteriormente en un fichero.
     * La firma electr&oacute;nica se habr&aacute; generado mediante una de las
     * operaciones de firma, cofirma o contrafirma. La ruta en donde se
     * almacenar&aacute; se establece mediante <code>setOutFilePath(String)</code>.
     * Si no se ha establecido una ruta en la que almacenar la firma, se
     * mostrar&aacute; un di&aacute;logo de guardado. <br>
     * <br>
     * Stores a previously generated electronic signature in a file. Signature
     * has been generated by a signature, cosignature or counter-signature
     * operation. Storage route is set with <code>setOutFilePath(String)</code>. If
     * no storage route has been set, a Save dialog box is displayed.
     * @return true si el proceso termina correctamente, false en otro caso. <br>
     *         true if process ends successfully. Else, false. */
    boolean saveSignToFile();

    /** Devuelve el certificado en base 64 con el que se ha firmado. Esta
     * operaci&oacute;n devuelve el &uacute;ltimo certificado cargado durante la
     * operacion de firma, luego una operaci&oacute;n de firma fallida tras la
     * selecci&oacute;n del certificado lleva a devolver la referencia a un
     * certificado que puede no ser el utilizado en la &uacute;ltima operacion
     * de firma satisfactoria. En caso de no haber ning&uacute;n certificado
     * seleccionado se devolver&aacute; cadena vac&iacute;a. <br>
     * <br>
     * Returns the base 64 certificate which has been used for signing. This
     * operation returns the last certificate loaded in the signature operation.
     * A failed signature operation results in a reference to a certificate that
     * may not be the one used in the last satisfactory operation. If no
     * certificate has been selected an empty string is returned.
     * @return El certificado de firma en base 64. <br>
     *         The signature certificate in base 64. */
    String getSignCertificateBase64Encoded();

    /** Devuelve el certificado con el que se ha firmado o ensobrado datos. Esta
     * operaci&oacute;n devuelve el &uacute;ltimo certificado cargado durante
     * una operacion de firma o envoltura de datos. Una operaci&oacute;n de
     * firma fallida tras la selecci&oacute;n del certificado lleva a devolver
     * la referencia a un certificado que puede no ser el utilizado en la
     * &uacute;ltima operacion de firma satisfactoria. En caso de no haber
     * ning&uacute;n certificado seleccionado se devolver&aacute; <code>null</code>. <br>
     * <br>
     * Returns the certificate which has been used for signing or enveloping.
     * This operation returns the last certificate loaded in the signature
     * operation. A failed signature operation results in a reference to a
     * certificate that may not be the one used in the last satisfactory
     * operation. If no certificate has been selected <code>null</code> is
     * returned.
     * @return El certificado de firma. <br>
     *         The signature certificate. */
    X509Certificate getSignCertificate();

    /** Establece el algoritmo de firma. Puede ser:
     * <ul>
     * <li><code>MD2withRSA</code></li>
     * <li><code>MD5withRSA</code></li>
     * <li><code>SHA1withRSA</code></li>
     * <li><code>SHA256withRSA</code></li>
     * <li><code>SHA384withRSA</code></li>
     * <li><code>SHA512withRSA</code> (Recomendado)</li>
     * </ul>
     * Se desaconsejan los algoritmos MD2 y MD5 por motivos de obsolescencia.
     * Algunos formatos de firma y algunos almacenes de certificados no soportan
     * todos los algoritmos. <br>
     * <br>
     * Sets the signature algorithm. It may be:
     * <ul>
     * <li><code>MD2withRSA</code></li>
     * <li><code>MD5withRSA</code></li>
     * <li><code>SHA1withRSA</code></li>
     * <li><code>SHA256withRSA</code></li>
     * <li><code>SHA384withRSA</code></li>
     * <li><code>SHA512withRSA</code> (Recommended)</li>
     * </ul>
     * The use of MD2 and MD5 algorithm's obsolescence. Some signature formats
     * and some keystores do not support all algorithms.
     * @param signatureAlgorithm
     *        Nombre del algoritmo a usar en las operaciones de firma. <br>
     *        Algorithm name to be used in signature operations. */
    void setSignatureAlgorithm(String signatureAlgorithm);

    /** Establece el formato de la firma electr&oacute;nica generada.
     * Sets the electronic signature format to be generated.
     * @param signatureFormat
     *        Formato de la firma electr&oacute;nica a generar <br>
     *        Electronic signature's format to be generated. */
    void setSignatureFormat(String signatureFormat);

    /** Establece la ruta del fichero en donde se almacenar&aacute;n los datos
     * resultados de la operaci&oacute;n realizada. Se muestra un di&aacute;logo al usuario
     * para que consienta la operaci&oacute;n. Si no lo hace, se aborta la operaci&oacute;n
     * y se establece un error.<br>
     * <br>
     * Sets the route to the file where the operation's resulting data will be
     * stored.
     * @param outFilePath
     *        Ruta por defecto del fichero de salida. <br>
     *        Output file's default route. */
    void setOutFilePath(String outFilePath);

    /** Devuelve la estructura de firma generada o establecida codificada en base
     * 64. Si no se dispone de una firma, se devuelve cadena vac&iacute;a. <br>
     * <br>
     * Returns a signature structure generated or set with base 64 encryption.
     * If no signature is available, an empty string is returned.
     * @return La firma en base 64. <br>
     *         Signature in base 64. */
    String getSignatureBase64Encoded();

    /** Devuelve la firma generada en el &uacute;ltimo proceso de firma o
     * establecida por en el cliente como un String (&uacute;til para firmas
     * XAdES, XMLDSign y firma web). Si no se ha generado una firma, se devuelve
     * cadena vac&iacute;a.<br>
     * El uso de este m&eacute;todo no esta recomendado ya que el resultado de
     * la que devuelve puede variar seg&uacute;n la codificaci&oacute;n
     * establecida por defecto. Su principal utilidad es mostrar el resultado de
     * una firma XML. Para firmas binarias debe utilizarse <code>getSignatureBase64Encoded()</code>.
     * Si no se dispone de una firma, se devuelve cadena vac&iacute;a. <br>
     * <br>
     * Returns the signature generated in the last signature process, or set by
     * the client as a string (useful for XAdES, XMLDSign and web signatures).
     * If no signature has been generated, an empty string is returned. <br>
     * The use of this method is discouraged, since returned results may vary
     * depending on the encryption set by default. Its main use is showing the
     * results of a XML signature. For binary signatures <code>getSignatureBase64Encoded()</code> should be used.
     * If no signature is available, an empty string is returned.
     *
     * @param charsetName Codificaci&oacute;n del texto. <br>
     * 					  Encoding.
     * @return La &uacute;ltima firma electr&oacute;nica generada o establecida
     *         por el cliente. <br>
     *         Last generated (or user set) electronic signature. */
    String getSignatureText(String charsetName);

    /** Establece los datos de entrada en base 64 para los procesos de firma,
     * co-firma (firma en paralelo) y generaci&oacute;n de sobres digitales.
     * Los datos introducidos mediante esta funci&oacute;n
     * sobreescribir&aacute;n cualquier otro establecido previamente mediante
     * los m&eacute;todos setFileuri o setHash.
     * <br>
     * Sets input data in base 64 for the signature, cosignature (parallel
     * signature) and counter-signature and digital envelope generation
     * processes. <br>
     * Data entered with this function will overwrite any other which may have
     * been previously set with setFileuri or setHash methods.
     * @param data
     *        Datos a operar codificados en base 64. <br>
     *        Data to operate with, encrypted in base 64. */
    void setData(String data);

    /** Establece la URI (o URL) que apunta al fichero que contiene los datos de
     * entrada para operar (cifrar, firmar, leer...) Se soporta el protocolo <code>file://</code> para ficheros en almacenamiento local.
     * Los datos introducidos mediante esta funci&oacute;n
     * sobreescribir&aacute;n cualquier otro establecido previamente mediante
     * los m&eacute;todos setData o setHash. <br>
     * Se muestra un di&aacute;logo al usuario
     * para que consienta la operaci&oacute;n. Si no lo hace, se aborta la operaci&oacute;n
     * y se establece un error.<br>
     * <br>
     * Sets the URI (or URL) pointing to the file which contains input data to
     * operate (i.e., encrypt, sign, read, etc.) with. <code>file://</code> protocol is supported, for locally stored files. <br>
     * Data entered with this function will overwrite any other which may have
     * been previously set with setData o setHash methods.
     * @param fileuri
     *        URI hacia el fichero de datos. <br>
     *        URI to the data file. */
    void setFileuri(String fileuri);

    /** Establece los datos contenidos en el fichero indicado (en donde se
     * encontrar&aacute;n codificados en base 64), como los datos de entrada
     * para las operaciones criptogr&aacute;ficas y establece la ruta
     * introducida como ruta de entrada.<br>
     * El contenido del fichero se interpretar&aacute; siempre como datos en
     * base 64 no realiz&aacute;ndose la comprobaci&oacute;n de los mismos. <br>
     *  Se muestra un di&aacute;logo al usuario
     * para que consienta la operaci&oacute;n. Si no lo hace, se aborta la operaci&oacute;n
     * y se establece un error.<br>
     * <br>
     * Sets the data contained in the indicated base 64-encrypted file as the
     * input data for cryptographic operations, and sets the indicated route as
     * input route.<br>
     * Contents of the file will always be interpreted as base 64 data. They
     * will not be checked.
     * @param fileuri
     *        URI hacia el fichero de datos. <br>
     *        URI to the data file. */
    void setFileuriBase64(String fileuri);

    /** Establece el hash de los datos a firmar para los procesos de firma. <br>
     * Estos entrada tendr&aacute; m&aacute;xima prioridad cuando se realice una
     * firma expl&iacute;cita. <br>
     * <br>
     * Sets the hash to the data to be signed for the signature processes. <br>
     * This entry will have maximum priority when an explicit signature is
     * performed.
     * @param hash
     *        Hash en base 64 que se desea firmar. */
    void setHash(final String hash);

    /** Devuelve los datos establecidos como entrada para la operaci&oacute;n de
     * firma. En caso de no haberse establecido ning&uacute;n dato, se devuelve
     * cadena vac&iacute;a. <br>
     * <br>
     * Returns the input Base64 data. If no data is established, empty string is
     * returned.
     * @return Cadena en Base 64. <br>
     *         String in base 64. */
    String getB64Data();

    /** Ruta al fichero en el que se ha guardado la firma. Esta direcci&oacute;n
     * puede hacerse establecido program&aacute;ticamente mediante <code>setOutFilePath(String)</code>
     * o mediante la interfaz que aparece cuando no hay un valor asignado. Una vez se establece el valor
     * del fichero de salida este permanece a&uacute;n cuando no se completa la
     * operaci&oacute;n de firma. Si no hay un fichero de salida establecido se
     * devuelve cadena vac&iacute;a. <br>
     * <br>
     * Route to the file where the signature has been stored. This address may
     * have been set programmatically by <code>setOutFilePath(String)</code> or by the interface
     * showing when there is no assigned value.
     * Once the output file's value is set, it remains unchanged, even in
     * cases where the signature operation may not be complete. If there is no
     * set output file, an empty string is returned.
     * @return Ruta al fichero en que se ha guardado la firma. <br>
     *         Route to the file containing the signature.
     * @see #setOutFilePath(String)*/
    String getFilePath();

    /** Devuelve la ruta del fichero de entrada. Esta direcci&oacute;n puede
     * haberse establecido program&aacute;ticamente mediante <code>setFileuri(String)</code>
     * o mediante la interfaz que aparece
     * cuando no hay un valor asignado. Una vez se establece el valor del
     * fichero de datos este permanece a&uacute;n cuando no se completa la
     * operaci&acute;n de firma. Si no hay un fichero de datos establecido se
     * devuelve cadena vac&iacute;a.<br>
     * La ruta del fichero se devolver&aacute; respetando el <i>URL
     * Encoding</i>, es decir, se realizar&aacute;n cambios tales como sustituir
     * los espacios (' ') por "%20". Esta codificaci&oacute;n impide obtener de
     * forma directa un fichero mediante la clase {@link java.io.File},
     * previamente deberemos obtener una URI. <br>
     * <br>
     * Returns the input file's path. This address may have been set
     * programmatically by <code>setFileuri(String)</code> or or by the
     * interface showing when there is no assigned value. Once the file's value
     * is set, it remains unchanged, even in cases where the signature operation
     * may not be complete. If there is no set output file, an empty string is
     * returned.<br>
     * The file's path will be returned observing <i>URL Encoding</i>, i.e.,
     * changes will be executed such as replacing (' ') by "%20". This method
     * does not allow directly acquiring a file by the class {@link java.io.File}. An URI must be previously acquired.
     * @return Fichero que seleccion&oacute; el usuario para firmar <br>
     *         File selected by the user for signature.
     * @see #setFileuri(String)*/
    String getFileUsedPath();

    /** Establece una firma electr&oacute;nica en base 64 como entrada para el
     * cliente de firma. Este metodo se utiliza principalmente para indicar la
     * firma electr&oacute;nica durante las operaciones de cofirma y contrafirma
     * (en cada una de sus variedades). <br>
     * Los datos introducidos mediante esta funci&oacute;n
     * sobreescribir&aacute;n cualquier otro establecido previamente mediante
     * el m&eacute;todo setElectronicSignatureFile.<br>
     * <br>
     * Sets an electronic signature in base 64 as input for the signature
     * client. This method is mainly used to indicate the electronic signature
     * during every type of cosignature and counter-signature operations.<br>
     * Data entered with this function will overwrite any other which may have
     * been previously set with setElectronicSignatureFile method.
     * @param inElectronicSignature
     *        Firma electr&oacute;nica en base 64. <br>
     *        Electronic signature in base 64. */
    void setElectronicSignature(String inElectronicSignature);

    /** Establece la ruta de un fichero de firma electr&oacute;nica como entrada
     * para el cliente de firma. Este m&eacute;todo se utiliza principalmente
     * para indicar la firma electr&oacute;nica durante las operaciones de
     * cofirma y contrafirma (en cada una de sus variedades).<br>
     * Los datos introducidos mediante esta funci&oacute;n
     * sobreescribir&aacute;n cualquier otro establecido previamente mediante
     * el m&eacute;todo setElectronicSignature.<br>
     * Se muestra un di&aacute;logo al usuario
     * para que consienta la operaci&oacute;n. Si no lo hace, se aborta la operaci&oacute;n
     * y se establece un error.<br>
     * <br>
     * Sets the route to an electronic signature file as input for the signature
     * client. This method is mainly used to indicate electronic signature
     * during every type of cosignature and counter-signature operations.<br>
     * Data entered with this function will overwrite any other which may have
     * been previously set with setElectronicSignature method.
     * @param inElectronicSignatureFile
     *        Ruta del fichero de firma. <br>
     *        Route to the signature file. */
    void setElectronicSignatureFile(String inElectronicSignatureFile);

    /** Establece los nodos que deben contrafirmarse durante las operaciones de
     * contrafirma de firmantes y de nodos. Cuando se va a realizar la
     * contrafirma de firmantes concretos se indicar&aacute;n los nombres de los
     * firmantes tal c&oacute;mo los devuelve <code>getSignersStructure()</code>. En
     * el caso de la contrafirma de nodos los indices de los nodos (en donde el
     * primer nodo es el 0).<br>
     * Los distintos elementos se indican consecutivos en
     * forma de cadena y separados por '\n' o '\r\n'. Los nodos s&oacute;lo se
     * contrafirmar&aacute;n una vez por cada operaci&oacute;n, as&iacute;
     * repetir firmantes o nodos no tendr&aacute; efecto sobre la
     * operaci&oacute;n. <br>
     * <br>
     * Sets the nodes to counter-sign in signers and nodes' counter-signature
     * operations. When a specific signer's counter-signature is to be
     * performed, signers names will be indicated in the way returned by <code>getSignersStructure()</code>. In case of nodes' counter-signature,
     * nodes' indexes (where first node is 0).<br>
     * Different elements are consecutively indicated as a string and
     * separated by '\n' or '\r\n'. Nodes will only be counter-signed once
     * for each operation. Thus, repeating signers or nodes will have no
     * effect on the operation.
     * @param signers
     *        Cadena de nombres de firmantes o &iacute;ndices de los nodos. <br>
     *        Signer names string or node indexes. */
    void setSignersToCounterSign(String signers);

    /** Guarda los datos provenientes de la funci&oacute;n realizada
     * anteriormente en un fichero especificado. Se muestra un di&aacute;logo al usuario
     * para que consienta la operaci&oacute;n. Si no lo hace, se aborta la operaci&oacute;n
     * y se establece un error.<br>
     * <br>
     * Stores data originating in the function previously performed on a
     * specified file.
     * @param fileUri
     *        Ruta del fichero destino. <br>
     *        Route to the destination file.
     * @return <code>true</code> si los datos se han guardado correctamente, <code>false</code> en caso contrario. <br>
     *         <code>true</code> if data has been stored successfully. Else, <code>false</code>. */
    boolean saveDataToFile(String fileUri);

    /** Guarda los datos provenientes de la funci&oacute;n realizada
     * anteriormente en un fichero. <br>
     * <br>
     * Stores data originating in the function previously performed on a file
     * @return <code>true</code> si los datos se han guardado correctamente, <code>false</code> en caso contrario. <br>
     *         <code>true</code> if data has been stored successfully. Else, <code>false</code> */
    boolean saveDataToFile();

    /** Define el tipo de datos mime de los datos tras introducirlos. Si no se
     * especifica se intentar&aacute; identificar el tipo autom&aacute;ticamente.
     * Si no es posible identificar el tipo, se utilizar&aacute; como mimetype
     * por defecto "application/octet-string"<br>
     * <br>
     * Defines the type of entered mime data. If it isn't specified, it will be
     * automatically attempted to obtain mimetype from the data. If
     * mimetype cannnot be automatically obtained,
     * application/octet-string will be regarded as default mimetype.
     * @param mimetype
     *        Tipo de datos para los datos actuales. <br>
     *        Type of data, for current data. */
    void setDataMimeType(String mimetype);

    /** Devuelve el modo de firma electr&oacute;nica empleado en la &uacute;ltima
     * firma. <br>
     * <br>
     * Returns the electronic signature mode used in the last signature.
     * @return Modo de firma. <br>
     *         Signature mode. */
    String getSignatureMode();

    /** Establece el modo de firma, que define si los datos se incrustar&aacute;n
     * o no en una firma generada. Puede ser "Explicit" o "Implicit" (o
     * "Explicita" e "Implicita", respectivamente). Las opciones son insensibles
     * a may&uacute;sculas y min&uacute;sculas.<br>
     * Un formato de firma puede definir modos de firma propios. <br>
     * <br>
     * Sets the signature mode defining whether data will be embedded or not in
     * a generated signature. It may be, respectively, explicit or implicit.
     * Options are not case sensitive. <br>
     * A signature format may define its own signature modes.
     * @param mode
     *        Modo de firma. <br>
     *        Signature mode. */
    void setSignatureMode(String mode);

    // *******************************************************************/
    // ************** FUNCIONALIDADES DE MULTIFIRMA MASIVA ***************/
    // ************** MASSIVE MULTI-SIGNATURE FUNCTIONALITIES ************/
    // *******************************************************************/

    /** Firma todos los archivos de un directorio seg&uacute;n la configuracion
     * establecida. La firma siempre se realizar&aacute; en modo
     * expl&iacute;cito salvo que el formato de firma concreto exija lo
     * contrario. El directorio de firma se puede establecer con <code>setInputDirectoryToSign(String)</code>, si no se hace, se
     * mostrar&aacute; un di&aacute;logo para la selecci&oacute;n del
     * directorio. Para indicar que se desea firmar tambi&eacute;n los ficheros
     * de los subdirectorios del directorio establecido, debe usarse <code>setInRecursiveDirectorySign(boolean)</code>. Es posible limitar los
     * ficheros que deben firmarse mediante un filtro establecido con <code>setInIncludeExtensions(String)</code>.
     * La operaci&oacute;n masiva a realizar ser&aacute; por defecto la de
     * firma, aunque puede modificarse para realizar las operaciones de cofirma
     * o contrafirma (de todo el arbol de firma o s&oacute;lo los nodos hoja).
     * El tipo de operaci&oacute;n se indica mediante <code>setMassiveOperation(String)</code>. <br>
     * Si no se indica un directorio de salida con <code>setOutputDirectoryToSign(String)</code> el resultado se
     * almacenar&aacute; en el directorio de entrada. Si se indica el directorio
     * de salida y este no existe, se crear&aacute;. <br>Si se produjo
     * alg&uacute;n error durante la firma de un fichero, el proceso
     * continuar&aacute;, se crear&aacute; un registro en el log de error en el
     * directorio de salida de firmas y se devolver&aacute; <code>false</code> para que se realicen las operaciones adecuadas. <br>
     * <br>
     * Signs all of a directory's files, according to established configuration.
     * Signature will always be performed in an explicit way, except in cases
     * where the specific signature format requires otherwise. Signature
     * directory can be set with <code>etInputDirectoryToSign(String)</code>. Else,
     * a dialog will be displayed, prompting the selection of a directory. <code>setInRecursiveDirectorySign(boolean)</code> must be used to indicate
     * that subdirectory files are also meant for signing. A limit may be
     * established on the files to be signed with a filter set with <code>setInIncludeExtensions(String)</code>. <br>
     * Massive operation to perform will by default be signature, although this
     * can be modified in order to perform cosignature or counter-signature
     * (whole signature tree or just the leave nodes). The kind of operation is
     * indicated with <code>setMassiveOperation(String)</code>. <br>
     * If no output directory is indicated with <code>setOutputDirectoryToSign(String)</code> results will be saved in the
     * input directory. If indicated output directory does not exist it will be
     * created. <br>If an error occurs during the signature of a file, the
     * process will continue, an entry will be created in the error log in the
     * signature output directory, and <code>false</code> will be returned, for
     * the adequate operations to be performed.
     * @return Devuelve <code>true</code> si la firma ha sido correcta para
     *         todos los ficheros afectados. <br>
     *         Returns <code>true</code> if the signature was correct for all
     *         appropriate files. */
    boolean signDirectory();

    /** Establece la operaci&oacute;n masiva a realizar en el proceso generado
     * por los m&eacute;todos <code>signDirectory()</code>, <code>massiveSignatureData(String)</code>
     * y <code>EntryPointsCrypto.massiveSignatureFile(String)</code>.<br>
     * Los tipos de operaci&oacute;n permitidos son:
     * <ul>
     * <li>FIRMAR: Firma de datos.</li>
     * <li>COFIRMAR: Cofirma de datos.</li>
     * <li>CONTRAFIRMA_ARBOL: Contrafirma de todos los nodos de firma.</li>
     * <li>CONTRAFIRMA_HOJAS: Contrafirma de todos los nodos hoja de firma.</li>
     * </ul>
     * <br>
     * <br>
     * Sets the massive operation to be performed in the process generated by
     * the <code>signDirectory()</code>, <code>massiveSignatureData(String)</code> and <code>EntryPointsCrypto.massiveSignatureFile(String)</code> methods. <br>
     * The massive operations are:
     * <ul>
     * <li>FIRMAR: Sign.</li>
     * <li>COFIRMAR: Cosign.</li>
     * <li>CONTRAFIRMA_ARBOL: Countersign of all nodes from the signature tree.</li>
     * <li>CONTRAFIRMA_HOJAS: Countersign of all leaves nodes from the signature tree.</li>
     * </ul>
     * @param massiveOperation
     *        Tipo de operaci&oacute;n masiva a realizar. <br>
     *        Type of massive operation to perform. */
    void setMassiveOperation(String massiveOperation);

    /** Indica si se debe respetar el formato de firma original para las
     * operaciones de multifirma masiva o, si en cambio, se usar&aacute; la
     * configuraci&oacute;n de firma establecida para todas las firmas. Por
     * defecto, se respeta el formato original. <br>
     * <br>
     * Sets whether the original signature format for massive multi-signature
     * operations or the signature configuration used for all signatures should
     * be used. By default, original format is used.
     * @param originalFormat
     *        Indica si respetar el formato original. <br>
     *        Sets whether or not to use the original format. */
    void setOriginalFormat(boolean originalFormat);

    /** Devuelve la ruta absoluta del directorio donde se ubican los ficheros a
     * ser firmados de forma masiva. <br>
     * <br>
     * Returns the absolute route of the directory where files to be massively
     * signed are found.
     * @return Ruta absoluta del directorio. <br>
     *         Absolute route of the directory. */
    String getInputDirectoryToSign();

    /** Selecciona el directorio de donde se tomar&aacute;n los ficheros de
     * firma y datos para la operaci&oacute;n de firma masiva.
     * Se muestra un di&aacute;logo al usuario
     * para que consienta la operaci&oacute;n. Si no lo hace, se aborta la operaci&oacute;n
     * y se establece un error.<br>
     * <br>
     * Selects the directory from where signature files and data for the massive
     * signature operation will be taken from.
     * @param directory
     *        Ruta absoluta del directorio. <br>
     *        Absolute route of the directory. */
    void setInputDirectoryToSign(String directory);

    /** Devuelve la ruta absoluta del directorio donde se almacenar&aacute;n las
     * firmas resultado de la operaci&oacute;n de firma masiva. <br>
     * <br>
     * Returns the absolute route of the directory where signatures resulting
     * from the massive signature operation will be stored.
     * @return Ruta absoluta del directorio de salida de la firma masiva. <br>
     *         Absolute route of the massive signature output directory. */
    String getOutputDirectoryToSign();

    /** Selecciona el directorio donde se depositar&aacute;n las firmas masivas
     * de los archivos situados en <code>InputDirectoryToSign</code>.
     * Se muestra un di&aacute;logo al usuario
     * para que consienta la operaci&oacute;n. Si no lo hace, se aborta la operaci&oacute;n
     * y se establece un error.<br>
     * <br>
     * Sets the output directory where massive signatures of files located in <code>InputDirectoryToSign</code> will be saved.
     * @param directory
     *        Ruta absoluta del directorio. <br>
     *        Absolute route of the directory. */
    void setOutputDirectoryToSign(String directory);

    /** Define las extensiones que se incluir&aacute;n en la firma de
     * directorios. La cadena que se debe introducir se corresponde con las
     * extensiones separadas por ','. Para eliminar el filtrado por
     * extensi&oacute;n, introducir <code>null</code>. Por ejemplo: <code>TXT,JPG,BMP,DAT</code>.
     * <br>
     * Sets extensions to be included in the signature of directories. String to
     * enter matches the extensions, separated by ','. To eliminate extension
     * filtering, <code>null</code> must be entered. Example: <code>TXT,JPG,BMP,DAT</code>
     * @param extensions
     *        Extensiones de los ficheros que se desean firmar. <br>
     *        File extensions for the files to be signed. */
    void setInIncludeExtensions(String extensions);

    /** Establece si la firma de directorios se efectuar&aacute; de forma
     * recursiva o no. <br>
     * <br>
     * Sets whether directory signature will be recursively executed.
     * @param recursiveSignDir
     *        <code>true</code> para firmar un directorio y sus
     *        subdirectorios. <code>false</code> en caso contrario. <br>
     *        <code>true</code> to sign a directory and its subdirectories. <code>false</code> otherwise. */
    void setInRecursiveDirectorySign(boolean recursiveSignDir);

    /** Toma la configuraci&oacute;n de firma del cliente y prepara el proceso de
     * firma masiva. Los datos que toma de la configuracion del cliente son:
     * <ul>
     * <li>Certificado de firma.</li>
     * <li>Clave de firma.</li>
     * <li>Algoritmo.</li>
     * <li>Modo.</li>
     * <li>Formato por defecto.</li>
     * <li>Respetar formato original de firma.</li>
     * <li>Operaci&oacute;n masiva a realizar (firma, cofirma, contrafirma de hojas o de &aacute;rbol).</li>
     * </ul>
     * En el caso del certificado de firma, se preguntar&aacute; cuando este no
     * se especifique.<br>
     * Este m&eacute;todo tambi&eacute; reinicia el log del procedimiento de
     * firma masiva. Una vez preparado el procedimiento de firma masiva,
     * cualquier cambio en la configuraci&oacute;n del cliente, salvo la
     * operaci&oacute;n a realizar, no lo afectar&aacute;. As&iacute;, por
     * ejemplo, se podr&iacute;a especificar otro certificado de firma, pero
     * todas las operaciones dentro del procedimiento de firma masiva utilizaran
     * el certificado indicado previamente o durante el proceso de
     * inicializaci&oacute;n.<br>
     * Una vez finalizado el proceso de firma masiva se recomienda utilizar el
     * m&eacute;todo <code>endMassiveSignature()</code> para eliminar la
     * configuraci&oacute;n del procedimiento. <br>
     * Al iniciar el proceso se muestra un di&aacute;logo al usuario
     * para que consienta que se acceda a ficheros en disco durante la operaci&oacute;n.
     * Si no lo hace, se aborta la operaci&oacute;n y se establece un error.<br>
     * <br>
     * Takes the client's massive signature configuration and prepares the
     * massive signature process. Data taken from the client's configuration:
     * <ul>
     * <li>Signature certificate.</li>
     * <li>Signature key.</li>
     * <li>Algorithm.</li>
     * <li>Mode.</li>
     * <li>Default format.</li>
     * <li>Use original signature format.</li>
     * <li>Massive operation to execute (sign, cosign, counter-sign leaves or tree).</li>
     * </ul>
     * In case of a signature certificate, it will be prompted for when
     * unspecified.<br>
     * This method also restarts the massive signature's procedure log. Once the
     * massive signature procedure has been prepared, any changes in client's
     * configuration (except for the operation to be executed) will not affect
     * it. Thus, for example, a different signature certificate could be
     * specified, but all operations within the massive signature procedure will
     * use the previously defined certificate, or the one set in the
     * initialization process.<br>
     * Once the massive signature process is ended, it is advisable to use <code>endMassiveSignature()</code> to eliminate the procedure's
     * configuration.
     * @return Devuelve <code>true</code> si la operaci&oacute;n masiva se
     *         inicializ&oacute; correctamente, <code>false</code> en caso
     *         contrario. <br>
     *         Returns <code>true</code> if the massive operation is properly
     *         initialized. Else, <code>false</code>. */
    boolean initMassiveSignature();

    /** Libera la configuraci&oacute;n del procedimiento de firma masiva
     * previamente inicializado. <br>
     * <br>
     * Frees the massive signature procedure configuration which was previously
     * initialized. */
    void endMassiveSignature();

    /** Firma datos con la configuraci&oacute;n de firma masiva establecida. En
     * el caso de la operaci&oacute;n de firma masiva tendremos que indicar los
     * datos que deseamos firmar, mientras que para las operaciones de cofirma y
     * contrafirma se indicar&aacute; la firma con la que se operar&aacute;. <br>
     * <br>
     * Signs data with the set massive signature configuration. In case of a
     * massive signature operation data to be signed must be indicated. In turn,
     * for massive co-sign and counter-signature operations, must be set the
     * signature to co-sign or counter-sign.
     * @param b64Data
     *        Datos en base 64 a firmar. <br>
     *        Data in base 64 to be signed.
     * @return Firma resultado en base 64 o <code>null</code> en caso de error. <br>
     *         Resulting signature in base 64 or <code>null</code> if error
     *         occurs. */
    String massiveSignatureData(String b64Data);

    /** Firma un hash con la configuraci&oacute;n de firma masiva establecida.
     * Esta m&eacute;todo s&oacute;lo es v&aacute;lido para la operaci&oacute;n
     * de firma masiva (ni cofirmas, ni contrafirmas). <br>
     * <br>
     * Signs a hash with the set massive signature configuration. This method is
     * valid only for the massive signature operation (i.e., not for cosignature
     * or counter-signature).
     * @param b64Hash
     *        Hash en base 64 a firmar. <br>
     *        Hash in base 64 to be signed.
     * @return Firma resultado en base 64 o <code>null</code> en caso de error. <br>
     *         Resulting signature in base 64, or <code>null</code> if error
     *         occurs. */
    String massiveSignatureHash(String b64Hash);

    /** Firma un fichero con la configuraci&oacute;n de firma masiva establecida.
     * En el caso de la operacion de firma masiva tendremos que indicar el
     * fichero con los datos que deseamos firmar, mientras que para la
     * operaci&oacute;n de contrafirma masiva se indicar&aacute; el fichero con
     * la firma que se desea contrafirmar. <br>
     * <br>
     * Signs a file with the set massive signature configuration. In case of a
     * massive signature operation the file with the data to be signed must be
     * indicated. In turn, for massive co-sign and counter-signature operations,
     * must be set the signature file to co-sign or counter-sign.
     * @param fileuri
     *        Ruta del fichero a firmar. <br>
     *        Route to the file to be signed.
     * @return Firma resultado en base 64 o <code>null</code> en caso de error. <br>
     *         Resulting signature in base 64. <code>null</code>, if error
     *         occurs. */
    String massiveSignatureFile(String fileuri);

    /** Devuelve la traza de log de la &uacute;ltima operaci&oacute;n del
     * procedimiento de firma masiva. <br>
     * <br>
     * Returns the log trace for the last massive signature procedure operation.
     * @return Log de la &uacute;ltima operaci&oacute;n. <br>
     *         Last operation's log. */
    String getMassiveSignatureCurrentLog();

    /** Devuelve el log completo del procedimiento de firma masiva. El log de
     * cada operaci&oacute;n se muestra en &uacute;nica l&iacute;nea y se
     * mantiene el orden.
     * @return Log completo del procedimiento de firma masiva. */
    String getMassiveSignatureLog();

    /** Almacena en un fichero todas las trazas de log generadas hasta el momento
     * por el procedimiento de firma masiva. La ruta del fichero se puede
     * establecer mediante <code>setOutFilePath(String)</code>. En caso de no
     * hacerlo aparecer&acute; una ventana para seleccionar donde guardar el
     * fichero de log. <br>
     * <br>
     * Stores in a file every log trace generated by the massive signature
     * procedure. File's route can be set with <code>setOutFilePath(String)</code>.
     * Else, a window will be displayed, prompting the user where to store the
     * log file. */

    void saveMassiveSignatureLog();

    // *******************************************************************/
    // ************* FUNCIONALIDADES DE GESTION DE KEYSTORES *************/
    // ************* KEYSTORE ADMINISTRATION FUNCTIONALITIES *************/
    // *******************************************************************/

    /** Establece el repositorio activo. Ser&aacute; a este repositorio al que se
     * acceder&aacute; para recuperar los certificados para la
     * realizaci&oacute;n de las firmas y el resto de operaciones
     * criptogr&aacute;fica. Los tipos aceptados son:
     * <ul>
     * <li><b>WINDOWS</b>: Repositorio de Microsoft Windows.</li>
     * <li><b>APPLE</b>: Repositorio de Apple Macintosh.</li>
     * <li><b>MOZILLA</b>: Repositorio de Mozilla Firefox.</li>
     * <li><b>P11</b>: Repositorio de tipo PKCS#11 controlador por una libreria de sistema.</li>
     * <li><b>P12</b>: Repositorios en disco en formato P12 o PFX.</li>
     * <li><b>JKS</b>: Repositorios en disco en formato JKS.</li>
     * <li><b>JAVACE</b>: Repositorios en disco en formato Java Case Exact.</li>
     * </ul>
     * Se recomienda utilizar {@code initialize()} antes de usar este
     * m&eacute;todo. <br>
     * Si el almac&eacute;n es o requiere un fichero en disco (PKCS#12, PKCS#11, JKS,...) se muestra
     * un di&aacute;logo al usuario para que consienta la operaci&oacute;n. Si no lo hace, se aborta la
     * operaci&oacute;n y se establece un error.<br>
     * <br>
     * Sets the active repository. This will be accessed for certificate
     * retrieval, in signature and other cryptographic operations. Accepted
     * types:
     * <ul>
     * <li><b>WINDOWS</b>: MS Windows Repository.</li>
     * <li><b>APPLE</b>: Apple Macintosh repository.</li>
     * <li><b>MOZILLA</b>: Mozilla Firefox repository.</li>
     * <li><b>P11</b>: Type PKCS#11 repository, controlled by a system's library.</li>
     * <li><b>P12</b>: Repositories on disk, under P12 or PFX formats.</li>
     * <li><b>JKS</b>: Repositories on disk, under JKS format.</li>
     * <li><b>JAVACE</b>: Repositories on disk, under Java Case Exact format.</li>
     * </ul>
     * It's recommended use {@code initialize()} previously.<br>
     * @param path
     *        Ruta al repositorio (o su controlador) si procede. <br>
     *        Repository route (or its driver, if applicable)
     * @param password
     *        Contrase&ntilde;a para el acceso al repositorio si procede. <br>
     *        Access password to the repository, if applicable.
     * @param type
     *        Tipo de repositorio. <br>
     *        Repository type. */
    void setKeyStore(String path, String password, String type);

    // *******************************************************************/
    // ******************* FUNCIONALIDADES NUEVAS AO *********************/
    // ********************* NEW AO FUNCTIONALITIES *********************/
    // *******************************************************************/

    /** Establece la pol&iacute;tica que debe aplicarse a las firmas que se
     * realicen. Se usa para la generaci&oacute;n de firmas avanzadas EPES.
     * <p>
     *  Para el correcto establecimiento de estos par&aacute;metros consulte detenidamente
     *  la documentaci&oacute;n de la pol&iacute;tica de firma que desee establecer.
     * </p>
     * <p>
     *  Un ejemplo de esteblecimiento de la pol&iacute;tica de firma de la AGE en su versi&oacute;n
     *  1.8 podr&iacute;a ser:
     * </p>
     *  <pre>
     *   setPolicy(
     *     "urn:oid:2.16.724.1.3.1.1.2.1.8", // Identificador (URN de tipo OID)
     *     "Politica de firma electronica para las Administraciones Publicas en Espana", // Descripcion
     *     "http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf", // Calificador
     *     "V8lVVNGDCPen6VELRD1Ja8HARFk=" // Huella digital en Base64
     *   );
     *  </pre>
     * Sets the policy for signatures. Used for the generation of EPES advanced
     * signatures.
     * @param identifier
     *        Identificador de la pol&iacute;tica de firma.<br>
     *        Normalmente una para firmas XAdES es una URL hacia la descripci&oacute;n
     *        formal de la pol&iacute;tica en formato XML o ASN.1 o una URN de tipo OID.<br>
     *        Para firmas CAdES y PAdES debe ser un OID o una URN de tipo OID (por
     *        ejemplo "urn:oid:2.16.724.1.3.2.2.3.1").<br>
     *        Signature policy identifier
     * @param description
     *        Descripci&oacute;n de la pol&iacute;tica.<br>
     *        Policy description.
     * @param qualifier
     *        URL calificadora de la pol&iacute;tica de firma (normalmente
     *        una URL hacia el documento que describe la pol&iacute;tica).<br>
     *        Policy qualifier.
     * @param hashB64
     * 		  Huella digital SHA-1 en Base64 de la pol&iacute;tica de firma.
     * 		  Signature policy's hash.*/
    void setPolicy(String identifier, String description, String qualifier, String hashB64);

    /** Obtiene todos los alias de los certificados disponibles para firmar o
     * cifrar y los devuelve como una &uacute;nica cadena en donde los alias
     * vienen separados por {@link es.gob.afirma.applet.SignApplet#STRING_SEPARATOR}. <br>
     * <br>
     * Gets all certificate aliases available for signing or encrypting, and
     * returns them as a single string with aliases separated by {@link es.gob.afirma.applet.SignApplet#STRING_SEPARATOR}
     * @return Todos los alias de los certificados disponibles. <br>
     *         All available certificate's aliases. */
    String getCertificatesAlias();

    /** Obtiene todos los alias de los certificados disponibles para firmar o
     * cifrar y los devuelve en un array. <br>
     * <br>
     * Gets all signature or encryption available certificates' aliases and
     * returns them in an array.
     * @return Alias de los certificados disponibles. <br>
     *         Available certificates' aliases. */
    String[] getArrayCertificatesAlias();

    /** Obtiene una cadena con todos los certificados del almac&eacute;n actual
     * en ASCII Base64 separados por {@link es.gob.afirma.applet.SignApplet#STRING_SEPARATOR}. <br>
     * <br>
     * Gets a Base 64 ASCII string with all current keystore certificates,
     * separated by {@link es.gob.afirma.applet.SignApplet#STRING_SEPARATOR}.
     * @return Cadena con todos los certificados. <br>
     *         String with all certificates. */
    String getCertificates();

    /** Obtiene un array con todos los certificados del almac&eacute;n actual en
     * ASCII Base64.
     * @return Array con todos los certificados. */
    String[] getArrayCertificates();

    /** Recupera un certificado del repositorio activo. El certificado se recupera en la forma:
     * <p>
     * <code>
     * Bag Attributes<br>
     * friendlyName: CommonName del certificado<br>
     * -----BEGIN CERTIFICATE-----<br>
     * Codificacion del certificado en base 64<br>
     * -----END CERTIFICATE-----<br>
     * </code>
     * </p>
     * <br>
     * <br>
     * Retrieves a certificate from the current repository, under the form:
     * <p>
     * <code>
     * Bag Attributes<br>
     * friendlyName: CertificateCommonName<br>
     * -----BEGIN CERTIFICATE-----<br>
     * Certificate encryption in base 64<br>
     * -----END CERTIFICATE-----<br>
     * </code>
     * </p>
     * @param alias
     *        Alias del certificado que deseamos recuperar. <br>
     *        Certificate alias meant for retrieval.
     * @return Certificado recuperado o <code>null</code> en caso de error. <br>
     *         Retrieved certificate or <code>null</code> if error occurs. */
    String getCertificate(String alias);

    /** Recupera la clave p&uacute;blica de un certificado del repositorio activo.
     *  La clave se recupera en la forma en la forma:
     * <p>
     * <code>
     * -----BEGIN RSA KEY-----<br>
     * Clave p&uacute;blica del certificado en base 64<br>
     * -----END RSA KEY-----<br>
     * </code>
     * </p>
     * <br>
     * <br>
     * Retrieves a certificate's key from the current repository, under
     * the form:
     * <p>
     * <code>
     * -----BEGIN RSA KEY-----<br>
     * Certificate's Key in base 64<br>
     * -----END RSA KEY-----<br>
     * </code>
     * </p>
     * @param alias
     *        Alias del certificado del que queremos recuperar la clave
     *        p&uacute;blica. <br>
     *        Alias of certificate to recover key from.
     * @return Clave p&uacute;blica del certificado o <code>null</code> en caso
     *         de error. <br>
     *         Certificate's key or <code>null</code> if error occurs. */
    String getCertificatePublicKey(String alias);

    /** Establece el alias del certificado activo. <br>
     * <br>
     * Sets the current certificate's alias.
     * @param certAlias
     *        Alias del certificado. Debe coincidir con un alias de un
     *        certificado presente en el <code>KeyStore</code> activo <br>
     *        Certificate's alias. Must match an alias of a certificate
     *        found in the active <code>KeyStore</code>. */
    void setSelectedCertificateAlias(String certAlias);

    /** Establece un filtro para la selecci&oacute;n de certificados, solo se
     * mostrar&aacute;n en el di&aacute;logo de selecci&oacute;n los que cumplan
     * las condiciones proporcionados. <b>Importante:</b> El filtro no se aplica
     * en la obtenci&oacute;n directa de los alias del almac&eacute;n
     * (getCertificatesAlias, getCertificates,...). <br>
     * <br>
     * Sets a filter for the selection of certificates. Only those complying
     * with selection criteria will be displayed in the selection dialog.
     * <b>N.B.:</b> Filter is not applied in the direct retrieval of keystore
     * aliases (getCertificatesAlias, getCertificates, etc.).
     * @param subjectFilter
     *        Filtro para el titular del certificado, seg&uacute;n formato
     *        definido en la normativa RFC 2554 <br>
     *        Certificate's holder filter, in accordance to format defined
     *        in RFC 2554.
     * @param issuerFilter
     *        Filtro para el emisor del certificado, seg&uacute;n formato
     *        definido en la normativa RFC 2554 <br>
     *        Certificate's issuer filter, as defined in RFC 2554.
     * @param onlySignatureCertificates
     *        Si se establece a <code>true</code> se muestran para
     *        selecci&oacute;n &uacute;nicamente los certificados con el bit
     *        <i>nonRepudiation</i> del campo <i>KeyUsage</i> activado, si
     *        se establece a <code>false</code> se muestran todos los
     *        certificados <br>
     *        If set to <code>true</code>, only certificates with an
     *        activated <i>nonRepudiation</i> bit in field <i>KeyUsage</i>
     *        bit are displayed for selection. If set to <code>false</code> all certificates are displayed. */
    void setCertFilterRFC2254(String subjectFilter, String issuerFilter, boolean onlySignatureCertificates);

    /** Establece una condici&oacute;n para seleccionar el certificado con el que
     * se ha de firmar. No se permite elegir certificado al usuario pues se
     * entiende que se le "obliga" a firmar con uno concreto. En el caso, de
     * encontrar m&aacute;s de un certificado que cumpla la condici&oacute;n se
     * considera que el usuario tiene m&aacute;s de un certificado de ese tipo y
     * S&Iacute; se le permite elegir cual usar. Si ning&uacute;n certificado
     * instalado en el navegador cumple la condici&oacute;n, se mostrar&aacute;
     * un error. <br>
     * <br>
     * Sets a condition to select a certificate with which to sign. Users are
     * not allowed to choose the certificate as they are deemed "forced" to sign
     * with a specific one. In case of more than one certificate satisfying the
     * condition, it is assumed that the user has more than one of that kind and
     * is thus allowed to choose. If no certificate installed in the browser
     * satisfies the condition, an error will be displayed.
     * @param subjectFilter
     *        Filtro para el titular del certificado, seg&uacute;n formato
     *        definido en la normativa RFC 2554 <br>
     *        Filter for Certificate's holder, as defined by RFC 2554.
     * @param issuerFilter
     *        Filtro para el emisor del certificado, seg&uacute;n formato
     *        definido en la normativa RFC 2554 <br>
     *        Filter for the certificate issuer, as defined by RFC 2554.
     * @param onlySignatureCertificates
     *        Si se establece a <code>true</code> se muestran para
     *        selecci&oacute;n &uacute;nicamente los certificados con el bit
     *        <i>nonRepudiation</i> del campo <i>KeyUsage</i> activado, si
     *        se establece a <code>false</code> se muestran todos los
     *        certificados <br>
     *        If set to <code>true</code> only certificates with an active
     *        <i>nonRepudiation</i> bit in field <i>KeyUsage</i> are
     *        displayed for selection, if set to <code>false</code> all
     *        certificates are displayed. */
    void setMandatoryCertificateConditionRFC2254(String subjectFilter, String issuerFilter, boolean onlySignatureCertificates);

    /** Muestra el di&aacute;logo de selecci&oacute;n de certificados del
     * almac&eacute;n seleccionado, aplicando los filtros de certificado de ser
     * necesario. Si ya exist&iacute;a un certificado seleccionado, lo devuelve
     * directamente. Devuelve nulo si hay alg&uacute;n problema durante la
     * selecci&oacute;n o se cancela el di&aacute;logo.<br>
     * El certificado seleccionado queda establecido como &uacute;ltimo
     * certificado utilizado, con lo cual puede recuperarse mediante los
     * m&eacute;todos <code>getSignCertificate()</code> y <code>getSignCertificateBase64Encoded()</code>.<br>
     * <br>
     * Shows the selected keystore certificate selection dialog, applying
     * filters, if necessary. If a certificate is selected already, it's returned.
     * Returns null if a problem occurs during the selection, or dialog is cancelled.<br>
     * Selected certificate is set as last used certificate, i.e., it may be
     * recovered with <code>getSignCertificate()</code> and <code>getSignCertificateBase64Encoded()</code>.
     * @return Alias real (con el que fue dado de alta en el almac&eacute;n de
     *         claves) del certificado seleccionado. <br>
     *         Selected certificate's real alias (alias used for registering in
     *         the keystore) */
    String showCertSelectionDialog();

}
