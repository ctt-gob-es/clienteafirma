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

import java.security.cert.X509Certificate;

/**
 * Contiene los puntos de entrada de las funcionalidades criptogr&aacute;ficas del
 * Cliente AFirma para su uso como Applet Java.
 */
public interface EntryPointsCrypto {

	/**
	 * Realiza el proceso de firma digital.<br>
	 * <br>
	 * <b>Par&aacute;metros de entrada:</b><br>
	 * <dl>
	 *  <dt>
	 *   Para la selecci&oacute;n de certificado (opcional, excluyentes), si no se especifica ninguno,
	 *   se muestra una ventana al usuario para que escojaentre todos los certificados que tenga instalados
	 *  </dt>
	 *  <dd>
	 *   <ul>
	 *    <li>mandatoryCertCondition</li>
	 *    <li>certFilter</li>
	 *   </ul>
	 *  </dd>
	 *  <dt>
	 *   Para la selecci&oacute;n de datos de entrada(opcional, excluyentes): Si no se especifica 
	 *   ninguno, se muestra una ventana al usuario para que escoja un fichero a firmar
	 *  </dt>
	 *  <dd>
	 *   <ul>
	 *    <li>data</li>
	 *    <li>hash</li>
	 *    <li>hashes (firma masiva)</li>
	 *    <li>fileUri</li>
	 *   </ul>
	 *  </dd>
	 *  <dt>
	 *   Para la firma digital(opcionales): Si no se especifican, se toman los valores por defecto
	 *  </dt>
	 *  <dd>
	 *   <ul>
	 *    <li>signatureAlgorithm</li>
	 *    <li>signatureFormat</li>
	 *   </ul>
	 *  </dd>
	 *  <dt>
	 *   Par&aacute;metros de salida:
	 *  </dt>
	 *  <dd>
	 *   <dl>
	 *    <dt>Si se produjo un error <code>(isError()==true)</code>:</dt>
	 *    <dd>errorMessage</dd>
	 *    <dt>Si no se produjo <code>(isError()==false)</code>:</dt>
	 *    <dd>
	 *     <ul>
	 *      <li>signatureBase64Encoded</li>
	 *      <li>fileUsedPath (si se han le&iacute;do los datos a firmar de un fichero)</li>
	 *      <li>signCertificateBase64Encoded</li>
	 *     </ul>
	 *    </dd>
	 *   </dl>
	 *  </dd>
	 * </dl>
	 * @see #setMandatoryCertificateCondition(String)
	 * @see #setCertFilterRFC2254(String, String, boolean)
	 * @see #setData(String)
	 * @see #setFileuri(String)
	 * @see #setSignatureAlgorithm(String)
	 * @see #setSignatureFormat(String)
	 * @return <code>true</code> si se ha ejecutado correctamente, <code>false</code> en caso contrario
	 */
	public boolean sign();

	/**
	 * Firma los datos pasados en base 64.
	 * 
	 * @param b64data Datos a firmar, en formato Base64
	 * @return <code>true</code> si &eacute;xito, falso si ha habido alg&uacute;n fallo
	 * @see #getSignatureBase64Encoded()
	 * @see #getSignatureText()
	 */
	public boolean signData(String b64data);

	/**
	 * Metodo que se mantiene por compatibilidad con las versi&oacute;n 4 y anteriores
	 * de la plataforma @firma. 
	 * @param   datos Datos codificado en base 64.
	 * @return  Cadena que contiene el certificado y los datos firmados 
	 *          todo codificado en base 64.
	 */
	public String Firma(String datos);

	/** A&ntilde;ade un atributo firmado al formato de firma seleccionado. Este formato debe reconocer el 
	 * OID especificado, siendo el atributo {@code value} su valor como cadena de texto.
	 * 
	 * @param oid Object Identifier. Identificador del objeto a introducir.
	 * @param value Valor asignado
	 * @return true si se ha realizado correctamente.
	 */
	public boolean addSignedAttribute(String oid, String value);

	/**
	 * Elimina el atributo con el OID indicado antes de realizar la firma.
	 * 
	 * @param oid Object Identifier.
	 * @return true si se ha eliminado correctamente. False si no exist&iacute;a o ha ocurrido alg&uacute;n error.
	 */
	public boolean removeSignedAttribute(String oid);

	/**
	 * A&ntilde;ade un atributo no firmado al formato de firma seleccionado. 
	 * 
	 * @param oid Object Identifier. Identificador del atributo a introducir.
	 * @param value Valor asignado
	 * @return true si se ha realizado correctamente.
	 */
	public boolean addUnsignedAttribute(String oid, String value);

	/**
	 * Elimina el atributo con el OID indicado antes de realizar la firma.
	 * 
	 * @param oid Object Identifier.
	 * @param value Valor especifico a borrar.
	 * @return true si se ha eliminado correctamente. False si no exist&iacute;a o ha ocurrido alg&uacute;n error.
	 */
	public boolean removeUnsignedAttribute(String oid, String value);

    /**
     * Agrega una propiedad adicional a la configuraci&oacute;n de firma.
     * @param key Propiedad que se desea establecer.
     * @param value Valor que se desea establecer a la propiedad.
     */
    public void addExtraParam(String key, String value);
	
    /**
     * Elimina una propiedad adicional de la configuraci&oacute;n de firma.
     * @param key propiedad que se desea eliminar.
     */
    public void removeExtraParam(String key);
    
    /**
     * Agrega una nueva transformaci&oacute;n XML a la configuraci&oacute;n del cliente.
     * Esta transformaci&oacute;n ser&aacute; tenida en cuenta por aquellos formatos de firma
     * que permitan configurar las transformaciones XML que deben aplicarse sobre los datos.<br/>
     * Este m&eacute;todo puede utilizarse varias veces para establecer un conjunto de
     * transformaciones que se apliquen a las firmas de forma ordenada.
     * Existen varios tipos de transformaciones, dentro de los cuales algunos tienen subtipos.
     * Los tipos y subtipos reglados son:
     * <ul>
     * <li><b>http://www.w3.org/TR/1999/REC-xpath-19991116</b>: Transformaci&oacute;n de tipo XPATH.</li>
     * <li><b>http://www.w3.org/2002/06/xmldsig-filter2</b>: Transformaci&oacute;n de tipo XPATH2.<br/>
     *     Subtipos:
     *     <ul>
     *         <li>subtract</li>
     *         <li>intersect</li>
     *         <li>union</li>
     *     </ul>
     * </li>
     * <li><b>http://www.w3.org/TR/1999/REC-xslt-19991116</b>: Transformaci&oacute;n de tipo XSLT.</li>
     * <li><b>http://www.w3.org/2000/09/xmldsig#base64</b>: Transformaci&oacute;n de tipo BASE64.</li>
     * <li><b>http://www.w3.org/2000/09/xmldsig#enveloped-signature</b>: Transformaci&oacute;n de tipo ENVELOPED.</li>
     * </ul>
     * En el caso en que una transformaci&oaute;n no tenga subtipo, se indicar&aacute; {@code null}.
     * Las transformaciones de tipo BASE64 y ENVELOPED no necesitan cuerpo (se usar&aacute; {@code null})
     * @param type Tipo de transformaci&oacute;n.
     * @param subtype Subtipo de transformaci&oacute;n (opcional).
     * @param body Cuerpo de la transformaci&oacute;n.
     */
    public void addXMLTransform(String type, String subtype, String body);
    
    /**
     * Elimina todas las transformaciones XML configuradas en el cliente.
     */
    public void resetXMLTransforms();
    
	/**
	 * Realiza el proceso de co-firmado (firma en paralelo). Los par&aacute;metros de operaci&oacute;n
	 * que pueden establecerse y los m&eacute;todos para ello son:<br>
	 * <ul>
	 * <li><b>Datos a firmar (excluyentes): </b></li>
	 * <ul>
	 * <li>{@link #setData(String)}</li>
	 * <li>{@link #setFileuri(String)}</li>
	 * </ul>
	 * <li><b>Firma original que se desea cofirmar (excluyentes): </b></li>
	 * <ul>
	 * <li>{@link #setElectronicSignature(String)}</li>
	 * <li>{@link #setElectronicSignatureFile(String)}</li>
	 * </ul>
	 * <li><b>Certificado de firma (excluyentes): </b></li>
	 * <ul>
	 * <li>{@link #setMandatoryCertificateCondition(String)}</li>
	 * <li>{@link #setCertFilterRFC2254(String, String, boolean)}</li>
	 * </ul>
	 * <li><b>Configuraci&oacute;n de firma: </b></li>
	 * <ul>
	 * <li>{@link #setSignatureAlgorithm(String)}</li>
	 * <li>{@link #setSignatureFormat(String)}</li>
	 * <li>{@link #setSignatureMode(String)}</li>
	 * </ul>
	 * </ul>
	 * Los par&aacute;mnetros establecidos como resultado de esta operaci&oacute;n son:
	 * <ul>
	 * <li>Firma codificada en base 64 ({@link #getSignatureBase64Encoded()})</li>
	 * <li>Firma en modo texto ({@link #getSignatureText()})</li>
	 * <li>Ruta al fichero firmado si no se introdujeron los datos directamente ({@link #getFileUsedPath()})</li>
	 * <li>Certificado de firma en base 64 ({@link #getSignCertificateBase64Encoded()})</li>
	 * </ul>
	 * En caso de error, la descripci&oacute;n del mismo puede recuperarse mediante el m&eacute;todo
	 * getErrorMessage() del cliente.
	 * 
	 * @return <code>true</code> si la operaci&oacute;n ha finalizado correctamente, <code>false</code> en caso contrario. 
	 */
	public boolean coSign();

	/**
	 * Devuelve la estructura de firmantes de una firma electr&oacute;nica. Los firmantes se separan
	 * por '\n' y comienzan por tantos '\t' como el nivel en el que est&aacute;n.<br>
	 * <br>
	 * Por ejemplo:
	 * <ul>
	 * <li>Firma en paralelo: "firmante1\nfirmante2\nfirmante3"</li>
	 * <li>Firma en cascada: "firmante1\n\tfirmante2\n\t\tfirmante3"</li>
	 * <li>Firma 'compleja': "firmante1\n\tfirmante2\n\tfirmante3\n\t\tfirmante4\nfirmante5"</li>
	 * </ul>
	 * La firma puede ser establecida mediante {@link #setElectronicSignature(String)} o
	 * {@link #setElectronicSignatureFile(String)}. El an&aacute;lisis de la firma se realiza
	 * primeramente por medio del manejador del formato de firma establecido con
	 * {@link #setSignatureFormat(String)} y, en caso de que este manejador no soporte la firma
	 * introducida, se buscar&aacute; entre el resto de manejadores el más adecuado para manejar
	 * la firma. Si no se encuentra ninguno, se devuelve <code>null</code>.
	 * @return Una cadena en la que se representa la estructura firmas (en paralelo y cascada).
	 */
	public String getSignersStructure();

	/**
	 * Realiza el proceso de contra-firmado (firma en cascada). Contra-firma todos los nodos
	 * de un o varios firmantes.<br>
	 * <br>
	 * <b>Par&aacute;metros de entrada:</b><br>
	 * - Para la selecci&oacute;n de certificado (opcional, excluyentes): Si no se especifica ninguno, se muestra una ventana al usuario para que escoja 
	 * entre todos los certificados que tenga instalados<br>
	 * <pre>    - mandatoryCertCondition</pre>
	 * <pre>    - certFilter</pre>
	 * - Para la firma digital(opcionales): Si no se especifican, se toman los valores por defecto <br>
	 * <pre>    - signatureAlgorithm</pre>
	 * <pre>    - signatureFormat</pre>
	 * - Firma electr&oacute;nica de entrada(opcional, excluyentes): 
	 * <pre>    - electronicSignature</pre>
	 * <pre>    - electronicSignatureFile</pre>
	 * - Firmantes a contra-firmar:
	 * <pre>    - signersToCounterSign (nombres de firmantes devueltos por getSignersStructure() separados por '\n' )</pre>
	 * <br>
	 * <b>Par&aacute;metros de salida:</b><br>
	 * - Si se produjo un error (isError()==true):<br>
	 * <pre>    - errorMessage</pre>
	 * - Si no se produjo (isError()==false)<br>
	 * <pre>    - signatureBase64Encoded</pre>
	 * <pre>    - fileUsedPath (si se han le&iacute;do los datos a firmar de un fichero) </pre>
	 * <pre>    - signCertificateBase64Encoded</pre>
	 * <br>
	 * 
	 * @see #setMandatoryCertificateCondition(String)
	 * @see #setCertFilterRFC2254(String, String, boolean)
	 * @see #setData(String)
	 * @see #setFileuri(String)
	 * @see #setSignatureAlgorithm(String)
	 * @see #setSignatureFormat(String)
	 * @see #setElectronicSignature(String)
	 * @see #setElectronicSignatureFile(String)
	 * @see #getSignatureBase64Encoded()
	 * @see #getFileUsedPath()
	 * @see #getSignCertificateBase64Encoded()
	 * @see #getSignersStructure()
	 * 
	 * @return true si se ha ejecutado correctamente
	 */
	public boolean counterSignSigners();

	/**
	 * Realiza el proceso de contra-firmado (firma en cascada). Contra-firma los nodos
	 * con determinados &iacute;ndices.<br>
	 * <br>
	 * <b>Par&aacute;metros de entrada:</b><br>
	 * - Para la selecci&oacute;n de certificado (opcional, excluyentes): Si no se especifica ninguno, se muestra una ventana al usuario para que escoja 
	 * entre todos los certificados que tenga instalados<br>
	 * <pre>    - mandatoryCertCondition</pre>
	 * <pre>    - certFilter</pre>
	 * - Para la firma digital(opcionales): Si no se especifican, se toman los valores por defecto <br>
	 * <pre>    - signatureAlgorithm</pre>
	 * <pre>    - signatureFormat</pre>
	 * - Firma electr&oacute;nica de entrada(opcional, excluyentes): 
	 * <pre>    - electronicSignature</pre>
	 * <pre>    - electronicSignatureFile</pre>
	 * - Firmantes a contra-firmar:
	 * <pre>    - signersToCounterSign (indices de nodos de getSignersStructure() separados por '\n')</pre>
	 * <br>
	 * <b>Par&aacute;metros de salida:</b><br>
	 * - Si se produjo un error (isError()==true):<br>
	 * <pre>    - errorMessage</pre>
	 * - Si no se produjo (isError()==false)<br>
	 * <pre>    - signatureBase64Encoded</pre>
	 * <pre>    - fileUsedPath (si se han le&iacute;do los datos a firmar de un fichero) </pre>
	 * <pre>    - signCertificateBase64Encoded</pre>
	 * <br>
	 * @return Devuelve <codetrue</code> si la operaci&oacute;n finaliz&oacute; correctamente, false en caso contrario.
	 * @see #setMandatoryCertificateCondition(String)
	 * @see #setCertFilterRFC2254(String, String, boolean)
	 * @see #setData(String)
	 * @see #setFileuri(String)
	 * @see #setSignatureAlgorithm(String)
	 * @see #setSignatureFormat(String)
	 * @see #setElectronicSignature(String)
	 * @see #setElectronicSignatureFile(String)
	 * @see #getSignatureBase64Encoded()
	 * @see #getFileUsedPath()
	 * @see #getSignCertificateBase64Encoded()
	 * 
	 */
	public boolean counterSignIndexes();

	/**
	 * Realiza el proceso de contra-firmado (firma en cascada). Contra-firma todos los nodos del &aacute;rbol.
	 * <br>
	 * <b>Par&aacute;metros de entrada:</b><br>
	 * - Para la selecci&oacute;n de certificado (opcional, excluyentes): Si no se especifica ninguno, se muestra una ventana al usuario para que escoja 
	 * entre todos los certificados que tenga instalados<br>
	 * <pre>    - mandatoryCertCondition</pre>
	 * <pre>    - certFilter</pre>
	 * - Para la firma digital(opcionales): Si no se especifican, se toman los valores por defecto <br>
	 * <pre>    - signatureAlgorithm</pre>
	 * <pre>    - signatureFormat</pre>
	 * - Firma electr&oacute;nica de entrada(opcional, excluyentes): 
	 * <pre>    - electronicSignature</pre>
	 * <pre>    - electronicSignatureFile</pre>
	 * <br>
	 * <b>Par&aacute;metros de salida:</b><br>
	 * - Si se produjo un error (isError()==true):<br>
	 * <pre>    - errorMessage</pre>
	 * - Si no se produjo (isError()==false)<br>
	 * <pre>    - signatureBase64Encoded</pre>
	 * <pre>    - fileUsedPath (si se han le&iacute;do los datos a firmar de un fichero) </pre>
	 * <pre>    - signCertificateBase64Encoded</pre>
	 * <br>
	 * @return true si se ha ejecutado correctamente
	 * 
	 * @see #setMandatoryCertificateCondition(String)
	 * @see #setCertFilterRFC2254(String, String, boolean)
	 * @see #setData(String)
	 * @see #setFileuri(String)
	 * @see #setSignatureAlgorithm(String)
	 * @see #setSignatureFormat(String)
	 * @see #setElectronicSignature(String)
	 * @see #setElectronicSignatureFile(String)
	 * @see #getSignatureBase64Encoded()
	 * @see #getFileUsedPath()
	 * @see #getSignCertificateBase64Encoded() 
	 */
	public boolean counterSignTree();

	/**
	 * Realiza el proceso de contra-firmado (firma en cascada). Contra-firma las hojas del &aacute;rbol.<br>
	 * <br>
	 * <b>Par&aacute;metros de entrada:</b><br>
	 * - Para la selecci&oacute;n de certificado (opcional, excluyentes): Si no se especifica ninguno, se muestra una ventana al usuario para que escoja 
	 * entre todos los certificados que tenga instalados<br>
	 * <pre>    - mandatoryCertCondition</pre>
	 * <pre>    - certFilter</pre>
	 * - Para la firma digital(opcionales): Si no se especifican, se toman los valores por defecto <br>
	 * <pre>    - signatureAlgorithm</pre>
	 * <pre>    - signatureFormat</pre>
	 * - Firma electr&oacute;nica de entrada(opcional, excluyentes): 
	 * <pre>    - electronicSignature</pre>
	 * <pre>    - electronicSignatureFile</pre>
	 * <br>
	 * <b>Par&aacute;metros de salida:</b><br>
	 * - Si se produjo un error (isError()==true):<br>
	 * <pre>    - errorMessage</pre>
	 * - Si no se produjo (isError()==false)<br>
	 * <pre>    - signatureBase64Encoded</pre>
	 * <pre>    - fileUsedPath (si se han le&iacute;do los datos a firmar de un fichero) </pre>
	 * <pre>    - signCertificateBase64Encoded</pre>
	 * <br>
	 * 
	 * 
	 * @return true si se ha ejecutado correctamente
	 * @see #setMandatoryCertificateCondition(String)
	 * @see #setCertFilterRFC2254(String, String, boolean)
	 * @see #setData(String)
	 * @see #setFileuri(String)
	 * @see #setSignatureAlgorithm(String)
	 * @see #setSignatureFormat(String)
	 * @see #setElectronicSignature(String)
	 * @see #setElectronicSignatureFile(String)
	 * @see #getSignatureBase64Encoded()
	 * @see #getFileUsedPath()
	 * @see #getSignCertificateBase64Encoded() 
	 */
	public boolean counterSignLeafs();


	/**
	 * Guarda una firma electr&oacute;nica generada  anteriormente en un fichero. La firma
	 * electr&oacute;nica se habr&aacute; generado mediante una de las operaciones de firma,
	 * cofirma o contrafirma. La ruta en donde se almacenar&aacute; se establece mediante
	 * {@link #setOutFilePath(String)}. Si no se ha establecido una ruta en la que almacenar
	 * la firma, se mostrar&aacute; un di&aacute;logo de guardado.   
	 * @return true si el proceso termina correctamente, false en otro caso.
	 */
	public boolean saveSignToFile();

	/**
	 * Devuelve el certificado en base 64 con el que se ha firmado. Esta operaci&oacute;n devuelve
	 * el &uacute;ltimo certificado cargado durante la operacion de firma, luego una operaci&oacute;n
	 * de firma fallida tras la selecci&oacute;n del certificado lleva a devolver la referencia a un
	 * certificado que puede no ser el utilizado en la &uacute;ltima operacion de firma satisfactoria.
	 * En caso de no haber ning&uacute;n certificado seleccionado se devolver&aacute; cadena
	 * vac&iacute;a.  
	 * @return El certificado de firma en base 64.
	 */
	public String getSignCertificateBase64Encoded();

	/**
	 * Devuelve el certificado con el que se ha firmado. Esta operaci&oacute;n devuelve
	 * el &uacute;ltimo certificado cargado durante la operacion de firma, luego una operaci&oacute;n
	 * de firma fallida tras la selecci&oacute;n del certificado lleva a devolver la referencia a un
	 * certificado que puede no ser el utilizado en la &uacute;ltima operacion de firma satisfactoria.
	 * En caso de no haber ning&uacute;n certificado seleccionado se devolver&aacute; <code>null</code>.  
	 * @return El certificado de firma.
	 */
	public X509Certificate getSignCertificate();

	/**
	 * Establece el algoritmo de firma. Puede ser <code>MD5withRSAEncryption</code> o
	 * <code>SHA1withRSAEncryption</code> (se desaconseja el primero por motivos de obsolescencia del
	 * algoritmo MD5). Algunos formatos de firma no soportan los dos algoritmos.
	 * @param signatureAlgorithm Nombre del algoritmo a usar en las operaciones de firma
	 */
	public void setSignatureAlgorithm(String signatureAlgorithm);


	/**
	 * Establece el formato de la firma electr&oacute;nica generada. Puede ser <cite>CMS</cite>,
	 * o <cite>NONE</cite>. Si se incorpora otros formatos, se permitir&aacute;n estos tambi&eacute;n. 
	 * @param signatureFormat Formato de la firma electr&oacute;nica a generar
	 */
	public void setSignatureFormat(String signatureFormat);


	/**
	 * Establece un filtro para los certificados que se mostrar&aacute;n al usuario.<br>
	 * Las condiciones simples siguen el siguiente patr&oacute;n:<br>
	 * {campoDelCertificado operador {"valor"}}<br>
	 * <b>campoDelCertificado</b>: ISSUER.DN (Domain Name del emisor),
	 * SUBJECT.DN (Domain Name del certificado), SUBJECT.FP(MD5) (Huella digital
	 * en MD5 del certificado, en hexadecimal separado por ':',
	 * (D0:BA:34:4D...)), SUBJECT.FP(SHA1) (Huella digital en SHA1 del
	 * certificado, en hexadecimal separado por ':', (D0:BA:34:4D...)),
	 * SUBJECT.SN (N&uacute;mero de serie del certificado)<br>
	 * <b>operador</b>: = (igual o equivalente), #MATCHES# (cumple un expresi&oacute;n
	 * regular)<br>
	 * <b>valor</b>: valor con el que se quiere operar<br>
	 * <br>
	 * Ejemplos de condiciones simples:<br>
	 * {ISSUER.DN={"OU = FNMT Clase 2 CA,O = FNMT,C = ES"}}<br>
	 * {ISSUER.DN#MATCHES#{"CN=AC DNIE 00(1|2|3),OU=DNIE,O=DIRECCION GENERAL DE
	 * LA POLICIA,C=ES"}}<br>
	 * {SUBJECT.DN#MATCHES#{".*ESPAÑOL ESPAÑOL.*"}}<br>
	 * <br>
	 * Las condiciones compuestas siguen el siguiente patr&oacute;n:<br>
	 * {(condicionSimple o condicionCompuesta) (nexo (condicionSimple o
	 * condicionCompuesta))*}<br>
	 * nexo: && (Y l&oacute;gico), || (O l&oacute;gico)<br>
	 * <br>
	 * Ejemplos de condiciones compuestas:<br>
	 * {SUBJECT.SN={"1014673794"}&&ISSUER.DN={"OU = FNMT Clase 2 CA,O= FNMT,C =
	 * ES"}}<br>
	 * {ISSUER.DN#MATCHES#{"CN=AC DNIE 00(1|2|3),OU=DNIE,O=DIRECCION GENERAL DE
	 * LA POLICIA,C=ES"}&&{SUBJECT.DN#MATCHES#{".*(FIRMA).*"}}}<br>
	 * 
	 * @param certFilter Filtro para los certificados a mostrar al usuario
	 * @deprecated Sustituido por {@link #setCertFilterRFC2254(String, String, boolean)}
	 * @see #setCertFilterRFC2254(String, String, boolean)
	 */
	@Deprecated
	public void setCertFilter(String certFilter);


	/**
	 * Establece una condici&oacute;n para seleccionar el certificado con el que se ha de firmar. No se permite elegir certificado al usuario
	 * pues se entiende que se le "obliga" a firmar con uno concreto. Si ning&uacute;n certificado instalado en el navegador cumple la condici&oacute;n,
	 * se mostrar&aacute; un error. Si m&aacute;s de uno la cumple, tambi&eacute;n (la condici&oacute;n no selecciona un certificado &uacute;nico).<br>
	 * <br>
	 * Las condiciones simples siguen el siguiente patr&oacute;n:<br>
	 * {campoDelCertificado operador {"valor"}}<br>
	 * <b>campoDelCertificado</b>: ISSUER.DN (Domain Name del emisor),
	 * SUBJECT.DN (Domain Name del certificado), SUBJECT.FP(MD5) (Huella digital
	 * en MD5 del certificado, en hexadecimal separado por ':',
	 * (D0:BA:34:4D...)), SUBJECT.FP(SHA1) (Huella digital en SHA1 del
	 * certificado, en hexadecimal separado por ':', (D0:BA:34:4D...)),
	 * SUBJECT.SN (N&uacute;mero de serie del certificado)<br>
	 * <b>operador</b>: = (igual o equivalente), #MATCHES# (cumple un expresi&oacute;n
	 * regular)<br>
	 * <b>valor</b>: valor con el que se quiere operar<br>
	 * <br>
	 * Ejemplos de condiciones simples:<br>
	 * {ISSUER.DN={"OU = FNMT Clase 2 CA,O = FNMT,C = ES"}}<br>
	 * {ISSUER.DN#MATCHES#{"CN=AC DNIE 00(1|2|3),OU=DNIE,O=DIRECCION GENERAL DE
	 * LA POLICIA,C=ES"}}<br>
	 * {SUBJECT.DN#MATCHES#{".*ESPAÑOL ESPAÑOL.*"}}<br>
	 * <br>
	 * Las condiciones compuestas siguen el siguiente patr&oacute;n:<br>
	 * {(condicionSimple o condicionCompuesta) (nexo (condicionSimple o
	 * condicionCompuesta))*}<br>
	 * nexo: && (Y l&oacute;gico), || (O l&oacute;gico)<br>
	 * <br>
	 * Ejemplos de condiciones compuestas:<br>
	 * {SUBJECT.SN={"1014673794"}&&ISSUER.DN={"OU = FNMT Clase 2 CA,O= FNMT,C =
	 * ES"}}<br>
	 * {ISSUER.DN#MATCHES#{"CN=AC DNIE 00(1|2|3),OU=DNIE,O=DIRECCION GENERAL DE
	 * LA POLICIA,C=ES"}&&{SUBJECT.DN#MATCHES#{".*(FIRMA).*"}}}<br>
	 * 
	 * @param mandatoryCertificateCondition Condici&oacute;n que selecciona un (y s&oacute;lo un) certificado. 
	 * @see #setCertFilter(String)
	 * @deprecated Usar &uacute;nicamente {@link #setMandatoryCertificateConditionRFC2254(String, String, boolean)}
	 * @see #setMandatoryCertificateConditionRFC2254(String, String, boolean)
	 */
	@Deprecated
	public void setMandatoryCertificateCondition(String mandatoryCertificateCondition);

	/**
	 * Establece los los datos cifrados en base 64 que se van a descifrar mediante una pr&oacute;xima
	 * llamada a {@link #decipherData()}. 
	 * @param data Datos cifrados en base 64.
	 */
	public void setCipherData(String data);


	/**
	 * Define los datos planos que se van a cifrar mediante una pr&oacute;xima llamada a {@link #cipherData()}.
	 * @param data Datos planos a cifrar.
	 */
	public void setPlainData(String data);


	/**
	 * Devuelve los datos cifrados. Si no se han establecido datos cifrados,
	 * se devolver&aacute; <code>null</code> 
	 * @return String en Base64 con el texto cifrado.
	 */
	public String getCipherData();


	/**
	 * Devuelve los datos planos que se han introducido para cifrar o el texto resultado de un descifrado
	 * (lo &uacute;ltimo que haya ocurrido). En caso de no haber ocurrido ninguna de estas condiciones, se
	 * devolver&aacute; <code>null</code>.
	 * @return String Texto plano de la operaci&oacute;n de cifrado / descifrado.
	 * @see #decipherData()
	 * @see #setPlainData(String)
	 */
	public String getPlainData();

	/**
	 * Establece la ruta del fichero en donde se almacenar&aacute;n los datos resultados
	 * de la operaci&oacute;n realizada. 
	 * @param outFilePath Ruta por defecto del fichero de salida.
	 */
	public void setOutFilePath(String outFilePath);


	/**
	 * Devuelve la estructura de firma generada o establecida codificada en base 64.
	 * Si no se dispone de una firma, se devuelve cadena vac&iacute;a.
	 * 
	 * @return La firma en base 64.
	 * @see #getSignatureText()
	 */
	public String getSignatureBase64Encoded();

	/**
	 * Devuelve la firma generadaen el &uacute;ltimo proceso de firma o establecida por en el cliente
	 * como un String (&uacute;til para firmas XAdES, XMLDSign y firma web). Si no se ha generado una
	 * firma, se devuelve cadena vac&iacute;a.<br/>
	 * El uso de este m&eacute;todo no esta recomendado ya que el resultado de la
	 * que devuelve puede variar seg&uacute;n la codificaci&oacute;n establecida por defecto. Su principal
	 * utilidad es mostrar el resultado de una firma XML. Para firmas binarias debe utilizarse
	 * {@link #getSignatureBase64Encoded()}.
	 *  
	 * Si no se dispone de una firma, se devuelve cadena vac&iacute;a.
	 * @return La &uacute;ltima firma electr&oacute;nica generada o establecida por el cliente. 
	 */
	public String getSignatureText();


	/**
	 * Establece los datos de entrada en base 64 para los procesos de firma, co-firma (firma en
	 * paralelo) y generación de sobres digitales.
	 * <br/>Los datos introducidos mediante esta funci&oacuten;n sobreescribir&aacute;n cualquier otro
	 * establecido previamente mediante los m&eacute;todos setFileuri o setHash.
	 * @param data Datos a operar codificados en base 64
	 * @see #setHash(String)
	 * @see #setFileuri(String)
	 */
	public void setData(String data);
	
	/**
	 * Establece la URI (o URL) que apunta al fichero que contiene los datos de entrada para operar
	 * (cifrar, firmar, leer...)
	 * Se soporta el protocolo <code>file://</code> para ficheros en almacenamiento local.
	 * <br/>Los datos introducidos mediante esta funci&oacuten;n sobreescribir&aacute;n cualquier
	 * otro establecido previamente mediante los m&eacute;todos setData o setHash.
	 * @param fileuri URI hacia el fichero de datos.
	 * @see #setData(String)
	 * @see #setHash(String)
	 * @see #setFileuriBase64(String)
	 */
	public void setFileuri(String fileuri);

	/**
	 * Establece los datos contenidos en el fichero indicado (en donde se encontrar&aacute;n
	 * codificados en base 64), como los datos de entrada para las operaciones criptográficas
	 * y establece la ruta introducida como ruta de entrada.<br/>
	 * El contenido del fichero se interpretar&aacute; siempre como datos en base 64 no
	 * realiz&aacute;ndose la comprobaci&oacute;n de los mismos.
	 * @param fileuri URI hacia el fichero de datos.
	 * @see #setFileuri(String)
	 */
	public void setFileuriBase64(String fileuri);

	/**
	 * Establece el hash de los datos a firmar para los procesos de firma.
	 * <br/>Estos entrada tendr&aacute; m&aacute;xima prioridad cuando se realice
	 * una firma expl&iacute;cita.
	 * @param hash Hash en base 64 que se desea firmar.
	 * @see #setData(String)
	 * @see #setFileuri(String)
	 */
	public void setHash(final String hash);

	/**
	 * Ruta al fichero en el que se ha guardado la firma. Esta direcci&oacute;n puede hacerse
	 * establecido program&aacute;ticamente mediante {@link #setOutFilePath(String) setOutFilePath}
	 * o mediante la interfaz que aparece cuando no hay un valor asignado. Una vez se establece el
	 * valor del fichero de salida este permanece a&uacute;n cuando no se completa la operaci&acute;n
	 * de firma. Si no hay un fichero de salida establecido se devuelve cadena vac&iacute;a.
	 * @return Ruta al fichero en que se ha guardado la firma.
	 */
	public String getFilePath();

	/**
	 * Devuelve la ruta hacia el fichero de entrada. Esta direcci&oacute;n puede haberse
	 * establecido program&aacute;ticamente mediante {@link #setFileuri(String) setFileuri}
	 * o mediante la interfaz que aparece cuando no hay un valor asignado. Una vez se establece el
	 * valor del fichero de datos este permanece a&uacute;n cuando no se completa la operaci&acute;n
	 * de firma. Si no hay un fichero de datos establecido se devuelve cadena vac&iacute;a.<br/>
	 * La cadena devuelta es la transcripci&oacute;n de una URI, no de un path de fichero. Es decir,
	 * en caso de haberse seleccionado un fichero de una unidad local de disco, se devolver&aacute:<br/>
	 * <code>file://carpeta/fichero.ext</code></br>
	 * Esta cadena se devolver&aacute; respetando el <i>URL Encoding</i>, es decir, se realizar&aacute;n
	 * cambios tales como sustituir los espacios (' ') por "%20". Esta codificación impide obtener de 
	 * forma directa un fichero mediante la clase {@link java.io.File}, previamente deberemos
	 * obtener una URI. 
	 * @return Fichero que seleccion&oacute; el usuario para firmar
	 */
	public String getFileUsedPath();

	/**
	 * Establece una firma electr&oacute;nica en base 64 como entrada para el cliente de firma.
	 * Este metodo se utiliza principalmente para indicar la firma electr&oacute;nica durante las
	 * operaciones de cofirma y contrafirma (en cada una de sus variedades). 
	 * @param inElectronicSignature Firma electr&oacute;nica en base 64.
	 */
	public void setElectronicSignature(String inElectronicSignature);

	/**
	 * Establece la ruta de un fichero de firma electr&oacute;nica como entrada para el cliente de firma.
	 * Este m&eacute;todo se utiliza principalmente para indicar la firma electr&oacute;nica durante
	 * las operaciones de cofirma y contrafirma (en cada una de sus variedades).
	 * @param inElectronicSignatureFile Ruta del fichero de firma.
	 */
	public void setElectronicSignatureFile(String inElectronicSignatureFile);

	/**
	 * Establece los nodos que deben contrafirmarse durante las operaciones de contrafirma de
	 * firmantes y de nodos. Cuando se va a realizar la contrafirma de firmantes concretos se
	 * indicar&aacute;n los nombres de los firmantes tal c&oacute;mo los devuelve
	 * {@link #getSignersStructure()}. En el caso de la contrafirma de nodos los indices de
	 * los nodos (en donde el primer nodo es el 0). Los distintos elementos se indican
	 * consecutivos en forma de cadena y separados por '\n' o '\r\n'. Los nodos s&oacute;lo se
	 * contrafirmar&aacute;n una vez por cada operaci&oacute;n, as&iacute; repetir firmantes
	 * o nodos no tendr&aacute; efecto sobre la operaci&oacute;n. 
	 * 
	 * @param signers Cadena de nombres de firmantes o &iacute;ndices de los nodos.
	 */
	public void setSignersToCounterSign(String signers);

	
	/**
	 * Guarda los datos provenientes de la funci&oacute;n realizada anteriormente en un fichero especificado.
	 * @param fileUri Ruta del fichero destino.
	 * @return <code>true</code> si los datos se han guardado correctamente, <code>false</code> en caso contrario.
	 */
	public boolean saveDataToFile(String fileUri);

	/**
	 * Guarda los datos provenientes de la funci&oacute;n realizada anteriormente en un fichero.
	 * @return <code>true</code> si los datos se han guardado correctamente, <code>false</code> en caso contrario.
	 */
	public boolean saveDataToFile();

	/**
	 * Define el tipo de datos mime de los datos tras introducirlos. 
	 * Si se especifica un archivo, el mimetype se intentar&aacute; obtener autom&aacute;ticamente
	 * a partir de la extensi&oacute;n del archivo.
	 * En caso de que no se especifiquen y no se puedan obtener autom&aacute;ticamente
	 * se tomar&aacute; como mimetype por defecto application/octet-string 
	 * @param mimetype Tipo de datos para los datos actuales.
	 */
	public void setDataMimeType(String mimetype);

	/**
	 * Muestra y firma un HTML (firma web).
	 * @param html HTML a mostrar y firmar
	 * @return Ruta al fichero que contiene la firma
	 */
	public String webSign(final String html);
	
	
	//*******************************************************************/
	//***************** FUNCIONALIDADES DE CIFRADO **********************/
	//*******************************************************************/

	/**
	 * Cifra los datos indicados mediante el m&eacute;todo setPlainData(String). Si no se le indica
	 * algoritmo de cifrado, modo de bloque y el padding, se usar&aacute; la configuracion por defecto.
	 * Puede establecerse el modo de clave con {@link #setKeyMode(String)} para seg&uacute;n deseemos
	 * clave autogenerada, clave manual o contrase&ntilde;a. Los datos se pueden recuperar con
	 * {@link #getCipherData()} y almacenar en disco con {@link #saveCipherDataToFile(String)}.
	 * @return true si la operaci&oacute;n se realiz&oacute; con &eacute;xito, false en caso contrario.
	 */
	public boolean cipherData();

	/** Cifra el fichero especificado. El resultado de esta operaci&oacute;n es el mismo que
	 * el de ejecutar el m&eacute;todo {@link #cipherData()} tras haber indicado un fichero
	 * con {@link #setFileuri(String)}.
	 * @param fileUri Fichero de datos a cifrar
	 * @return true si la operaci&oacute;n se efectu&oacute; con &eacute;xito
	 */
	public boolean cipherFile(String fileUri);

	/**
	 * Realiza una operaci&oacute;n de descifrado de datos. Los datos a descifrar pueden establecerse
	 * en base 64 mediante el m&eacute;todo {@link #setCipherData(String)} o, de tratarse de un fichero,
	 * a trav&eacute;s de {@link #setFileuri(String)}. La configuraci&oacute;n
	 * de cifrado que se utiliz&oacute; para los datos debe indicarse por medio del m&eacute;todo
	 * {@link #setCipherAlgorithm(String)} que puede contener el algoritmo de cifrado &uacute;nicamente
	 * o la configuraci&oacute;n completa de algoritmo de cifrado, modo de bloque y padding, con los datos
	 * separados por una barra ('/'). Por ejemplo:
	 * <ul>
	 * <li>AES</li>
	 * <li>AES/ECB/PKCS5PADDING</li>
	 * </ul>
	 * Seg&uacute;n el algoritmo de cifrado deberemos indicar mediante el m&eacute;todo {@link #setKeyMode(String)}
	 * y es necesaria una clave de cifrado, introducida en base 64 mediante {@link #setKey(String)}, o una
	 * contrase&ntilde;a que el propio usuario deber&aacute; establecer a trav&eacute;s de un di&aacute;logo
	 * modal que se le presentar&aacute;.</br>
	 * El resultado de la operaci&oacute;n puede recuperarse con {@link #getPlainData()} o salvarse directamente
	 * a disco con {@link #savePlainDataToFile(String)}.
	 * @return true si la operaci&oacute;n finaliz&oacute; con &eacute;xito, false en caso contrario.
	 */
	public boolean decipherData();

	/** 
	 * Desencripta el fichero indicado. La configuraci&oacute;n de cifrado que se utiliz&oacute; para
	 * los datos debe indicarse por medio del m&eacute;todo {@link #setCipherAlgorithm(String)} que
	 * puede contener el algoritmo de cifrado &uacute;nicamente o la configuraci&oacute;n completa de
	 * algoritmo de cifrado, modo de bloque y padding, con los datos separados por una barra ('/').
	 * Por ejemplo:
	 * <ul>
	 * <li>AES</li>
	 * <li>AES/ECB/PKCS5PADDING</li>
	 * </ul>
	 * Seg&uacute;n el algoritmo de cifrado deberemos indicar mediante el m&eacute;todo {@link #setKeyMode(String)}
	 * y es necesaria una clave de cifrado, introducida en base 64 mediante {@link #setKey(String)}, o una
	 * contrase&ntilde;a que el propio usuario deber&aacute; establecer a trav&eacute;s de un di&aacute;logo
	 * modal que se le presentar&aacute;.</br>
	 * El resultado de la operaci&oacute;n puede recuperarse con {@link #getPlainData()} o salvarse directamente
	 * a disco con {@link #savePlainDataToFile(String)}.
	 * @param fileUri Archivo de datos a descifrar
	 * @return true si la operaci&oacute;n finaliz&oacute; con &eacute;xito, false en caso contrario.
	 */
	public boolean decipherFile(String fileUri);

	/** 
	 * Devuelve la clave de cifrado actualmente en uso en Base64. 
	 * Si se acaba de realizar alguna acci&oacute;n se devolver&aacute; la clave utilizada para 
	 * esta accion. En caso contrario, se devolver&aacute; la clave de entrada
	 * (para el caso de clave manual) o null.  
	 * @return Clave en Base64.
	 */
	public String getKey();

	/**
	 * Define una nueva clave sim&eacute;trica para para el cifrado cuando el modo 
	 * de cifrado es mediante clave introducida por el usuario.
	 * Debe ser especificada en base64, igual que ser&aacute; obtenida mediante {@link #getKey()}.  
	 * @param newKey Clave en base64.
	 * @see #setKeyMode(String)
	 */
	public void setKey(String newKey);

	/**
	 * Establece la cadena de texto usada como contrase&ntilde;a para el cifrado de datos.
	 * Este m&eacute;todo debe usarse estableciendo el modo de clave para el uso de contrase&ntilde;as
	 * y acompa&ntilde;ado de un algoritmo de cifrado preparado para su uso con contrase&ntilde;as.
	 * La contrase&ntilde;a debe ser una cadena ASCII v&aacute;lida.
	 * @param password Cadena de texto usada como contrase&ntilde;a.
	 * @return Devuelve <code>true</code> si la contrase&ntilde;a es v&aacute;lida, <code>false</code>
	 * en caso contrario.
	 * @see #setKeyMode(String)
	 */
	public boolean setPassword(String password);

	/**
	 * Devuelve la cadena de texto usada como password en la &uacute;ltima operaci&oacute;n
	 * de cifrado.
	 * @return String Password o, si no se ha establecido una, nulo.
	 */
	public String getPassword();

	/** 
	 * Especifica el algoritmo a utilizar para la encriptaci&oacute;n/desencriptaci&oacute;n
	 * sim&eacute;trica.<br/> 
	 * Se utiliza tanto en la encriptaci&oacute;n simple como en la generaci&oacute;n de 
	 * sobres digitales.<br/>
	 * En caso de requerir el algoritmo algun tipo de configuraci&oacute;n de modo de clave y
	 * padding se podr&aacute; pasar junto a su nombre separados por '/' con el formato:
	 * <pre>      <code>Algoritmo/Modo/Padding</code></pre>
	 * Por ejemplo:
	 * <pre>      <code>AES/ECB/PKCS5PADDING</code></pre>
	 * Los algoritmos de cifrados aceptados se especifican en {@link es.gob.afirma.misc.AOConstants}.
	 * @param algorithm Algoritmo de encriptaci&oacute;n sim&eacute;trica
	 * @see #cipherData()
	 * @see #cipherFile(String)
	 * @see #buildCMSStructure()
	 */
	public void setCipherAlgorithm(String algorithm);

	/** 
	 * Devuelve el algoritmo sim&eacute;trico actualmente en uso.<br>
	 * Los algoritmos de cifrados aceptados se especifican en
	 * {@link es.gob.afirma.misc.AOConstants}.
	 * @return Algoritmo de cifrado.
	 */
	public String getCipherAlgorithm();

	/**
	 * Devuelve el modo de firma electr&oacute;nica empleado en la &uacute;ltima firma.
	 * @return Modo de firma.
	 */
	public String getSignatureMode();


	/**
	 * Establece el modo de firma, que define si los datos se incrustar&aacute;n o no en una firma
	 * generada. Puede ser "Explicit" o "Implicit" (o "Explicita" e "Implicita", respectivamente).
	 * Las opciones son insensibles a may&uacute;sculas y min&uacute;sculas.
	 * <br/>Un formato de firma puede definir modos de firma propios.
	 * @param mode Modo de firma.
	 */
	public void setSignatureMode(String mode);


	/**
	 * Devuelve el modo actual de generaci&oacute;n de clave.
	 * @return Modo de generaci&oacute;n.
	 * @see #setKeyMode(String)
	 */
	public String getKeyMode();

	/**
	 * Define el modo de obtenci&oacute;n de clave para cifrado / descifrado sim&eacute;trico.
	 * Los modos de obtenci&oacute;n de clave se especifican en {@link es.gob.afirma.misc.AOConstants}.
	 * @param keyMode Modo de obtenci&oacute;n de clave.
	 */
	public void setKeyMode(String keyMode);

	/**
	 * Graba los datos cifrados en un fichero.
	 * @param fileUri Direcci&oacute;n del fichero en donde se desean guardar los datos.
	 * @return Devuelve <code>true</code> si se ha realizado con &eacute;xito.
	 */
	public boolean saveCipherDataToFile(String fileUri);

	/**
	 * Guarda los datos descrifrados en una anterior operaci&oacute;n de desencriptado
	 * o el texto plano introducido anteriormente, seg&uacute;n sea lo &uacute;ltimo que ocurra,
	 * en un fichero especificado.
	 * @param fileUri Ruta del fichero donde guardar la informaci&oacute;n en plano.
	 * @return true si los datos se guardaron correctamente.
	 */
	public boolean savePlainDataToFile(String fileUri);

	// ***************** USO DEL ALMACEN DE CLAVES DE CIFRADO *****************

	/**
	 * Establece si debe permitirse al usuario almacenar sus claves de cifrado en su
	 * almac&eacute;n asociado.
	 * @param useKeyStore Indica si se debe usar el almacen de claves.
	 */
	public void setUseCipherKeyStore(boolean useKeyStore);
	
	//*******************************************************************/
	//************** FUNCIONALIDADES DE SOBRE DIGITAL  ******************/
	//*******************************************************************/

	/**
	 * Define la ubicaci&oacute;n de los certificados que se utilizar&aacute;n como 
	 * receptores de un sobre digital.   
	 * @param s Cadena de rutas a los ficheros, separados por 'retornos de l&iacute;nea' ('\n'). 
	 */
	public void setRecipientsToCMS(String s);

	/**
	 * Agrega un nuevo destinatario, por medio de su certificado, a la lista de destinatarios
	 * a los que se enviar&aacute; un sobre digital. Este destinario se agrega a los que
	 * se hayan especificado por medio del m&eacute;todo {@link #setRecipientsToCMS(String)}
	 * de tal forma que usar este m&eacute;todo no afectar&aacute; a los certificados
	 * introducidos mediante aquella funci&oacute;n y viceversa. Para eliminar un certificado
	 * de la lista agregado mediante este m&eacute;todo deber&aacute; usarse
	 * {@link #removeRecipientToCMS(String)}.
	 * @param certB64 Certificado en base 64.
	 */
	public void addRecipientToCMS(String certB64);

	/**
	 * Eliminar un destinatario de la lista de destinatarios a los que se enviar&aacute;
	 * un sobre digital siempre y cuando este se haya especificado mediante el m&eacute;todo
	 * {@link #addRecipientToCMS(String)}.
	 * @param certB64 Certificado en base 64 del destinatario que deseamos eliminar.
	 */
	public void removeRecipientToCMS(String certB64);

	/** Crea un objeto CMS encriptado (sin informaci&oacute;n de clave, emisor o receptor)
	 * con los datos establecidos mediante {@link #setData(String)} o {@link #setFileuri(String)}.
	 * La configuraci&oacute;n del cifrado se establece con {@link #setKeyMode(String)} para 
	 * seleccionar el tipo de cifrado (por clave o por password) y, en caso de establecerse el uso
	 * de claves (valor por defecto) ser&aacute; obligatorio establecer una clave mediante
	 * {@link #setKey(String)}. En cambio, la contrase&ntilda;a se pedir&aacute; siempre mediante una
	 * interfaz propia del cliente, sin posibilidad de introducirla program&aacute;ticamente.
	 * <br/>El algoritmo de cifrado se establecer&aacute; con {@link #setCipherAlgorithm(String)},
	 * el modo de bloque y el relleno siempre ser&aacute;n "CBC" y "PKCS#5 Padding" respectivamente.
	 * <br/>El resultado se puede recuperar mediante {@link #getData()} en formato binario o mediante
	 * {@link #getB64Data()} en base 64 y puede ser almacenado en un fichero mediante 
	 * {@link #saveCipherDataToFile(String)}.
	 * 
	 * @return <code>true</code> si la operaci&oacute;n se ha realizado con &eacute;xito.
	 * @see #saveDataToFile()
	 */
	public boolean buildCMSEncrypted();
	
	/**
	 * Crea un objeto CMS AuthenticatedData con los datos establecidos mediante {@link #setData(String)} 
	 * o {@link #setFileuri(String)}.
	 * @return <code>true</code> si la operaci&oacute;n se ha realizado con &eacute;xito, <code>false</code>
	 *         en caso contrario.
	 * @see #saveDataToFile()         
	 */
	public boolean buildCMSAuthenticated();

	/**
	 * Crea un objeto CMS envuelto utilizando los datos introducidos con {@link #setData(String)}
	 * o {@link #setFileuri(String)} y los certificados indicados mediante {@link #setRecipientsToCMS(String)} 
	 * como receptores. Es obligatorio introducir al menos un receptor. Los datos del emisor se introducir&aacute;
	 * en el envoltorio si se ha establecido el certificado del emisor mediante
	 * {@link #setSelectedCertificateAlias(String)}, en caso contrario no se incluir&aacute;.
	 * <br/>El resultado se podr&aacute; recuperar en base64 mediante {@link #getB64Data()} y 
	 * puede ser almacenado en un fichero mediante {@link #saveDataToFile()}.
	 * 
	 * @return Devuelve <code>true</code> si la operaci&oacute;n se ha realizado con &eacute;xito.
	 * @deprecated Sustituir por:
	 * {@code
	 * 		setCMSContentType(String)
	 * } por la configuraci&oacute;n del tipo de estructura con
	 * {@link #setCMSContentType(String)}} y el par&aacute;metro AOConstants.BINARY_ENVELOP_ENVELOPEDDATA,
	 * y la generaci&oacute;n de la estrutura. Sustuido por llamada {@link #buildCMSStructure()}   
	 */
	public boolean buildCMSEnveloped();
	
	/**
	 * Crea la estructura CMS del tipo indicado con {@link #setCMSContentType(String)}, por
	 * defecto la estructura definida en {@code AOConstants.DEFAULT_BINARY_ENVELOP}.<br/>
	 * La estructura de datos se generar&aacute; a partir de los datons indicados mediante
	 * {@link #setData(String)} o {@link #setFileuri(String)}.
	 * Adicionalmente, cada estructura requiere la configuraci&oacute;n de los siguientes
	 * par&aacute;metros:<br/>
	 * <ul>
	 * <li><b>EncryptedData:</b> Estructura de datos cifrados simetricamente. Requiere establecer
	 * una configuraci&oacute;n de cifrado.</li>
	 * <li><b>EnvelopedData:</b> Estructura de datos envuelta (Sobre digital). Requiere que se
	 * le indiquen los destinatarios del mensaje y, opcionalmente, el remitente.</li>
	 * <li><b>SignedAndEnvelopedData:</b> Estructura de datos envuelta y firmada (Sobre digital firmado).
	 * Requiere que se le indiquen los destinatarios del mensaje y el remitente.</li>
	 * <li><b>AuthEnvelopedData:</b> Estructura de datos autenticada y envuelta (Sobre digital
	 * autenticado). Requiere que se le indiquen los destinatarios del mensaje y el remitente.</li>
	 * </ul>
	 * Los destinatarios se deber&aacute;n indicar mediante {@link #setRecipientsToCMS(String)} y/o
	 * {@link #addRecipientToCMS(String)} y {@link #removeRecipientToCMS(String)}. El emisor puede
	 * indicarse mediante {@link #setSelectedCertificateAlias(String)}. De no hacerlo y ser obligatorio,
	 * se presentar&aacute; un di&aacute;logo al usuario para su selecci&oacute;n.
	 * <br/>El resultado se podr&aacute; recuperar en base64 mediante {@link #getB64Data()} y 
	 * puede ser almacenado en un fichero mediante {@link #saveDataToFile()}.
	 * 
	 * @return Devuelve <code>true</code> si la operaci&oacute;n finaliz&oacute; con &eacute;xito.
	 */
	public boolean buildCMSStructure();
	
	/**
	 * Establece un tipo de contenido para la generaci&oacute;n de una estructura CMS.
	 * Los tipos de contenido soportados son:
	 * <ul>
	 * <li><b>EncryptedData:</b> Estructura de datos cifrados sim&eacute;tricamente.</li>
	 * <li><b>EnvelopedData:</b> Estructura de datos envuelta (Sobre digital).</li>
	 * <li><b>SignedAndEnvelopedData:</b> Estructura de datos envuelta y firmada (Sobre digital
	 * firmado).</li>
	 * <li><b>AuthEnvelopedData:</b> Estructura de datos autenticada y envuelta (Sobre digital
	 * autenticado)</li>
	 * </ul>
	 * @param contentType Tipo de contenido.
	 * @see #buildCMSStructure()
	 */
	public void setCMSContentType(String contentType);
	
	/**
	 * Devuelve los datos generados en la &uacute;ltima operaci&oacute;n de envoltorio CMS codificados
	 * en Base 64. En caso de no haberse realizado ninguna operaci&oacute;n de este tipo se devuelve
	 * cadena vac&iacute;a. 
	 * @return String en Base 64.
	 * @see #buildCMSEncrypted
	 * @see #buildCMSEnveloped()
	 * @see #signAndPackData()
	 * @see #signAndPackFile(String)
	 */
	public String getB64Data();

	/**
	 * Recupera el contenido de un sobre digital. El sobre se indicar&aacute; mediante una cadena
	 * en Base 64 con <code>setData</code> o mediante un fichero con <code>setFileuri</code>.
	 * <br/>En el caso de los sobre <code>EncryptedData</code>, puede indicarse si se cifr&oacute;
	 * con contrase&ntilde; o clave mediante el m&eacute;todo {@link #setKeyMode(String)}, y esta
	 * con {@link #setPassword(String)} o {@link #setKey(String)}, respectivamente.
	 * <br/>En el caso de los sobres <code>EnvelopedData</code> y <code>SignedAndEnvelopedData</code>
	 * se puede indicar el certificado para descifrar mediante el m&eacute;todo
	 * {@link #setSelectedCertificateAlias(String)}.
	 * <br/>El resultado se podr&aacute; recuperar en base64 mediante {@link #getB64Data()} y 
	 * puede ser almacenado en un fichero mediante {@link #saveDataToFile()}.
	 * @return true si la operaci&oacute;n fue correcta. 
	 */
	public boolean recoverCMS();

	/**
	 * Formatea el objeto DER en Base64 especificado en la llamada y devuelve una
	 * cadena con los objetos CMS contenidos. Si el datos no est&aacute; codificado
	 * en base 64 o no es un dato CMS se devuelve cadena vac&iacute;a.
	 * @param b64 Objeto DER en Base64.
	 * @return Cadena correspondiente al objeto CMS.
	 */
	public String formatEnvelopedCMS(String b64);

	/**
	 * Formatea el objeto DER en Base64 especificado en la llamada y devuelve una
	 * cadena con los objetos CMS contenidos. Si el datos no est&aacute; codificado
	 * en base 64 o no es un dato CMS se devuelve cadena vac&iacute;a.
	 * @param b64 Objeto DER en Base64.
	 * @return Cadena correspondiente al objeto CMS.
	 */
	public String formatEncryptedCMS(String b64);

	/**
	 * Firma y empaqueta en un sobre digital los datos especificados por {@link #setData(String)}
	 * El resultado puede ser recuperado mediante {@link #getData()}
	 * @return true si la operacion se ha efectuado correctamente
	 */
	public boolean signAndPackData();

	/**
	 * Firma y empaqueta en un sobre digital el fichero especificado por uri. 
	 * El resultado puede ser recuperado mediante {@link #getData()}
	 * @param uri Localizaci&oacute;n en formato URI de los datos a firmar y empaquetar
	 * @return true si la operacion se ha efectuado correctamente
	 */
	public boolean signAndPackFile(String uri);

	// ************* SELECCION DE DESTINATARIO DESDE LDAP *******************/
	
	/**
	 * Establece la configuraci&oacute;n necesaria para que el cliente conecte con un servidor
	 * LDAP. Si no se indica el puerto o se indica un par&aacute;metro err&oacute;neo, se
	 * usar&aacute; el puerto por defecto de LDAP. La direcci&oacute;n del LDAP es obligatoria.
	 * @param address Direcci&oacute;n URL del LDAP (Actualmente, sin uso).
	 * @param port Puerto a trav&eacute;s del que se realiza la conexi&oacute;n.
	 * @param root Direcci&oacute;n ra&iacute;z del LDAP.
	 */
	public void setLdapConfiguration(String address, String port, String root);

	/**
	 * Establece el nombre "principal" del certificado que se desea recuperar.
	 * @param ldapCertificatePrincipal Nombre "principal".
	 */
	public void setLdapCertificatePrincipal(String ldapCertificatePrincipal);

	/**
	 * Obtiene un certificado en base 64 de un servidor LDAP.<br/>
	 * Es necesario haber establecido previamente la configuraci&oacute;n del LDAP
	 * y los requisitos para identificar al certificado en cuesti&oacute;n. 
	 * @return Certificado descargado en base 64.
	 * @see #setLdapConfiguration(String, String, String)
	 * @see #setLdapCertificatePrincipal(String)
	 */
	public String getLdapCertificate();
	
	//*******************************************************************/
	//************ FUNCIONALIDADES DE FIRMA MASIVA ORIGINAL *************/
	//*******************************************************************/

	/**
	 * A&ntilde;ade un hash en base 64 para la firma masiva.
	 * @param aHash Hash que se desea firmar.
	 */
	public void addMassiveHash(String aHash);

	//*******************************************************************/
	//************** FUNCIONALIDADES DE MULTIFIRMA MASIVA ***************/
	//*******************************************************************/
	
	/**
	 * Firma todos los archivos de un directorio seg&uacute;n la configuracion establecida. La
	 * firma siempre se realizar&aacute; en modo expl&iacute;cito salvo que el formato de firma
	 * concreto exija lo contrario. El directorio de firma se puede establecer con
	 * {@link #setInputDirectoryToSign(String)}, si no se hace, se mostrar&aacute; un di&aacute;logo
	 * para la selecci&oacute;n del directorio. Para indicar que se desea firmar tambi&eacute;n los
	 * ficheros de los subdirectorios del directorio establecido, debe usarse
	 * {@link #setInRecursiveDirectorySign(boolean)}. Es posible limitar los ficheros que deben firmarse
	 * mediante un filtro establecido con {@link #setInIncludeExtensions(String)}.
	 * <br/>La operaci&oacute;n masiva a realizar ser&aacute; por defecto la de firma, aunque puede
	 * modificarse para realizar las operaciones de cofirma o contrafirma (de todo el arbol de firma o
	 * s&oacute;lo los nodos hoja). El tipo de operaci&oacute;n se indica mediante
	 * {@link #setMassiveOperation(String)}. 
	 * <br/>Si no se indica un directorio de salida con {@link #setOutputDirectoryToSign(String)} el
	 * resultado se almacenar&aacute; internamente y se podr&aacute; recuperar mediante
	 * {@link #getSignaturesBase64Encoded()} (opci&oacute;n no recomendable). Si se indica el
	 * directorio de salida y este no existe, se crear&aacute;.
	 * </br>Si se produjo alg&uacute;n error durante la firma de un fichero, el proceso continuar&aacute;,
	 * se crear&aacute; un registro en el log de error en el directorio de salida de firmas y se
	 * devolver&aacute; <code>false</code> para que se realicen las operaciones adecuadas. 
	 * @return Devuelve <code>true</code> si la firma ha sido correcta para todos los ficheros afectados.
	 */
	public boolean signDirectory();

	/**
	 * Establece la operaci&oacute;n masiva a realizar en el proceso generado por el m&eacute;todo
	 * {@link #signDirectory()}. 
	 * @param massiveOperation Tipo de operaci&oacute;n masiva a realizar.
	 */
	public void setMassiveOperation(String massiveOperation);

	/**
	 * Indica si se debe respetar el formato de firma original para las operaciones de multifirma
	 * masiva o, si en cambio, se usar&aacute; la configuraci&oacute;n de firma establecida para
	 * todas las firmas. Por defecto, se respeta el formato original.
	 * @param originalFormat Indica si respetar el formato original.
	 */
	public void setOriginalFormat(boolean originalFormat);

	/**
	 * Devuelve la ruta absoluta del directorio donde se ubican los ficheros
	 * a ser firmados de forma masiva.
	 * @return Ruta absoluta del directorio.
	 */
	public String getInputDirectoryToSign();

	/**
	 * Selecciona el directorio de donde se tomar&aacute;an los ficheros de firma y datos para la
	 * operaci&oacute;n de firma masiva.
	 * 
	 * @param directory Ruta absoluta del directorio.
	 */
	public void setInputDirectoryToSign(String directory);

	/**
	 * Devuelve la ruta absoluta del directorio donde se almacenar&aacute;n las firmas resultado de
	 * la operaci&oacute;n de firma masiva.
	 * @return Ruta absoluta del directorio de salida de la firma masiva.
	 */
	public String getOutputDirectoryToSign();

	/**
	 * Selecciona el directorio donde se depositar&aacute;n las firmas masivas de los archivos
	 * situados en <code>InputDirectoryToSign</code>. Crear&aacute; el directorio en caso de que
	 * no exista.
	 * 
	 * @param directory Ruta absoluta del directorio.
	 */
	public void setOutputDirectoryToSign(String directory);

	/**
	 * Define las extensiones que se incluir&aacute;n en la firma de directorios.
	 * La cadena que se debe introducir se corresponde con las extensiones
	 * separadas por ','. Para eliminar el filtrado por exctensi&ioacute;n,
	 * introducir <code>null</code>.
	 * Por ejemplo: <code>TXT,JPG,BMP,DAT</code>
	 * @param extensions Extensiones de los ficheros que se desean firmar.
	 */
	public void setInIncludeExtensions(String extensions);

	/**
	 * Establece si la firma de directorios se efectuar&aacute; de forma recursiva o no.
	 * @param recursiveSignDir <code>true</code> para firmar un directorio y sus
	 * subdirectorios. <code>false</code> en caso contrario.
	 */
	public void setInRecursiveDirectorySign(boolean recursiveSignDir);
	
	/**
	 * Toma la configuraci&oacute;n de firma del cliente y prepara el proceso
	 * de firma masiva. Los datos que toma de la configuracion del cliente son:
	 * <ul>
	 * <li>Certificado de firma.</li>
	 * <li>Clave de firma.</li>
	 * <li>Algoritmo.</li>
	 * <li>Modo.</li>
	 * <li>Formato por defecto.</li>
	 * <li>Respetar formato original de firma.</li>
	 * <li>Operaci&oacute;n masiva a realizar (firma, cofirma, contrafirma de hojas o de &aacute;rbol).</li>
	 * </ul>
	 * En el caso del certificado de firma, se preguntar&aacute; cuando este no se especifique.<br/>
	 * Este m&eacute;todo tambi&eacute; reinicia el log del procedimiento de firma masiva.
	 * Una vez preparado el procedimiento de firma masiva, cualquier cambio en la
	 * configuraci&oacute;n del cliente, salvo la operaci&oacute;n a realizar, no lo afectar&aacute;.
	 * As&iacute;, por ejemplo, se podr&iacute;a especificar otro certificado de firma, pero todas 
	 * las operaciones dentro del procedimiento de firma masiva utilizaran el certificado indicado 
	 * previamente o durante el proceso de inicializaci&oacute;n.<br/>
	 * Una vez finalizado el proceso de firma masiva se recomienda utilizar el m&eacute;todo
	 * {@link #endMassiveSignature()} para eliminar la configuraci&oacute;n del procedimiento.
	 * @return Devuelve <code>true</code> si la operaci&oacute;n masiva se inicializ&oacute; correctamente,
	 * <code>false</code> en caso contrario.
	 * @see #setSignatureAlgorithm(String)
	 * @see #setSignatureFormat(String)
	 * @see #setSignatureMode(String)
	 * @see #setSelectedCertificateAlias(String)
	 * @see #setOriginalFormat(boolean)
	 * @see #setMassiveOperation(String)
	 */
	public boolean initMassiveSignature();

	/**
	 * Libera la configuraci&oacute;n del procedimiento de firma masiva previamente inicializado.
	 */
	public void endMassiveSignature();

	/**
	 * Firma datos con la configuraci&oacute;n de firma masiva establecida.
	 * En el caso de la operacion de firma masiva tendremos que indicar los
	 * datos que deseamos firmar, mientras que para la operaci&oacte;n de
	 * contrafirma masiva se indicar&aacute; la firma que se debe contrafirmar. 
	 * @param b64Data Datos en base 64 a firmar.
	 * @return Firma resultado en base 64 o <code>null</code> en caso de error. 
	 */
	public String massiveSignatureData(String b64Data);

	/**
	 * Firma un hash con la configuraci&oacute;n de firma masiva establecida.
	 * Esta m&eacute;todo s&oacute;lo es v&aacute;lido para la operaci&oacute;n de
	 * firma masiva (ni cofirmas, ni contrafirmas). 
	 * @param b64Hash Hash en base 64 a firmar.
	 * @return Firma resultado en base 64 o <code>null</code> en caso de error.
	 */
	public String massiveSignatureHash(String b64Hash);

	/**
	 * Firma un fichero con la configuraci&oacute;n de firma masiva establecida.
	 * En el caso de la operacion de firma masiva tendremos que indicar el fichero
	 * con los datos que deseamos firmar, mientras que para la operaci&oacte;n de
	 * contrafirma masiva se indicar&aacute; el fichero con la firma que se desea
	 * contrafirmar.
	 * @param fileuri Ruta del fichero a firmar.
	 * @return Firma resultado en base 64 o <code>null</code> en caso de error. 
	 */
	public String massiveSignatureFile(String fileuri);

	/**
	 * Devuelve la traza de log de la &uacte;ltima operaci&oacute;n del procedimiento de firma masiva. 
	 * @return Log de la &uacute;ltima operaci&oacute;n.
	 */
	public String getMassiveSignatureCurrentLog();

	/**
	 * Devuelve el log completo del procedimiento de firma masiva. El log de cada operaci&oacute;n
	 * se muestra en &uacute;nica l&iacute;nea y se mantiene el orden. 
	 * @return Log completo del procedimiento de firma masiva.
	 */
	public String getMassiveSignatureLog();

	/**
	 * Almacena en un fichero todas las trazas de log generadas hasta el momento por el procedimiento
	 * de firma masiva. La ruta del fichero se puede establecer mediante {@link #setOutFilePath(String)}.
	 * En caso de no hacerlo aparecer&acute; una ventana para seleccionar donde guardar el fichero de log. 
	 */
	public void saveMassiveSignatureLog();
	
	//*******************************************************************/
	//************* FUNCIONALIDADES DE GESTION DE KEYSTORES *************/
	//*******************************************************************/

	/**
	 * Establece el repositorio activo. Ser&aacute; a este repositorio al que se acceder&aacute;
	 * para recuperar los certificados para la realizaci&oacute;n de las firmas y el resto
	 * de operaciones criptogr&aacute;fica.
	 * Los tipos aceptados son:
	 * <ul>
	 * <li><b>WINDOWS</b>: Repositorio de Microsoft Windows.</li>
	 * <li><b>WINADDRESSBOOK</b>: Repositorio "Otras Personas" de Windows.</li>
	 * <li><b>APPLE</b>: Repositorio de Apple Macintosh.</li>
	 * <li><b>MOZILLA</b>: Repositorio de Mozilla Firefox.</li>
	 * <li><b>P11</b>: Repositorio de tipo PKCS#11 controlador por una libreria de sistema.</li>
	 * <li><b>P12</b>: Repositorios en disco en formato P12 o PFX.</li>
	 * <li><b>JKS</b>: Repositorios en disco en formato JKS.</li>
	 * <li><b>SINGLE</b>: Certificado suelto en disco.</li>
	 * <li><b>JAVACE</b>: Repositorios en disco en formato Java Case Exact.</li>
	 * <li><b>WINDOWS-CA</b>: Repositorio de Autoridades Intermedias de Certificaci&oacute;n de Windows.</li>
	 * <li><b>WINDOWS-ROOT</b>: Repositorio de Certificados Raiz de Windows.</li>
	 * </ul>
	 * @param path Ruta al repositorio (o su controlador) si procede.
	 * @param password Contrase&ntilde;a para el acceso al repositorio si procede.
	 * @param type Tipo de repositorio.
	 */
	public void setKeyStore(String path, String password, String type);

	//*******************************************************************/
	//******************* FUNCIONALIDADES NUEVAS AO *********************/
	//*******************************************************************/

	/**
	 * Establece la pol&iacute;tica que debe aplicarse a las firmas que se realicen.
	 * Se usa para la generaci&oacute;n de firmas avanzadas EPES.
	 * @param identifier URL identificadora de la pol&iacute;tica de firma (normalmente una URL hacia
	 *                   el documento que describe la pol&iacute;tica).
	 * @param description Descripci&oacute;n de la pol&iacute;tica.
	 * @param qualifier Oid cualificador de la pol&iacute;tica de firma.
	 */
	public void setPolicy(String identifier, String description, String qualifier);
	
	/**
	 * Obtiene todos los alias de los certificados disponibles para firmar o cifrar
	 * y los devuelve como una &uacute;nica cadena en donde los alias vienen separados
	 * por {@link es.gob.afirma.cliente.SignApplet#STRING_SEPARATOR}.
	 * @return Todos los alias de los certificados disponibles.
	 */
	public String getCertificatesAlias();

	/**
	 * Obtiene todos los alias de los certificados disponibles para firmar o cifrar
	 * y los devuelve en un array.
	 * @return Alias de los certificados disponibles.
	 */
	public String[] getArrayCertificatesAlias();

	/**
	 * Obtiene una cadena con todos los certificados del almac&eacute;n actual en ASCII Base64
	 * separados por {@link es.gob.afirma.cliente.SignApplet#STRING_SEPARATOR}.
	 * @return Cadena con todos los certificados.
	 */
	public String getCertificates();

	/**
	 * Obtiene un array con todos los certificados del almac&eacute;n actual en ASCII Base64.
	 * @return Array con todos los certificados.
	 */
	public String[] getArrayCertificates();

	/**
	 * Recupera un certificado del repositorio activo en la forma:
	 * <p><code>
	 * Bag Attributes<br/>
	 * friendlyName: CommonName del certificado<br/>
	 * -----BEGIN CERTIFICATE-----<br/>
	 * Codificacion del certificado en base 64<br/>
	 * -----END CERTIFICATE-----<br/>
	 * </code></p>
	 * @param alias Alias del certificado que deseamos recuperar.
	 * @return Certificado recuperado o <code>null</code> en caso de error.
	 */
	public String getCertificate(String alias);

	/**
	 * Recupera la clave p&uacute;blica de un certificado del repositorio activo en la forma:
	 * <p><code>
	 * -----BEGIN RSA PUBLIC KEY-----<br/>
	 * Clave p&uacute;blica del certificado en base 64<br/>
	 * -----END RSA PUBLIC KEY-----<br/>
	 * </code></p>
	 * @param alias Alias del certificado del que queremos recuperar la clave p&uacute;blica.
	 * @return Clave p&uacute;blica del certificado o <code>null</code> en caso de error.
	 */
	public String getCertificatePublicKey(String alias);

	/**
	 * Establece el alias del certificado activo.
	 * Dede implementarse como m&eacute;todo sincronizado para evitar problemas con los interfaces gr&aacute;ficos de
	 * usuario 
	 * @param certAlias Alias del certificado activo, debe coincidir con un alias de un certificado presente
	 *                  en el <code>KeyStore</code> activo
	 */
	public void setSelectedCertificateAlias(String certAlias);

	/**
	 * Establece un filtro para la selecci&oacute;n de certificados, solo se mostrar&aacute;n en el
	 * di&aacute;logo de selecci&oacute;n los que cumplan las condiciones proporcionados. 
	 * <b>Importante:</b> El filtro no se aplica en la obtenci&oacute;n directa de los alias del almac&eacute;n
	 * (getCertificatesAlias, getCertificates,...).
	 * @param subjectFilter Filtro para el titular del certificado, seg&uacute;n formato definido en la normativa RFC 2554
	 * @param issuerFilter Filtro para el emisor del certificado, seg&uacute;n formato definido en la normativa RFC 2554
	 * @param onlySignatureCertificates Si se establece a <code>true</code> se muestran para selecci&oacute;n 
	 *                                  &uacute;nicamente los certificados con el bit <i>nonRepudiation</i> del
	 *                                  campo <i>KeyUsage</i> activado, si se establece a <code>false</code> se
	 *                                  muestran todos los certificados
	 * @see #setMandatoryCertificateConditionRFC2254(String, String, boolean)
	 */
	public void setCertFilterRFC2254(String subjectFilter, String issuerFilter, boolean onlySignatureCertificates);

	/**
	 * Establece una condici&oacute;n para seleccionar el certificado con el que se ha de firmar.
	 * No se permite elegir certificado al usuario pues se entiende que se le "obliga" a firmar con
	 * uno concreto. En el caso, de encontrar m&aacute;s de un certificado que cumpla la condici&oacute;n
	 * se considera que el usuario tiene m&aacute;s de un certificado de ese tipo y S&Iacute; se le
	 * permite elegir cual usar. Si ning&uacute;n certificado instalado en el navegador cumple la condici&oacute;n,
	 * se mostrar&aacute; un error.
	 * @param subjectFilter Filtro para el titular del certificado, seg&uacute;n formato definido en la normativa RFC 2554
	 * @param issuerFilter Filtro para el emisor del certificado, seg&uacute;n formato definido en la normativa RFC 2554
	 * @param onlySignatureCertificates Si se establece a <code>true</code> se muestran para selecci&oacute;n 
	 *                                  &uacute;nicamente los certificados con el bit <i>nonRepudiation</i> del
	 *                                  campo <i>KeyUsage</i> activado, si se establece a <code>false</code> se
	 *                                  muestran todos los certificados
	 * @see #setCertFilterRFC2254(String, String, boolean)
	 */
	public void setMandatoryCertificateConditionRFC2254(String subjectFilter, String issuerFilter, boolean onlySignatureCertificates);

	
	/**
	 * Muestra el di&aacute;logo de selecci&oacute;n de certificados del almac&eacute;n seleccionado,
	 * aplicando los filtros de certificado de ser necesario. Devuelve nulo si hay alg&uacute;n problema
	 * durante la selecci&oacute;n o se cancela el di&aacute;logo.<br/>
	 * El certificado seleccionado queda establecido como &uacute;ltimo certificado utilizado, con lo
	 * cual puede recuperarse mediante los m&eacute;todos {@link #getSignCertificate()} y 
	 * {@link #getSignCertificateBase64Encoded()}. 
	 * @return Alias real (con el que fue dado de alta men el almac&eacute;n de claves) del certificado
	 * seleccionado.
	 */
	public String showCertSelectionDialog();
	
	//*******************************************************************//
	//******************** METODOS DEPRECADOS ***************************//
	//*******************************************************************//

	/**
	 * Devuelve los datos generados en la &uacute;ltima operaci&oacute;n sin codificar.
	 * En caso de no haberse realizado ninguna operaci&oacute;n de este tipo se devuelve
	 * cadena vac&iacute;a. 
	 * @return String de los datos de salida
	 * @deprecated Operaci&oacute;n no recomendada ya que el resultado puede variar seg&uacute;n la
	 * codificaci&oacute;n del sistema.
	 */
	@Deprecated
	public String getData();

	/**
	 * Devuelve los datos generados el la &uacute;ltima operación CMS
	 * @deprecated en favor de getB64Data()
	 * @return String en Base 64
	 * @see #getB64Data
	 */
	@Deprecated
	public String getCMSData();
	
	/**
	 * Tras una firma masiva (firma a partir de m&uacute;ltiples hashes), devuelve las firmas en base64
	 * separadas unas de otras por '!' (cierre de admiraci&oacute;n). Si no hay firmas establecidas <br/>
	 * <b>Advertencia: </b> Una vez se haya recuperado la firma, es aconsejable liberar
	 * los recursos del applet mediante el m&eacute;todo <code>initialize</code>.
	 * @return Las firmas en base64 separadas unas de otras por el signo de cierre de admiraci&oacute;n (!)
	 * @deprecated Por el consumo excesivo de memoria. 
	 */
	@Deprecated
	public String getSignaturesBase64Encoded();
	
}

