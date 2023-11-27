/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades;

import java.util.HashMap;
import java.util.Map;

/** Clase con los par&aacute;metros extra que pueden configurarse para el formato de firma XAdES. */
public final class XAdESExtraParams {

	/** Si se indica <code>true</code>, <u>no</u> se firma una referencia al nodo <code>KeyInfo</code>, si no
	 * se establece valor o se establece a otro valor distinto de <code>true</code>, se firma el nodo incluyendo
	 * una referencia a &eacute;l. */
	public static final String KEEP_KEYINFO_UNSIGNED = "keepKeyInfoUnsigned"; //$NON-NLS-1$

    /** URI en la que se encuentra el documento a firmar, necesario en el caso
     * del formato <i>XAdES Externally Detached</i>
     * (no aplica a contrafirmas). */
    public static final String URI = "uri"; //$NON-NLS-1$

    /** Prefijo con el que se pueden marcar los nombres de las propiedades con las URI de los datos
     * externos que se desean firmar. Es necesario para las firmas de m&uacute;ltiples documemntos
     * mediante manifest. */
    public static final String URI_PREFIX = "uri"; //$NON-NLS-1$

    /** Prefijo con el que se pueden marcar los nombres de las propiedades con las huellas digitales
     * de los datos que se desean firmar. Es necesario para las firmas de m&uacute;ltiples
     * documemntos mediante manifest. */
    public static final String MD_PREFIX = "md"; //$NON-NLS-1$

    /** Indica si se debe evitar la inclusi&oacute;n de la transformaci&oacute;n
     * XPATH2 que normalmente se a&ntilde;ade para posibilitar las cofirmas y
     * que elimina todas las firmas del documento para dejar &uacute;nicamente
     * el contenido. Por defecto, se encuentra a <code>false</code>.
     * &Uacute;nicamente aplica a firmas <i>enveloped</i>. */
    public static final String AVOID_XPATH_EXTRA_TRANSFORMS_ON_ENVELOPED = "avoidXpathExtraTransformsOnEnveloped";//$NON-NLS-1$

    /** Indica, mediante un <code>true</code> o <code>false</code>, que debe
     * incluirse en la firma &uacute;nicamente el certificado utilizado para
     * firmar y no su cadena de certificaci&oacute;n completa. Por defecto, se
     * incluir&aacute; toda la cadena de certificaci&oacute;n. <br>
     * Propiedad compartida con CAdES y PAdES. */
    public static final String INCLUDE_ONLY_SIGNNING_CERTIFICATE = "includeOnlySignningCertificate";//$NON-NLS-1$

    /** Indica, mediante <code>true</code> o <code>false</code> (por defecto), si
     * debe usarse un <a
     * href="http://www.w3.org/TR/xmldsig-core1/#sec-o-Manifest">Manifest</a> de
     * XMLDSig con las referencias de firma en vez de firmar directamente estas
     * referencias.<br>
     * Esto permite que sea opcional la comprobaci&oacute;n del destino y
     * huellas digitales de las referencias. */
    public static final String USE_MANIFEST = "useManifest";//$NON-NLS-1$

    /** Indica, mediante una expresi&oacute;n XPath (v1), el nodo bajo el cual
     * debe insertarse el nodo de firma en el caso de una firma <i>Enveloped</i>.<br>
     * Si la expresi&oacute;n devuelve m&aacute;s de un nodo, se usa solo el
     * primero. Si la expresi&oacute;n no devuelve nodos o est&aacute; mal
     * construida se lanzar&aacute; una excepci&oacute;n.<br>
     * Este par&aacute;metro solo tiene efecto en firmas <i>Enveloped</i>. */
    public static final String INSERT_ENVELOPED_SIGNATURE_ON_NODE_BY_XPATH = "insertEnvelopedSignatureOnNodeByXPath";//$NON-NLS-1$

    /** Indica el nodo del XML que debe firmarse. Cuando no se indica este par&aacute;metro,
     * se firma el documento entero.
     * El nodo se determina por su identificador. */
    public static final String NODE_TOSIGN = "nodeToSign";//$NON-NLS-1$

    /** No incluye la transformaci&oacute;n <code>Enveloped</code> cuando se ha indicado adem&aacute;s
     * <i>nodeToSign</i>.
     * Solo aplica a firmas Enveloped, y el usuario debe asegurarse de que la firma se inserte
     * en un lugar o nodo del XML que no interfiera con el contenido firmado. */
    public static final String AVOID_ENVELOPED_TRANSFORM_WHEN_SIGNING_NODE = "avoidEnvelopedTransformWhenSigningNode"; //$NON-NLS-1$

    /** Formato de firma. Se aceptan los siguientes valores:<br>
     * <ul>
     *  <li>
     *   <i>XAdES Detached</i> (<code>AOSignConstants.SIGN_FORMAT_XADES_DETACHED</code>)
     *  </li>
     *  <li>
     *   <i>XAdES Externally Detached</i> (<code>AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED</code>)
     *   <p>
     *    Para el uso del formato <i>XAdES Externally Detached</i> es necesario
     *    establecer tambi&eacute;n el par&aacute;metro <code>uri</code> con una
     *    direcci&oacute;n accesible universalmente.
     *   </p>
     *  </li>
     *  <li>
     *   <i>XAdES Enveloped</i> (<code>AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED</code>)
     *  </li>
     *  <li>
     *   <i>XAdES Enveloping</i> (<code>AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING</code>)
     *  </li>
     * </ul>
     */
    public static final String FORMAT = "format";//$NON-NLS-1$

    /**
     * Perfil de firma que se desea generar.
     */
	public static final String PROFILE = "profile"; //$NON-NLS-1$

    /** Algoritmo de huella digital a usar en las referencias XML
     * (referencesDigestMethod). Debe indicarse como una URL, acept&aacute;ndose
     * los siguientes valores:
     * <ul>
     *  <li><i>http://www.w3.org/2000/09/xmldsig#sha1</i> (SHA-1, valor deprecado)</li>
     *  <li><i>http://www.w3.org/2001/04/xmlenc#sha256</i> (SHA-256)</li>
     *  <li><i>http://www.w3.org/2001/04/xmlenc#sha512</i> (SHA-512)</li>
     * </ul>
     */
	public static final String REFERENCES_DIGEST_METHOD = "referencesDigestMethod";//$NON-NLS-1$

    /** Algoritmo de canonicalizaci&oacute;n. */
	public static final String CANONICALIZATION_ALGORITHM = "canonicalizationAlgorithm";//$NON-NLS-1$

    /** URL de definici&oacute;n del espacio de nombres de XAdES (y por
     * extensi&oacute;n, versi&oacute;n de XAdES). Si se establece este
     * par&aacute;metro es posible que se necesite establecer tambi&eacute;n el
     * par&aacute;metro <code>signedPropertiesTypeUrl</code> para evitar
     * incoherencias en la versi&oacute;n de XAdES. */
	public static final String XADES_NAMESPACE = "xadesNamespace";//$NON-NLS-1$

    /**
     * URL de definici&oacute;n del tipo de las propiedades firmadas (<i>Signed
     * Properties</i>) de XAdES. Si se establece este par&aacute;metro es
     * posible que se necesite establecer tambi&eacute;n el par&aacute;metro
     * <code>xadesNamespace</code> para evitar incoherencias en la
     * versi&oacute;n de XAdES.<br>
     * Si no se establece se usa el valor por defecto: <a
     * href="http://uri.etsi.org/01903#SignedProperties"
     * >http://uri.etsi.org/01903#SignedProperties</a>.
     */
	public static final String SIGNED_PROPERTIES_TYPE_URL = "signedPropertiesTypeUrl";//$NON-NLS-1$

    /**
     * Ignora las hojas de estilo externas de los XML (no las firma) si se
     * establece a <code>true</code>, si se establece a <code>false</code>
     * act&uacute;a normalmente (s&iacute; las firma). (s&iacute; las firma).
     * <br> No aplica a contrafirmas.
     */
	public static final String IGNORE_STYLE_SHEETS = "ignoreStyleSheets";//$NON-NLS-1$

    /**
     * No declara transformaciones Base64 incluso si son necesarias si se
     * establece a <code>true</code>, si se establece a <code>false</code>
     * act&uacute;a normalmente (s&iacute; las declara). (s&iacute; las
     * declara). <br>
     * No aplica a contrafirmas)
     */
	public static final String AVOID_BASE64_TRANSFORMS = "avoidBase64Transforms";//$NON-NLS-1$

    /** Evita cualquier interacci&oacute;n con el usuario si se establece a
     * <code>true</code>, si se establece a <code>false</code> act&uacute;a
     * normalmente (puede mostrar di&aacute;logos, por ejemplo, para la
     * dereferenciaci&oacute;n de hojas de estilo enlazadas con rutas
     * relativas). &Uacute;til para los procesos desatendidos y por lotes. */
	public static final String HEADLESS = "headless";//$NON-NLS-1$

    /** Indica, mediante true (por defecto) o false, si debe incluirse el nodo
     * KeyValue dentro de KeyInfo de XAdES. */
	public static final String ADD_KEY_INFO_KEY_VALUE = "addKeyInfoKeyValue";//$NON-NLS-1$

    /** Indica, mediante true o false (por defecto), si debe incluirse el nodo
     * KeyName dentro de KeyInfo de XAdES. */
    public static final String ADD_KEY_INFO_KEY_NAME = "addKeyInfoKeyName";//$NON-NLS-1$

    /** Indica, mediante true o false (por defecto), si debe incluirse el nodo
     * X509IssuerSerial dentro de KeyInfo de XAdES. */
    public static final String ADD_KEY_INFO_X509_ISSUER_SERIAL = "addKeyInfoX509IssuerSerial"; //$NON-NLS-1$

    /** Algoritmo utilizado para el c&aacute;lculo de la huella digital cuando se
     * proporciona esta en vez de los datos a firmar. Cuando se proporcione una
     * huella en vez de datos deben tenerse en cuenta los siguientes aspectos:
     * <ul>
     *  <li>
     *   La huella digital debe indicarse en lugar de los datos, en el mismo
     *   par&aacute;metro del m&eacute;todo de firma.</li>
     *  <li>
     *   Solo puede indicarse una huella cuando no se incluyan los datos dentro
     *   de la propia firma, es decir, en firmas <i>externally detached</i>,
     *   siendo conveniente adem&aacute;s hacer uso de un <i>Manifest</i>.
     *  </li>
     * </ul> */
    public static final String PRECALCULATED_HASH_ALGORITHM = "precalculatedHashAlgorithm";//$NON-NLS-1$

    /** Indica, mediante un <code>true</code> o <code>false</code>, si se deben
     * realizar las necesarias restricciones de comportamiento para la firma de
     * facturas electr&oacute;nicas (FACTURAe). Estas restricciones son, no
     * introducir la transformaci&oacute;n de canonicalizaci&oacute;n de la
     * firma, ni la transformaci&oacute;n XPATH en las firmas <i>Enveloped</i>. */
    public static final String FACTURAE_SIGN = "facturaeSign";//$NON-NLS-1$

    /** MIME-Type de los datos a firmar.
     * Si no se indica se realiza una auto-detecci&oacute;n cuyo resultado puede ser inexacto. */
    public static final String CONTENT_MIME_TYPE = "mimeType";//$NON-NLS-1$

    /**
     * OID que identifica el tipo de datos a firmar. <br> No aplica a contrafirmas.
     */
    public static final String CONTENT_TYPE_OID = "contentTypeOid";//$NON-NLS-1$

    /** Codificaci&oacute;n de los datos a firmar.<br>No aplica a contrafirmas.
     * Por restricc&oacute;n de esquema de XMLDsig debe ser una URI:
     * <pre>
     *    &lt;element name="Object" type="ds:ObjectType"/&gt;
     *    &lt;complexType name="ObjectType" mixed="true"&gt;
     *      &lt;sequence minOccurs="0" maxOccurs="unbounded"&gt;
     *        &lt;any namespace="##any" processContents="lax"/&gt;
     *      &lt;/sequence&gt;
     *      &lt;attribute name="Id" type="ID" use="optional"/&gt;
     *      &lt;attribute name="MimeType" type="string" use="optional"/&gt;
     *      &lt;attribute name="Encoding" type="anyURI" use="optional"/&gt;
     *    &lt;/complexType&gt;
     * </pre> */
    public static final String CONTENT_ENCODING = "encoding";//$NON-NLS-1$

    /** Prefijo del par&aacute;metro con el MIME-Type de los datos de una referencia concreta de
     * las que se firman. */
    public static final String CONTENT_MIME_TYPE_PREFIX = "mimeType";//$NON-NLS-1$

    /**
     * Prefijo del par&aacute;metro con el OID que identifica el tipo de datos a firmar para una
     * referencia concreta de las firmadas. No aplica a contrafirmas.
     */
    static final String CONTENT_TYPE_OID_PREFIX = "contentTypeOid";//$NON-NLS-1$

    /** Prefijo del par&aacute;metro con la codificaci&oacute;n de los datos a firmar para una
     * referencia concreta de las firmadas. No aplica a contrafirmas. */
    public static final String CONTENT_ENCODING_PREFIX = "encoding";//$NON-NLS-1$

    /** Codificaci&oacute;n del XML de salida.
     * Si no se indica este valor se intenta auto-detectar a partir del XML de entrada (si los datos a firmar son un XML). */
    public static final String OUTPUT_XML_ENCODING = "outputXmlEncoding"; //$NON-NLS-1$

    /**
     * Identificador de la pol&iacute;tica de firma (normalmente una URL hacia
     * la pol&iacute;tica en formato XML procesable o una URN de tipo OID).
     * <br> Propiedad compartida con CAdES y PAdES.
     */
    public static final String POLICY_IDENTIFIER = "policyIdentifier";//$NON-NLS-1$

    /**
     * Huella digital del documento de pol&iacute;tica de firma (normalmente del
     * mismo fichero en formato XML procesable). Si no se indica, es obligatorio
     * que el par&aacute;metro <code>policyIdentifier</code> sea una URL
     * accesible universalmente. <br>
     * Propiedad compartida con CAdES y PAdES.
     */
    public static final String POLICY_IDENTIFIER_HASH = "policyIdentifierHash";//$NON-NLS-1$

    /**
     * Algoritmo usado para el c&uacute;lculo de la huella digital indicada en
     * el par&uacute;metro policyIdentifierHash. <br>
     * Propiedad compartida con CAdES y PAdES.
     */
    public static final String POLICY_IDENTIFIER_HASH_ALGORITHM = "policyIdentifierHashAlgorithm";//$NON-NLS-1$

    /**
     * Descripci&oacute;n textual de la pol&iacute;tica
     */
    public static final String POLICY_DESCRIPTION = "policyDescription";//$NON-NLS-1$

    /**
     * URL hacia el documento (legible por personas, normalmente en formato PDF)
     * descriptivo de la pol&iacute;tica de firma. <br>
     * Propiedad compartida con CAdES y PAdES.
     */
    public static final String POLICY_QUALIFIER = "policyQualifier";//$NON-NLS-1$

    /**
     * Lista de cargos atribuidos al firmante separados por el car&uacute;cter
     * '|'. Los cargos de la lista no pueden contener el car&uacute;cter '|' (ya
     * que este se usa como separador).
     */
    public static final String SIGNER_CLAIMED_ROLES = "signerClaimedRoles";//$NON-NLS-1$

    /**
     * Ciudad en la que se realiza la firma. <br>
     * Propiedad compartida con CAdES y PAdES.
     */
    public static final String SIGNATURE_PRODUCTION_CITY = "signatureProductionCity";//$NON-NLS-1$

    /**
     * Calle de la direcci&oacute;n en la que se realiza la firma.
     */
    public static final String SIGNATURE_PRODUCCTION_STREET_ADDRESS = "signatureProductionStreetAddress";//$NON-NLS-1$

    /**
     * Provincia en la que se realiza la firma.
     */
    public static final String SIGNATURE_PRODUCTION_PROVINCE = "signatureProductionProvince";//$NON-NLS-1$

    /**
     * C&oacute;digo postal en el que se realiza la firma. <br>
     * Propiedad compartida con CAdES y PAdES.
     */
    public static final String SIGNATURE_PRODUCTION_POSTAL_CODE = "signatureProductionPostalCode";//$NON-NLS-1$

    /**
     * Pa&iacute;s en el que se realiza la firma. <br>
     * Propiedad compartida con CAdES y PAdES.
     */
    public static final String SIGNATURE_PRODUCTION_COUNTRY = "signatureProductionCountry";//$NON-NLS-1$

    /**
     * N&uacute;mero de CommitmentTypeIndications a a&ntilde;adir a la firma
     * XAdES. En los par&uacute;metros siguientes, los CommitmentTypeIndications
     * se numeran a partir de 0 (cero).
     */
    public static final String COMMITMENT_TYPE_INDICATIONS = "commitmentTypeIndications";//$NON-NLS-1$

    /**
     * Prefijo de las claves con las que se indican las propiedades de los <i>Commitment
     * Type Indications</i>. Se utilizar&aacute; este prefijo, seguido el n&uacute;mero del
     * commitmentTypeIndication al que queramos referirnos y la clave de la propiedad en
     * cuesti&oacute;n. As&iacute; pues, los par&aacute;metros son:
     * <ul>
     *  <li>commitmentTypeIndication<i>n</i>Identifier</li>
     *  <li>commitmentTypeIndication<i>n</i>Description</li>
     *  <li>commitmentTypeIndication<i>n</i>CommitmentTypeQualifiers</li>
     *  <li>commitmentTypeIndication<i>n</i>DocumentationReferences</li>
     * </ul>
     */
    public static final String COMMITMENT_TYPE_INDICATION_PREFIX = "commitmentTypeIndication"; //$NON-NLS-1$

    /**
     * Tipo de <i>CommitmentTypeIndication</i> (atributo obligatorio, se debe usar el
     * ordinal, nunca el OID directamente):
     * <ul>
     * <li><i>1</i> = Prueba de origen</li>
     * <li><i>2</i> = Prueba de recepci&oacute;n</li>
     * <li><i>3</i> = Prueba de entrega</li>
     * <li><i>4</i> = Prueba de env&iacute;o</li>
     * <li><i>5</i> = Prueba de aprobaci&oacute;n</li>
     * <li><i>6</i> = Prueba de creaci&oacute;n</li>
     * </ul>
     * <br>
     * Propiedad compartida con CAdES.
     */
    public static final String COMMITMENT_TYPE_INDICATION_IDENTIFIER = "Identifier";//$NON-NLS-1$

    /**
     * Descripci&oacute;n textual del CommitmentTypeIndication n&uacute;mero n (atributo opcional).
     */
    public static final String COMMITMENT_TYPE_INDICATION_DESCRIPTION = "Description";//$NON-NLS-1$

    /**
     * Lista de URL separadas por el car&aacute;cter '<i>|</i>' que se aportan como referencias
     * documentales del <i>CommitmentTypeIndication</i> n&uacute;mero <i>n</i>
     * (atributo opcional).<br>
     * Las URL de la lista no pueden contener el car&aacute;cter '<i>|</i>' (ya
     * que este se usa como separador)
     */
    public static final String COMMITMENT_TYPE_INDICATION_DOCUMENTATION_REFERENCE = "DocumentationReferences"; //$NON-NLS-1$

    /**
     * Lista de indicadores textuales separados por el car&aacute;cter '|' que se aportan como calificadores
     * adicionales del CommitmentTypeIndication n&uacute;mero n (atributo opcional). Normalmente son OID.
     * Los elementos de la lista no pueden contener el car&aacute;cter '|' (ya que este se usa como separador).
     * <br>
     * Propiedad compartida con CAdES.
     */
    public static final String COMMITMENT_TYPE_INDICATION_QUALIFIERS = "CommitmentTypeQualifiers"; //$NON-NLS-1$

    // Definicion de las URI para los Commitment Type Identifiers:
    // http://uri.etsi.org/01903/v1.2.2/ts_101903v010202p.pdf

	private static final String COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_ORIGIN   = "http://uri.etsi.org/01903/v1.2.2#ProofOfOrigin"; //$NON-NLS-1$
	private static final String COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_RECEIPT  = "http://uri.etsi.org/01903/v1.2.2#ProofOfReceipt"; //$NON-NLS-1$
	private static final String COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_DELIVERY = "http://uri.etsi.org/01903/v1.2.2#ProofOfDelivery"; //$NON-NLS-1$
	private static final String COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_SENDER   = "http://uri.etsi.org/01903/v1.2.2#ProofOfSender"; //$NON-NLS-1$
	private static final String COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_APPROVAL = "http://uri.etsi.org/01903/v1.2.2#ProofOfApproval"; //$NON-NLS-1$
	private static final String COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_CREATION = "http://uri.etsi.org/01903/v1.2.2#ProofOfCreation"; //$NON-NLS-1$
	static final Map<String, String> COMMITMENT_TYPE_IDENTIFIERS = new HashMap<>(6);
	static {
		COMMITMENT_TYPE_IDENTIFIERS.put("1", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_ORIGIN); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("2", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_RECEIPT); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("3", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_DELIVERY); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("4", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_SENDER); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("5", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_APPROVAL); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("6", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_CREATION); //$NON-NLS-1$
	}

    /**
     * Modo de firma. Se utiliza solo para se&ntilde;alar que ya ha dejado de usarse.
     */
	@Deprecated
    static final String MODE = "mode";//$NON-NLS-1$

    /**
     * Nombre del par&aacute;metro que almacena el identificador de la firma durante un proceso de
     * firma de lote.
     */
    static final String BATCH_SIGNATURE_ID = "SignatureId"; //$NON-NLS-1$

    /**
     * Nombre nodo ra&iacute;z de la firma.
     */
    public static final String ROOT_XML_NODE_NAME = "RootXmlNodeName"; //$NON-NLS-1$

    /**
     * Nombre del namespace en el nodo ra&iacute;z de la firma.
     */
    public static final String ROOT_XML_NODE_NAMESPACE = "RootXmlNodeNamespace"; //$NON-NLS-1$

    /**
     * Nombre del prefijo en el namespace en el nodo ra&iacute;z de la firma.
     */
    public static final String ROOT_XML_NODE_NAMESPACE_PREFIX = "RootXmlNodeNamespacePrefix"; //$NON-NLS-1$

    /**
     * Par&aacute;metro interno (no se puede usar desde el exterior) para desactivar la validaci&oacute;n del
     * PKCS#1 generado frente al certificado utilizado.
     */
    public static final String INTERNAL_VALIDATE_PKCS1 = "validatePkcs1"; //$NON-NLS-1$

    /**
     * Par&aacute;metro que permite solicitar confirmaci&oacute;n para multifirmar con un perfil
     * distinto al que usa la operaci&oacute;n de firma.
     */
    public static final String CONFIRM_DIFFERENT_PROFILE = "confirmDifferentProfile"; //$NON-NLS-1$

    /**
     * Par&aacute;metro que permite configurar si se permite la multifirma de firmas de archivo aunque
     * queden invalidadas.
     */
	public static final String ALLOW_SIGN_LTS_SIGNATURES = "allowSignLTSignature"; //$NON-NLS-1$

    /**
     * Par&aacute;metro que permite configurar si se deben evitar las incompatibilidades con la
     * pol&iacute;tica de firma de la AGE. Esto conllevar&aacute; a que se modifique la configuraci&oacute;n
     * de firma o, en caso de no pode o que se establezca a {@code false}, que se cancele la operaci&oacute;n.
     */
	public static final String AVOID_AGE_POLICY_INCOMPATIBILITIES = "avoidAGEPolicyIncompatibilities"; //$NON-NLS-1$

    /**
     * Constructor vac&iacute;o privado para que no se pueda instanciar la clase
     * ya que es est&aacute;tico.
     */
    private XAdESExtraParams() {
        // No instanciable
    }
}
