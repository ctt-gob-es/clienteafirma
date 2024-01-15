package es.gob.afirma.signers.xades;

import javax.xml.crypto.dsig.DigestMethod;

/**
 * Constantes de firma XML o XAdES utilizadas en distintos puntos del m&oacute;dulo.
 */
public class XAdESConstants {

	public static final String TAG_SIGNED_PROPERTIES = "SignedProperties"; //$NON-NLS-1$

	public static final String TAG_UNSIGNED_PROPERTIES = "UnsignedProperties";  //$NON-NLS-1$

	public static final String TAG_QUALIFYING_PROPERTIES = "QualifyingProperties"; //$NON-NLS-1$

	public static final String TAG_SIGNING_CERTIFICATE = "SigningCertificate"; //$NON-NLS-1$

	public static final String TAG_SIGNING_CERTIFICATE_V2 = "SigningCertificateV2"; //$NON-NLS-1$

	public static final String TAG_SIGNING_TIME = "SigningTime"; //$NON-NLS-1$

	public static final String TAG_SIGNED_DATA_OBJECT_PROPERTIES = "SignedDataObjectProperties"; //$NON-NLS-1$

	public static final String TAG_DATA_OBJECT_FORMAT = "DataObjectFormat"; //$NON-NLS-1$

	static final String TAG_REFERENCE = "Reference"; //$NON-NLS-1$

	public static final String TAG_MIME_TYPE = "MimeType"; //$NON-NLS-1$

	public static final String TAG_DESCRIPTION = "Description"; //$NON-NLS-1$

	public static final String TAG_CERTIFICATE_VALUES = "CertificateValues"; //$NON-NLS-1$

	public static final String TAG_REVOCATION_VALUES = "RevocationValues"; //$NON-NLS-1$

	public static final String TAG_TIME_STAMP_VALIDATION_DATA = "TimeStampValidationData"; //$NON-NLS-1$

	public static final String TAG_COMPLETE_CERTIFICATE_REFS = "CompleteCertificateRefs"; //$NON-NLS-1$

	public static final String TAG_COMPLETE_REVOCATION_REFS = "CompleteRevocationRefs"; //$NON-NLS-1$

	public static final String TAG_ATTRIBUTE_CERTIFICATE_REFS = "AttributeCertificateRefs"; //$NON-NLS-1$

	public static final String TAG_ATTRIBUTE_REVOCATION_REFS = "AttributeRevocationRefs"; //$NON-NLS-1$

	public static final String TAG_SIG_AND_REFS_TIMESTAMP = "SigAndRefsTimeStamp"; //$NON-NLS-1$

	public static final String TAG_REFS_ONLY_TIMESTAMP = "RefsOnlyTimeStamp"; //$NON-NLS-1$

	public static final String TAG_SIGNATURE_TIMESTAMP = "SignatureTimeStamp"; //$NON-NLS-1$

	public static final String TAG_QUALIFYING_PROPERTIES_REFERENCE = "QualifyingPropertiesReference"; //$NON-NLS-1$

	public static final String TAG_XML_TIMESTAMP = "XMLTimeStamp"; //$NON-NLS-1$

	public static final String TAG_TIMESTAMP = "XMLTimeStamp"; //$NON-NLS-1$

	public static final String TAG_SIGNATURE_METHOD = "SignatureMethod"; //$NON-NLS-1$

	public static final String TAG_IDENTIFIER = "Identifier"; //$NON-NLS-1$

	public static final String TAG_CITY = "City"; //$NON-NLS-1$

	public static final String TAG_STATE_OR_PROVINCE = "StateOrProvince"; //$NON-NLS-1$

	public static final String TAG_POSTAL_CODE = "PostalCode"; //$NON-NLS-1$

	public static final String TAG_COUNTRY_NAME = "CountryName"; //$NON-NLS-1$

	public static final String TAG_SIGNATURE_PRODUCTION_PLACE = "SignatureProductionPlace"; //$NON-NLS-1$

	public static final String TAG_SIGNATURE_PRODUCTION_PLACE_V2 = "SignatureProductionPlaceV2"; //$NON-NLS-1$

	public static final String TAG_DIGEST_METHOD = "DigestMethod"; //$NON-NLS-1$

	public static final String TAG_DIGEST_VALUE = "DigestValue"; //$NON-NLS-1$

	public static final String TAG_SPURI = "SPURI"; //$NON-NLS-1$

	public static final String TAG_CLAIMED_ROLE = "ClaimedRole"; //$NON-NLS-1$

	public static final String TAG_SIGNER_ROLE = "SignerRole"; //$NON-NLS-1$

	public static final String TAG_SIGNER_ROLE_V2 = "SignerRoleV2"; //$NON-NLS-1$

	public static final String TAG_X509_CERTIFICATE = "X509Certificate"; //$NON-NLS-1$

	public static final String TAG_STREET_ADDRESS = "StreetAddress"; //$NON-NLS-1$

	/** Nombre del nodo de atributos firmados de la firma. */
	public static final String TAG_SIGNED_SIGNATURE_PROPERTIES = "SignedSignatureProperties"; //$NON-NLS-1$

	/** Nombre del nodo de atributos sin firmar de la firma. */
    public static final String TAG_UNSIGNED_SIGNATURE_PROPERTIES = "UnsignedSignatureProperties"; //$NON-NLS-1$

    /** Nombre del identificador de pol&iacute;ticas. */
	public static final String TAG_SIGNATURE_POLICY_IDENTIFIER = "SignaturePolicyIdentifier"; //$NON-NLS-1$

	/** Nombre del nodo de la firma de archivo. */
    public static final String TAG_ARCHIVE_TIMESTAMP = "ArchiveTimeStamp"; //$NON-NLS-1$

    /** Nombre del nodo en el que se englobara&aacute;n las firmas cuando sea necesario. */
    static final String TAG_PARENT_NODE = "AFIRMA"; //$NON-NLS-1$

	/** Prefijo que por defecto se usar&aacute; para referir al namespace de XMLdSig. */
    static final String DEFAULT_XML_SIGNATURE_PREFIX = "ds"; //$NON-NLS-1$

    /** Prefijo que por defecto se usar&aacute; para referir al namespace de XAdES. */
    static final String DEFAULT_XADES_SIGNATURE_PREFIX = "xades"; //$NON-NLS-1$

    /** Algoritmo de huella digital por defecto para las referencias XML. */
    public static final String DEFAULT_DIGEST_METHOD = DigestMethod.SHA512;

    /** URI que define el espacio d nombre de XAdES sin indicar la versi&oacute;n. */
    public static final String NAMESPACE_XADES_NO_VERSION = "http://uri.etsi.org/01903#"; //$NON-NLS-1$

    /** URI que define el espacio de nombres de XAdES v1.1.1. */
    public static final String NAMESPACE_XADES_1_1_1 = "http://uri.etsi.org/01903/v1.1.1#"; //$NON-NLS-1$

    /** URI que define el espacio de nombres de XAdES v1.2.2. */
    public static final String NAMESPACE_XADES_1_2_2 = "http://uri.etsi.org/01903/v1.2.2#"; //$NON-NLS-1$

    /** URI que define el espacio de nombres de XAdES v1.3.2. */
    public static final String NAMESPACE_XADES_1_3_2 = "http://uri.etsi.org/01903/v1.3.2#"; //$NON-NLS-1$

    /** URI que define el espacio de nombres de XAdES v1.4.1. */
    public static final String NAMESPACE_XADES_1_4_1 = "http://uri.etsi.org/01903/v1.4.1#"; //$NON-NLS-1$

    /** URI que define la versi&oacute;n por defecto de XAdES. */
    static final String DEFAULT_NAMESPACE_XADES = NAMESPACE_XADES_1_3_2;

    /** URI con referencia a SignedProperties que define el espacio de nombres de XAdES sin indicar versi&oacute;n. */
    static final String NAMESPACE_XADES_NO_VERSION_SIGNED_PROPERTIES = NAMESPACE_XADES_NO_VERSION + TAG_SIGNED_PROPERTIES;

    /** URI con referencia a SignedProperties que define el espacio de nombres de XAdES v1.2.2. */
    static final String NAMESPACE_XADES_1_2_2_SIGNED_PROPERTIES = NAMESPACE_XADES_1_2_2 + TAG_SIGNED_PROPERTIES;

    /** URI que define el espacio de nombres de XAdES v1.2.2. */
    static final String NAMESPACE_XADES_1_3_2_SIGNED_PROPERTIES = NAMESPACE_XADES_1_3_2 + TAG_SIGNED_PROPERTIES;

    /** URI que define el espacio de nombres de XAdES v1.4.1. */
    static final String NAMESPACE_XADES_1_4_1_SIGNED_PROPERTIES = NAMESPACE_XADES_1_4_1 + TAG_SIGNED_PROPERTIES;

    /** URI que define el tipo de propiedades firmadas de XAdES. */
    static final String REFERENCE_TYPE_SIGNED_PROPERTIES = NAMESPACE_XADES_NO_VERSION + TAG_SIGNED_PROPERTIES;

    /** URI que define una referencia de tipo MANIFEST. */
    static final String REFERENCE_TYPE_MANIFEST = "http://www.w3.org/2000/09/xmldsig#Manifest"; //$NON-NLS-1$

    /** Atributo identificador de elementos. */
    static final String ID_IDENTIFIER = "Id"; //$NON-NLS-1$

}
