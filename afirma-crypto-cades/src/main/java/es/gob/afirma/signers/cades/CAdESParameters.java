package es.gob.afirma.signers.cades;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;

/**
 * Detalles de configuraci&oacute; para la generacion de una firma CAdES.
 */
public class CAdESParameters {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String SHA1_ALGORITHM = "SHA1"; //$NON-NLS-1$

	private AdESPolicy externalPolicy;

	private boolean signingCertificateV2 = true;

	private boolean includedOnlySigningCertificate = false;

	private String digestAlgorithm;

	private byte[] dataDigest;

	private  boolean contentNeeded = true;

	private byte[] contentData;

	private Date signingTime;

	private String contentTypeOid;

	private String contentDescription;

	private List<CommitmentTypeIndicationBean> commitmentTypeIndications;

	private String[] claimedRoles;

	private CAdESSignerMetadata metadata;

	private boolean includedPolicyOnSigningCertificate = true;

	private boolean includedIssuerSerial = true;

	private Properties extraParams;

	private String profileSet = AOSignConstants.DEFAULT_SIGN_PROFILE;

	public CAdESParameters() {
		// No se permite construir el objeto externamente
	}

	/**
	 * Carga la configuraci&oacute;n de firma CAdES a partir del los par&aacute;metros establecidos.
	 * @param data Huella digital de los datos si se establece el
	 * "precalculatedMessageDigest". Si no, ser&aacute;n los propios datos o el "procesable array"
	 * de un PDF en caso de querer la firma para incluirla en una PAdES.
	 * @param algorithm Algoritmo de firma.
	 * @param config Par&aacute;metros extra de configuraci&oacute;n.
	 * @return Par&aacute;metros para la creaci&oacute;n de la firma.
	 * @throws AOException Cuando ocurre un error grave al procesasr los par&aacute;metros.
	 */
	public static CAdESParameters load(final byte[] data, final String algorithm, final Properties config) throws AOException {

		final CAdESParameters dataConfig = new CAdESParameters();

		// Identificamos si se debe incluir el atributo SigningCertificateV2
		boolean signingCertificateV2;
		if (AOSignConstants.isSHA2SignatureAlgorithm(algorithm)) {
			signingCertificateV2 = true;
		}
		else if (config.containsKey(CAdESExtraParams.SIGNING_CERTIFICATE_V2)) {
			signingCertificateV2 = Boolean.parseBoolean(config.getProperty(CAdESExtraParams.SIGNING_CERTIFICATE_V2));
		}
		else {
			signingCertificateV2 = !SHA1_ALGORITHM.equals(AOSignConstants.getDigestAlgorithmName(algorithm));
		}
		dataConfig.setSigningCertificateV2(signingCertificateV2);

		// Algoritmo usado cuando se proporciona la huella digital precalculada
		final String precalculatedDigestAlgorithm = config.getProperty(CAdESExtraParams.PRECALCULATED_HASH_ALGORITHM);

		// Comprobamos si tenemos que omitir los datos en la firma o no
		boolean omitContent = false;
		final String mode = config.getProperty(CAdESExtraParams.MODE, AOSignConstants.DEFAULT_SIGN_MODE);
		if (precalculatedDigestAlgorithm != null || AOSignConstants.SIGN_MODE_EXPLICIT.equalsIgnoreCase(mode)) {
			omitContent = true;
		}
		dataConfig.setContentNeeded(!omitContent);

		// Algoritmo de huella interna usado finalmente
		final byte[] dataDigest;
		final byte[] contentData;
		final String digestAlgorithmName;
		if (precalculatedDigestAlgorithm != null) {
			digestAlgorithmName = AOSignConstants.getDigestAlgorithmName(precalculatedDigestAlgorithm);
			dataDigest = data;
			contentData = null;
		}
		else {
			digestAlgorithmName = AOSignConstants.getDigestAlgorithmName(algorithm);
			contentData = omitContent ? null : data;
			if (data != null) {
				try {
					dataDigest = MessageDigest.getInstance(digestAlgorithmName).digest(data);
				}
				catch (final NoSuchAlgorithmException e) {
					throw new AOException("Algoritmo no soportado: " + e, e); //$NON-NLS-1$
				}
			}
			else {
				dataDigest = null;
			}
		}
		dataConfig.setContentData(contentData);
		dataConfig.setDigestAlgorithm(digestAlgorithmName);
		dataConfig.setDataDigest(dataDigest);

		// Roles declarados
		String[] claimedRoles = null;
		final String claimedRolesParam = config.getProperty(CAdESExtraParams.SIGNER_CLAIMED_ROLES);
		if (claimedRolesParam != null && !claimedRolesParam.isEmpty()) {
			claimedRoles = claimedRolesParam.split(Pattern.quote("|")); //$NON-NLS-1$
		}
		dataConfig.setClaimedRoles(claimedRoles);

		// Incluir politica de certificacion del certificado firmante en la firma
		final boolean doNotIncludePolicyOnSigningCertificate = Boolean.parseBoolean(
				config.getProperty(
						CAdESExtraParams.DO_NOT_INCLUDE_POLICY_ON_SIGNING_CERTIFICATE, Boolean.FALSE.toString()
						)
				);
		dataConfig.setIncludedPolicyOnSigningCertificate(!doNotIncludePolicyOnSigningCertificate);

		// Politica de firma
		dataConfig.setExternalPolicy(AdESPolicy.buildAdESPolicy(config));

		// Incluir solo certificado de firma
		dataConfig.setIncludedOnlySigningCertificate(Boolean.parseBoolean(
				config.getProperty(CAdESExtraParams.INCLUDE_ONLY_SIGNNING_CERTIFICATE, Boolean.FALSE.toString())));

		// Commintment type indications declarados
		dataConfig.setCommitmentTypeIndications(CommitmentTypeIndicationsHelper.getCommitmentTypeIndications(config));

		// La marca de tiempo se incluira siempre salvo que desde el exterior se indique que no se haga
		boolean includeSigningTime = true;
		if (config.containsKey(CAdESExtraParams.INCLUDE_SIGNING_TIME_ATTRIBUTE)) {
			includeSigningTime = Boolean.parseBoolean(config.getProperty(CAdESExtraParams.INCLUDE_SIGNING_TIME_ATTRIBUTE));
		}
		if (includeSigningTime) {
			dataConfig.setSigningTime(new Date());
		}

		// El atributo content-hint se incluira siempre salvo que desde el exterior se indique que no se haga
		if (!config.containsKey(CAdESExtraParams.INCLUDE_CONTENT_HINT_ATTRIBUTE) ||
				!Boolean.FALSE.toString().equalsIgnoreCase(CAdESExtraParams.INCLUDE_CONTENT_HINT_ATTRIBUTE)) {
			// Determinamos el tipo de contenido, ya sea
			// que obtengamos esta informacion del exterior o que se analicen los datos.
			String contentTypeOid = config.getProperty(CAdESExtraParams.CONTENT_TYPE_OID);
			String contentTypeDescription = config.getProperty(CAdESExtraParams.CONTENT_DESCRIPTION);
			if (data != null && (contentTypeOid == null || contentTypeDescription == null)) {
				try {
					final MimeHelper mimeHelper = new MimeHelper(data);
					if (contentTypeOid == null) {
						contentTypeOid = MimeHelper.transformMimeTypeToOid(mimeHelper.getMimeType());
					}
					if (contentTypeDescription == null) {
						contentTypeDescription = mimeHelper.getDescription();
					}
				}
				catch (final Exception e) {
					LOGGER.warning(
							"No se han podido cargar las librerias para identificar el tipo de dato firmado: " + e //$NON-NLS-1$
							);
				}
			}
			dataConfig.setContentTypeOid(contentTypeOid != null ?
					contentTypeOid : MimeHelper.DEFAULT_CONTENT_OID_DATA);
			dataConfig.setContentDescription(contentTypeDescription != null ?
					contentTypeDescription : MimeHelper.DEFAULT_CONTENT_DESCRIPTION);
		}

		// Metadatos con la localizacion de firma
		dataConfig.setMetadata(CAdESSignerMetadataHelper.getCAdESSignerMetadata(config));

		/// Identificamos si debemos generar una firma baseline para establecer la configuracion
		// propia para estas firmas
		final boolean baselineProfile = AOSignConstants.SIGN_PROFILE_BASELINE.equals(
				config.getProperty(CAdESExtraParams.PROFILE));

		// Almacenamos el perfil configurado que, en caso de no ser baseline, sera el avanzado
		if (baselineProfile) {
			dataConfig.setProfileSet(AOSignConstants.SIGN_PROFILE_BASELINE);
		} else {
			dataConfig.setProfileSet(AOSignConstants.SIGN_PROFILE_ADVANCED);
		}

		// En el estandar baseline ETSI EN 319 122-1 V1.1.1 se indica expresamente que no se
		// deberia incluir el IssuerSerial en el atributo del certificado firmante. Sin embargo,
		// los validadores suelen emitir advertencias cuando se omite este atributo, asi que
		// se seguira agregando
		//dataConfig.setIncludedIssuerSerial(!baselineProfile);

		// Configuracion establecida, que puede contener mas informacion que la requerida para la generacion de la firma CAdES
		dataConfig.setExtraParams(config);

		return dataConfig;
	}

	/**
	 * Recupera la informaci&oacute;n de la pol&iacute;tica de firma configurada para la
	 * generaci&oacute;n de una firma CAdES-EPES o CAdES-B-Level con pol&iacute;tica.
	 * @return Informaci&oacute;n de la pol&iacute;tica de firma o {@code null} si no se
	 * estableci&oacute;.
	 */
	public AdESPolicy getExternalPolicy() {
		return this.externalPolicy;
	}

	/**
	 * Establece la informaci&oacute;n de la pol&iacute;tica de firma configurada para la
	 * generaci&oacute;n de una firma CAdES-EPES o CAdES-B-Level con pol&iacute;tica.
	 * @param externalPolicy Informaci&oacute;n de la pol&iacute;tica de firma.
	 */
	public void setExternalPolicy(final AdESPolicy externalPolicy) {
		this.externalPolicy = externalPolicy;
	}

	/**
	 * Indica si la firma se generar&aacute;con el atributo SigningCertificateV2. Este atributo
	 * debe establecerse cuando se generen firmas con algoritmos de firma SHA-2 o superiores.
	 * @return {@code true} si se debe generar una firma con el atributo SigningCertificateV2,
	 * {@code false} si debe usarse el atributo SigningCertificate.
	 */
	public boolean isSigningCertificateV2() {
		return this.signingCertificateV2;
	}

	/**
	 * Establece si debe generarse la firma con el atributo SigningCertificateV2. Este atributo
	 * debe establecerse cuando se generen firmas con algoritmos de firma SHA-2 o superiores.
	 * @param signingCertificateV2 {@code true} si se debe generar una firma con el atributo
	 * SigningCertificateV2, {@code false} si debe usarse el atributo SigningCertificate.
	 */
	public void setSigningCertificateV2(final boolean signingCertificateV2) {
		this.signingCertificateV2 = signingCertificateV2;
	}

	/**
	 * Indica si debe incorporarse a la firma s&oacute;lo el certificado de firma y no toda la
	 * cadena de certificaci&oacute;n.
	 * @return {@code true} si se debe incorporarse s&oacute;lo el certificado de firma, {@code false}
	 * si se introduci&aacute; toda la cadena de certificaci&oacute;n.
	 */
	public boolean isIncludedOnlySigningCertificate() {
		return this.includedOnlySigningCertificate;
	}

	/**
	 * Establece si debe incorporarse a la firma s&oacute;lo el certificado de firma y no toda la
	 * cadena de certificaci&oacute;n.
	 * @param includedOnlySigningCertificate {@code true} si se debe incorporarse s&oacute;lo el
	 * certificado de firma, {@code false} si se introduci&aacute; toda la cadena de certificaci&oacute;n.
	 */
	public void setIncludedOnlySigningCertificate(final boolean includedOnlySigningCertificate) {
		this.includedOnlySigningCertificate = includedOnlySigningCertificate;
	}

	/**
	 * Recupera el algorimo de huella digital a utilizar internamente para el calculo de la huella
	 * de los datos. Es obligatorio indicar el algoritmo de huella.
	 * @return Nombre del algoritmo de huella digital o {@code null} si no se ha establecido.
	 */
	public String getDigestAlgorithm() {
		return this.digestAlgorithm;
	}

	/**
	 * Establece el algorimo de huella digital a utilizar internamente para el calculo de la huella
	 * de los datos.
	 * @param digestAlgorithm Nombre del algoritmo de huella digital.
	 */
	public void setDigestAlgorithm(final String digestAlgorithm) {
		this.digestAlgorithm = digestAlgorithm;
	}

	/**
	 * Recupera la huella digital de los datos a firmar. Esta huella debe generarse con el algoritmo
	 * que se obtiene en la llamada a {@code #getDigestAlgorithm()}. Si no se establece, ser&aacute;
	 * necesario proporcionar e incluir los datos en la firma.
	 * @return Huella digital de los datos o {@code null} si no se ha establecido.
	 * @see #getDigestAlgorithm()
	 */
	public byte[] getDataDigest() {
		return this.dataDigest;
	}

	/**
	 * Establece la huella digital de los datos a firmar. Esta huella debe
	 * generarse con el algoritmo que se obtiene en la llamada a
	 * {@code #getDigestAlgorithm()}. Si se establece a {@code null} la huella
	 * se generar&aacute; en el momento de la firma en base a los datos.
	 * @param dataDigest Huella digital de los datos o {@code null} si no se ha
	 * establecido.
	 * @see #getDigestAlgorithm()
	 */
	public void setDataDigest(final byte[] dataDigest) {
		this.dataDigest = dataDigest;
	}

	/**
	 * Establece si deben incluirse los datos firmados en la firma.
	 * @param contentNeeded {@code true} si se deben incluirse los datos,
	 * {@code false} en caso contrario.
	 */
	public void setContentNeeded(final boolean contentNeeded) {
		this.contentNeeded = contentNeeded;
	}

	/**
	 * Indica si deben incorporarse los datos firmados a la firma. Por defecto, se incluir&aacute;n.
	 * @return {@code true} si se deben incorporarse los datos, {@code false}
	 * en caso contrario.
	 */
	public boolean isContentNeeded() {
		return this.contentNeeded;
	}

	/**
	 * Recupera los datos que se firman e incluir&aacute;n en la firma.
	 * @return Datos que se firmar&aacute;n o {@code null} si los datos no se deben introducir en la firma.
	 */
	public byte[] getContentData() {
		return this.contentData;
	}

	/**
	 * Establece los datos que se firman e incluir&aacute;n en la firma. Si no se desea incluir los datos
	 * en la firma (firma detached o expl&iacute;cita) se debe indicar la huella de los datos mediante el
	 * m&eacute;todo {@code #setDataDigest(byte[])}.
	 * @param contentData Datos que se firmar&aacute;n e incluir&aacute;n en la firma.
	 * @see #setDataDigest(byte[])
	 */
	public void setContentData(final byte[] contentData) {
		this.contentData = contentData;
	}

	/**
	 * Recupera la hora que se debe introducir en la firma.
	 * @return Hora de firma o {@code null} si no se debe introducir la hora.
	 */
	public Date getSigningTime() {
		return this.signingTime;
	}

	/**
	 * Establece la hora que se debe introducir en la firma.
	 * @param signingTime Hora de firma o {@code null} si no se estableci&oacute;.
	 */
	public void setSigningTime(final Date signingTime) {
		this.signingTime = signingTime;
	}

	/**
	 * Recupera el OID correspondiente al tipo de contenido.
	 * @return OID del tipo de contenido o {@code null} si no se estableci&oacute;.
	 */
	public String getContentTypeOid() {
		return this.contentTypeOid;
	}

	/**
	 * Establece el OID correspondiente al tipo de contenido.
	 * @param contentTypeOid OID del tipo de contenido.
	 */
	public void setContentTypeOid(final String contentTypeOid) {
		this.contentTypeOid = contentTypeOid;
	}

	/**
	 * Recupera el texto descriptivo del tipo de contenido.
	 * @return Descripci&oacute;n del tipo de contenido o {@code null} si no se estableci&oacute;.
	 */
	public String getContentDescription() {
		return this.contentDescription;
	}

	/**
	 * Establece el texto descriptivo del tipo de contenido.
	 * @param contentDescription Descripci&oacute;n del tipo de contenido o {@code null} si no se estableci&oacute;.
	 */
	public void setContentDescription(final String contentDescription) {
		this.contentDescription = contentDescription;
	}

	/**
	 * Recupera el listado de tipos de compromisos declarados por el firmante en el momento de la firma.
	 * @return Listado de tipos de compromisos declarados o {@code null} si no se estableci&oacute;.
	 */
	public List<CommitmentTypeIndicationBean> getCommitmentTypeIndications() {
		return this.commitmentTypeIndications;
	}

	/**
	 * Establece el listado de tipos de compromisos declarados por el firmante en el momento de la firma.
	 * @param commitmentTypeIndications Listado de tipos de compromisos declarados.
	 */
	public void setCommitmentTypeIndications(final List<CommitmentTypeIndicationBean> commitmentTypeIndications) {
		this.commitmentTypeIndications = commitmentTypeIndications;
	}

	/**
	 * Recupera el listado de roles declarados por el firmante.
	 * @return Listado de roles declarados o {@code null} si no se estableci&oacute;.
	 */
	public String[] getClaimedRoles() {
		return this.claimedRoles;
	}

	/**
	 * Establece el listado de roles declarados por el firmante.
	 * @param claimedRoles Listado de roles declarados.
	 */
	public void setClaimedRoles(final String[] claimedRoles) {
		this.claimedRoles = claimedRoles;
	}

	/**
	 * Recupera los metadatos declarados en la firma con el lugar de firma.
	 * @return Metadatos declarados con el lugar de firma o {@code null} si no se estableci&oacute;.
	 */
	public CAdESSignerMetadata getMetadata() {
		return this.metadata;
	}

	/**
	 * Establece los metadatos declarados en la firma con el lugar de firma.
	 * @param metadata Metadatos declarados con el lugar de firma.
	 */
	public void setMetadata(final CAdESSignerMetadata metadata) {
		this.metadata = metadata;
	}

	/**
	 * Indica si se debe incluir la politica de certificaci&oacute;n del certificado en la firma.
	 * Por defecto, s&iacute; se incluir&aacute;.
	 * @return {@code true} si debe incluirse, {@code false} en caso contrario.
	 */
	public boolean isIncludedPolicyOnSigningCertificate() {
		return this.includedPolicyOnSigningCertificate;
	}

	/**
	 * Establece si se debe incluir la politica de certificaci&oacute;n del certificado en la firma.
	 * @param includedPolicyOnSigningCertificate {@code true} si debe incluirse, {@code false} en caso contrario.
	 */
	public void setIncludedPolicyOnSigningCertificate(final boolean includedPolicyOnSigningCertificate) {
		this.includedPolicyOnSigningCertificate = includedPolicyOnSigningCertificate;
	}

	/**
	 * Indica si se debe incluir el n&uacute;mero de serie del certificado de firma y el Principal de su
	 * emisor en el SigningCertificate de la firma. Por defecto, se incluir&aacute;.
	 * @return {@code true} si debe incluirse, {@code false} en caso contrario.
	 */
	public boolean isIncludedIssuerSerial() {
		return this.includedIssuerSerial;
	}

	/**
	 * Establece si se debe incluir el n&uacute;mero de serie del certificado de firma y el Principal de su
	 * emisor en el SigningCertificate de la firma.
	 * @param includedIssuerSerial {@code true} si debe incluirse, {@code false} en caso contrario.
	 */
	public void setIncludedIssuerSerial(final boolean includedIssuerSerial) {
		this.includedIssuerSerial = includedIssuerSerial;
	}

	/**
	 * Obtiene la configuraci&oacute;n de firma establecida al cargar este objeto.
	 * @return Propiedades con la configuraci&oacute;n de firma indicada.
	 */
	public Properties getExtraParams() {
		return this.extraParams;
	}

	/**
	 * Establece una configuraci&oacute;n de firma externa. Esta configuraci&oacute;n no se
	 * cargar&aacute;. Es necesario proporcionarla mediante el uso del m&eacute;todo
	 * {@code #load(byte[], String, Properties)}.
	 * @param extraParams Propiedades con la configuraci&oacute;n de firma establecida.
	 * @see #load(byte[], String, Properties)
	 */
	public void setExtraParams(final Properties extraParams) {
		this.extraParams = extraParams;
	}

	/**
	 * Obtiene el juego de perfiles de firma configurado: Avanzados (BES, EPES) o Baseline (B-Level).
	 * @return Juego de perfiles a emplear.
	 * @see AOSignConstants#SIGN_PROFILE_ADVANCED
	 * @see AOSignConstants#SIGN_PROFILE_BASELINE
	 */
	public String getProfileSet() {
		return this.profileSet;
	}

	/**
	 * Establece el juego de perfiles de firma configurado: Avanzados (BES, EPES) o Baseline (B-Level).
	 * @param profileSet Juego de perfiles a emplear.
	 * @see AOSignConstants#SIGN_PROFILE_ADVANCED
	 * @see AOSignConstants#SIGN_PROFILE_BASELINE
	 */
	public void setProfileSet(final String profileSet) {
		this.profileSet = profileSet;
	}
}
