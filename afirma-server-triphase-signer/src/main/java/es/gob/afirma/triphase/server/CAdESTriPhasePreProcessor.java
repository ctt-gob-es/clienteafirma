package es.gob.afirma.triphase.server;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.cades.CAdESSignerMetadataHelper;
import es.gob.afirma.signers.cades.CAdESTriPhaseSigner;
import es.gob.afirma.signers.cades.CommitmentTypeIndicationsHelper;
import es.gob.afirma.signers.multi.cades.AOCAdESCounterSigner;
import es.gob.afirma.signers.multi.cades.CAdESTriPhaseCoSigner;
import es.gob.afirma.signers.pkcs7.ObtainContentSignedData;
import es.gob.afirma.triphase.server.cades.AOCAdESTriPhaseCounterSigner;
import es.gob.afirma.triphase.server.cades.CAdESFakePkcs1Signer;

final class CAdESTriPhasePreProcessor implements TriPhasePreProcessor {

//	/** Clave de la propiedad de firma. */
//	private static final String PROPERTY_NAME_SIGN = "SIGN"; //$NON-NLS-1$
//
//	/** Nombre de la propiedad que contiene el n&uacute;mero de firmas proporcionadas. */
//	private static final String PROPERTY_NAME_SIGN_COUNT = "SIGN_COUNT"; //$NON-NLS-1$

	/** Indica si la postfirma requiere la prefirma. */
	private static final String PROPERTY_NAME_NEED_DATA = "NEED_DATA"; //$NON-NLS-1$

	/** Indica si la postfirma requiere la prefirma. */
	private static final String PROPERTY_NAME_NEED_PRE = "NEED_PRE"; //$NON-NLS-1$

	/** Prefijo para cada prefirma. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Firma PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Firma PKCS#1 temporal. */
	private static final String PROPERTY_NAME_DUMMY_PK1 = "DPK1"; //$NON-NLS-1$

	/** Fecha de firma. */
	private static final String PARAM_DATE = "DATE"; //$NON-NLS-1$

	/** Manejador de log. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$


	@Override
	public byte[] preProcessPreSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws IOException, AOException {

		LOGGER.info("Prefirma CAdES - Firma - INICIO"); //$NON-NLS-1$

		if (data == null || data.length < 1) {
			throw new IllegalArgumentException("Los datos no pueden ser nulos"); //$NON-NLS-1$
		}

		boolean signingCertificateV2;
		if (AOSignConstants.isSHA2SignatureAlgorithm(algorithm)) {
			signingCertificateV2 = true;
		}
		else if (extraParams.containsKey("signingCertificateV2")) { //$NON-NLS-1$
			signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2")); //$NON-NLS-1$
		}
		else {
			signingCertificateV2 = !"SHA1".equals(AOSignConstants.getDigestAlgorithmName(algorithm));	 //$NON-NLS-1$
		}

		boolean omitContent = false;
		if (extraParams.containsKey("mode")) { //$NON-NLS-1$
			omitContent = "explicit".equalsIgnoreCase(extraParams.getProperty("mode")); //$NON-NLS-1$ //$NON-NLS-2$
		}

		String contentTypeOid = MimeHelper.DEFAULT_CONTENT_OID_DATA;
		String contentDescription = MimeHelper.DEFAULT_CONTENT_DESCRIPTION;

		try {
			final MimeHelper mimeHelper = new MimeHelper(data);
			contentTypeOid = MimeHelper.transformMimeTypeToOid(mimeHelper.getMimeType());
			contentDescription = mimeHelper.getDescription();
		}
		catch(final Exception e) {
			LOGGER.warning("No se ha podido determinar el tipo de los datos: " + e); //$NON-NLS-1$
		}

		final byte[] messageDigest;
		final String digestAlgorithm;
		final String precalculatedHashAlgorithm = extraParams.getProperty("precalculatedHashAlgorithm"); //$NON-NLS-1$
		if (precalculatedHashAlgorithm != null) {
			digestAlgorithm = precalculatedHashAlgorithm;
			messageDigest = data;
		}
		else {
			digestAlgorithm = AOSignConstants.getDigestAlgorithmName(algorithm);
			try {
				messageDigest = MessageDigest.getInstance(digestAlgorithm).digest(data);
			} catch (final NoSuchAlgorithmException e) {
				throw new IllegalArgumentException("Algoritmo de huella digital no soportado: " + digestAlgorithm, e); //$NON-NLS-1$
			}
		}

		LOGGER.info("Se invocan las funciones internas de prefirma CAdES"); //$NON-NLS-1$
		final byte[] presign = CAdESTriPhaseSigner.preSign(
				digestAlgorithm,
				omitContent ? null : data,
						new X509Certificate[] { cert },
						AdESPolicy.buildAdESPolicy(extraParams),
						signingCertificateV2,
						messageDigest,
						new Date(),
						false,           // PAdES Mode
						contentTypeOid,
						contentDescription,
						CommitmentTypeIndicationsHelper.getCommitmentTypeIndications(extraParams),
						CAdESSignerMetadataHelper.getCAdESSignerMetadata(extraParams)
				);

		LOGGER.info("Se prepara la respuesta de la prefirma CAdES"); //$NON-NLS-1$

		// Generamos el mensaje para la configuracion de la operacion
		final TriphaseData triphaseData = new TriphaseData(
				AOSignConstants.SIGN_FORMAT_CADES, AOSignConstants.MASSIVE_OPERATION_SIGN);

		final Map<String, String> signConfig = new HashMap<String, String>();
		signConfig.put(PROPERTY_NAME_PRESIGN, Base64.encode(presign));
		signConfig.put(PROPERTY_NAME_NEED_PRE, Boolean.TRUE.toString());
		if (!omitContent) {
			signConfig.put(PROPERTY_NAME_NEED_DATA, Boolean.TRUE.toString());
		}

		triphaseData.addSignOperation(signConfig);

		LOGGER.info("Prefirma CAdES - Firma - FIN"); //$NON-NLS-1$

		return triphaseData.toString().getBytes();
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams,
			final byte[] session) throws NoSuchAlgorithmException, AOException, IOException {

		LOGGER.info("Postfirma CAdES - Firma - INICIO"); //$NON-NLS-1$

		// Generamos el mensaje para la configuracion de la operacion
		final TriphaseData sessionData = TriphaseData.parser(session);

		boolean omitContent = false;
		if (extraParams.containsKey("mode")) { //$NON-NLS-1$
			omitContent = "explicit".equalsIgnoreCase(extraParams.getProperty("mode")); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// Cargamos la configuracion de la operacion
		if (sessionData.getSignsCount() < 1) {
			LOGGER.severe("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
			throw new AOException("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
		}

		Map<String, String> config = sessionData.getSign(0);

		LOGGER.info("Se invocan las funciones internas de postfirma CAdES"); //$NON-NLS-1$
		final byte[] signature = CAdESTriPhaseSigner.postSign(
				AOSignConstants.getDigestAlgorithmName(algorithm),
				omitContent ? null : data,
						new X509Certificate[] { cert },
						Base64.decode(config.get(PROPERTY_NAME_PKCS1_SIGN)),
						Base64.decode(config.get(PROPERTY_NAME_PRESIGN))
				);

		LOGGER.info("Postfirma CAdES - Firma - FIN"); //$NON-NLS-1$

		return signature;
	}

	@Override
	public byte[] preProcessPreCoSign(final byte[] sign,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws IOException, AOException {

		LOGGER.info("Prefirma CAdES - Cofirma - INICIO"); //$NON-NLS-1$

		if (sign == null || sign.length < 1) {
			throw new IllegalArgumentException("Las firma no puede ser nula ni vacia"); //$NON-NLS-1$
		}

		boolean signingCertificateV2;
		if (AOSignConstants.isSHA2SignatureAlgorithm(algorithm)) {
			signingCertificateV2 = true;
		}
		else if (extraParams.containsKey("signingCertificateV2")) { //$NON-NLS-1$
			signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2")); //$NON-NLS-1$
		}
		else {
			signingCertificateV2 = !"SHA1".equals(AOSignConstants.getDigestAlgorithmName(algorithm));	 //$NON-NLS-1$
		}

		byte[] messageDigest = null;
		final byte[] data = ObtainContentSignedData.obtainData(sign);
		if (data == null) {
			messageDigest = ObtainContentSignedData.obtainMessageDigest(sign, AOSignConstants.getDigestAlgorithmName(algorithm));
			if (messageDigest == null) {
				throw new AOException("No se han encontrado datos dentro de la firma ni una huella digital compatible con el algoritmo: " + algorithm); //$NON-NLS-1$
			}
		}

		String contentTypeOid = MimeHelper.DEFAULT_CONTENT_OID_DATA;
		String contentDescription = MimeHelper.DEFAULT_CONTENT_DESCRIPTION;

		if (data != null) {
			try {
				final MimeHelper mimeHelper = new MimeHelper(data);
				contentTypeOid = MimeHelper.transformMimeTypeToOid(mimeHelper.getMimeType());
				contentDescription = mimeHelper.getDescription();
			}
			catch(final Exception e) {
				LOGGER.warning("No se ha podido determinar el tipo de los datos: " + e); //$NON-NLS-1$
			}
		}

		LOGGER.info("Se invocan las funciones internas de pre-cofirma CAdES"); //$NON-NLS-1$
		final byte[] presign;
		try {
			presign = CAdESTriPhaseCoSigner.preCoSign(
					data,
					algorithm,
					new X509Certificate[] { cert },
					AdESPolicy.buildAdESPolicy(extraParams),
					signingCertificateV2,
					messageDigest,
					contentTypeOid,
					contentDescription,
					new Date(),
					CommitmentTypeIndicationsHelper.getCommitmentTypeIndications(extraParams),
					CAdESSignerMetadataHelper.getCAdESSignerMetadata(extraParams)
					);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error de codificaci\u00F3n de certificado en la pre-cofirma CAdES", e); //$NON-NLS-1$
		}
		catch (final NoSuchAlgorithmException e) {
			throw new AOException("Error de algoritmo no soportado en la pre-cofirma CAdES", e); //$NON-NLS-1$
		}

		LOGGER.info("Se prepara la respuesta de la pre-cofirma CAdES"); //$NON-NLS-1$

		// Ahora pasamos al cliente los datos de la prefirma
		final String presignB64 = Base64.encode(presign).replace("\n", "").replace("\r", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		final TriphaseData triphaseData = new TriphaseData(
				AOSignConstants.SIGN_FORMAT_CADES, AOSignConstants.MASSIVE_OPERATION_COSIGN);

		final Map<String, String> signConfig = new HashMap<String, String>();
		signConfig.put(PROPERTY_NAME_PRESIGN, presignB64);
		signConfig.put(PROPERTY_NAME_NEED_DATA, Boolean.toString(true));
		signConfig.put(PROPERTY_NAME_NEED_PRE, Boolean.toString(true));

		triphaseData.addSignOperation(signConfig);

		LOGGER.info("Prefirma CAdES - Cofirma - FIN"); //$NON-NLS-1$

		return triphaseData.toString().getBytes();
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] sign,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams,
			final byte[] session) throws NoSuchAlgorithmException, AOException, IOException {

		LOGGER.info("Postfirma CAdES - Cofirma - INICIO"); //$NON-NLS-1$

		final TriphaseData sessionData = TriphaseData.parser(session);

		byte[] messageDigest = null;
		final byte[] data = ObtainContentSignedData.obtainData(sign);
		if (data == null) {
			messageDigest = ObtainContentSignedData.obtainMessageDigest(sign, AOSignConstants.getDigestAlgorithmName(algorithm));
			if (messageDigest == null) {
				throw new AOException("No se han encontrado datos dentro de la firma ni una huella digital compatible con el algoritmo: " + algorithm); //$NON-NLS-1$
			}
		}

		// Cargamos la configuracion de la operacion
		if (sessionData.getSignsCount() < 1) {
			LOGGER.severe("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
			throw new AOException("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
		}

		Map<String, String> config = sessionData.getSign(0);

		final byte[] pk1 = Base64.decode(config.get(PROPERTY_NAME_PKCS1_SIGN));
		config.remove(PROPERTY_NAME_PKCS1_SIGN);

		final byte[] presign = Base64.decode(config.get(PROPERTY_NAME_PRESIGN));
		config.remove(PROPERTY_NAME_PRESIGN);

		LOGGER.info("Se invocan las funciones internas de post-cofirma CAdES"); //$NON-NLS-1$
		final byte[] signature;
		try {
			signature = CAdESTriPhaseCoSigner.postCoSign(
					pk1,
					presign,
					data, // Contenido
					algorithm,
					new X509Certificate[] { cert },
					sign
					);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error de codificaci\u00F3n de certificado en la post-cofirma CAdES", e); //$NON-NLS-1$
		}

		LOGGER.info("Postfirma CAdES - Cofirma - FIN"); //$NON-NLS-1$

		return signature;
	}


	@Override
	public byte[] preProcessPreCounterSign(final byte[] sign,
			                               final String algorithm,
			                               final X509Certificate cert,
			                               final Properties extraParams,
			                               final CounterSignTarget targetType) throws IOException,
			                                                                          AOException {

		LOGGER.info("Prefirma CAdES - Contrafirma - INICIO"); //$NON-NLS-1$

		return AOCAdESTriPhaseCounterSigner.preCountersign(
				sign,
				algorithm,
				targetType,
				null,
				new X509Certificate[] { cert },
				extraParams,
				new Date()
			);
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			                                final String algorithm,
			                                final X509Certificate cert,
			                                final Properties extraParams,
			                                final byte[] session,
			                                final CounterSignTarget targetType) throws NoSuchAlgorithmException,
			                                                                        AOException,
			                                                                        IOException {

		LOGGER.info("Postfirma CAdES - Contrafirma - INICIO"); //$NON-NLS-1$

		if (session == null) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos"); //$NON-NLS-1$
		}

		// Recreamos la firma
		final TriphaseData triphaseData = TriphaseData.parser(session);

		final Date date = new Date(Long.parseLong(triphaseData.getSign(0).get(PARAM_DATE)));

		byte[] newSign = new AOCAdESCounterSigner(new CAdESFakePkcs1Signer(triphaseData, false), date).countersign(
			sign,
			algorithm,
			targetType,
			null,
			null,
			new X509Certificate[] { cert },
			extraParams
		);

		// En esta contrafirma las firmas PKCS#1 son falsas, asi que vamos buscando los
		// valores reales para insertarlo, teniendo en cuenta que en esta contrafirma y
		// en la sesion se comparte el valor de los datos que se firman con PKCS#1
		for (int i = 0; i < triphaseData.getSignsCount(); i++) {

			final Map<String, String> signConfig = triphaseData.getSign(i);

			// Los datos que hay que sustituir en la firma recien creada
			final byte[] dataToReplace = Base64.decode(signConfig.get(PROPERTY_NAME_DUMMY_PK1));

			// La firma real PKCS#1
			final byte[] pkcs1Sign = Base64.decode(signConfig.get(PROPERTY_NAME_PKCS1_SIGN));

			// Reemplazamos
			newSign = searchAndReplace(newSign, dataToReplace, pkcs1Sign);
		}

		return newSign;
	}

	/**
	 * Reemplaza un subarray por otro del mismo tama&ntilde;o dentro un un array contenedor.
	 * @param source Array contenedor en el que se realiza la b&uacute;squeda.
	 * @param search SubArray que hay que sustituir.
	 * @param replace SubArray por el que se sustituye.
	 * @return Array contenedor con el reemplazo hecho.
	 */
	private static byte[] searchAndReplace(final byte[] source, final byte[] search, final byte[] replace) {
		if (search.length != replace.length) {
			return source;
		}
		int p = searchFor(source, search);
		if (p == -1) {
			throw new IllegalArgumentException("No se ha encontrado la cadena a sustituir"); //$NON-NLS-1$
		}
		final byte[] result = Arrays.copyOf(source, source.length);



		for (final byte element : replace) {
			result[p] = element;
			p++;
		}

		return result;
	}

	/**
	 * Busca un subarray dentro de otro array.
	 * @param array Array sobre el que se realiza la b&uacute;squeda.
	 * @param subArray SubArray que buscamos.
	 * @return Posici&oacute;n en la que se encuentra por primera vez o -1 si no se encuentra.
	 */
	private static int searchFor(final byte[] array, final byte[] subArray) {
		if (subArray.length > array.length) {
			return -1;
		}
		for (int i = 0; i <= (array.length - subArray.length); i++) {
			if (array[i] == subArray[0]) {
				int j;
				for (j = 1; j < subArray.length; j++) {
					if (array[i + j] != subArray[j]) {
						break;
					}
				}
				if (j == subArray.length) {
					return i;
				}
			}
		}
		return -1;
	}
}
