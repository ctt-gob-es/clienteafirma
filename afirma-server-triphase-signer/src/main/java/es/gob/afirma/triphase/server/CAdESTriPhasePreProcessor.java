package es.gob.afirma.triphase.server;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cades.CAdESTriPhaseSigner;
import es.gob.afirma.signers.multi.cades.CAdESTriPhaseCoSigner;
import es.gob.afirma.signers.multi.cades.CAdESTriPhaseCounterSigner;
import es.gob.afirma.signers.multi.cades.CAdESTriPhaseCounterSigner.CAdESPreCounterSignResult;
import es.gob.afirma.signers.pkcs7.ObtainContentSignedData;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

final class CAdESTriPhasePreProcessor implements TriPhasePreProcessor {

	/** Prefirma. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Nombre de la propiedad de los sesi&oacute;n necesarios para completar la firma. */
	private static final String PROPERTY_NAME_SESSION_DATA = "SESSION"; //$NON-NLS-1$

	/** Firma PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	@Override
	public byte[] preProcessPreSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws IOException, AOException {

		if (data == null) {
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
			Logger.getLogger("es.gob.afirma").warning("No se ha podido determinar el tipo de los datos: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}

		final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(algorithm);
		byte[] messageDigest = null;
		if (omitContent) {
			try {
				messageDigest = data != null ? MessageDigest.getInstance(digestAlgorithm).digest(data) : null;
			} catch (final NoSuchAlgorithmException e) {
				throw new IllegalArgumentException("Algoritmo de huella digital no soportado: " + digestAlgorithm, e); //$NON-NLS-1$
			}
		}

		final byte[] presign = CAdESTriPhaseSigner.preSign(
				digestAlgorithm,
				omitContent ? null : data,
						new X509Certificate[] { cert },
						new AdESPolicy(extraParams),
						signingCertificateV2,
						messageDigest,
						new Date(),
						false,           // PAdES Mode
						contentTypeOid,
						contentDescription
				);

		// Ahora pasamos al cliente los datos de la prefirma
		final StringBuilder builder = new StringBuilder();
		builder.append(PROPERTY_NAME_PRESIGN).append("=").append(Base64.encode(presign)).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
		builder.append(PROPERTY_NAME_SESSION_DATA).append("=").append(Base64.encode(presign)); //$NON-NLS-1$

		return builder.toString().getBytes();

	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws NoSuchAlgorithmException, AOException, IOException {

		boolean omitContent = false;
		if (extraParams.containsKey("mode")) { //$NON-NLS-1$
			omitContent = "explicit".equalsIgnoreCase(extraParams.getProperty("mode")); //$NON-NLS-1$ //$NON-NLS-2$
		}

		return CAdESTriPhaseSigner.postSign(
				AOSignConstants.getDigestAlgorithmName(algorithm),
				omitContent ? null : data,
						new X509Certificate[] { cert },
						Base64.decode(extraParams.getProperty(PROPERTY_NAME_PKCS1_SIGN)),
						Base64.decode(extraParams.getProperty(PROPERTY_NAME_SESSION_DATA))
				);

	}

	@Override
	public byte[] preProcessPreCoSign(final byte[] sign,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws IOException, AOException {
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
				Logger.getLogger("es.gob.afirma").warning("No se ha podido determinar el tipo de los datos: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

		final byte[] presign;
		try {
			presign = CAdESTriPhaseCoSigner.preCoSign(
					data,
					algorithm,
					new X509Certificate[] { cert },
					new AdESPolicy(extraParams),
					signingCertificateV2,
					messageDigest,
					contentTypeOid,
					contentDescription,
					new Date()
					);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error de la precofirma CAdES", e); //$NON-NLS-1$
		}
		catch (final NoSuchAlgorithmException e) {
			throw new AOException("Error de la precofirma CAdES", e); //$NON-NLS-1$
		}

		// Ahora pasamos al cliente los datos de la prefirma
		final StringBuilder builder = new StringBuilder();
		builder.append(PROPERTY_NAME_PRESIGN).append("=").append(Base64.encode(presign)).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
		builder.append(PROPERTY_NAME_SESSION_DATA).append("=").append(Base64.encode(presign)); //$NON-NLS-1$

		return builder.toString().getBytes();
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] sign,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws NoSuchAlgorithmException, AOException, IOException {

		byte[] messageDigest = null;
		final byte[] data = ObtainContentSignedData.obtainData(sign);
		if (data == null) {
			messageDigest = ObtainContentSignedData.obtainMessageDigest(sign, AOSignConstants.getDigestAlgorithmName(algorithm));
			if (messageDigest == null) {
				throw new AOException("No se han encontrado datos dentro de la firma ni una huella digital compatible con el algoritmo: " + algorithm); //$NON-NLS-1$
			}
		}

		try {
			return CAdESTriPhaseCoSigner.postCoSign(
					Base64.decode(extraParams.getProperty(PROPERTY_NAME_PKCS1_SIGN)),
					Base64.decode(extraParams.getProperty(PROPERTY_NAME_SESSION_DATA)),
					data, // Contenido
					algorithm,
					new X509Certificate[] { cert },
					sign
					);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error de la postcofirma CAdES", e); //$NON-NLS-1$
		}
	}

	@Override
	public byte[] preProcessPreCounterSign(final byte[] sign, final String algorithm,
			final X509Certificate cert, final Properties extraParams, final CounterSignTarget targetType) throws IOException, AOException {

		final Properties config = extraParams != null ? extraParams : new Properties();

		boolean signingCertificateV2;
		if (AOSignConstants.isSHA2SignatureAlgorithm(algorithm)) {
			signingCertificateV2 = true;
		}
		else if (config.containsKey("signingCertificateV2")) { //$NON-NLS-1$
			signingCertificateV2 = Boolean.parseBoolean(config.getProperty("signingCertificateV2")); //$NON-NLS-1$
		}
		else {
			signingCertificateV2 = !"SHA1".equals(AOSignConstants.getDigestAlgorithmName(algorithm));	 //$NON-NLS-1$
		}

		String contentTypeOid = MimeHelper.DEFAULT_CONTENT_OID_DATA;
		String contentDescription = MimeHelper.DEFAULT_CONTENT_DESCRIPTION;
		final byte[] data = new AOCAdESSigner().getData(sign);
		if (data != null) {
			final MimeHelper mimeHelper = new MimeHelper(data);
			contentDescription = mimeHelper.getDescription();
			contentTypeOid = MimeHelper.transformMimeTypeToOid(mimeHelper.getMimeType());
		}

		final CAdESPreCounterSignResult preSign;
		try {
			preSign = new CAdESTriPhaseCounterSigner().preCounterSign(
					new P7ContentSignerParameters(
							sign,
							algorithm
							),
							sign,
							targetType,
							null,	// La clave privada no es necesaria para la prefirma
							new X509Certificate[] { cert },
							new AdESPolicy(config),
							signingCertificateV2,
							contentTypeOid,
							contentDescription
					);
		}
		catch (final Exception e) {
			throw new AOException("Error generando la PreContrafirma CAdES: " + e, e); //$NON-NLS-1$
		}

		// Ahora pasamos al cliente los datos de la prefirma
		final StringBuilder builder = new StringBuilder();
		builder.append(PROPERTY_NAME_SESSION_DATA).append("=").append(Base64.encode(preSign.toString().getBytes())); //$NON-NLS-1$

		return builder.toString().getBytes();
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign, final String algorithm,
			final X509Certificate cert, final Properties extraParams, final CounterSignTarget targets)
					throws NoSuchAlgorithmException, AOException, IOException {

		if (!extraParams.containsKey(CAdESTriPhaseCounterSigner.KEY_PKCS1_SIGN_COUNT) ||
				!extraParams.containsKey(CAdESTriPhaseCounterSigner.KEY_SIGN)) {
			throw new AOException("La firma no contiene las claves de propiedades obligatorias"); //$NON-NLS-1$
		}

		// Ahora vamos sustitutendo los PKCS#1 en la firma
		byte[] signPlacement = Base64.decode(extraParams.getProperty(CAdESTriPhaseCounterSigner.KEY_SIGN));
		final int p1Count = Integer.parseInt(extraParams.getProperty(CAdESTriPhaseCounterSigner.KEY_PKCS1_SIGN_COUNT));
		for (int i=0;i<p1Count;i++) {
			// El PKCS#1 de verdad
			final byte[] realP1 = Base64.decode(extraParams.getProperty(CAdESTriPhaseCounterSigner.KEY_PKCS1_SIGN + '.' + Integer.toString(i)));
			// El troncho a buscar y sustituir
			final byte[] hop = new byte[CAdESTriPhaseCounterSigner.PKCS1_DEFAULT_SIZE];
			Arrays.fill(hop, (byte) Integer.toString(i).toCharArray()[0]);
			signPlacement = searchAndReplace(signPlacement, hop, realP1);
		}

		return signPlacement;
	}

	private static byte[] searchAndReplace(final byte[] array, final byte[] search, final byte[] replace) {
		if (search.length != replace.length) {
			return array;
		}
		int p = searchFor(array, search);
		if (p == -1) {
			throw new IllegalArgumentException("No se ha encontrado la cadena a sustituir"); //$NON-NLS-1$
		}
		final byte[] result = Arrays.copyOf(array, array.length);
		for (final byte element : replace) {
			result[p] = element;
			p++;
		}
		return result;
	}

	private static int searchFor(final byte[] array, final byte[] subArray) {
		if (subArray.length > array.length) {
			return -1;
		}
		final int p = new String(array).indexOf(new String(subArray));
		for (int i = 1; i < subArray.length; i++) {
			if (array[p + i] != subArray[i]) {
				return -1;
			}
		}
		return p;
	}
}
