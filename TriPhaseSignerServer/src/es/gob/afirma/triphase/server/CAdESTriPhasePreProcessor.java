package es.gob.afirma.triphase.server;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.cades.CAdESTriPhaseSigner;
import es.gob.afirma.signers.multi.cades.CAdESTriPhaseCoSigner;
import es.gob.afirma.signers.pkcs7.ObtainContentSignedData;

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

		//TODO: Crear la alternativa para firmas explicitas (se obtienen datos nulos), en las que hay que extraer el MessageDigest
		// de la firma original
		final byte[] data = ObtainContentSignedData.obtainData(sign);

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

		final byte[] presign;
		try {
			presign = CAdESTriPhaseCoSigner.preCoSign(
					data,
					algorithm,
					new X509Certificate[] { cert },
					new AdESPolicy(extraParams),
					signingCertificateV2,
					null,                        // MessageDigest
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

		//TODO: Crear la alternativa para firmas explicitas (se obtienen datos nulos), en las que hay que extraer el Messa
		final byte[] data = ObtainContentSignedData.obtainData(sign);

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
}
