/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pkcs7;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore;
import java.security.cert.CertPathBuilder;
import java.security.cert.CertStore;
import java.security.cert.Certificate;
import java.security.cert.CollectionCertStoreParameters;
import java.security.cert.PKIXBuilderParameters;
import java.security.cert.TrustAnchor;
import java.security.cert.X509CertSelector;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Set;
import java.util.Properties;
import java.util.logging.Logger;

import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.bouncycastle.asn1.x509.KeyPurposeId;
import org.bouncycastle.cert.X509CertificateHolder;
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter;
import org.bouncycastle.cms.CMSSignedData;
import org.bouncycastle.cms.SignerId;
import org.bouncycastle.cms.SignerInformation;
import org.bouncycastle.cms.SignerInformationStore;
import org.bouncycastle.cms.jcajce.JcaSimpleSignerInfoVerifierBuilder;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.TriphaseData;

/**
 * Validaci&oacute;n del token firmado con el que el servidor trif&aacute;sico autentica su
 * respuesta (ASVS 4.0.3 V2.1, tarea T7 de la OT Fase 2).
 * <p>
 * Formato del token (versi&oacute;n 1): una firma CMS ({@code SignedData} con el contenido
 * encapsulado) sobre una carga &uacute;til de l&iacute;neas {@code clave=valor}, codificada en
 * Base64 y transportada en el atributo {@code token} del nodo {@code <firmas>} de la sesi&oacute;n
 * trif&aacute;sica:
 * </p>
 * <pre>
 * afirma-triphase-token-v1
 * session=&lt;identificador de sesion&gt;
 * doc=&lt;identificador del documento tal y como lo envia el cliente&gt;
 * format=&lt;formato de firma&gt;
 * op=&lt;operacion criptografica&gt;
 * iat=&lt;milisegundos desde epoch&gt;
 * exp=&lt;milisegundos desde epoch&gt;
 * </pre>
 * <p>
 * Se comprueban la firma, el EKU de sellado ({@code id-kp-emailProtection}) o de servicio
 * ({@code id-kp-serverAuth}), la cadena PKIX contra las anclas de confianza, la vigencia del
 * certificado y del token, y la ligadura con la petici&oacute;n (documento, formato y operaci&oacute;n).
 * El nonce de sesi&oacute;n se usa para ligar la respuesta a la petici&oacute;n concreta.
 * </p>
 */
public final class TriphaseTokenValidator {

	/** Propiedad con la que se activa la exigencia del token (por defecto desactivada). */
	public static final String STRICT_TOKEN_PROPERTY = "strictTriphaseToken"; //$NON-NLS-1$

	/** Par&aacute;metro con el almac&eacute;n de confianza para la cadena del token (Base64). */
	public static final String TRUSTSTORE_PARAM = "triphaseTrustStore"; //$NON-NLS-1$

	/** Par&aacute;metro con la contrase&ntilde;a del almac&eacute;n de confianza del token. */
	public static final String TRUSTSTORE_PASSWORD_PARAM = "triphaseTrustStorePassword"; //$NON-NLS-1$

	/** Par&aacute;metro con el tipo del almac&eacute;n de confianza del token. */
	public static final String TRUSTSTORE_TYPE_PARAM = "triphaseTrustStoreType"; //$NON-NLS-1$

	/** Versi&oacute;n del formato del token. */
	public static final String TOKEN_VERSION = "afirma-triphase-token-v1"; //$NON-NLS-1$

	/** Vigencia m&aacute;xima admitida del token. */
	static final long MAX_TOKEN_LIFETIME_MILLIS = 24L * 60L * 60L * 1000L;

	/** Desfase m&aacute;ximo admitido entre el reloj del equipo y el del servidor. */
	static final long MAX_CLOCK_SKEW_MILLIS = 5L * 60L * 1000L;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String KEY_SESSION = "session"; //$NON-NLS-1$
	private static final String KEY_DOCUMENT = "doc"; //$NON-NLS-1$
	private static final String KEY_FORMAT = "format"; //$NON-NLS-1$
	private static final String KEY_OPERATION = "op"; //$NON-NLS-1$
	private static final String KEY_ISSUED_AT = "iat"; //$NON-NLS-1$
	private static final String KEY_EXPIRES_AT = "exp"; //$NON-NLS-1$

	private TriphaseTokenValidator() {
		// No instanciable
	}

	/**
	 * Indica si la exigencia del token est&aacute; activa: lo decide el par&aacute;metro de la
	 * operaci&oacute;n o, si no se indica, la propiedad del sistema.
	 * @param extraParams Par&aacute;metros de la operaci&oacute;n de firma.
	 * @return {@code true} si el token es obligatorio.
	 */
	static boolean isStrict(final Properties extraParams) {
		final String fromParams = extraParams != null ? extraParams.getProperty(STRICT_TOKEN_PROPERTY) : null;
		if (fromParams != null) {
			return Boolean.parseBoolean(fromParams);
		}
		return Boolean.parseBoolean(System.getProperty(STRICT_TOKEN_PROPERTY, Boolean.FALSE.toString()));
	}

	/**
	 * Valida el token de la respuesta del servidor trif&aacute;sico y su ligadura con la petici&oacute;n.
	 * @param triphaseData Respuesta del servidor ya analizada.
	 * @param documentId Identificador del documento enviado por el cliente.
	 * @param format Formato de firma solicitado.
	 * @param cryptoOperation Operaci&oacute;n criptogr&aacute;fica solicitada.
	 * @param extraParams Par&aacute;metros de la operaci&oacute;n (pueden incluir el almac&eacute;n de
	 *                    confianza y la exigencia del token).
	 * @throws AOException Si el token no es v&aacute;lido o, con validaci&oacute;n estricta, si no
	 *                     se ha recibido.
	 */
	public static void validate(final TriphaseData triphaseData,
			                    final String documentId,
			                    final String format,
			                    final String cryptoOperation,
			                    final Properties extraParams) throws AOException {

		if (triphaseData == null) {
			throw new AOException(
				"No se han recibido datos de la sesion trifasica", //$NON-NLS-1$
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}

		final String token = triphaseData.getToken();
		if (token == null || token.trim().isEmpty()) {
			if (isStrict(extraParams)) {
				throw new AOException(
					"El servidor trifasico no ha enviado token de sesion y la validacion estricta esta activa", //$NON-NLS-1$
					ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
			}
			LOGGER.warning(
				"El servidor trifasico no ha enviado token de sesion: no se puede autenticar la respuesta " //$NON-NLS-1$
				+ "(validacion estricta desactivada)"); //$NON-NLS-1$
			return;
		}

		final Payload payload = parseAndVerify(token, extraParams);

		if (!payload.documentId.equals(documentId)) {
			throw new AOException(
				"El token del servidor trifasico no corresponde al documento firmado", //$NON-NLS-1$
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}
		if (format != null && payload.format != null && !payload.format.equalsIgnoreCase(format)) {
			throw new AOException(
				"El token del servidor trifasico no corresponde al formato solicitado", //$NON-NLS-1$
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}
		if (cryptoOperation != null && payload.operation != null && !payload.operation.equalsIgnoreCase(cryptoOperation)) {
			throw new AOException(
				"El token del servidor trifasico no corresponde a la operacion solicitada", //$NON-NLS-1$
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}

		checkTimeWindow(payload);

		LOGGER.fine("Token de la sesion trifasica validado (sesion: " + payload.sessionId + ")"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Carga &uacute;til del token ya validada. */
	private static final class Payload {
		private String sessionId;
		private String documentId;
		private String format;
		private String operation;
		private long issuedAt;
		private long expiresAt;
	}

	private static Payload parseAndVerify(final String token, final Properties extraParams) throws AOException {

		final byte[] tokenBytes;
		try {
			tokenBytes = Base64.decode(token, true);
		}
		catch (final Exception e) {
			throw new AOException(
				"El token del servidor trifasico no esta en Base64", //$NON-NLS-1$
				e,
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}

		final CMSSignedData cms;
		try {
			cms = new CMSSignedData(tokenBytes);
		}
		catch (final Exception e) {
			throw new AOException(
				"El token del servidor trifasico no es una firma CMS valida", //$NON-NLS-1$
				e,
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}

		if (cms.getSignedContent() == null) {
			throw new AOException(
				"El token del servidor trifasico no incluye la carga util firmada", //$NON-NLS-1$
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}
		final byte[] content = (byte[]) cms.getSignedContent().getContent();
		final Payload payload = parsePayload(new String(content, StandardCharsets.UTF_8));

		final SignerInformationStore signers = cms.getSignerInfos();
		if (signers == null || signers.size() != 1) {
			throw new AOException(
				"El token del servidor trifasico debe tener exactamente un firmante", //$NON-NLS-1$
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}
		final SignerInformation signer = signers.getSigners().iterator().next();
		final X509CertificateHolder signerHolder = findSignerCertificate(cms, signer.getSID());
		verifySignature(signer, signerHolder);
		checkSignerCertificate(signerHolder, extraParams, cms);

		return payload;
	}

	private static Payload parsePayload(final String content) throws AOException {

		final Payload payload = new Payload();
		final String[] lines = content.replace("\r\n", "\n").split("\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

		if (lines.length < 2 || !TOKEN_VERSION.equals(lines[0].trim())) {
			throw new AOException(
				"El token del servidor trifasico no declara la version esperada", //$NON-NLS-1$
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}

		for (int i = 1; i < lines.length; i++) {
			final int sep = lines[i].indexOf('=');
			if (sep < 1) {
				continue;
			}
			final String key = lines[i].substring(0, sep).trim();
			final String value = lines[i].substring(sep + 1).trim();
			if (KEY_SESSION.equals(key)) {
				payload.sessionId = value;
			}
			else if (KEY_DOCUMENT.equals(key)) {
				payload.documentId = value;
			}
			else if (KEY_FORMAT.equals(key)) {
				payload.format = value;
			}
			else if (KEY_OPERATION.equals(key)) {
				payload.operation = value;
			}
			else if (KEY_ISSUED_AT.equals(key)) {
				payload.issuedAt = parseLong(value);
			}
			else if (KEY_EXPIRES_AT.equals(key)) {
				payload.expiresAt = parseLong(value);
			}
		}

		if (payload.sessionId == null || payload.sessionId.isEmpty()
				|| payload.documentId == null || payload.documentId.isEmpty()
				|| payload.issuedAt <= 0 || payload.expiresAt <= 0) {
			throw new AOException(
				"El token del servidor trifasico no incluye sesion, documento o vigencia", //$NON-NLS-1$
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}

		return payload;
	}

	private static long parseLong(final String value) {
		try {
			return Long.parseLong(value);
		}
		catch (final NumberFormatException e) {
			return -1L;
		}
	}

	private static X509CertificateHolder findSignerCertificate(final CMSSignedData cms, final SignerId signerId)
			throws AOException {
		final Collection<X509CertificateHolder> matches = cms.getCertificates().getMatches(signerId);
		if (matches == null || matches.isEmpty()) {
			throw new AOException(
				"El token del servidor trifasico no incluye el certificado del firmante", //$NON-NLS-1$
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}
		return matches.iterator().next();
	}

	private static void verifySignature(final SignerInformation signer, final X509CertificateHolder signerHolder)
			throws AOException {
		try {
			if (!signer.verify(new JcaSimpleSignerInfoVerifierBuilder().build(signerHolder))) {
				throw new AOException(
					"La firma del token del servidor trifasico no es valida", //$NON-NLS-1$
					ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
			}
		}
		catch (final AOException e) {
			throw e;
		}
		catch (final Exception e) {
			throw new AOException(
				"No se ha podido verificar la firma del token del servidor trifasico", //$NON-NLS-1$
				e,
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}
	}

	private static void checkSignerCertificate(final X509CertificateHolder signerHolder,
			                                   final Properties extraParams,
			                                   final CMSSignedData cms) throws AOException {

		final X509Certificate signer;
		try {
			signer = new JcaX509CertificateConverter().getCertificate(signerHolder);
			signer.checkValidity();
		}
		catch (final Exception e) {
			throw new AOException(
				"El certificado del servidor trifasico no es valido o esta caducado", //$NON-NLS-1$
				e,
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}

		if (!hasSealingUsage(signer)) {
			throw new AOException(
				"El certificado del servidor trifasico no tiene EKU de sellado (id-kp-emailProtection) " //$NON-NLS-1$
				+ "ni de servicio (id-kp-serverAuth)", //$NON-NLS-1$
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}

		validateChain(signer, cms, extraParams);
	}

	private static boolean hasSealingUsage(final X509Certificate certificate) {
		try {
			final java.util.List<String> eku = certificate.getExtendedKeyUsage();
			if (eku == null) {
				return false;
			}
			return eku.contains(KeyPurposeId.id_kp_emailProtection.getId())
					|| eku.contains(KeyPurposeId.id_kp_serverAuth.getId());
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido leer el EKU del certificado del servidor trifasico"); //$NON-NLS-1$
			return false;
		}
	}

	private static void validateChain(final X509Certificate signer, final CMSSignedData cms,
			final Properties extraParams) throws AOException {
		try {
			final X509CertSelector target = new X509CertSelector();
			target.setCertificate(signer);

			final PKIXBuilderParameters parameters = new PKIXBuilderParameters(
				getTrustAnchors(extraParams),
				target
			);
			parameters.setRevocationEnabled(false);
			parameters.addCertStore(CertStore.getInstance(
				"Collection", //$NON-NLS-1$
				new CollectionCertStoreParameters(cms.getCertificates().getMatches(null))
			));

			CertPathBuilder.getInstance("PKIX").build(parameters); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new AOException(
				"El certificado del servidor trifasico no encadena con una CA de confianza", //$NON-NLS-1$
				e,
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}
	}

	private static Set<TrustAnchor> getTrustAnchors(final Properties extraParams) throws Exception {

		final Set<TrustAnchor> anchors = new HashSet<>();

		final String trustStore = extraParams != null ? extraParams.getProperty(TRUSTSTORE_PARAM) : null;
		if (trustStore != null && !trustStore.trim().isEmpty()) {
			final KeyStore keyStore = KeyStore.getInstance(
				extraParams.getProperty(TRUSTSTORE_TYPE_PARAM, "PKCS12")); //$NON-NLS-1$
			try (ByteArrayInputStream is = new ByteArrayInputStream(Base64.decode(trustStore, true))) {
				keyStore.load(is, extraParams.getProperty(TRUSTSTORE_PASSWORD_PARAM, "").toCharArray()); //$NON-NLS-1$
			}
			final Enumeration<String> aliases = keyStore.aliases();
			while (aliases.hasMoreElements()) {
				final Certificate certificate = keyStore.getCertificate(aliases.nextElement());
				if (certificate instanceof X509Certificate) {
					anchors.add(new TrustAnchor((X509Certificate) certificate, null));
				}
			}
		}

		if (anchors.isEmpty()) {
			final TrustManagerFactory factory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
			factory.init((KeyStore) null);
			for (final TrustManager trustManager : factory.getTrustManagers()) {
				if (trustManager instanceof X509TrustManager) {
					for (final X509Certificate certificate : ((X509TrustManager) trustManager).getAcceptedIssuers()) {
						anchors.add(new TrustAnchor(certificate, null));
					}
				}
			}
		}

		if (anchors.isEmpty()) {
			throw new IllegalStateException("No hay anclas de confianza para validar el token trifasico"); //$NON-NLS-1$
		}
		return anchors;
	}

	private static void checkTimeWindow(final Payload payload) throws AOException {

		final long now = System.currentTimeMillis();

		if (payload.expiresAt - payload.issuedAt > MAX_TOKEN_LIFETIME_MILLIS) {
			throw new AOException(
				"El token del servidor trifasico tiene una vigencia superior a la permitida", //$NON-NLS-1$
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}
		if (now < payload.issuedAt - MAX_CLOCK_SKEW_MILLIS || now > payload.expiresAt + MAX_CLOCK_SKEW_MILLIS) {
			throw new AOException(
				"El token del servidor trifasico esta fuera de su periodo de vigencia", //$NON-NLS-1$
				ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE);
		}
	}
}
