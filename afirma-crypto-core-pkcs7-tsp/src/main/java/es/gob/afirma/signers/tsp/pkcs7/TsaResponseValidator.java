/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.tsp.pkcs7;

import java.io.ByteArrayInputStream;
import java.security.KeyStore;
import java.security.cert.CertPathBuilder;
import java.security.cert.CertStore;
import java.security.cert.Certificate;
import java.security.cert.PKIXBuilderParameters;
import java.security.cert.TrustAnchor;
import java.security.cert.X509CertSelector;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.bouncycastle.asn1.x509.KeyPurposeId;
import org.bouncycastle.cert.X509CertificateHolder;
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter;
import org.bouncycastle.cms.SignerId;
import org.bouncycastle.cms.jcajce.JcaSimpleSignerInfoVerifierBuilder;
import org.bouncycastle.tsp.TimeStampToken;

import es.gob.afirma.core.AOException;
import es.gob.afirma.signers.pkcs7.BinaryErrorCode;

/**
 * Validaci&oacute;n de la respuesta de una TSA (RFC 3161) m&aacute;s all&aacute; del estado
 * PKI: firma del sello, EKU de sellado de tiempo, cadena de confianza (en modo estricto) y
 * hora razonable. El nonce y la huella (imprint) los comprueba
 * {@link org.bouncycastle.tsp.TimeStampResponse#validate(org.bouncycastle.tsp.TimeStampRequest)}.
 * <p>
 * Tarea T6 de la OT Fase 2 (ASVS 4.0.3 V9).
 * </p>
 */
final class TsaResponseValidator {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Desfase m&aacute;ximo admitido entre la hora del sello y la del equipo. */
	static final long MAX_CLOCK_SKEW_MILLIS = 24L * 60L * 60L * 1000L;

	private TsaResponseValidator() {
		// No instanciable
	}

	/**
	 * Valida el sello de tiempo de una respuesta concedida.
	 * @param token Sello de tiempo devuelto por la TSA.
	 * @param trustStore Almac&eacute;n con las CA de confianza para la TSA (puede ser {@code null}
	 *                   para usar el almac&eacute;n de confianza del sistema).
	 * @param strict Indica si deben aplicarse las comprobaciones estrictas (cadena PKIX).
	 * @throws AOException Si el sello no es v&aacute;lido.
	 */
	static void validate(final TimeStampToken token, final KeyStore trustStore, final boolean strict) throws AOException {

		if (token == null) {
			throw new AOException(
				"La respuesta de la TSA no contiene un sello de tiempo", //$NON-NLS-1$
				BinaryErrorCode.Internal.GENERATING_TIMESTAMP_ERROR);
		}

		final X509CertificateHolder signerHolder = findSignerCertificate(token);
		verifySignature(token, signerHolder);
		checkTimeStampingUsage(signerHolder, strict);

		if (strict) {
			validateChain(token, signerHolder, trustStore);
		}

		checkReasonableTime(token);
	}

	/**
	 * Carga un almac&eacute;n PKCS#12/JKS a partir de su contenido.
	 * @param content Contenido del almac&eacute;n.
	 * @param password Contrase&ntilde;a.
	 * @param type Tipo de almac&eacute;n (por defecto PKCS#12).
	 * @return Almac&eacute;n cargado.
	 * @throws AOException Si no se puede cargar.
	 */
	static KeyStore loadTrustStore(final byte[] content, final String password, final String type) throws AOException {
		try {
			final KeyStore keyStore = KeyStore.getInstance(type != null ? type : "PKCS12"); //$NON-NLS-1$
			try (ByteArrayInputStream is = new ByteArrayInputStream(content)) {
				keyStore.load(is, password != null ? password.toCharArray() : new char[0]);
			}
			return keyStore;
		}
		catch (final Exception e) {
			throw new AOException(
				"No se ha podido cargar el almacen de confianza de la TSA: " + e, //$NON-NLS-1$
				e,
				BinaryErrorCode.Internal.GENERATING_TIMESTAMP_ERROR);
		}
	}

	private static X509CertificateHolder findSignerCertificate(final TimeStampToken token) throws AOException {

		final SignerId signerId = token.getSID();
		final Collection<X509CertificateHolder> matches = token.getCertificates().getMatches(signerId);
		if (matches == null || matches.isEmpty()) {
			throw new AOException(
				"El sello de tiempo no incluye el certificado del firmante: no se puede verificar la firma", //$NON-NLS-1$
				BinaryErrorCode.Internal.GENERATING_TIMESTAMP_ERROR);
		}
		return matches.iterator().next();
	}

	private static void verifySignature(final TimeStampToken token, final X509CertificateHolder signerHolder)
			throws AOException {
		try {
			if (!token.isSignatureValid(new JcaSimpleSignerInfoVerifierBuilder().build(signerHolder))) {
				throw new AOException(
					"La firma del sello de tiempo no es valida", //$NON-NLS-1$
					BinaryErrorCode.Internal.GENERATING_TIMESTAMP_ERROR);
			}
		}
		catch (final AOException e) {
			throw e;
		}
		catch (final Exception e) {
			throw new AOException(
				"No se ha podido verificar la firma del sello de tiempo: " + e, //$NON-NLS-1$
				e,
				BinaryErrorCode.Internal.GENERATING_TIMESTAMP_ERROR);
		}
	}

	/**
	 * Comprueba que el certificado firmante puede sellar tiempo: EKU
	 * <code>id-kp-timeStamping</code> y, si declara usos de clave, firma o no repudio.
	 */
	private static void checkTimeStampingUsage(final X509CertificateHolder signerHolder, final boolean strict)
			throws AOException {

		final X509Certificate certificate;
		try {
			certificate = new JcaX509CertificateConverter().getCertificate(signerHolder);
		}
		catch (final Exception e) {
			throw new AOException(
				"No se ha podido leer el certificado de la TSA: " + e, //$NON-NLS-1$
				e,
				BinaryErrorCode.Internal.GENERATING_TIMESTAMP_ERROR);
		}

		boolean hasTimeStamping = false;
		try {
			final java.util.List<String> extendedKeyUsage = certificate.getExtendedKeyUsage();
			if (extendedKeyUsage != null) {
				hasTimeStamping = extendedKeyUsage.contains(KeyPurposeId.id_kp_timeStamping.getId());
			}
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido leer el EKU del certificado de la TSA: " + e); //$NON-NLS-1$
		}

		if (!hasTimeStamping) {
			throw new AOException(
				"El certificado de la TSA no incluye el EKU de sellado de tiempo (id-kp-timeStamping): sello rechazado", //$NON-NLS-1$
				BinaryErrorCode.Internal.GENERATING_TIMESTAMP_ERROR);
		}

		final boolean[] keyUsage = certificate.getKeyUsage();
		if (keyUsage != null
				&& !(keyUsage.length > 0 && keyUsage[0])
				&& !(keyUsage.length > 1 && keyUsage[1])) {
			throw new AOException(
				"El certificado de la TSA no permite firmar (keyUsage): sello rechazado", //$NON-NLS-1$
				BinaryErrorCode.Internal.GENERATING_TIMESTAMP_ERROR);
		}

		if (!strict) {
			LOGGER.fine("Validacion de la cadena de la TSA omitida (strictSslChecks=false)"); //$NON-NLS-1$
		}
	}

	/** Construye y valida la cadena del certificado de la TSA contra las anclas de confianza.
	 * La revocaci&oacute;n queda desactivada: el cliente no consulta CRL/OCSP en el sellado. */
	private static void validateChain(final TimeStampToken token, final X509CertificateHolder signerHolder,
			final KeyStore trustStore) throws AOException {

		try {
			final X509Certificate signer = new JcaX509CertificateConverter().getCertificate(signerHolder);
			final X509CertSelector target = new X509CertSelector();
			target.setCertificate(signer);

			final PKIXBuilderParameters parameters = new PKIXBuilderParameters(
				getTrustAnchors(trustStore),
				target
			);
			parameters.setRevocationEnabled(false);
			parameters.addCertStore(CertStore.getInstance(
				"Collection", //$NON-NLS-1$
				new java.security.cert.CollectionCertStoreParameters(token.getCertificates().getMatches(null))
			));

			CertPathBuilder.getInstance("PKIX").build(parameters); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new AOException(
				"El certificado de la TSA no encadena con una CA de confianza: sello rechazado", //$NON-NLS-1$
				e,
				BinaryErrorCode.Internal.GENERATING_TIMESTAMP_ERROR);
		}
	}

	private static Set<TrustAnchor> getTrustAnchors(final KeyStore trustStore) throws Exception {

		final Set<TrustAnchor> anchors = new HashSet<>();
		if (trustStore != null) {
			final java.util.Enumeration<String> aliases = trustStore.aliases();
			while (aliases.hasMoreElements()) {
				final Certificate certificate = trustStore.getCertificate(aliases.nextElement());
				if (certificate instanceof X509Certificate) {
					anchors.add(new TrustAnchor((X509Certificate) certificate, null));
				}
			}
		}

		if (anchors.isEmpty()) {
			// Anclas del sistema: los emisores aceptados por el trust manager por defecto
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
			throw new IllegalStateException("No hay anclas de confianza para validar la TSA"); //$NON-NLS-1$
		}
		return anchors;
	}

	/** Comprueba que la hora del sello no se aleja de la del equipo m&aacute;s de lo razonable. */
	private static void checkReasonableTime(final TimeStampToken token) throws AOException {

		final Date genTime = token.getTimeStampInfo().getGenTime();
		if (genTime == null) {
			throw new AOException(
				"El sello de tiempo no incluye la hora de generacion", //$NON-NLS-1$
				BinaryErrorCode.Internal.GENERATING_TIMESTAMP_ERROR);
		}

		final long skew = Math.abs(System.currentTimeMillis() - genTime.getTime());
		if (skew > MAX_CLOCK_SKEW_MILLIS) {
			throw new AOException(
				"La hora del sello de tiempo no es razonable: se desvia mas de 24 horas de la hora del equipo", //$NON-NLS-1$
				BinaryErrorCode.Internal.GENERATING_TIMESTAMP_ERROR);
		}
	}
}
