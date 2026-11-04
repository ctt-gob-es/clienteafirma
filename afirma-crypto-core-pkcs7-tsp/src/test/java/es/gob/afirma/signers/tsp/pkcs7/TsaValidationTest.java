/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.tsp.pkcs7;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.nio.charset.StandardCharsets;
import java.security.KeyStore;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Base64;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Properties;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import org.bouncycastle.asn1.nist.NISTObjectIdentifiers;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.http.UrlHttpManagerImpl;

/**
 * Pruebas de la validaci&oacute;n de la respuesta de una TSA (RFC 3161) y del canal TLS, con
 * una TSA sint&eacute;tica local y sin red externa. Tarea T6 de la OT Fase 2 (ASVS 4.0.3 V9).
 */
public class TsaValidationTest {

	private static final String POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$
	private static final String SIGNATURE = "firma sintetica de prueba"; //$NON-NLS-1$

	private SyntheticTsa tsa;
	private CapturingHandler logHandler;

	@Before
	public void setUp() throws Exception {
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS, Boolean.TRUE.toString());
		this.tsa = SyntheticTsa.start();
		this.logHandler = new CapturingHandler();
		Logger.getLogger("es.gob.afirma").addHandler(this.logHandler); //$NON-NLS-1$
	}

	@After
	public void tearDown() {
		Logger.getLogger("es.gob.afirma").removeHandler(this.logHandler); //$NON-NLS-1$
		System.clearProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS);
		if (this.tsa != null) {
			this.tsa.close();
		}
	}

	/** Respuesta v&aacute;lida con TLS de confianza y la CA de la TSA anclada. */
	@Test
	public void validResponseIsAccepted() throws Exception {

		final byte[] token = getToken(this.tsa.getHttpsUrl(), this.tsa.getTrustStoreBytes());
		assertNotNull(token);
		assertTrue(token.length > 0);

		// La peticion lleva nonce aleatorio para que la TSA lo devuelva
		assertNotNull("La peticion debe incluir un nonce", this.tsa.getLastRequest().getNonce()); //$NON-NLS-1$
	}

	/** Respuesta con un nonce distinto del solicitado. */
	@Test
	public void wrongNonceIsRejected() throws Exception {
		this.tsa.setBehaviour(SyntheticTsa.Behaviour.WRONG_NONCE);
		assertRejected(this.tsa.getHttpsUrl(), this.tsa.getTrustStoreBytes(), "nonce"); //$NON-NLS-1$
	}

	/** Respuesta con una huella (imprint) distinta de la solicitada. */
	@Test
	public void wrongImprintIsRejected() throws Exception {
		this.tsa.setBehaviour(SyntheticTsa.Behaviour.WRONG_IMPRINT);
		assertRejected(this.tsa.getHttpsUrl(), this.tsa.getTrustStoreBytes(), "imprint"); //$NON-NLS-1$
	}

	/** Certificado de la TSA sin EKU de sellado de tiempo. */
	@Test
	public void certificateWithoutTimeStampingEkuIsRejected() throws Exception {
		this.tsa.setBehaviour(SyntheticTsa.Behaviour.NO_TIME_STAMPING_EKU);
		assertRejected(this.tsa.getHttpsUrl(), this.tsa.getTrustStoreBytes(), "EKU"); //$NON-NLS-1$
	}

	/** Sello sin los certificados del firmante: no se puede verificar la firma. */
	@Test
	public void tokenWithoutCertificatesIsRejected() throws Exception {
		this.tsa.setBehaviour(SyntheticTsa.Behaviour.NO_CERTIFICATES);
		assertRejected(this.tsa.getHttpsUrl(), this.tsa.getTrustStoreBytes(), "certificado"); //$NON-NLS-1$
	}

	/** El certificado de la TSA no encadena con ninguna CA de confianza. */
	@Test
	public void tsaCertificateNotAnchoredIsRejected() throws Exception {
		// Truststore con solo el certificado del servidor TLS: la cadena de la TSA no ancla
		final byte[] tlsOnlyTrustStore = tlsOnlyTrustStore();
		assertRejected(this.tsa.getHttpsUrl(), tlsOnlyTrustStore, "cadena"); //$NON-NLS-1$
	}

	/** Certificado del servidor TLS no confiable con comprobaciones estrictas. */
	@Test
	public void untrustedTlsIsRejectedInStrictMode() throws Exception {
		this.tsa.setBehaviour(SyntheticTsa.Behaviour.VALID);
		assertRejected(this.tsa.getHttpsUrl(), null, null);
	}

	/** TSA por HTTP en claro con comprobaciones estrictas. */
	@Test
	public void cleartextTsaIsRejectedInStrictMode() throws Exception {
		this.tsa.setBehaviour(SyntheticTsa.Behaviour.VALID);
		assertRejected(this.tsa.getHttpUrl(), null, "en claro"); //$NON-NLS-1$
	}

	/** En modo permisivo la TSA por HTTP funciona y deja aviso. */
	@Test
	public void cleartextTsaIsAllowedInPermissiveModeWithWarning() throws Exception {
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS, Boolean.FALSE.toString());
		this.tsa.setBehaviour(SyntheticTsa.Behaviour.VALID);
		final byte[] token = getToken(this.tsa.getHttpUrl(), null);
		assertNotNull(token);
		assertTrue("Debe avisarse de la conexion sin cifrar",
			this.logHandler.contains(Level.WARNING, "sin cifrar")); //$NON-NLS-1$
	}

	/** Sin algoritmo indicado se solicita SHA-256, nunca SHA-1. */
	@Test
	public void defaultHashAlgorithmIsSha256() throws Exception {
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS, Boolean.FALSE.toString());
		final CMSTimestamper timestamper = new CMSTimestamper(new TsaParams(params(this.tsa.getHttpUrl(), null)));
		timestamper.getTimeStampToken(digest(), null, new GregorianCalendar());
		assertEquals(
			NISTObjectIdentifiers.id_sha256.getId(),
			this.tsa.getLastRequest().getMessageImprintAlgOID().getId()
		);
	}

	/** SHA-1 explícito se rechaza con comprobaciones estrictas. */
	@Test
	public void sha1IsRejectedInStrictMode() throws Exception {
		final CMSTimestamper timestamper = new CMSTimestamper(new TsaParams(params(this.tsa.getHttpsUrl(), this.tsa.getTrustStoreBytes())));
		try {
			timestamper.getTimeStampToken(digest(), "SHA-1", new GregorianCalendar()); //$NON-NLS-1$
			fail("SHA-1 debe rechazarse con strictSslChecks activo"); //$NON-NLS-1$
		}
		catch (final IllegalArgumentException expected) {
			assertTrue(expected.getMessage().contains("SHA-1")); //$NON-NLS-1$
		}
	}

	/** Hora del sello alejada de la del equipo más de lo razonable. */
	@Test
	public void unreasonableTimeIsRejected() throws Exception {
		this.tsa.setBehaviour(SyntheticTsa.Behaviour.FUTURE_TIME);
		assertRejected(this.tsa.getHttpsUrl(), this.tsa.getTrustStoreBytes(), "hora"); //$NON-NLS-1$
	}

	/** Los avisos no incluyen la URL con la ruta de la TSA. */
	@Test
	public void warningsDoNotIncludeTsaPath() throws Exception {
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS, Boolean.FALSE.toString());
		getToken(this.tsa.getHttpUrl(), null);
		assertFalse("El aviso no debe incluir la ruta de la TSA",
			this.logHandler.containsAny("/tsa", "?token=", "12345678Z")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	private byte[] getToken(final String url, final byte[] trustStore) throws Exception {
		return new CMSTimestamper(new TsaParams(params(url, trustStore)))
			.getTimeStampToken(digest(), "SHA-256", new GregorianCalendar()); //$NON-NLS-1$
	}

	private void assertRejected(final String url, final byte[] trustStore, final String expectedInMessage) {
		try {
			getToken(url, trustStore);
			fail("La respuesta de la TSA debe rechazarse"); //$NON-NLS-1$
		}
		catch (final AOException e) {
			if (expectedInMessage != null) {
				assertNotNull(e.getMessage());
				assertTrue(
					"Mensaje inesperado: " + e.getMessage(), //$NON-NLS-1$
					e.getMessage().toLowerCase().contains(expectedInMessage.toLowerCase()));
			}
		}
		catch (final Exception e) {
			if (expectedInMessage != null) {
				assertTrue(
					"Excepcion inesperada: " + e, //$NON-NLS-1$
					String.valueOf(e.getMessage()).toLowerCase().contains(expectedInMessage.toLowerCase()));
			}
		}
	}

	private byte[] tlsOnlyTrustStore() throws Exception {
		final KeyStore trustStore = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		trustStore.load(null, null);
		trustStore.setCertificateEntry("tls", this.tsa.getPki().tlsCertificate); //$NON-NLS-1$
		return SyntheticTsa.store(trustStore);
	}

	private static Properties params(final String url, final byte[] trustStore) {
		final Properties properties = new Properties();
		properties.setProperty("tsaURL", url); //$NON-NLS-1$
		properties.setProperty("tsaPolicy", POLICY); //$NON-NLS-1$
		if (trustStore != null) {
			properties.setProperty("tsaSslTrustStore", Base64.getEncoder().encodeToString(trustStore)); //$NON-NLS-1$
			properties.setProperty("tsaSslTrustStorePassword", "changeit"); //$NON-NLS-1$ //$NON-NLS-2$
			properties.setProperty("tsaSslTrustStoreType", "PKCS12"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return properties;
	}

	private static byte[] digest() throws Exception {
		return MessageDigest.getInstance("SHA-256").digest(SIGNATURE.getBytes(StandardCharsets.UTF_8)); //$NON-NLS-1$
	}

	/** Recoge los avisos de la aplicacion para poder comprobarlos. */
	private static final class CapturingHandler extends Handler {

		private final List<LogRecord> records = new ArrayList<>();

		CapturingHandler() {
			super();
		}

		@Override
		public void publish(final LogRecord record) {
			this.records.add(record);
		}

		@Override
		public void flush() {
			// Sin buffer
		}

		@Override
		public void close() {
			// Sin recursos
		}

		boolean contains(final Level level, final String text) {
			for (final LogRecord record : this.records) {
				if (record.getLevel().intValue() >= level.intValue()
						&& record.getMessage() != null
						&& record.getMessage().contains(text)) {
					return true;
				}
			}
			return false;
		}

		boolean containsAny(final String... texts) {
			for (final LogRecord record : this.records) {
				final String message = record.getMessage();
				if (message == null) {
					continue;
				}
				for (final String text : texts) {
					if (message.contains(text)) {
						return true;
					}
				}
			}
			return false;
		}
	}
}
