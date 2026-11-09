/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cadestri.client;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.security.Security;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.BasicConstraints;
import org.bouncycastle.asn1.x509.ExtendedKeyUsage;
import org.bouncycastle.asn1.x509.Extension;
import org.bouncycastle.asn1.x509.KeyPurposeId;
import org.bouncycastle.asn1.x509.KeyUsage;
import org.bouncycastle.cert.X509CertificateHolder;
import org.bouncycastle.cert.X509v3CertificateBuilder;
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter;
import org.bouncycastle.cert.jcajce.JcaX509v3CertificateBuilder;
import org.bouncycastle.cms.CMSProcessableByteArray;
import org.bouncycastle.cms.CMSSignedData;
import org.bouncycastle.cms.CMSSignedDataGenerator;
import org.bouncycastle.cms.SignerInfoGenerator;
import org.bouncycastle.cms.jcajce.JcaSignerInfoGeneratorBuilder;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.operator.ContentSigner;
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder;
import org.bouncycastle.operator.jcajce.JcaDigestCalculatorProviderBuilder;
import org.bouncycastle.util.CollectionStore;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;

/**
 * Pruebas del token firmado de la sesi&oacute;n trif&aacute;sica (ASVS 4.0.3 V2.1, tarea T7) con
 * un servidor trif&aacute;sico sint&eacute;tico local y sin red externa.
 * <p>
 * El servidor devuelve la prefirma y la postfirma del protocolo real; el token se firma con
 * certificados sint&eacute;ticos y se altera en cada caso.
 * </p>
 */
public class TriphaseTokenTest {

	private static final String PROVIDER = BouncyCastleProvider.PROVIDER_NAME;
	private static final char[] STORE_PASS = "changeit".toCharArray(); //$NON-NLS-1$
	private static final String STRICT_PROPERTY = "strictTriphaseToken"; //$NON-NLS-1$
	private static final String DOCUMENT = "documento sintetico de prueba de T7"; //$NON-NLS-1$

	private HttpServer server;
	private final AtomicInteger postCalls = new AtomicInteger(0);
	private volatile String preSignBody;

	private X509Certificate caCertificate;
	private KeyPair caKeyPair;
	private X509Certificate signerCertificate;
	private KeyPair signerKeyPair;
	private X509Certificate expiredCertificate;
	private KeyPair expiredKeyPair;
	private X509Certificate noEkuCertificate;
	private KeyPair noEkuKeyPair;

	private KeyPair userKeyPair;
	private X509Certificate userCertificate;

	private CapturingHandler logHandler;

	@Before
	public void setUp() throws Exception {
		if (Security.getProvider(PROVIDER) == null) {
			Security.addProvider(new BouncyCastleProvider());
		}
		generatePki();
		this.userKeyPair = generateKeyPair();
		this.userCertificate = makeCertificate(
			new X500Name("CN=Firmante de prueba, O=Autofirma, C=ES"), //$NON-NLS-1$
			KeyPurposeId.id_kp_emailProtection,
			this.caCertificate, this.caKeyPair.getPrivate(),
			new Date(System.currentTimeMillis() - 60000L),
			new Date(System.currentTimeMillis() + 86400000L),
			this.userKeyPair.getPublic()
		);

		this.postCalls.set(0);
		this.logHandler = new CapturingHandler();
		Logger.getLogger("es.gob.afirma").addHandler(this.logHandler); //$NON-NLS-1$

		this.server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0); //$NON-NLS-1$
		this.server.createContext("/", new ServerHandler()); //$NON-NLS-1$
		this.server.start();
	}

	@After
	public void tearDown() {
		Logger.getLogger("es.gob.afirma").removeHandler(this.logHandler); //$NON-NLS-1$
		System.clearProperty(STRICT_PROPERTY);
		if (this.server != null) {
			this.server.stop(0);
		}
	}

	/** Token v&aacute;lido: la firma de la sesi&oacute;n se completa. */
	@Test
	public void validTokenIsAccepted() throws Exception {
		setToken(token(payload(documentId(), "CAdES", "sign", 60000L), //$NON-NLS-1$ //$NON-NLS-2$
			this.signerCertificate, this.signerKeyPair.getPrivate(), true));
		final byte[] result = sign();
		assertNotNull(result);
		assertEquals(1, this.postCalls.get());
	}

	/** Firma del token alterada: se rechaza y no se llega a la postfirma. */
	@Test
	public void tamperedSignatureIsRejected() throws Exception {
		setToken(tamperedToken());
		assertRejectedBeforePost();
	}

	/** Certificado del servidor sin EKU de sellado: se rechaza. */
	@Test
	public void certificateWithoutSealingEkuIsRejected() throws Exception {
		setToken(token(payload(documentId(), "CAdES", "sign", 60000L), //$NON-NLS-1$ //$NON-NLS-2$
			this.noEkuCertificate, this.noEkuKeyPair.getPrivate(), true));
		assertRejectedBeforePost();
	}

	/** Certificado del servidor sin ancla de confianza: se rechaza. */
	@Test
	public void certificateWithoutTrustAnchorIsRejected() throws Exception {
		setToken(token(payload(documentId(), "CAdES", "sign", 60000L), //$NON-NLS-1$ //$NON-NLS-2$
			this.signerCertificate, this.signerKeyPair.getPrivate(), true));
		try {
			signWithTrustStore(foreignTrustStore());
			fail("Un token sin ancla de confianza debe rechazarse"); //$NON-NLS-1$
		}
		catch (final AOException expected) {
			assertTrue(String.valueOf(expected.getMessage()).contains("CA de confianza")); //$NON-NLS-1$
		}
		assertEquals("No debe llamarse a la postfirma", 0, this.postCalls.get()); //$NON-NLS-1$
	}

	/** Certificado del servidor caducado: se rechaza. */
	@Test
	public void expiredCertificateIsRejected() throws Exception {
		setToken(token(payload(documentId(), "CAdES", "sign", 60000L), //$NON-NLS-1$ //$NON-NLS-2$
			this.expiredCertificate, this.expiredKeyPair.getPrivate(), true));
		assertRejectedBeforePost();
	}

	/** Token caducado: se rechaza. */
	@Test
	public void expiredTokenIsRejected() throws Exception {
		setToken(token(payload(documentId(), "CAdES", "sign", -3600000L), //$NON-NLS-1$ //$NON-NLS-2$
			this.signerCertificate, this.signerKeyPair.getPrivate(), true));
		assertRejectedBeforePost();
	}

	/** Token de otra sesión (otro documento): se rechaza. */
	@Test
	public void tokenFromAnotherRequestIsRejected() throws Exception {
		setToken(token(payload(Base64.encode("otro documento".getBytes(StandardCharsets.UTF_8), true), //$NON-NLS-1$
			"CAdES", "sign", 60000L), //$NON-NLS-1$ //$NON-NLS-2$
			this.signerCertificate, this.signerKeyPair.getPrivate(), true));
		assertRejectedBeforePost();
	}

	/** Sin token: por defecto se avisa y se continúa (compatibilidad). */
	@Test
	public void missingTokenIsAllowedByDefaultWithWarning() throws Exception {
		setToken(null);
		final byte[] result = sign();
		assertNotNull(result);
		assertTrue("Debe avisarse de la ausencia de token", //$NON-NLS-1$
			this.logHandler.contains(Level.WARNING, "no ha enviado token")); //$NON-NLS-1$
	}

	/** Sin token y con validación estricta activa: se rechaza. */
	@Test
	public void missingTokenIsRejectedInStrictMode() throws Exception {
		System.setProperty(STRICT_PROPERTY, Boolean.TRUE.toString());
		setToken(null);
		assertRejectedBeforePost();
	}

	/** Los avisos y errores no incluyen el token, el documento ni la URL con parámetros. */
	@Test
	public void logsAndErrorsDoNotLeakSessionData() throws Exception {
		final String tampered = tamperedToken();
		setToken(tampered);
		try {
			sign();
			fail("El token alterado debe rechazarse"); //$NON-NLS-1$
		}
		catch (final AOException e) {
			final String message = String.valueOf(e.getMessage());
			assertFalse(message.contains(tampered));
			assertFalse(message.contains(documentId()));
			assertFalse(message.contains("token=")); //$NON-NLS-1$
		}
		assertFalse(this.logHandler.containsAny(tampered, documentId(), "op=pre", "op=post")); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private void assertRejectedBeforePost() throws Exception {
		try {
			sign();
			fail("El token debe rechazarse"); //$NON-NLS-1$
		}
		catch (final AOException expected) {
			assertNotNull(expected.getMessage());
		}
		assertEquals("No debe llamarse a la postfirma con un token no valido", 0, this.postCalls.get()); //$NON-NLS-1$
	}

	private byte[] sign() throws Exception {
		return signWithTrustStore(trustStore());
	}

	private byte[] signWithTrustStore(final byte[] trustStore) throws Exception {
		final Properties params = new Properties();
		params.setProperty("serverUrl", "http://127.0.0.1:" + this.server.getAddress().getPort() + "/"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		params.setProperty("triphaseTrustStore", Base64.encode(trustStore, true)); //$NON-NLS-1$
		params.setProperty("triphaseTrustStorePassword", new String(STORE_PASS)); //$NON-NLS-1$
		params.setProperty("triphaseTrustStoreType", "PKCS12"); //$NON-NLS-1$ //$NON-NLS-2$
		return new AOCAdESTriPhaseSigner().sign(
			DOCUMENT.getBytes(StandardCharsets.UTF_8),
			"SHA256withRSA", //$NON-NLS-1$
			this.userKeyPair.getPrivate(),
			new Certificate[] { this.userCertificate },
			params
		);
	}

	private String documentId() {
		return Base64.encode(DOCUMENT.getBytes(StandardCharsets.UTF_8), true);
	}

	private void setToken(final String token) {
		final StringBuilder xml = new StringBuilder();
		xml.append("<xml><firmas format=\"CAdES\""); //$NON-NLS-1$
		if (token != null) {
			xml.append(" token=\"").append(token).append('"'); //$NON-NLS-1$
		}
		xml.append("><firma Id=\"1\">"); //$NON-NLS-1$
		xml.append("<param n=\"NEED_PRE\">true</param>"); //$NON-NLS-1$
		xml.append("<param n=\"PRE\">") //$NON-NLS-1$
			.append(Base64.encode("prefirma sintetica".getBytes(StandardCharsets.UTF_8))) //$NON-NLS-1$
			.append("</param>"); //$NON-NLS-1$
		xml.append("</firma></firmas></xml>"); //$NON-NLS-1$
		this.preSignBody = Base64.encode(xml.toString().getBytes(StandardCharsets.UTF_8), true);
	}

	private String payload(final String document, final String format, final String operation, final long lifetime) {
		final long now = System.currentTimeMillis();
		return "afirma-triphase-token-v1\n" //$NON-NLS-1$
				+ "session=sintetica-1\n" //$NON-NLS-1$
				+ "doc=" + document + "\n" //$NON-NLS-1$ //$NON-NLS-2$
				+ "format=" + format + "\n" //$NON-NLS-1$ //$NON-NLS-2$
				+ "op=" + operation + "\n" //$NON-NLS-1$ //$NON-NLS-2$
				+ "iat=" + (now - 1000L) + "\n" //$NON-NLS-1$ //$NON-NLS-2$
				+ "exp=" + (now + lifetime) + "\n"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	private String token(final String payload, final X509Certificate certificate, final PrivateKey key,
			final boolean validSignature) throws Exception {
		// Para el caso de firma alterada se firma con una clave que no corresponde al certificado
		final ContentSigner contentSigner = new JcaContentSignerBuilder("SHA256withRSA") //$NON-NLS-1$
			.setProvider(PROVIDER)
			.build(validSignature ? key : generateKeyPair().getPrivate());
		final SignerInfoGenerator signerInfoGenerator = new JcaSignerInfoGeneratorBuilder(
			new JcaDigestCalculatorProviderBuilder().setProvider(PROVIDER).build())
			.build(contentSigner, new X509CertificateHolder(certificate.getEncoded()));

		final CMSSignedDataGenerator generator = new CMSSignedDataGenerator();
		generator.addSignerInfoGenerator(signerInfoGenerator);
		final List<X509CertificateHolder> holders = new ArrayList<>();
		holders.add(new X509CertificateHolder(certificate.getEncoded()));
		holders.add(new X509CertificateHolder(this.caCertificate.getEncoded()));
		generator.addCertificates(new CollectionStore<>(holders));

		final CMSSignedData cms = generator.generate(
			new CMSProcessableByteArray(payload.getBytes(StandardCharsets.UTF_8)), true);

		return Base64.encode(cms.getEncoded(), true);
	}

	private String tamperedToken() throws Exception {
		return token(payload(documentId(), "CAdES", "sign", 60000L), //$NON-NLS-1$ //$NON-NLS-2$
			this.signerCertificate, this.signerKeyPair.getPrivate(), false);
	}

	private byte[] trustStore() throws Exception {
		return trustStore(this.caCertificate);
	}

	private byte[] foreignTrustStore() throws Exception {
		final KeyPair keyPair = generateKeyPair();
		final X500Name name = new X500Name("CN=Otra CA, O=Autofirma, C=ES"); //$NON-NLS-1$
		final X509Certificate foreignCa = makeCertificate(
			name, null, null, keyPair.getPrivate(), null,
			new Date(System.currentTimeMillis() + 86400000L), keyPair.getPublic());
		return trustStore(foreignCa);
	}

	private static byte[] trustStore(final X509Certificate ca) throws Exception {
		final KeyStore keyStore = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		keyStore.load(null, null);
		keyStore.setCertificateEntry("ca", ca); //$NON-NLS-1$
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		keyStore.store(baos, STORE_PASS);
		return baos.toByteArray();
	}

	private void generatePki() throws Exception {
		this.caKeyPair = generateKeyPair();
		final X500Name caName = new X500Name("CN=CA trifasica de prueba, O=Autofirma, C=ES"); //$NON-NLS-1$
		this.caCertificate = makeCertificate(
			caName, null, null, this.caKeyPair.getPrivate(), null,
			new Date(System.currentTimeMillis() + 86400000L), this.caKeyPair.getPublic(), true);

		this.signerKeyPair = generateKeyPair();
		this.signerCertificate = makeCertificate(
			new X500Name("CN=Servidor trifasico, O=Autofirma, C=ES"), //$NON-NLS-1$
			KeyPurposeId.id_kp_emailProtection,
			this.caCertificate, this.caKeyPair.getPrivate(),
			new Date(System.currentTimeMillis() - 60000L),
			new Date(System.currentTimeMillis() + 86400000L),
			this.signerKeyPair.getPublic());

		this.expiredKeyPair = generateKeyPair();
		this.expiredCertificate = makeCertificate(
			new X500Name("CN=Servidor caducado, O=Autofirma, C=ES"), //$NON-NLS-1$
			KeyPurposeId.id_kp_emailProtection,
			this.caCertificate, this.caKeyPair.getPrivate(),
			new Date(System.currentTimeMillis() - 172800000L),
			new Date(System.currentTimeMillis() - 86400000L),
			this.expiredKeyPair.getPublic());

		this.noEkuKeyPair = generateKeyPair();
		this.noEkuCertificate = makeCertificate(
			new X500Name("CN=Servidor sin EKU, O=Autofirma, C=ES"), //$NON-NLS-1$
			null,
			this.caCertificate, this.caKeyPair.getPrivate(),
			new Date(System.currentTimeMillis() - 60000L),
			new Date(System.currentTimeMillis() + 86400000L),
			this.noEkuKeyPair.getPublic());
	}

	private static KeyPair generateKeyPair() throws Exception {
		final KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA"); //$NON-NLS-1$
		generator.initialize(2048);
		return generator.generateKeyPair();
	}

	private static X509Certificate makeCertificate(final X500Name subject, final KeyPurposeId eku,
			final X509Certificate issuer, final PrivateKey issuerKey,
			final Date notBefore, final Date notAfter, final java.security.PublicKey publicKey) throws Exception {
		return makeCertificate(subject, eku, issuer, issuerKey, notBefore, notAfter, publicKey, false);
	}

	private static X509Certificate makeCertificate(final X500Name subject, final KeyPurposeId eku,
			final X509Certificate issuer, final PrivateKey issuerKey,
			final Date notBefore, final Date notAfter, final java.security.PublicKey publicKey,
			final boolean isCa) throws Exception {

		final Date from = notBefore != null ? notBefore : new Date(System.currentTimeMillis() - 60000L);
		final Date to = notAfter != null ? notAfter : new Date(System.currentTimeMillis() + 86400000L);
		// Se reutiliza la codificacion DER del sujeto del emisor para que el nombre coincida
		final X500Name issuerName = issuer != null
			? X500Name.getInstance(issuer.getSubjectX500Principal().getEncoded())
			: subject;
		final PrivateKey signingKey = issuerKey != null ? issuerKey : null;

		final X509v3CertificateBuilder builder = new JcaX509v3CertificateBuilder(
			issuerName,
			BigInteger.valueOf(System.nanoTime()),
			from,
			to,
			subject,
			publicKey
		);
		builder.addExtension(Extension.basicConstraints, true, new BasicConstraints(isCa));
		builder.addExtension(Extension.keyUsage, true,
			new KeyUsage(isCa ? KeyUsage.keyCertSign | KeyUsage.cRLSign : KeyUsage.digitalSignature));
		if (eku != null) {
			builder.addExtension(Extension.extendedKeyUsage, true, new ExtendedKeyUsage(eku));
		}

		final ContentSigner signer = new JcaContentSignerBuilder("SHA256withRSA") //$NON-NLS-1$
			.setProvider(PROVIDER)
			.build(signingKey);
		return new JcaX509CertificateConverter().setProvider(PROVIDER).getCertificate(builder.build(signer));
	}

	private static byte[] readAll(final InputStream is) throws IOException {
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final byte[] buffer = new byte[4096];
		int read;
		while ((read = is.read(buffer)) != -1) {
			baos.write(buffer, 0, read);
		}
		return baos.toByteArray();
	}

	private final class ServerHandler implements HttpHandler {

		@Override
		public void handle(final HttpExchange exchange) throws IOException {
			// El gestor HTTP traslada los parametros de la URL al cuerpo en las peticiones POST
			final String params = new String(readAll(exchange.getRequestBody()), StandardCharsets.UTF_8);
			final byte[] response;
			if (params.contains("op=pre")) { //$NON-NLS-1$
				final String body = TriphaseTokenTest.this.preSignBody;
				response = body != null ? body.getBytes(StandardCharsets.UTF_8) : new byte[0];
			}
			else {
				TriphaseTokenTest.this.postCalls.incrementAndGet();
				response = ("OK NEWID=" //$NON-NLS-1$
						+ Base64.encode("firma sintetica".getBytes(StandardCharsets.UTF_8), true)) //$NON-NLS-1$
						.getBytes(StandardCharsets.UTF_8);
			}
			exchange.sendResponseHeaders(200, response.length);
			try (OutputStream os = exchange.getResponseBody()) {
				os.write(response);
			}
			exchange.close();
		}
	}

	/** Recoge los avisos de la aplicaci&oacute;n para comprobar que no hay fugas. */
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
					if (text != null && message.contains(text)) {
						return true;
					}
				}
			}
			return false;
		}
	}
}
