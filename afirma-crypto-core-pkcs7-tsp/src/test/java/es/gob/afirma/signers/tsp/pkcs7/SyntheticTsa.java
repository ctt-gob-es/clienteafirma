/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.tsp.pkcs7;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.net.InetSocketAddress;
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

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;

import org.bouncycastle.asn1.ASN1GeneralizedTime;
import org.bouncycastle.asn1.ASN1Integer;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.cmp.PKIStatus;
import org.bouncycastle.asn1.cmp.PKIStatusInfo;
import org.bouncycastle.asn1.nist.NISTObjectIdentifiers;
import org.bouncycastle.asn1.tsp.MessageImprint;
import org.bouncycastle.asn1.tsp.TSTInfo;
import org.bouncycastle.asn1.tsp.TimeStampResp;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.BasicConstraints;
import org.bouncycastle.asn1.x509.ExtendedKeyUsage;
import org.bouncycastle.asn1.x509.Extension;
import org.bouncycastle.asn1.x509.GeneralName;
import org.bouncycastle.asn1.x509.GeneralNames;
import org.bouncycastle.asn1.x509.KeyPurposeId;
import org.bouncycastle.asn1.x509.KeyUsage;
import org.bouncycastle.asn1.ess.ESSCertIDv2;
import org.bouncycastle.asn1.ess.SigningCertificateV2;
import org.bouncycastle.cert.X509CertificateHolder;
import org.bouncycastle.cert.X509v3CertificateBuilder;
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter;
import org.bouncycastle.cert.jcajce.JcaX509v3CertificateBuilder;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.cms.CMSProcessableByteArray;
import org.bouncycastle.cms.DefaultSignedAttributeTableGenerator;
import org.bouncycastle.cms.CMSSignedData;
import org.bouncycastle.cms.CMSSignedDataGenerator;
import org.bouncycastle.cms.SignerInfoGenerator;
import org.bouncycastle.cms.jcajce.JcaSignerInfoGeneratorBuilder;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.operator.ContentSigner;
import org.bouncycastle.operator.DigestCalculator;
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder;
import org.bouncycastle.operator.jcajce.JcaDigestCalculatorProviderBuilder;
import org.bouncycastle.tsp.TSPAlgorithms;
import org.bouncycastle.tsp.TimeStampRequest;
import org.bouncycastle.tsp.TimeStampRequestGenerator;
import org.bouncycastle.tsp.TimeStampResponse;
import org.bouncycastle.tsp.TimeStampResponseGenerator;
import org.bouncycastle.tsp.TimeStampToken;
import org.bouncycastle.tsp.TimeStampTokenGenerator;
import org.bouncycastle.util.CollectionStore;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import com.sun.net.httpserver.HttpsConfigurator;
import com.sun.net.httpserver.HttpsServer;

/**
 * TSA sint&eacute;tica local para las pruebas de T6: sin red externa, con certificados
 * generados en memoria y respuestas RFC 3161 construidas con BouncyCastle. Permite servir
 * respuestas correctas o manipuladas (nonce, imprint, EKU, certificados incluidos) por HTTP
 * en claro y por HTTPS.
 */
final class SyntheticTsa implements AutoCloseable {

	/** Comportamiento de la respuesta que sirve la TSA. */
	enum Behaviour {
		/** Respuesta correcta con la cadena de la TSA. */
		VALID,
		/** Respuesta generada para otra peticion (nonce distinto). */
		WRONG_NONCE,
		/** Respuesta generada con otra huella (imprint distinto). */
		WRONG_IMPRINT,
		/** Sello firmado con un certificado sin EKU de sellado de tiempo. */
		NO_TIME_STAMPING_EKU,
		/** Sello sin los certificados de la TSA. */
		NO_CERTIFICATES,
		/** La TSA no concede el sello. */
		REJECTED,
		/** Sello concedido con una hora muy posterior a la del equipo. */
		FUTURE_TIME
	}

	/** Material de la CA, la TSA y el servidor TLS de la TSA sintetica. */
	static final class Pki {
		X509Certificate caCertificate;
		X509Certificate tsaCertificate;
		KeyPair tsaKeyPair;
		X509Certificate tsaCertificateWithoutEku;
		KeyPair tsaWithoutEkuKeyPair;
		X509Certificate tlsCertificate;
		PrivateKey tlsPrivateKey;
	}

	private static final String PROVIDER = BouncyCastleProvider.PROVIDER_NAME;
	private static final char[] STORE_PASSWORD = "changeit".toCharArray(); //$NON-NLS-1$

	private final Pki pki;
	private final HttpServer httpServer;
	private final HttpsServer httpsServer;
	private volatile Behaviour behaviour = Behaviour.VALID;
	private volatile byte[] lastRequestBody;

	private SyntheticTsa(final Pki pki) throws Exception {
		this.pki = pki;

		this.httpServer = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0); //$NON-NLS-1$
		this.httpServer.createContext("/tsa", new TsaHandler()); //$NON-NLS-1$
		this.httpServer.start();

		this.httpsServer = HttpsServer.create(new InetSocketAddress("127.0.0.1", 0), 0); //$NON-NLS-1$
		this.httpsServer.createContext("/tsa", new TsaHandler()); //$NON-NLS-1$
		this.httpsServer.setHttpsConfigurator(new HttpsConfigurator(createServerSslContext()));
		this.httpsServer.start();
	}

	static SyntheticTsa start() throws Exception {
		if (Security.getProvider(PROVIDER) == null) {
			Security.addProvider(new BouncyCastleProvider());
		}
		return new SyntheticTsa(generatePki());
	}

	Pki getPki() {
		return this.pki;
	}

	/** URL HTTP en claro de la TSA sintetica. */
	String getHttpUrl() {
		return "http://127.0.0.1:" + this.httpServer.getAddress().getPort() + "/tsa"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** URL HTTPS de la TSA sintetica. */
	String getHttpsUrl() {
		return "https://127.0.0.1:" + this.httpsServer.getAddress().getPort() + "/tsa"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Almac&eacute;n PKCS#12 con la CA de la TSA como ancla de confianza. */
	byte[] getTrustStoreBytes() throws Exception {
		final KeyStore trustStore = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		trustStore.load(null, null);
		trustStore.setCertificateEntry("ca", this.pki.caCertificate); //$NON-NLS-1$
		return store(trustStore);
	}

	/** Almac&eacute;n PKCS#12 con otra CA distinta (no debe servir de ancla para esta TSA). */
	static byte[] getForeignTrustStoreBytes() throws Exception {
		final KeyPair keyPair = generateKeyPair();
		final long now = System.currentTimeMillis();
		final X500Name name = new X500Name("CN=Otra CA, O=Autofirma, C=ES"); //$NON-NLS-1$
		final X509v3CertificateBuilder builder = new JcaX509v3CertificateBuilder(
			name, BigInteger.valueOf(now), new Date(now - 60000L), new Date(now + 86400000L),
			name, keyPair.getPublic());
		builder.addExtension(Extension.basicConstraints, true, new BasicConstraints(true));
		final X509Certificate certificate = convert(builder, keyPair.getPrivate());

		final KeyStore trustStore = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		trustStore.load(null, null);
		trustStore.setCertificateEntry("otra", certificate); //$NON-NLS-1$
		return store(trustStore);
	}

	static byte[] store(final KeyStore keyStore) throws Exception {
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		keyStore.store(baos, STORE_PASSWORD);
		return baos.toByteArray();
	}

	static char[] getStorePassword() {
		return STORE_PASSWORD.clone();
	}

	void setBehaviour(final Behaviour behaviour) {
		this.behaviour = behaviour;
	}

	/** Ultima peticion recibida, para comprobar el algoritmo de huella o el nonce. */
	TimeStampRequest getLastRequest() throws Exception {
		return this.lastRequestBody != null ? new TimeStampRequest(this.lastRequestBody) : null;
	}

	@Override
	public void close() {
		this.httpServer.stop(0);
		this.httpsServer.stop(0);
	}

	private SSLContext createServerSslContext() throws Exception {
		final KeyStore keyStore = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		keyStore.load(null, null);
		keyStore.setKeyEntry(
			"tls", //$NON-NLS-1$
			this.pki.tlsPrivateKey,
			STORE_PASSWORD,
			new Certificate[] { this.pki.tlsCertificate }
		);
		final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
		kmf.init(keyStore, STORE_PASSWORD);
		final SSLContext context = SSLContext.getInstance("TLSv1.2"); //$NON-NLS-1$
		context.init(kmf.getKeyManagers(), null, null);
		return context;
	}

	private final class TsaHandler implements HttpHandler {

		@Override
		public void handle(final HttpExchange exchange) throws IOException {
			try {
				final byte[] body = readAll(exchange.getRequestBody());
				SyntheticTsa.this.lastRequestBody = body;
				final TimeStampRequest request = new TimeStampRequest(body);

				final byte[] response = buildResponse(request).getEncoded();
				exchange.getResponseHeaders().add("Content-Type", "application/timestamp-reply"); //$NON-NLS-1$ //$NON-NLS-2$
				exchange.sendResponseHeaders(200, response.length);
				try (OutputStream os = exchange.getResponseBody()) {
					os.write(response);
				}
			}
			catch (final Exception e) {
				exchange.sendResponseHeaders(500, -1);
			}
			finally {
				exchange.close();
			}
		}
	}

	private TimeStampResponse buildResponse(final TimeStampRequest request) throws Exception {

		if (this.behaviour == Behaviour.REJECTED) {
			return new TimeStampResponseGenerator(buildTokenGenerator(true, true), TSPAlgorithms.ALLOWED)
				.generateRejectedResponse(new Exception("Politica no aceptada")); //$NON-NLS-1$
		}

		TimeStampRequest requestForToken = request;
		if (this.behaviour == Behaviour.WRONG_NONCE) {
			requestForToken = new TimeStampRequestGenerator().generate(
				request.getMessageImprintAlgOID(),
				request.getMessageImprintDigest(),
				BigInteger.valueOf(987654321L)
			);
		}
		else if (this.behaviour == Behaviour.WRONG_IMPRINT) {
			requestForToken = new TimeStampRequestGenerator().generate(
				request.getMessageImprintAlgOID(),
				new byte[request.getMessageImprintDigest().length],
				request.getNonce()
			);
		}

		if (this.behaviour == Behaviour.NO_TIME_STAMPING_EKU) {
			// BouncyCastle no permite firmar un sello con un certificado sin EKU critico de
			// sellado de tiempo, asi que se construye el token a mano para poder probar el
			// rechazo en el cliente.
			return buildTokenWithoutEku(request);
		}

		final boolean withCertificates = this.behaviour != Behaviour.NO_CERTIFICATES;

		final TimeStampResponseGenerator generator = new TimeStampResponseGenerator(
			buildTokenGenerator(true, withCertificates),
			TSPAlgorithms.ALLOWED
		);
		final Date genTime = this.behaviour == Behaviour.FUTURE_TIME
				? new Date(System.currentTimeMillis() + 10L * 24L * 60L * 60L * 1000L)
				: new Date();
		return generator.generateGrantedResponse(requestForToken, BigInteger.valueOf(System.nanoTime()), genTime);
	}

	private TimeStampResponse buildTokenWithoutEku(final TimeStampRequest request) throws Exception {

		final TSTInfo tstInfo = new TSTInfo(
			request.getReqPolicy() != null ? request.getReqPolicy() : new ASN1ObjectIdentifier("1.2.3.4.1"), //$NON-NLS-1$
			new MessageImprint(
				new AlgorithmIdentifier(request.getMessageImprintAlgOID()),
				request.getMessageImprintDigest()),
			new ASN1Integer(BigInteger.valueOf(System.nanoTime())),
			new ASN1GeneralizedTime(new Date()),
			null,
			null,
			request.getNonce() != null ? new ASN1Integer(request.getNonce()) : null,
			null,
			null
		);

		final ContentSigner contentSigner = new JcaContentSignerBuilder("SHA256withRSA") //$NON-NLS-1$
			.setProvider(PROVIDER)
			.build(this.pki.tsaWithoutEkuKeyPair.getPrivate());
		final X509CertificateHolder signingCertHolder =
			new X509CertificateHolder(this.pki.tsaCertificateWithoutEku.getEncoded());
		final byte[] certHash = java.security.MessageDigest.getInstance("SHA-256") //$NON-NLS-1$
			.digest(this.pki.tsaCertificateWithoutEku.getEncoded());
		final Attribute signingCertificateAttribute = new Attribute(
			PKCSObjectIdentifiers.id_aa_signingCertificateV2,
			new DERSet(new SigningCertificateV2(new ESSCertIDv2(
				new AlgorithmIdentifier(NISTObjectIdentifiers.id_sha256), certHash))));

		final SignerInfoGenerator signerInfoGenerator = new JcaSignerInfoGeneratorBuilder(
			new JcaDigestCalculatorProviderBuilder().setProvider(PROVIDER).build())
			.setSignedAttributeGenerator(new DefaultSignedAttributeTableGenerator(
				new AttributeTable(signingCertificateAttribute)))
			.build(contentSigner, signingCertHolder);

		final CMSSignedDataGenerator signedDataGenerator = new CMSSignedDataGenerator();
		signedDataGenerator.addSignerInfoGenerator(signerInfoGenerator);
		final List<X509CertificateHolder> holders = new ArrayList<>();
		holders.add(new X509CertificateHolder(this.pki.tsaCertificateWithoutEku.getEncoded()));
		holders.add(new X509CertificateHolder(this.pki.caCertificate.getEncoded()));
		signedDataGenerator.addCertificates(new CollectionStore<>(holders));

		final CMSSignedData signedData = signedDataGenerator.generate(
			new CMSProcessableByteArray(new ASN1ObjectIdentifier("1.2.840.113549.1.9.16.1.4"), tstInfo.getEncoded()), //$NON-NLS-1$
			true);

		return new TimeStampResponse(new TimeStampResp(
			new PKIStatusInfo(PKIStatus.granted),
			signedData.toASN1Structure()
		));
	}

	private TimeStampTokenGenerator buildTokenGenerator(final boolean withEku, final boolean withCertificates)
			throws Exception {

		final PrivateKey signingKey = withEku ? this.pki.tsaKeyPair.getPrivate() : this.pki.tsaWithoutEkuKeyPair.getPrivate();
		final X509Certificate signingCert = withEku ? this.pki.tsaCertificate : this.pki.tsaCertificateWithoutEku;

		final ContentSigner contentSigner = new JcaContentSignerBuilder("SHA256withRSA") //$NON-NLS-1$
			.setProvider(PROVIDER)
			.build(signingKey);
		final SignerInfoGenerator signerInfoGenerator = new JcaSignerInfoGeneratorBuilder(
			new JcaDigestCalculatorProviderBuilder().setProvider(PROVIDER).build())
			.build(contentSigner, new X509CertificateHolder(signingCert.getEncoded()));

		final DigestCalculator digestCalculator = new JcaDigestCalculatorProviderBuilder()
			.setProvider(PROVIDER)
			.build()
			.get(new AlgorithmIdentifier(NISTObjectIdentifiers.id_sha256));

		final TimeStampTokenGenerator tokenGenerator = new TimeStampTokenGenerator(
			signerInfoGenerator,
			digestCalculator,
			new ASN1ObjectIdentifier("1.2.3.4.1") //$NON-NLS-1$
		);

		if (withCertificates) {
			final List<X509CertificateHolder> holders = new ArrayList<>();
			holders.add(new X509CertificateHolder(signingCert.getEncoded()));
			holders.add(new X509CertificateHolder(this.pki.caCertificate.getEncoded()));
			tokenGenerator.addCertificates(new CollectionStore<>(holders));
		}

		return tokenGenerator;
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

	private static KeyPair generateKeyPair() throws Exception {
		final KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA"); //$NON-NLS-1$
		generator.initialize(2048);
		return generator.generateKeyPair();
	}

	private static Pki generatePki() throws Exception {

		final Pki pki = new Pki();
		final long now = System.currentTimeMillis();

		// CA raiz autofirmada
		final KeyPair caKeyPair = generateKeyPair();
		final X500Name caName = new X500Name("CN=TSA Test CA, O=Autofirma, C=ES"); //$NON-NLS-1$
		final X509v3CertificateBuilder caBuilder = new JcaX509v3CertificateBuilder(
			caName, BigInteger.valueOf(now), new Date(now - 60000L), new Date(now + 86400000L),
			caName, caKeyPair.getPublic());
		caBuilder.addExtension(Extension.basicConstraints, true, new BasicConstraints(true));
		caBuilder.addExtension(Extension.keyUsage, true, new KeyUsage(KeyUsage.keyCertSign | KeyUsage.cRLSign));
		pki.caCertificate = convert(caBuilder, caKeyPair.getPrivate());

		// Certificado de sello de tiempo con EKU id-kp-timeStamping
		pki.tsaKeyPair = generateKeyPair();
		final X509v3CertificateBuilder tsaBuilder = new JcaX509v3CertificateBuilder(
			caName, BigInteger.valueOf(now + 1), new Date(now - 60000L), new Date(now + 86400000L),
			new X500Name("CN=TSA Test, O=Autofirma, C=ES"), pki.tsaKeyPair.getPublic()); //$NON-NLS-1$
		tsaBuilder.addExtension(Extension.basicConstraints, true, new BasicConstraints(false));
		tsaBuilder.addExtension(Extension.keyUsage, true,
			new KeyUsage(KeyUsage.digitalSignature | KeyUsage.nonRepudiation));
		tsaBuilder.addExtension(Extension.extendedKeyUsage, true,
			new ExtendedKeyUsage(KeyPurposeId.id_kp_timeStamping));
		pki.tsaCertificate = convert(tsaBuilder, caKeyPair.getPrivate());

		// Certificado de sello de tiempo SIN EKU
		pki.tsaWithoutEkuKeyPair = generateKeyPair();
		final X509v3CertificateBuilder noEkuBuilder = new JcaX509v3CertificateBuilder(
			caName, BigInteger.valueOf(now + 2), new Date(now - 60000L), new Date(now + 86400000L),
			new X500Name("CN=TSA Test sin EKU, O=Autofirma, C=ES"), //$NON-NLS-1$
			pki.tsaWithoutEkuKeyPair.getPublic());
		noEkuBuilder.addExtension(Extension.basicConstraints, true, new BasicConstraints(false));
		noEkuBuilder.addExtension(Extension.keyUsage, true, new KeyUsage(KeyUsage.digitalSignature));
		pki.tsaCertificateWithoutEku = convert(noEkuBuilder, caKeyPair.getPrivate());

		// Certificado TLS del servidor de la TSA, emitido por la CA
		final KeyPair tlsKeyPair = generateKeyPair();
		final X509v3CertificateBuilder tlsBuilder = new JcaX509v3CertificateBuilder(
			caName, BigInteger.valueOf(now + 3), new Date(now - 60000L), new Date(now + 86400000L),
			new X500Name("CN=127.0.0.1, O=Autofirma, C=ES"), tlsKeyPair.getPublic()); //$NON-NLS-1$
		tlsBuilder.addExtension(Extension.basicConstraints, true, new BasicConstraints(false));
		tlsBuilder.addExtension(Extension.subjectAlternativeName, false, new GeneralNames(
			new GeneralName(GeneralName.iPAddress, "127.0.0.1"))); //$NON-NLS-1$
		pki.tlsCertificate = convert(tlsBuilder, caKeyPair.getPrivate());
		pki.tlsPrivateKey = tlsKeyPair.getPrivate();

		return pki;
	}

	private static X509Certificate convert(final X509v3CertificateBuilder builder, final PrivateKey signingKey)
			throws Exception {
		final ContentSigner signer = new JcaContentSignerBuilder("SHA256withRSA") //$NON-NLS-1$
			.setProvider(PROVIDER)
			.build(signingKey);
		return new JcaX509CertificateConverter().setProvider(PROVIDER).getCertificate(builder.build(signer));
	}
}
