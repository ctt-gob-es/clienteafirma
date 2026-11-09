package es.gob.afirma.interop.dss;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PrivateKey;
import java.security.SecureRandom;
import java.security.Security;
import java.security.cert.X509Certificate;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.List;

import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.BasicConstraints;
import org.bouncycastle.asn1.x509.Extension;
import org.bouncycastle.asn1.x509.KeyUsage;
import org.bouncycastle.cert.X509v3CertificateBuilder;
import org.bouncycastle.cert.jcajce.JcaCertStore;
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter;
import org.bouncycastle.cert.jcajce.JcaX509v3CertificateBuilder;
import org.bouncycastle.cms.CMSProcessableByteArray;
import org.bouncycastle.cms.CMSSignedData;
import org.bouncycastle.cms.CMSSignedDataGenerator;
import org.bouncycastle.cms.jcajce.JcaSignerInfoGeneratorBuilder;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.operator.ContentSigner;
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder;
import org.bouncycastle.operator.jcajce.JcaDigestCalculatorProviderBuilder;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Tests del validador (T3b · A4): una firma con su CA da TOTAL_PASSED y con otra CA da
 * INDETERMINATE. Todo sint&eacute;tico y en memoria: no se versiona ning&uacute;n certificado.
 */
class DssValidatorTest {

	private static final byte[] CONTENT = "vector sintetico de interoperabilidad".getBytes(StandardCharsets.UTF_8); //$NON-NLS-1$
	private static X509Certificate ca1;
	private static X509Certificate ca2;
	private static byte[] signature;
	private static byte[] detachedSignature;
	// Escenario tipo FNMT: raiz e intermedia con el MISMO nombre.
	private static X509Certificate sameNameRoot;
	private static X509Certificate sameNameIntermediate;
	private static byte[] sameNameSignature;

	@BeforeAll
	static void setUp() throws Exception {
		Security.addProvider(new BouncyCastleProvider());

		final KeyPair ca1Keys = newKeyPair();
		ca1 = selfSignedCa(ca1Keys, "Interop Test CA 1"); //$NON-NLS-1$

		final KeyPair ca2Keys = newKeyPair();
		ca2 = selfSignedCa(ca2Keys, "Interop Test CA 2"); //$NON-NLS-1$

		final KeyPair leafKeys = newKeyPair();
		final X509Certificate leaf = leaf(ca1Keys, leafKeys, "Interop Test Signer"); //$NON-NLS-1$

		signature = cadesSignature(CONTENT, leafKeys.getPrivate(), leaf, ca1, true);
		// CAdES detached: el contenido no viaja dentro de la firma.
		detachedSignature = cadesSignature(CONTENT, leafKeys.getPrivate(), leaf, ca1, false);

		// Cadena con nombres repetidos: raiz autofirmada y subordinada con el mismo DN.
		final String repeated = "C=ES, O=Interop Test, CN=AC Repetida"; //$NON-NLS-1$
		final KeyPair sameNameRootKeys = newKeyPair();
		sameNameRoot = build(new X500Name(repeated), new X500Name(repeated), sameNameRootKeys, sameNameRootKeys, true);
		final KeyPair sameNameIntKeys = newKeyPair();
		sameNameIntermediate = build(new X500Name(repeated), new X500Name(repeated),
			sameNameRootKeys, sameNameIntKeys, true);
		final KeyPair sameNameLeafKeys = newKeyPair();
		final X509Certificate sameNameLeaf = build(new X500Name(repeated),
			new X500Name("C=ES, O=Interop Test, CN=Hoja Repetida"), //$NON-NLS-1$
			sameNameIntKeys, sameNameLeafKeys, false);
		sameNameSignature = cadesSignature(CONTENT, sameNameLeafKeys.getPrivate(), sameNameLeaf, sameNameIntermediate, false);
	}

	@Test
	@DisplayName("firma valida con su CA da TOTAL_PASSED") //$NON-NLS-1$
	void validSignatureWithItsCa() throws Exception {
		final DssValidator.Result result = DssValidator.validate(signature, ca1.getEncoded());
		assertNotNull(result);
		assertEquals("TOTAL_PASSED", result.indication(), "indicacion: " + result.subIndication()); //$NON-NLS-1$ //$NON-NLS-2$
		assertEquals(1, result.validSignaturesCount());
		assertNotNull(result.signatureFormat());
	}

	@Test
	@DisplayName("firma con otra CA da INDETERMINATE") //$NON-NLS-1$
	void signatureWithAnotherCaIsIndeterminate() throws Exception {
		final DssValidator.Result result = DssValidator.validate(signature, ca2.getEncoded());
		assertNotNull(result);
		assertEquals("INDETERMINATE", result.indication()); //$NON-NLS-1$
		assertEquals(0, result.validSignaturesCount());
	}

	@Test
	@DisplayName("CAdES detached con content da TOTAL_PASSED") //$NON-NLS-1$
	void detachedSignatureWithContent() throws Exception {
		final DssValidator.Result result = DssValidator.validate(detachedSignature, ca1.getEncoded(), CONTENT);
		assertNotNull(result);
		assertEquals("TOTAL_PASSED", result.indication(), "indicacion: " + result.subIndication()); //$NON-NLS-1$ //$NON-NLS-2$
		assertEquals(1, result.validSignaturesCount());
	}

	@Test
	@DisplayName("CAdES detached sin content da SIGNED_DATA_NOT_FOUND") //$NON-NLS-1$
	void detachedSignatureWithoutContent() throws Exception {
		final DssValidator.Result result = DssValidator.validate(detachedSignature, ca1.getEncoded());
		assertNotNull(result);
		assertEquals("INDETERMINATE", result.indication()); //$NON-NLS-1$
		assertEquals("SIGNED_DATA_NOT_FOUND", result.subIndication()); //$NON-NLS-1$
		assertEquals(0, result.validSignaturesCount());
	}

	@Test
	@DisplayName("cadena con nombres repetidos: intermedia + raiz da TOTAL_PASSED") //$NON-NLS-1$
	void repeatedNamesWithFullChain() throws Exception {
		final DssValidator.Result result = DssValidator.validate(
			sameNameSignature,
			List.of(sameNameIntermediate.getEncoded(), sameNameRoot.getEncoded()),
			CONTENT);
		assertNotNull(result);
		assertEquals("TOTAL_PASSED", result.indication(), "indicacion: " + result.subIndication()); //$NON-NLS-1$ //$NON-NLS-2$
		assertEquals(1, result.validSignaturesCount());
	}

	@Test
	@DisplayName("CA que no es un certificado X.509 se rechaza") //$NON-NLS-1$
	void invalidCaIsRejected() {
		boolean rejected = false;
		try {
			DssValidator.validate(signature, "no-es-un-certificado".getBytes(StandardCharsets.UTF_8)); //$NON-NLS-1$
		}
		catch (final DssValidator.ValidationException e) {
			rejected = true;
		}
		assertTrue(rejected, "se esperaba ValidationException"); //$NON-NLS-1$
	}

	private static KeyPair newKeyPair() throws Exception {
		final KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA"); //$NON-NLS-1$
		generator.initialize(2048);
		return generator.generateKeyPair();
	}

	private static X509Certificate selfSignedCa(final KeyPair keys, final String cn) throws Exception {
		final X500Name subject = new X500Name("C=ES, O=Interop Test, CN=" + cn); //$NON-NLS-1$
		return build(subject, subject, keys, keys, true);
	}

	private static X509Certificate leaf(final KeyPair caKeys, final KeyPair leafKeys, final String cn) throws Exception {
		final X500Name issuer = new X500Name("C=ES, O=Interop Test, CN=Interop Test CA 1"); //$NON-NLS-1$
		final X500Name subject = new X500Name("C=ES, O=Interop Test, CN=" + cn); //$NON-NLS-1$
		return build(issuer, subject, caKeys, leafKeys, false);
	}

	private static X509Certificate build(final X500Name issuer, final X500Name subject,
			final KeyPair issuerKeys, final KeyPair subjectKeys, final boolean isCa) throws Exception {

		final Instant now = Instant.now();
		final X509v3CertificateBuilder builder = new JcaX509v3CertificateBuilder(
			issuer,
			BigInteger.valueOf(new SecureRandom().nextLong()).abs(),
			Date.from(now.minus(1, ChronoUnit.DAYS)),
			Date.from(now.plus(30, ChronoUnit.DAYS)),
			subject,
			subjectKeys.getPublic());

		final org.bouncycastle.cert.jcajce.JcaX509ExtensionUtils extUtils =
			new org.bouncycastle.cert.jcajce.JcaX509ExtensionUtils();
		builder.addExtension(Extension.subjectKeyIdentifier, false,
			extUtils.createSubjectKeyIdentifier(subjectKeys.getPublic()));
		if (!isCa) {
			// noRevAvail (ETSI EN 319 412-5, 2.5.29.56) en el certificado de firma: la politica
			// por defecto de DSS permite omitir los datos de revocacion. Sin CRL ni OCSP ni red.
			builder.addExtension(new org.bouncycastle.asn1.ASN1ObjectIdentifier("2.5.29.56"),
				false, org.bouncycastle.asn1.DERNull.INSTANCE);
		}
		builder.addExtension(Extension.basicConstraints, true, new BasicConstraints(isCa));
		if (isCa) {
			builder.addExtension(Extension.keyUsage, true, new KeyUsage(KeyUsage.keyCertSign | KeyUsage.cRLSign));
		}
		else {
			builder.addExtension(Extension.keyUsage, true, new KeyUsage(KeyUsage.digitalSignature | KeyUsage.nonRepudiation));
		}

		final ContentSigner signer = new JcaContentSignerBuilder("SHA256withRSA") //$NON-NLS-1$
			.setProvider("BC") //$NON-NLS-1$
			.build(issuerKeys.getPrivate());

		return new JcaX509CertificateConverter().setProvider("BC").getCertificate(builder.build(signer)); //$NON-NLS-1$
	}

	/** Firma CAdES-BES en memoria; {@code encapsulated=false} genera una firma detached. */
	private static byte[] cadesSignature(final byte[] content, final PrivateKey key,
			final X509Certificate signer, final X509Certificate ca, final boolean encapsulated) throws Exception {

		final CMSSignedDataGenerator generator = new CMSSignedDataGenerator();
		final ContentSigner contentSigner = new JcaContentSignerBuilder("SHA256withRSA") //$NON-NLS-1$
			.setProvider("BC") //$NON-NLS-1$
			.build(key);
		generator.addSignerInfoGenerator(
			new JcaSignerInfoGeneratorBuilder(new JcaDigestCalculatorProviderBuilder().setProvider("BC").build()) //$NON-NLS-1$
				.build(contentSigner, signer));
		generator.addCertificates(new JcaCertStore(List.of(signer, ca)));

		final CMSSignedData signed = generator.generate(new CMSProcessableByteArray(content), encapsulated);
		return signed.getEncoded();
	}
}
