package es.gob.afirma.core.signers;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Signature;
import java.util.Base64;

import org.junit.Assert;
import org.junit.Test;

public class Pkcs1SignerTest {

    private static final String CERT_RSA_PATH = "EIDAS_CERTIFICADO_PRUEBAS___99999999R__1234.p12"; //$NON-NLS-1$
    private static final char[] CERT_RSA_PASS = "1234".toCharArray(); //$NON-NLS-1$
    private static final String CERT_RSA_ALIAS = "eidas_certificado_pruebas___99999999r"; //$NON-NLS-1$

    private static final String CERT_EC_PATH = "ciudadanohw_ecc_2023v1.p12"; //$NON-NLS-1$
    private static final char[] CERT_EC_PASS = "ciudadanohw_ecc_2023v1".toCharArray(); //$NON-NLS-1$
    private static final String CERT_EC_ALIAS = "manuela blanco vidal - nif:10000322z"; //$NON-NLS-1$

	@SuppressWarnings("static-method")
	@Test
	public void testPkcs1RsaSign() throws Exception {

		final byte[] data = "Hola Mundo!!".getBytes(StandardCharsets.UTF_8); //$NON-NLS-1$
		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA;
		final PrivateKeyEntry pke = loadPrivateKeyEntry(CERT_RSA_PATH, CERT_RSA_PASS, CERT_RSA_ALIAS);

		final AOPkcs1Signer signer = new AOPkcs1Signer();
		final byte[] pkcs1 = signer.sign(
				data,
				algorithm,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				null);

		final Signature sig = Signature.getInstance(algorithm);
		sig.initVerify(pke.getCertificate().getPublicKey());
		sig.update(data);

		Assert.assertTrue("Error al verificar la firma RSA", sig.verify(pkcs1)); //$NON-NLS-1$
	}

	@SuppressWarnings("static-method")
	@Test
	public void testPkcs1EcdsaSign() throws Exception {

		final byte[] data = "Hola Mundo!!".getBytes(StandardCharsets.UTF_8); //$NON-NLS-1$
		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA256WITHECDSA;
		final PrivateKeyEntry pke = loadPrivateKeyEntry(CERT_EC_PATH, CERT_EC_PASS, CERT_EC_ALIAS);

		final Signature sig1 = Signature.getInstance(algorithm);
		sig1.initSign(pke.getPrivateKey());
		sig1.update(data);

		final byte[] pkcs1 = sig1.sign();
		System.out.println("PKCS#1: " + Base64.getEncoder().encodeToString(pkcs1)); //$NON-NLS-1$

		final Signature sig = Signature.getInstance(algorithm);
		sig.initVerify(pke.getCertificate().getPublicKey());
		sig.update(data);

		Assert.assertTrue("Error al verificar la firma ECDSA", sig.verify(pkcs1)); //$NON-NLS-1$
	}

	private static PrivateKeyEntry loadPrivateKeyEntry(final String path, final char[] pass, final String alias) throws Exception {
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (InputStream is = ClassLoader.getSystemResourceAsStream(path)) {
			ks.load(is, pass);
		}
        return (PrivateKeyEntry) ks.getEntry(alias, new KeyStore.PasswordProtection(pass));
	}
}
