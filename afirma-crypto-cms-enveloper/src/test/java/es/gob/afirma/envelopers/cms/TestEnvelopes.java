package es.gob.afirma.envelopers.cms;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Security;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Enumeration;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherBlockMode;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherPadding;
import es.gob.afirma.core.misc.AOUtil;

/** Pruebas de sobre digitales.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestEnvelopes {

    private static final String CERT_PATH = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

    @SuppressWarnings("static-method")
	@Before
    public void initProvider() {
    	if (Security.getProvider(org.spongycastle.jce.provider.BouncyCastleProvider.PROVIDER_NAME) == null) {
    		Security.addProvider(new org.spongycastle.jce.provider.BouncyCastleProvider());
    	}
    }

	/** Prueba de apertura de sobre.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void openEnvelope() throws Exception {

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final byte[] envelope = AOUtil.getDataFromInputStream(
			TestEnvelopes.class.getResourceAsStream("/sample.enveloped") //$NON-NLS-1$
		);

		final byte[] originalData = AOUtil.getDataFromInputStream(
				TestEnvelopes.class.getResourceAsStream("/sample") //$NON-NLS-1$
		);

		final byte[] recoveredData = new AOCMSEnveloper().recoverData(envelope, pke);

		Assert.assertArrayEquals(originalData, recoveredData);
	}

	/** Prueba de creaci&oacute;n de sobre.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void createAndOpenEnvelope() throws Exception {

		final byte[] content ="Hola mundo".getBytes(); //$NON-NLS-1$

		// Destinatario del sobre
		final X509Certificate recipientCert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
			TestEnvelopes.class.getResourceAsStream(
				"/ANFCERT.cer" // <- DESTINATARIO EN DIRECTORIO DE RECURSOS DE PRUEBA //$NON-NLS-1$
			)
		);

		final byte[] envelope = new AOCMSEnveloper().createCMSEnvelopedData(
			content,
			null,
			new AOCipherConfig(
				AOCipherAlgorithm.AES,
				AOCipherBlockMode.ECB,
				AOCipherPadding.PKCS5PADDING
			),
			new X509Certificate[] { recipientCert },
			Integer.valueOf(128) // <- CAMBIAR A 256 SI SE TIENE DESACTIVADA LA RESTRICCION EN EL JRE
		);

		// Ahora abrimos el sobre para comprobar que todo esta bien

		final KeyStore ksOpen = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ksOpen.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pkeOpen = (PrivateKeyEntry) ksOpen.getEntry(
			CERT_ALIAS,
			new KeyStore.PasswordProtection(CERT_PASS.toCharArray())
		);

		// Comprobacion de que se esta intentando usar el certificado bueno
		Assert.assertEquals(
			"Prueba mal concebida, se esta intentando abrir el sobre con un certificado que no es destinatario", //$NON-NLS-1$
			recipientCert.getSerialNumber().toString(),
			((X509Certificate)pkeOpen.getCertificate()).getSerialNumber().toString()
		);

		// Y comprobacion de que abre bien
		final byte[] recoveredData = new AOCMSEnveloper().recoverData(envelope, pkeOpen);
		Assert.assertArrayEquals(
			"El contenido desensobrado no coincide con el ensobrado", //$NON-NLS-1$
			content,
			recoveredData
		);

		System.out.println(new String(recoveredData));

	}

	/** Prueba de creaci&oacute;n de sobre con curva el&iacute;ptica.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore	// Se ignora por problemas al encontrar proveedor de curva eliptica
	public void createAndOpenEnvelopeEc() throws Exception {

		final byte[] content ="Hola mundo".getBytes(); //$NON-NLS-1$

		// Destinatario del sobre
		final X509Certificate recipientCert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
			TestEnvelopes.class.getResourceAsStream(
				"/juaneliptico.cer" // <- DESTINATARIO EN DIRECTORIO DE RECURSOS DE PRUEBA //$NON-NLS-1$
			)
		);

		final byte[] envelope = new AOCMSEnveloper().createCMSEnvelopedData(
			content,
			null,
			new AOCipherConfig(
				AOCipherAlgorithm.AES,
				AOCipherBlockMode.ECB,
				AOCipherPadding.PKCS5PADDING
			),
			new X509Certificate[] { recipientCert },
			Integer.valueOf(128) // <- CAMBIAR A 256 SI SE TIENE DESACTIVADA LA RESTRICCION EN EL JRE
		);

		// Ahora abrimos el sobre para comprobar que todo esta bien

		final KeyStore ksOpen = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ksOpen.load(TestEnvelopes.class.getResourceAsStream("/juaneliptico.p12"), "12341234".toCharArray()); //$NON-NLS-1$ //$NON-NLS-2$
		final Enumeration<String> aliases = ksOpen.aliases();
		final String alias = aliases.nextElement();

		final PrivateKeyEntry pkeOpen = (PrivateKeyEntry) ksOpen.getEntry(
			alias,
			new KeyStore.PasswordProtection(CERT_PASS.toCharArray())
		);

		// Comprobacion de que abre bien
		final byte[] recoveredData = new AOCMSEnveloper().recoverData(envelope, pkeOpen);
		Assert.assertArrayEquals(
			"El contenido desensobrado no coincide con el ensobrado", //$NON-NLS-1$
			content,
			recoveredData
		);

		System.out.println(new String(recoveredData));

	}

}
