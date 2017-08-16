package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.pades.AOPDFSigner;

/** Pruebas de cofirmas. */
public class TestCosignature {

	private final static String TEST_FILE = "TEST_PDF.pdf"; //$NON-NLS-1$

	private final static String DEFAULT_SIGNATURE_ALGORITHM = "SHA512withRSA"; //$NON-NLS-1$

	private final static String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private final static String CERT_PASS = "12341234"; //$NON-NLS-1$
	private final static String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	/** Prueba de cofirma de PDF.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCosignPdf() throws Exception {
		Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
			"Prueba de cofirma PDF" //$NON-NLS-1$
		);

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties extraParams = new Properties();

		final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));

		final AOPDFSigner signer = new AOPDFSigner();
		byte[] signedPdf = signer.sign(
			testPdf,
			DEFAULT_SIGNATURE_ALGORITHM,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			extraParams
		);

		File tempFile = File.createTempFile("afirmaPDF-OneSign_", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
    		final OutputStream fos = new FileOutputStream(tempFile);
		) {
	        fos.write(signedPdf);
	        fos.flush();
        }

		signedPdf = signer.sign(
				signedPdf,
				DEFAULT_SIGNATURE_ALGORITHM,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				extraParams
		);

		tempFile = File.createTempFile("afirmaPDF-TwoSign_", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
    		final OutputStream fos = new FileOutputStream(tempFile);
		) {
	        fos.write(signedPdf);
	        fos.flush();
        }

	}

}
