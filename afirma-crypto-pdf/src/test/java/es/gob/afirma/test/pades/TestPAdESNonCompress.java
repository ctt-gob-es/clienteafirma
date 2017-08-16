package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.pades.AOPDFSigner;

/**
 * Prueba de firma PDF sin comprimir.
 */
public class TestPAdESNonCompress {

	private final static String TEST_FILE = "TEST_PDF.pdf"; //$NON-NLS-1$

	private final static String DEFAULT_SIGNATURE_ALGORITHM = "SHA512withRSA"; //$NON-NLS-1$

	private final static String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private final static String CERT_PASS = "12341234"; //$NON-NLS-1$
	private final static String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	/**
	 * Ejecuta una firma PAdES sin comprimir.
	 * @throws Exception Cuando ocurre alg&uacute;n error.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testPadesDoNotCompressPdf() throws Exception {

		Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
				"Prueba de firma PAdES sin comprimir" //$NON-NLS-1$
				);

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties extraParams = new Properties();
		extraParams.setProperty("doNotCompressPdf", "true"); //$NON-NLS-1$ //$NON-NLS-2$

		final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));

		final AOPDFSigner signer = new AOPDFSigner();
		final byte[] signedPdf = signer.sign(
				testPdf,
				DEFAULT_SIGNATURE_ALGORITHM,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				extraParams
				);

		final File tempFile = File.createTempFile("afirmaPAdES-doNotCompressPdf", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final FileOutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(signedPdf);
		}

		Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
				"Fichero generado: " + tempFile.getAbsolutePath() //$NON-NLS-1$
				);

	}
}
