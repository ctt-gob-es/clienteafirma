package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.pades.AOPDFSigner;

/** Pruebas espec&iacute;ficas para PDF con datos tras la marca de fin de fichero.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestPdfVersioning {

	private final static String TEST_FILE = "TEST_PDF.pdf"; //$NON-NLS-1$

	private final static String DEFAULT_SIGNATURE_ALGORITHM = "SHA512withRSA"; //$NON-NLS-1$

    private static final String CERT_PATH = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

	/** Prueba la declaraci&oacute;n de un PDF/A-1B.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testPdfA1B() throws Exception {

		final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));
		final AOPDFSigner signer = new AOPDFSigner();

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final Properties extraParams = new Properties();
        extraParams.put("pdfXConformance", "4"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] result = signer.sign(
    		testPdf,
    		DEFAULT_SIGNATURE_ALGORITHM,
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		extraParams
		);

        final File saveFile = File.createTempFile("PDFA1B-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream os = new FileOutputStream(saveFile);
		) {
	        os.write(result);
	        os.flush();
        }
        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/** Prueba la declaraci&oacute;n de versi&oacute;n PDF.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testPdfVersion() throws Exception {

		final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));
		final AOPDFSigner signer = new AOPDFSigner();

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final Properties extraParams = new Properties();
        extraParams.put("pdfVersion", "7"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("includeOnlySignningCertificate", "false"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] result = signer.sign(
    		testPdf,
    		DEFAULT_SIGNATURE_ALGORITHM,
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		extraParams
		);

        final File saveFile = File.createTempFile("PDF7-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream os = new FileOutputStream(saveFile);
		) {
	        os.write(result);
	        os.flush();
        }
        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/** Prueba la declaraci&oacute;n de versi&oacute;n PDF mas conformidad PDF/X.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testPdfVersionOnPdfA() throws Exception {

		final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));
		final AOPDFSigner signer = new AOPDFSigner();

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final Properties extraParams = new Properties();
        extraParams.put("pdfVersion", "2"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("pdfXConformance", "4"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] result = signer.sign(
    		testPdf,
    		DEFAULT_SIGNATURE_ALGORITHM,
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		extraParams
		);

        final File saveFile = File.createTempFile("PDF2A-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream os = new FileOutputStream(saveFile);
		) {
	        os.write(result);
	        os.flush();
        }
        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$
	}

}
