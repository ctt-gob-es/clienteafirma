package es.gob.afirma.signers.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;

/** Pruebas de firma con fecha pre-establecida.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestSignTime {

	private final static String TEST_FILE = "TEST_PDF.pdf"; //$NON-NLS-1$

	private final static String DEFAULT_SIGNATURE_ALGORITHM = "SHA512withRSA"; //$NON-NLS-1$

	private final static String CERT_PATH = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
	private final static String CERT_PASS = "12341234"; //$NON-NLS-1$
	private final static String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$


	/** Prueba de firma con fecha pre-establecida.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testSignTime() throws Exception {

		final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));
		final AOPDFSigner signer = new AOPDFSigner();

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final Properties extraParams = new Properties();
        extraParams.put("signTime", "2010:12:25:12:30:01"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] res = signer.sign(
    		testPdf,
    		DEFAULT_SIGNATURE_ALGORITHM,
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		extraParams
		);

        try (
    		final OutputStream fos = new FileOutputStream(File.createTempFile("PDF_TIME_", ".pdf")); //$NON-NLS-1$ //$NON-NLS-2$
		) {
        	fos.write(res);
        }
	}

}
