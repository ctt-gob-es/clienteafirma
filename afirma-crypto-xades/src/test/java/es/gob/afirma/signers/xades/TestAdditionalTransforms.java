package es.gob.afirma.signers.xades;

import java.io.File;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;

/** Pruebas de transformaciones adicionales en firmas XAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestAdditionalTransforms {

    private static final String CERT_PATH = "CERES.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "1234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "nombre garc\u00EDa-mer\u00E1s capote tom\u00E1s - nif 11830960j"; //$NON-NLS-1$

    private static final Properties XPATH1 = new Properties();
    private static final Properties BASE64 = new Properties();

    static {
        XPATH1.setProperty("xmlTransforms", "1"); //$NON-NLS-1$ //$NON-NLS-2$
        XPATH1.setProperty("xmlTransform0Type", "http://www.w3.org/TR/1999/REC-xpath-19991116"); //$NON-NLS-1$ //$NON-NLS-2$
        XPATH1.setProperty("xmlTransform0Body", "/bookstore/book[1]/title");  //$NON-NLS-1$//$NON-NLS-2$
        XPATH1.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$

        BASE64.setProperty("xmlTransforms", "1"); //$NON-NLS-1$ //$NON-NLS-2$
        BASE64.setProperty("xmlTransform0Type", "http://www.w3.org/2000/09/xmldsig#base64"); //$NON-NLS-1$ //$NON-NLS-2$
        BASE64.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$
        BASE64.setProperty("encoding", "base64"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    private static final String TEST_FILE = "xpathnodenveloped.xml"; //$NON-NLS-1$

    /** Prueba de transformaciones a medida XPATH1.
     * @throws Exception en cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testXPATH1Transform() throws Exception {

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

    	final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));

    	final AOXAdESSigner signer = new AOXAdESSigner();
    	final byte[] signature = signer.sign(data,
			AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			XPATH1
		);

		final File f = File.createTempFile("xades-TRANS-XPATH1-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
		) {
			fos.write(signature);
			fos.flush();
		}
		System.out.println("Firma para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

    /** Prueba de transformaciones a medida BASE64.
     * @throws Exception en cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testBASE64Transform() throws Exception {

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

    	final byte[] data = Base64.encode(AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE))).getBytes();

    	final AOXAdESSigner signer = new AOXAdESSigner();
    	final byte[] signature = signer.sign(data,
			AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			BASE64
		);

		final File f = File.createTempFile("xades-TRANS-BASE64-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
		) {
			fos.write(signature);
			fos.flush();
		}
		System.out.println("Firma para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

}
