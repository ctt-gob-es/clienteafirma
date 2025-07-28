package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;

/** Pruebas de PDF con contrase&ntilde;a.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestPasswordPDF {


    private static final String TEST_FILE_PWD = "TEST_PDF_Password.pdf"; //$NON-NLS-1$
    private static final String TEST_FILE_PWD_MOD = "TEST_PDF_Password_Modification.pdf"; //$NON-NLS-1$

    PrivateKeyEntry pke = null;

    @Before
    public void loadKeys() throws Exception {

    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
    	try (InputStream is = ClassLoader.getSystemResourceAsStream(TestContants.CERT_PATH)) {
    		ks.load(is, TestContants.CERT_PASS.toCharArray());
    	}
        this.pke = (PrivateKeyEntry) ks.getEntry(TestContants.CERT_ALIAS, new KeyStore.PasswordProtection(TestContants.CERT_PASS.toCharArray()));
    }

    /** Prueba la firma de un PDF protegido con contrase&ntilde;a.
     * @throws Exception en cualquier error */
    @Test
    public void testPasswordSignature() throws Exception {

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf = loadResource(TEST_FILE_PWD);

        Assert.assertTrue("No se ha reconocido como un PDF", signer.isValidDataFile(testPdf)); //$NON-NLS-1$

        final String prueba = "Firma PAdES de PDF con contrasena en SHA512withRSA"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        extraParams.put("headless", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("ownerPassword", "password"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA",  //$NON-NLS-1$
    		this.pke.getPrivateKey(),
    		this.pke.getCertificateChain(),
    		extraParams
		);

        Assert.assertNotNull(prueba, result);

        final File saveFile = saveTempFile(result);
        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

    }

    /** Prueba la firma de un PDF protegido con contrase&ntilde;a contra modificaci&oacute;n.
     * @throws Exception en cualquier error */
    @Test
    public void testModificationPasswordSignature() throws Exception {

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf = loadResource(TEST_FILE_PWD_MOD);

        Assert.assertTrue("No se ha reconocido como un PDF", signer.isValidDataFile(testPdf)); //$NON-NLS-1$

        final String prueba = "Firma PAdES de PDF con contrasena en SHA512withRSA"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        extraParams.put("headless", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("userPassword", "1111"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA",  //$NON-NLS-1$
    		this.pke.getPrivateKey(),
    		this.pke.getCertificateChain(),
    		extraParams
		);

        Assert.assertNotNull(prueba, result);

        final File out = saveTempFile(result);
        System.out.println("Temporal para comprobacion manual: " + out.getAbsolutePath()); //$NON-NLS-1$

    }

    /** Prueba la firma de un PDF protegido con contrase&ntilde;a contra escritura
     * con algoritmo AES-256.
     * @throws Exception en cualquier error */
    @Test
    public void testOwnerPasswordAES256() throws Exception {

        final String prueba = "Firma PAdES de PDF con cifrado AES-256 contra modificaciones"; //$NON-NLS-1$
        System.out.println(prueba);

        final Properties extraParams = new Properties();
        extraParams.put("headless", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("ownerPassword", "123456"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] testPdf = loadResource("PDF_de_prueba_AES256_MODIFICACION_123456.pdf"); //$NON-NLS-1$

        final AOSigner signer = new AOPDFSigner();

        Assert.assertTrue("No se ha reconocido como un PDF", signer.isValidDataFile(testPdf)); //$NON-NLS-1$

        final byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA",  //$NON-NLS-1$
    		this.pke.getPrivateKey(),
    		this.pke.getCertificateChain(),
    		extraParams
		);

        Assert.assertNotNull(prueba, result);

        final File saveFile = saveTempFile(result);
        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

    }

    /** Prueba la firma de un PDF protegido con contrase&ntilde;a contra lectura
     * con algoritmo AES-256.
     * @throws Exception en cualquier error */
    @Test
    public void testUserPasswordAES256() throws Exception {

        final String prueba = "Firma PAdES de PDF con cifrado AES-256 contra apertura"; //$NON-NLS-1$
        System.out.println(prueba);

        final Properties extraParams = new Properties();
        extraParams.put("headless", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("userPassword", "123456"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] testPdf = loadResource("PDF_de_prueba_AES256_LECTURA_123456.pdf"); //$NON-NLS-1$

        final AOSigner signer = new AOPDFSigner();

        Assert.assertTrue("No se ha reconocido como un PDF", signer.isValidDataFile(testPdf)); //$NON-NLS-1$

        final byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA",  //$NON-NLS-1$
    		this.pke.getPrivateKey(),
    		this.pke.getCertificateChain(),
    		extraParams
		);

        Assert.assertNotNull(prueba, result);

        final File saveFile = saveTempFile(result);
        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

    }

    private static byte[] loadResource(final String resource) throws IOException {
    	byte[] data;
    	try (InputStream is = ClassLoader.getSystemResourceAsStream(resource)) {
    		data = AOUtil.getDataFromInputStream(is);
    	}
    	return data;
    }

    private static File saveTempFile(final byte[] data) throws IOException {
    	final File saveFile = File.createTempFile("PWD-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream os = new FileOutputStream(saveFile);
		) {
	        os.write(data);
	        os.flush();
        }
        return saveFile;
    }
}
