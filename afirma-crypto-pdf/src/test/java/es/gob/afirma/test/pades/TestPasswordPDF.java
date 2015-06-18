package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;

/** Pruebas de PDF con contrase&ntilde;a.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestPasswordPDF {

    private static final String CERT_PATH = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

    private static final String TEST_FILE_PWD = "TEST_PDF_Password.pdf"; //$NON-NLS-1$
    private static final String TEST_FILE_PWD_MOD = "TEST_PDF_Password_Modification.pdf"; //$NON-NLS-1$

    /** Prueba la firma de un PDF protegido con contrase&ntilde;a.
     * @throws Exception en cualquier error */
    @SuppressWarnings("static-method")
	@Test
    public void testPasswordSignature() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE_PWD));

        Assert.assertTrue("No se ha reconocido como un PDF", signer.isValidDataFile(testPdf)); //$NON-NLS-1$

        final String prueba = "Firma PAdES de PDF con contrasena en SHA512withRSA"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        extraParams.put("headless", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("ownerPassword", "password"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA",  //$NON-NLS-1$
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		extraParams
		);

        Assert.assertNotNull(prueba, result);

        final File saveFile = File.createTempFile("PWD-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        final OutputStream os = new FileOutputStream(saveFile);
        os.write(result);
        os.flush();
        os.close();
        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

    }

    /** Prueba la firma de un PDF protegido con contrase&ntilde;a contra modificaci&oacute;n.
     * @throws Exception en cualquier error */
    @SuppressWarnings("static-method")
	@Test
    public void testModificationPasswordSignature() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE_PWD_MOD));

        Assert.assertTrue("No se ha reconocido como un PDF", signer.isValidDataFile(testPdf)); //$NON-NLS-1$

        final String prueba = "Firma PAdES de PDF con contrasena en SHA512withRSA"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        extraParams.put("headless", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("userPassword", "1111"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA",  //$NON-NLS-1$
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		extraParams
		);

        Assert.assertNotNull(prueba, result);

        final File out = File.createTempFile("TEST-PWD", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        final FileOutputStream fos = new FileOutputStream(out);
        fos.write(result);
        fos.flush();
        fos.close();
        System.out.println("Temporal para comprobacion manual: " + out.getAbsolutePath()); //$NON-NLS-1$

    }

}
