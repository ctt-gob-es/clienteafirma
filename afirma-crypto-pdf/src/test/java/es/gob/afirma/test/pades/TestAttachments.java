package es.gob.afirma.test.pades;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.tsp.pkcs7.CMSTimestamper;

/** Pruebas de firmas de PDF con adjuntos y empotrados.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestAttachments {

	private static final String[] TEST_FILES = { "TEST_PDF.pdf" }; //$NON-NLS-1$

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final Properties P1 = new Properties();

    static {

        P1.setProperty("format", AOSignConstants.SIGN_FORMAT_PDF); //$NON-NLS-1$
        P1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        P1.setProperty("signReason", "test"); //$NON-NLS-1$ //$NON-NLS-2$
        P1.setProperty("signatureProductionCity", "madrid"); //$NON-NLS-1$ //$NON-NLS-2$
        P1.setProperty("signerContact", "sink@usa.net"); //$NON-NLS-1$ //$NON-NLS-2$
        P1.setProperty("policyQualifier", "http://google.com/"); //$NON-NLS-1$ //$NON-NLS-2$
        P1.setProperty("policyIdentifier", "2.16.724.1.3.1.1.2"); //$NON-NLS-1$ //$NON-NLS-2$
        P1.setProperty("policyIdentifierHash", "0"); //$NON-NLS-1$ //$NON-NLS-2$

        P1.put("tsaURL", CMSTimestamper.CATCERT_TSP); //$NON-NLS-1$
        P1.put("tsaPolicy", CMSTimestamper.CATCERT_POLICY); //$NON-NLS-1$
        P1.put("tsaRequireCert", CMSTimestamper.CATCERT_REQUIRECERT); //$NON-NLS-1$
        P1.put("tsaHashAlgorithm", "SHA1"); //$NON-NLS-1$ //$NON-NLS-2$

        P1.setProperty("attach", "RXN0YXMgc29uIGxhcyBtYfFhbml0YXMgcXVlIGNhbnRhYmEgZWwgUmV5IERhdmlkLi4u"); //$NON-NLS-1$ //$NON-NLS-2$
        P1.setProperty("attachFileName", "metadatos.txt"); //$NON-NLS-1$ //$NON-NLS-2$
        P1.setProperty("attachDescription", "Metadatos del documento PDF acordes al ENI"); //$NON-NLS-1$ //$NON-NLS-2$

    }

    /** Prueba de la firma con adjuntos (incompleta).
     * @throws Exception */
    @SuppressWarnings("static-method")
	@Test
	public void testAttachmentSignature() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILES[0]));

        final String prueba = "Firma PAdES de PDF con sello de tiempo en SHA512withRSA"; //$NON-NLS-1$

        System.out.println(prueba);

        final byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA", //$NON-NLS-1$
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		P1
		);

        Assert.assertNotNull(result);

//        final File tmpFile = File.createTempFile("AFIRMA", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
//        final OutputStream fos = new FileOutputStream(tmpFile);
//        fos.write(result);
//        fos.flush();
//        fos.close();
//        System.out.println("Resultado guardado en " + tmpFile.getAbsolutePath()); //$NON-NLS-1$
	}
}
