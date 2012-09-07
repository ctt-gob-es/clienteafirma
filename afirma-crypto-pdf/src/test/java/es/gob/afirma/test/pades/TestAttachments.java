package es.gob.afirma.test.pades;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.junit.Ignore;
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

    static {
        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_PDF); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p1.setProperty("signReason", "test"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signatureProductionCity", "madrid"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signerContact", "sink@usa.net"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyQualifier", "http://google.com/"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifier", "2.16.724.1.3.1.1.2"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifierHash", "0"); //$NON-NLS-1$ //$NON-NLS-2$

    }

    /** Prueba de la firma de adjuntos (incompleta).
     * @throws Exception */
    @SuppressWarnings("static-method")
	@Ignore
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

        final Properties extraParams = new Properties();
        extraParams.put("tsaURL", CMSTimestamper.CATCERT_TSP); //$NON-NLS-1$
        extraParams.put("tsaPolicy", CMSTimestamper.CATCERT_POLICY); //$NON-NLS-1$
        extraParams.put("tsaRequireCert", CMSTimestamper.CATCERT_REQUIRECERT); //$NON-NLS-1$
        extraParams.put("tsaHashAlgorithm", "SHA1"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] result = signer.sign(testPdf, "SHA512withRSA", pke, extraParams); //$NON-NLS-1$

        Assert.assertNotNull(result);
	}
}
