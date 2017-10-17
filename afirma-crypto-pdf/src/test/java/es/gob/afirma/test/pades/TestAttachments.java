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
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;

/** Pruebas de firmas de PDF con adjuntos y empotrados.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestAttachments {

	private static final String[] TEST_FILES = { "TEST_PDF.pdf" }; //$NON-NLS-1$

	private static final String CATCERT_POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$
	private static final String CATCERT_TSP = "http://psis.catcert.net/psis/catcert/tsp"; //$NON-NLS-1$
	private static final Boolean CATCERT_REQUIRECERT = Boolean.TRUE;

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

        P1.put("tsaURL", CATCERT_TSP); //$NON-NLS-1$
        P1.put("tsaPolicy", CATCERT_POLICY); //$NON-NLS-1$
        P1.put("tsaRequireCert", CATCERT_REQUIRECERT); //$NON-NLS-1$
        P1.put("tsaHashAlgorithm", "SHA1"); //$NON-NLS-1$ //$NON-NLS-2$

        P1.setProperty("attach", "RXN0YXMgc29uIGxhcyBtYfFhbml0YXMgcXVlIGNhbnRhYmEgZWwgUmV5IERhdmlkLi4u"); //$NON-NLS-1$ //$NON-NLS-2$
        P1.setProperty("attachFileName", "metadatos.txt"); //$NON-NLS-1$ //$NON-NLS-2$
        P1.setProperty("attachDescription", "Metadatos del documento PDF acordes al ENI"); //$NON-NLS-1$ //$NON-NLS-2$

    }

    /** Prueba de la firma con adjuntos (incompleta).
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
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

        final File tmpFile = File.createTempFile("AFIRMA", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream fos = new FileOutputStream(tmpFile);
		) {
	        fos.write(result);
	        fos.flush();
        }
        System.out.println("Resultado guardado en " + tmpFile.getAbsolutePath()); //$NON-NLS-1$
	}
}
