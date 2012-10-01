package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pades.PAdESBiPhaseSigner;
import es.gob.afirma.signers.pades.PAdESBiPhaseSigner.PAdESBiPhasePreSignResult;

/** Pruebas del m&oacute;dulo PAdES en modo bif&aacute;sico de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestPAdESBiPhase {

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final String[] TEST_FILES = { "TEST_PDF.pdf" }; //$NON-NLS-1$

    private final static Properties p1;
    static {
        p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_PDF); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p1.setProperty("signReason", "test"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signatureProductionCity", "madrid"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signerContact", "sink@usa.net"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyQualifier", "http://google.com/"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifier", "2.16.724.1.3.1.1.2"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifierHash", "0"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
    };

    private static byte[] sign(final byte[] data, final String algorithm, final PrivateKeyEntry keyEntry, final Properties xParams) throws Exception {

        final X509Certificate[] certChain = (X509Certificate[]) keyEntry.getCertificateChain();

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        // Primera fase (servidor)

        // Las firmas CAdES internas a PAdES son siempre explicitas
        extraParams.put("mode", "explicit");  //$NON-NLS-1$ //$NON-NLS-2$

        final PAdESBiPhasePreSignResult preSignature = PAdESBiPhaseSigner.preSign(
             algorithm,
             data,
             certChain,
             extraParams
        );

        // Segunda fase (dispositivo)
        final byte[] pkcs1 = new AOPkcs1Signer().sign(preSignature.getPreSign(), algorithm, keyEntry, null);

        return new String(preSignature.getSignature(), "ISO-8859-1").replace(preSignature.getPKCS1ToReplace(), AOUtil.hexify(pkcs1, false).toLowerCase()).getBytes("ISO-8859-1"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /** Prueba de firma convencional.
     * @throws Exception en cualquier error */
    @SuppressWarnings("static-method")
	@Test
    public void testBiPhaseSignature() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        String prueba;

        for (final String algo : ALGOS) {
            for (final String file : TEST_FILES) {

                final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(file));

                prueba = "Firma bifasica PAdES con el algoritmo ': " + //$NON-NLS-1$
                algo +
                "' y el fichero '" +  //$NON-NLS-1$
                file +
                "'"; //$NON-NLS-1$

               System.out.println(prueba);

                final byte[] result = sign(testPdf, algo, pke, p1);

                Assert.assertNotNull(prueba, result);

                final File saveFile = File.createTempFile(algo + "-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
                final OutputStream os = new FileOutputStream(saveFile);
                os.write(result);
                os.flush();
                os.close();
                System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

            }

        }
    }

}
