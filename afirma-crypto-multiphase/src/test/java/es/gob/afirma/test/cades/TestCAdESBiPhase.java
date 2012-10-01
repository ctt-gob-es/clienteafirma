package es.gob.afirma.test.cades;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.cades.CAdESBiPhaseSigner;
import es.gob.afirma.signers.cades.CAdESBiPhaseSigner.CAdESBiPhasePreSignResult;

/** Pruebas de las firmas CAdES en dos fases.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestCAdESBiPhase {

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final byte[] DATA = "data".getBytes(); //$NON-NLS-1$

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_MD2WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_MD5WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
    };

    private static final Properties[] CADES_MODES;

    static {
        final Properties p1 = new Properties();
        p1.setProperty("signingCertificateV2", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

        final Properties p2 = new Properties();
        p2.setProperty("signingCertificateV2", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        p2.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

        CADES_MODES = new Properties[] {
                p1, p2
        };
    }


    /** Prueba simple de la firma CAdES en dos fases.
     * @throws Exception */
    @SuppressWarnings("static-method")
	@Test
    public void Test2PhaseSign() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        final PrivateKeyEntry pke;
        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        for(final Properties p : CADES_MODES) {
            for (final String algo : ALGOS) {

                System.out.println("Prueba bifasica con " + algo + " y parametros=" + p); //$NON-NLS-1$ //$NON-NLS-2$

                final CAdESBiPhasePreSignResult preSignature = CAdESBiPhaseSigner.preSign(algo, DATA, (X509Certificate[])pke.getCertificateChain(), p);
                final byte[] pkcs1 = new AOPkcs1Signer().sign(
                	preSignature.getPreSign(),
            		algo,
            		pke,
            		null
        		);
                final byte[] result2 = preSignature.getSignature();
                for (int i=0;i<pkcs1.length;i++) {
                    result2[preSignature.getIndexOfPKCS1ToReplace()+i] = pkcs1[i];
                }

            }
        }

    }


}
