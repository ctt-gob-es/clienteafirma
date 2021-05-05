package es.gob.afirma.test.keystores.single;

import java.security.KeyStore;
import java.security.Provider;
import java.security.Security;
import java.security.cert.X509Certificate;
import java.util.Enumeration;

import org.junit.Test;

import es.gob.afirma.keystores.single.SingleCertKeyStoreProvider;

/** Pruebas de apertura de certificados sueltos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestSingeCert {

    private static String[] TEST_FILES = new String[] {
       "b64cert1.txt", //$NON-NLS-1$
       "Equifax_Secure_Certificate_Authority.pem", //$NON-NLS-1$
       "juaneliptico.cer", //$NON-NLS-1$
       "www_google_com.crt" //$NON-NLS-1$
    };

    /**
     * Pruebas de apertura de certificados sueltos.
     * @throws Exception Cuando falla la carga del almac&eacute;n.
     */
    @SuppressWarnings("static-method")
	@Test
    public void testSingleKeyStore() throws Exception {

        final Provider p = new SingleCertKeyStoreProvider();
        Security.addProvider(p);
        final KeyStore ks = KeyStore.getInstance("PKCS7", p); //$NON-NLS-1$

        for(final String filename : TEST_FILES) {
            ks.load(ClassLoader.getSystemResourceAsStream(filename), null);
            final Enumeration<String> aliases = ks.aliases();
            while (aliases.hasMoreElements()) {
            	final String alias = aliases.nextElement();
                System.out.println(alias);
                final X509Certificate cert = (X509Certificate) ks.getCertificate(alias);
                System.out.println(cert.getPublicKey().getAlgorithm());
            }
        }
    }

}
