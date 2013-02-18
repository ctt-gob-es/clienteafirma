package es.gob.afirma.test.keystores.single;

import java.security.KeyStore;
import java.security.Provider;
import java.security.Security;
import java.util.Enumeration;

import org.junit.Test;

import es.gob.afirma.keystores.single.SingleCertKeyStoreProvider;

/** Pruebas de apertura de certificados sueltos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestSingeCert {

    private static String[] TEST_FILES = new String[] {
       "b64cert1.txt", //$NON-NLS-1$
       "Equifax_Secure_Certificate_Authority.pem" //$NON-NLS-1$
    };

    /** Pruebas de apertura de certificados sueltos.
     * @throws Exception */
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
                System.out.println(aliases.nextElement());
            }
        }
    }

}
