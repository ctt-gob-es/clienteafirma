package es.gob.afirma.test.keystores;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.Signature;

import javax.security.auth.callback.PasswordCallback;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.keystores.main.callbacks.CachePasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;

/** Prueba simple de firma con PKCS#11.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestClauerCSP {

	private static final char[] PIN = "12341234".toCharArray(); //$NON-NLS-1$
	private static final String ALIAS = "CLAUER_PERSONA FISICA DE LA PEÇA DE PROVES - 111118550"; //$NON-NLS-1$

	/** Prueba de firma con CSP.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testCapi() throws Exception {

		final PasswordCallback pc = new CachePasswordCallback(PIN);

		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    		AOKeyStore.WINDOWS,
    		null,
    		"Windows", //$NON-NLS-1$
    		pc,
    		null
		);

		final PrivateKeyEntry pke = ksm.getKeyEntry(ALIAS, pc);

		try {
			final Signature signature = Signature.getInstance("SHA1withRSA"); //$NON-NLS-1$
			signature.initSign(pke.getPrivateKey());
			signature.update("Hola Mundo!!".getBytes()); //$NON-NLS-1$
			final byte[] mySignature = signature.sign();
			System.out.println(Base64.encode(mySignature));
		} catch (final Exception e) {
			System.err.println("Ocurrio un error durante el proceso de firma: " + e); //$NON-NLS-1$
			e.printStackTrace();
		}
	}

	/** M&eacute;todo de entrada.
	 * @param args
	 * @throws Exception */
	public static void main(final String args[]) throws Exception {
		new TestClauerCSP().testCapi();
	}

}
