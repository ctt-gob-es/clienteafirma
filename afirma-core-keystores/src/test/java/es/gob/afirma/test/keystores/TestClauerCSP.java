package es.gob.afirma.test.keystores;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Signature;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.Base64;

/** Prueba simple de firma con PKCS#11.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestClauerCSP {

	private static final String ALIAS = "CLAUER_PERSONA FISICA DE LA PE\u199A DE PROVES"; //$NON-NLS-1$

	/** Prueba de firma con CSP.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita un CLAUER en Windows
	public void testCapi() throws Exception {

		final KeyStore ks = KeyStore.getInstance("WINDOWS-MY"); //$NON-NLS-1$
		ks.load(null, null);

		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ALIAS, new KeyStore.PasswordProtection(new char[0]));

		final Signature signature = Signature.getInstance("SHA256withRSA"); //$NON-NLS-1$
		signature.initSign(pke.getPrivateKey());
		signature.update("Hola Mundo!!".getBytes()); //$NON-NLS-1$
		final byte[] mySignature = signature.sign();

		System.out.println(Base64.encode(mySignature));
	}

	/** M&eacute;todo de entrada.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String args[]) throws Exception {
		new TestClauerCSP().testCapi();
	}

}
