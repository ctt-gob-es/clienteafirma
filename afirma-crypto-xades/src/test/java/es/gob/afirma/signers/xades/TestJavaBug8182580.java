package es.gob.afirma.signers.xades;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.signers.AOSigner;

/** Pruebas del error de Java 8182580.
 * <a href="https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8182580">https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8182580</a>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestJavaBug8182580 {

	/** Main para pruebas.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String[] args) throws Exception {
		new TestJavaBug8182580().testSignXadesEc();
	}

	/** Prueba simple de firma XAdES con clave de curva el&iacute;ptica.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testSignXadesEc() throws Exception {
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(
			TestJavaBug8182580.class.getResourceAsStream("/juaneliptico.p12"), //$NON-NLS-1$
			"12341234".toCharArray() //$NON-NLS-1$
		);
		final String alias = ks.aliases().nextElement();
		System.out.println("Se usara el alias: " + alias); //$NON-NLS-1$
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
			alias,
			new KeyStore.PasswordProtection("12341234".toCharArray()) //$NON-NLS-1$
		);
		final AOSigner signer = new AOXAdESSigner();
		final byte[] sign = signer.sign(
			"sdjhgajdgajsgd".getBytes(), //$NON-NLS-1$
			"SHA512withECDSA", //$NON-NLS-1$
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			null // extraParams
		);
		System.out.println("Firma:\n" + new String(sign)); //$NON-NLS-1$
	}

}
