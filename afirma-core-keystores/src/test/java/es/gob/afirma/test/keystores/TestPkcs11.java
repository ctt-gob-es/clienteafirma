package es.gob.afirma.test.keystores;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Provider;
import java.security.Security;
import java.security.Signature;
import java.util.Enumeration;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;

/** Prueba simple de firma con PKCS#11.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestPkcs11 {

	//private static final String LIB_NAME = "C:\\WINDOWS\\System32\\DNIe_P11_priv.dll"; //$NON-NLS-1$
	private static final String LIB_NAME = "C:\\WINDOWS\\SysWOW64\\siecap11.dll"; //$NON-NLS-1$
	private static final char[] PIN = "1111".toCharArray(); //$NON-NLS-1$

	/** Prueba de firma con PKCS#11.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Ignore
	@Test
	public void testPkcs11() throws Exception {
		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    		AOKeyStore.PKCS11,
    		LIB_NAME,
    		"Afirma-P11", //$NON-NLS-1$
    		AOKeyStore.PKCS11.getStorePasswordCallback(null),
    		null
		);
		String al = null;
		for (final String alias : ksm.getAliases()) {
			System.out.println(alias);
			al = alias;
		}

		final Signature s = Signature.getInstance("SHA1withRSA"); //$NON-NLS-1$
		s.initSign(ksm.getKeyEntry(al, AOKeyStore.PKCS11.getCertificatePasswordCallback(null)).getPrivateKey());
		s.update("Hola".getBytes()); //$NON-NLS-1$
		System.out.println("Firma: " + new String(s.sign())); //$NON-NLS-1$

	}

	/** Prueba de firma con PKCS#11 usando directamente JRE.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testRawPkcs11() throws Exception {

		final Constructor<?> sunPKCS11Contructor = Class.forName("sun.security.pkcs11.SunPKCS11").getConstructor(InputStream.class); //$NON-NLS-1$
		final Provider p = (Provider) sunPKCS11Contructor.newInstance(
			new ByteArrayInputStream(
				(
					"name=pkcs11-win_dll\n" + //$NON-NLS-1$
					"library=" + LIB_NAME + "\n" + //$NON-NLS-1$ //$NON-NLS-2$
					"showInfo=true" //$NON-NLS-1$
				).getBytes()
			)
		);

		Security.addProvider(p);

		final KeyStore ks = KeyStore.getInstance("PKCS11"); //$NON-NLS-1$
		ks.load(null, PIN);
		final Enumeration<String> aliases = ks.aliases();
		final String alias = aliases.nextElement();
		System.out.println("Alias para la firma: " + alias); //$NON-NLS-1$

		final Signature s = Signature.getInstance("SHA1withRSA", p); //$NON-NLS-1$
		s.initSign(
			((PrivateKeyEntry)ks.getEntry(alias, null)).getPrivateKey()
		);
		s.update("Hola".getBytes()); //$NON-NLS-1$
		System.out.println("Firma: " + new String(s.sign())); //$NON-NLS-1$

	}

	/** Pruebas de DNIe.
	 * @throws Exception */
	@Test
	@Ignore
	@SuppressWarnings("static-method")
	public void testDnie() throws Exception {
		final AOKeyStoreManager aoks = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			AOKeyStore.DNIE,
			null,
			"DNIe_10", //$NON-NLS-1$
			AOKeyStore.DNIE.getStorePasswordCallback(null),
			null
		);
		System.out.println(
			aoks.getKeyEntry(
				aoks.getAliases()[0],
				AOKeyStore.DNIE.getCertificatePasswordCallback(null)
			).getPrivateKey()
		);
	}

	/** M&eacute;todo de entrada.
	 * @param args
	 * @throws Exception */
	public static void main(final String args[]) throws Exception {
		new TestPkcs11().testPkcs11();
	}

}
