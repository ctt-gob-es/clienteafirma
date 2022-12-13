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

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.callbacks.CachePasswordCallback;

/** Prueba simple de firma con PKCS#11.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestPkcs11 {

	//private static final String LIB_NAME = "C:\\WINDOWS\\System32\\DNIe_P11_priv.dll"; //$NON-NLS-1$
	//private static final String LIB_NAME = "C:\\WINDOWS\\SysWOW64\\siecap11.dll"; //$NON-NLS-1$
	//private static final String LIB_NAME = "C:\\Users\\tomas\\workspace_32\\afirma-core-keystores\\src\\test\\resources\\CardOS\\cardos11.dll"; //$NON-NLS-1$
	//private static final String LIB_NAME = "C:\\WINDOWS\\SysWOW64\\cardos11.dll"; //$NON-NLS-1$
	//private static final String LIB_NAME = "C:\\WINDOWS\\SysWOW64\\TIF_P11.dll"; //$NON-NLS-1$
	private static final String LIB_NAME = "C:\\WINDOWS\\SysWOW64\\DNIe_P11_priv.dll"; //$NON-NLS-1$

	private static final char[] PIN_DNIE = "12345678".toCharArray(); //$NON-NLS-1$

	private static final String LIB_NAME_FNMT = "C:\\WINDOWS\\System32\\FNMT_P11_x64.dll"; //$NON-NLS-1$
	//private static final String LIB_NAME_FNMT = "C:\\WINDOWS\\System32\\FNMT_P11.dll"; //$NON-NLS-1$
	//private static final String LIB_NAME_FNMT = "C:\\WINDOWS\\SysWOW64\\FNMT_P11.dll"; //$NON-NLS-1$

	private static final char[] PIN_FNMT = "1234".toCharArray(); //$NON-NLS-1$

	/** Prueba de firma con PKCS#11 del DNIe.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore //Dependiente del PKCS#11
	public void testPkcs11Dnie() throws Exception {
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

		final Signature s = Signature.getInstance("SHA256withRSA"); //$NON-NLS-1$
		s.initSign(ksm.getKeyEntry(al).getPrivateKey());
		s.update("Hola".getBytes()); //$NON-NLS-1$
		System.out.println("Firma: " + Base64.encode(s.sign())); //$NON-NLS-1$

	}

	/** Prueba de firma con PKCS#11 de la tarjeta CERES.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore //Dependiente del PKCS#11
	public void testPkcs11Fnmt() throws Exception {
		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    		AOKeyStore.PKCS11,
    		LIB_NAME_FNMT,
    		"Afirma-P11", //$NON-NLS-1$
    		new CachePasswordCallback(PIN_FNMT),
    		null
		);
		String al = null;
		for (final String alias : ksm.getAliases()) {
			System.out.println(alias);
			al = alias;
		}

		final Signature s = Signature.getInstance("SHA256withRSA"); //$NON-NLS-1$
		s.initSign(ksm.getKeyEntry(al).getPrivateKey());
		s.update("Hola".getBytes()); //$NON-NLS-1$
		System.out.println("Firma: " + Base64.encode(s.sign())); //$NON-NLS-1$

	}

	/** Prueba de firma con PKCS#11 usando directamente JRE.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Dependiente del PKCS#11
	public void testRawPkcs11() throws Exception {

        final Constructor<?> sunPKCS11Contructor = Class.forName("sun.security.pkcs11.SunPKCS11").getConstructor(InputStream.class); //$NON-NLS-1$
        final Provider p = (Provider) sunPKCS11Contructor.newInstance(
    		new ByteArrayInputStream((
				"name=pkcs11-win_dll\n" + //$NON-NLS-1$
				"library=" + LIB_NAME + "\n" + //$NON-NLS-1$ //$NON-NLS-2$
				"showInfo=false" //$NON-NLS-1$
			).getBytes())
		);

		Security.addProvider(p);

		final KeyStore ks = KeyStore.getInstance("PKCS11"); //$NON-NLS-1$
		ks.load(null, PIN_DNIE);
		final Enumeration<String> aliases = ks.aliases();
		final String alias = aliases.nextElement();
		System.out.println("Alias para la firma: " + alias); //$NON-NLS-1$

		final Signature s = Signature.getInstance("SHA256withRSA", p); //$NON-NLS-1$
		s.initSign(
			((PrivateKeyEntry)ks.getEntry(
				alias,
				null
			)).getPrivateKey()
		);

		s.update("Hola".getBytes()); //$NON-NLS-1$

		System.out.println("Firma: " + Base64.encode(s.sign())); //$NON-NLS-1$

	}

	/** M&eacute;todo de entrada.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String args[]) throws Exception {
		new TestPkcs11().testPkcs11Dnie();
	}

}
