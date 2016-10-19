package es.gob.afirma.test.keystores;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Provider;
import java.security.Security;
import java.security.Signature;
import java.util.Enumeration;

import javax.security.auth.callback.PasswordCallback;

import org.junit.Test;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.Platform.OS;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;

/** Prueba simple de firma con PKCS#11.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestPkcs11 {


	/** Prueba de firma con PKCS#11.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testPkcs11() throws Exception {
		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    		AOKeyStore.PKCS11,
    		getLibNameByOS(),
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
		System.out.println("Firma: " + new String(s.sign())); //$NON-NLS-1$

	}


	/** Prueba de firma con PKCS#11 usando directamente JRE.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testRawPkcs11() throws Exception {
        final Constructor<?> sunPKCS11Contructor = Class.forName("sun.security.pkcs11.SunPKCS11").getConstructor(InputStream.class); //$NON-NLS-1$
        final Provider p = (Provider) sunPKCS11Contructor.newInstance(
    		new ByteArrayInputStream((
				"name=pkcs11-win_dll\n" + //$NON-NLS-1$
				"library=" + getLibNameByOS() + "\n" + //$NON-NLS-1$ //$NON-NLS-2$
				"showInfo=false" //$NON-NLS-1$
			).getBytes())
		);

		Security.addProvider(p);

		
		
		PasswordCallback c = AOKeyStore.PKCS11.getStorePasswordCallback(null);
		char[] password = c.getPassword();

		final KeyStore ks = KeyStore.getInstance("PKCS11"); //$NON-NLS-1$
		ks.load(null, password);
		final Enumeration<String> aliases = ks.aliases();
		String alias = aliases.nextElement();
		System.out.println("Alias para la firma: " + alias); //$NON-NLS-1$
		if ("CertAutenticacion".equals(alias)) {
			alias = aliases.nextElement();
			System.out.println("Alias para la firma: " + alias); //$NON-NLS-1$
		}
		

		final Signature s = Signature.getInstance("SHA1withRSA", p); //$NON-NLS-1$
		s.initSign(
			((PrivateKeyEntry)ks.getEntry(
				alias,
				null
			)).getPrivateKey()
		);
		try {
			s.update("Hola".getBytes()); //$NON-NLS-1$
		}
		catch(final Exception e) {
			e.printStackTrace();
			throw e;
		}
		System.out.println("Firma: " + new String(s.sign())); //$NON-NLS-1$

	}
	
	
	private String getLibNameByOS() {
		File f = null;
		if (Platform.getOS() == OS.LINUX ) {
			f = new File("/usr/lib/x86_64-linux-gnu/opensc-pkcs11.so");
		}
		else if (Platform.getOS() == OS.WINDOWS) {
			f = new File ("C:\\WINDOWS\\SysWOW64\\DNIe_P11_priv.dll"	);
		}
		if (f.exists() && f.canRead()) {
			return f.getAbsolutePath();
		}
		return null;
	}


}
