package test.es.gob.jmulticard.jse.provider;

import java.io.IOException;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.jmulticard.CryptoHelper;
import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.jse.provider.JseCryptoHelper;

/** Pruebas de las funcionalidades de CryptoHelper. */
public class TestCryptoHelper {
	
	/** Prueba de huella digital con algoritmo nulo. 
	 * @throws IOException En caso de error en la huella, no debe darse nunca en esta prueba */
	@SuppressWarnings("static-method")
	@Test(expected=IllegalArgumentException.class)
	public void testDigestWithNullAlgorithm() throws IOException {
		CryptoHelper ch = new JseCryptoHelper();
		ch.digest(null, "data".getBytes()); //$NON-NLS-1$
	}
	
	/** Prueba que se acepan nombres alternativos para las huellas digitales.
	 * @throws IOException En caso de error en la huella, no debe darse nunca en esta prueba */
	@SuppressWarnings("static-method")
	public void testDigestWithAlternateName() throws IOException {
		final byte[] data = "data".getBytes(); //$NON-NLS-1$
		CryptoHelper ch = new JseCryptoHelper();
		Assert.assertTrue(HexUtils.arrayEquals(ch.digest("SHA", data), ch.digest("SHA1", data))); //$NON-NLS-1$ //$NON-NLS-2$
		Assert.assertTrue(HexUtils.arrayEquals(ch.digest("SHA", data), ch.digest("SHA-1", data))); //$NON-NLS-1$ //$NON-NLS-2$
		Assert.assertTrue(HexUtils.arrayEquals(ch.digest("SHA256", data), ch.digest("SHA-256", data))); //$NON-NLS-1$ //$NON-NLS-2$
		Assert.assertTrue(HexUtils.arrayEquals(ch.digest("SHA384", data), ch.digest("SHA-384", data))); //$NON-NLS-1$ //$NON-NLS-2$
		Assert.assertTrue(HexUtils.arrayEquals(ch.digest("SHA512", data), ch.digest("SHA-512", data))); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	/** Prueba de cifrado con clave nula.
	 * @throws IOException En caso de error , no debe darse nunca en esta prueba */
	@SuppressWarnings("static-method")
	@Test(expected=IllegalArgumentException.class)
	public void testDesEncryptionsWithNullKey() throws IOException {
		new JseCryptoHelper().desEncrypt("data".getBytes(), null); //$NON-NLS-1$
	}
	
	/** Prueba de cifrado con clave nula.
	 * @throws IOException En caso de error , no debe darse nunca en esta prueba */
	@SuppressWarnings("static-method")
	@Test(expected=IllegalArgumentException.class)
	public void testDesedeEncryptionsWithNullKey() throws IOException {
		new JseCryptoHelper().desedeEncrypt("data".getBytes(), null); //$NON-NLS-1$
	}
	
	/** Prueba de cifrado con clave nula.
	 * @throws IOException En caso de error , no debe darse nunca en esta prueba */
	@SuppressWarnings("static-method")
	@Test(expected=IllegalArgumentException.class)
	public void testDesDecryptionsWithNullKey() throws IOException {
		new JseCryptoHelper().desDecrypt("data".getBytes(), null); //$NON-NLS-1$
	}
	
	/** Prueba de cifrado con clave nula.
	 * @throws IOException En caso de error , no debe darse nunca en esta prueba */
	@SuppressWarnings("static-method")
	@Test(expected=IllegalArgumentException.class)
	public void testDesedeDecryptionsWithNullKey() throws IOException {
		new JseCryptoHelper().desedeDecrypt("data".getBytes(), null); //$NON-NLS-1$
	}
	
	/** Prueba de cifrado con clave nula.
	 * @throws IOException En caso de error , no debe darse nunca en esta prueba */
	@SuppressWarnings("static-method")
	@Test(expected=IllegalArgumentException.class)
	public void testDesEncryptionsWithBadKey() throws IOException {
		new JseCryptoHelper().desEncrypt("data".getBytes(), "key".getBytes()); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	/** Prueba de cifrado con clave nula.
	 * @throws IOException En caso de error , no debe darse nunca en esta prueba */
	@SuppressWarnings("static-method")
	@Test(expected=IllegalArgumentException.class)
	public void testDesedeEncryptionsWithBadKey() throws IOException {
		new JseCryptoHelper().desedeEncrypt("data".getBytes(), "key".getBytes()); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	/** Prueba de cifrado con clave nula.
	 * @throws IOException En caso de error , no debe darse nunca en esta prueba */
	@SuppressWarnings("static-method")
	@Test(expected=IllegalArgumentException.class)
	public void testDesDecryptionsWithBadKey() throws IOException {
		new JseCryptoHelper().desDecrypt("data".getBytes(), "key".getBytes()); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	/** Prueba de cifrado con clave nula.
	 * @throws IOException En caso de error , no debe darse nunca en esta prueba */
	@SuppressWarnings("static-method")
	@Test(expected=IllegalArgumentException.class)
	public void testDesedeDecryptionsWithBadKey() throws IOException {
		new JseCryptoHelper().desedeDecrypt("data".getBytes(), "key".getBytes()); //$NON-NLS-1$ //$NON-NLS-2$
	}

}
