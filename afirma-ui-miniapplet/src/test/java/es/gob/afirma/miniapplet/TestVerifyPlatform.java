package es.gob.afirma.miniapplet;

import org.junit.Test;

/** Prueba la verificaci&oacute;n de plataforma. */
public class TestVerifyPlatform {

	/** Prueba la verificaci&oacute;n de plataforma.
	 * @throws Exception
	 */
	@SuppressWarnings({ "static-method", "deprecation" })
	@Test
	public void testVerifyPlatform() throws Exception {
		new MiniAfirmaApplet().verifyPlatform();
	}

}
