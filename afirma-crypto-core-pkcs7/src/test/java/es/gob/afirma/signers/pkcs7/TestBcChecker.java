package es.gob.afirma.signers.pkcs7;

import org.junit.Test;

/** Pruebas de la comprobaci&oacute;n de la versi&oacute;n de BouncyCastle.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestBcChecker {

	/** Prueba la comprobaci&oacute;n de la versi&oacute;n de BouncyCastle. */
	@SuppressWarnings("static-method")
	@Test
	public void testBcCheck() {
		new BCChecker().checkBouncyCastle();
	}

}
