package es.gob.afirma.signers.pkcs7;

import org.junit.Test;

/** Pruebas de la comprobaci&oacute;n de la versi&oacute;n de SpongyCastle.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestScChecker {

	/** Prueba la comprobaci&oacute;n de la versi&oacute;n de SpongyCastle. */
	@SuppressWarnings("static-method")
	@Test
	public void testScCheck() {
		new SCChecker().checkSpongyCastle();
	}

}
