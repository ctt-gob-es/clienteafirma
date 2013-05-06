package es.gob.afirma.core.misc;

import org.junit.Assert;
import org.junit.Test;

/** Prueba la obtenci&oacute;n de versi&oacute;n de BouncyCastle.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestBCVersion {

	/** Prueba la obtenci&oacute;n de versi&oacute;n de BouncyCastle.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	public void getBCVersion() throws Exception {
		final String bcver = Platform.getBouncyCastleVersion();
		Assert.assertNull("Se esperaba BC nulo, pero se ha encontrado " + bcver, bcver); //$NON-NLS-1$
	}

}
