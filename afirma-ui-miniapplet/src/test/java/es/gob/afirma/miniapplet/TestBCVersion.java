package es.gob.afirma.miniapplet;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.Platform;

/** Prueba la obtenci&oacute;n de versi&oacute;n de BouncyCastle.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestBCVersion {

	/** Prueba la obtenci&oacute;n de versi&oacute;n de BouncyCastle.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	public void getBCVersion() throws Exception {
		final String bcver = Platform.getBouncyCastleVersion();
		Assert.assertEquals("Se esperaba BC 1.46, pero se ha encontrado " + bcver, "1.46", bcver); //$NON-NLS-1$ //$NON-NLS-2$
	}

}
