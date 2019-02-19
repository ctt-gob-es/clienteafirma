package es.gob.afirma.core.misc;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.signers.AOSignConstants;

/** Pruebas de obtenci&oacute;n de constantes.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestConstants {

	/** Prueba de obtenci&oacute;n de nombre de algoritmo de huella <i>NONE</i>. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testDigestNone() {
		final String digestAlgo = AOSignConstants.getDigestAlgorithmName("NONEwithRSA"); //$NON-NLS-1$
		System.out.println(digestAlgo);
		Assert.assertEquals("NONE", digestAlgo); //$NON-NLS-1$
	}

}
