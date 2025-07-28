package es.gob.afirma.signers.pades;

import org.junit.Assert;
import org.junit.Test;

/** Pruebas de firmas visibles.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestVisibleSignatures {

	/** Prueba de texto en capa con patrones. */
	@SuppressWarnings("static-method")
	@Test
	public void testLayerText() {
		System.out.println(
			PdfVisibleAreasUtils.getLayerText("Texto $$SIGNDATE=hh:mm:ss$$", null, null, null, null, null, false, null) //$NON-NLS-1$
		);
	}

	/**
	 * Comprueba que la funci&oacute;n de ofuscaci&oacute;n de identificadores
	 * de usuario para las firmas visibles PDF funcione correctamente.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testObfuscateText() {

		final PdfTextMask mask = new PdfTextMask();
		Assert.assertEquals("***4567**", PdfVisibleAreasUtils.obfuscateIds("12345678X", mask)); //$NON-NLS-1$ //$NON-NLS-2$
		Assert.assertEquals("****4567*", PdfVisibleAreasUtils.obfuscateIds("L1234567X", mask)); //$NON-NLS-1$ //$NON-NLS-2$
		Assert.assertEquals("*****3456", PdfVisibleAreasUtils.obfuscateIds("ABC123456", mask)); //$NON-NLS-1$ //$NON-NLS-2$
		Assert.assertEquals("*****4567***", PdfVisibleAreasUtils.obfuscateIds("XY12345678AB", mask)); //$NON-NLS-1$ //$NON-NLS-2$
		Assert.assertEquals("*****23XY", PdfVisibleAreasUtils.obfuscateIds("ABCD123XY", mask)); //$NON-NLS-1$ //$NON-NLS-2$

	}
}
