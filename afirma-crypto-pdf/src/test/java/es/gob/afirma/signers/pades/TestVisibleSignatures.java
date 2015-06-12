package es.gob.afirma.signers.pades;

import org.junit.Test;

/** Pruebas de firmas visibles.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestVisibleSignatures {

	/** Prueba de texto en capa con patrones. */
	@SuppressWarnings("static-method")
	@Test
	public void testLayerText() {
		System.out.println(
			PdfVisibleAreasUtils.getLayerText("Texto $$SIGNDATE=hh:mm:ss$$", null, null) //$NON-NLS-1$
		);
	}

}
