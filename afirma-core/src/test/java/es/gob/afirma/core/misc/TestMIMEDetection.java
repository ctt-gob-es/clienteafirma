package es.gob.afirma.core.misc;

import org.junit.Assert;
import org.junit.Test;

/** Pruebas de detecci&oacute;n de formato de varios tipos de documentos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestMIMEDetection {

	/** Prueba la detecci&oacute;n de documentos PDF.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	public void testPdfDetection() throws Exception {
		final byte[] file = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("pdf.pdf")); //$NON-NLS-1$
		final String mime = new MimeHelper(file).getMimeType();
		Assert.assertEquals("El MIME-Type obtenido no es correcto para el fichero PDF: " + mime, "application/pdf", mime); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Prueba la detecci&oacute;n de documentos TXT.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	public void testTxtDetection() throws Exception {
		final byte[] file = "Hola Mundo!!".getBytes(); //$NON-NLS-1$
		final String mime = new MimeHelper(file).getMimeType();
		Assert.assertEquals("El MIME-Type obtenido no es correcto para un fichero de texto: " + mime, "text/plain", mime); //$NON-NLS-1$ //$NON-NLS-2$
	}
}
