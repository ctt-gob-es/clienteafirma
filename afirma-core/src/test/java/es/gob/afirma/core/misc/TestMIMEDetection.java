package es.gob.afirma.core.misc;

import java.io.InputStream;

import org.junit.Assert;
import org.junit.Test;

/** Pruebas de detecci&oacute;n de formato de varios tipos de documentos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestMIMEDetection {

	/** Prueba la detecci&oacute;n de documentos PDF.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testPdfDetection() throws Exception {
		final byte[] file;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("pdf.pdf") //$NON-NLS-1$
		) {
			file = AOUtil.getDataFromInputStream(is);
		}
		final String mime = new MimeHelper(file).getMimeType();
		Assert.assertEquals("El MIME-Type obtenido no es correcto para el fichero PDF: " + mime, "application/pdf", mime); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Prueba la detecci&oacute;n de documentos TXT.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testTxtDetection() throws Exception {
		final byte[] file = "Hola Mundo!!".getBytes(); //$NON-NLS-1$
		final String mime = new MimeHelper(file).getMimeType();
		Assert.assertEquals("El MIME-Type obtenido no es correcto para un fichero de texto: " + mime, "text/plain", mime); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Prueba la detecci&oacute;n de documentos XML.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testXmlDetection() throws Exception {
		final byte[] file;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("xml.xml") //$NON-NLS-1$
		) {
			file = AOUtil.getDataFromInputStream(is);
		}
		final String mime = new MimeHelper(file).getMimeType();
		Assert.assertEquals("El MIME-Type obtenido no es correcto para un fichero XML: " + mime, "text/xml", mime); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Prueba la detecci&oacute;n de archivos RAR.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testRarDetection() throws Exception {
		final byte[] file;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("rar.rar") //$NON-NLS-1$
		) {
			file = AOUtil.getDataFromInputStream(is);
		}
		final String mime = new MimeHelper(file).getMimeType();
		Assert.assertEquals("El MIME-Type obtenido no es correcto para un fichero RAR: " + mime, "application/x-rar-compressed", mime); //$NON-NLS-1$ //$NON-NLS-2$
	}
}
