package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.pades.PdfAConverter;

/** Pruebas de conversi&oacute;n a PDF/X.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestPdfAConverter {

	private final static String TEST_FILE = "TEST_PDF.pdf"; //$NON-NLS-1$

	/** Prueba de conversi&oacute;n a PDF/A-1B.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testPdfA1B() throws Exception {

		final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));
        final byte[] result = PdfAConverter.convertToPdfA(testPdf);

        final File saveFile = File.createTempFile("PDFACONVERTED-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        final OutputStream os = new FileOutputStream(saveFile);
        os.write(result);
        os.flush();
        os.close();
        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$
	}
}
