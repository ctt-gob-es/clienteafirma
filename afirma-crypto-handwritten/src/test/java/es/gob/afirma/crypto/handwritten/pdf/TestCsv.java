package es.gob.afirma.crypto.handwritten.pdf;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.Calendar;
import java.util.GregorianCalendar;

import org.junit.Test;

import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.misc.AOUtil;

/** Prueba de inserci&oacute;n de CSV.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCsv {

	/** Prueba de inserci&oacute;n de CSV.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCsvInsertion() throws Exception {
		// PDF de ejemplo
		final PdfReader reader = new PdfReader(
			AOUtil.getDataFromInputStream(
				ClassLoader.getSystemResourceAsStream("TEST_PDF.pdf") //$NON-NLS-1$
			)
		);
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final Calendar globalDate = new GregorianCalendar();
		final PdfStamper stamper = new PdfStamper(reader, baos, globalDate);

		final Csv csv = new Csv(1, "TEXTO DEL CSV", 40, 180, 0, 0, 1000, 1000); //$NON-NLS-1$
		csv.applyCsv(stamper);

		stamper.close(globalDate);
		reader.close();

        // Guardamos el resultado
        final File tmpFile = File.createTempFile("TESTCSV_", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        final OutputStream fos = new FileOutputStream(tmpFile);
        fos.write(baos.toByteArray());
        fos.flush();
        fos.close();

	}
}
