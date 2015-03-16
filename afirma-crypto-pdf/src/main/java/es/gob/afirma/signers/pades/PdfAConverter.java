package es.gob.afirma.signers.pades;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Image;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.pdf.PdfString;
import com.lowagie.text.pdf.PdfWriter;

/** Conversor de PDF a PDF/X.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfAConverter {

	/** Convierte un PDF en PDF/A1B.
	 * @param originalPdf PDF original.
	 * @return PDF original convertido a PDF/A1B.
	 * @throws IOException Si ocurre cualquier problema en la conversi&oacute;n. */
	public static byte[] convertToPdfA(final byte[] originalPdf) throws IOException {
		return convertToPdfA(originalPdf, 7);
	}

	/** Convierte un PDF en PDF/A1B.
	 * @param pdf PDF original.
	 * @param outputPdfVersion Versi&oacute;n (de la especificaci&oacute;n PDF) del PDF de salida.
	 * @return PDF original convertido a PDF/A1B.
	 * @throws IOException Si ocurre cualquier problema en la conversi&oacute;n. */
	public static byte[] convertToPdfA(final byte[] pdf, final int outputPdfVersion) throws IOException {

		if (pdf == null) {
			throw new IllegalArgumentException("El PDF de entrada no puede ser nulo"); //$NON-NLS-1$
		}

		if (outputPdfVersion < 4 || outputPdfVersion > 7) {
			throw new IllegalArgumentException(
				"La version del PDF de salida debe estar comprendida entre 4 y 7: " + outputPdfVersion //$NON-NLS-1$
			);
		}

		final ByteArrayOutputStream baos = new ByteArrayOutputStream();

		final PdfReader reader = new PdfReader(pdf);

		final Calendar cal = new GregorianCalendar();

		final PdfStamper stamper;
		try {
			stamper = new PdfStamper(reader, baos, cal);
		}
		catch (final DocumentException e) {
			throw new IOException(e);
		}

        final PdfWriter writer = stamper.getWriter();
        writer.setPDFXConformance(PdfWriter.PDFA1B);

		final Document document = new Document();
		document.open();

		final int numberPages = reader.getNumberOfPages();

		final PdfDictionary outi = new PdfDictionary(PdfName.OUTPUTINTENT);
        outi.put(PdfName.OUTPUTCONDITIONIDENTIFIER, new PdfString("sRGB IEC61966-2.1")); //$NON-NLS-1$
        outi.put(PdfName.INFO, new PdfString("sRGB IEC61966-2.1")); //$NON-NLS-1$
        outi.put(PdfName.S, PdfName.GTS_PDFA1);
        writer.getExtraCatalog().put(PdfName.OUTPUTINTENTS, new PdfArray(outi));

        try {
	        for(int i=0; i< numberPages; i++){
	            document.add(Image.getInstance(writer.getImportedPage(reader, i+1)));
	        }
        }
        catch (final DocumentException e) {
			throw new IOException(e);
		}

        writer.createXmpMetadata();

        document.close();

        final HashMap<?, ?> info = reader.getInfo();
        stamper.setMoreInfo(info);
        try {
			stamper.close(cal);
		}
        catch (final Exception e) {
        	throw new IOException(e);
		}

        return baos.toByteArray();

	}

}
