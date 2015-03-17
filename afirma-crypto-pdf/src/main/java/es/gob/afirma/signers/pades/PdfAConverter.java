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

	/** Tipos de PDF/X. */
	public enum PDFX {
		/** PDF normal. */
		PDF,
		/** PDF/A (ISO 19005). */
		PDFA,
		/** PDF/A-1 (ISO 19005-1). */
		PDFA1,
		/** PDF/A-1a (ISO 19005-1 Level A). */
		PDFA1A,
		/** PDF/A-1b (ISO 19005-1 Level B). */
		PDFA1B,
		/** PDF/A-2 (ISO 19005 parte 2). */
		PDFA2,
		/** PDF/A-3 (ISO 19005-3:2012 Parte 3). */
		PDFA3
	}

	private static final int PDF_MAX_VERSION = 7;
	private static final int PDF_MIN_VERSION = 4;

	private PdfAConverter() {
		// No instanciable
	}

	/** Convierte un PDF en PDF/A1B.
	 * @param originalPdf PDF original.
	 * @return PDF original convertido a PDF/A1B.
	 * @throws IOException Si ocurre cualquier problema en la conversi&oacute;n. */
	public static byte[] convertToPdfA(final byte[] originalPdf) throws IOException {
		return convertToPdfA(originalPdf, PDF_MAX_VERSION);
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

		if (outputPdfVersion < PDF_MIN_VERSION || outputPdfVersion > PDF_MAX_VERSION) {
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

	/** Indica el nivel de adherencia a las especificaciones PDF/A del PDF indicado.
	 * @param pdfIn PDF de entrada.
	 * @return Nivel de adherencia a las especificaciones PDF/A.
	 * @throws IOException Si hay problemas en el tratamiento de datos. */
	public static PDFX getPdfXConformance(final byte[] pdfIn) throws IOException {
		final PdfReader reader = new PdfReader(pdfIn);
		final String meta = new String(reader.getMetadata());
		if (meta.contains("<pdfaid:part>1</pdfaid:part>")) { //$NON-NLS-1$
			if (meta.contains("<pdfaid:conformance>A</pdfaid:conformance>")) { //$NON-NLS-1$
				return PDFX.PDFA1A;
			}
			else if (meta.contains("<pdfaid:conformance>B</pdfaid:conformance>")) { //$NON-NLS-1$
				return PDFX.PDFA1B;
			}
			return PDFX.PDFA1;
		}
		if (meta.contains("<pdfaid:part>2</pdfaid:part>")) { //$NON-NLS-1$
			return PDFX.PDFA2;
		}
		if (meta.contains("<pdfaid:part>3</pdfaid:part>")) { //$NON-NLS-1$
			return PDFX.PDFA3;
		}
		return PDFX.PDF;
	}

}
