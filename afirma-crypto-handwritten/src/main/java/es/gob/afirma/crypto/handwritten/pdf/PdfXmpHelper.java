package es.gob.afirma.crypto.handwritten.pdf;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;

import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.xml.xmp.XmpWriter;

/** Funciones de XMP para PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfXmpHelper {

	/** A&ntilde;ade los datos de una firma biom&eacute;trica a un PDF en forma de XMP.
	 * @param pdfStamper <code>PDFStamper</code>.
	 * @param xmpData Datos XMP a a&ntilde;adir.
	 * @throws IOException En caso de errores en el tratamiento de datos. */
	static void addBioXmpDataToPdf(final PdfStamper pdfStamper,
			                       final byte[] xmpData) throws IOException {
		if (pdfStamper == null) {
			throw new IllegalArgumentException(
				"El PDF de entrada no puede ser nulo" //$NON-NLS-1$
			);
		}
		pdfStamper.setXmpMetadata(xmpData);
	}

	/** Construye una estructura XMP de metadatos para su posterior inserci&oacute;n en un PDF.
	 * @param signs Firmas biom&eacute;tricas.
	 * @return Estructura XMP.
	 * @throws IOException Cuando ocurre un error al construir la estructura. */
	public static byte[] buildXmp(final List<XmpSignStructure> signs) throws IOException {

		// Datos a insertar como XMP
		final BioMetadataSchema schema = new BioMetadataSchema(signs);

		// Insertamos los datos en el XMP
		final ByteArrayOutputStream os = new ByteArrayOutputStream();
		final XmpWriter xmp = new XmpWriter(os);
		xmp.addRdfDescription(schema);
		xmp.close();

		return os.toByteArray();
	}

}
