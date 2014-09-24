package es.gob.afirma.crypto.handwritten.pdf;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.xml.xmp.XmpWriter;

import es.gob.afirma.crypto.handwritten.SignerInfoBean;

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
	 * @param bioSignData Datos biom&eacute;tricos de firma.
	 * @param pkDn DN de la clave de firma.
	 * @param signerInfo Informaci&oacute;n del firmante.
	 * @return Estructura XMP.
	 * @throws IOException Cuando ocurre un error al construir la estructura. */
	public static byte[] buildXmp(final byte[] bioSignData,
			                      final String pkDn,
			                      final SignerInfoBean signerInfo) throws IOException {

		// Datos a insertar como XMP
		final BioMetadataSchema schema = new BioMetadataSchema();
		if (bioSignData != null) {
			schema.addBioSignData(bioSignData);
		}

		// DN de la clave de firma
		if (pkDn != null) {
			schema.addKeyDn(pkDn);
		}

		// Datos del firmante
		if (signerInfo != null &&
				(signerInfo.getName() != null ||
				signerInfo.getSurname1() != null ||
				signerInfo.getSurname2() != null ||
				signerInfo.getId() != null)) {
			schema.addSignerData(signerInfo.getName(), signerInfo.getSurname1(), signerInfo.getSurname2(), signerInfo.getId());
		}

		// Insertamos los datos en el XMP
		final ByteArrayOutputStream os = new ByteArrayOutputStream();
		final XmpWriter xmp = new XmpWriter(os);
		xmp.addRdfDescription(schema);
		xmp.close();

		return os.toByteArray();
	}

}
