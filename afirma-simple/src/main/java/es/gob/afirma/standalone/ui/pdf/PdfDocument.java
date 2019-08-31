package es.gob.afirma.standalone.ui.pdf;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.List;

/** Almacena los datos para la previsualizaci&oacute;n de
 * un PDF y aquellos necesarios para regenerarlos. */
final class PdfDocument {

	private byte[] bytesPdf;

	/** Recarga el listado de las previsualizaciones que se necesitan de un PDF
	 * en base a la p&aacute;fina en la que nos encontremos.
	 * @param previews Listado de ministaturas.
	 * @param currentPage P&aacute;fina actual.
	 * @throws IOException Cuando ocurre un error al generar las miniaturas de
	 *                     las p&aacute;ginas. */
	void loadNewPages(final List<BufferedImage> previews, final int currentPage) throws IOException {
		if (this.bytesPdf != null) {
			try {
				Pdf2ImagesConverter.updateUsefulSections(this.bytesPdf, currentPage, previews);
			} catch (final IOException e) {
				throw new IOException("Error al crear previsualizaciones de pagina", e); //$NON-NLS-1$
			}
		}
	}

	/** Obtiene el PDF.
	 * @return Contenido del PDF. */
	byte[] getBytesPdf() {
		return this.bytesPdf;
	}

	/** Establece el PDF.
	 * @param inPdf Contenido del PDF. */
	void setBytesPdf( final byte[] inPdf) {
		this.bytesPdf = inPdf;
	}
}
