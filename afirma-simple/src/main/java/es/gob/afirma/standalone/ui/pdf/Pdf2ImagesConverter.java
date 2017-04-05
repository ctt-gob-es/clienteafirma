package es.gob.afirma.standalone.ui.pdf;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.rendering.ImageType;
import org.apache.pdfbox.rendering.PDFRenderer;

/** Conversor de PDF a conjunto de im&aacute;genes.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
final class Pdf2ImagesConverter {

	static List<BufferedImage> pdf2Images(final byte[] inPdf) throws IOException {
		List<BufferedImage> pagesAsImages;
		try (final PDDocument document = PDDocument.load(new ByteArrayInputStream(inPdf))) {
			final PDFRenderer pdfRenderer = new PDFRenderer(document);
			pagesAsImages = new ArrayList<>(document.getNumberOfPages());
			for (int i = 0; i < document.getNumberOfPages(); i++) {
				pagesAsImages.add(pdfRenderer.renderImageWithDPI(i, 150, ImageType.RGB));
			}
        }
        return pagesAsImages;
	}

}
