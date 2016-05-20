package es.gob.afirma.standalone.ui.pdf;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;

/** Conversor de PDF a conjunto de im&aacute;genes.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
final class Pdf2ImagesConverter {

	static List<BufferedImage> pdf2Images(final byte[] inPdf) throws IOException {
		final PDDocument document = PDDocument.load(new ByteArrayInputStream(inPdf));
		final List<?> pdfPages = document.getDocumentCatalog().getAllPages();
		final List<BufferedImage> pagesAsImages = new ArrayList<>(pdfPages.size());
		for (final Object page : pdfPages) {
           pagesAsImages.add(((PDPage) page).convertToImage());
        }
		document.close();
        return pagesAsImages;
	}

}
