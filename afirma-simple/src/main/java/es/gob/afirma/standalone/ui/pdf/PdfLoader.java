/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.pdf;

import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.ArrayList;
import java.util.EventListener;
import java.util.List;

import com.aowagie.text.pdf.PdfReader;

final class PdfLoader {

	private PdfLoader() {
		// No instanciable
	}

	interface PdfLoaderListener extends EventListener {
		void pdfLoaded(boolean isSing, final boolean isMassiveSign, List<BufferedImage> pages, List<Dimension> pageSizes, byte[] pdf);
		void pdfLoadedFailed(Throwable cause);
	}

	static void loadPdf(final boolean isSign, final boolean isMassiveSign, final byte[] inPdf, final PdfLoaderListener pll) {
		new Thread(() ->  {
				try {
					pll.pdfLoaded(
						isSign,
						isMassiveSign,
						Pdf2ImagesConverter.pdf2ImagesUsefulSections(inPdf, 0),
						getPageSizes(inPdf),
						inPdf
					);
				}
				catch(final OutOfMemoryError | IOException e) {
					pll.pdfLoadedFailed(e);
				}
			}
		).start();
	}

	static List<Dimension> getPageSizes(final byte[] inPdf) throws IOException {
		final PdfReader reader = new PdfReader(inPdf);
		final int numberOfPages = reader.getNumberOfPages();
		final List<Dimension> pageSizes = new ArrayList<>(numberOfPages);
		for(int i=1;i<=numberOfPages;i++) {
			final com.aowagie.text.Rectangle rect = reader.getPageSize(i);
			pageSizes.add(
				new Dimension(
					Math.round(rect.getWidth()),
					Math.round(rect.getHeight())
				)
			);
		}
		return pageSizes;
	}

}
