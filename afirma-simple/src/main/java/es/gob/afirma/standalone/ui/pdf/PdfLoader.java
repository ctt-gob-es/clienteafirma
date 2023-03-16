/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.pdf;

import java.io.IOException;
import java.util.EventListener;

final class PdfLoader {

	private PdfLoader() {
		// No instanciable
	}

	interface PdfLoaderListener extends EventListener {
		void pdfLoaded(boolean isSign, final boolean isMassiveSign, byte[] pdf) throws IOException;
		void pdfLoadedFailed(Throwable cause);
	}

	static void loadPdf(final boolean isSign, final boolean isMassiveSign, final byte[] inPdf, final PdfLoaderListener pll) {
		new Thread(() ->  {
				try {
					pll.pdfLoaded(
						isSign,
						isMassiveSign,
						inPdf
					);
				}
				catch(final OutOfMemoryError | IOException e) {
					pll.pdfLoadedFailed(e);
				}
			}
		).start();
	}
}
