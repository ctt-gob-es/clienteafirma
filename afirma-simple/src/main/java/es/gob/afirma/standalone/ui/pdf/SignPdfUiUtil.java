/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.pdf;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GraphicsEnvironment;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import com.aowagie.text.pdf.PdfReader;

final class SignPdfUiUtil {

	private SignPdfUiUtil() {
		// No instanciable
	}

	public static Rectangle getScreenBounds(final Component wnd) {

	    final Rectangle sb;
	    if(wnd == null || wnd.getGraphicsConfiguration() == null) {
	        sb = GraphicsEnvironment
	           .getLocalGraphicsEnvironment()
	           .getDefaultScreenDevice()
	           .getDefaultConfiguration()
	           .getBounds();
        }
	    else {
	        sb = wnd.getGraphicsConfiguration().getBounds();
        }

	    final Insets si = getScreenInsets(wnd);

	    sb.x     +=si.left;
	    sb.y     +=si.top;
	    sb.width -=si.left+si.right;
	    sb.height-=si.top+si.bottom;

	    return sb;
    }

	private static Insets getScreenInsets(final Component wnd) {
	    if(wnd == null || wnd.getToolkit() == null || wnd.getGraphicsConfiguration() == null) {
	        return Toolkit.getDefaultToolkit().getScreenInsets(GraphicsEnvironment
	           .getLocalGraphicsEnvironment()
	           .getDefaultScreenDevice()
	           .getDefaultConfiguration());
        }
		return wnd.getToolkit().getScreenInsets(wnd.getGraphicsConfiguration());
    }

	static List<Dimension> getPageSizes(final byte[] inPdf, final char[] pwdChars) throws IOException {
		final byte[] password = pwdChars != null ? new String(pwdChars).getBytes(StandardCharsets.UTF_8) : null;
		final PdfReader reader = new PdfReader(inPdf, password);
		final int numberOfPages = reader.getNumberOfPages();
		final List<Dimension> pageSizes = new ArrayList<>(numberOfPages);
		for(int i=1;i<=numberOfPages;i++) {
			final com.aowagie.text.Rectangle rect = reader.getPageSizeWithRotation(i);
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
