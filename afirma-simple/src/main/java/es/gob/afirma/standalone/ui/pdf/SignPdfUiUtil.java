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
import java.awt.GraphicsEnvironment;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;

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

}
