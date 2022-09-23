/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.pdf;

import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.io.InputStream;

enum FontResource {

	// Ficheros de fuentes cargados de la biblioteca afirma-crypto-pdf
	TIMES_ROMAN(  "Times Roman",  "/fonts/times.ttf",        2), //$NON-NLS-1$//$NON-NLS-2$
	COURIER(      "Courier",      "/fonts/courier.ttf",      0), //$NON-NLS-1$ //$NON-NLS-2$
	HELVETICA(    "Helvetica",    "/fonts/helvetica.ttf",    1); //$NON-NLS-1$ //$NON-NLS-2$

	private final String fontName;
	private Font font = null;
	private final int pdfFontIndex;

    private final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
    private final Font[] fonts = this.ge.getAllFonts(); // Get the fonts

	FontResource(final String name,
			             final String file,
			             final int index) {
		this.fontName = name;
		this.pdfFontIndex = index;
		for (final Font f : this.fonts) {
	      if (f.getFontName().equals(this.fontName)) {
	    	  this.font = f;
	      }
	    }
		if (this.font == null) {
	    	try (
        		final InputStream is = FontResource.class.getResourceAsStream(file);
            ) {
	    		this.font = Font.createFont(Font.TRUETYPE_FONT, is);
	    		this.ge.registerFont(this.font);
	    	}
	    	catch(final Exception e) {
	    		throw new IllegalStateException(
    				"Error creando el tipo de letra " + this.fontName + ": " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
	    	}
		}
	}

	@Override
	public String toString() {
		return this.fontName;
	}

	Font getFont() {
		return this.font;
	}

	static FontResource[] getAllFontresources() {
		return new FontResource[] {
			COURIER,
			HELVETICA,
			TIMES_ROMAN
		};
	}

	String getPdfFontIndex() {
		return Integer.toString(this.pdfFontIndex);
	}

}
