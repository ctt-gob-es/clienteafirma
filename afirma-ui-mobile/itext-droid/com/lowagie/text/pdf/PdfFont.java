/*
 * $Id: PdfFont.java 3373 2008-05-12 16:21:24Z xlv $
 *
 * Copyright 1999, 2000, 2001, 2002 Bruno Lowagie
 *
 * The contents of this file are subject to the Mozilla Public License Version 1.1
 * (the "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the License.
 *
 * The Original Code is 'iText, a free JAVA-PDF library'.
 *
 * The Initial Developer of the Original Code is Bruno Lowagie. Portions created by
 * the Initial Developer are Copyright (C) 1999, 2000, 2001, 2002 by Bruno Lowagie.
 * All Rights Reserved.
 * Co-Developer of the code is Paulo Soares. Portions created by the Co-Developer
 * are Copyright (C) 2000, 2001, 2002 by Paulo Soares. All Rights Reserved.
 *
 * Contributor(s): all the names of the contributors are added in the source code
 * where applicable.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * LGPL license (the "GNU LIBRARY GENERAL PUBLIC LICENSE"), in which case the
 * provisions of LGPL are applicable instead of those above.  If you wish to
 * allow use of your version of this file only under the terms of the LGPL
 * License and not to allow others to use your version of this file under
 * the MPL, indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by the LGPL.
 * If you do not delete the provisions above, a recipient may use your version
 * of this file under either the MPL or the GNU LIBRARY GENERAL PUBLIC LICENSE.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the MPL as stated above or under the terms of the GNU
 * Library General Public License as published by the Free Software Foundation;
 * either version 2 of the License, or any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library general Public License for more
 * details.
 *
 * If you didn't download this code from the following link, you should check if
 * you aren't using an obsolete version:
 * http://www.lowagie.com/iText/
 */

package com.lowagie.text.pdf;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;

/**
 * <CODE>PdfFont</CODE> is the Pdf Font object.
 * <P>
 * Limitation: in this class only base 14 Type 1 fonts (courier, courier bold, courier oblique,
 * courier boldoblique, helvetica, helvetica bold, helvetica oblique, helvetica boldoblique,
 * symbol, times roman, times bold, times italic, times bolditalic, zapfdingbats) and their
 * standard encoding (standard, MacRoman, (MacExpert,) WinAnsi) are supported.<BR>
 * This object is described in the 'Portable Document Format Reference Manual version 1.3'
 * section 7.7 (page 198-203).
 *
 * @see		PdfName
 * @see		PdfDictionary
 * @see		BadPdfFormatException
 */

class PdfFont implements Comparable {


    /** the font metrics. */
    private final BaseFont font;

    /** the size. */
    private final float size;

    /** an image. */
    private Image image;

    private float hScale = 1;

    // constructors

    PdfFont(final BaseFont bf, final float size) {
        this.size = size;
        this.font = bf;
    }

    // methods

    /**
     * Compares this <CODE>PdfFont</CODE> with another
     *
     * @param	object	the other <CODE>PdfFont</CODE>
     * @return	a value
     */

    @Override
	public int compareTo(final Object object) {
        if (this.image != null) {
			return 0;
		}
        if (object == null) {
            return -1;
        }
        PdfFont pdfFont;
        try {
            pdfFont = (PdfFont) object;
            if (this.font != pdfFont.font) {
                return 1;
            }
            if (this.size() != pdfFont.size()) {
                return 2;
            }
            return 0;
        }
        catch(final ClassCastException cce) {
            return -2;
        }
    }

    /**
     * Returns the size of this font.
     *
     * @return		a size
     */

    float size() {
        if (this.image == null) {
			return this.size;
		} else {
            return this.image.getScaledHeight();
        }
    }

    /**
     * Returns the approximative width of 1 character of this font.
     *
     * @return		a width in Text Space
     */

    float width() {
        return width(' ');
    }

    /**
     * Returns the width of a certain character of this font.
     *
     * @param		character	a certain character
     * @return		a width in Text Space
     */

    float width(final int character) {
        if (this.image == null) {
			return this.font.getWidthPoint(character, this.size) * this.hScale;
		} else {
			return this.image.getScaledWidth();
		}
    }

    float width(final String s) {
        if (this.image == null) {
			return this.font.getWidthPoint(s, this.size) * this.hScale;
		} else {
			return this.image.getScaledWidth();
		}
    }

    BaseFont getFont() {
        return this.font;
    }

    void setImage(final Image image) {
        this.image = image;
    }

    static PdfFont getDefaultFont() {
        try {
            final BaseFont bf = BaseFont.createFont(BaseFont.HELVETICA, BaseFont.WINANSI, false);
            return new PdfFont(bf, 12);
        }
        catch (final Exception ee) {
            throw new ExceptionConverter(ee);
        }
    }
    void setHorizontalScaling(final float hScale) {
        this.hScale = hScale;
    }
}
