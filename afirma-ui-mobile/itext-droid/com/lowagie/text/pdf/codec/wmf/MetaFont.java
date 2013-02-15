/*
 * $Id: MetaFont.java 3373 2008-05-12 16:21:24Z xlv $
 *
 * Copyright 2001, 2002 Paulo Soares
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

package com.lowagie.text.pdf.codec.wmf;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

import com.lowagie.text.Document;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.pdf.BaseFont;

class MetaFont extends MetaObject {
    private static final String fontNames[] = {
        "Courier", "Courier-Bold", "Courier-Oblique", "Courier-BoldOblique",
        "Helvetica", "Helvetica-Bold", "Helvetica-Oblique", "Helvetica-BoldOblique",
        "Times-Roman", "Times-Bold", "Times-Italic", "Times-BoldItalic",
        "Symbol", "ZapfDingbats"};

    private static final int MARKER_BOLD = 1;
    private static final int MARKER_ITALIC = 2;
    private static final int MARKER_COURIER = 0;
    private static final int MARKER_HELVETICA = 4;
    private static final int MARKER_TIMES = 8;
    private static final int MARKER_SYMBOL = 12;


    private static final int FIXED_PITCH = 1;


    private static final int FF_ROMAN = 1;
    private static final int FF_SWISS = 2;
    private static final int FF_MODERN = 3;
    private static final int FF_SCRIPT = 4;
    private static final int FF_DECORATIVE = 5;
    private static final int BOLDTHRESHOLD = 600;
    private static final int nameSize = 32;
    static final int ETO_OPAQUE = 2;
    static final int ETO_CLIPPED = 4;

    private int height;
    private float angle;
    private int bold;
    private int italic;
    private boolean underline;
    private boolean strikeout;
    private int charset;
    private int pitchAndFamily;
    private String faceName = "arial";
    private BaseFont font = null;

    public MetaFont() {
        this.type = META_FONT;
    }

    public void init(final InputMeta in) throws IOException {
        this.height = Math.abs(in.readShort());
        in.skip(2);
        this.angle = (float)(in.readShort() / 1800.0 * Math.PI);
        in.skip(2);
        this.bold = in.readShort() >= BOLDTHRESHOLD ? MARKER_BOLD : 0;
        this.italic = in.readByte() != 0 ? MARKER_ITALIC : 0;
        this.underline = in.readByte() != 0;
        this.strikeout = in.readByte() != 0;
        this.charset = in.readByte();
        in.skip(3);
        this.pitchAndFamily = in.readByte();
        final byte name[] = new byte[nameSize];
        int k;
        for (k = 0; k < nameSize; ++k) {
            final int c = in.readByte();
            if (c == 0) {
                break;
            }
            name[k] = (byte)c;
        }
        try {
            this.faceName = new String(name, 0, k, "Cp1252");
        }
        catch (final UnsupportedEncodingException e) {
            this.faceName = new String(name, 0, k);
        }
        this.faceName = this.faceName.toLowerCase();
    }

    public BaseFont getFont() {
        if (this.font != null) {
			return this.font;
		}
        final Font ff2 = FontFactory.getFont(this.faceName, BaseFont.CP1252, true, 10, (this.italic != 0 ? Font.ITALIC : 0) | (this.bold != 0 ? Font.BOLD : 0));
        this.font = ff2.getBaseFont();
        if (this.font != null) {
			return this.font;
		}
        String fontName;
        if (this.faceName.indexOf("courier") != -1 || this.faceName.indexOf("terminal") != -1
            || this.faceName.indexOf("fixedsys") != -1) {
            fontName = fontNames[MARKER_COURIER + this.italic + this.bold];
        }
        else if (this.faceName.indexOf("ms sans serif") != -1 || this.faceName.indexOf("arial") != -1
            || this.faceName.indexOf("system") != -1) {
            fontName = fontNames[MARKER_HELVETICA + this.italic + this.bold];
        }
        else if (this.faceName.indexOf("arial black") != -1) {
            fontName = fontNames[MARKER_HELVETICA + this.italic + MARKER_BOLD];
        }
        else if (this.faceName.indexOf("times") != -1 || this.faceName.indexOf("ms serif") != -1
            || this.faceName.indexOf("roman") != -1) {
            fontName = fontNames[MARKER_TIMES + this.italic + this.bold];
        }
        else if (this.faceName.indexOf("symbol") != -1) {
            fontName = fontNames[MARKER_SYMBOL];
        }
        else {
            final int pitch = this.pitchAndFamily & 3;
            final int family = this.pitchAndFamily >> 4 & 7;
            switch (family) {
                case FF_MODERN:
                    fontName = fontNames[MARKER_COURIER + this.italic + this.bold];
                    break;
                case FF_ROMAN:
                    fontName = fontNames[MARKER_TIMES + this.italic + this.bold];
                    break;
                case FF_SWISS:
                case FF_SCRIPT:
                case FF_DECORATIVE:
                    fontName = fontNames[MARKER_HELVETICA + this.italic + this.bold];
                    break;
                default:
                {
                    switch (pitch) {
                        case FIXED_PITCH:
                            fontName = fontNames[MARKER_COURIER + this.italic + this.bold];
                            break;
                        default:
                            fontName = fontNames[MARKER_HELVETICA + this.italic + this.bold];
                            break;
                    }
                }
            }
        }
        try {
            this.font = BaseFont.createFont(fontName, "Cp1252", false);
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }

        return this.font;
    }

    public float getAngle() {
        return this.angle;
    }

    public boolean isUnderline() {
        return this.underline;
    }

    public boolean isStrikeout() {
        return this.strikeout;
    }

    public float getFontSize(final MetaState state) {
        return Math.abs(state.transformY(this.height) - state.transformY(0)) * Document.wmfFontCorrection;
    }
}
