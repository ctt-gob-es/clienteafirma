/*
 * Copyright 2004 by Paulo Soares.
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

import java.awt.Font;
import java.util.HashMap;

import com.lowagie.text.ExceptionConverter;
/** Default class to map awt fonts to BaseFont.
 * @author Paulo Soares (psoares@consiste.pt)
 */

class DefaultFontMapper implements FontMapper {

    /** A representation of BaseFont parameters.
     */
    static class BaseFontParameters {
        /** The font name.
         */
        String fontName;
        /** The encoding for that font.
         */
        String encoding;
        /** The embedding for that font.
         */
        boolean embedded;
        /** Whether the font is cached of not.
         */
        boolean cached;
        /** The font bytes for ttf and afm.
         */
        byte ttfAfm[];
        /** The font bytes for pfb.
         */
        byte pfb[];

        /** Constructs default BaseFont parameters.
         * @param fontName the font name or location
         */
        private BaseFontParameters(final String fontName) {
            this.fontName = fontName;
            this.encoding = BaseFont.CP1252;
            this.embedded = BaseFont.EMBEDDED;
            this.cached = BaseFont.CACHED;
        }
    }

    /** Maps aliases to names.
     */
    private final HashMap aliases = new HashMap();
    /** Maps names to BaseFont parameters.
     */
    private final HashMap mapper = new HashMap();
    /**
     * Returns a BaseFont which can be used to represent the given AWT Font
     *
     * @param	font		the font to be converted
     * @return	a BaseFont which has similar properties to the provided Font
     */

    @Override
	public BaseFont awtToPdf(final Font font) {
        try {
            final BaseFontParameters p = getBaseFontParameters(font.getFontName());
            if (p != null) {
				return BaseFont.createFont(p.fontName, p.encoding, p.embedded, p.cached, p.ttfAfm, p.pfb);
			}
            String fontKey = null;
            final String logicalName = font.getName();

            if (logicalName.equalsIgnoreCase("DialogInput") || logicalName.equalsIgnoreCase("Monospaced") || logicalName.equalsIgnoreCase("Courier")) {

                if (font.isItalic()) {
                    if (font.isBold()) {
                        fontKey = BaseFont.COURIER_BOLDOBLIQUE;

                    } else {
                        fontKey = BaseFont.COURIER_OBLIQUE;
                    }

                } else {
                    if (font.isBold()) {
                        fontKey = BaseFont.COURIER_BOLD;

                    } else {
                        fontKey = BaseFont.COURIER;
                    }
                }

            } else if (logicalName.equalsIgnoreCase("Serif") || logicalName.equalsIgnoreCase("TimesRoman")) {

                if (font.isItalic()) {
                    if (font.isBold()) {
                        fontKey = BaseFont.TIMES_BOLDITALIC;

                    } else {
                        fontKey = BaseFont.TIMES_ITALIC;
                    }

                } else {
                    if (font.isBold()) {
                        fontKey = BaseFont.TIMES_BOLD;

                    } else {
                        fontKey = BaseFont.TIMES_ROMAN;
                    }
                }

            } else {  // default, this catches Dialog and SansSerif

                if (font.isItalic()) {
                    if (font.isBold()) {
                        fontKey = BaseFont.HELVETICA_BOLDOBLIQUE;

                    } else {
                        fontKey = BaseFont.HELVETICA_OBLIQUE;
                    }

                } else {
                    if (font.isBold()) {
                        fontKey = BaseFont.HELVETICA_BOLD;
                    } else {
                        fontKey = BaseFont.HELVETICA;
                    }
                }
            }
            return BaseFont.createFont(fontKey, BaseFont.CP1252, false);
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }







    /** Looks for a BaseFont parameter associated with a name.
     * @param name the name
     * @return the BaseFont parameter or <CODE>null</CODE> if not found.
     */
    BaseFontParameters getBaseFontParameters(final String name) {
        final String alias = (String)this.aliases.get(name);
        if (alias == null) {
			return (BaseFontParameters)this.mapper.get(name);
		}
        final BaseFontParameters p = (BaseFontParameters)this.mapper.get(alias);
        if (p == null) {
			return (BaseFontParameters)this.mapper.get(name);
		} else {
			return p;
		}
    }





    public HashMap getMapper() {
        return this.mapper;
    }

    public HashMap getAliases() {
        return this.aliases;
    }
}
