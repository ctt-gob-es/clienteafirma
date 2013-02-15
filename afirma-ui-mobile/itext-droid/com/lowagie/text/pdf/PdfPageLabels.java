/*
 * $Id: PdfPageLabels.java 3373 2008-05-12 16:21:24Z xlv $
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

package com.lowagie.text.pdf;

import java.io.IOException;
import java.util.HashMap;

import com.lowagie.text.ExceptionConverter;

/** Page labels are used to identify each
 * page visually on the screen or in print.
 * @author  Paulo Soares (psoares@consiste.pt)
 */
class PdfPageLabels {

    /** Logical pages will have the form 1,2,3,...
     */
    private static final int DECIMAL_ARABIC_NUMERALS = 0;





    /** Dictionary values to set the logical page styles
     */
    private static PdfName numberingStyle[] = new PdfName[]{PdfName.D, PdfName.R,
                new PdfName("r"), PdfName.A, new PdfName("a")};
    /** The sequence of logical pages. Will contain at least a value for page 1
     */
    private final HashMap map;

    /** Creates a new PdfPageLabel with a default logical page 1
     */
    public PdfPageLabels() {
        this.map = new HashMap();
        addPageLabel(1, DECIMAL_ARABIC_NUMERALS, null, 1);
    }

    /** Adds or replaces a page label.
     * @param page the real page to start the numbering. First page is 1
     * @param numberStyle the numbering style such as LOWERCASE_ROMAN_NUMERALS
     * @param text the text to prefix the number. Can be <CODE>null</CODE> or empty
     * @param firstPage the first logical page number
     */
    private void addPageLabel(final int page, final int numberStyle, final String text, final int firstPage) {
        if (page < 1 || firstPage < 1) {
			throw new IllegalArgumentException("In a page label the page numbers must be greater or equal to 1.");
		}
        final PdfDictionary dic = new PdfDictionary();
        if (numberStyle >= 0 && numberStyle < numberingStyle.length) {
			dic.put(PdfName.S, numberingStyle[numberStyle]);
		}
        if (text != null) {
			dic.put(PdfName.P, new PdfString(text, PdfObject.TEXT_UNICODE));
		}
        if (firstPage != 1) {
			dic.put(PdfName.ST, new PdfNumber(firstPage));
		}
        this.map.put(new Integer(page - 1), dic);
    }









    /** Gets the page label dictionary to insert into the document.
     * @return the page label dictionary
     */
    PdfDictionary getDictionary(final PdfWriter writer) {
        try {
            return PdfNumberTree.writeTree(this.map, writer);
        }
        catch (final IOException e) {
            throw new ExceptionConverter(e);
        }
    }





    private static class PdfPageLabelFormat {

        private final int physicalPage;
        private final int numberStyle;
        private final String prefix;
        private final int logicalPage;

        /** Creates a page label format.
         * @param physicalPage the real page to start the numbering. First page is 1
         * @param numberStyle the numbering style such as LOWERCASE_ROMAN_NUMERALS
         * @param prefix the text to prefix the number. Can be <CODE>null</CODE> or empty
         * @param logicalPage the first logical page number
         */
        private PdfPageLabelFormat(final int physicalPage, final int numberStyle, final String prefix, final int logicalPage) {
            this.physicalPage = physicalPage;
            this.numberStyle = numberStyle;
            this.prefix = prefix;
            this.logicalPage = logicalPage;
        }
    }
}