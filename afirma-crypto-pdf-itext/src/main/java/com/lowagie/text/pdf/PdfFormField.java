/*
 * Copyright 2002 by Paulo Soares.
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
import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.Rectangle;

/** Implements form fields.
 *
 * @author Paulo Soares (psoares@consiste.pt)
 */
public class PdfFormField extends PdfAnnotation {

    static final int FF_READ_ONLY = 1;
    static final int FF_REQUIRED = 2;

    private static final int FF_NO_TOGGLE_TO_OFF = 16384;
    static final int FF_RADIO = 32768;
    static final int FF_PUSHBUTTON = 65536;
    static final int FF_MULTILINE = 4096;
    static final int FF_PASSWORD = 8192;
    static final int FF_COMBO = 131072;
    static final int FF_EDIT = 262144;
    static final int FF_FILESELECT = 1048576;

    static final int FF_DONOTSPELLCHECK = 4194304;
    static final int FF_DONOTSCROLL = 8388608;
    static final int FF_COMB = 16777216;
    static final int FF_RADIOSINUNISON = 1 << 25;

    static final int Q_CENTER = 1;
    static final int Q_RIGHT = 2;

















    private static PdfName mergeTarget[] = {PdfName.FONT, PdfName.XOBJECT, PdfName.COLORSPACE, PdfName.PATTERN};

    /** Holds value of property parent. */
    private PdfFormField parent;

    private ArrayList kids;



    /** Creates new PdfFormField */
    private PdfFormField(final PdfWriter writer) {
        super(writer, null);
        this.form = true;
        this.annotation = false;
    }

    void setWidget(final Rectangle rect, final PdfName highlight) {
        put(PdfName.TYPE, PdfName.ANNOT);
        put(PdfName.SUBTYPE, PdfName.WIDGET);
        put(PdfName.RECT, new PdfRectangle(rect));
        this.annotation = true;
        if (highlight != null && !highlight.equals(HIGHLIGHT_INVERT)) {
			put(PdfName.H, highlight);
		}
    }

    static PdfFormField createEmpty(final PdfWriter writer) {
        final PdfFormField field = new PdfFormField(writer);
        return field;
    }

    public void setButton(final int flags) {
        put(PdfName.FT, PdfName.BTN);
        if (flags != 0) {
			put(PdfName.FF, new PdfNumber(flags));
		}
    }

    private static PdfFormField createButton(final PdfWriter writer, final int flags) {
        final PdfFormField field = new PdfFormField(writer);
        field.setButton(flags);
        return field;
    }

    static PdfFormField createPushButton(final PdfWriter writer) {
        return createButton(writer, FF_PUSHBUTTON);
    }

    static PdfFormField createCheckBox(final PdfWriter writer) {
        return createButton(writer, 0);
    }

    static PdfFormField createRadioButton(final PdfWriter writer, final boolean noToggleToOff) {
        return createButton(writer, FF_RADIO + (noToggleToOff ? FF_NO_TOGGLE_TO_OFF : 0));
    }

    static PdfFormField createTextField(final PdfWriter writer, final boolean multiline, final boolean password, final int maxLen) {
        final PdfFormField field = new PdfFormField(writer);
        field.put(PdfName.FT, PdfName.TX);
        int flags = multiline ? FF_MULTILINE : 0;
        flags += password ? FF_PASSWORD : 0;
        field.put(PdfName.FF, new PdfNumber(flags));
        if (maxLen > 0) {
			field.put(PdfName.MAXLEN, new PdfNumber(maxLen));
		}
        return field;
    }

    private static PdfFormField createChoice(final PdfWriter writer, final int flags, final PdfArray options, final int topIndex) {
        final PdfFormField field = new PdfFormField(writer);
        field.put(PdfName.FT, PdfName.CH);
        field.put(PdfName.FF, new PdfNumber(flags));
        field.put(PdfName.OPT, options);
        if (topIndex > 0) {
			field.put(PdfName.TI, new PdfNumber(topIndex));
		}
        return field;
    }

    static PdfFormField createList(final PdfWriter writer, final String options[], final int topIndex) {
        return createChoice(writer, 0, processOptions(options), topIndex);
    }

    static PdfFormField createList(final PdfWriter writer, final String options[][], final int topIndex) {
        return createChoice(writer, 0, processOptions(options), topIndex);
    }

    static PdfFormField createCombo(final PdfWriter writer, final boolean edit, final String options[], final int topIndex) {
        return createChoice(writer, FF_COMBO + (edit ? FF_EDIT : 0), processOptions(options), topIndex);
    }

    static PdfFormField createCombo(final PdfWriter writer, final boolean edit, final String options[][], final int topIndex) {
        return createChoice(writer, FF_COMBO + (edit ? FF_EDIT : 0), processOptions(options), topIndex);
    }

    private static PdfArray processOptions(final String options[]) {
        final PdfArray array = new PdfArray();
        for (final String option : options) {
            array.add(new PdfString(option, PdfObject.TEXT_UNICODE));
        }
        return array;
    }

    private static PdfArray processOptions(final String options[][]) {
        final PdfArray array = new PdfArray();
        for (final String[] subOption : options) {
            final PdfArray ar2 = new PdfArray(new PdfString(subOption[0], PdfObject.TEXT_UNICODE));
            ar2.add(new PdfString(subOption[1], PdfObject.TEXT_UNICODE));
            array.add(ar2);
        }
        return array;
    }

    static PdfFormField createSignature(final PdfWriter writer) {
        final PdfFormField field = new PdfFormField(writer);
        field.put(PdfName.FT, PdfName.SIG);
        return field;
    }

    /** Getter for property parent.
     * @return Value of property parent.
     */
    public PdfFormField getParent() {
        return this.parent;
    }

    public void addKid(final PdfFormField field) {
        field.parent = this;
        if (this.kids == null) {
			this.kids = new ArrayList();
		}
        this.kids.add(field);
    }

    public ArrayList getKids() {
        return this.kids;
    }

    int setFieldFlags(final int flags) {
        final PdfNumber obj = (PdfNumber)get(PdfName.FF);
        int old;
        if (obj == null) {
			old = 0;
		} else {
			old = obj.intValue();
		}
        final int v = old | flags;
        put(PdfName.FF, new PdfNumber(v));
        return old;
    }

    public void setValueAsString(final String s) {
        put(PdfName.V, new PdfString(s, PdfObject.TEXT_UNICODE));
    }

    public void setValueAsName(final String s) {
        put(PdfName.V, new PdfName(s));
    }

    public void setValue(final PdfSignature sig) {
        put(PdfName.V, sig);
    }

    public void setDefaultValueAsString(final String s) {
        put(PdfName.DV, new PdfString(s, PdfObject.TEXT_UNICODE));
    }

    public void setDefaultValueAsName(final String s) {
        put(PdfName.DV, new PdfName(s));
    }

    public void setFieldName(final String s) {
        if (s != null) {
			put(PdfName.T, new PdfString(s, PdfObject.TEXT_UNICODE));
		}
    }

    public void setUserName(final String s) {
        put(PdfName.TU, new PdfString(s, PdfObject.TEXT_UNICODE));
    }

    public void setMappingName(final String s) {
        put(PdfName.TM, new PdfString(s, PdfObject.TEXT_UNICODE));
    }

    public void setQuadding(final int v) {
        put(PdfName.Q, new PdfNumber(v));
    }

    static void mergeResources(final PdfDictionary result, final PdfDictionary source, final PdfStamperImp writer) {
        PdfDictionary dic = null;
        PdfDictionary res = null;
        PdfName target = null;
        for (final PdfName element : mergeTarget) {
            target = element;
            final PdfDictionary pdfDict = source.getAsDict(target);
            if ((dic = pdfDict) != null) {
                if ((res = (PdfDictionary)PdfReader.getPdfObject(result.get(target), result)) == null) {
                    res = new PdfDictionary();
                }
                res.mergeDifferent(dic);
                result.put(target, res);
                if (writer != null) {
					writer.markUsed(res);
				}
            }
        }
    }

    static void mergeResources(final PdfDictionary result, final PdfDictionary source) {
        mergeResources(result, source, null);
    }

    @Override
	public void setUsed() {
        this.used = true;
        if (this.parent != null) {
			put(PdfName.PARENT, this.parent.getIndirectReference());
		}
        if (this.kids != null) {
            final PdfArray array = new PdfArray();
            for (int k = 0; k < this.kids.size(); ++k) {
				array.add(((PdfFormField)this.kids.get(k)).getIndirectReference());
			}
            put(PdfName.KIDS, array);
        }
        if (this.templates == null) {
			return;
		}
        final PdfDictionary dic = new PdfDictionary();
        for (final Iterator it = this.templates.keySet().iterator(); it.hasNext();) {
            final PdfTemplate template = (PdfTemplate)it.next();
            mergeResources(dic, (PdfDictionary)template.getResources());
        }
        put(PdfName.DR, dic);
    }

    static PdfAnnotation shallowDuplicate(final PdfAnnotation annot) {
        PdfAnnotation dup;
        if (annot.isForm()) {
            dup = new PdfFormField(annot.writer);
            final PdfFormField dupField = (PdfFormField)dup;
            final PdfFormField srcField = (PdfFormField)annot;
            dupField.parent = srcField.parent;
            dupField.kids = srcField.kids;
        } else {
			dup = new PdfAnnotation(annot.writer, null);
		}
        dup.merge(annot);
        dup.form = annot.form;
        dup.annotation = annot.annotation;
        dup.templates = annot.templates;
        return dup;
    }
}
