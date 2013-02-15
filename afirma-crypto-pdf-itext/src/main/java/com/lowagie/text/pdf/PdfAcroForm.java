/*
 * $Id: PdfAcroForm.java 3912 2009-04-26 08:38:15Z blowagie $
 *
 * Copyright 2002 Bruno Lowagie
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

import java.util.HashMap;
import java.util.Iterator;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Rectangle;

/**
 * Each PDF document can contain maximum 1 AcroForm.
 */

public class PdfAcroForm extends PdfDictionary {

    private final PdfWriter writer;


    /** This is a map containing FieldTemplates. */
    private final HashMap fieldTemplates = new HashMap();

    /** This is an array containing DocumentFields. */
    private final PdfArray documentFields = new PdfArray();

    /** This is an array containing the calculationorder of the fields. */
    private final PdfArray calculationOrder = new PdfArray();

    /** Contains the signature flags. */
    private int sigFlags = 0;

    /** Creates new PdfAcroForm
     * @param writer
     */
    public PdfAcroForm(final PdfWriter writer) {
        super();
        this.writer = writer;
    }

    public void setNeedAppearances(final boolean value) {
    	put(PdfName.NEEDAPPEARANCES, new PdfBoolean(value));
    }

    /**
     * Adds fieldTemplates.
     * @param ft
     */

    public void addFieldTemplates(final HashMap ft) {
        this.fieldTemplates.putAll(ft);
    }

    /**
     * Adds documentFields.
     * @param ref
     */

    public void addDocumentField(final PdfIndirectReference ref) {
        this.documentFields.add(ref);
    }

    /**
     * Checks if the Acroform is valid
     * @return true if the Acroform is valid
     */

    public boolean isValid() {
        if (this.documentFields.size() == 0) {
			return false;
		}
        put(PdfName.FIELDS, this.documentFields);
        if (this.sigFlags != 0) {
			put(PdfName.SIGFLAGS, new PdfNumber(this.sigFlags));
		}
        if (this.calculationOrder.size() > 0) {
			put(PdfName.CO, this.calculationOrder);
		}
        if (this.fieldTemplates.isEmpty()) {
			return true;
		}
        final PdfDictionary dic = new PdfDictionary();
        for (final Iterator it = this.fieldTemplates.keySet().iterator(); it.hasNext();) {
            final PdfTemplate template = (PdfTemplate)it.next();
            PdfFormField.mergeResources(dic, (PdfDictionary)template.getResources());
        }
        put(PdfName.DR, dic);
        put(PdfName.DA, new PdfString("/Helv 0 Tf 0 g "));
        final PdfDictionary fonts = (PdfDictionary)dic.get(PdfName.FONT);
        if (fonts != null) {
            this.writer.eliminateFontSubset(fonts);
        }
        return true;
    }

    /**
     * Adds an object to the calculationOrder.
     * @param formField
     */

    public void addCalculationOrder(final PdfFormField formField) {
        this.calculationOrder.add(formField.getIndirectReference());
    }

    /**
     * Sets the signature flags.
     * @param f
     */

    public void setSigFlags(final int f) {
        this.sigFlags |= f;
    }

    /**
     * Adds a formfield to the AcroForm.
     * @param formField
     */

    private void addFormField(final PdfFormField formField) {
        this.writer.addAnnotation(formField);
    }







    /**
     * @param button
     * @param characteristics
     * @param name
     * @param value
     */
    private void setButtonParams(final PdfFormField button, final int characteristics, final String name, final String value) {
        button.setButton(characteristics);
        button.setFlags(PdfAnnotation.FLAGS_PRINT);
        button.setPage();
        button.setFieldName(name);
        if (value != null) {
			button.setValueAsString(value);
		}
    }

    /**
     * @param button
     * @param caption
     * @param font
     * @param fontSize
     * @param llx
     * @param lly
     * @param urx
     * @param ury
     */
    private void drawButton(final PdfFormField button, final String caption, final BaseFont font, final float fontSize, final float llx, final float lly, final float urx, final float ury) {
        final PdfAppearance pa = PdfAppearance.createAppearance(this.writer, urx - llx, ury - lly);
        pa.drawButton(0f, 0f, urx - llx, ury - lly, caption, font, fontSize);
        button.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, pa);
    }









    /**
     * @param field
     * @param text
     * @param name
     * @param llx
     * @param lly
     * @param urx
     * @param ury
     */
    private void setTextFieldParams(final PdfFormField field, final String text, final String name, final float llx, final float lly, final float urx, final float ury) {
        field.setWidget(new Rectangle(llx, lly, urx, ury), PdfAnnotation.HIGHLIGHT_INVERT);
        field.setValueAsString(text);
        field.setDefaultValueAsString(text);
        field.setFieldName(name);
        field.setFlags(PdfAnnotation.FLAGS_PRINT);
        field.setPage();
    }

    /**
     * @param field
     * @param text
     * @param font
     * @param fontSize
     * @param llx
     * @param lly
     * @param urx
     * @param ury
     */
    private void drawSingleLineOfText(final PdfFormField field, final String text, final BaseFont font, final float fontSize, final float llx, final float lly, final float urx, final float ury) {
        final PdfAppearance tp = PdfAppearance.createAppearance(this.writer, urx - llx, ury - lly);
        final PdfAppearance tp2 = (PdfAppearance)tp.getDuplicate();
        tp2.setFontAndSize(font, fontSize);
        tp2.resetRGBColorFill();
        field.setDefaultAppearanceString(tp2);
        tp.drawTextField(0f, 0f, urx - llx, ury - lly);
        tp.beginVariableText();
        tp.saveState();
        tp.rectangle(3f, 3f, urx - llx - 6f, ury - lly - 6f);
        tp.clip();
        tp.newPath();
        tp.beginText();
        tp.setFontAndSize(font, fontSize);
        tp.resetRGBColorFill();
        tp.setTextMatrix(4, (ury - lly) / 2 - fontSize * 0.3f);
        tp.showText(text);
        tp.endText();
        tp.restoreState();
        tp.endVariableText();
        field.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, tp);
    }

    /**
     * @param field
     * @param text
     * @param font
     * @param fontSize
     * @param llx
     * @param lly
     * @param urx
     * @param ury
     */
    private void drawMultiLineOfText(final PdfFormField field, final String text, final BaseFont font, final float fontSize, final float llx, final float lly, final float urx, final float ury) {
        final PdfAppearance tp = PdfAppearance.createAppearance(this.writer, urx - llx, ury - lly);
        final PdfAppearance tp2 = (PdfAppearance)tp.getDuplicate();
        tp2.setFontAndSize(font, fontSize);
        tp2.resetRGBColorFill();
        field.setDefaultAppearanceString(tp2);
        tp.drawTextField(0f, 0f, urx - llx, ury - lly);
        tp.beginVariableText();
        tp.saveState();
        tp.rectangle(3f, 3f, urx - llx - 6f, ury - lly - 6f);
        tp.clip();
        tp.newPath();
        tp.beginText();
        tp.setFontAndSize(font, fontSize);
        tp.resetRGBColorFill();
        tp.setTextMatrix(4, 5);
        final java.util.StringTokenizer tokenizer = new java.util.StringTokenizer(text, "\n");
        float yPos = ury - lly;
        while (tokenizer.hasMoreTokens()) {
            yPos -= fontSize * 1.2f;
            tp.showTextAligned(PdfContentByte.ALIGN_LEFT, tokenizer.nextToken(), 3, yPos, 0);
        }
        tp.endText();
        tp.restoreState();
        tp.endVariableText();
        field.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, tp);
    }



    /**
     * @param field
     * @param name
     * @param value
     * @param status
     * @param llx
     * @param lly
     * @param urx
     * @param ury
     */
    private void setCheckBoxParams(final PdfFormField field, final String name, final String value, final boolean status, final float llx, final float lly, final float urx, final float ury) {
        field.setWidget(new Rectangle(llx, lly, urx, ury), PdfAnnotation.HIGHLIGHT_TOGGLE);
        field.setFieldName(name);
        if (status) {
            field.setValueAsName(value);
            field.setAppearanceState(value);
        }
        else {
            field.setValueAsName("Off");
            field.setAppearanceState("Off");
        }
        field.setFlags(PdfAnnotation.FLAGS_PRINT);
        field.setPage();
        field.setBorderStyle(new PdfBorderDictionary(1, PdfBorderDictionary.STYLE_SOLID));
    }

    /**
     * @param field
     * @param value
     * @param llx
     * @param lly
     * @param urx
     * @param ury
     */
    private void drawCheckBoxAppearences(final PdfFormField field, final String value, final float llx, final float lly, final float urx, final float ury) {
        BaseFont font = null;
        try {
            font = BaseFont.createFont(BaseFont.ZAPFDINGBATS, BaseFont.WINANSI, BaseFont.NOT_EMBEDDED);
        }
        catch(final Exception e) {
            throw new ExceptionConverter(e);
        }
        final float size = ury - lly;
        final PdfAppearance tpOn = PdfAppearance.createAppearance(this.writer, urx - llx, ury - lly);
        final PdfAppearance tp2 = (PdfAppearance)tpOn.getDuplicate();
        tp2.setFontAndSize(font, size);
        tp2.resetRGBColorFill();
        field.setDefaultAppearanceString(tp2);
        tpOn.drawTextField(0f, 0f, urx - llx, ury - lly);
        tpOn.saveState();
        tpOn.resetRGBColorFill();
        tpOn.beginText();
        tpOn.setFontAndSize(font, size);
        tpOn.showTextAligned(PdfContentByte.ALIGN_CENTER, "4", (urx - llx) / 2, (ury - lly) / 2 - size * 0.3f, 0);
        tpOn.endText();
        tpOn.restoreState();
        field.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, value, tpOn);
        final PdfAppearance tpOff = PdfAppearance.createAppearance(this.writer, urx - llx, ury - lly);
        tpOff.drawTextField(0f, 0f, urx - llx, ury - lly);
        field.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, "Off", tpOff);
    }







    /**
     * @param field
     * @param value
     * @param llx
     * @param lly
     * @param urx
     * @param ury
     */
    private void drawRadioAppearences(final PdfFormField field, final String value, final float llx, final float lly, final float urx, final float ury) {
        final PdfAppearance tpOn = PdfAppearance.createAppearance(this.writer, urx - llx, ury - lly);
        tpOn.drawRadioField(0f, 0f, urx - llx, ury - lly, true);
        field.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, value, tpOn);
        final PdfAppearance tpOff = PdfAppearance.createAppearance(this.writer, urx - llx, ury - lly);
        tpOff.drawRadioField(0f, 0f, urx - llx, ury - lly, false);
        field.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, "Off", tpOff);
    }









    /**
     * @param field
     * @param name
     * @param defaultValue
     * @param llx
     * @param lly
     * @param urx
     * @param ury
     */
    private void setChoiceParams(final PdfFormField field, final String name, final String defaultValue, final float llx, final float lly, final float urx, final float ury) {
        field.setWidget(new Rectangle(llx, lly, urx, ury), PdfAnnotation.HIGHLIGHT_INVERT);
        if (defaultValue != null) {
            field.setValueAsString(defaultValue);
            field.setDefaultValueAsString(defaultValue);
        }
        field.setFieldName(name);
        field.setFlags(PdfAnnotation.FLAGS_PRINT);
        field.setPage();
        field.setBorderStyle(new PdfBorderDictionary(2, PdfBorderDictionary.STYLE_SOLID));
    }






}