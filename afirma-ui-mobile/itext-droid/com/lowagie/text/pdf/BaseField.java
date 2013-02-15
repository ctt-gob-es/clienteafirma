/*
 * Copyright 2005 by Paulo Soares.
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

import harmony.java.awt.Color;

import java.io.IOException;
import java.util.HashMap;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Rectangle;

/** Common field variables.
 * @author Paulo Soares (psoares@consiste.pt)
 */
abstract class BaseField {

    /** A thin border with 1 point width. */
    private static final float BORDER_WIDTH_THIN = 1;


    /** The field is visible. */
    static final int VISIBLE = 0;
    /** The field is hidden. */
    public static final int HIDDEN = 1;
    /** The field is visible but does not print. */
    static final int VISIBLE_BUT_DOES_NOT_PRINT = 2;
    /** The field is hidden but is printable. */
    static final int HIDDEN_BUT_PRINTABLE = 3;
    /** The user may not change the value of the field. */
    static final int READ_ONLY = PdfFormField.FF_READ_ONLY;
    /** The field must have a value at the time it is exported by a submit-form
     * action.
     */
    static final int REQUIRED = PdfFormField.FF_REQUIRED;
    /** The field may contain multiple lines of text.
     * This flag is only meaningful with text fields.
     */
    static final int MULTILINE = PdfFormField.FF_MULTILINE;
    /** The field will not scroll (horizontally for single-line
     * fields, vertically for multiple-line fields) to accommodate more text
     * than will fit within its annotation rectangle. Once the field is full, no
     * further text will be accepted.
     */
    static final int DO_NOT_SCROLL = PdfFormField.FF_DONOTSCROLL;
    /** The field is intended for entering a secure password that should
     * not be echoed visibly to the screen.
     */
    static final int PASSWORD = PdfFormField.FF_PASSWORD;
    /** The text entered in the field represents the pathname of
     * a file whose contents are to be submitted as the value of the field.
     */
    static final int FILE_SELECTION = PdfFormField.FF_FILESELECT;
    /** The text entered in the field will not be spell-checked.
     * This flag is meaningful only in text fields and in combo
     * fields with the <CODE>EDIT</CODE> flag set.
     */
    static final int DO_NOT_SPELL_CHECK = PdfFormField.FF_DONOTSPELLCHECK;
    /** If set the combo box includes an editable text box as well as a drop list; if
     * clear, it includes only a drop list.
     * This flag is only meaningful with combo fields.
     */
    public static final int EDIT = PdfFormField.FF_EDIT;

    /**
     * combo box flag.
     */
    public static final int COMB = PdfFormField.FF_COMB;

    protected float borderWidth = BORDER_WIDTH_THIN;
    protected int borderStyle = PdfBorderDictionary.STYLE_SOLID;
    protected Color borderColor;
    protected Color backgroundColor;
    protected Color textColor;
    private BaseFont font;
    protected float fontSize = 0;
    protected int alignment = Element.ALIGN_LEFT;
    protected PdfWriter writer;
    protected String text;
    protected Rectangle box;

    /** Holds value of property rotation. */
    protected int rotation = 0;

    /** Holds value of property visibility. */
    protected int visibility;

    /** Holds value of property fieldName. */
    protected String fieldName;

    /** Holds value of property options. */
    protected int options;

    /** Holds value of property maxCharacterLength. */
    protected int maxCharacterLength;

    private final static HashMap fieldKeys = new HashMap();

    static {
        fieldKeys.putAll(PdfCopyFieldsImp.fieldKeys);
        fieldKeys.put(PdfName.T, new Integer(1));
    }
    /** Creates a new <CODE>TextField</CODE>.
     * @param writer the document <CODE>PdfWriter</CODE>
     * @param box the field location and dimensions
     * @param fieldName the field name. If <CODE>null</CODE> only the widget keys
     * will be included in the field allowing it to be used as a kid field.
     */
    public BaseField(final PdfWriter writer, final Rectangle box, final String fieldName) {
        this.writer = writer;
        setBox(box);
        this.fieldName = fieldName;
    }

    protected BaseFont getRealFont() throws IOException, DocumentException {
        if (this.font == null) {
			return BaseFont.createFont(BaseFont.HELVETICA, BaseFont.WINANSI, false);
		} else {
			return this.font;
		}
    }

    protected PdfAppearance getBorderAppearance() {
        final PdfAppearance app = PdfAppearance.createAppearance(this.writer, this.box.getWidth(), this.box.getHeight());
        switch (this.rotation) {
            case 90:
                app.setMatrix(0, 1, -1, 0, this.box.getHeight(), 0);
                break;
            case 180:
                app.setMatrix(-1, 0, 0, -1, this.box.getWidth(), this.box.getHeight());
                break;
            case 270:
                app.setMatrix(0, -1, 1, 0, 0, this.box.getWidth());
                break;
        }
        app.saveState();
        // background
        if (this.backgroundColor != null) {
            app.setColorFill(this.backgroundColor);
            app.rectangle(0, 0, this.box.getWidth(), this.box.getHeight());
            app.fill();
        }
        // border
        if (this.borderStyle == PdfBorderDictionary.STYLE_UNDERLINE) {
            if (this.borderWidth != 0 && this.borderColor != null) {
                app.setColorStroke(this.borderColor);
                app.setLineWidth(this.borderWidth);
                app.moveTo(0, this.borderWidth / 2);
                app.lineTo(this.box.getWidth(), this.borderWidth / 2);
                app.stroke();
            }
        }
        else if (this.borderStyle == PdfBorderDictionary.STYLE_BEVELED) {
            if (this.borderWidth != 0 && this.borderColor != null) {
                app.setColorStroke(this.borderColor);
                app.setLineWidth(this.borderWidth);
                app.rectangle(this.borderWidth / 2, this.borderWidth / 2, this.box.getWidth() - this.borderWidth, this.box.getHeight() - this.borderWidth);
                app.stroke();
            }
            // beveled
            Color actual = this.backgroundColor;
            if (actual == null) {
				actual = Color.white;
			}
            app.setGrayFill(1);
            drawTopFrame(app);
            app.setColorFill(actual.darker());
            drawBottomFrame(app);
        }
        else if (this.borderStyle == PdfBorderDictionary.STYLE_INSET) {
            if (this.borderWidth != 0 && this.borderColor != null) {
                app.setColorStroke(this.borderColor);
                app.setLineWidth(this.borderWidth);
                app.rectangle(this.borderWidth / 2, this.borderWidth / 2, this.box.getWidth() - this.borderWidth, this.box.getHeight() - this.borderWidth);
                app.stroke();
            }
            // inset
            app.setGrayFill(0.5f);
            drawTopFrame(app);
            app.setGrayFill(0.75f);
            drawBottomFrame(app);
        }
        else {
            if (this.borderWidth != 0 && this.borderColor != null) {
                if (this.borderStyle == PdfBorderDictionary.STYLE_DASHED) {
					app.setLineDash(3, 0);
				}
                app.setColorStroke(this.borderColor);
                app.setLineWidth(this.borderWidth);
                app.rectangle(this.borderWidth / 2, this.borderWidth / 2, this.box.getWidth() - this.borderWidth, this.box.getHeight() - this.borderWidth);
                app.stroke();
                if ((this.options & COMB) != 0 && this.maxCharacterLength > 1) {
                    final float step = this.box.getWidth() / this.maxCharacterLength;
                    final float yb = this.borderWidth / 2;
                    final float yt = this.box.getHeight() - this.borderWidth / 2;
                    for (int k = 1; k < this.maxCharacterLength; ++k) {
                        final float x = step * k;
                        app.moveTo(x, yb);
                        app.lineTo(x, yt);
                    }
                    app.stroke();
                }
            }
        }
        app.restoreState();
        return app;
    }



    private static void trimRight(final StringBuffer buf) {
        int len = buf.length();
        while (true) {
            if (len == 0) {
				return;
			}
            if (buf.charAt(--len) != ' ') {
				return;
			}
            buf.setLength(len);
        }
    }



    private void drawTopFrame(final PdfAppearance app) {
        app.moveTo(this.borderWidth, this.borderWidth);
        app.lineTo(this.borderWidth, this.box.getHeight() - this.borderWidth);
        app.lineTo(this.box.getWidth() - this.borderWidth, this.box.getHeight() - this.borderWidth);
        app.lineTo(this.box.getWidth() - 2 * this.borderWidth, this.box.getHeight() - 2 * this.borderWidth);
        app.lineTo(2 * this.borderWidth, this.box.getHeight() - 2 * this.borderWidth);
        app.lineTo(2 * this.borderWidth, 2 * this.borderWidth);
        app.lineTo(this.borderWidth, this.borderWidth);
        app.fill();
    }

    private void drawBottomFrame(final PdfAppearance app) {
        app.moveTo(this.borderWidth, this.borderWidth);
        app.lineTo(this.box.getWidth() - this.borderWidth, this.borderWidth);
        app.lineTo(this.box.getWidth() - this.borderWidth, this.box.getHeight() - this.borderWidth);
        app.lineTo(this.box.getWidth() - 2 * this.borderWidth, this.box.getHeight() - 2 * this.borderWidth);
        app.lineTo(this.box.getWidth() - 2 * this.borderWidth, 2 * this.borderWidth);
        app.lineTo(2 * this.borderWidth, 2 * this.borderWidth);
        app.lineTo(this.borderWidth, this.borderWidth);
        app.fill();
    }
    /** Gets the border width in points.
     * @return the border width in points
     */
    public float getBorderWidth() {
        return this.borderWidth;
    }

    /** Sets the border width in points. To eliminate the border
     * set the border color to <CODE>null</CODE>.
     * @param borderWidth the border width in points
     */
    public void setBorderWidth(final float borderWidth) {
        this.borderWidth = borderWidth;
    }

    /** Gets the border style.
     * @return the border style
     */
    public int getBorderStyle() {
        return this.borderStyle;
    }

    /** Sets the border style. The styles are found in <CODE>PdfBorderDictionary</CODE>
     * and can be <CODE>STYLE_SOLID</CODE>, <CODE>STYLE_DASHED</CODE>,
     * <CODE>STYLE_BEVELED</CODE>, <CODE>STYLE_INSET</CODE> and
     * <CODE>STYLE_UNDERLINE</CODE>.
     * @param borderStyle the border style
     */
    public void setBorderStyle(final int borderStyle) {
        this.borderStyle = borderStyle;
    }

    /** Gets the border color.
     * @return the border color
     */
    public Color getBorderColor() {
        return this.borderColor;
    }

    /** Sets the border color. Set to <CODE>null</CODE> to remove
     * the border.
     * @param borderColor the border color
     */
    public void setBorderColor(final Color borderColor) {
        this.borderColor = borderColor;
    }

    /** Gets the background color.
     * @return the background color
     */
    public Color getBackgroundColor() {
        return this.backgroundColor;
    }

    /** Sets the background color. Set to <CODE>null</CODE> for
     * transparent background.
     * @param backgroundColor the background color
     */
    public void setBackgroundColor(final Color backgroundColor) {
        this.backgroundColor = backgroundColor;
    }

    /** Gets the text color.
     * @return the text color
     */
    public Color getTextColor() {
        return this.textColor;
    }

    /** Sets the text color. If <CODE>null</CODE> the color used
     * will be black.
     * @param textColor the text color
     */
    public void setTextColor(final Color textColor) {
        this.textColor = textColor;
    }

    /** Gets the text font.
     * @return the text font
     */
    public BaseFont getFont() {
        return this.font;
    }

    /** Sets the text font. If <CODE>null</CODE> then Helvetica
     * will be used.
     * @param font the text font
     */
    public void setFont(final BaseFont font) {
        this.font = font;
    }

    /** Gets the font size.
     * @return the font size
     */
    public float getFontSize() {
        return this.fontSize;
    }

    /** Sets the font size. If 0 then auto-sizing will be used but
     * only for text fields.
     * @param fontSize the font size
     */
    public void setFontSize(final float fontSize) {
        this.fontSize = fontSize;
    }

    /** Gets the text horizontal alignment.
     * @return the text horizontal alignment
     */
    public int getAlignment() {
        return this.alignment;
    }

    /** Sets the text horizontal alignment. It can be <CODE>Element.ALIGN_LEFT</CODE>,
     * <CODE>Element.ALIGN_CENTER</CODE> and <CODE>Element.ALIGN_RIGHT</CODE>.
     * @param alignment the text horizontal alignment
     */
    public void setAlignment(final int alignment) {
        this.alignment = alignment;
    }

    /** Gets the text.
     * @return the text
     */
    public String getText() {
        return this.text;
    }

    /** Sets the text for text fields.
     * @param text the text
     */
    public void setText(final String text) {
        this.text = text;
    }

    /** Gets the field dimension and position.
     * @return the field dimension and position
     */
    public Rectangle getBox() {
        return this.box;
    }

    /** Sets the field dimension and position.
     * @param box the field dimension and position
     */
    public void setBox(final Rectangle box) {
    	if (box == null) {
    		this.box = null;
    	}
    	else {
    		this.box = new Rectangle(box);
    		this.box.normalize();
    	}
    }

    /** Gets the field rotation.
     * @return the field rotation
     */
    public int getRotation() {
        return this.rotation;
    }

    /** Sets the field rotation. This value should be the same as
     * the page rotation where the field will be shown.
     * @param rotation the field rotation
     */
    public void setRotation(int rotation) {
        if (rotation % 90 != 0) {
			throw new IllegalArgumentException("Rotation must be a multiple of 90.");
		}
        rotation %= 360;
        if (rotation < 0) {
			rotation += 360;
		}
        this.rotation = rotation;
    }

    /** Convenience method to set the field rotation the same as the
     * page rotation.
     * @param page the page
     */
    public void setRotationFromPage(final Rectangle page) {
        setRotation(page.getRotation());
    }

    /** Gets the field visibility flag.
     * @return the field visibility flag
     */
    public int getVisibility() {
        return this.visibility;
    }

    /** Sets the field visibility flag. This flags can be one of
     * <CODE>VISIBLE</CODE>, <CODE>HIDDEN</CODE>, <CODE>VISIBLE_BUT_DOES_NOT_PRINT</CODE>
     * and <CODE>HIDDEN_BUT_PRINTABLE</CODE>.
     * @param visibility field visibility flag
     */
    public void setVisibility(final int visibility) {
        this.visibility = visibility;
    }

    /** Gets the field name.
     * @return the field name
     */
    public String getFieldName() {
        return this.fieldName;
    }

    /** Sets the field name.
     * @param fieldName the field name. If <CODE>null</CODE> only the widget keys
     * will be included in the field allowing it to be used as a kid field.
     */
    public void setFieldName(final String fieldName) {
        this.fieldName = fieldName;
    }

    /** Gets the option flags.
     * @return the option flags
     */
    public int getOptions() {
        return this.options;
    }

    /** Sets the option flags. The option flags can be a combination by oring of
     * <CODE>READ_ONLY</CODE>, <CODE>REQUIRED</CODE>,
     * <CODE>MULTILINE</CODE>, <CODE>DO_NOT_SCROLL</CODE>,
     * <CODE>PASSWORD</CODE>, <CODE>FILE_SELECTION</CODE>,
     * <CODE>DO_NOT_SPELL_CHECK</CODE> and <CODE>EDIT</CODE>.
     * @param options the option flags
     */
    public void setOptions(final int options) {
        this.options = options;
    }

    /** Gets the maximum length of the field's text, in characters.
     * @return the maximum length of the field's text, in characters.
     */
    public int getMaxCharacterLength() {
        return this.maxCharacterLength;
    }

    /** Sets the maximum length of the field's text, in characters.
     * It is only meaningful for text fields.
     * @param maxCharacterLength the maximum length of the field's text, in characters
     */
    public void setMaxCharacterLength(final int maxCharacterLength) {
        this.maxCharacterLength = maxCharacterLength;
    }

    /**
     * Getter for property writer.
     * @return Value of property writer.
     */
    public PdfWriter getWriter() {
        return this.writer;
    }

    /**
     * Setter for property writer.
     * @param writer New value of property writer.
     */
    public void setWriter(final PdfWriter writer) {
        this.writer = writer;
    }


}
