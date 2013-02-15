/*
 * $Id: PdfAnnotation.java 3917 2009-04-27 12:52:16Z blowagie $
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

import java.awt.Color;
import java.io.IOException;
import java.util.HashMap;

import com.lowagie.text.Rectangle;
/**
 * A <CODE>PdfAnnotation</CODE> is a note that is associated with a page.
 *
 * @see		PdfDictionary
 */

public class PdfAnnotation extends PdfDictionary {

    /** highlight attributename */
    static final PdfName HIGHLIGHT_INVERT = PdfName.I;


    /** highlight attributename */
    static final PdfName HIGHLIGHT_TOGGLE = PdfName.T;

    /** flagvalue */
    static final int FLAGS_HIDDEN = 2;
    /** flagvalue */
    static final int FLAGS_PRINT = 4;


    /** flagvalue */
    static final int FLAGS_NOVIEW = 32;

    /** flagvalue */
    static final int FLAGS_LOCKED = 128;

    /** appearance attributename */
    static final PdfName APPEARANCE_NORMAL = PdfName.N;

    /** attributevalue */
    private static final int MARKUP_UNDERLINE = 1;
    /** attributevalue */
    private static final int MARKUP_STRIKEOUT = 2;
    /**
     * attributevalue
     * @since 2.1.3
     */
    private static final int MARKUP_SQUIGGLY = 3;

    protected PdfWriter writer;
    /**
     * Reference to this annotation.
     * @since	2.1.6; was removed in 2.1.5, but restored in 2.1.6
     */
    private PdfIndirectReference reference;
    protected HashMap templates;
    protected boolean form = false;
    protected boolean annotation = true;

    /** Holds value of property used. */
    protected boolean used = false;

    /** Holds value of property placeInPage. */
    private int placeInPage = -1;

    // constructors
    PdfAnnotation(final PdfWriter writer, final Rectangle rect) {
        this.writer = writer;
        if (rect != null) {
			put(PdfName.RECT, new PdfRectangle(rect));
		}
    }

/**
 * Constructs a new <CODE>PdfAnnotation</CODE> of subtype text.
 * @param writer
 * @param llx
 * @param lly
 * @param urx
 * @param ury
 * @param title
 * @param content
 */

    public PdfAnnotation(final PdfWriter writer, final float llx, final float lly, final float urx, final float ury, final PdfString title, final PdfString content) {
        this.writer = writer;
        put(PdfName.SUBTYPE, PdfName.TEXT);
        put(PdfName.T, title);
        put(PdfName.RECT, new PdfRectangle(llx, lly, urx, ury));
        put(PdfName.CONTENTS, content);
    }

/**
 * Constructs a new <CODE>PdfAnnotation</CODE> of subtype link (Action).
 * @param writer
 * @param llx
 * @param lly
 * @param urx
 * @param ury
 * @param action
 */

    public PdfAnnotation(final PdfWriter writer, final float llx, final float lly, final float urx, final float ury, final PdfAction action) {
        this.writer = writer;
        put(PdfName.SUBTYPE, PdfName.LINK);
        put(PdfName.RECT, new PdfRectangle(llx, lly, urx, ury));
        put(PdfName.A, action);
        put(PdfName.BORDER, new PdfBorderArray(0, 0, 0));
        put(PdfName.C, new PdfColor(0x00, 0x00, 0xFF));
    }

    /**
     * Creates a screen PdfAnnotation
     * @param writer
     * @param rect
     * @param clipTitle
     * @param fs
     * @param mimeType
     * @param playOnDisplay
     * @return a screen PdfAnnotation
     * @throws IOException
     */
    public static PdfAnnotation createScreen(final PdfWriter writer, final Rectangle rect, final String clipTitle, final PdfFileSpecification fs,
                                             final String mimeType, final boolean playOnDisplay) throws IOException {
        final PdfAnnotation ann = new PdfAnnotation(writer, rect);
        ann.put(PdfName.SUBTYPE, PdfName.SCREEN);
        ann.put (PdfName.F, new PdfNumber(FLAGS_PRINT));
        ann.put(PdfName.TYPE, PdfName.ANNOT);
        ann.setPage();
        final PdfIndirectReference ref = ann.getIndirectReference();
        final PdfAction action = PdfAction.rendition(clipTitle,fs,mimeType, ref);
        final PdfIndirectReference actionRef = writer.addToBody(action).getIndirectReference();
        // for play on display add trigger event
        if (playOnDisplay)
        {
            final PdfDictionary aa = new PdfDictionary();
            aa.put(new PdfName("PV"), actionRef);
            ann.put(PdfName.AA, aa);
        }
        ann.put(PdfName.A, actionRef);
        return ann;
    }

    /**
     * Returns an indirect reference to the annotation
     * @return the indirect reference
     */
    public PdfIndirectReference getIndirectReference() {
        if (this.reference == null) {
        	this.reference = this.writer.getPdfIndirectReference();
        }
        return this.reference;
    }



    /**
     * Creates a link.
     * @param writer
     * @param rect
     * @param highlight
     * @return A PdfAnnotation
     */
    private static PdfAnnotation createLink(final PdfWriter writer, final Rectangle rect, final PdfName highlight) {
        final PdfAnnotation annot = new PdfAnnotation(writer, rect);
        annot.put(PdfName.SUBTYPE, PdfName.LINK);
        if (!highlight.equals(HIGHLIGHT_INVERT)) {
			annot.put(PdfName.H, highlight);
		}
        return annot;
    }



    /** Creates a file attachment annotation
     * @param writer
     * @param rect
     * @param contents
     * @param fs
     * @return the annotation
     * @throws IOException
     */
    private static PdfAnnotation createFileAttachment(final PdfWriter writer, final Rectangle rect, final String contents, final PdfFileSpecification fs) throws IOException {
        final PdfAnnotation annot = new PdfAnnotation(writer, rect);
        annot.put(PdfName.SUBTYPE, PdfName.FILEATTACHMENT);
        if (contents != null) {
			annot.put(PdfName.CONTENTS, new PdfString(contents, PdfObject.TEXT_UNICODE));
		}
        annot.put(PdfName.FS, fs.getReference());
        return annot;
    }



    public void setDefaultAppearanceString(final PdfContentByte cb) {
        final byte b[] = cb.getInternalBuffer().toByteArray();
        final int len = b.length;
        for (int k = 0; k < len; ++k) {
            if (b[k] == '\n') {
				b[k] = 32;
			}
        }
        put(PdfName.DA, new PdfString(b));
    }

    public void setFlags(final int flags) {
        if (flags == 0) {
			remove(PdfName.F);
		} else {
			put(PdfName.F, new PdfNumber(flags));
		}
    }

    public void setBorder(final PdfBorderArray border) {
        put(PdfName.BORDER, border);
    }

    public void setBorderStyle(final PdfBorderDictionary border) {
        put(PdfName.BS, border);
    }

    /**
     * Sets the annotation's highlighting mode. The values can be
     * <CODE>HIGHLIGHT_NONE</CODE>, <CODE>HIGHLIGHT_INVERT</CODE>,
     * <CODE>HIGHLIGHT_OUTLINE</CODE> and <CODE>HIGHLIGHT_PUSH</CODE>;
     * @param highlight the annotation's highlighting mode
     */
    public void setHighlighting(final PdfName highlight) {
        if (highlight.equals(HIGHLIGHT_INVERT)) {
			remove(PdfName.H);
		} else {
			put(PdfName.H, highlight);
		}
    }

    void setAppearance(final PdfName ap, final PdfTemplate template) {
        PdfDictionary dic = (PdfDictionary)get(PdfName.AP);
        if (dic == null) {
			dic = new PdfDictionary();
		}
        dic.put(ap, template.getIndirectReference());
        put(PdfName.AP, dic);
        if (!this.form) {
			return;
		}
        if (this.templates == null) {
			this.templates = new HashMap();
		}
        this.templates.put(template, null);
    }

    void setAppearance(final PdfName ap, final String state, final PdfTemplate template) {
        PdfDictionary dicAp = (PdfDictionary)get(PdfName.AP);
        if (dicAp == null) {
			dicAp = new PdfDictionary();
		}

        PdfDictionary dic;
        final PdfObject obj = dicAp.get(ap);
        if (obj != null && obj.isDictionary()) {
			dic = (PdfDictionary)obj;
		} else {
			dic = new PdfDictionary();
		}
        dic.put(new PdfName(state), template.getIndirectReference());
        dicAp.put(ap, dic);
        put(PdfName.AP, dicAp);
        if (!this.form) {
			return;
		}
        if (this.templates == null) {
			this.templates = new HashMap();
		}
        this.templates.put(template, null);
    }

    public void setAppearanceState(final String state) {
        if (state == null) {
            remove(PdfName.AS);
            return;
        }
        put(PdfName.AS, new PdfName(state));
    }

    public void setColor(final Color color) {
        put(PdfName.C, new PdfColor(color));
    }

    public void setTitle(final String title) {
        if (title == null) {
            remove(PdfName.T);
            return;
        }
        put(PdfName.T, new PdfString(title, PdfObject.TEXT_UNICODE));
    }

    public void setPopup(final PdfAnnotation popup) {
        put(PdfName.POPUP, popup.getIndirectReference());
        popup.put(PdfName.PARENT, getIndirectReference());
    }

    public void setAction(final PdfAction action) {
        put(PdfName.A, action);
    }

    /** Getter for property used.
     * @return Value of property used.
     */
    public boolean isUsed() {
        return this.used;
    }

    /** Setter for property used.
     */
    public void setUsed() {
        this.used = true;
    }

    public HashMap getTemplates() {
        return this.templates;
    }

    /** Getter for property form.
     * @return Value of property form.
     */
    public boolean isForm() {
        return this.form;
    }

    /** Getter for property annotation.
     * @return Value of property annotation.
     */
    public boolean isAnnotation() {
        return this.annotation;
    }

    public void setPage(final int page) {
        put(PdfName.P, this.writer.getPageReference(page));
    }

    void setPage() {
        put(PdfName.P, this.writer.getCurrentPage());
    }

    /** Getter for property placeInPage.
     * @return Value of property placeInPage.
     */
    public int getPlaceInPage() {
        return this.placeInPage;
    }

    /** Places the annotation in a specified page that must be greater
     * or equal to the current one. With <code>PdfStamper</code> the page
     * can be any. The first page is 1.
     * @param placeInPage New value of property placeInPage.
     */
    public void setPlaceInPage(final int placeInPage) {
        this.placeInPage = placeInPage;
    }

    public void setRotate(final int v) {
        put(PdfName.ROTATE, new PdfNumber(v));
    }

    private PdfDictionary getMK() {
        PdfDictionary mk = (PdfDictionary)get(PdfName.MK);
        if (mk == null) {
            mk = new PdfDictionary();
            put(PdfName.MK, mk);
        }
        return mk;
    }

    public void setMKRotation(final int rotation) {
        getMK().put(PdfName.R, new PdfNumber(rotation));
    }

    private static PdfArray getMKColor(final Color color) {
        final PdfArray array = new PdfArray();
        final int type = ExtendedColor.getType(color);
        switch (type) {
            case ExtendedColor.TYPE_GRAY: {
                array.add(new PdfNumber(((GrayColor)color).getGray()));
                break;
            }
            case ExtendedColor.TYPE_CMYK: {
                final CMYKColor cmyk = (CMYKColor)color;
                array.add(new PdfNumber(cmyk.getCyan()));
                array.add(new PdfNumber(cmyk.getMagenta()));
                array.add(new PdfNumber(cmyk.getYellow()));
                array.add(new PdfNumber(cmyk.getBlack()));
                break;
            }
            case ExtendedColor.TYPE_SEPARATION:
            case ExtendedColor.TYPE_PATTERN:
            case ExtendedColor.TYPE_SHADING:
                throw new RuntimeException("Separations, patterns and shadings are not allowed in MK dictionary.");
            default:
                array.add(new PdfNumber(color.getRed() / 255f));
                array.add(new PdfNumber(color.getGreen() / 255f));
                array.add(new PdfNumber(color.getBlue() / 255f));
        }
        return array;
    }

    public void setMKBorderColor(final Color color) {
        if (color == null) {
			getMK().remove(PdfName.BC);
		} else {
			getMK().put(PdfName.BC, getMKColor(color));
		}
    }

    public void setMKBackgroundColor(final Color color) {
        if (color == null) {
			getMK().remove(PdfName.BG);
		} else {
			getMK().put(PdfName.BG, getMKColor(color));
		}
    }

    public void setMKNormalCaption(final String caption) {
        getMK().put(PdfName.CA, new PdfString(caption, PdfObject.TEXT_UNICODE));
    }

    public void setMKRolloverCaption(final String caption) {
        getMK().put(PdfName.RC, new PdfString(caption, PdfObject.TEXT_UNICODE));
    }

    public void setMKAlternateCaption(final String caption) {
        getMK().put(PdfName.AC, new PdfString(caption, PdfObject.TEXT_UNICODE));
    }

    public void setMKNormalIcon(final PdfTemplate template) {
        getMK().put(PdfName.I, template.getIndirectReference());
    }

    public void setMKRolloverIcon(final PdfTemplate template) {
        getMK().put(PdfName.RI, template.getIndirectReference());
    }

    public void setMKAlternateIcon(final PdfTemplate template) {
        getMK().put(PdfName.IX, template.getIndirectReference());
    }

    void setMKIconFit(final PdfName scale, final PdfName scalingType, final float leftoverLeft, final float leftoverBottom, final boolean fitInBounds) {
        final PdfDictionary dic = new PdfDictionary();
        if (!scale.equals(PdfName.A)) {
			dic.put(PdfName.SW, scale);
		}
        if (!scalingType.equals(PdfName.P)) {
			dic.put(PdfName.S, scalingType);
		}
        if (leftoverLeft != 0.5f || leftoverBottom != 0.5f) {
            final PdfArray array = new PdfArray(new PdfNumber(leftoverLeft));
            array.add(new PdfNumber(leftoverBottom));
            dic.put(PdfName.A, array);
        }
        if (fitInBounds) {
			dic.put(PdfName.FB, PdfBoolean.PDFTRUE);
		}
        getMK().put(PdfName.IF, dic);
    }

    public void setMKTextPosition(final int tp) {
        getMK().put(PdfName.TP, new PdfNumber(tp));
    }

    /**
     * Sets the layer this annotation belongs to.
     * @param layer the layer this annotation belongs to
     */
    public void setLayer(final PdfOCG layer) {
        put(PdfName.OC, layer.getRef());
    }

    /**
     * Sets the name of the annotation.
     * With this name the annotation can be identified among
     * all the annotations on a page (it has to be unique).
     */
    public void setName(final String name) {
    	put(PdfName.NM, new PdfString(name));
    }


}
