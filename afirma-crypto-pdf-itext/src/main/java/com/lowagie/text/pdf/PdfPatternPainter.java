/*
 * Copyright 2002 by Phillip Pan
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

import com.lowagie.text.DocumentException;
import com.lowagie.text.Image;
import com.lowagie.text.Rectangle;

/**
 * Implements the pattern.
 */

public final class PdfPatternPainter extends PdfTemplate {

    private float xstep, ystep;
    private boolean stencil = false;
    private Color defaultColor;

    /**
     *Creates a <CODE>PdfPattern</CODE>.
     */

    private PdfPatternPainter() {
        super();
        this.type = TYPE_PATTERN;
    }

    /**
     * Creates new PdfPattern
     *
     * @param wr the <CODE>PdfWriter</CODE>
     */

    PdfPatternPainter(final PdfWriter wr) {
        super(wr);
        this.type = TYPE_PATTERN;
    }

    PdfPatternPainter(final PdfWriter wr, final Color defaultColor) {
        this(wr);
        this.stencil = true;
        if (defaultColor == null) {
			this.defaultColor = Color.gray;
		} else {
			this.defaultColor = defaultColor;
		}
    }

    /**
     * Sets the horizontal interval of this pattern.
     *
     * @param xstep the xstep in horizontal painting
     */

    public void setXStep(final float xstep) {
        this.xstep = xstep;
    }

    /**
     * Sets the vertical interval of this pattern.
     *
     * @param ystep in vertical painting
     */

    public void setYStep(final float ystep) {
        this.ystep = ystep;
    }

    /**
     * Returns the horizontal interval when repeating the pattern.
     * @return a value
     */
    public float getXStep() {
        return this.xstep;
    }

    /**
     * Returns the vertical interval when repeating the pattern.
     * @return a value
     */
    public float getYStep() {
        return this.ystep;
    }

    /**
     * Tells you if this pattern is colored/uncolored (stencil = uncolored, you need to set a default color).
     * @return true if the pattern is an uncolored tiling pattern (stencil).
     */
    public boolean isStencil() {
        return this.stencil;
    }

    /**
     * Sets the transformation matrix for the pattern.
     * @param a
     * @param b
     * @param c
     * @param d
     * @param e
     * @param f
     */
    void setPatternMatrix(final float a, final float b, final float c, final float d, final float e, final float f) {
        setMatrix(a, b, c, d, e, f);
    }


    /**
     * Gets the stream representing this pattern
     * @param	compressionLevel	the compression level of the stream
     * @return the stream representing this pattern
     * @since	2.1.3
     */
    PdfPattern getPattern(final int compressionLevel) {
        return new PdfPattern(this, compressionLevel);
    }

    /**
     * Gets a duplicate of this <CODE>PdfPatternPainter</CODE>. All
     * the members are copied by reference but the buffer stays different.
     * @return a copy of this <CODE>PdfPatternPainter</CODE>
     */

    @Override
	public PdfContentByte getDuplicate() {
        final PdfPatternPainter tpl = new PdfPatternPainter();
        tpl.writer = this.writer;
        tpl.pdf = this.pdf;
        tpl.thisReference = this.thisReference;
        tpl.pageResources = this.pageResources;
        tpl.bBox = new Rectangle(this.bBox);
        tpl.xstep = this.xstep;
        tpl.ystep = this.ystep;
        tpl.matrix = this.matrix;
        tpl.stencil = this.stencil;
        tpl.defaultColor = this.defaultColor;
        return tpl;
    }

    /**
     * Returns the default color of the pattern.
     * @return a Color
     */
    public Color getDefaultColor() {
        return this.defaultColor;
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setGrayFill(float)
     */
    @Override
	public void setGrayFill(final float gray) {
        checkNoColor();
        super.setGrayFill(gray);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#resetGrayFill()
     */
    @Override
	public void resetGrayFill() {
        checkNoColor();
        super.resetGrayFill();
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setGrayStroke(float)
     */
    @Override
	public void setGrayStroke(final float gray) {
        checkNoColor();
        super.setGrayStroke(gray);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#resetGrayStroke()
     */
    @Override
	public void resetGrayStroke() {
        checkNoColor();
        super.resetGrayStroke();
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setRGBColorFillF(float, float, float)
     */
    @Override
	public void setRGBColorFillF(final float red, final float green, final float blue) {
        checkNoColor();
        super.setRGBColorFillF(red, green, blue);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#resetRGBColorFill()
     */
    @Override
	public void resetRGBColorFill() {
        checkNoColor();
        super.resetRGBColorFill();
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setRGBColorStrokeF(float, float, float)
     */
    @Override
	public void setRGBColorStrokeF(final float red, final float green, final float blue) {
        checkNoColor();
        super.setRGBColorStrokeF(red, green, blue);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#resetRGBColorStroke()
     */
    @Override
	public void resetRGBColorStroke() {
        checkNoColor();
        super.resetRGBColorStroke();
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setCMYKColorFillF(float, float, float, float)
     */
    @Override
	public void setCMYKColorFillF(final float cyan, final float magenta, final float yellow, final float black) {
        checkNoColor();
        super.setCMYKColorFillF(cyan, magenta, yellow, black);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#resetCMYKColorFill()
     */
    @Override
	public void resetCMYKColorFill() {
        checkNoColor();
        super.resetCMYKColorFill();
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setCMYKColorStrokeF(float, float, float, float)
     */
    @Override
	public void setCMYKColorStrokeF(final float cyan, final float magenta, final float yellow, final float black) {
        checkNoColor();
        super.setCMYKColorStrokeF(cyan, magenta, yellow, black);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#resetCMYKColorStroke()
     */
    @Override
	public void resetCMYKColorStroke() {
        checkNoColor();
        super.resetCMYKColorStroke();
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#addImage(com.lowagie.text.Image, float, float, float, float, float, float)
     */
    @Override
	public void addImage(final Image image, final float a, final float b, final float c, final float d, final float e, final float f) throws DocumentException {
        if (this.stencil && !image.isMask()) {
			checkNoColor();
		}
        super.addImage(image, a, b, c, d, e, f);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setCMYKColorFill(int, int, int, int)
     */
    @Override
	public void setCMYKColorFill(final int cyan, final int magenta, final int yellow, final int black) {
        checkNoColor();
        super.setCMYKColorFill(cyan, magenta, yellow, black);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setCMYKColorStroke(int, int, int, int)
     */
    @Override
	public void setCMYKColorStroke(final int cyan, final int magenta, final int yellow, final int black) {
        checkNoColor();
        super.setCMYKColorStroke(cyan, magenta, yellow, black);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setRGBColorFill(int, int, int)
     */
    @Override
	public void setRGBColorFill(final int red, final int green, final int blue) {
        checkNoColor();
        super.setRGBColorFill(red, green, blue);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setRGBColorStroke(int, int, int)
     */
    @Override
	public void setRGBColorStroke(final int red, final int green, final int blue) {
        checkNoColor();
        super.setRGBColorStroke(red, green, blue);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setColorStroke(java.awt.Color)
     */
    @Override
	public void setColorStroke(final Color color) {
        checkNoColor();
        super.setColorStroke(color);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setColorFill(java.awt.Color)
     */
    @Override
	public void setColorFill(final Color color) {
        checkNoColor();
        super.setColorFill(color);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setColorFill(com.lowagie.text.pdf.PdfSpotColor, float)
     */
    @Override
	public void setColorFill(final PdfSpotColor sp, final float tint) {
        checkNoColor();
        super.setColorFill(sp, tint);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setColorStroke(com.lowagie.text.pdf.PdfSpotColor, float)
     */
    @Override
	public void setColorStroke(final PdfSpotColor sp, final float tint) {
        checkNoColor();
        super.setColorStroke(sp, tint);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setPatternFill(com.lowagie.text.pdf.PdfPatternPainter)
     */
    @Override
	public void setPatternFill(final PdfPatternPainter p) {
        checkNoColor();
        super.setPatternFill(p);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setPatternFill(com.lowagie.text.pdf.PdfPatternPainter, java.awt.Color, float)
     */
    @Override
	public void setPatternFill(final PdfPatternPainter p, final Color color, final float tint) {
        checkNoColor();
        super.setPatternFill(p, color, tint);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setPatternStroke(com.lowagie.text.pdf.PdfPatternPainter, java.awt.Color, float)
     */
    @Override
	public void setPatternStroke(final PdfPatternPainter p, final Color color, final float tint) {
        checkNoColor();
        super.setPatternStroke(p, color, tint);
    }

    /**
     * @see com.lowagie.text.pdf.PdfContentByte#setPatternStroke(com.lowagie.text.pdf.PdfPatternPainter)
     */
    @Override
	public void setPatternStroke(final PdfPatternPainter p) {
        checkNoColor();
        super.setPatternStroke(p);
    }

    private void checkNoColor() {
        if (this.stencil) {
			throw new RuntimeException("Colors are not allowed in uncolored tile patterns.");
		}
    }
}
