/*
 * $Id: PdfContentByte.java 3912 2009-04-26 08:38:15Z blowagie $
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
import harmony.java.awt.Color;

import java.util.ArrayList;
import java.util.HashMap;

import com.lowagie.text.Annotation;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.ImgJBIG2;
import com.lowagie.text.Rectangle;
import com.lowagie.text.exceptions.IllegalPdfSyntaxException;
import com.lowagie.text.pdf.internal.PdfAnnotationsImp;
import com.lowagie.text.pdf.internal.PdfXConformanceImp;

/**
 * <CODE>PdfContentByte</CODE> is an object containing the user positioned
 * text and graphic contents of a page. It knows how to apply the proper
 * font encoding.
 */

public class PdfContentByte {

    /**
     * This class keeps the graphic state of the current page
     */

    static class GraphicState {

        /** This is the font in use */
        FontDetails fontDetails;

        /** This is the color in use */
        private ColorDetails colorDetails;

        /** This is the font size in use */
        float size;

        /** The x position of the text line matrix. */
        private float xTLM = 0;
        /** The y position of the text line matrix. */
        private float yTLM = 0;

        /** The current text leading. */
        private float leading = 0;

        /** The current horizontal scaling */
        private float scale = 100;

        /** The current character spacing */
        private float charSpace = 0;

        /** The current word spacing */
        private float wordSpace = 0;

        GraphicState() {
        }

        private GraphicState(final GraphicState cp) {
            this.fontDetails = cp.fontDetails;
            this.colorDetails = cp.colorDetails;
            this.size = cp.size;
            this.xTLM = cp.xTLM;
            this.yTLM = cp.yTLM;
            this.leading = cp.leading;
            this.scale = cp.scale;
            this.charSpace = cp.charSpace;
            this.wordSpace = cp.wordSpace;
        }
    }

    /** The alignment is center */
    static final int ALIGN_CENTER = Element.ALIGN_CENTER;

    /** The alignment is left */
    static final int ALIGN_LEFT = Element.ALIGN_LEFT;

    /** The alignment is right */
    private static final int ALIGN_RIGHT = Element.ALIGN_RIGHT;

    /** A possible line cap value */
    private static final int LINE_CAP_BUTT = 0;



    /** A possible line join value */
    private static final int LINE_JOIN_MITER = 0;



    /** A possible text rendering value */
    static final int TEXT_RENDER_MODE_FILL = 0;
    /** A possible text rendering value */
    static final int TEXT_RENDER_MODE_STROKE = 1;
    /** A possible text rendering value */
    static final int TEXT_RENDER_MODE_FILL_STROKE = 2;






    private static final float[] unitRect = {0, 0, 0, 1, 1, 0, 1, 1};
    // membervariables

    /** This is the actual content */
    protected ByteBuffer content = new ByteBuffer();

    /** This is the writer */
    protected PdfWriter writer;

    /** This is the PdfDocument */
    protected PdfDocument pdf;

    /** This is the GraphicState in use */
    protected GraphicState state = new GraphicState();

    /** The list were we save/restore the state */
    private final ArrayList stateList = new ArrayList();

    /** The list were we save/restore the layer depth */
    private ArrayList layerDepth;

    /** The separator between commands.
     */
    protected int separator = '\n';

    private int mcDepth = 0;
    private boolean inText = false;

    private static HashMap abrev = new HashMap();

    static {
        abrev.put(PdfName.BITSPERCOMPONENT, "/BPC ");
        abrev.put(PdfName.COLORSPACE, "/CS ");
        abrev.put(PdfName.DECODE, "/D ");
        abrev.put(PdfName.DECODEPARMS, "/DP ");
        abrev.put(PdfName.FILTER, "/F ");
        abrev.put(PdfName.HEIGHT, "/H ");
        abrev.put(PdfName.IMAGEMASK, "/IM ");
        abrev.put(PdfName.INTENT, "/Intent ");
        abrev.put(PdfName.INTERPOLATE, "/I ");
        abrev.put(PdfName.WIDTH, "/W ");
    }

    // constructors

    /**
     * Constructs a new <CODE>PdfContentByte</CODE>-object.
     *
     * @param wr the writer associated to this content
     */

    PdfContentByte(final PdfWriter wr) {
        if (wr != null) {
            this.writer = wr;
            this.pdf = this.writer.getPdfDocument();
        }
    }

    // methods to get the content of this object

    /**
     * Returns the <CODE>String</CODE> representation of this <CODE>PdfContentByte</CODE>-object.
     *
     * @return      a <CODE>String</CODE>
     */

    @Override
	public String toString() {
        return this.content.toString();
    }

    /**
     * Gets the internal buffer.
     * @return the internal buffer
     */
    public ByteBuffer getInternalBuffer() {
        return this.content;
    }

    /** Returns the PDF representation of this <CODE>PdfContentByte</CODE>-object.
     *
     * @param writer the <CODE>PdfWriter</CODE>
     * @return a <CODE>byte</CODE> array with the representation
     */

    byte[] toPdf(final PdfWriter writer) {
    	sanityCheck();
        return this.content.toByteArray();
    }

    // methods to add graphical content

    /**
     * Adds the content of another <CODE>PdfContent</CODE>-object to this object.
     *
     * @param       other       another <CODE>PdfByteContent</CODE>-object
     */

    void add(final PdfContentByte other) {
        if (other.writer != null && this.writer != other.writer) {
			throw new RuntimeException("Inconsistent writers. Are you mixing two documents?");
		}
        this.content.append(other.content);
    }

    /**
     * Gets the x position of the text line matrix.
     *
     * @return the x position of the text line matrix
     */
    public float getXTLM() {
        return this.state.xTLM;
    }

    /**
     * Gets the y position of the text line matrix.
     *
     * @return the y position of the text line matrix
     */
    public float getYTLM() {
        return this.state.yTLM;
    }

    /**
     * Gets the current text leading.
     *
     * @return the current text leading
     */
    public float getLeading() {
        return this.state.leading;
    }

    /**
     * Gets the current character spacing.
     *
     * @return the current character spacing
     */
    public float getCharacterSpacing() {
        return this.state.charSpace;
    }

    /**
     * Gets the current word spacing.
     *
     * @return the current word spacing
     */
    public float getWordSpacing() {
        return this.state.wordSpace;
    }

    /**
     * Gets the current character spacing.
     *
     * @return the current character spacing
     */
    public float getHorizontalScaling() {
        return this.state.scale;
    }

    /**
     * Changes the <VAR>Flatness</VAR>.
     * <P>
     * <VAR>Flatness</VAR> sets the maximum permitted distance in device pixels between the
     * mathematically correct path and an approximation constructed from straight line segments.<BR>
     *
     * @param       flatness        a value
     */

    public void setFlatness(final float flatness) {
        if (flatness >= 0 && flatness <= 100) {
            this.content.append(flatness).append(" i").append_i(this.separator);
        }
    }

    /**
     * Changes the <VAR>Line cap style</VAR>.
     * <P>
     * The <VAR>line cap style</VAR> specifies the shape to be used at the end of open subpaths
     * when they are stroked.<BR>
     * Allowed values are LINE_CAP_BUTT, LINE_CAP_ROUND and LINE_CAP_PROJECTING_SQUARE.<BR>
     *
     * @param       style       a value
     */

    public void setLineCap(final int style) {
        if (style >= 0 && style <= 2) {
            this.content.append(style).append(" J").append_i(this.separator);
        }
    }

    /**
     * Changes the value of the <VAR>line dash pattern</VAR>.
     * <P>
     * The line dash pattern controls the pattern of dashes and gaps used to stroke paths.
     * It is specified by an <I>array</I> and a <I>phase</I>. The array specifies the length
     * of the alternating dashes and gaps. The phase specifies the distance into the dash
     * pattern to start the dash.<BR>
     *
     * @param       phase       the value of the phase
     */

    public void setLineDash(final float phase) {
        this.content.append("[] ").append(phase).append(" d").append_i(this.separator);
    }

    /**
     * Changes the value of the <VAR>line dash pattern</VAR>.
     * <P>
     * The line dash pattern controls the pattern of dashes and gaps used to stroke paths.
     * It is specified by an <I>array</I> and a <I>phase</I>. The array specifies the length
     * of the alternating dashes and gaps. The phase specifies the distance into the dash
     * pattern to start the dash.<BR>
     *
     * @param       phase       the value of the phase
     * @param       unitsOn     the number of units that must be 'on' (equals the number of units that must be 'off').
     */

    public void setLineDash(final float unitsOn, final float phase) {
        this.content.append("[").append(unitsOn).append("] ").append(phase).append(" d").append_i(this.separator);
    }

    /**
     * Changes the value of the <VAR>line dash pattern</VAR>.
     * <P>
     * The line dash pattern controls the pattern of dashes and gaps used to stroke paths.
     * It is specified by an <I>array</I> and a <I>phase</I>. The array specifies the length
     * of the alternating dashes and gaps. The phase specifies the distance into the dash
     * pattern to start the dash.<BR>
     *
     * @param       phase       the value of the phase
     * @param       unitsOn     the number of units that must be 'on'
     * @param       unitsOff    the number of units that must be 'off'
     */

    public void setLineDash(final float unitsOn, final float unitsOff, final float phase) {
        this.content.append("[").append(unitsOn).append(' ').append(unitsOff).append("] ").append(phase).append(" d").append_i(this.separator);
    }



    /**
     * Changes the <VAR>Line join style</VAR>.
     * <P>
     * The <VAR>line join style</VAR> specifies the shape to be used at the corners of paths
     * that are stroked.<BR>
     * Allowed values are LINE_JOIN_MITER (Miter joins), LINE_JOIN_ROUND (Round joins) and LINE_JOIN_BEVEL (Bevel joins).<BR>
     *
     * @param       style       a value
     */

    public void setLineJoin(final int style) {
        if (style >= 0 && style <= 2) {
            this.content.append(style).append(" j").append_i(this.separator);
        }
    }

    /**
     * Changes the <VAR>line width</VAR>.
     * <P>
     * The line width specifies the thickness of the line used to stroke a path and is measured
     * in user space units.<BR>
     *
     * @param       w           a width
     */

    public void setLineWidth(final float w) {
        this.content.append(w).append(" w").append_i(this.separator);
    }

    /**
     * Changes the <VAR>Miter limit</VAR>.
     * <P>
     * When two line segments meet at a sharp angle and mitered joins have been specified as the
     * line join style, it is possible for the miter to extend far beyond the thickness of the line
     * stroking path. The miter limit imposes a maximum on the ratio of the miter length to the line
     * witdh. When the limit is exceeded, the join is converted from a miter to a bevel.<BR>
     *
     * @param       miterLimit      a miter limit
     */

    public void setMiterLimit(final float miterLimit) {
        if (miterLimit > 1) {
            this.content.append(miterLimit).append(" M").append_i(this.separator);
        }
    }

    /**
     * Modify the current clipping path by intersecting it with the current path, using the
     * nonzero winding number rule to determine which regions lie inside the clipping
     * path.
     */

    public void clip() {
        this.content.append("W").append_i(this.separator);
    }

    /**
     * Modify the current clipping path by intersecting it with the current path, using the
     * even-odd rule to determine which regions lie inside the clipping path.
     */

    public void eoClip() {
        this.content.append("W*").append_i(this.separator);
    }

    /**
     * Changes the currentgray tint for filling paths (device dependent colors!).
     * <P>
     * Sets the color space to <B>DeviceGray</B> (or the <B>DefaultGray</B> color space),
     * and sets the gray tint to use for filling paths.</P>
     *
     * @param   gray    a value between 0 (black) and 1 (white)
     */

    public void setGrayFill(final float gray) {
        this.content.append(gray).append(" g").append_i(this.separator);
    }

    /**
     * Changes the current gray tint for filling paths to black.
     */

    public void resetGrayFill() {
        this.content.append("0 g").append_i(this.separator);
    }

    /**
     * Changes the currentgray tint for stroking paths (device dependent colors!).
     * <P>
     * Sets the color space to <B>DeviceGray</B> (or the <B>DefaultGray</B> color space),
     * and sets the gray tint to use for stroking paths.</P>
     *
     * @param   gray    a value between 0 (black) and 1 (white)
     */

    public void setGrayStroke(final float gray) {
        this.content.append(gray).append(" G").append_i(this.separator);
    }

    /**
     * Changes the current gray tint for stroking paths to black.
     */

    public void resetGrayStroke() {
        this.content.append("0 G").append_i(this.separator);
    }

    /**
     * Helper to validate and write the RGB color components
     * @param   red     the intensity of red. A value between 0 and 1
     * @param   green   the intensity of green. A value between 0 and 1
     * @param   blue    the intensity of blue. A value between 0 and 1
     */
    private void HelperRGB(float red, float green, float blue) {
    	PdfXConformanceImp.checkPDFXConformance(this.writer, PdfXConformanceImp.PDFXKEY_RGB, null);
        if (red < 0) {
			red = 0.0f;
		} else if (red > 1.0f) {
			red = 1.0f;
		}
        if (green < 0) {
			green = 0.0f;
		} else if (green > 1.0f) {
			green = 1.0f;
		}
        if (blue < 0) {
			blue = 0.0f;
		} else if (blue > 1.0f) {
			blue = 1.0f;
		}
        this.content.append(red).append(' ').append(green).append(' ').append(blue);
    }

    /**
     * Changes the current color for filling paths (device dependent colors!).
     * <P>
     * Sets the color space to <B>DeviceRGB</B> (or the <B>DefaultRGB</B> color space),
     * and sets the color to use for filling paths.</P>
     * <P>
     * Following the PDF manual, each operand must be a number between 0 (minimum intensity) and
     * 1 (maximum intensity).</P>
     *
     * @param   red     the intensity of red. A value between 0 and 1
     * @param   green   the intensity of green. A value between 0 and 1
     * @param   blue    the intensity of blue. A value between 0 and 1
     */

    public void setRGBColorFillF(final float red, final float green, final float blue) {
        HelperRGB(red, green, blue);
        this.content.append(" rg").append_i(this.separator);
    }

    /**
     * Changes the current color for filling paths to black.
     */

    public void resetRGBColorFill() {
        this.content.append("0 g").append_i(this.separator);
    }

    /**
     * Changes the current color for stroking paths (device dependent colors!).
     * <P>
     * Sets the color space to <B>DeviceRGB</B> (or the <B>DefaultRGB</B> color space),
     * and sets the color to use for stroking paths.</P>
     * <P>
     * Following the PDF manual, each operand must be a number between 0 (miniumum intensity) and
     * 1 (maximum intensity).
     *
     * @param   red     the intensity of red. A value between 0 and 1
     * @param   green   the intensity of green. A value between 0 and 1
     * @param   blue    the intensity of blue. A value between 0 and 1
     */

    public void setRGBColorStrokeF(final float red, final float green, final float blue) {
        HelperRGB(red, green, blue);
        this.content.append(" RG").append_i(this.separator);
    }

    /**
     * Changes the current color for stroking paths to black.
     *
     */

    public void resetRGBColorStroke() {
        this.content.append("0 G").append_i(this.separator);
    }

    /**
     * Helper to validate and write the CMYK color components.
     *
     * @param   cyan    the intensity of cyan. A value between 0 and 1
     * @param   magenta the intensity of magenta. A value between 0 and 1
     * @param   yellow  the intensity of yellow. A value between 0 and 1
     * @param   black   the intensity of black. A value between 0 and 1
     */
    private void HelperCMYK(float cyan, float magenta, float yellow, float black) {
        if (cyan < 0) {
			cyan = 0.0f;
		} else if (cyan > 1.0f) {
			cyan = 1.0f;
		}
        if (magenta < 0) {
			magenta = 0.0f;
		} else if (magenta > 1.0f) {
			magenta = 1.0f;
		}
        if (yellow < 0) {
			yellow = 0.0f;
		} else if (yellow > 1.0f) {
			yellow = 1.0f;
		}
        if (black < 0) {
			black = 0.0f;
		} else if (black > 1.0f) {
			black = 1.0f;
		}
        this.content.append(cyan).append(' ').append(magenta).append(' ').append(yellow).append(' ').append(black);
    }

    /**
     * Changes the current color for filling paths (device dependent colors!).
     * <P>
     * Sets the color space to <B>DeviceCMYK</B> (or the <B>DefaultCMYK</B> color space),
     * and sets the color to use for filling paths.</P>
     * <P>
     * Following the PDF manual, each operand must be a number between 0 (no ink) and
     * 1 (maximum ink).</P>
     *
     * @param   cyan    the intensity of cyan. A value between 0 and 1
     * @param   magenta the intensity of magenta. A value between 0 and 1
     * @param   yellow  the intensity of yellow. A value between 0 and 1
     * @param   black   the intensity of black. A value between 0 and 1
     */

    public void setCMYKColorFillF(final float cyan, final float magenta, final float yellow, final float black) {
        HelperCMYK(cyan, magenta, yellow, black);
        this.content.append(" k").append_i(this.separator);
    }

    /**
     * Changes the current color for filling paths to black.
     *
     */

    public void resetCMYKColorFill() {
        this.content.append("0 0 0 1 k").append_i(this.separator);
    }

    /**
     * Changes the current color for stroking paths (device dependent colors!).
     * <P>
     * Sets the color space to <B>DeviceCMYK</B> (or the <B>DefaultCMYK</B> color space),
     * and sets the color to use for stroking paths.</P>
     * <P>
     * Following the PDF manual, each operand must be a number between 0 (miniumum intensity) and
     * 1 (maximum intensity).
     *
     * @param   cyan    the intensity of cyan. A value between 0 and 1
     * @param   magenta the intensity of magenta. A value between 0 and 1
     * @param   yellow  the intensity of yellow. A value between 0 and 1
     * @param   black   the intensity of black. A value between 0 and 1
     */

    public void setCMYKColorStrokeF(final float cyan, final float magenta, final float yellow, final float black) {
        HelperCMYK(cyan, magenta, yellow, black);
        this.content.append(" K").append_i(this.separator);
    }

    /**
     * Changes the current color for stroking paths to black.
     *
     */

    public void resetCMYKColorStroke() {
        this.content.append("0 0 0 1 K").append_i(this.separator);
    }

    /**
     * Move the current point <I>(x, y)</I>, omitting any connecting line segment.
     *
     * @param       x               new x-coordinate
     * @param       y               new y-coordinate
     */

    public void moveTo(final float x, final float y) {
        this.content.append(x).append(' ').append(y).append(" m").append_i(this.separator);
    }

    /**
     * Appends a straight line segment from the current point <I>(x, y)</I>. The new current
     * point is <I>(x, y)</I>.
     *
     * @param       x               new x-coordinate
     * @param       y               new y-coordinate
     */

    public void lineTo(final float x, final float y) {
        this.content.append(x).append(' ').append(y).append(" l").append_i(this.separator);
    }

    /**
     * Appends a B&#xea;zier curve to the path, starting from the current point.
     *
     * @param       x1      x-coordinate of the first control point
     * @param       y1      y-coordinate of the first control point
     * @param       x2      x-coordinate of the second control point
     * @param       y2      y-coordinate of the second control point
     * @param       x3      x-coordinate of the ending point (= new current point)
     * @param       y3      y-coordinate of the ending point (= new current point)
     */

    public void curveTo(final float x1, final float y1, final float x2, final float y2, final float x3, final float y3) {
        this.content.append(x1).append(' ').append(y1).append(' ').append(x2).append(' ').append(y2).append(' ').append(x3).append(' ').append(y3).append(" c").append_i(this.separator);
    }

    /**
     * Appends a B&#xea;zier curve to the path, starting from the current point.
     *
     * @param       x2      x-coordinate of the second control point
     * @param       y2      y-coordinate of the second control point
     * @param       x3      x-coordinate of the ending point (= new current point)
     * @param       y3      y-coordinate of the ending point (= new current point)
     */

    void curveTo(final float x2, final float y2, final float x3, final float y3) {
        this.content.append(x2).append(' ').append(y2).append(' ').append(x3).append(' ').append(y3).append(" v").append_i(this.separator);
    }







    /**
     * Adds a rectangle to the current path.
     *
     * @param       x       x-coordinate of the starting point
     * @param       y       y-coordinate of the starting point
     * @param       w       width
     * @param       h       height
     */

    public void rectangle(final float x, final float y, final float w, final float h) {
        this.content.append(x).append(' ').append(y).append(' ').append(w).append(' ').append(h).append(" re").append_i(this.separator);
    }

    private boolean compareColors(final Color c1, final Color c2) {
        if (c1 == null && c2 == null) {
			return true;
		}
        if (c1 == null || c2 == null) {
			return false;
		}
        if (c1 instanceof ExtendedColor) {
			return c1.equals(c2);
		}
        return c2.equals(c1);
    }

    /**
     * Adds a variable width border to the current path.
     * Only use if {@link com.lowagie.text.Rectangle#isUseVariableBorders() Rectangle.isUseVariableBorders}
     * = true.
     * @param rect a <CODE>Rectangle</CODE>
     */
    private void variableRectangle(final Rectangle rect) {
        final float t = rect.getTop();
        final float b = rect.getBottom();
        final float r = rect.getRight();
        final float l = rect.getLeft();
        final float wt = rect.getBorderWidthTop();
        final float wb = rect.getBorderWidthBottom();
        final float wr = rect.getBorderWidthRight();
        final float wl = rect.getBorderWidthLeft();
        final Color ct = rect.getBorderColorTop();
        final Color cb = rect.getBorderColorBottom();
        final Color cr = rect.getBorderColorRight();
        final Color cl = rect.getBorderColorLeft();
        saveState();
        setLineCap(PdfContentByte.LINE_CAP_BUTT);
        setLineJoin(PdfContentByte.LINE_JOIN_MITER);
        float clw = 0;
        boolean cdef = false;
        Color ccol = null;
        boolean cdefi = false;
        Color cfil = null;
        // draw top
        if (wt > 0) {
            setLineWidth(clw = wt);
            cdef = true;
            if (ct == null) {
				resetRGBColorStroke();
			} else {
				setColorStroke(ct);
			}
            ccol = ct;
            moveTo(l, t - wt / 2f);
            lineTo(r, t - wt / 2f);
            stroke();
        }

        // Draw bottom
        if (wb > 0) {
            if (wb != clw) {
				setLineWidth(clw = wb);
			}
            if (!cdef || !compareColors(ccol, cb)) {
                cdef = true;
                if (cb == null) {
					resetRGBColorStroke();
				} else {
					setColorStroke(cb);
				}
                ccol = cb;
            }
            moveTo(r, b + wb / 2f);
            lineTo(l, b + wb / 2f);
            stroke();
        }

        // Draw right
        if (wr > 0) {
            if (wr != clw) {
				setLineWidth(clw = wr);
			}
            if (!cdef || !compareColors(ccol, cr)) {
                cdef = true;
                if (cr == null) {
					resetRGBColorStroke();
				} else {
					setColorStroke(cr);
				}
                ccol = cr;
            }
            final boolean bt = compareColors(ct, cr);
            final boolean bb = compareColors(cb, cr);
            moveTo(r - wr / 2f, bt ? t : t - wt);
            lineTo(r - wr / 2f, bb ? b : b + wb);
            stroke();
            if (!bt || !bb) {
                cdefi = true;
                if (cr == null) {
					resetRGBColorFill();
				} else {
					setColorFill(cr);
				}
                cfil = cr;
                if (!bt) {
                    moveTo(r, t);
                    lineTo(r, t - wt);
                    lineTo(r - wr, t - wt);
                    fill();
                }
                if (!bb) {
                    moveTo(r, b);
                    lineTo(r, b + wb);
                    lineTo(r - wr, b + wb);
                    fill();
                }
            }
        }

        // Draw Left
        if (wl > 0) {
            if (wl != clw) {
				setLineWidth(wl);
			}
            if (!cdef || !compareColors(ccol, cl)) {
                if (cl == null) {
					resetRGBColorStroke();
				} else {
					setColorStroke(cl);
				}
            }
            final boolean bt = compareColors(ct, cl);
            final boolean bb = compareColors(cb, cl);
            moveTo(l + wl / 2f, bt ? t : t - wt);
            lineTo(l + wl / 2f, bb ? b : b + wb);
            stroke();
            if (!bt || !bb) {
                if (!cdefi || !compareColors(cfil, cl)) {
                    if (cl == null) {
						resetRGBColorFill();
					} else {
						setColorFill(cl);
					}
                }
                if (!bt) {
                    moveTo(l, t);
                    lineTo(l, t - wt);
                    lineTo(l + wl, t - wt);
                    fill();
                }
                if (!bb) {
                    moveTo(l, b);
                    lineTo(l, b + wb);
                    lineTo(l + wl, b + wb);
                    fill();
                }
            }
        }
        restoreState();
    }

    /**
     * Adds a border (complete or partially) to the current path..
     *
     * @param       rectangle       a <CODE>Rectangle</CODE>
     */

    public void rectangle(final Rectangle rectangle) {
        // the coordinates of the border are retrieved
        final float x1 = rectangle.getLeft();
        final float y1 = rectangle.getBottom();
        final float x2 = rectangle.getRight();
        final float y2 = rectangle.getTop();

        // the backgroundcolor is set
        final Color background = rectangle.getBackgroundColor();
        if (background != null) {
            setColorFill(background);
            rectangle(x1, y1, x2 - x1, y2 - y1);
            fill();
            resetRGBColorFill();
        }

        // if the element hasn't got any borders, nothing is added
        if (! rectangle.hasBorders()) {
            return;
        }

        // if any of the individual border colors are set
        // we draw the borders all around using the
        // different colors
        if (rectangle.isUseVariableBorders()) {
            variableRectangle(rectangle);
        }
        else {
            // the width is set to the width of the element
            if (rectangle.getBorderWidth() != Rectangle.UNDEFINED) {
                setLineWidth(rectangle.getBorderWidth());
            }

            // the color is set to the color of the element
            final Color color = rectangle.getBorderColor();
            if (color != null) {
                setColorStroke(color);
            }

            // if the box is a rectangle, it is added as a rectangle
            if (rectangle.hasBorder(Rectangle.BOX)) {
               rectangle(x1, y1, x2 - x1, y2 - y1);
            }
            // if the border isn't a rectangle, the different sides are added apart
            else {
                if (rectangle.hasBorder(Rectangle.RIGHT)) {
                    moveTo(x2, y1);
                    lineTo(x2, y2);
                }
                if (rectangle.hasBorder(Rectangle.LEFT)) {
                    moveTo(x1, y1);
                    lineTo(x1, y2);
                }
                if (rectangle.hasBorder(Rectangle.BOTTOM)) {
                    moveTo(x1, y1);
                    lineTo(x2, y1);
                }
                if (rectangle.hasBorder(Rectangle.TOP)) {
                    moveTo(x1, y2);
                    lineTo(x2, y2);
                }
            }

            stroke();

            if (color != null) {
                resetRGBColorStroke();
            }
        }
    }

    /**
     * Closes the current subpath by appending a straight line segment from the current point
     * to the starting point of the subpath.
     */

    public void closePath() {
        this.content.append("h").append_i(this.separator);
    }

    /**
     * Ends the path without filling or stroking it.
     */

    public void newPath() {
        this.content.append("n").append_i(this.separator);
    }

    /**
     * Strokes the path.
     */

    public void stroke() {
        this.content.append("S").append_i(this.separator);
    }

    /**
     * Closes the path and strokes it.
     */

    public void closePathStroke() {
        this.content.append("s").append_i(this.separator);
    }

    /**
     * Fills the path, using the non-zero winding number rule to determine the region to fill.
     */

    public void fill() {
        this.content.append("f").append_i(this.separator);
    }

    /**
     * Fills the path, using the even-odd rule to determine the region to fill.
     */

    public void eoFill() {
        this.content.append("f*").append_i(this.separator);
    }



    /**
     * Closes the path, fills it using the non-zero winding number rule to determine the region to fill and strokes it.
     */

    public void closePathFillStroke() {
        this.content.append("b").append_i(this.separator);
    }



    /**
     * Closes the path, fills it using the even-odd rule to determine the region to fill and strokes it.
     */

    public void closePathEoFillStroke() {
        this.content.append("b*").append_i(this.separator);
    }

    /**
     * Adds an <CODE>Image</CODE> to the page. The <CODE>Image</CODE> must have
     * absolute positioning.
     * @param image the <CODE>Image</CODE> object
     * @throws DocumentException if the <CODE>Image</CODE> does not have absolute positioning
     */
    public void addImage(final Image image) throws DocumentException {
        addImage(image, false);
    }

    /**
     * Adds an <CODE>Image</CODE> to the page. The <CODE>Image</CODE> must have
     * absolute positioning. The image can be placed inline.
     * @param image the <CODE>Image</CODE> object
     * @param inlineImage <CODE>true</CODE> to place this image inline, <CODE>false</CODE> otherwise
     * @throws DocumentException if the <CODE>Image</CODE> does not have absolute positioning
     */
    private void addImage(final Image image, final boolean inlineImage) throws DocumentException {
        if (!image.hasAbsoluteY()) {
			throw new DocumentException("The image must have absolute positioning.");
		}
        final float matrix[] = image.matrix();
        matrix[Image.CX] = image.getAbsoluteX() - matrix[Image.CX];
        matrix[Image.CY] = image.getAbsoluteY() - matrix[Image.CY];
        addImage(image, matrix[0], matrix[1], matrix[2], matrix[3], matrix[4], matrix[5], inlineImage);
    }

    /**
     * Adds an <CODE>Image</CODE> to the page. The positioning of the <CODE>Image</CODE>
     * is done with the transformation matrix. To position an <CODE>image</CODE> at (x,y)
     * use addImage(image, image_width, 0, 0, image_height, x, y).
     * @param image the <CODE>Image</CODE> object
     * @param a an element of the transformation matrix
     * @param b an element of the transformation matrix
     * @param c an element of the transformation matrix
     * @param d an element of the transformation matrix
     * @param e an element of the transformation matrix
     * @param f an element of the transformation matrix
     * @throws DocumentException on error
     */
    public void addImage(final Image image, final float a, final float b, final float c, final float d, final float e, final float f) throws DocumentException {
        addImage(image, a, b, c, d, e, f, false);
    }

    /**
     * Adds an <CODE>Image</CODE> to the page. The positioning of the <CODE>Image</CODE>
     * is done with the transformation matrix. To position an <CODE>image</CODE> at (x,y)
     * use addImage(image, image_width, 0, 0, image_height, x, y). The image can be placed inline.
     * @param image the <CODE>Image</CODE> object
     * @param a an element of the transformation matrix
     * @param b an element of the transformation matrix
     * @param c an element of the transformation matrix
     * @param d an element of the transformation matrix
     * @param e an element of the transformation matrix
     * @param f an element of the transformation matrix
     * @param inlineImage <CODE>true</CODE> to place this image inline, <CODE>false</CODE> otherwise
     * @throws DocumentException on error
     */
    public void addImage(final Image image, final float a, final float b, final float c, final float d, final float e, final float f, final boolean inlineImage) throws DocumentException {
        try {
            if (image.getLayer() != null) {
				beginLayer(image.getLayer());
			}
            if (image.isImgTemplate()) {
                this.writer.addDirectImageSimple(image);
                final PdfTemplate template = image.getTemplateData();
                final float w = template.getWidth();
                final float h = template.getHeight();
                addTemplate(template, a / w, b / w, c / h, d / h, e, f);
            }
            else {
                this.content.append("q ");
                this.content.append(a).append(' ');
                this.content.append(b).append(' ');
                this.content.append(c).append(' ');
                this.content.append(d).append(' ');
                this.content.append(e).append(' ');
                this.content.append(f).append(" cm");
                if (inlineImage) {
                    this.content.append("\nBI\n");
                    final PdfImage pimage = new PdfImage(image, "", null);
                    if (image instanceof ImgJBIG2) {
                    	final byte[] globals = ((ImgJBIG2)image).getGlobalBytes();
                    	if (globals != null) {
                    		final PdfDictionary decodeparms = new PdfDictionary();
                    		decodeparms.put(PdfName.JBIG2GLOBALS, this.writer.getReferenceJBIG2Globals(globals));
                    		pimage.put(PdfName.DECODEPARMS, decodeparms);
                    	}
                    }
                    for (final Object element : pimage.getKeys()) {
                        final PdfName key = (PdfName)element;
                        PdfObject value = pimage.get(key);
                        final String s = (String)abrev.get(key);
                        if (s == null) {
							continue;
						}
                        this.content.append(s);
                        boolean check = true;
                        if (key.equals(PdfName.COLORSPACE) && value.isArray()) {
                            final PdfArray ar = (PdfArray)value;
                            if (ar.size() == 4
                                && PdfName.INDEXED.equals(ar.getAsName(0))
                                && ar.getPdfObject(1).isName()
                                && ar.getPdfObject(2).isNumber()
                                && ar.getPdfObject(3).isString()
                            ) {
                                check = false;
                            }

                        }
                        if (check && key.equals(PdfName.COLORSPACE) && !value.isName()) {
                            final PdfName cs = this.writer.getColorspaceName();
                            final PageResources prs = getPageResources();
                            prs.addColor(cs, this.writer.addToBody(value).getIndirectReference());
                            value = cs;
                        }
                        value.toPdf(null, this.content);
                        this.content.append('\n');
                    }
                    this.content.append("ID\n");
                    pimage.writeContent(this.content);
                    this.content.append("\nEI\nQ").append_i(this.separator);
                }
                else {
                    PdfName name;
                    final PageResources prs = getPageResources();
                    final Image maskImage = image.getImageMask();
                    if (maskImage != null) {
                        name = this.writer.addDirectImageSimple(maskImage);
                        prs.addXObject(name, this.writer.getImageReference(name));
                    }
                    name = this.writer.addDirectImageSimple(image);
                    name = prs.addXObject(name, this.writer.getImageReference(name));
                    this.content.append(' ').append(name.getBytes()).append(" Do Q").append_i(this.separator);
                }
            }
            if (image.hasBorders()) {
                saveState();
                final float w = image.getWidth();
                final float h = image.getHeight();
                concatCTM(a / w, b / w, c / h, d / h, e, f);
                rectangle(image);
                restoreState();
            }
            if (image.getLayer() != null) {
				endLayer();
			}
            Annotation annot = image.getAnnotation();
            if (annot == null) {
				return;
			}
            final float[] r = new float[unitRect.length];
            for (int k = 0; k < unitRect.length; k += 2) {
                r[k] = a * unitRect[k] + c * unitRect[k + 1] + e;
                r[k + 1] = b * unitRect[k] + d * unitRect[k + 1] + f;
            }
            float llx = r[0];
            float lly = r[1];
            float urx = llx;
            float ury = lly;
            for (int k = 2; k < r.length; k += 2) {
                llx = Math.min(llx, r[k]);
                lly = Math.min(lly, r[k + 1]);
                urx = Math.max(urx, r[k]);
                ury = Math.max(ury, r[k + 1]);
            }
            annot = new Annotation(annot);
            annot.setDimensions(llx, lly, urx, ury);
            final PdfAnnotation an = PdfAnnotationsImp.convertAnnotation(this.writer, annot, new Rectangle(llx, lly, urx, ury));
            if (an == null) {
				return;
			}
            addAnnotation(an);
        }
        catch (final Exception ee) {
            throw new DocumentException(ee);
        }
    }

    /**
     * Makes this <CODE>PdfContentByte</CODE> empty.
     * Calls <code>reset( true )</code>
     */
    void reset() {
        reset( true );
    }

    /**
     * Makes this <CODE>PdfContentByte</CODE> empty.
     * @param validateContent will call <code>sanityCheck()</code> if true.
     * @since 2.1.6
     */
    private void reset( final boolean validateContent ) {
        this.content.reset();
        if (validateContent) {
        	sanityCheck();
        }
        this.state = new GraphicState();
    }


    /**
     * Starts the writing of text.
     */
    public void beginText() {
    	if (this.inText) {
    		throw new IllegalPdfSyntaxException("Unbalanced begin/end text operators." );
    	}
    	this.inText = true;
        this.state.xTLM = 0;
        this.state.yTLM = 0;
        this.content.append("BT").append_i(this.separator);
    }

    /**
     * Ends the writing of text and makes the current font invalid.
     */
    public void endText() {
    	if (!this.inText) {
    		throw new IllegalPdfSyntaxException("Unbalanced begin/end text operators." );
    	}
    	this.inText = false;
        this.content.append("ET").append_i(this.separator);
    }

    /**
     * Saves the graphic state. <CODE>saveState</CODE> and
     * <CODE>restoreState</CODE> must be balanced.
     */
    public void saveState() {
        this.content.append("q").append_i(this.separator);
        this.stateList.add(new GraphicState(this.state));
    }

    /**
     * Restores the graphic state. <CODE>saveState</CODE> and
     * <CODE>restoreState</CODE> must be balanced.
     */
    public void restoreState() {
        this.content.append("Q").append_i(this.separator);
        final int idx = this.stateList.size() - 1;
        if (idx < 0) {
			throw new IllegalPdfSyntaxException("Unbalanced save/restore state operators.");
		}
        this.state = (GraphicState)this.stateList.get(idx);
        this.stateList.remove(idx);
    }

    /**
     * Sets the character spacing parameter.
     *
     * @param       charSpace           a parameter
     */
    public void setCharacterSpacing(final float charSpace) {
        this.state.charSpace = charSpace;
        this.content.append(charSpace).append(" Tc").append_i(this.separator);
    }

    /**
     * Sets the word spacing parameter.
     *
     * @param       wordSpace           a parameter
     */
    public void setWordSpacing(final float wordSpace) {
        this.state.wordSpace = wordSpace;
        this.content.append(wordSpace).append(" Tw").append_i(this.separator);
    }

    /**
     * Sets the horizontal scaling parameter.
     *
     * @param       scale               a parameter
     */
    public void setHorizontalScaling(final float scale) {
        this.state.scale = scale;
        this.content.append(scale).append(" Tz").append_i(this.separator);
    }

    /**
     * Sets the text leading parameter.
     * <P>
     * The leading parameter is measured in text space units. It specifies the vertical distance
     * between the baselines of adjacent lines of text.</P>
     *
     * @param       leading         the new leading
     */
    public void setLeading(final float leading) {
        this.state.leading = leading;
        this.content.append(leading).append(" TL").append_i(this.separator);
    }

    /**
     * Set the font and the size for the subsequent text writing.
     *
     * @param bf the font
     * @param size the font size in points
     */
    public void setFontAndSize(final BaseFont bf, final float size) {
        checkWriter();
        if (size < 0.0001f && size > -0.0001f) {
			throw new IllegalArgumentException("Font size too small: " + size);
		}
        this.state.size = size;
        this.state.fontDetails = this.writer.addSimple(bf);
        final PageResources prs = getPageResources();
        PdfName name = this.state.fontDetails.getFontName();
        name = prs.addFont(name, this.state.fontDetails.getIndirectReference());
        this.content.append(name.getBytes()).append(' ').append(size).append(" Tf").append_i(this.separator);
    }

    /**
     * Sets the text rendering parameter.
     *
     * @param       rendering               a parameter
     */
    public void setTextRenderingMode(final int rendering) {
        this.content.append(rendering).append(" Tr").append_i(this.separator);
    }

    /**
     * Sets the text rise parameter.
     * <P>
     * This allows to write text in subscript or superscript mode.</P>
     *
     * @param       rise                a parameter
     */
    public void setTextRise(final float rise) {
        this.content.append(rise).append(" Ts").append_i(this.separator);
    }

    /**
     * A helper to insert into the content stream the <CODE>text</CODE>
     * converted to bytes according to the font's encoding.
     *
     * @param text the text to write
     */
    private void showText2(final String text) {
        if (this.state.fontDetails == null) {
			throw new NullPointerException("Font and size must be set before writing any text");
		}
        final byte b[] = this.state.fontDetails.convertToBytes(text);
        escapeString(b, this.content);
    }

    /**
     * Shows the <CODE>text</CODE>.
     *
     * @param text the text to write
     */
    public void showText(final String text) {
        showText2(text);
        this.content.append("Tj").append_i(this.separator);
    }

    /**
     * Constructs a kern array for a text in a certain font
     * @param text the text
     * @param font the font
     * @return a PdfTextArray
     */
    private static PdfTextArray getKernArray(final String text, final BaseFont font) {
        final PdfTextArray pa = new PdfTextArray();
        final StringBuffer acc = new StringBuffer();
        final int len = text.length() - 1;
        final char c[] = text.toCharArray();
        if (len >= 0) {
			acc.append(c, 0, 1);
		}
        for (int k = 0; k < len; ++k) {
            final char c2 = c[k + 1];
            final int kern = font.getKerning(c[k], c2);
            if (kern == 0) {
                acc.append(c2);
            }
            else {
                pa.add(acc.toString());
                acc.setLength(0);
                acc.append(c, k + 1, 1);
                pa.add(-kern);
            }
        }
        pa.add(acc.toString());
        return pa;
    }

    /**
     * Shows the <CODE>text</CODE> kerned.
     *
     * @param text the text to write
     */
    private void showTextKerned(final String text) {
        if (this.state.fontDetails == null) {
			throw new NullPointerException("Font and size must be set before writing any text");
		}
        final BaseFont bf = this.state.fontDetails.getBaseFont();
        if (bf.hasKernPairs()) {
			showText(getKernArray(text, bf));
		} else {
			showText(text);
		}
    }





    /**
     * Changes the text matrix.
     * <P>
     * Remark: this operation also initializes the current point position.</P>
     *
     * @param       a           operand 1,1 in the matrix
     * @param       b           operand 1,2 in the matrix
     * @param       c           operand 2,1 in the matrix
     * @param       d           operand 2,2 in the matrix
     * @param       x           operand 3,1 in the matrix
     * @param       y           operand 3,2 in the matrix
     */
    void setTextMatrix(final float a, final float b, final float c, final float d, final float x, final float y) {
        this.state.xTLM = x;
        this.state.yTLM = y;
        this.content.append(a).append(' ').append(b).append_i(' ')
        .append(c).append_i(' ').append(d).append_i(' ')
        .append(x).append_i(' ').append(y).append(" Tm").append_i(this.separator);
    }

    /**
     * Changes the text matrix. The first four parameters are {1,0,0,1}.
     * <P>
     * Remark: this operation also initializes the current point position.</P>
     *
     * @param       x           operand 3,1 in the matrix
     * @param       y           operand 3,2 in the matrix
     */
    public void setTextMatrix(final float x, final float y) {
        setTextMatrix(1, 0, 0, 1, x, y);
    }

    /**
     * Moves to the start of the next line, offset from the start of the current line.
     *
     * @param       x           x-coordinate of the new current point
     * @param       y           y-coordinate of the new current point
     */
    void moveText(final float x, final float y) {
        this.state.xTLM += x;
        this.state.yTLM += y;
        this.content.append(x).append(' ').append(y).append(" Td").append_i(this.separator);
    }





    /**
     * Gets the size of this content.
     *
     * @return the size of the content
     */
    int size() {
        return this.content.size();
    }

    /**
     * Escapes a <CODE>byte</CODE> array according to the PDF conventions.
     *
     * @param b the <CODE>byte</CODE> array to escape
     * @return an escaped <CODE>byte</CODE> array
     */
    static byte[] escapeString(final byte b[]) {
        final ByteBuffer content = new ByteBuffer();
        escapeString(b, content);
        return content.toByteArray();
    }

    /**
     * Escapes a <CODE>byte</CODE> array according to the PDF conventions.
     *
     * @param b the <CODE>byte</CODE> array to escape
     * @param content the content
     */
    private static void escapeString(final byte b[], final ByteBuffer content) {
        content.append_i('(');
        for (final byte c : b) {
            switch (c) {
                case '\r':
                    content.append("\\r");
                    break;
                case '\n':
                    content.append("\\n");
                    break;
                case '\t':
                    content.append("\\t");
                    break;
                case '\b':
                    content.append("\\b");
                    break;
                case '\f':
                    content.append("\\f");
                    break;
                case '(':
                case ')':
                case '\\':
                    content.append_i('\\').append_i(c);
                    break;
                default:
                    content.append_i(c);
            }
        }
        content.append(")");
    }


    /**
     * Gets the root outline.
     *
     * @return the root outline
     */
    public PdfOutline getRootOutline() {
        checkWriter();
        return this.pdf.getRootOutline();
    }

    /**
     * Computes the width of the given string taking in account
     * the current values of "Character spacing", "Word Spacing"
     * and "Horizontal Scaling".
     * The additional spacing is not computed for the last character
     * of the string.
     * @param text the string to get width of
     * @param kerned the kerning option
     * @return the width
     */

    private float getEffectiveStringWidth(final String text, final boolean kerned) {
        final BaseFont bf = this.state.fontDetails.getBaseFont();

        float w;
        if (kerned) {
			w = bf.getWidthPointKerned(text, this.state.size);
		} else {
			w = bf.getWidthPoint(text, this.state.size);
		}

        if (this.state.charSpace != 0.0f && text.length() > 1) {
            w += this.state.charSpace * (text.length() -1);
        }

        final int ft = bf.getFontType();
        if (this.state.wordSpace != 0.0f && (ft == BaseFont.FONT_TYPE_T1 || ft == BaseFont.FONT_TYPE_TT || ft == BaseFont.FONT_TYPE_T3)) {
            for (int i = 0; i < text.length() -1; i++) {
                if (text.charAt(i) == ' ') {
					w += this.state.wordSpace;
				}
            }
        }
        if (this.state.scale != 100.0) {
			w = w * this.state.scale / 100.0f;
		}

        //System.out.println("String width = " + Float.toString(w));
        return w;
    }

    /**
     * Shows text right, left or center aligned with rotation.
     * @param alignment the alignment can be ALIGN_CENTER, ALIGN_RIGHT or ALIGN_LEFT
     * @param text the text to show
     * @param x the x pivot position
     * @param y the y pivot position
     * @param rotation the rotation to be applied in degrees counterclockwise
     */
    void showTextAligned(final int alignment, final String text, final float x, final float y, final float rotation) {
        showTextAligned(alignment, text, x, y, rotation, false);
    }

    private void showTextAligned(final int alignment, final String text, float x, float y, final float rotation, final boolean kerned) {
        if (this.state.fontDetails == null) {
			throw new NullPointerException("Font and size must be set before writing any text");
		}
        if (rotation == 0) {
            switch (alignment) {
                case ALIGN_CENTER:
                    x -= getEffectiveStringWidth(text, kerned) / 2;
                    break;
                case ALIGN_RIGHT:
                    x -= getEffectiveStringWidth(text, kerned);
                    break;
            }
            setTextMatrix(x, y);
            if (kerned) {
				showTextKerned(text);
			} else {
				showText(text);
			}
        }
        else {
            final double alpha = rotation * Math.PI / 180.0;
            final float cos = (float)Math.cos(alpha);
            final float sin = (float)Math.sin(alpha);
            float len;
            switch (alignment) {
                case ALIGN_CENTER:
                    len = getEffectiveStringWidth(text, kerned) / 2;
                    x -=  len * cos;
                    y -=  len * sin;
                    break;
                case ALIGN_RIGHT:
                    len = getEffectiveStringWidth(text, kerned);
                    x -=  len * cos;
                    y -=  len * sin;
                    break;
            }
            setTextMatrix(cos, sin, -sin, cos, x, y);
            if (kerned) {
				showTextKerned(text);
			} else {
				showText(text);
			}
            setTextMatrix(0f, 0f);
        }
    }



    /**
     * Concatenate a matrix to the current transformation matrix.
     * @param a an element of the transformation matrix
     * @param b an element of the transformation matrix
     * @param c an element of the transformation matrix
     * @param d an element of the transformation matrix
     * @param e an element of the transformation matrix
     * @param f an element of the transformation matrix
     **/
    public void concatCTM(final float a, final float b, final float c, final float d, final float e, final float f) {
        this.content.append(a).append(' ').append(b).append(' ').append(c).append(' ');
        this.content.append(d).append(' ').append(e).append(' ').append(f).append(" cm").append_i(this.separator);
    }

    /**
     * Generates an array of bezier curves to draw an arc.
     * <P>
     * (x1, y1) and (x2, y2) are the corners of the enclosing rectangle.
     * Angles, measured in degrees, start with 0 to the right (the positive X
     * axis) and increase counter-clockwise.  The arc extends from startAng
     * to startAng+extent.  I.e. startAng=0 and extent=180 yields an openside-down
     * semi-circle.
     * <P>
     * The resulting coordinates are of the form float[]{x1,y1,x2,y2,x3,y3, x4,y4}
     * such that the curve goes from (x1, y1) to (x4, y4) with (x2, y2) and
     * (x3, y3) as their respective Bezier control points.
     * <P>
     * Note: this code was taken from ReportLab (www.reportlab.org), an excellent
     * PDF generator for Python (BSD license: http://www.reportlab.org/devfaq.html#1.3 ).
     *
     * @param x1 a corner of the enclosing rectangle
     * @param y1 a corner of the enclosing rectangle
     * @param x2 a corner of the enclosing rectangle
     * @param y2 a corner of the enclosing rectangle
     * @param startAng starting angle in degrees
     * @param extent angle extent in degrees
     * @return a list of float[] with the bezier curves
     */
    public static ArrayList bezierArc(float x1, float y1, float x2, float y2, final float startAng, final float extent) {
        float tmp;
        if (x1 > x2) {
            tmp = x1;
            x1 = x2;
            x2 = tmp;
        }
        if (y2 > y1) {
            tmp = y1;
            y1 = y2;
            y2 = tmp;
        }

        float fragAngle;
        int Nfrag;
        if (Math.abs(extent) <= 90f) {
            fragAngle = extent;
            Nfrag = 1;
        }
        else {
            Nfrag = (int)Math.ceil(Math.abs(extent)/90f);
            fragAngle = extent / Nfrag;
        }
        final float x_cen = (x1+x2)/2f;
        final float y_cen = (y1+y2)/2f;
        final float rx = (x2-x1)/2f;
        final float ry = (y2-y1)/2f;
        final float halfAng = (float)(fragAngle * Math.PI / 360.);
        final float kappa = (float)Math.abs(4. / 3. * (1. - Math.cos(halfAng)) / Math.sin(halfAng));
        final ArrayList pointList = new ArrayList();
        for (int i = 0; i < Nfrag; ++i) {
            final float theta0 = (float)((startAng + i*fragAngle) * Math.PI / 180.);
            final float theta1 = (float)((startAng + (i+1)*fragAngle) * Math.PI / 180.);
            final float cos0 = (float)Math.cos(theta0);
            final float cos1 = (float)Math.cos(theta1);
            final float sin0 = (float)Math.sin(theta0);
            final float sin1 = (float)Math.sin(theta1);
            if (fragAngle > 0f) {
                pointList.add(new float[]{x_cen + rx * cos0,
                y_cen - ry * sin0,
                x_cen + rx * (cos0 - kappa * sin0),
                y_cen - ry * (sin0 + kappa * cos0),
                x_cen + rx * (cos1 + kappa * sin1),
                y_cen - ry * (sin1 - kappa * cos1),
                x_cen + rx * cos1,
                y_cen - ry * sin1});
            }
            else {
                pointList.add(new float[]{x_cen + rx * cos0,
                y_cen - ry * sin0,
                x_cen + rx * (cos0 + kappa * sin0),
                y_cen - ry * (sin0 - kappa * cos0),
                x_cen + rx * (cos1 - kappa * sin1),
                y_cen - ry * (sin1 + kappa * cos1),
                x_cen + rx * cos1,
                y_cen - ry * sin1});
            }
        }
        return pointList;
    }

    /**
     * Draws a partial ellipse inscribed within the rectangle x1,y1,x2,y2,
     * starting at startAng degrees and covering extent degrees. Angles
     * start with 0 to the right (+x) and increase counter-clockwise.
     *
     * @param x1 a corner of the enclosing rectangle
     * @param y1 a corner of the enclosing rectangle
     * @param x2 a corner of the enclosing rectangle
     * @param y2 a corner of the enclosing rectangle
     * @param startAng starting angle in degrees
     * @param extent angle extent in degrees
     */
    public void arc(final float x1, final float y1, final float x2, final float y2, final float startAng, final float extent) {
        final ArrayList ar = bezierArc(x1, y1, x2, y2, startAng, extent);
        if (ar.isEmpty()) {
			return;
		}
        float pt[] = (float [])ar.get(0);
        moveTo(pt[0], pt[1]);
        for (int k = 0; k < ar.size(); ++k) {
            pt = (float [])ar.get(k);
            curveTo(pt[2], pt[3], pt[4], pt[5], pt[6], pt[7]);
        }
    }



    /**
     * Create a new colored tiling pattern.
     *
     * @param width the width of the pattern
     * @param height the height of the pattern
     * @param xstep the desired horizontal spacing between pattern cells.
     * May be either positive or negative, but not zero.
     * @param ystep the desired vertical spacing between pattern cells.
     * May be either positive or negative, but not zero.
     * @return the <CODE>PdfPatternPainter</CODE> where the pattern will be created
     */
    private PdfPatternPainter createPattern(final float width, final float height, final float xstep, final float ystep) {
        checkWriter();
        if ( xstep == 0.0f || ystep == 0.0f ) {
			throw new RuntimeException("XStep or YStep can not be ZERO.");
		}
        final PdfPatternPainter painter = new PdfPatternPainter(this.writer);
        painter.setWidth(width);
        painter.setHeight(height);
        painter.setXStep(xstep);
        painter.setYStep(ystep);
        this.writer.addSimplePattern(painter);
        return painter;
    }

    /**
     * Create a new colored tiling pattern. Variables xstep and ystep are set to the same values
     * of width and height.
     * @param width the width of the pattern
     * @param height the height of the pattern
     * @return the <CODE>PdfPatternPainter</CODE> where the pattern will be created
     */
    PdfPatternPainter createPattern(final float width, final float height) {
        return createPattern(width, height, width, height);
    }

    /**
     * Create a new uncolored tiling pattern.
     *
     * @param width the width of the pattern
     * @param height the height of the pattern
     * @param xstep the desired horizontal spacing between pattern cells.
     * May be either positive or negative, but not zero.
     * @param ystep the desired vertical spacing between pattern cells.
     * May be either positive or negative, but not zero.
     * @param color the default color. Can be <CODE>null</CODE>
     * @return the <CODE>PdfPatternPainter</CODE> where the pattern will be created
     */
    private PdfPatternPainter createPattern(final float width, final float height, final float xstep, final float ystep, final Color color) {
        checkWriter();
        if ( xstep == 0.0f || ystep == 0.0f ) {
			throw new RuntimeException("XStep or YStep can not be ZERO.");
		}
        final PdfPatternPainter painter = new PdfPatternPainter(this.writer, color);
        painter.setWidth(width);
        painter.setHeight(height);
        painter.setXStep(xstep);
        painter.setYStep(ystep);
        this.writer.addSimplePattern(painter);
        return painter;
    }





    private PdfTemplate createTemplate(final float width, final float height, final PdfName forcedName) {
        checkWriter();
        final PdfTemplate template = new PdfTemplate(this.writer);
        template.setWidth(width);
        template.setHeight(height);
        this.writer.addDirectTemplateSimple(template, forcedName);
        return template;
    }



    private PdfAppearance createAppearance(final float width, final float height, final PdfName forcedName) {
        checkWriter();
        final PdfAppearance template = new PdfAppearance(this.writer);
        template.setWidth(width);
        template.setHeight(height);
        this.writer.addDirectTemplateSimple(template, forcedName);
        return template;
    }



    /**
     * Adds a template to this content.
     *
     * @param template the template
     * @param a an element of the transformation matrix
     * @param b an element of the transformation matrix
     * @param c an element of the transformation matrix
     * @param d an element of the transformation matrix
     * @param e an element of the transformation matrix
     * @param f an element of the transformation matrix
     */
    public void addTemplate(final PdfTemplate template, final float a, final float b, final float c, final float d, final float e, final float f) {
        checkWriter();
        checkNoPattern(template);
        PdfName name = this.writer.addDirectTemplateSimple(template, null);
        final PageResources prs = getPageResources();
        name = prs.addXObject(name, template.getIndirectReference());
        this.content.append("q ");
        this.content.append(a).append(' ');
        this.content.append(b).append(' ');
        this.content.append(c).append(' ');
        this.content.append(d).append(' ');
        this.content.append(e).append(' ');
        this.content.append(f).append(" cm ");
        this.content.append(name.getBytes()).append(" Do Q").append_i(this.separator);
    }

    void addTemplateReference(final PdfIndirectReference template, PdfName name, final float a, final float b, final float c, final float d, final float e, final float f) {
        checkWriter();
        final PageResources prs = getPageResources();
        name = prs.addXObject(name, template);
        this.content.append("q ");
        this.content.append(a).append(' ');
        this.content.append(b).append(' ');
        this.content.append(c).append(' ');
        this.content.append(d).append(' ');
        this.content.append(e).append(' ');
        this.content.append(f).append(" cm ");
        this.content.append(name.getBytes()).append(" Do Q").append_i(this.separator);
    }

    /**
     * Adds a template to this content.
     *
     * @param template the template
     * @param x the x location of this template
     * @param y the y location of this template
     */
    void addTemplate(final PdfTemplate template, final float x, final float y) {
        addTemplate(template, 1, 0, 0, 1, x, y);
    }

    /**
     * Changes the current color for filling paths (device dependent colors!).
     * <P>
     * Sets the color space to <B>DeviceCMYK</B> (or the <B>DefaultCMYK</B> color space),
     * and sets the color to use for filling paths.</P>
     * <P>
     * This method is described in the 'Portable Document Format Reference Manual version 1.3'
     * section 8.5.2.1 (page 331).</P>
     * <P>
     * Following the PDF manual, each operand must be a number between 0 (no ink) and
     * 1 (maximum ink). This method however accepts only integers between 0x00 and 0xFF.</P>
     *
     * @param cyan the intensity of cyan
     * @param magenta the intensity of magenta
     * @param yellow the intensity of yellow
     * @param black the intensity of black
     */

    public void setCMYKColorFill(final int cyan, final int magenta, final int yellow, final int black) {
        this.content.append((float)(cyan & 0xFF) / 0xFF);
        this.content.append(' ');
        this.content.append((float)(magenta & 0xFF) / 0xFF);
        this.content.append(' ');
        this.content.append((float)(yellow & 0xFF) / 0xFF);
        this.content.append(' ');
        this.content.append((float)(black & 0xFF) / 0xFF);
        this.content.append(" k").append_i(this.separator);
    }
    /**
     * Changes the current color for stroking paths (device dependent colors!).
     * <P>
     * Sets the color space to <B>DeviceCMYK</B> (or the <B>DefaultCMYK</B> color space),
     * and sets the color to use for stroking paths.</P>
     * <P>
     * This method is described in the 'Portable Document Format Reference Manual version 1.3'
     * section 8.5.2.1 (page 331).</P>
     * Following the PDF manual, each operand must be a number between 0 (minimum intensity) and
     * 1 (maximum intensity). This method however accepts only integers between 0x00 and 0xFF.
     *
     * @param cyan the intensity of red
     * @param magenta the intensity of green
     * @param yellow the intensity of blue
     * @param black the intensity of black
     */

    public void setCMYKColorStroke(final int cyan, final int magenta, final int yellow, final int black) {
        this.content.append((float)(cyan & 0xFF) / 0xFF);
        this.content.append(' ');
        this.content.append((float)(magenta & 0xFF) / 0xFF);
        this.content.append(' ');
        this.content.append((float)(yellow & 0xFF) / 0xFF);
        this.content.append(' ');
        this.content.append((float)(black & 0xFF) / 0xFF);
        this.content.append(" K").append_i(this.separator);
    }

    /**
     * Changes the current color for filling paths (device dependent colors!).
     * <P>
     * Sets the color space to <B>DeviceRGB</B> (or the <B>DefaultRGB</B> color space),
     * and sets the color to use for filling paths.</P>
     * <P>
     * This method is described in the 'Portable Document Format Reference Manual version 1.3'
     * section 8.5.2.1 (page 331).</P>
     * <P>
     * Following the PDF manual, each operand must be a number between 0 (minimum intensity) and
     * 1 (maximum intensity). This method however accepts only integers between 0x00 and 0xFF.</P>
     *
     * @param red the intensity of red
     * @param green the intensity of green
     * @param blue the intensity of blue
     */

    public void setRGBColorFill(final int red, final int green, final int blue) {
        HelperRGB((float)(red & 0xFF) / 0xFF, (float)(green & 0xFF) / 0xFF, (float)(blue & 0xFF) / 0xFF);
        this.content.append(" rg").append_i(this.separator);
    }

    /**
     * Changes the current color for stroking paths (device dependent colors!).
     * <P>
     * Sets the color space to <B>DeviceRGB</B> (or the <B>DefaultRGB</B> color space),
     * and sets the color to use for stroking paths.</P>
     * <P>
     * This method is described in the 'Portable Document Format Reference Manual version 1.3'
     * section 8.5.2.1 (page 331).</P>
     * Following the PDF manual, each operand must be a number between 0 (minimum intensity) and
     * 1 (maximum intensity). This method however accepts only integers between 0x00 and 0xFF.
     *
     * @param red the intensity of red
     * @param green the intensity of green
     * @param blue the intensity of blue
     */

    public void setRGBColorStroke(final int red, final int green, final int blue) {
        HelperRGB((float)(red & 0xFF) / 0xFF, (float)(green & 0xFF) / 0xFF, (float)(blue & 0xFF) / 0xFF);
        this.content.append(" RG").append_i(this.separator);
    }

    /** Sets the stroke color. <CODE>color</CODE> can be an
     * <CODE>ExtendedColor</CODE>.
     * @param color the color
     */
    public void setColorStroke(final Color color) {
    	PdfXConformanceImp.checkPDFXConformance(this.writer, PdfXConformanceImp.PDFXKEY_COLOR, color);
        final int type = ExtendedColor.getType(color);
        switch (type) {
            case ExtendedColor.TYPE_GRAY: {
                setGrayStroke(((GrayColor)color).getGray());
                break;
            }
            case ExtendedColor.TYPE_CMYK: {
                final CMYKColor cmyk = (CMYKColor)color;
                setCMYKColorStrokeF(cmyk.getCyan(), cmyk.getMagenta(), cmyk.getYellow(), cmyk.getBlack());
                break;
            }
            case ExtendedColor.TYPE_SEPARATION: {
                final SpotColor spot = (SpotColor)color;
                setColorStroke(spot.getPdfSpotColor(), spot.getTint());
                break;
            }
            case ExtendedColor.TYPE_PATTERN: {
                final PatternColor pat = (PatternColor) color;
                setPatternStroke(pat.getPainter());
                break;
            }
            case ExtendedColor.TYPE_SHADING: {
                final ShadingColor shading = (ShadingColor) color;
                setShadingStroke(shading.getPdfShadingPattern());
                break;
            }
            default:
                setRGBColorStroke(color.getRed(), color.getGreen(), color.getBlue());
        }
    }

    /** Sets the fill color. <CODE>color</CODE> can be an
     * <CODE>ExtendedColor</CODE>.
     * @param color the color
     */
    public void setColorFill(final Color color) {
    	PdfXConformanceImp.checkPDFXConformance(this.writer, PdfXConformanceImp.PDFXKEY_COLOR, color);
        final int type = ExtendedColor.getType(color);
        switch (type) {
            case ExtendedColor.TYPE_GRAY: {
                setGrayFill(((GrayColor)color).getGray());
                break;
            }
            case ExtendedColor.TYPE_CMYK: {
                final CMYKColor cmyk = (CMYKColor)color;
                setCMYKColorFillF(cmyk.getCyan(), cmyk.getMagenta(), cmyk.getYellow(), cmyk.getBlack());
                break;
            }
            case ExtendedColor.TYPE_SEPARATION: {
                final SpotColor spot = (SpotColor)color;
                setColorFill(spot.getPdfSpotColor(), spot.getTint());
                break;
            }
            case ExtendedColor.TYPE_PATTERN: {
                final PatternColor pat = (PatternColor) color;
                setPatternFill(pat.getPainter());
                break;
            }
            case ExtendedColor.TYPE_SHADING: {
                final ShadingColor shading = (ShadingColor) color;
                setShadingFill(shading.getPdfShadingPattern());
                break;
            }
            default:
                setRGBColorFill(color.getRed(), color.getGreen(), color.getBlue());
        }
    }

    /** Sets the fill color to a spot color.
     * @param sp the spot color
     * @param tint the tint for the spot color. 0 is no color and 1
     * is 100% color
     */
    public void setColorFill(final PdfSpotColor sp, final float tint) {
        checkWriter();
        this.state.colorDetails = this.writer.addSimple(sp);
        final PageResources prs = getPageResources();
        PdfName name = this.state.colorDetails.getColorName();
        name = prs.addColor(name, this.state.colorDetails.getIndirectReference());
        this.content.append(name.getBytes()).append(" cs ").append(tint).append(" scn").append_i(this.separator);
    }

    /** Sets the stroke color to a spot color.
     * @param sp the spot color
     * @param tint the tint for the spot color. 0 is no color and 1
     * is 100% color
     */
    public void setColorStroke(final PdfSpotColor sp, final float tint) {
        checkWriter();
        this.state.colorDetails = this.writer.addSimple(sp);
        final PageResources prs = getPageResources();
        PdfName name = this.state.colorDetails.getColorName();
        name = prs.addColor(name, this.state.colorDetails.getIndirectReference());
        this.content.append(name.getBytes()).append(" CS ").append(tint).append(" SCN").append_i(this.separator);
    }

    /** Sets the fill color to a pattern. The pattern can be
     * colored or uncolored.
     * @param p the pattern
     */
    public void setPatternFill(final PdfPatternPainter p) {
        if (p.isStencil()) {
            setPatternFill(p, p.getDefaultColor());
            return;
        }
        checkWriter();
        final PageResources prs = getPageResources();
        PdfName name = this.writer.addSimplePattern(p);
        name = prs.addPattern(name, p.getIndirectReference());
        this.content.append(PdfName.PATTERN.getBytes()).append(" cs ").append(name.getBytes()).append(" scn").append_i(this.separator);
    }

    /** Outputs the color values to the content.
     * @param color The color
     * @param tint the tint if it is a spot color, ignored otherwise
     */
    private void outputColorNumbers(final Color color, final float tint) {
    	PdfXConformanceImp.checkPDFXConformance(this.writer, PdfXConformanceImp.PDFXKEY_COLOR, color);
        final int type = ExtendedColor.getType(color);
        switch (type) {
            case ExtendedColor.TYPE_RGB:
                this.content.append((float)color.getRed() / 0xFF);
                this.content.append(' ');
                this.content.append((float)color.getGreen() / 0xFF);
                this.content.append(' ');
                this.content.append((float)color.getBlue() / 0xFF);
                break;
            case ExtendedColor.TYPE_GRAY:
                this.content.append(((GrayColor)color).getGray());
                break;
            case ExtendedColor.TYPE_CMYK: {
                final CMYKColor cmyk = (CMYKColor)color;
                this.content.append(cmyk.getCyan()).append(' ').append(cmyk.getMagenta());
                this.content.append(' ').append(cmyk.getYellow()).append(' ').append(cmyk.getBlack());
                break;
            }
            case ExtendedColor.TYPE_SEPARATION:
                this.content.append(tint);
                break;
            default:
                throw new RuntimeException("Invalid color type.");
        }
    }

    /** Sets the fill color to an uncolored pattern.
     * @param p the pattern
     * @param color the color of the pattern
     */
    private void setPatternFill(final PdfPatternPainter p, final Color color) {
        if (ExtendedColor.getType(color) == ExtendedColor.TYPE_SEPARATION) {
			setPatternFill(p, color, ((SpotColor)color).getTint());
		} else {
			setPatternFill(p, color, 0);
		}
    }

    /** Sets the fill color to an uncolored pattern.
     * @param p the pattern
     * @param color the color of the pattern
     * @param tint the tint if the color is a spot color, ignored otherwise
     */
    public void setPatternFill(final PdfPatternPainter p, final Color color, final float tint) {
        checkWriter();
        if (!p.isStencil()) {
			throw new RuntimeException("An uncolored pattern was expected.");
		}
        final PageResources prs = getPageResources();
        PdfName name = this.writer.addSimplePattern(p);
        name = prs.addPattern(name, p.getIndirectReference());
        final ColorDetails csDetail = this.writer.addSimplePatternColorspace(color);
        final PdfName cName = prs.addColor(csDetail.getColorName(), csDetail.getIndirectReference());
        this.content.append(cName.getBytes()).append(" cs").append_i(this.separator);
        outputColorNumbers(color, tint);
        this.content.append(' ').append(name.getBytes()).append(" scn").append_i(this.separator);
    }

    /** Sets the stroke color to an uncolored pattern.
     * @param p the pattern
     * @param color the color of the pattern
     */
    private void setPatternStroke(final PdfPatternPainter p, final Color color) {
        if (ExtendedColor.getType(color) == ExtendedColor.TYPE_SEPARATION) {
			setPatternStroke(p, color, ((SpotColor)color).getTint());
		} else {
			setPatternStroke(p, color, 0);
		}
    }

    /** Sets the stroke color to an uncolored pattern.
     * @param p the pattern
     * @param color the color of the pattern
     * @param tint the tint if the color is a spot color, ignored otherwise
     */
    public void setPatternStroke(final PdfPatternPainter p, final Color color, final float tint) {
        checkWriter();
        if (!p.isStencil()) {
			throw new RuntimeException("An uncolored pattern was expected.");
		}
        final PageResources prs = getPageResources();
        PdfName name = this.writer.addSimplePattern(p);
        name = prs.addPattern(name, p.getIndirectReference());
        final ColorDetails csDetail = this.writer.addSimplePatternColorspace(color);
        final PdfName cName = prs.addColor(csDetail.getColorName(), csDetail.getIndirectReference());
        this.content.append(cName.getBytes()).append(" CS").append_i(this.separator);
        outputColorNumbers(color, tint);
        this.content.append(' ').append(name.getBytes()).append(" SCN").append_i(this.separator);
    }

    /** Sets the stroke color to a pattern. The pattern can be
     * colored or uncolored.
     * @param p the pattern
     */
    public void setPatternStroke(final PdfPatternPainter p) {
        if (p.isStencil()) {
            setPatternStroke(p, p.getDefaultColor());
            return;
        }
        checkWriter();
        final PageResources prs = getPageResources();
        PdfName name = this.writer.addSimplePattern(p);
        name = prs.addPattern(name, p.getIndirectReference());
        this.content.append(PdfName.PATTERN.getBytes()).append(" CS ").append(name.getBytes()).append(" SCN").append_i(this.separator);
    }

    /**
     * Paints using a shading object.
     * @param shading the shading object
     */
    private void paintShading(final PdfShading shading) {
        this.writer.addSimpleShading(shading);
        final PageResources prs = getPageResources();
        final PdfName name = prs.addShading(shading.getShadingName(), shading.getShadingReference());
        this.content.append(name.getBytes()).append(" sh").append_i(this.separator);
        final ColorDetails details = shading.getColorDetails();
        if (details != null) {
			prs.addColor(details.getColorName(), details.getIndirectReference());
		}
    }



    /**
     * Sets the shading fill pattern.
     * @param shading the shading pattern
     */
    public void setShadingFill(final PdfShadingPattern shading) {
        this.writer.addSimpleShadingPattern(shading);
        final PageResources prs = getPageResources();
        final PdfName name = prs.addPattern(shading.getPatternName(), shading.getPatternReference());
        this.content.append(PdfName.PATTERN.getBytes()).append(" cs ").append(name.getBytes()).append(" scn").append_i(this.separator);
        final ColorDetails details = shading.getColorDetails();
        if (details != null) {
			prs.addColor(details.getColorName(), details.getIndirectReference());
		}
    }

    /**
     * Sets the shading stroke pattern
     * @param shading the shading pattern
     */
    public void setShadingStroke(final PdfShadingPattern shading) {
        this.writer.addSimpleShadingPattern(shading);
        final PageResources prs = getPageResources();
        final PdfName name = prs.addPattern(shading.getPatternName(), shading.getPatternReference());
        this.content.append(PdfName.PATTERN.getBytes()).append(" CS ").append(name.getBytes()).append(" SCN").append_i(this.separator);
        final ColorDetails details = shading.getColorDetails();
        if (details != null) {
			prs.addColor(details.getColorName(), details.getIndirectReference());
		}
    }

    /** Check if we have a valid PdfWriter.
     *
     */
    protected void checkWriter() {
        if (this.writer == null) {
			throw new NullPointerException("The writer in PdfContentByte is null.");
		}
    }

    /**
     * Show an array of text.
     * @param text array of text
     */
    void showText(final PdfTextArray text) {
        if (this.state.fontDetails == null) {
			throw new NullPointerException("Font and size must be set before writing any text");
		}
        this.content.append("[");
        final ArrayList arrayList = text.getArrayList();
        boolean lastWasNumber = false;
        for (int k = 0; k < arrayList.size(); ++k) {
            final Object obj = arrayList.get(k);
            if (obj instanceof String) {
                showText2((String)obj);
                lastWasNumber = false;
            }
            else {
                if (lastWasNumber) {
					this.content.append(' ');
				} else {
					lastWasNumber = true;
				}
                this.content.append(((Float)obj).floatValue());
            }
        }
        this.content.append("]TJ").append_i(this.separator);
    }

    /**
     * Gets the <CODE>PdfWriter</CODE> in use by this object.
     * @return the <CODE>PdfWriter</CODE> in use by this object
     */
    public PdfWriter getPdfWriter() {
        return this.writer;
    }

    /**
     * Gets the <CODE>PdfDocument</CODE> in use by this object.
     * @return the <CODE>PdfDocument</CODE> in use by this object
     */
    public PdfDocument getPdfDocument() {
        return this.pdf;
    }





    /**
     * Gets a duplicate of this <CODE>PdfContentByte</CODE>. All
     * the members are copied by reference but the buffer stays different.
     *
     * @return a copy of this <CODE>PdfContentByte</CODE>
     */
    public PdfContentByte getDuplicate() {
        return new PdfContentByte(this.writer);
    }




    /**
     * Adds a round rectangle to the current path.
     *
     * @param x x-coordinate of the starting point
     * @param y y-coordinate of the starting point
     * @param w width
     * @param h height
     * @param r radius of the arc corner
     */
    public void roundRectangle(float x, float y, float w, float h, float r) {
        if (w < 0) {
            x += w;
            w = -w;
        }
        if (h < 0) {
            y += h;
            h = -h;
        }
        if (r < 0) {
			r = -r;
		}
        final float b = 0.4477f;
        moveTo(x + r, y);
        lineTo(x + w - r, y);
        curveTo(x + w - r * b, y, x + w, y + r * b, x + w, y + r);
        lineTo(x + w, y + h - r);
        curveTo(x + w, y + h - r * b, x + w - r * b, y + h, x + w - r, y + h);
        lineTo(x + r, y + h);
        curveTo(x + r * b, y + h, x, y + h - r * b, x, y + h - r);
        lineTo(x, y + r);
        curveTo(x, y + r * b, x + r * b, y, x + r, y);
    }

    /** Implements an action in an area.
     * @param action the <CODE>PdfAction</CODE>
     * @param llx the lower left x corner of the activation area
     * @param lly the lower left y corner of the activation area
     * @param urx the upper right x corner of the activation area
     * @param ury the upper right y corner of the activation area
     */
    public void setAction(final PdfAction action, final float llx, final float lly, final float urx, final float ury) {
        this.pdf.setAction(action, llx, lly, urx, ury);
    }

    /** Outputs a <CODE>String</CODE> directly to the content.
     * @param s the <CODE>String</CODE>
     */
    public void setLiteral(final String s) {
        this.content.append(s);
    }

    /** Outputs a <CODE>char</CODE> directly to the content.
     * @param c the <CODE>char</CODE>
     */
    public void setLiteral(final char c) {
        this.content.append(c);
    }

    /** Outputs a <CODE>float</CODE> directly to the content.
     * @param n the <CODE>float</CODE>
     */
    public void setLiteral(final float n) {
        this.content.append(n);
    }

    /** Throws an error if it is a pattern.
     * @param t the object to check
     */
    private void checkNoPattern(final PdfTemplate t) {
        if (t.getType() == PdfTemplate.TYPE_PATTERN) {
			throw new RuntimeException("Invalid use of a pattern. A template was expected.");
		}
    }

    /**
     * Draws a TextField.
     * @param llx
     * @param lly
     * @param urx
     * @param ury
     * @param on
     */
    void drawRadioField(float llx, float lly, float urx, float ury, final boolean on) {
        if (llx > urx) { final float x = llx; llx = urx; urx = x; }
        if (lly > ury) { final float y = lly; lly = ury; ury = y; }
        // silver circle
        setLineWidth(1);
        setLineCap(1);
        setColorStroke(new Color(0xC0, 0xC0, 0xC0));
        arc(llx + 1f, lly + 1f, urx - 1f, ury - 1f, 0f, 360f);
        stroke();
        // gray circle-segment
        setLineWidth(1);
        setLineCap(1);
        setColorStroke(new Color(0xA0, 0xA0, 0xA0));
        arc(llx + 0.5f, lly + 0.5f, urx - 0.5f, ury - 0.5f, 45, 180);
        stroke();
        // black circle-segment
        setLineWidth(1);
        setLineCap(1);
        setColorStroke(new Color(0x00, 0x00, 0x00));
        arc(llx + 1.5f, lly + 1.5f, urx - 1.5f, ury - 1.5f, 45, 180);
        stroke();
        if (on) {
            // gray circle
            setLineWidth(1);
            setLineCap(1);
            setColorFill(new Color(0x00, 0x00, 0x00));
            arc(llx + 4f, lly + 4f, urx - 4f, ury - 4f, 0, 360);
            fill();
        }
    }

    /**
     * Draws a TextField.
     * @param llx
     * @param lly
     * @param urx
     * @param ury
     */
    void drawTextField(float llx, float lly, float urx, float ury) {
        if (llx > urx) { final float x = llx; llx = urx; urx = x; }
        if (lly > ury) { final float y = lly; lly = ury; ury = y; }
        // silver rectangle not filled
        setColorStroke(new Color(0xC0, 0xC0, 0xC0));
        setLineWidth(1);
        setLineCap(0);
        rectangle(llx, lly, urx - llx, ury - lly);
        stroke();
        // white rectangle filled
        setLineWidth(1);
        setLineCap(0);
        setColorFill(new Color(0xFF, 0xFF, 0xFF));
        rectangle(llx + 0.5f, lly + 0.5f, urx - llx - 1f, ury -lly - 1f);
        fill();
        // silver lines
        setColorStroke(new Color(0xC0, 0xC0, 0xC0));
        setLineWidth(1);
        setLineCap(0);
        moveTo(llx + 1f, lly + 1.5f);
        lineTo(urx - 1.5f, lly + 1.5f);
        lineTo(urx - 1.5f, ury - 1f);
        stroke();
        // gray lines
        setColorStroke(new Color(0xA0, 0xA0, 0xA0));
        setLineWidth(1);
        setLineCap(0);
        moveTo(llx + 1f, lly + 1);
        lineTo(llx + 1f, ury - 1f);
        lineTo(urx - 1f, ury - 1f);
        stroke();
        // black lines
        setColorStroke(new Color(0x00, 0x00, 0x00));
        setLineWidth(1);
        setLineCap(0);
        moveTo(llx + 2f, lly + 2f);
        lineTo(llx + 2f, ury - 2f);
        lineTo(urx - 2f, ury - 2f);
        stroke();
    }

    /**
     * Draws a button.
     * @param llx
     * @param lly
     * @param urx
     * @param ury
     * @param text
     * @param bf
     * @param size
     */
    void drawButton(float llx, float lly, float urx, float ury, final String text, final BaseFont bf, final float size) {
        if (llx > urx) { final float x = llx; llx = urx; urx = x; }
        if (lly > ury) { final float y = lly; lly = ury; ury = y; }
        // black rectangle not filled
        setColorStroke(new Color(0x00, 0x00, 0x00));
        setLineWidth(1);
        setLineCap(0);
        rectangle(llx, lly, urx - llx, ury - lly);
        stroke();
        // silver rectangle filled
        setLineWidth(1);
        setLineCap(0);
        setColorFill(new Color(0xC0, 0xC0, 0xC0));
        rectangle(llx + 0.5f, lly + 0.5f, urx - llx - 1f, ury -lly - 1f);
        fill();
        // white lines
        setColorStroke(new Color(0xFF, 0xFF, 0xFF));
        setLineWidth(1);
        setLineCap(0);
        moveTo(llx + 1f, lly + 1f);
        lineTo(llx + 1f, ury - 1f);
        lineTo(urx - 1f, ury - 1f);
        stroke();
        // dark grey lines
        setColorStroke(new Color(0xA0, 0xA0, 0xA0));
        setLineWidth(1);
        setLineCap(0);
        moveTo(llx + 1f, lly + 1f);
        lineTo(urx - 1f, lly + 1f);
        lineTo(urx - 1f, ury - 1f);
        stroke();
        // text
        resetRGBColorFill();
        beginText();
        setFontAndSize(bf, size);
        showTextAligned(PdfContentByte.ALIGN_CENTER, text, llx + (urx - llx) / 2, lly + (ury - lly - size) / 2, 0);
        endText();
    }

    PageResources getPageResources() {
        return this.pdf.getPageResources();
    }

    /** Sets the graphic state
     * @param gstate the graphic state
     */
    public void setGState(final PdfGState gstate) {
        final PdfObject obj[] = this.writer.addSimpleExtGState(gstate);
        final PageResources prs = getPageResources();
        final PdfName name = prs.addExtGState((PdfName)obj[0], (PdfIndirectReference)obj[1]);
        this.content.append(name.getBytes()).append(" gs").append_i(this.separator);
    }

    /**
     * Begins a graphic block whose visibility is controlled by the <CODE>layer</CODE>.
     * Blocks can be nested. Each block must be terminated by an {@link #endLayer()}.<p>
     * Note that nested layers with {@link PdfLayer#addChild(PdfLayer)} only require a single
     * call to this method and a single call to {@link #endLayer()}; all the nesting control
     * is built in.
     * @param layer the layer
     */
    private void beginLayer(final PdfOCG layer) {
        if (layer instanceof PdfLayer && ((PdfLayer)layer).getTitle() != null) {
			throw new IllegalArgumentException("A title is not a layer");
		}
        if (this.layerDepth == null) {
			this.layerDepth = new ArrayList();
		}
        if (layer instanceof PdfLayerMembership) {
            this.layerDepth.add(new Integer(1));
            beginLayer2(layer);
            return;
        }
        int n = 0;
        PdfLayer la = (PdfLayer)layer;
        while (la != null) {
            if (la.getTitle() == null) {
                beginLayer2(la);
                ++n;
            }
            la = la.getParent();
        }
        this.layerDepth.add(new Integer(n));
    }

    private void beginLayer2(final PdfOCG layer) {
        PdfName name = (PdfName)this.writer.addSimpleProperty(layer, layer.getRef())[0];
        final PageResources prs = getPageResources();
        name = prs.addProperty(name, layer.getRef());
        this.content.append("/OC ").append(name.getBytes()).append(" BDC").append_i(this.separator);
    }

    /**
     * Ends a layer controlled graphic block. It will end the most recent open block.
     */
    private void endLayer() {
        int n = 1;
        if (this.layerDepth != null && !this.layerDepth.isEmpty()) {
            n = ((Integer)this.layerDepth.get(this.layerDepth.size() - 1)).intValue();
            this.layerDepth.remove(this.layerDepth.size() - 1);
        } else {
        	throw new IllegalPdfSyntaxException("Unbalanced layer operators." );
        }
        while (n-- > 0) {
			this.content.append("EMC").append_i(this.separator);
		}
    }



    void addAnnotation(final PdfAnnotation annot) {
        this.writer.addAnnotation(annot);
    }


    /**
     * Begins a marked content sequence. If property is <CODE>null</CODE> the mark will be of the type
     * <CODE>BMC</CODE> otherwise it will be <CODE>BDC</CODE>.
     * @param tag the tag
     * @param property the property
     * @param inline <CODE>true</CODE> to include the property in the content or <CODE>false</CODE>
     * to include the property in the resource dictionary with the possibility of reusing
     */
    private void beginMarkedContentSequence(final PdfName tag, final PdfDictionary property, final boolean inline) {
        if (property == null) {
            this.content.append(tag.getBytes()).append(" BMC").append_i(this.separator);
            return;
        }
        this.content.append(tag.getBytes()).append(' ');
        if (inline) {
			try {
                property.toPdf(this.writer, this.content);
            }
            catch (final Exception e) {
                throw new ExceptionConverter(e);
            }
		} else {
            PdfObject[] objs;
            if (this.writer.propertyExists(property)) {
				objs = this.writer.addSimpleProperty(property, null);
			} else {
				objs = this.writer.addSimpleProperty(property, this.writer.getPdfIndirectReference());
			}
            PdfName name = (PdfName)objs[0];
            final PageResources prs = getPageResources();
            name = prs.addProperty(name, (PdfIndirectReference)objs[1]);
            this.content.append(name.getBytes());
        }
        this.content.append(" BDC").append_i(this.separator);
        ++this.mcDepth;
    }



    /**
     * Checks for any dangling state: Mismatched save/restore state, begin/end text,
     * begin/end layer, or begin/end marked content sequence.
     * If found, this function will throw.  This function is called automatically
     * during a reset() (from Document.newPage() for example), and before writing
     * itself out in toPdf().
     * One possible cause: not calling myPdfGraphics2D.dispose() will leave dangling
     *                     saveState() calls.
     * @since 2.1.6
     * @throws IllegalPdfSyntaxException (a runtime exception)
     */
    private void sanityCheck() {
    	if (this.mcDepth != 0) {
    		throw new IllegalPdfSyntaxException("Unbalanced marked content operators." );
    	}
    	if (this.inText) {
    		throw new IllegalPdfSyntaxException("Unbalanced begin/end text operators." );
    	}
    	if (this.layerDepth != null && !this.layerDepth.isEmpty()) {
    		throw new IllegalPdfSyntaxException("Unbalanced layer operators." );
    	}
    	if (!this.stateList.isEmpty()) {
    		throw new IllegalPdfSyntaxException("Unbalanced save/restore state operators." );
    	}
    }
}
