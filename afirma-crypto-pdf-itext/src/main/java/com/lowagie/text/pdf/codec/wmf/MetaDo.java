/*
 * $Id: MetaDo.java 3373 2008-05-12 16:21:24Z xlv $
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
import java.awt.Color;
import java.awt.Point;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Image;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.codec.BmpImage;

public class MetaDo {

    private static final int META_SETBKCOLOR            = 0x0201;
    private static final int META_SETBKMODE             = 0x0102;



    private static final int META_SETPOLYFILLMODE       = 0x0106;


    private static final int META_SETTEXTCOLOR          = 0x0209;

    private static final int META_SETWINDOWORG          = 0x020B;
    private static final int META_SETWINDOWEXT          = 0x020C;






    private static final int META_LINETO                = 0x0213;
    private static final int META_MOVETO                = 0x0214;

    private static final int META_INTERSECTCLIPRECT     = 0x0416;
    private static final int META_ARC                   = 0x0817;
    private static final int META_ELLIPSE               = 0x0418;

    private static final int META_PIE                   = 0x081A;
    private static final int META_RECTANGLE             = 0x041B;
    private static final int META_ROUNDRECT             = 0x061C;

    private static final int META_SAVEDC                = 0x001E;
    private static final int META_SETPIXEL              = 0x041F;

    private static final int META_TEXTOUT               = 0x0521;


    private static final int META_POLYGON               = 0x0324;
    private static final int META_POLYLINE              = 0x0325;

    private static final int META_RESTOREDC             = 0x0127;

    private static final int META_SELECTOBJECT          = 0x012D;
    private static final int META_SETTEXTALIGN          = 0x012E;
    private static final int META_CHORD                 = 0x0830;

    private static final int META_EXTTEXTOUT            = 0x0a32;

    private static final int META_POLYPOLYGON           = 0x0538;

    private static final int META_DIBSTRETCHBLT         = 0x0b41;
    private static final int META_DIBCREATEPATTERNBRUSH = 0x0142;
    private static final int META_STRETCHDIB            = 0x0f43;

    private static final int META_DELETEOBJECT          = 0x01f0;
    private static final int META_CREATEPALETTE         = 0x00f7;

    private static final int META_CREATEPENINDIRECT     = 0x02FA;
    private static final int META_CREATEFONTINDIRECT    = 0x02FB;
    private static final int META_CREATEBRUSHINDIRECT   = 0x02FC;
    private static final int META_CREATEREGION          = 0x06FF;

    private final PdfContentByte cb;
    private final InputMeta in;
    private int left;
    private int top;
    private int right;
    private int bottom;
    private int inch;
    private final MetaState state = new MetaState();

    public MetaDo(final InputStream in, final PdfContentByte cb) {
        this.cb = cb;
        this.in = new InputMeta(in);
    }

    public void readAll() throws IOException, DocumentException{
        if (this.in.readInt() != 0x9AC6CDD7) {
            throw new DocumentException("Not a placeable windows metafile");
        }
        this.in.readWord();
        this.left = this.in.readShort();
        this.top = this.in.readShort();
        this.right = this.in.readShort();
        this.bottom = this.in.readShort();
        this.inch = this.in.readWord();
        this.state.setScalingX((float)(this.right - this.left) / (float)this.inch * 72f);
        this.state.setScalingY((float)(this.bottom - this.top) / (float)this.inch * 72f);
        this.state.setOffsetWx(this.left);
        this.state.setOffsetWy(this.top);
        this.state.setExtentWx(this.right - this.left);
        this.state.setExtentWy(this.bottom - this.top);
        this.in.readInt();
        this.in.readWord();
        this.in.skip(18);

        int tsize;
        int function;
        this.cb.setLineCap(1);
        this.cb.setLineJoin(1);
        for (;;) {
            final int lenMarker = this.in.getLength();
            tsize = this.in.readInt();
            if (tsize < 3) {
				break;
			}
            function = this.in.readWord();
            switch (function) {
                case 0:
                    break;
                case META_CREATEPALETTE:
                case META_CREATEREGION:
                case META_DIBCREATEPATTERNBRUSH:
                    this.state.addMetaObject(new MetaObject());
                    break;
                case META_CREATEPENINDIRECT:
                {
                    final MetaPen pen = new MetaPen();
                    pen.init(this.in);
                    this.state.addMetaObject(pen);
                    break;
                }
                case META_CREATEBRUSHINDIRECT:
                {
                    final MetaBrush brush = new MetaBrush();
                    brush.init(this.in);
                    this.state.addMetaObject(brush);
                    break;
                }
                case META_CREATEFONTINDIRECT:
                {
                    final MetaFont font = new MetaFont();
                    font.init(this.in);
                    this.state.addMetaObject(font);
                    break;
                }
                case META_SELECTOBJECT:
                {
                    final int idx = this.in.readWord();
                    this.state.selectMetaObject(idx, this.cb);
                    break;
                }
                case META_DELETEOBJECT:
                {
                    final int idx = this.in.readWord();
                    this.state.deleteMetaObject(idx);
                    break;
                }
                case META_SAVEDC:
                    this.state.saveState(this.cb);
                    break;
                case META_RESTOREDC:
                {
                    final int idx = this.in.readShort();
                    this.state.restoreState(idx, this.cb);
                    break;
                }
                case META_SETWINDOWORG:
                    this.state.setOffsetWy(this.in.readShort());
                    this.state.setOffsetWx(this.in.readShort());
                    break;
                case META_SETWINDOWEXT:
                    this.state.setExtentWy(this.in.readShort());
                    this.state.setExtentWx(this.in.readShort());
                    break;
                case META_MOVETO:
                {
                    final int y = this.in.readShort();
                    final Point p = new Point(this.in.readShort(), y);
                    this.state.setCurrentPoint(p);
                    break;
                }
                case META_LINETO:
                {
                    final int y = this.in.readShort();
                    final int x = this.in.readShort();
                    final Point p = this.state.getCurrentPoint();
                    this.cb.moveTo(this.state.transformX(p.x), this.state.transformY(p.y));
                    this.cb.lineTo(this.state.transformX(x), this.state.transformY(y));
                    this.cb.stroke();
                    this.state.setCurrentPoint(new Point(x, y));
                    break;
                }
                case META_POLYLINE:
                {
                    this.state.setLineJoinPolygon(this.cb);
                    final int len = this.in.readWord();
                    int x = this.in.readShort();
                    int y = this.in.readShort();
                    this.cb.moveTo(this.state.transformX(x), this.state.transformY(y));
                    for (int k = 1; k < len; ++k) {
                        x = this.in.readShort();
                        y = this.in.readShort();
                        this.cb.lineTo(this.state.transformX(x), this.state.transformY(y));
                    }
                    this.cb.stroke();
                    break;
                }
                case META_POLYGON:
                {
                    if (isNullStrokeFill(false)) {
						break;
					}
                    final int len = this.in.readWord();
                    final int sx = this.in.readShort();
                    final int sy = this.in.readShort();
                    this.cb.moveTo(this.state.transformX(sx), this.state.transformY(sy));
                    for (int k = 1; k < len; ++k) {
                        final int x = this.in.readShort();
                        final int y = this.in.readShort();
                        this.cb.lineTo(this.state.transformX(x), this.state.transformY(y));
                    }
                    this.cb.lineTo(this.state.transformX(sx), this.state.transformY(sy));
                    strokeAndFill();
                    break;
                }
                case META_POLYPOLYGON:
                {
                    if (isNullStrokeFill(false)) {
						break;
					}
                    final int numPoly = this.in.readWord();
                    final int lens[] = new int[numPoly];
                    for (int k = 0; k < lens.length; ++k) {
						lens[k] = this.in.readWord();
					}
                    for (final int len : lens) {
                        final int sx = this.in.readShort();
                        final int sy = this.in.readShort();
                        this.cb.moveTo(this.state.transformX(sx), this.state.transformY(sy));
                        for (int k = 1; k < len; ++k) {
                            final int x = this.in.readShort();
                            final int y = this.in.readShort();
                            this.cb.lineTo(this.state.transformX(x), this.state.transformY(y));
                        }
                        this.cb.lineTo(this.state.transformX(sx), this.state.transformY(sy));
                    }
                    strokeAndFill();
                    break;
                }
                case META_ELLIPSE:
                {
                    if (isNullStrokeFill(this.state.getLineNeutral())) {
						break;
					}
                    final int b = this.in.readShort();
                    final int r = this.in.readShort();
                    final int t = this.in.readShort();
                    final int l = this.in.readShort();
                    this.cb.arc(this.state.transformX(l), this.state.transformY(b), this.state.transformX(r), this.state.transformY(t), 0, 360);
                    strokeAndFill();
                    break;
                }
                case META_ARC:
                {
                    if (isNullStrokeFill(this.state.getLineNeutral())) {
						break;
					}
                    final float yend = this.state.transformY(this.in.readShort());
                    final float xend = this.state.transformX(this.in.readShort());
                    final float ystart = this.state.transformY(this.in.readShort());
                    final float xstart = this.state.transformX(this.in.readShort());
                    final float b = this.state.transformY(this.in.readShort());
                    final float r = this.state.transformX(this.in.readShort());
                    final float t = this.state.transformY(this.in.readShort());
                    final float l = this.state.transformX(this.in.readShort());
                    final float cx = (r + l) / 2;
                    final float cy = (t + b) / 2;
                    final float arc1 = getArc(cx, cy, xstart, ystart);
                    float arc2 = getArc(cx, cy, xend, yend);
                    arc2 -= arc1;
                    if (arc2 <= 0) {
						arc2 += 360;
					}
                    this.cb.arc(l, b, r, t, arc1, arc2);
                    this.cb.stroke();
                    break;
                }
                case META_PIE:
                {
                    if (isNullStrokeFill(this.state.getLineNeutral())) {
						break;
					}
                    final float yend = this.state.transformY(this.in.readShort());
                    final float xend = this.state.transformX(this.in.readShort());
                    final float ystart = this.state.transformY(this.in.readShort());
                    final float xstart = this.state.transformX(this.in.readShort());
                    final float b = this.state.transformY(this.in.readShort());
                    final float r = this.state.transformX(this.in.readShort());
                    final float t = this.state.transformY(this.in.readShort());
                    final float l = this.state.transformX(this.in.readShort());
                    final float cx = (r + l) / 2;
                    final float cy = (t + b) / 2;
                    final float arc1 = getArc(cx, cy, xstart, ystart);
                    float arc2 = getArc(cx, cy, xend, yend);
                    arc2 -= arc1;
                    if (arc2 <= 0) {
						arc2 += 360;
					}
                    final ArrayList ar = PdfContentByte.bezierArc(l, b, r, t, arc1, arc2);
                    if (ar.isEmpty()) {
						break;
					}
                    float pt[] = (float [])ar.get(0);
                    this.cb.moveTo(cx, cy);
                    this.cb.lineTo(pt[0], pt[1]);
                    for (int k = 0; k < ar.size(); ++k) {
                        pt = (float [])ar.get(k);
                        this.cb.curveTo(pt[2], pt[3], pt[4], pt[5], pt[6], pt[7]);
                    }
                    this.cb.lineTo(cx, cy);
                    strokeAndFill();
                    break;
                }
                case META_CHORD:
                {
                    if (isNullStrokeFill(this.state.getLineNeutral())) {
						break;
					}
                    final float yend = this.state.transformY(this.in.readShort());
                    final float xend = this.state.transformX(this.in.readShort());
                    final float ystart = this.state.transformY(this.in.readShort());
                    final float xstart = this.state.transformX(this.in.readShort());
                    final float b = this.state.transformY(this.in.readShort());
                    final float r = this.state.transformX(this.in.readShort());
                    final float t = this.state.transformY(this.in.readShort());
                    final float l = this.state.transformX(this.in.readShort());
                    float cx = (r + l) / 2;
                    float cy = (t + b) / 2;
                    final float arc1 = getArc(cx, cy, xstart, ystart);
                    float arc2 = getArc(cx, cy, xend, yend);
                    arc2 -= arc1;
                    if (arc2 <= 0) {
						arc2 += 360;
					}
                    final ArrayList ar = PdfContentByte.bezierArc(l, b, r, t, arc1, arc2);
                    if (ar.isEmpty()) {
						break;
					}
                    float pt[] = (float [])ar.get(0);
                    cx = pt[0];
                    cy = pt[1];
                    this.cb.moveTo(cx, cy);
                    for (int k = 0; k < ar.size(); ++k) {
                        pt = (float [])ar.get(k);
                        this.cb.curveTo(pt[2], pt[3], pt[4], pt[5], pt[6], pt[7]);
                    }
                    this.cb.lineTo(cx, cy);
                    strokeAndFill();
                    break;
                }
                case META_RECTANGLE:
                {
                    if (isNullStrokeFill(true)) {
						break;
					}
                    final float b = this.state.transformY(this.in.readShort());
                    final float r = this.state.transformX(this.in.readShort());
                    final float t = this.state.transformY(this.in.readShort());
                    final float l = this.state.transformX(this.in.readShort());
                    this.cb.rectangle(l, b, r - l, t - b);
                    strokeAndFill();
                    break;
                }
                case META_ROUNDRECT:
                {
                    if (isNullStrokeFill(true)) {
						break;
					}
                    final float h = this.state.transformY(0) - this.state.transformY(this.in.readShort());
                    final float w = this.state.transformX(this.in.readShort()) - this.state.transformX(0);
                    final float b = this.state.transformY(this.in.readShort());
                    final float r = this.state.transformX(this.in.readShort());
                    final float t = this.state.transformY(this.in.readShort());
                    final float l = this.state.transformX(this.in.readShort());
                    this.cb.roundRectangle(l, b, r - l, t - b, (h + w) / 4);
                    strokeAndFill();
                    break;
                }
                case META_INTERSECTCLIPRECT:
                {
                    final float b = this.state.transformY(this.in.readShort());
                    final float r = this.state.transformX(this.in.readShort());
                    final float t = this.state.transformY(this.in.readShort());
                    final float l = this.state.transformX(this.in.readShort());
                    this.cb.rectangle(l, b, r - l, t - b);
                    this.cb.eoClip();
                    this.cb.newPath();
                    break;
                }
                case META_EXTTEXTOUT:
                {
                    final int y = this.in.readShort();
                    final int x = this.in.readShort();
                    final int count = this.in.readWord();
                    final int flag = this.in.readWord();
                    int x1 = 0;
                    int y1 = 0;
                    int x2 = 0;
                    int y2 = 0;
                    if ((flag & (MetaFont.ETO_CLIPPED | MetaFont.ETO_OPAQUE)) != 0) {
                        x1 = this.in.readShort();
                        y1 = this.in.readShort();
                        x2 = this.in.readShort();
                        y2 = this.in.readShort();
                    }
                    final byte text[] = new byte[count];
                    int k;
                    for (k = 0; k < count; ++k) {
                        final byte c = (byte)this.in.readByte();
                        if (c == 0) {
							break;
						}
                        text[k] = c;
                    }
                    String s;
                    try {
                        s = new String(text, 0, k, "Cp1252");
                    }
                    catch (final UnsupportedEncodingException e) {
                        s = new String(text, 0, k);
                    }
                    outputText(x, y, flag, x1, y1, x2, y2, s);
                    break;
                }
                case META_TEXTOUT:
                {
                    int count = this.in.readWord();
                    final byte text[] = new byte[count];
                    int k;
                    for (k = 0; k < count; ++k) {
                        final byte c = (byte)this.in.readByte();
                        if (c == 0) {
							break;
						}
                        text[k] = c;
                    }
                    String s;
                    try {
                        s = new String(text, 0, k, "Cp1252");
                    }
                    catch (final UnsupportedEncodingException e) {
                        s = new String(text, 0, k);
                    }
                    count = count + 1 & 0xfffe;
                    this.in.skip(count - k);
                    final int y = this.in.readShort();
                    final int x = this.in.readShort();
                    outputText(x, y, 0, 0, 0, 0, 0, s);
                    break;
                }
                case META_SETBKCOLOR:
                    this.state.setCurrentBackgroundColor(this.in.readColor());
                    break;
                case META_SETTEXTCOLOR:
                    this.state.setCurrentTextColor(this.in.readColor());
                    break;
                case META_SETTEXTALIGN:
                    this.state.setTextAlign(this.in.readWord());
                    break;
                case META_SETBKMODE:
                    this.state.setBackgroundMode(this.in.readWord());
                    break;
                case META_SETPOLYFILLMODE:
                    this.state.setPolyFillMode(this.in.readWord());
                    break;
                case META_SETPIXEL:
                {
                    final Color color = this.in.readColor();
                    final int y = this.in.readShort();
                    final int x = this.in.readShort();
                    this.cb.saveState();
                    this.cb.setColorFill(color);
                    this.cb.rectangle(this.state.transformX(x), this.state.transformY(y), .2f, .2f);
                    this.cb.fill();
                    this.cb.restoreState();
                    break;
                }
                case META_DIBSTRETCHBLT:
                case META_STRETCHDIB: {
                    final int rop = this.in.readInt();
                    if (function == META_STRETCHDIB) {
                        /*int usage = */ this.in.readWord();
                    }
                    final int srcHeight = this.in.readShort();
                    final int srcWidth = this.in.readShort();
                    final int ySrc = this.in.readShort();
                    final int xSrc = this.in.readShort();
                    final float destHeight = this.state.transformY(this.in.readShort()) - this.state.transformY(0);
                    final float destWidth = this.state.transformX(this.in.readShort()) - this.state.transformX(0);
                    final float yDest = this.state.transformY(this.in.readShort());
                    final float xDest = this.state.transformX(this.in.readShort());
                    final byte b[] = new byte[tsize * 2 - (this.in.getLength() - lenMarker)];
                    for (int k = 0; k < b.length; ++k) {
						b[k] = (byte)this.in.readByte();
					}
                    try {
                        final ByteArrayInputStream inb = new ByteArrayInputStream(b);
                        final Image bmp = BmpImage.getImage(inb, true, b.length);
                        this.cb.saveState();
                        this.cb.rectangle(xDest, yDest, destWidth, destHeight);
                        this.cb.clip();
                        this.cb.newPath();
                        bmp.scaleAbsolute(destWidth * bmp.getWidth() / srcWidth, -destHeight * bmp.getHeight() / srcHeight);
                        bmp.setAbsolutePosition(xDest - destWidth * xSrc / srcWidth, yDest + destHeight * ySrc / srcHeight - bmp.getScaledHeight());
                        this.cb.addImage(bmp);
                        this.cb.restoreState();
                    }
                    catch (final Exception e) {
                        // empty on purpose
                    }
                    break;
                }
            }
            this.in.skip(tsize * 2 - (this.in.getLength() - lenMarker));
        }
        this.state.cleanup(this.cb);
    }

    private void outputText(final int x, final int y, final int flag, final int x1, final int y1, final int x2, final int y2, final String text) {
        final MetaFont font = this.state.getCurrentFont();
        final float refX = this.state.transformX(x);
        final float refY = this.state.transformY(y);
        final float angle = this.state.transformAngle(font.getAngle());
        final float sin = (float)Math.sin(angle);
        final float cos = (float)Math.cos(angle);
        final float fontSize = font.getFontSize(this.state);
        final BaseFont bf = font.getFont();
        final int align = this.state.getTextAlign();
        final float textWidth = bf.getWidthPoint(text, fontSize);
        float tx = 0;
        float ty = 0;
        final float descender = bf.getFontDescriptor(BaseFont.DESCENT, fontSize);
        final float ury = bf.getFontDescriptor(BaseFont.BBOXURY, fontSize);
        this.cb.saveState();
        this.cb.concatCTM(cos, sin, -sin, cos, refX, refY);
        if ((align & MetaState.TA_CENTER) == MetaState.TA_CENTER) {
			tx = -textWidth / 2;
		} else if ((align & MetaState.TA_RIGHT) == MetaState.TA_RIGHT) {
			tx = -textWidth;
		}
        if ((align & MetaState.TA_BASELINE) == MetaState.TA_BASELINE) {
			ty = 0;
		} else if ((align & MetaState.TA_BOTTOM) == MetaState.TA_BOTTOM) {
			ty = -descender;
		} else {
			ty = -ury;
		}
        Color textColor;
        if (this.state.getBackgroundMode() == MetaState.OPAQUE) {
            textColor = this.state.getCurrentBackgroundColor();
            this.cb.setColorFill(textColor);
            this.cb.rectangle(tx, ty + descender, textWidth, ury - descender);
            this.cb.fill();
        }
        textColor = this.state.getCurrentTextColor();
        this.cb.setColorFill(textColor);
        this.cb.beginText();
        this.cb.setFontAndSize(bf, fontSize);
        this.cb.setTextMatrix(tx, ty);
        this.cb.showText(text);
        this.cb.endText();
        if (font.isUnderline()) {
            this.cb.rectangle(tx, ty - fontSize / 4, textWidth, fontSize / 15);
            this.cb.fill();
        }
        if (font.isStrikeout()) {
            this.cb.rectangle(tx, ty + fontSize / 3, textWidth, fontSize / 15);
            this.cb.fill();
        }
        this.cb.restoreState();
    }

    private boolean isNullStrokeFill(final boolean isRectangle) {
        final MetaPen pen = this.state.getCurrentPen();
        final MetaBrush brush = this.state.getCurrentBrush();
        final boolean noPen = pen.getStyle() == MetaPen.PS_NULL;
        final int style = brush.getStyle();
        final boolean isBrush = style == MetaBrush.BS_SOLID || style == MetaBrush.BS_HATCHED && this.state.getBackgroundMode() == MetaState.OPAQUE;
        final boolean result = noPen && !isBrush;
        if (!noPen) {
            if (isRectangle) {
				this.state.setLineJoinRectangle(this.cb);
			} else {
				this.state.setLineJoinPolygon(this.cb);
			}
        }
        return result;
    }

    private void strokeAndFill(){
        final MetaPen pen = this.state.getCurrentPen();
        final MetaBrush brush = this.state.getCurrentBrush();
        final int penStyle = pen.getStyle();
        final int brushStyle = brush.getStyle();
        if (penStyle == MetaPen.PS_NULL) {
            this.cb.closePath();
            if (this.state.getPolyFillMode() == MetaState.ALTERNATE) {
                this.cb.eoFill();
            }
            else {
                this.cb.fill();
            }
        }
        else {
            final boolean isBrush = brushStyle == MetaBrush.BS_SOLID || brushStyle == MetaBrush.BS_HATCHED && this.state.getBackgroundMode() == MetaState.OPAQUE;
            if (isBrush) {
                if (this.state.getPolyFillMode() == MetaState.ALTERNATE) {
					this.cb.closePathEoFillStroke();
				} else {
					this.cb.closePathFillStroke();
				}
            }
            else {
                this.cb.closePathStroke();
            }
        }
    }

    private static float getArc(final float xCenter, final float yCenter, final float xDot, final float yDot) {
        double s = Math.atan2(yDot - yCenter, xDot - xCenter);
        if (s < 0) {
			s += Math.PI * 2;
		}
        return (float)(s / Math.PI * 180);
    }



    private static void writeWord(final OutputStream os, final int v) throws IOException {
        os.write(v & 0xff);
        os.write(v >>> 8 & 0xff);
    }

    private static void writeDWord(final OutputStream os, final int v) throws IOException {
        writeWord(os, v & 0xffff);
        writeWord(os, v >>> 16 & 0xffff);
    }
}