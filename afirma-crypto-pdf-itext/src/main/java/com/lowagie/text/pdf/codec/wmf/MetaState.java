/*
 * $Id: MetaState.java 3427 2008-05-24 18:32:31Z xlv $
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
import java.util.ArrayList;
import java.util.Stack;

import com.lowagie.text.pdf.PdfContentByte;

class MetaState {




    static final int TA_RIGHT = 2;
    static final int TA_CENTER = 6;

    static final int TA_BOTTOM = 8;
    static final int TA_BASELINE = 24;


    public static final int OPAQUE = 2;

    static final int ALTERNATE = 1;


    private Stack savedStates;
    private ArrayList MetaObjects;
    private Point currentPoint;
    private MetaPen currentPen;
    private MetaBrush currentBrush;
    private MetaFont currentFont;
    private Color currentBackgroundColor = Color.white;
    private Color currentTextColor = Color.black;
    private int backgroundMode = OPAQUE;
    private int polyFillMode = ALTERNATE;
    private int lineJoin = 1;
    private int textAlign;
    private int offsetWx;
    private int offsetWy;
    private int extentWx;
    private int extentWy;
    private float scalingX;
    private float scalingY;


    /** Creates new MetaState */
    public MetaState() {
        this.savedStates = new Stack();
        this.MetaObjects = new ArrayList();
        this.currentPoint = new Point(0, 0);
        this.currentPen = new MetaPen();
        this.currentBrush = new MetaBrush();
        this.currentFont = new MetaFont();
    }

    private MetaState(final MetaState state) {
        setMetaState(state);
    }

    public void setMetaState(final MetaState state) {
        this.savedStates = state.savedStates;
        this.MetaObjects = state.MetaObjects;
        this.currentPoint = state.currentPoint;
        this.currentPen = state.currentPen;
        this.currentBrush = state.currentBrush;
        this.currentFont = state.currentFont;
        this.currentBackgroundColor = state.currentBackgroundColor;
        this.currentTextColor = state.currentTextColor;
        this.backgroundMode = state.backgroundMode;
        this.polyFillMode = state.polyFillMode;
        this.textAlign = state.textAlign;
        this.lineJoin = state.lineJoin;
        this.offsetWx = state.offsetWx;
        this.offsetWy = state.offsetWy;
        this.extentWx = state.extentWx;
        this.extentWy = state.extentWy;
        this.scalingX = state.scalingX;
        this.scalingY = state.scalingY;
    }

    public void addMetaObject(final MetaObject object) {
        for (int k = 0; k < this.MetaObjects.size(); ++k) {
            if (this.MetaObjects.get(k) == null) {
                this.MetaObjects.set(k, object);
                return;
            }
        }
        this.MetaObjects.add(object);
    }

    public void selectMetaObject(final int index, final PdfContentByte cb) {
        final MetaObject obj = (MetaObject)this.MetaObjects.get(index);
        if (obj == null) {
			return;
		}
        int style;
        switch (obj.getType()) {
            case MetaObject.META_BRUSH:
                this.currentBrush = (MetaBrush)obj;
                style = this.currentBrush.getStyle();
                if (style == MetaBrush.BS_SOLID) {
                    final Color color = this.currentBrush.getColor();
                    cb.setColorFill(color);
                }
                else if (style == MetaBrush.BS_HATCHED) {
                    final Color color = this.currentBackgroundColor;
                    cb.setColorFill(color);
                }
                break;
            case MetaObject.META_PEN:
            {
                this.currentPen = (MetaPen)obj;
                style = this.currentPen.getStyle();
                if (style != MetaPen.PS_NULL) {
                    final Color color = this.currentPen.getColor();
                    cb.setColorStroke(color);
                    cb.setLineWidth(Math.abs(this.currentPen.getPenWidth() * this.scalingX / this.extentWx));
                    switch (style) {
                        case MetaPen.PS_DASH:
                            cb.setLineDash(18, 6, 0);
                            break;
                        case MetaPen.PS_DASHDOT:
                            cb.setLiteral("[9 6 3 6]0 d\n");
                            break;
                        case MetaPen.PS_DASHDOTDOT:
                            cb.setLiteral("[9 3 3 3 3 3]0 d\n");
                            break;
                        case MetaPen.PS_DOT:
                            cb.setLineDash(3, 0);
                            break;
                        default:
                            cb.setLineDash(0);
                            break;
                    }
                }
                break;
            }
            case MetaObject.META_FONT:
            {
                this.currentFont = (MetaFont)obj;
                break;
            }
        }
    }

    public void deleteMetaObject(final int index) {
        this.MetaObjects.set(index, null);
    }

    public void saveState(final PdfContentByte cb) {
        cb.saveState();
        final MetaState state = new MetaState(this);
        this.savedStates.push(state);
    }

    public void restoreState(final int index, final PdfContentByte cb) {
        int pops;
        if (index < 0) {
			pops = Math.min(-index, this.savedStates.size());
		} else {
			pops = Math.max(this.savedStates.size() - index, 0);
		}
        if (pops == 0) {
			return;
		}
        MetaState state = null;
        while (pops-- != 0) {
            cb.restoreState();
            state = (MetaState)this.savedStates.pop();
        }
        setMetaState(state);
    }

    public void cleanup(final PdfContentByte cb) {
        int k = this.savedStates.size();
        while (k-- > 0) {
			cb.restoreState();
		}
    }

    public float transformX(final int x) {
        return ((float)x - this.offsetWx) * this.scalingX / this.extentWx;
    }

    public float transformY(final int y) {
        return (1f - ((float)y - this.offsetWy) / this.extentWy) * this.scalingY;
    }

    public void setScalingX(final float scalingX) {
        this.scalingX = scalingX;
    }

    public void setScalingY(final float scalingY) {
        this.scalingY = scalingY;
    }

    public void setOffsetWx(final int offsetWx) {
        this.offsetWx = offsetWx;
    }

    public void setOffsetWy(final int offsetWy) {
        this.offsetWy = offsetWy;
    }

    public void setExtentWx(final int extentWx) {
        this.extentWx = extentWx;
    }

    public void setExtentWy(final int extentWy) {
        this.extentWy = extentWy;
    }

    public float transformAngle(final float angle) {
        final float ta = this.scalingY < 0 ? -angle : angle;
        return (float)(this.scalingX < 0 ? Math.PI - ta : ta);
    }

    public void setCurrentPoint(final Point p) {
        this.currentPoint = p;
    }

    public Point getCurrentPoint() {
        return this.currentPoint;
    }

    public MetaBrush getCurrentBrush() {
        return this.currentBrush;
    }

    public MetaPen getCurrentPen() {
        return this.currentPen;
    }

    public MetaFont getCurrentFont() {
        return this.currentFont;
    }

    /** Getter for property currentBackgroundColor.
     * @return Value of property currentBackgroundColor.
     */
    public Color getCurrentBackgroundColor() {
        return this.currentBackgroundColor;
    }

    /** Setter for property currentBackgroundColor.
     * @param currentBackgroundColor New value of property currentBackgroundColor.
     */
    public void setCurrentBackgroundColor(final Color currentBackgroundColor) {
        this.currentBackgroundColor = currentBackgroundColor;
    }

    /** Getter for property currentTextColor.
     * @return Value of property currentTextColor.
     */
    public Color getCurrentTextColor() {
        return this.currentTextColor;
    }

    /** Setter for property currentTextColor.
     * @param currentTextColor New value of property currentTextColor.
     */
    public void setCurrentTextColor(final Color currentTextColor) {
        this.currentTextColor = currentTextColor;
    }

    /** Getter for property backgroundMode.
     * @return Value of property backgroundMode.
     */
    public int getBackgroundMode() {
        return this.backgroundMode;
    }

    /** Setter for property backgroundMode.
     * @param backgroundMode New value of property backgroundMode.
     */
    public void setBackgroundMode(final int backgroundMode) {
        this.backgroundMode = backgroundMode;
    }

    /** Getter for property textAlign.
     * @return Value of property textAlign.
     */
    public int getTextAlign() {
        return this.textAlign;
    }

    /** Setter for property textAlign.
     * @param textAlign New value of property textAlign.
     */
    public void setTextAlign(final int textAlign) {
        this.textAlign = textAlign;
    }

    /** Getter for property polyFillMode.
     * @return Value of property polyFillMode.
     */
    public int getPolyFillMode() {
        return this.polyFillMode;
    }

    /** Setter for property polyFillMode.
     * @param polyFillMode New value of property polyFillMode.
     */
    public void setPolyFillMode(final int polyFillMode) {
        this.polyFillMode = polyFillMode;
    }

    public void setLineJoinRectangle(final PdfContentByte cb) {
        if (this.lineJoin != 0) {
            this.lineJoin = 0;
            cb.setLineJoin(0);
        }
    }

    public void setLineJoinPolygon(final PdfContentByte cb) {
        if (this.lineJoin == 0) {
            this.lineJoin = 1;
            cb.setLineJoin(1);
        }
    }

    public boolean getLineNeutral() {
        return this.lineJoin == 0;
    }

}
