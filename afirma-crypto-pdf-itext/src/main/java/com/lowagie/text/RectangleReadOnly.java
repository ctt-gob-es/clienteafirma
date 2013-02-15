/*
 * $Id: RectangleReadOnly.java 3746 2009-03-04 10:13:52Z blowagie $
 *
 * Copyright 1999, 2000, 2001, 2002 by Bruno Lowagie.
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

package com.lowagie.text;

import java.awt.Color;

/**
 * A <CODE>RectangleReadOnly</CODE> is the representation of a geometric figure.
 * It's the same as a <CODE>Rectangle</CODE> but immutable.
 * Rectangles support constant width borders using
 * {@link #setBorderWidth(float)}and {@link #setBorder(int)}.
 * They also support borders that vary in width/color on each side using
 * methods like {@link #setBorderWidthLeft(float)}or
 * {@link #setBorderColorLeft(java.awt.Color)}.
 *
 * @see Element
 * @see Table
 * @see Cell
 * @see HeaderFooter
 * @since 2.1.2
 */

class RectangleReadOnly extends Rectangle {

	// CONSTRUCTORS

	/**
	 * Constructs a <CODE>RectangleReadOnly</CODE> -object starting from the origin
	 * (0, 0).
	 *
	 * @param urx	upper right x
	 * @param ury	upper right y
	 */
	public RectangleReadOnly(final float urx, final float ury) {
		super(0, 0, urx, ury);
	}

	/**
	 * Throws an error because of the read only nature of this object.
	 */
    private void throwReadOnlyError() {
        throw new UnsupportedOperationException("RectangleReadOnly: this Rectangle is read only.");
    }

	// OVERWRITE METHODS SETTING THE DIMENSIONS:

	/**
	 * Sets the lower left x-coordinate.
	 *
	 * @param llx	the new value
	 */
	@Override
	public void setLeft(final float llx) {
		throwReadOnlyError();
	}

	/**
	 * Sets the upper right x-coordinate.
	 *
	 * @param urx	the new value
	 */

	@Override
	public void setRight(final float urx) {
		throwReadOnlyError();
	}

	/**
	 * Sets the upper right y-coordinate.
	 *
	 * @param ury	the new value
	 */
	@Override
	public void setTop(final float ury) {
		throwReadOnlyError();
	}

	/**
	 * Sets the lower left y-coordinate.
	 *
	 * @param lly	the new value
	 */
	@Override
	public void setBottom(final float lly) {
		throwReadOnlyError();
	}

	/**
	 * Normalizes the rectangle.
     * Switches lower left with upper right if necessary.
	 */
	@Override
	public void normalize() {
		throwReadOnlyError();
	}

	// OVERWRITE METHODS SETTING THE BACKGROUND COLOR:

	/**
	 * Sets the backgroundcolor of the rectangle.
	 *
	 * @param value	the new value
	 */
	@Override
	public void setBackgroundColor(final Color value) {
		throwReadOnlyError();
	}

	/**
	 * Sets the grayscale of the rectangle.
	 *
	 * @param value	the new value
	 */
	@Override
	public void setGrayFill(final float value) {
		throwReadOnlyError();
	}

	// OVERWRITE METHODS SETTING THE BORDER:

	/**
	 * Enables/Disables the border on the specified sides.
	 * The border is specified as an integer bitwise combination of
	 * the constants: <CODE>LEFT, RIGHT, TOP, BOTTOM</CODE>.
	 *
	 * @see #enableBorderSide(int)
	 * @see #disableBorderSide(int)
	 * @param border	the new value
	 */
	@Override
	public void setBorder(final int border) {
		throwReadOnlyError();
	}

	/**
	 * Sets a parameter indicating if the rectangle has variable borders
	 *
	 * @param useVariableBorders	indication if the rectangle has variable borders
	 */
	@Override
	public void setUseVariableBorders(final boolean useVariableBorders) {
		throwReadOnlyError();
	}

	/**
	 * Enables the border on the specified side.
	 *
	 * @param side	the side to enable.
	 * One of <CODE>LEFT, RIGHT, TOP, BOTTOM</CODE>
	 */
	@Override
	public void enableBorderSide(final int side) {
		throwReadOnlyError();
	}

	/**
	 * Disables the border on the specified side.
	 *
	 * @param side	the side to disable.
	 * One of <CODE>LEFT, RIGHT, TOP, BOTTOM</CODE>
	 */
	@Override
	public void disableBorderSide(final int side) {
		throwReadOnlyError();
	}

	// OVERWRITE METHODS SETTING THE BORDER WIDTH:

	/**
	 * Sets the borderwidth of the table.
	 *
	 * @param borderWidth	the new value
	 */

	@Override
	public void setBorderWidth(final float borderWidth) {
		throwReadOnlyError();
	}

	/**
	 * Sets the width of the left border
	 *
	 * @param borderWidthLeft	a width
	 */
	@Override
	public void setBorderWidthLeft(final float borderWidthLeft) {
		throwReadOnlyError();
	}

	/**
	 * Sets the width of the right border
	 *
	 * @param borderWidthRight	a width
	 */
	@Override
	public void setBorderWidthRight(final float borderWidthRight) {
		throwReadOnlyError();
	}

	/**
	 * Sets the width of the top border
	 *
	 * @param borderWidthTop	a width
	 */
	@Override
	public void setBorderWidthTop(final float borderWidthTop) {
		throwReadOnlyError();
	}

	/**
	 * Sets the width of the bottom border
	 *
	 * @param borderWidthBottom	a width
	 */
	@Override
	public void setBorderWidthBottom(final float borderWidthBottom) {
		throwReadOnlyError();
	}

	// METHODS TO GET/SET THE BORDER COLOR:

	/**
	 * Sets the color of the border.
	 *
	 * @param borderColor	a <CODE>Color</CODE>
	 */

	@Override
	public void setBorderColor(final Color borderColor) {
		throwReadOnlyError();
	}

	/**
	 * Sets the color of the left border.
	 *
	 * @param borderColorLeft	a <CODE>Color</CODE>
	 */
	@Override
	public void setBorderColorLeft(final Color borderColorLeft) {
		throwReadOnlyError();
	}

	/**
	 * Sets the color of the right border
	 *
	 * @param borderColorRight	a <CODE>Color</CODE>
	 */
	@Override
	public void setBorderColorRight(final Color borderColorRight) {
		throwReadOnlyError();
	}

	/**
	 * Sets the color of the top border.
	 *
	 * @param borderColorTop	a <CODE>Color</CODE>
	 */
	@Override
	public void setBorderColorTop(final Color borderColorTop) {
		throwReadOnlyError();
	}

	/**
	 * Sets the color of the bottom border.
	 *
	 * @param borderColorBottom	a <CODE>Color</CODE>
	 */
	@Override
	public void setBorderColorBottom(final Color borderColorBottom) {
		throwReadOnlyError();
	}

	// SPECIAL METHODS:

	/**
	 * Copies each of the parameters, except the position, from a
     * <CODE>Rectangle</CODE> object
	 *
	 * @param rect	<CODE>Rectangle</CODE> to copy from
	 */
	@Override
	public void cloneNonPositionParameters(final Rectangle rect) {
		throwReadOnlyError();
	}

	/**
	 * Copies each of the parameters, except the position, from a
     * <CODE>Rectangle</CODE> object if the value is set there.
	 *
	 * @param rect	<CODE>Rectangle</CODE> to copy from
	 */
	@Override
	public void softCloneNonPositionParameters(final Rectangle rect) {
		throwReadOnlyError();
	}

	/**
	 * @return	String version of the most important rectangle properties
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuffer buf = new StringBuffer("RectangleReadOnly: ");
		buf.append(getWidth());
		buf.append('x');
		buf.append(getHeight());
		buf.append(" (rot: ");
		buf.append(this.rotation);
		buf.append(" degrees)");
		return buf.toString();
	}
}