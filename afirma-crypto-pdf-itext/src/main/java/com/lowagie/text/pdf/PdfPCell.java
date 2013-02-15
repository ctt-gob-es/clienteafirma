/*
 * $Id: PdfPCell.java 3992 2009-06-19 12:05:06Z blowagie $
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

import java.util.List;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.events.PdfPCellEventForwarder;

/**
 * A cell in a PdfPTable.
 */

public class PdfPCell extends Rectangle{

    private ColumnText column = new ColumnText(null);

    /** Vertical alignment of the cell. */
    private int verticalAlignment = Element.ALIGN_TOP;

    /** Left padding of the cell. */
    private float paddingLeft = 2;

    /** Right padding of the cell. */
    private float paddingRight = 2;

    /** Top padding of the cell. */
    private float paddingTop = 2;

    /** Bottom padding of the cell. */
    private float paddingBottom = 2;

    /** Fixed height of the cell. */
    private float fixedHeight = 0;

    /** Minimum height of the cell. */
    private float minimumHeight;

    /** Holds value of property noWrap. */
    private boolean noWrap = false;

    /** Holds value of property table. */
    private PdfPTable table;

    /** Holds value of property colspan. */
    private int colspan = 1;

    /**
     * Holds value of property rowspan.
     * @since	2.1.6
     */
    private int rowspan = 1;

    /** Holds value of property image. */
    private Image image;

    /** Holds value of property cellEvent. */
    private PdfPCellEvent cellEvent;

    /** Holds value of property useDescender. */
    private boolean useDescender;

    /** Increases padding to include border if true */
    private boolean useBorderPadding = false;

    /** The text in the cell. */
    private Phrase phrase;

    /**
     * The rotation of the cell. Possible values are
     * 0, 90, 180 and 270.
     */
    private int rotation;

    /**
     * Constructs an empty <CODE>PdfPCell</CODE>.
     * The default padding is 2.
     */
    public PdfPCell() {
        super(0, 0, 0, 0);
        this.borderWidth = 0.5f;
        this.border = BOX;
        this.column.setLeading(0, 1);
    }

    /**
     * Constructs a <CODE>PdfPCell</CODE> with a <CODE>Phrase</CODE>.
     * The default padding is 2.
     *
     * @param phrase the text
     */
    PdfPCell(final Phrase phrase) {
        super(0, 0, 0, 0);
        this.borderWidth = 0.5f;
        this.border = BOX;
        this.column.addText(this.phrase = phrase);
        this.column.setLeading(0, 1);
    }



    /**
     * Constructs a <CODE>PdfPCell</CODE> with an <CODE>Image</CODE>.
     * The default padding is 0.25 for a border width of 0.5.
     *
     * @param image the <CODE>Image</CODE>
     * @param fit <CODE>true</CODE> to fit the image to the cell
     */
    PdfPCell(final Image image, final boolean fit) {
        super(0, 0, 0, 0);
        this.borderWidth = 0.5f;
        this.border = BOX;
        if (fit) {
            this.image = image;
            this.column.setLeading(0, 1);
            setPadding(this.borderWidth / 2);
        }
        else {
            this.column.addText(this.phrase = new Phrase(new Chunk(image, 0, 0)));
            this.column.setLeading(0, 1);
            setPadding(0);
        }
    }

    /**
     * Constructs a <CODE>PdfPCell</CODE> with a <CODE>PdfPtable</CODE>.
     * This constructor allows nested tables.
     * The default padding is 0.
     *
     * @param table The <CODE>PdfPTable</CODE>
     */
    public PdfPCell(final PdfPTable table) {
        this(table, null);
    }

    /**
     * Constructs a <CODE>PdfPCell</CODE> with a <CODE>PdfPtable</CODE>.
     * This constructor allows nested tables.
     *
     * @param table The <CODE>PdfPTable</CODE>
     * @param style	The style to apply to the cell (you could use getDefaultCell())
     * @since 2.1.0
     */
    private PdfPCell(final PdfPTable table, final PdfPCell style) {
        super(0, 0, 0, 0);
        this.borderWidth = 0.5f;
        this.border = BOX;
        this.column.setLeading(0, 1);
        this.table = table;
        table.setWidthPercentage(100);
        table.setExtendLastRow(true);
        this.column.addElement(table);
        if (style != null) {
        	cloneNonPositionParameters(style);
        	this.verticalAlignment = style.verticalAlignment;
        	this.paddingLeft = style.paddingLeft;
        	this.paddingRight = style.paddingRight;
        	this.paddingTop = style.paddingTop;
        	this.paddingBottom = style.paddingBottom;
        	this.colspan = style.colspan;
        	this.rowspan = style.rowspan;
        	this.cellEvent = style.cellEvent;
        	this.useDescender = style.useDescender;
        	this.useBorderPadding = style.useBorderPadding;
        	this.rotation = style.rotation;
        } else {
			setPadding(0);
		}
    }

    /**
     * Constructs a deep copy of a <CODE>PdfPCell</CODE>.
     *
     * @param cell the <CODE>PdfPCell</CODE> to duplicate
     */
    PdfPCell(final PdfPCell cell) {
        super(cell.llx, cell.lly, cell.urx, cell.ury);
        cloneNonPositionParameters(cell);
        this.verticalAlignment = cell.verticalAlignment;
        this.paddingLeft = cell.paddingLeft;
        this.paddingRight = cell.paddingRight;
        this.paddingTop = cell.paddingTop;
        this.paddingBottom = cell.paddingBottom;
        this.phrase = cell.phrase;
        this.fixedHeight = cell.fixedHeight;
        this.minimumHeight = cell.minimumHeight;
        this.noWrap = cell.noWrap;
        this.colspan = cell.colspan;
        this.rowspan = cell.rowspan;
        if (cell.table != null) {
			this.table = new PdfPTable(cell.table);
		}
        this.image = Image.getInstance(cell.image);
        this.cellEvent = cell.cellEvent;
        this.useDescender = cell.useDescender;
        this.column = ColumnText.duplicate(cell.column);
        this.useBorderPadding = cell.useBorderPadding;
        this.rotation = cell.rotation;
    }

    /**
     * Adds an iText element to the cell.
     *
     * @param element
     */
    public void addElement(final Element element) {
        if (this.table != null) {
            this.table = null;
            this.column.setText(null);
        }
        this.column.addElement(element);
    }

    /**
     * Gets the <CODE>Phrase</CODE> from this cell.
     *
     * @return the <CODE>Phrase</CODE>
     */
    public Phrase getPhrase() {
        return this.phrase;
    }

    /**
     * Sets the <CODE>Phrase</CODE> for this cell.
     *
     * @param phrase the <CODE>Phrase</CODE>
     */
    public void setPhrase(final Phrase phrase) {
        this.table = null;
        this.image = null;
        this.column.setText(this.phrase = phrase);
    }

    /**
     * Gets the horizontal alignment for the cell.
     *
     * @return the horizontal alignment for the cell
     */
    public int getHorizontalAlignment() {
        return this.column.getAlignment();
    }

    /**
     * Sets the horizontal alignment for the cell. It could be
     * <CODE>Element.ALIGN_CENTER</CODE> for example.
     *
     * @param horizontalAlignment The horizontal alignment
     */
    public void setHorizontalAlignment(final int horizontalAlignment) {
        this.column.setAlignment(horizontalAlignment);
    }

    /**
     * Gets the vertical alignment for the cell.
     *
     * @return the vertical alignment for the cell
     */
    public int getVerticalAlignment() {
        return this.verticalAlignment;
    }

    /**
     * Sets the vertical alignment for the cell. It could be
     * <CODE>Element.ALIGN_MIDDLE</CODE> for example.
     *
     * @param verticalAlignment The vertical alignment
     */
    public void setVerticalAlignment(final int verticalAlignment) {
        if (this.table != null) {
			this.table.setExtendLastRow(verticalAlignment == Element.ALIGN_TOP);
		}
        this.verticalAlignment = verticalAlignment;
    }

    /**
     * Gets the effective left padding.
     * This will include the left border width if
     * {@link #isUseBorderPadding()} is true.
     *
     * @return effective value of property paddingLeft.
     */
    public float getEffectivePaddingLeft() {
    	if (isUseBorderPadding()) {
    		final float border = getBorderWidthLeft() / (isUseVariableBorders() ? 1f : 2f);
    	    return this.paddingLeft + border;
    	}
    	return this.paddingLeft;
    }

    /**
     * @return Value of property paddingLeft.
     */
    public float getPaddingLeft() {
        return this.paddingLeft;
    }

    /**
     * Setter for property paddingLeft.
     *
     * @param paddingLeft New value of property paddingLeft.
     */
    public void setPaddingLeft(final float paddingLeft) {
        this.paddingLeft = paddingLeft;
    }

    /**
     * Gets the effective right padding.  This will include
     * the right border width if {@link #isUseBorderPadding()} is true.
     *
     * @return effective value of property paddingRight.
     */
    public float getEffectivePaddingRight() {
    	if (isUseBorderPadding()) {
    		final float border = getBorderWidthRight() / (isUseVariableBorders() ? 1f : 2f);
    		return this.paddingRight + border;
    	}
    	return this.paddingRight;
    }

    /**
     * Getter for property paddingRight.
     *
     * @return Value of property paddingRight.
     */
    public float getPaddingRight() {
        return this.paddingRight;
    }

    /**
     * Setter for property paddingRight.
     *
     * @param paddingRight New value of property paddingRight.
     */
    public void setPaddingRight(final float paddingRight) {
        this.paddingRight = paddingRight;
    }

    /**
     * Gets the effective top padding.  This will include
     * the top border width if {@link #isUseBorderPadding()} is true.
     *
     * @return effective value of property paddingTop.
     */
    public float getEffectivePaddingTop() {
    	if (isUseBorderPadding()) {
    		final float border = getBorderWidthTop()/(isUseVariableBorders()?1f:2f);
    		return this.paddingTop + border;
    	}
        return this.paddingTop;
    }

    /**
     * Getter for property paddingTop.
     *
     * @return Value of property paddingTop.
     */
    public float getPaddingTop() {
        return this.paddingTop;
    }

    /**
     * Setter for property paddingTop.
     *
     * @param paddingTop New value of property paddingTop.
     */
    public void setPaddingTop(final float paddingTop) {
        this.paddingTop = paddingTop;
    }

    /**
     * Gets the effective bottom padding.
     * This will include  the bottom border width if
     * {@link #isUseBorderPadding()} is true.
     *
     * @return effective value of property paddingBottom.
     */
    public float getEffectivePaddingBottom() {
    	if (isUseBorderPadding()) {
    		final float border = getBorderWidthBottom()/(isUseVariableBorders()?1f:2f);
    		return this.paddingBottom + border;
    	}
        return this.paddingBottom;
    }

    /**
     * Getter for property paddingBottom.
     *
     * @return Value of property paddingBottom.
     */
    public float getPaddingBottom() {
        return this.paddingBottom;
    }

    /**
     * Setter for property paddingBottom.
     *
     * @param paddingBottom New value of property paddingBottom.
     */
    public void setPaddingBottom(final float paddingBottom) {
        this.paddingBottom = paddingBottom;
    }

    /**
     * Sets the padding of the contents in the cell (space between content and border).
     *
     * @param padding
     */
    public void setPadding(final float padding) {
        this.paddingBottom = padding;
        this.paddingTop = padding;
        this.paddingLeft = padding;
        this.paddingRight = padding;
    }

    /**
     * If true, then effective padding will include border widths
     *
     * @return true if effective padding includes border widths
     */
    public boolean isUseBorderPadding() {
        return this.useBorderPadding;
    }

    /**
     * Adjusts effective padding to include border widths.
     *
     * @param use adjust effective padding if true
     */
    public void setUseBorderPadding(final boolean use) {
        this.useBorderPadding = use;
    }

    /**
     * Sets the leading fixed and variable.
     * The resultant leading will be:
     * fixedLeading+multipliedLeading*maxFontSize
     * where maxFontSize is the size of the biggest font in the line.
     *
     * @param fixedLeading the fixed leading
     * @param multipliedLeading the variable leading
     */
    public void setLeading(final float fixedLeading, final float multipliedLeading) {
        this.column.setLeading(fixedLeading, multipliedLeading);
    }

    /**
     * Gets the fixed leading.
     *
     * @return the leading
     */
    public float getLeading() {
        return this.column.getLeading();
    }

    /**
     * Gets the variable leading.
     *
     * @return the leading
     */
    public float getMultipliedLeading() {
        return this.column.getMultipliedLeading();
    }

    /**
     * Sets the first paragraph line indent.
     *
     * @param indent the indent
     */
    public void setIndent(final float indent) {
        this.column.setIndent(indent);
    }

    /**
     * Gets the first paragraph line indent.
     *
     * @return the indent
     */
    public float getIndent() {
        return this.column.getIndent();
    }

    /**
     * Gets the extra space between paragraphs.
     *
     * @return the extra space between paragraphs
     */
    public float getExtraParagraphSpace() {
        return this.column.getExtraParagraphSpace();
    }

    /**
     * Sets the extra space between paragraphs.
     *
     * @param extraParagraphSpace the extra space between paragraphs
     */
    public void setExtraParagraphSpace(final float extraParagraphSpace) {
        this.column.setExtraParagraphSpace(extraParagraphSpace);
    }

    /**
     * Set a fixed height for the cell.
     * This will automatically unset minimumHeight, if set.
     *
     * @param fixedHeight New value of property fixedHeight.
     */
    public void setFixedHeight(final float fixedHeight) {
        this.fixedHeight = fixedHeight;
        this.minimumHeight = 0;
    }

    /**
     * Get the fixed height of the cell.
     *
     * @return Value of property fixedHeight.
     */
    public float getFixedHeight() {
        return this.fixedHeight;
    }

    /**
     * Tells you whether the cell has a fixed height.
     *
     * @return	true is a fixed height was set.
     * @since 2.1.5
     */
    private boolean hasFixedHeight() {
    	return getFixedHeight() > 0;
    }

    /**
     * Set a minimum height for the cell.
     * This will automatically unset fixedHeight, if set.
     *
     * @param minimumHeight New value of property minimumHeight.
     */
    public void setMinimumHeight(final float minimumHeight) {
        this.minimumHeight = minimumHeight;
        this.fixedHeight = 0;
    }

    /**
     * Get the minimum height of the cell.
     *
     * @return Value of property minimumHeight.
     */
    public float getMinimumHeight() {
        return this.minimumHeight;
    }



    /**
     * Getter for property noWrap.
     *
     * @return Value of property noWrap.
     */
    public boolean isNoWrap() {
        return this.noWrap;
    }

    /**
     * Setter for property noWrap.
     *
     * @param noWrap New value of property noWrap.
     */
    public void setNoWrap(final boolean noWrap) {
        this.noWrap = noWrap;
    }

    /**
     * Getter for property table.
     *
     * @return Value of property table.
     * @since 2.x
     */
    public PdfPTable getTable() {
        return this.table;
    }



    /**
     * Getter for property colspan.
     *
     * @return Value of property colspan.
     */
    public int getColspan() {
        return this.colspan;
    }

    /**
     * Setter for property colspan.
     *
     * @param colspan New value of property colspan.
     */
    public void setColspan(final int colspan) {
        this.colspan = colspan;
    }

    /**
     * Getter for property rowspan.
     *
     * @return Value of property rowspan.
     * @since	2.1.6
     */
    public int getRowspan() {
        return this.rowspan;
    }

    /**
     * Setter for property rowspan.
     *
     * @param rowspan New value of property rowspan.
     * @since	2.1.6
     */
    public void setRowspan(final int rowspan) {
        this.rowspan = rowspan;
    }

    /**
     * Sets the following paragraph lines indent.
     *
     * @param indent the indent
     */
    public void setFollowingIndent(final float indent) {
        this.column.setFollowingIndent(indent);
    }

    /**
     * Gets the following paragraph lines indent.
     *
     * @return the indent
     */
    public float getFollowingIndent() {
        return this.column.getFollowingIndent();
    }

    /**
     * Sets the right paragraph lines indent.
     *
     * @param indent the indent
     */
    public void setRightIndent(final float indent) {
        this.column.setRightIndent(indent);
    }

    /**
     * Gets the right paragraph lines indent.
     *
     * @return the indent
     */
    public float getRightIndent() {
        return this.column.getRightIndent();
    }

    /**
     * Gets the space/character extra spacing ratio for fully justified text.
     *
     * @return the space/character extra spacing ratio
     */
    public float getSpaceCharRatio() {
        return this.column.getSpaceCharRatio();
    }

    /** Sets the ratio between the extra word spacing and the
     * extra character spacing when the text is fully justified.
     * Extra word spacing will grow <CODE>spaceCharRatio</CODE> times more
     * than extra character spacing.
     * If the ratio is <CODE>PdfWriter.NO_SPACE_CHAR_RATIO</CODE> then the
     * extra character spacing will be zero.
     *
     * @param spaceCharRatio the ratio between the extra word spacing and the extra character spacing
     */
    public void setSpaceCharRatio(final float spaceCharRatio) {
        this.column.setSpaceCharRatio(spaceCharRatio);
    }

    /**
     * Sets the run direction of the text content in the cell.
     * May be either of:
     * PdfWriter.RUN_DIRECTION_DEFAULT, PdfWriter.RUN_DIRECTION_NO_BIDI,
     * PdfWriter.RUN_DIRECTION_LTR or PdfWriter.RUN_DIRECTION_RTL.
     * @param runDirection
     */
    public void setRunDirection(final int runDirection) {
        this.column.setRunDirection(runDirection);
    }

    /**
     * Gets the run direction of the text content in the cell
     *
     * @return One of the following values:
     * PdfWriter.RUN_DIRECTION_DEFAULT, PdfWriter.RUN_DIRECTION_NO_BIDI,
     * PdfWriter.RUN_DIRECTION_LTR or PdfWriter.RUN_DIRECTION_RTL.
     */
    public int getRunDirection() {
        return this.column.getRunDirection();
    }

    /**
     * Getter for property image.
     *
     * @return Value of property image.
     */
    public Image getImage() {
        return this.image;
    }

    /**
     * Setter for property image.
     *
     * @param image New value of property image.
     */
    public void setImage(final Image image) {
        this.column.setText(null);
        this.table = null;
        this.image = image;
    }

    /**
     * Gets the cell event for this cell.
     *
     * @return the cell event
     */
    public PdfPCellEvent getCellEvent() {
        return this.cellEvent;
    }

    /**
     * Sets the cell event for this cell.
     *
     * @param cellEvent the cell event
     */
    public void setCellEvent(final PdfPCellEvent cellEvent) {
    	if (cellEvent == null) {
			this.cellEvent = null;
		} else if (this.cellEvent == null) {
			this.cellEvent = cellEvent;
		} else if (this.cellEvent instanceof PdfPCellEventForwarder) {
			((PdfPCellEventForwarder)this.cellEvent).addCellEvent(cellEvent);
		} else {
    		final PdfPCellEventForwarder forward = new PdfPCellEventForwarder();
    		forward.addCellEvent(this.cellEvent);
    		forward.addCellEvent(cellEvent);
    		this.cellEvent = forward;
    	}
    }

    /**
     * Gets the arabic shaping options.
     *
     * @return the arabic shaping options
     */
    public int getArabicOptions() {
        return this.column.getArabicOptions();
    }

    /**
     * Sets the arabic shaping options.
     * The option can be AR_NOVOWEL, AR_COMPOSEDTASHKEEL and AR_LIG.
     *
     * @param arabicOptions the arabic shaping options
     */
    public void setArabicOptions(final int arabicOptions) {
        this.column.setArabicOptions(arabicOptions);
    }

    /**
     * Gets state of first line height based on max ascender
     *
     * @return true if an ascender is to be used.
     */
    public boolean isUseAscender() {
        return this.column.isUseAscender();
    }

    /**
     * Enables/ Disables adjustment of first line height based on max ascender.
     *
     * @param useAscender adjust height if true
     */
    public void setUseAscender(final boolean useAscender) {
        this.column.setUseAscender(useAscender);
    }


    /**
     * Getter for property useDescender.
     *
     * @return Value of property useDescender.
     */
    public boolean isUseDescender() {
        return this.useDescender;
    }

    /**
     * Setter for property useDescender.
     *
     * @param useDescender New value of property useDescender.
     */
    public void setUseDescender(final boolean useDescender) {
        this.useDescender = useDescender;
    }

    /**
     * Gets the ColumnText with the content of the cell.
     *
     * @return a columntext object
     */
    public ColumnText getColumn() {
        return this.column;
    }

    /**
     * Returns the list of composite elements of the column.
     *
     * @return	a List object.
     * @since	2.1.1
     */
    public List getCompositeElements() {
    	return getColumn().compositeElements;
    }

    /**
     * Sets the columntext in the cell.
     *
     * @param column
     */
    public void setColumn(final ColumnText column) {
        this.column = column;
    }

    /**
     * Gets the rotation of the cell.
     *
     * @return the rotation of the cell.
     */
    @Override
	public int getRotation() {
        return this.rotation;
    }

    /**
     * Sets the rotation of the cell.
     * Possible values are 0, 90, 180 and 270.
     *
     * @param rotation the rotation of the cell
     */
    public void setRotation(int rotation) {
        rotation %= 360;
        if (rotation < 0) {
			rotation += 360;
		}
        if (rotation % 90 != 0) {
			throw new IllegalArgumentException("Rotation must be a multiple of 90.");
		}
        this.rotation = rotation;
    }

    /**
     * Consumes part of the content of the cell.
     * @param	height	the hight of the part that has to be consumed
     * @since	2.1.6
     */
    void consumeHeight(final float height) {
        final float rightLimit = getRight() - getEffectivePaddingRight();
        final float leftLimit = getLeft() + getEffectivePaddingLeft();
        final float bry = height - getEffectivePaddingTop() - getEffectivePaddingBottom();
        if (getRotation() != 90 && getRotation() != 270) {
            this.column.setSimpleColumn(leftLimit, bry + 0.001f,	rightLimit, 0);
        }
        else {
        	this.column.setSimpleColumn(0, leftLimit, bry + 0.001f, rightLimit);
        }
        try {
        	this.column.go(true);
		} catch (final DocumentException e) {
			// do nothing
		}
    }

	/**
	 * Returns the height of the cell.
	 * @return	the height of the cell
	 * @since	3.0.0
	 */
	public float getMaxHeight() {
		final boolean pivoted = getRotation() == 90 || getRotation() == 270;
		final Image img = getImage();
		if (img != null) {
			img.scalePercent(100);
			final float refWidth = pivoted ? img.getScaledHeight() : img.getScaledWidth();
			final float scale = (getRight() - getEffectivePaddingRight()
                    - getEffectivePaddingLeft() - getLeft()) / refWidth;
			img.scalePercent(scale * 100);
			final float refHeight = pivoted ? img.getScaledWidth() : img.getScaledHeight();
			setBottom(getTop() - getEffectivePaddingTop() - getEffectivePaddingBottom() - refHeight);
		}
		else {
			if (pivoted && hasFixedHeight()) {
				setBottom(getTop() - getFixedHeight());
			} else {
				final ColumnText ct = ColumnText.duplicate(getColumn());
				float right, top, left, bottom;
				if (pivoted) {
					right = PdfPRow.RIGHT_LIMIT;
					top = getRight() - getEffectivePaddingRight();
					left = 0;
					bottom = getLeft() + getEffectivePaddingLeft();
				}
				else {
					right = isNoWrap() ? PdfPRow.RIGHT_LIMIT : getRight() - getEffectivePaddingRight();
					top = getTop() - getEffectivePaddingTop();
					left = getLeft() + getEffectivePaddingLeft();
					bottom = hasFixedHeight() ? top + getEffectivePaddingBottom() - getFixedHeight() : PdfPRow.BOTTOM_LIMIT;
				}
				PdfPRow.setColumn(ct, left, bottom, right, top);
				try {
					ct.go(true);
				} catch (final DocumentException e) {
					throw new ExceptionConverter(e);
				}
				if (pivoted) {
					setBottom(getTop() - getEffectivePaddingTop() - getEffectivePaddingBottom() - ct.getFilledWidth());
				} else {
					float yLine = ct.getYLine();
					if (isUseDescender()) {
						yLine += ct.getDescender();
					}
					setBottom(yLine - getEffectivePaddingBottom());
				}
			}
		}
		float height = getHeight();
		if (height < getFixedHeight()) {
			height = getFixedHeight();
		} else if (height < getMinimumHeight()) {
			height = getMinimumHeight();
		}
		return height;
	}
}