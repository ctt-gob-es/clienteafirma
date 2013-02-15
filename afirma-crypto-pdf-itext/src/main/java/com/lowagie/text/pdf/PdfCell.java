/*
 * $Id: PdfCell.java 3671 2009-02-01 14:46:09Z blowagie $
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

import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.Anchor;
import com.lowagie.text.Cell;
import com.lowagie.text.Chunk;
import com.lowagie.text.Element;
import com.lowagie.text.Image;
import com.lowagie.text.List;
import com.lowagie.text.ListItem;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;

/**
 * A <CODE>PdfCell</CODE> is the PDF translation of a <CODE>Cell</CODE>.
 * <P>
 * A <CODE>PdfCell</CODE> is an <CODE>ArrayList</CODE> of <CODE>PdfLine</CODE>s.
 * <P>
 * When using variable borders ({@link com.lowagie.text.Rectangle#isUseVariableBorders isUseVariableBorders()} == true),
 * the borders are drawn completely inside the cell Rectangle
 * so that adjacent cell borders will not overlap.
 * Otherwise, the borders are drawn on top of the edges of the
 * cell Rectangle and will overlap the borders of adjacent
 * cells.
 *
 * @see		com.lowagie.text.Rectangle
 * @see		com.lowagie.text.Cell
 * @see		PdfLine
 * @see		PdfTable
 */

class PdfCell extends Rectangle {

    // membervariables

    /**
     * These are the PdfLines in the Cell.
     */
    private ArrayList lines;

    /**
     * These are the PdfLines in the Cell.
     */
    private PdfLine line;

    /**
     * These are the Images in the Cell.
     */
    private final ArrayList images;

    /**
     * This is the leading of the lines.
     */
    private final float leading;

    /**
     * This is the number of the row the cell is in.
     */
    private final int rownumber;

    /**
     * This is the rowspan of the cell.
     */
    private final int rowspan;

    /**
     * This is the cellspacing of the cell.
     */
    private final float cellspacing;

    /**
     * This is the cellpadding of the cell.
     */
    private final float cellpadding;

    /**
     * Indicates if this cell belongs to the header of a <CODE>PdfTable</CODE>
     */
    private boolean header = false;

    /**
     * This is the total height of the content of the cell.  Note that the actual cell
     * height may be larger due to another cell on the row *
     */
    private float contentHeight = 0.0f;

    /**
     * Indicates that the largest ascender height should be used to
     * determine the height of the first line. Setting this to true can help
     * with vertical alignment problems. */
    private boolean useAscender;

    /**
     * Indicates that the largest descender height should be added to the height of
     * the last line (so characters like y don't dip into the border). */
    private boolean useDescender;

    /**
     * Adjusts the cell contents to compensate for border widths.
     */
    private boolean useBorderPadding;

    private final int verticalAlignment;

    private PdfLine firstLine;
    private PdfLine lastLine;

    // constructors

    /**
     * Constructs a <CODE>PdfCell</CODE>-object.
     *
     * @param	cell		the original <CODE>Cell</CODE>
     * @param	rownumber	the number of the <CODE>Row</CODE> the <CODE>Cell</CODE> was in.
     * @param	left		the left border of the <CODE>PdfCell</CODE>
     * @param	right		the right border of the <CODE>PdfCell</CODE>
     * @param	top			the top border of the <CODE>PdfCell</CODE>
     * @param	cellspacing	the cellspacing of the <CODE>Table</CODE>
     * @param	cellpadding	the cellpadding	of the <CODE>Table</CODE>
     */

    public PdfCell(final Cell cell, final int rownumber, float left, float right, final float top, final float cellspacing, final float cellpadding) {
        // constructs a Rectangle (the bottomvalue will be changed afterwards)
        super(left, top, right, top);
        // copying the other Rectangle attributes from class Cell
        cloneNonPositionParameters(cell);
        this.cellpadding = cellpadding;
        this.cellspacing = cellspacing;
        this.verticalAlignment = cell.getVerticalAlignment();
        this.useAscender = cell.isUseAscender();
        this.useDescender = cell.isUseDescender();
        this.useBorderPadding = cell.isUseBorderPadding();

        // initialization of some parameters
        PdfChunk chunk;
        Element element;
        PdfChunk overflow;
        this.lines = new ArrayList();
        this.images = new ArrayList();
        this.leading = cell.getLeading();
        final int alignment = cell.getHorizontalAlignment();
        left += cellspacing + cellpadding;
        right -= cellspacing + cellpadding;

        left += getBorderWidthInside(LEFT);
        right -= getBorderWidthInside(RIGHT);


        this.contentHeight = 0;

        this.rowspan = cell.getRowspan();

        ArrayList allActions;
        int aCounter;
        // we loop over all the elements of the cell
        for (final Iterator i = cell.getElements(); i.hasNext();) {
            element = (Element) i.next();
            switch (element.type()) {
                case Element.JPEG:
                case Element.JPEG2000:
                case Element.JBIG2:
                case Element.IMGRAW:
                case Element.IMGTEMPLATE:
                    addImage((Image) element, left, right, 0.4f * this.leading, alignment); //
                    break;
                    // if the element is a list
                case Element.LIST:
                    if (this.line != null && this.line.size() > 0) {
                        this.line.resetAlignment();
                        addLine(this.line);
                    }
                    // we loop over all the listitems
                    addList((List)element, left, right, alignment);
                    this.line = new PdfLine(left, right, alignment, this.leading);
                    break;
                    // if the element is something else
                default:
                    allActions = new ArrayList();
                    processActions(element, null, allActions);
                    aCounter = 0;

                    float currentLineLeading = this.leading;
                    float currentLeft = left;
                    float currentRight = right;
                    if (element instanceof Phrase) {
                        currentLineLeading = ((Phrase) element).getLeading();
                    }
                    if (element instanceof Paragraph) {
                        final Paragraph p = (Paragraph) element;
                        currentLeft += p.getIndentationLeft();
                        currentRight -= p.getIndentationRight();
                    }
                    if (this.line == null) {
                        this.line = new PdfLine(currentLeft, currentRight, alignment, currentLineLeading);
                    }
                    // we loop over the chunks
                    final ArrayList chunks = element.getChunks();
                    if (chunks.isEmpty()) {
                       addLine(this.line); // add empty line - all cells need some lines even if they are empty
                       this.line = new PdfLine(currentLeft, currentRight, alignment, currentLineLeading);
                    }
                    else {
                        for (final Iterator j = chunks.iterator(); j.hasNext();) {
                            final Chunk c = (Chunk) j.next();
                            chunk = new PdfChunk(c, (PdfAction) allActions.get(aCounter++));
                            while ((overflow = this.line.add(chunk)) != null) {
                                addLine(this.line);
                                this.line = new PdfLine(currentLeft, currentRight, alignment, currentLineLeading);
                                chunk = overflow;
                            }
                        }
                    }
                    // if the element is a paragraph, section or chapter, we reset the alignment and add the line
                    switch (element.type()) {
                        case Element.PARAGRAPH:
                        case Element.SECTION:
                        case Element.CHAPTER:
                            this.line.resetAlignment();
                            flushCurrentLine();
                    }
            }
        }
        flushCurrentLine();
        if (this.lines.size() > cell.getMaxLines()) {
            while (this.lines.size() > cell.getMaxLines()) {
                removeLine(this.lines.size() - 1);
            }
            if (cell.getMaxLines() > 0) {
                final String more = cell.getShowTruncation();
                if (more != null && more.length() > 0) {
                    // Denote that the content has been truncated
                    this.lastLine = (PdfLine) this.lines.get(this.lines.size() - 1);
                    if (this.lastLine.size() >= 0) {
                        final PdfChunk lastChunk = this.lastLine.getChunk(this.lastLine.size() - 1);
                        final float moreWidth = new PdfChunk(more, lastChunk).width();
                        while (lastChunk.toString().length() > 0 && lastChunk.width() + moreWidth > right - left) {
                            // Remove characters to leave room for the 'more' indicator
                            lastChunk.setValue(lastChunk.toString().substring(0, lastChunk.length() - 1));
                        }
                        lastChunk.setValue(lastChunk.toString() + more);
                    } else {
                        this.lastLine.add(new PdfChunk(new Chunk(more), null));
                    }
                }
            }
        }
        // we set some additional parameters
        if (this.useDescender && this.lastLine != null) {
            this.contentHeight -= this.lastLine.getDescender();
        }

        // adjust first line height so that it touches the top
        if (!this.lines.isEmpty()) {
            this.firstLine = (PdfLine) this.lines.get(0);
            final float firstLineRealHeight = firstLineRealHeight();
            this.contentHeight -= this.firstLine.height();
            this.firstLine.height = firstLineRealHeight;
            this.contentHeight += firstLineRealHeight;
        }

        float newBottom = top - this.contentHeight - 2f * cellpadding() - 2f * cellspacing();
        newBottom -= getBorderWidthInside(TOP) + getBorderWidthInside(BOTTOM);
        setBottom(newBottom);

        this.rownumber = rownumber;
    }

    private void addList(final List list, final float left, final float right, final int alignment) {
        PdfChunk chunk;
        PdfChunk overflow;
        final ArrayList allActions = new ArrayList();
        processActions(list, null, allActions);
        int aCounter = 0;
        for (final Iterator it = list.getItems().iterator(); it.hasNext();) {
            final Element ele = (Element)it.next();
            switch (ele.type()) {
                case Element.LISTITEM:
                    final ListItem item = (ListItem)ele;
                    this.line = new PdfLine(left + item.getIndentationLeft(), right, alignment, item.getLeading());
                    this.line.setListItem(item);
                    for (final Iterator j = item.getChunks().iterator(); j.hasNext();) {
                        chunk = new PdfChunk((Chunk) j.next(), (PdfAction) allActions.get(aCounter++));
                        while ((overflow = this.line.add(chunk)) != null) {
                            addLine(this.line);
                            this.line = new PdfLine(left + item.getIndentationLeft(), right, alignment, item.getLeading());
                            chunk = overflow;
                        }
                        this.line.resetAlignment();
                        addLine(this.line);
                        this.line = new PdfLine(left + item.getIndentationLeft(), right, alignment, this.leading);
                    }
                    break;
                case Element.LIST:
                    final List sublist = (List)ele;
                    addList(sublist, left + sublist.getIndentationLeft(), right, alignment);
                    break;
            }
        }
    }

    // overriding of the Rectangle methods


    /**
     * Sets the bottom of the Rectangle and determines the proper {link #verticalOffset}
     * to appropriately align the contents vertically.
     * @param value
     */
    @Override
	public void setBottom(final float value) {
        super.setBottom(value);
        final float firstLineRealHeight = firstLineRealHeight();

        final float totalHeight = this.ury - value; // can't use top (already compensates for cellspacing)
        float nonContentHeight = cellpadding() * 2f + cellspacing() * 2f;
        nonContentHeight += getBorderWidthInside(TOP) + getBorderWidthInside(BOTTOM);

        final float interiorHeight = totalHeight - nonContentHeight;
        float extraHeight = 0.0f;

        switch (this.verticalAlignment) {
            case Element.ALIGN_BOTTOM:
                extraHeight = interiorHeight - this.contentHeight;
                break;
            case Element.ALIGN_MIDDLE:
                extraHeight = (interiorHeight - this.contentHeight) / 2.0f;
                break;
            default:    // ALIGN_TOP
                extraHeight = 0f;
        }

        extraHeight += cellpadding() + cellspacing();
        extraHeight += getBorderWidthInside(TOP);
        if (this.firstLine != null) {
            this.firstLine.height = firstLineRealHeight + extraHeight;
        }
    }

	/**
     * Returns the lower left x-coordinate.
     *
     * @return		the lower left x-coordinate
     */

    @Override
	public float getLeft() {
        return super.getLeft(this.cellspacing);
    }

	/**
     * Returns the upper right x-coordinate.
     *
     * @return		the upper right x-coordinate
     */

    @Override
	public float getRight() {
        return super.getRight(this.cellspacing);
    }

	/**
     * Returns the upper right y-coordinate.
     *
     * @return		the upper right y-coordinate
     */

    @Override
	public float getTop() {
        return super.getTop(this.cellspacing);
    }

	/**
     * Returns the lower left y-coordinate.
     *
     * @return		the lower left y-coordinate
     */

    @Override
	public float getBottom() {
        return super.getBottom(this.cellspacing);
    }

    // methods

    private void addLine(final PdfLine line) {
        this.lines.add(line);
        this.contentHeight += line.height();
        this.lastLine = line;
        this.line = null;
    }

    private PdfLine removeLine(final int index) {
        final PdfLine oldLine = (PdfLine) this.lines.remove(index);
        this.contentHeight -= oldLine.height();
        if (index == 0) {
            if (!this.lines.isEmpty()) {
                this.firstLine = (PdfLine) this.lines.get(0);
                final float firstLineRealHeight = firstLineRealHeight();
                this.contentHeight -= this.firstLine.height();
                this.firstLine.height = firstLineRealHeight;
                this.contentHeight += firstLineRealHeight;
            }
        }
        return oldLine;
    }

    private void flushCurrentLine() {
        if (this.line != null && this.line.size() > 0) {
            addLine(this.line);
        }
    }

    /**
     * Calculates what the height of the first line should be so that the content will be
     * flush with the top.  For text, this is the height of the ascender.  For an image,
     * it is the actual height of the image.
     * @return the real height of the first line
     */
    private float firstLineRealHeight() {
        float firstLineRealHeight = 0f;
        if (this.firstLine != null) {
            final PdfChunk chunk = this.firstLine.getChunk(0);
            if (chunk != null) {
                final Image image = chunk.getImage();
                if (image != null) {
                    firstLineRealHeight = this.firstLine.getChunk(0).getImage().getScaledHeight();
                } else {
                    firstLineRealHeight = this.useAscender ? this.firstLine.getAscender() : this.leading;
                }
            }
        }
        return firstLineRealHeight;
    }

    /**
     * Gets the amount of the border for the specified side that is inside the Rectangle.
     * For non-variable width borders this is only 1/2 the border width on that side.  This
     * always returns 0 if {@link #useBorderPadding} is false;
     * @param side the side to check.  One of the side constants in {@link com.lowagie.text.Rectangle}
     * @return the borderwidth inside the cell
     */
    private float getBorderWidthInside(final int side) {
        float width = 0f;
        if (this.useBorderPadding) {
            switch (side) {
                case Rectangle.LEFT:
                    width = getBorderWidthLeft();
                    break;

                case Rectangle.RIGHT:
                    width = getBorderWidthRight();
                    break;

                case Rectangle.TOP:
                    width = getBorderWidthTop();
                    break;

                default:    // default and BOTTOM
                    width = getBorderWidthBottom();
                    break;
            }
            // non-variable (original style) borders overlap the rectangle (only 1/2 counts)
            if (!isUseVariableBorders()) {
                width = width / 2f;
            }
        }
        return width;
    }


    /**
     * Adds an image to this Cell.
     *
     * @param i           the image to add
     * @param left        the left border
     * @param right       the right border
     * @param extraHeight extra height to add above image
     * @param alignment   horizontal alignment (constant from Element class)
     * @return the height of the image
     */

    private float addImage(final Image i, float left, float right, final float extraHeight, final int alignment) {
        final Image image = Image.getInstance(i);
        if (image.getScaledWidth() > right - left) {
            image.scaleToFit(right - left, Float.MAX_VALUE);
        }
        flushCurrentLine();
        if (this.line == null) {
            this.line = new PdfLine(left, right, alignment, this.leading);
        }
        final PdfLine imageLine = this.line;

        // left and right in chunk is relative to the start of the line
        right = right - left;
        left = 0f;

        if ((image.getAlignment() & Image.RIGHT) == Image.RIGHT) {
            left = right - image.getScaledWidth();
        } else if ((image.getAlignment() & Image.MIDDLE) == Image.MIDDLE) {
            left = left + (right - left - image.getScaledWidth()) / 2f;
        }
        final Chunk imageChunk = new Chunk(image, left, 0);
        imageLine.add(new PdfChunk(imageChunk, null));
        addLine(imageLine);
        return imageLine.height();
    }

    /**
     * Gets the lines of a cell that can be drawn between certain limits.
     * <P>
     * Remark: all the lines that can be drawn are removed from the object!
     *
     * @param	top		the top of the part of the table that can be drawn
     * @param	bottom	the bottom of the part of the table that can be drawn
     * @return	an <CODE>ArrayList</CODE> of <CODE>PdfLine</CODE>s
     */

    public ArrayList getLines(final float top, final float bottom) {
        float lineHeight;
        float currentPosition = Math.min(getTop(), top);
        setTop(currentPosition + this.cellspacing);
        final ArrayList result = new ArrayList();

        // if the bottom of the page is higher than the top of the cell: do nothing
        if (getTop() < bottom) {
            return result;
        }

        // we loop over the lines
        int size = this.lines.size();
        boolean aboveBottom = true;
        for (int i = 0; i < size && aboveBottom; i++) {
            this.line = (PdfLine) this.lines.get(i);
            lineHeight = this.line.height();
            currentPosition -= lineHeight;
            // if the currentPosition is higher than the bottom, we add the line to the result
            if (currentPosition > bottom + this.cellpadding + getBorderWidthInside(BOTTOM)) {
                result.add(this.line);
            } else {
                aboveBottom = false;
            }
        }
        // if the bottom of the cell is higher than the bottom of the page, the cell is written, so we can remove all lines
        float difference = 0f;
        if (!this.header) {
            if (aboveBottom) {
                this.lines = new ArrayList();
                this.contentHeight = 0f;
            } else {
                size = result.size();
                for (int i = 0; i < size; i++) {
                    this.line = removeLine(0);
                    difference += this.line.height();
                }
            }
        }
        if (difference > 0) {
            Image image;
            for (final Iterator i = this.images.iterator(); i.hasNext();) {
                image = (Image) i.next();
                image.setAbsolutePosition(image.getAbsoluteX(), image.getAbsoluteY() - difference - this.leading);
            }
        }
        return result;
    }

    /**
     * Gets the images of a cell that can be drawn between certain limits.
     * <P>
     * Remark: all the lines that can be drawn are removed from the object!
     *
     * @param	top		the top of the part of the table that can be drawn
     * @param	bottom	the bottom of the part of the table that can be drawn
     * @return	an <CODE>ArrayList</CODE> of <CODE>Image</CODE>s
     */

    public ArrayList getImages(float top, final float bottom) {

        // if the bottom of the page is higher than the top of the cell: do nothing
        if (getTop() < bottom) {
            return new ArrayList();
        }
        top = Math.min(getTop(), top);
        // initializations
        Image image;
        float height;
        final ArrayList result = new ArrayList();
        // we loop over the images
        for (final Iterator i = this.images.iterator(); i.hasNext() && !this.header;) {
            image = (Image) i.next();
            height = image.getAbsoluteY();
            // if the currentPosition is higher than the bottom, we add the line to the result
            if (top - height > bottom + this.cellpadding) {
                image.setAbsolutePosition(image.getAbsoluteX(), top - height);
                result.add(image);
                i.remove();
            }
        }
        return result;
    }

    /**
     * Checks if this cell belongs to the header of a <CODE>PdfTable</CODE>.
     *
     * @return	<CODE>void</CODE>
     */

    boolean isHeader() {
        return this.header;
    }

    /**
     * Indicates that this cell belongs to the header of a <CODE>PdfTable</CODE>.
     */

    void setHeader() {
        this.header = true;
    }

    /**
     * Checks if the cell may be removed.
     * <P>
     * Headers may always be removed, even if they are drawn only partially:
     * they will be repeated on each following page anyway!
     *
     * @return	<CODE>true</CODE> if all the lines are already drawn; <CODE>false</CODE> otherwise.
     */

    boolean mayBeRemoved() {
        return this.header || this.lines.isEmpty() && this.images.isEmpty();
    }



    /**
     * Returns the total height of all the lines in the cell.
     *
     * @return	a value
     */
    private float remainingLinesHeight() {
        if (this.lines.isEmpty()) {
			return 0;
		}
        float result = 0;
        final int size = this.lines.size();
        PdfLine line;
        for (int i = 0; i < size; i++) {
            line = (PdfLine) this.lines.get(i);
            result += line.height();
        }
        return result;
    }

    /**
     * Returns the height needed to draw the remaining text.
     *
     * @return a height
     */

    public float remainingHeight() {
        float result = 0f;
        for (final Iterator i = this.images.iterator(); i.hasNext();) {
            final Image image = (Image) i.next();
            result += image.getScaledHeight();
        }
        return remainingLinesHeight() + this.cellspacing + 2 * this.cellpadding + result;
    }

    // methods to retrieve membervariables



    /**
     * Gets the number of the row this cell is in..
     *
     * @return	a number
     */

    int rownumber() {
        return this.rownumber;
    }

    /**
     * Gets the rowspan of a cell.
     *
     * @return	the rowspan of the cell
     */

    public int rowspan() {
        return this.rowspan;
    }

    /**
     * Gets the cellspacing of a cell.
     *
     * @return	a value
     */

    private float cellspacing() {
        return this.cellspacing;
    }

    /**
     * Gets the cellpadding of a cell..
     *
     * @return	a value
     */

    private float cellpadding() {
        return this.cellpadding;
    }

    /**
     * Processes all actions contained in the cell.
     * @param element	an element in the cell
     * @param action	an action that should be coupled to the cell
     * @param allActions
     */

    private void processActions(final Element element, PdfAction action, final ArrayList allActions) {
        if (element.type() == Element.ANCHOR) {
            final String url = ((Anchor) element).getReference();
            if (url != null) {
                action = new PdfAction(url);
            }
        }
        Iterator i;
        switch (element.type()) {
            case Element.PHRASE:
            case Element.SECTION:
            case Element.ANCHOR:
            case Element.CHAPTER:
            case Element.LISTITEM:
            case Element.PARAGRAPH:
                for (i = ((ArrayList) element).iterator(); i.hasNext();) {
                    processActions((Element) i.next(), action, allActions);
                }
                break;
            case Element.CHUNK:
                allActions.add(action);
                break;
            case Element.LIST:
                for (i = ((List) element).getItems().iterator(); i.hasNext();) {
                    processActions((Element) i.next(), action, allActions);
                }
                break;
            default:
                int n = element.getChunks().size();
                while (n-- > 0) {
					allActions.add(action);
				}
                break;
        }
    }

    /**
     * This is the number of the group the cell is in.
     */
    private int groupNumber;

    /**
     * Gets the number of the group this cell is in..
     *
     * @return	a number
     */

    public int getGroupNumber() {
        return this.groupNumber;
    }

    /**
     * Sets the group number.
     * @param number
     */

    void setGroupNumber(final int number) {
        this.groupNumber = number;
    }

    /**
     * Gets a Rectangle that is altered to fit on the page.
     *
     * @param	top		the top position
     * @param	bottom	the bottom position
     * @return	a <CODE>Rectangle</CODE>
     */

    @Override
	public Rectangle rectangle(final float top, final float bottom) {
        final Rectangle tmp = new Rectangle(getLeft(), getBottom(), getRight(), getTop());
        tmp.cloneNonPositionParameters(this);
        if (getTop() > top) {
            tmp.setTop(top);
            tmp.setBorder(this.border - (this.border & TOP));
        }
        if (getBottom() < bottom) {
            tmp.setBottom(bottom);
            tmp.setBorder(this.border - (this.border & BOTTOM));
        }
        return tmp;
    }

    /**
     * Sets the value of useAscender.
     * @param use use ascender height if true
     */
    public void setUseAscender(final boolean use) {
        this.useAscender = use;
    }

    /**
     * Gets the value of useAscender
     * @return useAscender
     */
    public boolean isUseAscender() {
        return this.useAscender;
    }

    /**
     * Sets the value of useDescender.
     * @param use use descender height if true
     */
    public void setUseDescender(final boolean use) {
        this.useDescender = use;
    }

    /**
     * gets the value of useDescender
     * @return useDescender
     */
    public boolean isUseDescender() {
        return this.useDescender;
    }

    /**
     * Sets the value of useBorderPadding.
     * @param use adjust layout for borders if true
     */
    public void setUseBorderPadding(final boolean use) {
        this.useBorderPadding = use;
    }

    /**
     * Gets the value of useBorderPadding.
     * @return useBorderPadding
     */
    public boolean isUseBorderPadding() {
        return this.useBorderPadding;
    }

}
