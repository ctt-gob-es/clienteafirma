/*
 * $Id: Cell.java 3373 2008-05-12 16:21:24Z xlv $
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

import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.pdf.PdfPCell;

/**
 * A <CODE>Cell</CODE> is a <CODE>Rectangle</CODE> containing other
 * <CODE>Element</CODE>s.
 * <P>
 * A <CODE>Cell</CODE> must be added to a <CODE>Table</CODE>.
 * The <CODE>Table</CODE> will place the <CODE>Cell</CODE> in
 * a <CODE>Row</CODE>.
 * <P>
 * Example:
 * <BLOCKQUOTE><PRE>
 * Table table = new Table(3);
 * table.setBorderWidth(1);
 * table.setBorderColor(new Color(0, 0, 255));
 * table.setCellpadding(5);
 * table.setCellspacing(5);
 * <STRONG>Cell cell = new Cell("header");</STRONG>
 * <STRONG>cell.setHeader(true);</STRONG>
 * <STRONG>cell.setColspan(3);</STRONG>
 * table.addCell(cell);
 * <STRONG>cell = new Cell("example cell with colspan 1 and rowspan 2");</STRONG>
 * <STRONG>cell.setRowspan(2);</STRONG>
 * <STRONG>cell.setBorderColor(new Color(255, 0, 0));</STRONG>
 * table.addCell(cell);
 * table.addCell("1.1");
 * table.addCell("2.1");
 * table.addCell("1.2");
 * table.addCell("2.2");
 * </PRE></BLOCKQUOTE>
 *
 * @see		Rectangle
 * @see		Element
 * @see		Table
 * @see		Row
 */

public class Cell extends Rectangle implements TextElementArray {

	// membervariables

	/**
	 * The <CODE>ArrayList</CODE> of <CODE>Element</CODE>s
	 * that are part of the content of the Cell.
	 */
	private ArrayList arrayList = null;

	/** The horizontal alignment of the cell content. */
	private int horizontalAlignment = Element.ALIGN_UNDEFINED;

	/** The vertical alignment of the cell content. */
	private int verticalAlignment = Element.ALIGN_UNDEFINED;

	/**
	 * The width of the cell as a String.
	 * It can be an absolute value "100" or a percentage "20%".
	 */
	private float width;
	private boolean percentage = false;

	/** The colspan of the cell. */
	private int colspan = 1;

	/** The rowspan of the cell. */
	private int rowspan = 1;

	/** The leading of the content inside the cell. */
	private float leading = Float.NaN;

	/** Is this <CODE>Cell</CODE> a header? */
	private boolean header;

	/**
	 * Maximum number of lines allowed in the cell.
	 * The default value of this property is not to limit the maximum number of lines
	 * (contributed by dperezcar@fcc.es)
	 */
	private int maxLines = Integer.MAX_VALUE;

	/**
	 * If a truncation happens due to the maxLines property, then this text will
	 * be added to indicate a truncation has happened.
	 * Default value is null, and means avoiding marking the truncation.
	 * A useful value of this property could be e.g. "..."
	 * (contributed by dperezcar@fcc.es)
	 */
	private String showTruncation;

    /**
     * Indicates that the largest ascender height should be used to determine the
     * height of the first line.  Note that this only has an effect when rendered
     * to PDF.  Setting this to true can help with vertical alignment problems.
     */
	private boolean useAscender = false;

    /**
     * Indicates that the largest descender height should be added to the height of
     * the last line (so characters like y don't dip into the border).   Note that
     * this only has an effect when rendered to PDF.
     */
	private boolean useDescender = false;

    /**
     * Adjusts the cell contents to compensate for border widths.  Note that
     * this only has an effect when rendered to PDF.
     */
    private boolean useBorderPadding;

	/** Does this <CODE>Cell</CODE> force a group change? */
    private boolean groupChange = true;

	// constructors

    /** Constructs an empty <CODE>Cell</CODE>. */
	public Cell() {
		// creates a Rectangle with BY DEFAULT a border of 0.5
		super(0, 0, 0, 0);
		setBorder(UNDEFINED);
		setBorderWidth(0.5f);
		// initializes the arraylist
		this.arrayList = new ArrayList();
	}

	/**
	 * Constructs an empty <CODE>Cell</CODE> (for internal use only).
	 *
	 * @param   dummy   a dummy value
	 */
	Cell(final boolean dummy) {
		this();
		this.arrayList.add(new Paragraph(0));
	}

	/**
	 * Constructs a <CODE>Cell</CODE> with a certain <CODE>Element</CODE>.<p>
	 * if the element is a <CODE>ListItem</CODE>, <CODE>Row</CODE> or
	 * <CODE>Cell</CODE>, an exception will be thrown.
	 *
	 * @param	element		the element
	 * @throws	BadElementException when the creator was called with a <CODE>ListItem</CODE>, <CODE>Row</CODE> or <CODE>Cell</CODE>
	 */
	Cell(final Element element) throws BadElementException {
		this();
 		if(element instanceof Phrase) {
			setLeading(((Phrase)element).getLeading());
		}
		addElement(element);
	}

	// implementation of the Element-methods

	/**
	 * Processes the element by adding it (or the different parts) to an
	 * <CODE>ElementListener</CODE>.
	 *
	 * @param	listener	an <CODE>ElementListener</CODE>
	 * @return	<CODE>true</CODE> if the element was processed successfully
	 */
	@Override
	public boolean process(final ElementListener listener) {
		try {
			return listener.add(this);
		}
		catch(final DocumentException de) {
			return false;
		}
	}

	/**
	 * Gets the type of the text element.
	 *
	 * @return	a type
	 */
	@Override
	public int type() {
		return Element.CELL;
	}

	/**
	 * Gets all the chunks in this element.
	 *
	 * @return	an <CODE>ArrayList</CODE>
	 */
	@Override
	public ArrayList getChunks() {
		final ArrayList tmp = new ArrayList();
		for (final Iterator i = this.arrayList.iterator(); i.hasNext(); ) {
			tmp.addAll(((Element) i.next()).getChunks());
		}
		return tmp;
	}

	// Getters and setters

	/**
     * Gets the horizontal alignment.
     *
     * @return	a value
     */
   	public int getHorizontalAlignment() {
   		return this.horizontalAlignment;
   	}

	/**
	 * Sets the horizontal alignment.
	 * @param	value	the new value
	 */
	public void setHorizontalAlignment(final int value) {
		this.horizontalAlignment = value;
	}

	/**
	 * Sets the alignment of this cell.
	 * This methods allows you to set the alignment as a String.
	 * @param	alignment		the new alignment as a <CODE>String</CODE>
	 */
	public void setHorizontalAlignment(final String alignment) {
		setHorizontalAlignment(ElementTags.alignmentValue(alignment));
	}

	/**
	 * Gets the vertical alignment.
	 * @return	a value
	 */
	public int getVerticalAlignment() {
		return this.verticalAlignment;
	}

	/**
	 * Sets the vertical alignment.
	 * @param	value	the new value
	 */
	public void setVerticalAlignment(final int value) {
		this.verticalAlignment = value;
	}

	/**
	 * Sets the alignment of this paragraph.
	 *
	 * @param	alignment		the new alignment as a <CODE>String</CODE>
	 */
	public void setVerticalAlignment(final String alignment) {
		setVerticalAlignment(ElementTags.alignmentValue(alignment));
	}

	/**
	 * Sets the width.
	 *
	 * @param	value	the new value
	 */
	public void setWidth(final float value) {
		this.width = value;
	}

	/**
	 * Sets the width.
	 * It can be an absolute value "100" or a percentage "20%"
	 *
	 * @param	value	the new value
	 */
	public void setWidth(String value) {
		if (value.endsWith("%")) {
			value = value.substring(0, value.length() - 1);
			this.percentage = true;
		}
		this.width = Integer.parseInt(value);
	}

	/**
	 * Gets the width.
	 */
	@Override
	public float getWidth() {
		return this.width;
	}

	/**
	 * Gets the width as a String.
	 *
	 * @return	a value
	 */
	public String getWidthAsString() {
		String w = String.valueOf(this.width);
		if (w.endsWith(".0")) {
			w = w.substring(0, w.length() - 2);
		}
		if (this.percentage) {
			w += "%";
		}
		return w;
	}

	/**
	 * Sets the colspan.
	 *
	 * @param	value	the new value
	 */
	public void setColspan(final int value) {
		this.colspan = value;
	}

	/**
	 * Gets the colspan.
	 * @return	a value
	 */
	public int getColspan() {
		return this.colspan;
	}

	/**
	 * Sets the rowspan.
	 *
	 * @param	value	the new value
	 */
	public void setRowspan(final int value) {
		this.rowspan = value;
	}

	/**
	 * Gets the rowspan.
	 * @return	a value
	 */
	public int getRowspan() {
		return this.rowspan;
	}

	/**
	 * Sets the leading.
	 *
	 * @param	value	the new value
	 */
	public void setLeading(final float value) {
		this.leading = value;
	}

	/**
	 * Gets the leading.
	 *
	 * @return	a value
	 */
	public float getLeading() {
		if (Float.isNaN(this.leading)) {
			return 16;
		}
		return this.leading;
	}

	/**
	 * Sets header.
	 *
	 * @param	value	the new value
	 */
	public void setHeader(final boolean value) {
		this.header = value;
	}

	/**
	 * Is this <CODE>Cell</CODE> a header?
	 *
	 * @return	a value
	 */
	public boolean isHeader() {
		return this.header;
	}

	/**
	 * Setter for maxLines
	 * @param value the maximum number of lines
	 */
	public void setMaxLines(final int value) {
		this.maxLines = value;
	}

	/**
	 * Getter for maxLines
	 * @return the maxLines value
	 */
	public int getMaxLines() {
		return this.maxLines;
	}

	/**
	 * Setter for showTruncation
	 * @param value	Can be null for avoiding marking the truncation.
	 */
	public void setShowTruncation(final String value) {
		this.showTruncation = value;
	}

	/**
	 * Getter for showTruncation
	 * @return the showTruncation value
	 */
	public String getShowTruncation() {
		return this.showTruncation;
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

	/**
	 * Does this <CODE>Cell</CODE> force a group change?
	 *
	 * @return	a value
	 */
	public boolean getGroupChange() {
		return this.groupChange;
	}

	/**
	 * Sets group change.
	 *
	 * @param	value	the new value
	 */
	public void setGroupChange(final boolean value) {
		this.groupChange = value;
	}

// arraylist stuff

	/**
	 * Gets the number of <CODE>Element</CODE>s in the Cell.
	 *
	 * @return	a <CODE>size</CODE>.
	 */
	private int size() {
		return this.arrayList.size();
	}

	/**
	 * Gets an iterator of <CODE>Element</CODE>s.
	 *
	 * @return	an <CODE>Iterator</CODE>.
	 */
	public Iterator getElements() {
		return this.arrayList.iterator();
	}

	/**
	 * Clears all the <CODE>Element</CODE>s of this <CODE>Cell</CODE>.
	 */
	private void clear() {
		this.arrayList.clear();
	}

	/**
	 * Checks if the <CODE>Cell</CODE> is empty.
	 *
	 * @return	<CODE>false</CODE> if there are non-empty <CODE>Element</CODE>s in the <CODE>Cell</CODE>.
	 */
	public boolean isEmpty() {
		switch(size()) {
			case 0:
				return true;
			case 1:
				final Element element = (Element) this.arrayList.get(0);
				switch (element.type()) {
					case Element.CHUNK:
						return ((Chunk) element).isEmpty();
					case Element.ANCHOR:
					case Element.PHRASE:
					case Element.PARAGRAPH:
						return ((Phrase) element).isEmpty();
					case Element.LIST:
						return ((List) element).isEmpty();
				}
			return false;
			default:
				return false;
		}
	}

	/**
	 * Makes sure there is at least 1 object in the Cell.
	 *
	 * Otherwise it might not be shown in the table.
	 */
	void fill() {
		if (size() == 0) {
			this.arrayList.add(new Paragraph(0));
		}
	}

	/**
	 * Checks if this <CODE>Cell</CODE> is a placeholder for a (nested) table.
	 *
	 * @return	true if the only element in this cell is a table
	 */
	public boolean isTable() {
		return size() == 1
			&& ((Element)this.arrayList.get(0)).type() == Element.TABLE;
	}

	/**
	 * Adds an element to this <CODE>Cell</CODE>.
	 * <P>
	 * Remark: you can't add <CODE>ListItem</CODE>s, <CODE>Row</CODE>s, <CODE>Cell</CODE>s,
	 * <CODE>JPEG</CODE>s, <CODE>GIF</CODE>s or <CODE>PNG</CODE>s to a <CODE>Cell</CODE>.
	 *
	 * @param element The <CODE>Element</CODE> to add
	 * @throws BadElementException if the method was called with a <CODE>ListItem</CODE>, <CODE>Row</CODE> or <CODE>Cell</CODE>
	 */
	private void addElement(final Element element) throws BadElementException {
		if (isTable()) {
			final Table table = (Table) this.arrayList.get(0);
			final Cell tmp = new Cell(element);
			tmp.setBorder(NO_BORDER);
			tmp.setColspan(table.getColumns());
			table.addCell(tmp);
			return;
		}
		switch(element.type()) {
			case Element.LISTITEM:
			case Element.ROW:
			case Element.CELL:
				throw new BadElementException("You can't add listitems, rows or cells to a cell.");
			case Element.LIST:
				final List list = (List)element;
				if (Float.isNaN(this.leading)) {
					setLeading(list.getTotalLeading());
				}
				if (list.isEmpty()) {
					return;
				}
				this.arrayList.add(element);
				return;
			case Element.ANCHOR:
			case Element.PARAGRAPH:
			case Element.PHRASE:
				final Phrase p = (Phrase)element;
				if (Float.isNaN(this.leading)) {
					setLeading(p.getLeading());
				}
				if (p.isEmpty()) {
					return;
				}
				this.arrayList.add(element);
				return;
			case Element.CHUNK:
				if (((Chunk) element).isEmpty()) {
					return;
				}
				this.arrayList.add(element);
				return;
			case Element.TABLE:
				final Table table = new Table(3);
				final float[] widths = new float[3];
				widths[1] = ((Table)element).getWidth();
				switch(((Table)element).getAlignment()) {
					case Element.ALIGN_LEFT:
						widths[0] = 0f;
						widths[2] = 100f - widths[1];
						break;
					case Element.ALIGN_CENTER:
						widths[0] = (100f - widths[1]) / 2f;
						widths[2] = widths[0];
						break;
					case Element.ALIGN_RIGHT:
						widths[0] = 100f - widths[1];
						widths[2] = 0f;
				}
				table.setWidths(widths);
				Cell tmp;
				if (this.arrayList.isEmpty()) {
					table.addCell(getDummyCell());
				}
				else {
					tmp = new Cell();
					tmp.setBorder(NO_BORDER);
					tmp.setColspan(3);
					for (final Iterator i = this.arrayList.iterator(); i.hasNext(); ) {
						tmp.add(i.next());
					}
					table.addCell(tmp);
				}
				tmp = new Cell();
				tmp.setBorder(NO_BORDER);
				table.addCell(tmp);
				table.insertTable((Table)element);
				tmp = new Cell();
				tmp.setBorder(NO_BORDER);
				table.addCell(tmp);
				table.addCell(getDummyCell());
				clear();
				this.arrayList.add(table);
				return;
			default:
				this.arrayList.add(element);
		}
	}

	/**
	 * Add an <CODE>Object</CODE> to this cell.
	 *
	 * @param o the object to add
	 * @return always <CODE>true</CODE>
	 */
	@Override
	public boolean add(final Object o) {
		try {
			this.addElement((Element) o);
			return true;
		}
		catch(final ClassCastException cce) {
			throw new ClassCastException("You can only add objects that implement the Element interface.");
		}
		catch(final BadElementException bee) {
			throw new ClassCastException(bee.getMessage());
		}
	}

	// helper methods

	/**
     * Get dummy cell used when merging inner tables.
     * @return a cell with colspan 3 and no border
     */
    private static Cell getDummyCell() {
        final Cell cell = new Cell(true);
        cell.setColspan(3);
        cell.setBorder(NO_BORDER);
        return cell;
	}

	/**
	 * Creates a PdfPCell based on this Cell object.
	 * @return a PdfPCell
	 * @throws BadElementException
	 */
	PdfPCell createPdfPCell() throws BadElementException {
		if (this.rowspan > 1) {
			throw new BadElementException("PdfPCells can't have a rowspan > 1");
		}
		if (isTable()) {
			return new PdfPCell(((Table)this.arrayList.get(0)).createPdfPTable());
		}
		final PdfPCell cell = new PdfPCell();
		cell.setVerticalAlignment(this.verticalAlignment);
		cell.setHorizontalAlignment(this.horizontalAlignment);
		cell.setColspan(this.colspan);
		cell.setUseBorderPadding(this.useBorderPadding);
		cell.setUseDescender(this.useDescender);
		cell.setLeading(getLeading(), 0);
		cell.cloneNonPositionParameters(this);
		cell.setNoWrap(getMaxLines() == 1);
		for (final Iterator i = getElements(); i.hasNext(); ) {
            Element e = (Element)i.next();
            if (e.type() == Element.PHRASE || e.type() == Element.PARAGRAPH) {
                final Paragraph p = new Paragraph((Phrase)e);
                p.setAlignment(this.horizontalAlignment);
                e = p;
            }
			cell.addElement(e);
		}
		return cell;
	}

	// unsupported Rectangle methods

	/**
	 * This method throws an <CODE>UnsupportedOperationException</CODE>.
	 * @return NA
	 */
	@Override
	public float getTop() {
		throw new UnsupportedOperationException("Dimensions of a Cell can't be calculated. See the FAQ.");
	}

	/**
	 * This method throws an <CODE>UnsupportedOperationException</CODE>.
	 * @return NA
	 */
	@Override
	public float getBottom() {
		throw new UnsupportedOperationException("Dimensions of a Cell can't be calculated. See the FAQ.");
	}

	/**
	 * This method throws an <CODE>UnsupportedOperationException</CODE>.
	 * @return NA
	 */
	@Override
	public float getLeft() {
		throw new UnsupportedOperationException("Dimensions of a Cell can't be calculated. See the FAQ.");
	}

	/**
	 * This method throws an <CODE>UnsupportedOperationException</CODE>.
	 * @return NA
	 */
	@Override
	public float getRight() {
		throw new UnsupportedOperationException("Dimensions of a Cell can't be calculated. See the FAQ.");
	}

	/**
	 * This method throws an <CODE>UnsupportedOperationException</CODE>.
	 * @param value NA
	 */
	public void setTop(final int value) {
		throw new UnsupportedOperationException("Dimensions of a Cell are attributed automagically. See the FAQ.");
	}

	/**
	 * This method throws an <CODE>UnsupportedOperationException</CODE>.
	 * @param value NA
	 */
	public void setBottom(final int value) {
		throw new UnsupportedOperationException("Dimensions of a Cell are attributed automagically. See the FAQ.");
	}

	/**
	 * This method throws an <CODE>UnsupportedOperationException</CODE>.
	 * @param value NA
	 */
	public void setLeft(final int value) {
		throw new UnsupportedOperationException("Dimensions of a Cell are attributed automagically. See the FAQ.");
	}

	/**
	 * This method throws an <CODE>UnsupportedOperationException</CODE>.
	 * @param value NA
	 */
	public void setRight(final int value) {
		throw new UnsupportedOperationException("Dimensions of a Cell are attributed automagically. See the FAQ.");
	}

}
