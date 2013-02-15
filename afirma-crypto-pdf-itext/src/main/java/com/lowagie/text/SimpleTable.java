/*
 * $Id: SimpleTable.java 3752 2009-03-04 18:02:40Z blowagie $
 *
 * Copyright 2005 by Bruno Lowagie.
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
 * of this file under either the MPL or the GNU LIBRARY GENERAL PUBLIC LICENSE
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the MPL as stated above or under the terms of the GNU
 * Library General Public License as published by the Free Software Foundation;
 * either version 2 of the License, or any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU LIBRARY GENERAL PUBLIC LICENSE for more
 * details.
 *
 * If you didn't download this code from the following link, you should check if
 * you aren't using an obsolete version:
 * http://www.lowagie.com/iText/
 */
package com.lowagie.text;

import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPTable;
import com.lowagie.text.pdf.PdfPTableEvent;

/**
 * Rectangle that can be used for Cells.
 * This Rectangle is padded and knows how to draw itself in a PdfPTable or PdfPcellEvent.
 */
public class SimpleTable extends Rectangle implements PdfPTableEvent, TextElementArray {

	/** the content of a Table. */
	private final ArrayList content = new ArrayList();
	/** the width of the Table. */
	private float width = 0f;
	/** the widthpercentage of the Table. */
	private float widthpercentage = 0f;
	/** the spacing of the Cells. */
	private float cellspacing;
	/** the padding of the Cells. */
	private float cellpadding;
	/** the alignment of the table. */
	private int alignment;

	/**
	 * A RectangleCell is always constructed without any dimensions.
	 * Dimensions are defined after creation.
	 */
	public SimpleTable() {
		super(0f, 0f, 0f, 0f);
		setBorder(BOX);
		setBorderWidth(2f);
	}

	/**
	 * Adds content to this object.
	 * @param element
	 * @throws BadElementException
	 */
	private void addElement(final SimpleCell element) throws BadElementException {
		if(!element.isCellgroup()) {
			throw new BadElementException("You can't add cells to a table directly, add them to a row first.");
		}
		this.content.add(element);
	}



	/**
	 * Creates a PdfPTable object based on this TableAttributes object.
	 * @return a com.lowagie.text.pdf.PdfPTable object
	 * @throws DocumentException
	 */
	public PdfPTable createPdfPTable() throws DocumentException {
		if (this.content.isEmpty()) {
			throw new BadElementException("Trying to create a table without rows.");
		}
		SimpleCell row = (SimpleCell)this.content.get(0);
		SimpleCell cell;
		int columns = 0;
		for (final Iterator i = row.getContent().iterator(); i.hasNext(); ) {
			cell = (SimpleCell)i.next();
			columns += cell.getColspan();
		}
		final float[] widths = new float[columns];
		final float[] widthpercentages = new float[columns];
		final PdfPTable table = new PdfPTable(columns);
		table.setTableEvent(this);
		table.setHorizontalAlignment(this.alignment);
		int pos;
		for (final Iterator rows = this.content.iterator(); rows.hasNext(); ) {
			row = (SimpleCell)rows.next();
			pos = 0;
			for (final Iterator cells = row.getContent().iterator(); cells.hasNext(); ) {
				cell = (SimpleCell)cells.next();
				if (Float.isNaN(cell.getSpacing_left()))	{
					cell.setSpacing_left(this.cellspacing / 2f);
				}
				if (Float.isNaN(cell.getSpacing_right()))	{
					cell.setSpacing_right(this.cellspacing / 2f);
				}
				if (Float.isNaN(cell.getSpacing_top()))	{
					cell.setSpacing_top(this.cellspacing / 2f);
				}
				if (Float.isNaN(cell.getSpacing_bottom()))	{
					cell.setSpacing_bottom(this.cellspacing / 2f);
				}
				cell.setPadding(this.cellpadding);
				table.addCell(cell.createPdfPCell(row));
				if (cell.getColspan() == 1) {
					if (cell.getWidth() > 0) {
						widths[pos] = cell.getWidth();
					}
					if (cell.getWidthpercentage() > 0) {
						widthpercentages[pos] = cell.getWidthpercentage();
					}
				}
				pos += cell.getColspan();
			}
		}
		float sumWidths = 0f;
		for(int i = 0; i < columns; i++) {
			if (widths[i] == 0) {
				sumWidths = 0;
				break;
			}
			sumWidths += widths[i];
		}
		if (sumWidths > 0) {
			table.setTotalWidth(sumWidths);
			table.setWidths(widths);
		}
		else {
			for(int i = 0; i < columns; i++) {
				if (widthpercentages[i] == 0) {
					sumWidths = 0;
					break;
				}
				sumWidths += widthpercentages[i];
			}
			if (sumWidths > 0) {
				table.setWidths(widthpercentages);
			}
		}
		if (this.width > 0) {
			table.setTotalWidth(this.width);
		}
		if (this.widthpercentage > 0) {
			table.setWidthPercentage(this.widthpercentage);
		}
		return table;
	}

	/**
     * @see com.lowagie.text.pdf.PdfPTableEvent#tableLayout(com.lowagie.text.pdf.PdfPTable, float[][], float[], int, int, com.lowagie.text.pdf.PdfContentByte[])
 	 */
	@Override
	public void tableLayout(final PdfPTable table, final float[][] widths, final float[] heights, final int headerRows, final int rowStart, final PdfContentByte[] canvases) {
		final float[] width = widths[0];
		final Rectangle rect = new Rectangle(width[0], heights[heights.length - 1], width[width.length - 1], heights[0]);
		rect.cloneNonPositionParameters(this);
        final int bd = rect.getBorder();
        rect.setBorder(Rectangle.NO_BORDER);
		canvases[PdfPTable.BACKGROUNDCANVAS].rectangle(rect);
        rect.setBorder(bd);
		rect.setBackgroundColor(null);
		canvases[PdfPTable.LINECANVAS].rectangle(rect);
	}

	/**
	 * @return Returns the cellpadding.
	 */
	public float getCellpadding() {
		return this.cellpadding;
	}
	/**
	 * @param cellpadding The cellpadding to set.
	 */
	public void setCellpadding(final float cellpadding) {
		this.cellpadding = cellpadding;
	}
	/**
	 * @return Returns the cellspacing.
	 */
	public float getCellspacing() {
		return this.cellspacing;
	}
	/**
	 * @param cellspacing The cellspacing to set.
	 */
	public void setCellspacing(final float cellspacing) {
		this.cellspacing = cellspacing;
	}

	/**
	 * @return Returns the alignment.
	 */
	public int getAlignment() {
		return this.alignment;
	}
	/**
	 * @param alignment The alignment to set.
	 */
	public void setAlignment(final int alignment) {
		this.alignment = alignment;
	}
	/**
	 * @return Returns the width.
	 */
	@Override
	public float getWidth() {
		return this.width;
	}
	/**
	 * @param width The width to set.
	 */
	public void setWidth(final float width) {
		this.width = width;
	}
	/**
	 * @return Returns the widthpercentage.
	 */
	public float getWidthpercentage() {
		return this.widthpercentage;
	}
	/**
	 * @param widthpercentage The widthpercentage to set.
	 */
	public void setWidthpercentage(final float widthpercentage) {
		this.widthpercentage = widthpercentage;
	}
	/**
	 * @see com.lowagie.text.Element#type()
	 */
	@Override
	public int type() {
		return Element.TABLE;
	}

	/**
	 * @see com.lowagie.text.Element#isNestable()
	 * @since	iText 2.0.8
	 */
	@Override
	public boolean isNestable() {
		return true;
	}

	/**
	 * @see com.lowagie.text.TextElementArray#add(java.lang.Object)
	 */
	@Override
	public boolean add(final Object o) {
		try {
			addElement((SimpleCell)o);
			return true;
		}
		catch(final ClassCastException e) {
			return false;
		}
		catch(final BadElementException e) {
			throw new ExceptionConverter(e);
		}
	}
}