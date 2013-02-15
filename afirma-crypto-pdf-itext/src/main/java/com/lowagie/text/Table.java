/*
 * $Id: Table.java 3754 2009-03-04 19:05:20Z blowagie $
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
 *
 * Some methods in this class were contributed by Geert Poels, Kris Jespers and
 * Steve Ogryzek. Check the CVS repository.
 */

package com.lowagie.text;

import java.awt.Dimension;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;

/**
 * A <CODE>Table</CODE> is a <CODE>Rectangle</CODE> that contains <CODE>Cell</CODE>s,
 * ordered in some kind of matrix.
 * <P>
 * Tables that span multiple pages are cut into different parts automatically.
 * If you want a table header to be repeated on every page, you may not forget to
 * mark the end of the header section by using the method <CODE>endHeaders()</CODE>.
 * <P>
 * The matrix of a table is not necessarily an m x n-matrix. It can contain holes
 * or cells that are bigger than the unit. Believe me or not, but it took some serious
 * thinking to make this as user friendly as possible. I hope you will find the result
 * quite simple (I love simple solutions, especially for complex problems).
 * I didn't want it to be something as complex as the Java <CODE>GridBagLayout</CODE>.
 * <P>
 * Example:
 * <BLOCKQUOTE><PRE>
 * // Remark: You MUST know the number of columns when constructing a Table.
 * //         The number of rows is not important.
 * <STRONG>Table table = new Table(3);</STRONG>
 * <STRONG>table.setBorderWidth(1);</STRONG>
 * <STRONG>table.setBorderColor(new Color(0, 0, 255));</STRONG>
 * <STRONG>table.setPadding(5);</STRONG>
 * <STRONG>table.setSpacing(5);</STRONG>
 * Cell cell = new Cell("header");
 * cell.setHeader(true);
 * cell.setColspan(3);
 * <STRONG>table.addCell(cell);</STRONG>
 * <STRONG>table.endHeaders();</STRONG>
 * cell = new Cell("example cell with colspan 1 and rowspan 2");
 * cell.setRowspan(2);
 * cell.setBorderColor(new Color(255, 0, 0));
 * <STRONG>table.addCell(cell);</STRONG>
 * <STRONG>table.addCell("1.1");</STRONG>
 * <STRONG>table.addCell("2.1");</STRONG>
 * <STRONG>table.addCell("1.2");</STRONG>
 * <STRONG>table.addCell("2.2");</STRONG>
 * <STRONG>table.addCell("cell test1");</STRONG>
 * cell = new Cell("big cell");
 * cell.setRowspan(2);
 * cell.setColspan(2);
 * <STRONG>table.addCell(cell);</STRONG>
 * <STRONG>table.addCell("cell test2");</STRONG>
 * </PRE></BLOCKQUOTE>
 * The result of this code is a table:
 *      <TABLE ALIGN="Center" BORDER="1" BORDERCOLOR="#0000ff" CELLPADDING="5" CELLSPACING="5">
 *              <TR ALIGN="Left" VALIGN="Left">
 *                      <TH ALIGN="Left" COLSPAN="3" VALIGN="Left">
 *                              header
 *                      </TH>
 *              </TR>
 *              <TR ALIGN="Left" VALIGN="Left">
 *                      <TD ALIGN="Left" BORDERCOLOR="#ff0000" ROWSPAN="2" VALIGN="Left">
 *                              example cell with colspan 1 and rowspan 2
 *                      </TD>
 *                      <TD ALIGN="Left" VALIGN="Left">
 *                              1.1
 *                      </TD>
 *                      <TD ALIGN="Left" VALIGN="Left">
 *                              2.1
 *                      </TD>
 *              </TR>
 *              <TR ALIGN="Left" VALIGN="Left">
 *                      <TD ALIGN="Left" VALIGN="Left">
 *                              1.2
 *                      </TD>
 *                      <TD ALIGN="Left" VALIGN="Left">
 *                              2.2
 *                      </TD>
 *              </TR>
 *              <TR ALIGN="Left" VALIGN="Left">
 *                      <TD ALIGN="Left" VALIGN="Left">
 *                              cell test1
 *                      </TD>
 *                      <TD ALIGN="Left" COLSPAN="2" ROWSPAN="2" VALIGN="Left">
 *                              big cell
 *                      </TD>
 *              </TR>
 *              <TR ALIGN="Left" VALIGN="Left">
 *                      <TD ALIGN="Left" VALIGN="Left">
 *                              cell test2
 *                      </TD>
 *              </TR>
 *      </TABLE>
 *
 * @see         Rectangle
 * @see         Element
 * @see         Row
 * @see         Cell
 */

public class Table extends Rectangle implements LargeElement {

    // membervariables

    /** This is the number of columns in the <CODE>Table</CODE>. */
    private int columns;

    /** This is the list of <CODE>Row</CODE>s. */
    private ArrayList rows = new ArrayList();

    /** The current Position in the table. */
    private Point curPosition = new Point(0, 0);

    /** This Empty Cell contains the DEFAULT layout of each Cell added with the method addCell(String content). */
    private Cell defaultCell = new Cell(true);

    /** This is the number of the last row of the table headers. */
    private int lastHeaderRow = -1;

    /** This is the horizontal alignment. */
    private int alignment = Element.ALIGN_CENTER;

    /** This is cellpadding. */
    private float cellpadding;

    /** This is cellspacing. */
    private float cellspacing;

    /** This is the width of the table (in percent of the available space). */
    private float width = 80;

    /** Is the width a percentage (false) or an absolute width (true)? */
    private boolean locked = false;

    /** This is an array containing the widths (in percentages) of every column. */
    private float[] widths;

    /** Boolean to track if a table was inserted (to avoid unnecessary computations afterwards) */
    private boolean mTableInserted = false;

    /**
     * Boolean to automatically fill empty cells before a table is rendered
     *  (takes CPU so may be set to false in case of certainty)
     */
    private boolean autoFillEmptyCells = false;

    /** If true this table may not be split over two pages. */
    private boolean tableFitsPage = false;

    /** If true cells may not be split over two pages. */
    private boolean cellsFitPage = false;

    /** This is the offset of the table. */
    private float offset = Float.NaN;

    /** if you want to generate tables the old way, set this value to false. */
    private boolean convert2pdfptable = false;

    /**
     * Indicates if this is the first time the section was added.
     * @since	iText 2.0.8
     */
    private boolean notAddedYet = true;

    /**
     * Indicates if the PdfPTable is complete once added to the document.
     * @since	iText 2.0.8
     */
    private boolean complete = true;

    // constructors

    /**
     * Constructs a <CODE>Table</CODE> with a certain number of columns.
     *
     * @param       columns         The number of columns in the table
     * @throws      BadElementException if the creator was called with less than 1 column
     */
    public Table(final int columns) throws BadElementException {
        this(columns, 1);
    }

    /**
     * Constructs a <CODE>Table</CODE> with a certain number of columns
     * and a certain number of <CODE>Row</CODE>s.
     *
     * @param       columns         The number of columns in the table
     * @param       rows            The number of rows
     * @throws      BadElementException if the creator was called with less than 1 column
     */
    private Table(final int columns, final int rows) throws BadElementException {
        // a Rectangle is create with BY DEFAULT a border with a width of 1
        super(0, 0, 0, 0);
        setBorder(BOX);
        setBorderWidth(1);
        this.defaultCell.setBorder(BOX);

        // a table should have at least 1 column
        if (columns <= 0) {
            throw new BadElementException("A table should have at least 1 column.");
        }
        this.columns = columns;

        // a certain number of rows are created
        for (int i = 0; i < rows; i++) {
            this.rows.add(new Row(columns));
        }
        this.curPosition = new Point(0, 0);

        // the DEFAULT widths are calculated
        this.widths = new float[columns];
        final float width = 100f / columns;
        for (int i = 0; i < columns; i++) {
            this.widths[i] = width;
        }
    }

    // implementation of the Element-methods

    /**
     * Processes the element by adding it (or the different parts) to an
     * <CODE>ElementListener</CODE>.
     *
     * @param       listener        an <CODE>ElementListener</CODE>
     * @return <CODE>true</CODE> if the element was processed successfully
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
     * @return  a type
     */
    @Override
	public int type() {
        return Element.TABLE;
    }

    /**
     * Gets all the chunks in this element.
     *
     * @return  an <CODE>ArrayList</CODE>
     */

    @Override
	public ArrayList getChunks() {
        return new ArrayList();
    }

	/**
	 * @see com.lowagie.text.Element#isNestable()
	 * @since	iText 2.0.8
	 */
	@Override
	public boolean isNestable() {
		return true;
	}

    // getters and setters

	/**
     * Gets the number of columns.
     *
     * @return    a value
     */
    public int getColumns() {
        return this.columns;
    }

    /**
     * Gets the number of rows in this <CODE>Table</CODE>.
     *
     * @return      the number of rows in this <CODE>Table</CODE>
     */
    public int size() {
        return this.rows.size();
    }

    /**
     * Gets the dimension of this table
     *
     * @return  dimension
     */
    public Dimension getDimension() {
        return new Dimension(this.columns, size());
    }

    /**
     * Gets the default layout of the Table.
     * @return a cell with all the defaults
     * @since 2.0.7
     */
    public Cell getDefaultCell() {
        return this.defaultCell;
    }

    /**
     * Sets the default layout of the Table to
     * the provided Cell
     * @param value a cell with all the defaults
     * @since 2.0.7
     */
    public void setDefaultCell(final Cell value) {
        this.defaultCell = value;
    }

	/**
     * Gets the last number of the rows that contain headers.
     *
     * @return a rownumber
     */
    public int getLastHeaderRow() {
        return this.lastHeaderRow;
    }

    /**
     * Sets the horizontal alignment.
     *
     * @param       value   the new value
     */
    public void setLastHeaderRow(final int value) {
        this.lastHeaderRow = value;
    }

	/**
     * Gets the horizontal alignment.
     *
     * @return  a value
     */
    public int getAlignment() {
        return this.alignment;
    }

    /**
     * Sets the horizontal alignment.
     *
     * @param       value   the new value
     */
    public void setAlignment(final int value) {
        this.alignment = value;
    }

    /**
     * Sets the alignment of this paragraph.
     *
     * @param    alignment        the new alignment as a <CODE>String</CODE>
     */
    public void setAlignment(final String alignment) {
        if (ElementTags.ALIGN_LEFT.equalsIgnoreCase(alignment)) {
            this.alignment = Element.ALIGN_LEFT;
            return;
        }
        if (ElementTags.RIGHT.equalsIgnoreCase(alignment)) {
            this.alignment = Element.ALIGN_RIGHT;
            return;
        }
        this.alignment = Element.ALIGN_CENTER;
    }

	/**
     * Gets the cellpadding.
     *
     * @return  a value
     */
    public float getPadding() {
        return this.cellpadding;
    }

    /**
     * Sets the cellpadding.
     *
     * @param       value   the new value
     */
    public void setPadding(final float value) {
        this.cellpadding = value;
    }

	/**
     * Gets the cellspacing.
     *
     * @return  a value
     */
    public float getSpacing() {
        return this.cellspacing;
    }

    /**
     * Sets the cellspacing.
     *
     * @param       value   the new value
     */
    public void setSpacing(final float value) {
        this.cellspacing = value;
    }

    /**
     * Enables/disables automatic insertion of empty cells before table is rendered. (default = false)
     * As some people may want to create a table, fill only a couple of the cells and don't bother with
     * investigating which empty ones need to be added, this default behavior may be very welcome.
     * Disabling is recommended to increase speed. (empty cells should be added through extra code then)
     *
     * @param       aDoAutoFill   enable/disable autofill
     */
    public void setAutoFillEmptyCells(final boolean aDoAutoFill) {
        this.autoFillEmptyCells = aDoAutoFill;
    }

	/**
     * Gets the table width (a percentage).
     *
     * @return      the table width
     */
    @Override
	public float getWidth() {
        return this.width;
    }

    /**
     * Sets the width of this table (in percentage of the available space).
     *
     * @param       width           the width
     */
    public void setWidth(final float width) {
        this.width = width;
    }

    /**
	 * @return the locked
	 */
	public boolean isLocked() {
		return this.locked;
	}

	/**
	 * @param locked the locked to set
	 */
	public void setLocked(final boolean locked) {
		this.locked = locked;
	}

	/**
     * Gets the proportional widths of the columns in this <CODE>Table</CODE>.
     *
     * @return      the proportional widths of the columns in this <CODE>Table</CODE>
     */
    public float[] getProportionalWidths() {
        return this.widths;
    }

    /**
     * Sets the widths of the different columns (percentages).
     * <P>
     * You can give up relative values of borderwidths.
     * The sum of these values will be considered 100%.
     * The values will be recalculated as percentages of this sum.
     * <P>
     * example:
     * <BLOCKQUOTE><PRE>
     * float[] widths = {2, 1, 1};
     * <STRONG>table.setWidths(widths)</STRONG>
     * </PRE></BLOCKQUOTE>
     * The widths will be: a width of 50% for the first column,
     * 25% for the second and third column.
     *
     * @param       widths  an array with values
     * @throws BadElementException
     */
    public void setWidths(final float[] widths) throws BadElementException {
        if (widths.length != this.columns) {
            throw new BadElementException("Wrong number of columns.");
        }

        // The sum of all values is 100%
        float hundredPercent = 0;
        for (int i = 0; i < this.columns; i++) {
            hundredPercent += widths[i];
        }

        // The different percentages are calculated
        float width;
        this.widths[this.columns - 1] = 100;
        for (int i = 0; i < this.columns - 1; i++) {
            width = 100.0f * widths[i] / hundredPercent;
            this.widths[i] = width;
            this.widths[this.columns - 1] -= width;
        }
    }

    /**
     * Sets the widths of the different columns (percentages).
     * <P>
     * You can give up relative values of borderwidths.
     * The sum of these values will be considered 100%.
     * The values will be recalculated as percentages of this sum.
     *
     * @param       widths  an array with values
     * @throws DocumentException
     */
    public void setWidths(final int[] widths) throws DocumentException {
        final float tb[] = new float[widths.length];
        for (int k = 0; k < widths.length; ++k) {
			tb[k] = widths[k];
		}
        setWidths(tb);
    }

    /**
     * Checks if this <CODE>Table</CODE> has to fit a page.
     *
     * @return  true if the table may not be split
     */
    public boolean isTableFitsPage() {
        return this.tableFitsPage;
    }

    /**
     * Allows you to control when a page break occurs.
     * <P>
     * When a table doesn't fit a page, it is split in two parts.
     * If you want to avoid this, you should set the <VAR>tableFitsPage</VAR> value to true.
     *
     * @param   fitPage    enter true if you don't want to split cells
     */
    public void setTableFitsPage(final boolean fitPage) {
        this.tableFitsPage = fitPage;
        if (fitPage) {
			setCellsFitPage(true);
		}
    }

    /**
     * Checks if the cells of this <CODE>Table</CODE> have to fit a page.
     *
     * @return  true if the cells may not be split
     */
    public boolean isCellsFitPage() {
        return this.cellsFitPage;
    }

    /**
     * Allows you to control when a page break occurs.
     * <P>
     * When a cell doesn't fit a page, it is split in two parts.
     * If you want to avoid this, you should set the <VAR>cellsFitPage</VAR> value to true.
     *
     * @param   fitPage    enter true if you don't want to split cells
     */
    public void setCellsFitPage(final boolean fitPage) {
        this.cellsFitPage = fitPage;
    }

    /**
     * Sets the offset of this table.
     *
     * Normally a newline is added before you add a Table object.
     * This newline uses the current leading.
     * If you want to control the space between the table and the previous
     * element yourself, you have to set the offset of this table.
     *
     * @param   offset  the space between this table and the previous object.
     */
    public void setOffset(final float offset) {
        this.offset = offset;
    }

    /**
     * Gets the offset of this table.
     *
     * @return  the space between this table and the previous element.
     */
    public float getOffset() {
        return this.offset;
    }

	/**
	 * Method to check if the Table should be converted to a PdfPTable or not.
	 * @return false if the table should be handled the old fashioned way.
	 */
	public boolean isConvert2pdfptable() {
		return this.convert2pdfptable;
	}
	/**
	 * If set to true, iText will try to convert the Table to a PdfPTable.
	 * @param convert2pdfptable true if you want iText to try to convert the Table to a PdfPTable
	 */
	public void setConvert2pdfptable(final boolean convert2pdfptable) {
		this.convert2pdfptable = convert2pdfptable;
	}

    // methods to add content to the table

    /**
     * Adds a <CODE>Cell</CODE> to the <CODE>Table</CODE> at a certain location.
     *
     * @param       aCell        The <CODE>Cell</CODE> to add
     * @param       aLocation    The location where the <CODE>Cell</CODE> will be added
     * @throws BadElementException
     */
	private void addCell(final Cell aCell, final Point aLocation) throws BadElementException {
        if (aCell == null) {
			throw new NullPointerException("addCell - cell has null-value");
		}
        if (aLocation == null) {
			throw new NullPointerException("addCell - point has null-value");
		}
        if (aCell.isTable()) {
			insertTable((Table)aCell.getElements().next(), aLocation);
		}

        if (aLocation.x < 0) {
			throw new BadElementException("row coordinate of location must be >= 0");
		}
        if (aLocation.y <= 0 && aLocation.y > this.columns) {
			throw new BadElementException("column coordinate of location must be >= 0 and < nr of columns");
		}
        if (!isValidLocation(aCell, aLocation)) {
			throw new BadElementException("Adding a cell at the location (" + aLocation.x + "," + aLocation.y + ") with a colspan of " + aCell.getColspan() + " and a rowspan of " + aCell.getRowspan() + " is illegal (beyond boundaries/overlapping).");
		}

        if (aCell.getBorder() == UNDEFINED) {
			aCell.setBorder(this.defaultCell.getBorder());
		}
        aCell.fill();
        placeCell(this.rows, aCell, aLocation);
        setCurrentLocationToNextValidPosition(aLocation);
    }

    /**
     * Adds a <CODE>Cell</CODE> to the <CODE>Table</CODE>.
     *
     * @param       cell         a <CODE>Cell</CODE>
     */
    public void addCell(final Cell cell) {
        try {
            addCell(cell, this.curPosition);
        }
        catch(final BadElementException bee) {
            // don't add the cell
        }
    }

    /**
     * Adds a <CODE>Cell</CODE> to the <CODE>Table</CODE>.
     * <P>
     * This is a shortcut for <CODE>addCell(Cell cell, Point location)</CODE>.
     * The <CODE>Phrase</CODE> will be converted to a <CODE>Cell</CODE>.
     *
     * @param       content         a <CODE>Phrase</CODE>
     * @param       location        a <CODE>Point</CODE>
     * @throws      BadElementException this should never happen
     */
    private void addCell(final Phrase content, final Point location) throws BadElementException {
        final Cell cell = new Cell(content);
        cell.setBorder(this.defaultCell.getBorder());
        cell.setBorderWidth(this.defaultCell.getBorderWidth());
        cell.setBorderColor(this.defaultCell.getBorderColor());
        cell.setBackgroundColor(this.defaultCell.getBackgroundColor());
        cell.setHorizontalAlignment(this.defaultCell.getHorizontalAlignment());
        cell.setVerticalAlignment(this.defaultCell.getVerticalAlignment());
        cell.setColspan(this.defaultCell.getColspan());
        cell.setRowspan(this.defaultCell.getRowspan());
        addCell(cell, location);
    }

    /**
     * To put a table within the existing table at the current position
     * generateTable will of course re-arrange the widths of the columns.
     *
     * @param   aTable      the table you want to insert
     */
    void insertTable(final Table aTable) {
        if (aTable == null) {
			throw new NullPointerException("insertTable - table has null-value");
		}
        insertTable(aTable, this.curPosition);
    }

    /**
     * To put a table within the existing table at the given position
     * generateTable will of course re-arrange the widths of the columns.
     *
     * @param   aTable      the table you want to insert
     * @param   aLocation   a <CODE>Point</CODE>
     */
    private void insertTable(final Table aTable, final Point aLocation) {

        if (aTable == null) {
			throw new NullPointerException("insertTable - table has null-value");
		}
        if (aLocation == null) {
			throw new NullPointerException("insertTable - point has null-value");
		}
        this.mTableInserted = true;
        aTable.complete();

        if (aLocation.y > this.columns) {
        	throw new IllegalArgumentException("insertTable -- wrong columnposition("+ aLocation.y + ") of location; max =" + this.columns);
        }

        final int rowCount = aLocation.x + 1 - this.rows.size();
        int i = 0;
        if ( rowCount > 0 ) {   //create new rows ?
            for (; i < rowCount; i++) {
                this.rows.add(new Row(this.columns));
            }
        }

        ((Row) this.rows.get(aLocation.x)).setElement(aTable,aLocation.y);

        setCurrentLocationToNextValidPosition(aLocation);
    }

    /**
     * Gives you the possibility to add columns.
     *
     * @param   aColumns    the number of columns to add
     */
    public void addColumns(final int aColumns) {
        final ArrayList newRows = new ArrayList(this.rows.size());

        final int newColumns = this.columns + aColumns;
        Row row;
        for (int i = 0; i < this.rows.size(); i++) {
            row = new Row(newColumns);
            for (int j = 0; j < this.columns; j++) {
                row.setElement(((Row) this.rows.get(i)).getCell(j) ,j);
            }
            for (int j = this.columns; j < newColumns && i < this.curPosition.x; j++) {
                row.setElement(null, j);
            }
            newRows.add(row);
        }
        // applied 1 column-fix; last column needs to have a width of 0
        final float [] newWidths = new float[newColumns];
        System.arraycopy(this.widths, 0, newWidths, 0, this.columns);
        for (int j = this.columns; j < newColumns ; j++) {
            newWidths[j] = 0;
        }
        this.columns = newColumns;
        this.widths = newWidths;
        this.rows = newRows;
    }



    /**
     * Will fill empty cells with valid blank <CODE>Cell</CODE>s
     */
    public void complete() {
        if (this.mTableInserted) {
            mergeInsertedTables();  // integrate tables in the table
            this.mTableInserted = false;
        }
        if (this.autoFillEmptyCells) {
            fillEmptyMatrixCells();
        }
    }

    // private helper classes

    /**
     * returns the element at the position row, column
     *          (Cast to Cell or Table)
     *
     * @param row
     * @param column
     * @return  dimension
     * @since  2.1.0 (was made private in 2.0.3)
     */
    private Object getElement(final int row, final int column) {
        return ((Row) this.rows.get(row)).getCell(column);
    }

    /**
     * Integrates all added tables and recalculates column widths.
     */
    private void mergeInsertedTables() {
        int i=0, j=0;
        float [] lNewWidths = null;
        final int [] lDummyWidths = new int[this.columns];     // to keep track in how many new cols this one will be split
        final float[][] lDummyColumnWidths = new float[this.columns][]; // bugfix Tony Copping
        final int [] lDummyHeights = new int[this.rows.size()]; // to keep track in how many new rows this one will be split
        ArrayList newRows = null;
        boolean isTable=false;
        int lTotalRows  = 0, lTotalColumns      = 0;
        int lNewMaxRows = 0, lNewMaxColumns     = 0;

        Table lDummyTable = null;

        // first we'll add new columns when needed
        // check one column at a time, find maximum needed nr of cols
        // Search internal tables and find one with max columns
        for (j=0; j < this.columns; j++) {
            lNewMaxColumns = 1; // value to hold in how many columns the current one will be split
            float [] tmpWidths = null;
            for (i=0; i < this.rows.size(); i++) {
                if ( Table.class.isInstance(((Row) this.rows.get(i)).getCell(j)) ) {
                    isTable=true;
                    lDummyTable = (Table) ((Row) this.rows.get(i)).getCell(j);
                    if( tmpWidths == null) {
                        tmpWidths = lDummyTable.widths;
                        lNewMaxColumns=tmpWidths.length;
                    }
                    else {
                        final int cols = lDummyTable.getDimension().width;
                        final float [] tmpWidthsN = new float[ cols * tmpWidths.length];
                        float tpW=0, btW=0, totW=0;
                        int tpI=0, btI=0, totI=0;
                        tpW+=tmpWidths[0];
                        btW+=lDummyTable.widths[0];
                        while( tpI<tmpWidths.length && btI<cols) {
                            if( btW>tpW) {
                                tmpWidthsN[totI] = tpW-totW;
                                tpI++;
                                if(tpI<tmpWidths.length) {
                                    tpW+=tmpWidths[tpI];
                                }
                            }
                            else {
                                tmpWidthsN[totI] = btW-totW;
                                btI++;
                                if(Math.abs(btW - tpW) < 0.0001) {
                                    tpI++;
                                    if(tpI<tmpWidths.length) {
                                        tpW+=tmpWidths[tpI];
                                    }
                                }
                                if(btI<cols) {
                                    btW+=lDummyTable.widths[btI];
                                }
                            }
                            totW+=tmpWidthsN[totI];
                            totI++;
                        }
                       /*if( tpI<tmpWidths.length)
                       {
                           System.arraycopy(tmpWidths, tpI, tmpWidthsN, totI, tmpWidths.length-tpI);
                           totI +=tmpWidths.length-tpI;
                       }
                       else if(btI<cols)
                       {
                           System.arraycopy(lDummyTable.widths, btI, tmpWidthsN, totI, lDummyTable.widths.length-btI);
                           totI +=lDummyTable.widths.length-btI;                                                  }*/
                        tmpWidths = new float[totI];
                        System.arraycopy(tmpWidthsN, 0, tmpWidths, 0, totI);
                        lNewMaxColumns=totI;
                    }
                                     /*if ( lDummyTable.getDimension().width > lNewMaxColumns )
                   {
                       lNewMaxColumns = lDummyTable.getDimension().width;
                       lDummyColumnWidths[j] = lDummyTable.widths; // bugfix Tony Copping
                   }*/
                }
            }
            lDummyColumnWidths[j] = tmpWidths;
            lTotalColumns += lNewMaxColumns;
            lDummyWidths [j] = lNewMaxColumns;
        }

        // next we'll add new rows when needed
        for (i=0; i < this.rows.size(); i++) {
            lNewMaxRows = 1;    // holds value in how many rows the current one will be split
            for (j=0; j < this.columns; j++) {
                if ( Table.class.isInstance(((Row) this.rows.get(i)).getCell(j)) ) {
                    isTable=true;
                    lDummyTable = (Table) ((Row) this.rows.get(i)).getCell(j);
                    if ( lDummyTable.getDimension().height > lNewMaxRows ) {
                        lNewMaxRows = lDummyTable.getDimension().height;
                    }
                }
            }
            lTotalRows += lNewMaxRows;
            lDummyHeights [i] = lNewMaxRows;
        }

        if ( lTotalColumns != this.columns || lTotalRows != this.rows.size() || isTable)    // NO ADJUSTMENT
        {
            // ** WIDTH
            // set correct width for new columns
            // divide width over new nr of columns
            // Take new max columns of internal table and work out widths for each col
            lNewWidths = new float [lTotalColumns];
            int lDummy = 0;
            for (int tel=0; tel < this.widths.length;tel++) {
                if ( lDummyWidths[tel] != 1) {
                    // divide
                    for (int tel2 = 0; tel2 < lDummyWidths[tel]; tel2++) {
                        // lNewWidths[lDummy] = widths[tel] / lDummyWidths[tel];
                        lNewWidths[lDummy] = this.widths[tel] * lDummyColumnWidths[tel][tel2] / 100f; // bugfix Tony Copping
                        lDummy++;
                    }
                }
                else {
                    lNewWidths[lDummy] = this.widths[tel];
                    lDummy++;
                }
            }

            // ** FILL OUR NEW TABLE
            // generate new table
            // set new widths
            // copy old values
            newRows = new ArrayList(lTotalRows);
            for (i = 0; i < lTotalRows; i++) {
                newRows.add(new Row(lTotalColumns));
            }
            int lDummyRow = 0, lDummyColumn = 0;        // to remember where we are in the new, larger table
            Object lDummyElement = null;
            for (i=0; i < this.rows.size(); i++) {
                lDummyColumn = 0;
                lNewMaxRows = 1;
                for (j=0; j < this.columns; j++) {
                    if ( Table.class.isInstance(((Row) this.rows.get(i)).getCell(j)) )       // copy values from embedded table
                    {
                        lDummyTable = (Table) ((Row) this.rows.get(i)).getCell(j);

                        // Work out where columns in table table correspond to columns in current table
                        final int colMap[] = new int[lDummyTable.widths.length+1];
                        int cb=0, ct=0;

                        for( ; cb<lDummyTable.widths.length;cb++) {
                            colMap[cb] = lDummyColumn+ct;

                            float wb;
                            wb = lDummyTable.widths[cb];

                            float wt=0;
                            while( ct<lDummyWidths[j]) {
                                wt+=lDummyColumnWidths[j][ct++];
                                if(Math.abs(wb - wt) < 0.0001) {
									break;
								}
                            }
                        }
                        colMap[cb] = lDummyColumn+ct;

                        // need to change this to work out how many cols to span
                        for (int k=0; k < lDummyTable.getDimension().height; k++) {
                            for (int l=0; l < lDummyTable.getDimension().width; l++) {
                                lDummyElement = lDummyTable.getElement(k,l);
                                if (lDummyElement != null) {
                                    int col=lDummyColumn+l;

                                    if ( Cell.class.isInstance(lDummyElement) ) {
                                        final Cell lDummyC = (Cell)lDummyElement;
                                        // Find col to add cell in and set col span
                                        col = colMap[l];
                                        final int ot = colMap[l+lDummyC.getColspan()];

                                        lDummyC.setColspan(ot-col);
                                    }

                                    ((Row) newRows.get(k + lDummyRow)).addElement(lDummyElement,col);  // use addElement to set reserved status ok in row
                                }
                            }
                        }
                    }
                    else        // copy others values
                    {
                        final Object aElement = getElement(i,j);

                        if ( Cell.class.isInstance(aElement) ) {

                            // adjust spans for cell
                            ((Cell) aElement).setRowspan(((Cell) ((Row) this.rows.get(i)).getCell(j)).getRowspan() + lDummyHeights[i] - 1);
                            ((Cell) aElement).setColspan(((Cell) ((Row) this.rows.get(i)).getCell(j)).getColspan() + lDummyWidths[j] - 1);

                            // most likely this cell covers a larger area because of the row/cols splits : define not-to-be-filled cells
                            placeCell(newRows,(Cell) aElement, new Point(lDummyRow,lDummyColumn));
                        }
                    }
                    lDummyColumn += lDummyWidths[j];
                }
                lDummyRow += lDummyHeights[i];
            }

            // Set our new matrix
            this.columns     = lTotalColumns;
            this.rows = newRows;
            this.widths = lNewWidths;
        }
    }

    /**
     * adds new<CODE>Cell</CODE>'s to empty/null spaces.
     */
    private void fillEmptyMatrixCells() {
        try {
            for (int i=0; i < this.rows.size(); i++) {
                for (int j=0; j < this.columns; j++) {
                    if (!((Row) this.rows.get(i)).isReserved(j)) {
                        addCell(this.defaultCell, new Point(i, j));
                    }
                }
            }
        }
        catch(final BadElementException bee) {
            throw new ExceptionConverter(bee);
        }
    }

    /**
     * check if <CODE>Cell</CODE> 'fits' the table.
     * <P>
     * <UL><LI>rowspan/colspan not beyond borders
     *     <LI>spanned cell don't overlap existing cells</UL>
     *
     * @param   aCell       the cell that has to be checked
     * @param   aLocation   the location where the cell has to be placed
     * @return true if the location was valid
     */
    private boolean isValidLocation(final Cell aCell, final Point aLocation) {
        // rowspan not beyond last column
        if ( aLocation.x < this.rows.size() )        // if false : new location is already at new, not-yet-created area so no check
        {
            if (aLocation.y + aCell.getColspan() > this.columns) {
                return false;
            }

            final int difx = this.rows.size() - aLocation.x >  aCell.getRowspan() ? aCell.getRowspan() : this.rows.size() - aLocation.x;
            final int dify = this.columns - aLocation.y >  aCell.getColspan() ? aCell.getColspan() : this.columns - aLocation.y;
            // no other content at cells targeted by rowspan/colspan
            for (int i=aLocation.x; i < aLocation.x + difx; i++) {
                for (int j=aLocation.y; j < aLocation.y + dify; j++) {
                    if (((Row) this.rows.get(i)).isReserved(j)) {
                        return false;
                    }
                }
            }
        }
        else {
            if (aLocation.y + aCell.getColspan() > this.columns) {
                return false;
            }
        }

        return true;
    }

    /**
     * Sets the unset cell properties to be the table defaults.
     *
     * @param aCell The cell to set to table defaults as necessary.
     */
    private void assumeTableDefaults(final Cell aCell) {

        if (aCell.getBorder() == Rectangle.UNDEFINED) {
            aCell.setBorder(this.defaultCell.getBorder());
        }
        if (aCell.getBorderWidth() == Rectangle.UNDEFINED) {
            aCell.setBorderWidth(this.defaultCell.getBorderWidth());
        }
        if (aCell.getBorderColor() == null) {
            aCell.setBorderColor(this.defaultCell.getBorderColor());
        }
        if (aCell.getBackgroundColor() == null) {
            aCell.setBackgroundColor(this.defaultCell.getBackgroundColor());
        }
        if (aCell.getHorizontalAlignment() == Element.ALIGN_UNDEFINED) {
            aCell.setHorizontalAlignment(this.defaultCell.getHorizontalAlignment());
        }
        if (aCell.getVerticalAlignment() == Element.ALIGN_UNDEFINED) {
            aCell.setVerticalAlignment(this.defaultCell.getVerticalAlignment());
        }
    }

    /**
     * Inserts a Cell in a cell-array and reserves cells defined by row-/colspan.
     *
     * @param   someRows    some rows
     * @param   aCell       the cell that has to be inserted
     * @param   aPosition   the position where the cell has to be placed
     */
    private void placeCell(final ArrayList someRows, final Cell aCell, final Point aPosition) {
        int i;
        Row row = null;
        final int rowCount = aPosition.x + aCell.getRowspan() - someRows.size();
        assumeTableDefaults(aCell);
        if ( aPosition.x + aCell.getRowspan() > someRows.size() ) {
            for (i = 0; i < rowCount; i++) {
                row = new Row(this.columns);
                someRows.add(row);
            }
        }

        // reserve cell in rows below
        for (i = aPosition.x + 1; i < aPosition.x  + aCell.getRowspan(); i++) {
            if ( !((Row) someRows.get(i)).reserve(aPosition.y, aCell.getColspan())) {

                // should be impossible to come here :-)
                throw new RuntimeException("addCell - error in reserve");
            }
        }
        row = (Row) someRows.get(aPosition.x);
        row.addElement(aCell, aPosition.y);

    }

    /**
     *  Sets current col/row to valid(empty) pos after addCell/Table
     * @param aLocation a location in the Table
     */
    private void setCurrentLocationToNextValidPosition(final Point aLocation)    {
        // set latest location to next valid position
        int i, j;
        i = aLocation.x;
        j = aLocation.y;
        do {
            if ( j + 1  == this.columns ) {    // goto next row
                i++;
                j = 0;
            }
            else {
                j++;
            }
        }
        while (
        i < this.rows.size() && j < this.columns && ((Row) this.rows.get(i)).isReserved(j)
        );
        this.curPosition = new Point(i, j);
    }

    // public helper methods

    /**
     * Gets an array with the positions of the borders between every column.
     * <P>
     * This method translates the widths expressed in percentages into the
     * x-coordinate of the borders of the columns on a real document.
     *
     * @param       left            this is the position of the first border at the left (cellpadding not included)
     * @param       totalWidth      this is the space between the first border at the left
     *                                              and the last border at the right (cellpadding not included)
     * @return      an array with border positions
     */
    public float[] getWidths(final float left, float totalWidth) {
        // for x columns, there are x+1 borders
        final float[] w = new float[this.columns + 1];
        float wPercentage;
        if (this.locked) {
        	wPercentage = 100 * this.width / totalWidth;
        }
        else {
        	wPercentage = this.width;
        }
        // the border at the left is calculated
        switch(this.alignment) {
            case Element.ALIGN_LEFT:
                w[0] = left;
                break;
            case Element.ALIGN_RIGHT:
                w[0] = left + totalWidth * (100 - wPercentage) / 100;
                break;
            case Element.ALIGN_CENTER:
            default:
                w[0] = left + totalWidth * (100 - wPercentage) / 200;
        }
        // the total available width is changed
        totalWidth = totalWidth * wPercentage / 100;
        // the inner borders are calculated
        for (int i = 1; i < this.columns; i++) {
            w[i] = w[i - 1] + this.widths[i - 1] * totalWidth / 100;
        }
        // the border at the right is calculated
        w[this.columns] = w[0] + totalWidth;
        return w;
    }

    /**
     * Gets an <CODE>Iterator</CODE> of all the <CODE>Row</CODE>s.
     *
     * @return      an <CODE>Iterator</CODE>
     */
    public Iterator iterator() {
        return this.rows.iterator();
    }

    /**
     * Create a PdfPTable based on this Table object.
     * @return a PdfPTable object
     * @throws BadElementException
     */
    PdfPTable createPdfPTable() throws BadElementException {
    	if (!this.convert2pdfptable) {
    		throw new BadElementException("No error, just an old style table");
    	}
        setAutoFillEmptyCells(true);
    	complete();
    	final PdfPTable pdfptable = new PdfPTable(this.widths);
    	pdfptable.setComplete(this.complete);
    	if (isNotAddedYet()) {
			pdfptable.setSkipFirstHeader(true);
		}
    	final SimpleTable t_evt = new SimpleTable();
		t_evt.cloneNonPositionParameters(this);
		t_evt.setCellspacing(this.cellspacing);
    	pdfptable.setTableEvent(t_evt);
    	pdfptable.setHeaderRows(this.lastHeaderRow + 1);
    	pdfptable.setSplitLate(this.cellsFitPage);
    	pdfptable.setKeepTogether(this.tableFitsPage);
    	if (!Float.isNaN(this.offset)) {
    		pdfptable.setSpacingBefore(this.offset);
    	}
    	pdfptable.setHorizontalAlignment(this.alignment);
    	if (this.locked) {
    		pdfptable.setTotalWidth(this.width);
    		pdfptable.setLockedWidth(true);
    	}
    	else {
    		pdfptable.setWidthPercentage(this.width);
    	}
    	Row row;
        for (final Iterator iterator = iterator(); iterator.hasNext(); ) {
            row = (Row) iterator.next();
            Element cell;
            PdfPCell pcell;
            for (int i = 0; i < row.getColumns(); i++) {
                if ((cell = (Element)row.getCell(i)) != null) {
                	if (cell instanceof Table) {
                		pcell = new PdfPCell(((Table)cell).createPdfPTable());
                	}
                	else if (cell instanceof Cell) {
                		pcell = ((Cell)cell).createPdfPCell();
                		pcell.setPadding(this.cellpadding + this.cellspacing / 2f);
                		final SimpleCell c_evt = new SimpleCell(SimpleCell.CELL);
                		c_evt.cloneNonPositionParameters((Cell)cell);
                		c_evt.setSpacing(this.cellspacing * 2f);
                        pcell.setCellEvent(c_evt);
                	}
                	else {
                		pcell = new PdfPCell();
                	}
                	pdfptable.addCell(pcell);
                }
            }
        }
    	return pdfptable;
    }

	/**
	 * Indicates if this is the first time the section is added.
	 * @since	iText2.0.8
	 * @return	true if the section wasn't added yet
	 */
	public boolean isNotAddedYet() {
		return this.notAddedYet;
	}

	/**
	 * Sets the indication if the section was already added to
	 * the document.
	 * @since	iText2.0.8
	 * @param notAddedYet
	 */
	public void setNotAddedYet(final boolean notAddedYet) {
		this.notAddedYet = notAddedYet;
	}

	/**
	 * @since	iText 2.0.8
	 * @see com.lowagie.text.LargeElement#flushContent()
	 */
	@Override
	public void flushContent() {
		this.setNotAddedYet(false);
        final ArrayList headerrows = new ArrayList();
        for (int i = 0; i < getLastHeaderRow() + 1; i++) {
            headerrows.add(this.rows.get(i));
        }
        this.rows = headerrows;
	}

	/**
     * @since	iText 2.0.8
	 * @see com.lowagie.text.LargeElement#isComplete()
	 */
	@Override
	public boolean isComplete() {
		return this.complete;
	}

	/**
     * @since	iText 2.0.8
	 * @see com.lowagie.text.LargeElement#setComplete(boolean)
	 */
	@Override
	public void setComplete(final boolean complete) {
		this.complete = complete;
	}

    /**
     * Gets the default layout of the Table.
     * @return a cell with all the defaults
     * @deprecated As of iText 2.0.7, replaced by {@link #getDefaultCell()},
     * scheduled for removal at 2.2.0
     */
    @Deprecated
	public Cell getDefaultLayout() {
        return getDefaultCell();
    }

    /**
     * Sets the default layout of the Table to
     * the provided Cell
     * @param value a cell with all the defaults
     * @deprecated As of iText 2.0.7, replaced by {@link #setDefaultCell(Cell)},
     * scheduled for removal at 2.2.0
     */
    @Deprecated
	public void setDefaultLayout(final Cell value) {
        this.defaultCell = value;
    }
}
