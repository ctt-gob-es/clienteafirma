/*
 * $Id: ColumnText.java 3904 2009-04-24 10:09:01Z blowagie $
 *
 * Copyright 2001, 2002 by Paulo Soares.
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
import java.util.LinkedList;
import java.util.Stack;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.ListItem;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.SimpleTable;
import com.lowagie.text.pdf.draw.DrawInterface;

/**
 * Formats text in a columnwise form. The text is bound
 * on the left and on the right by a sequence of lines. This allows the column
 * to have any shape, not only rectangular.
 * <P>
 * Several parameters can be set like the first paragraph line indent and
 * extra space between paragraphs.
 * <P>
 * A call to the method <CODE>go</CODE> will return one of the following
 * situations: the column ended or the text ended.
 * <P>
 * I the column ended, a new column definition can be loaded with the method
 * <CODE>setColumns</CODE> and the method <CODE>go</CODE> can be called again.
 * <P>
 * If the text ended, more text can be loaded with <CODE>addText</CODE>
 * and the method <CODE>go</CODE> can be called again.<BR>
 * The only limitation is that one or more complete paragraphs must be loaded
 * each time.
 * <P>
 * Full bidirectional reordering is supported. If the run direction is
 * <CODE>PdfWriter.RUN_DIRECTION_RTL</CODE> the meaning of the horizontal
 * alignments and margins is mirrored.
 * @author Paulo Soares (psoares@consiste.pt)
 */

class ColumnText {















    private int runDirection = PdfWriter.RUN_DIRECTION_DEFAULT;

    /** the space char ratio */
    private static final float GLOBAL_SPACE_CHAR_RATIO = 0;



    /** Signals that there is no more text available. */
    static final int NO_MORE_TEXT = 1;

    /** Signals that there is no more column. */
    static final int NO_MORE_COLUMN = 2;

    /** The column is valid. */
    private static final int LINE_STATUS_OK = 0;

    /** The line is out the column limits. */
    private static final int LINE_STATUS_OFFLIMITS = 1;

    /** The line cannot fit this column position. */
    private static final int LINE_STATUS_NOLINE = 2;

    /** Upper bound of the column. */
    private float maxY;

    /** Lower bound of the column. */
    private float minY;

    private float leftX;

    private float rightX;

    /** The column alignment. Default is left alignment. */
    private int alignment = Element.ALIGN_LEFT;

    /** The left column bound. */
    private ArrayList leftWall;

    /** The right column bound. */
    private ArrayList rightWall;

    /** The chunks that form the text. */
//    protected ArrayList chunks = new ArrayList();
    private BidiLine bidiLine;

    /** The current y line location. Text will be written at this line minus the leading. */
    private float yLine;

    /** The leading for the current line. */
    private float currentLeading = 16;

    /** The fixed text leading. */
    private float fixedLeading = 16;

    /** The text leading that is multiplied by the biggest font size in the line. */
    private float multipliedLeading = 0;

    /** The <CODE>PdfContent</CODE> where the text will be written to. */
    private PdfContentByte canvas;

    private PdfContentByte[] canvases;

    /** The line status when trying to fit a line to a column. */
    private int lineStatus;

    /** The first paragraph line indent. */
    private float indent = 0;

    /** The following paragraph lines indent. */
    private float followingIndent = 0;

    /** The right paragraph lines indent. */
    private float rightIndent = 0;

    /** The extra space between paragraphs. */
    private float extraParagraphSpace = 0;

    /** The width of the line when the column is defined as a simple rectangle. */
    private float rectangularWidth = -1;

    private boolean rectangularMode = false;
    /** Holds value of property spaceCharRatio. */
    private float spaceCharRatio = GLOBAL_SPACE_CHAR_RATIO;

    private boolean lastWasNewline = true;

    /** Holds value of property linesWritten. */
    private int linesWritten;

    private float firstLineY;
    private boolean firstLineYDone = false;

    /** Holds value of property arabicOptions. */
    private int arabicOptions = 0;

    private float descender;

    private boolean composite = false;

    private ColumnText compositeColumn;

    protected LinkedList compositeElements;

    private int listIdx = 0;

    private boolean splittedRow;

    private Phrase waitPhrase;

    /** if true, first line height is adjusted so that the max ascender touches the top */
    private boolean useAscender = false;

    /** Holds value of property filledWidth. */
    private float filledWidth;

    private boolean adjustFirstLine = true;

    /**
     * Creates a <CODE>ColumnText</CODE>.
     *
     * @param canvas the place where the text will be written to. Can
     * be a template.
     */
    ColumnText(final PdfContentByte canvas) {
        this.canvas = canvas;
    }

    /**
     * Creates an independent duplicated of the instance <CODE>org</CODE>.
     *
     * @param org the original <CODE>ColumnText</CODE>
     * @return the duplicated
     */
    static ColumnText duplicate(final ColumnText org) {
        final ColumnText ct = new ColumnText(null);
        ct.setACopy(org);
        return ct;
    }

    /**
     * Makes this instance an independent copy of <CODE>org</CODE>.
     *
     * @param org the original <CODE>ColumnText</CODE>
     * @return itself
     */
    private ColumnText setACopy(final ColumnText org) {
        setSimpleVars(org);
        if (org.bidiLine != null) {
			this.bidiLine = new BidiLine(org.bidiLine);
		}
        return this;
    }

    private void setSimpleVars(final ColumnText org) {
        this.maxY = org.maxY;
        this.minY = org.minY;
        this.alignment = org.alignment;
        this.leftWall = null;
        if (org.leftWall != null) {
			this.leftWall = new ArrayList(org.leftWall);
		}
        this.rightWall = null;
        if (org.rightWall != null) {
			this.rightWall = new ArrayList(org.rightWall);
		}
        this.yLine = org.yLine;
        this.currentLeading = org.currentLeading;
        this.fixedLeading = org.fixedLeading;
        this.multipliedLeading = org.multipliedLeading;
        this.canvas = org.canvas;
        this.canvases = org.canvases;
        this.lineStatus = org.lineStatus;
        this.indent = org.indent;
        this.followingIndent = org.followingIndent;
        this.rightIndent = org.rightIndent;
        this.extraParagraphSpace = org.extraParagraphSpace;
        this.rectangularWidth = org.rectangularWidth;
        this.rectangularMode = org.rectangularMode;
        this.spaceCharRatio = org.spaceCharRatio;
        this.lastWasNewline = org.lastWasNewline;
        this.linesWritten = org.linesWritten;
        this.arabicOptions = org.arabicOptions;
        this.runDirection = org.runDirection;
        this.descender = org.descender;
        this.composite = org.composite;
        this.splittedRow = org.splittedRow;
        if (org.composite) {
            this.compositeElements = new LinkedList(org.compositeElements);
            if (this.splittedRow) {
                final PdfPTable table = (PdfPTable)this.compositeElements.getFirst();
                this.compositeElements.set(0, new PdfPTable(table));
            }
            if (org.compositeColumn != null) {
				this.compositeColumn = duplicate(org.compositeColumn);
			}
        }
        this.listIdx = org.listIdx;
        this.firstLineY = org.firstLineY;
        this.leftX = org.leftX;
        this.rightX = org.rightX;
        this.firstLineYDone = org.firstLineYDone;
        this.waitPhrase = org.waitPhrase;
        this.useAscender = org.useAscender;
        this.filledWidth = org.filledWidth;
        this.adjustFirstLine = org.adjustFirstLine;
    }

    private void addWaitingPhrase() {
        if (this.bidiLine == null && this.waitPhrase != null) {
            this.bidiLine = new BidiLine();
            for (final Iterator j = this.waitPhrase.getChunks().iterator(); j.hasNext();) {
                this.bidiLine.addChunk(new PdfChunk((Chunk)j.next(), null));
            }
            this.waitPhrase = null;
        }
    }

    /**
     * Adds a <CODE>Phrase</CODE> to the current text array.
     * Will not have any effect if addElement() was called before.
     *
     * @param phrase the text
     */
    void addText(final Phrase phrase) {
        if (phrase == null || this.composite) {
			return;
		}
        addWaitingPhrase();
        if (this.bidiLine == null) {
            this.waitPhrase = phrase;
            return;
        }
        for (final Iterator j = phrase.getChunks().iterator(); j.hasNext();) {
            this.bidiLine.addChunk(new PdfChunk((Chunk)j.next(), null));
        }
    }

    /**
     * Replaces the current text array with this <CODE>Phrase</CODE>.
     * Anything added previously with addElement() is lost.
     *
     * @param phrase the text
     */
    public void setText(final Phrase phrase) {
        this.bidiLine = null;
        this.composite = false;
        this.compositeColumn = null;
        this.compositeElements = null;
        this.listIdx = 0;
        this.splittedRow = false;
        this.waitPhrase = phrase;
    }



    /**
     * Adds an element. Elements supported are <CODE>Paragraph</CODE>,
     * <CODE>List</CODE>, <CODE>PdfPTable</CODE>, <CODE>Image</CODE> and
     * <CODE>Graphic</CODE>.
     * <p>
     * It removes all the text placed with <CODE>addText()</CODE>.
     *
     * @param element the <CODE>Element</CODE>
     */
    void addElement(Element element) {
        if (element == null) {
			return;
		}
        if (element instanceof Image) {
            final Image img = (Image)element;
            final PdfPTable t = new PdfPTable(1);
            final float w = img.getWidthPercentage();
            if (w == 0) {
                t.setTotalWidth(img.getScaledWidth());
                t.setLockedWidth(true);
            } else {
				t.setWidthPercentage(w);
			}
            t.setSpacingAfter(img.getSpacingAfter());
            t.setSpacingBefore(img.getSpacingBefore());
            switch (img.getAlignment()) {
                case Image.LEFT:
                    t.setHorizontalAlignment(Element.ALIGN_LEFT);
                    break;
                case Image.RIGHT:
                    t.setHorizontalAlignment(Element.ALIGN_RIGHT);
                    break;
                default:
                    t.setHorizontalAlignment(Element.ALIGN_CENTER);
                    break;
            }
            final PdfPCell c = new PdfPCell(img, true);
            c.setPadding(0);
            c.setBorder(img.getBorder());
            c.setBorderColor(img.getBorderColor());
            c.setBorderWidth(img.getBorderWidth());
            c.setBackgroundColor(img.getBackgroundColor());
            t.addCell(c);
            element = t;
        }
        if (element.type() == Element.CHUNK) {
        	element = new Paragraph((Chunk)element);
        }
        else if (element.type() == Element.PHRASE) {
        	element = new Paragraph((Phrase)element);
        }
        if (element instanceof SimpleTable) {
        	try {
				element = ((SimpleTable)element).createPdfPTable();
			} catch (final DocumentException e) {
				throw new IllegalArgumentException("Element not allowed.");
			}
        }
        else if (element.type() != Element.PARAGRAPH && element.type() != Element.LIST && element.type() != Element.PTABLE && element.type() != Element.YMARK) {
			throw new IllegalArgumentException("Element not allowed.");
		}
        if (!this.composite) {
            this.composite = true;
            this.compositeElements = new LinkedList();
            this.bidiLine = null;
            this.waitPhrase = null;
        }
        this.compositeElements.add(element);
    }

    /**
     * Converts a sequence of lines representing one of the column bounds into
     * an internal format.
     * <p>
     * Each array element will contain a <CODE>float[4]</CODE> representing
     * the line x = ax + b.
     *
     * @param cLine the column array
     * @return the converted array
     */
    private ArrayList convertColumn(final float cLine[]) {
        if (cLine.length < 4) {
			throw new RuntimeException("No valid column line found.");
		}
        final ArrayList cc = new ArrayList();
        for (int k = 0; k < cLine.length - 2; k += 2) {
            final float x1 = cLine[k];
            final float y1 = cLine[k + 1];
            final float x2 = cLine[k + 2];
            final float y2 = cLine[k + 3];
            if (y1 == y2) {
				continue;
			}
            // x = ay + b
            final float a = (x1 - x2) / (y1 - y2);
            final float b = x1 - a * y1;
            final float r[] = new float[4];
            r[0] = Math.min(y1, y2);
            r[1] = Math.max(y1, y2);
            r[2] = a;
            r[3] = b;
            cc.add(r);
            this.maxY = Math.max(this.maxY, r[1]);
            this.minY = Math.min(this.minY, r[0]);
        }
        if (cc.isEmpty()) {
			throw new RuntimeException("No valid column line found.");
		}
        return cc;
    }

    /**
     * Finds the intersection between the <CODE>yLine</CODE> and the column. It will
     * set the <CODE>lineStatus</CODE> appropriately.
     *
     * @param wall the column to intersect
     * @return the x coordinate of the intersection
     */
    private float findLimitsPoint(final ArrayList wall) {
        this.lineStatus = LINE_STATUS_OK;
        if (this.yLine < this.minY || this.yLine > this.maxY) {
            this.lineStatus = LINE_STATUS_OFFLIMITS;
            return 0;
        }
        for (int k = 0; k < wall.size(); ++k) {
            final float r[] = (float[])wall.get(k);
            if (this.yLine < r[0] || this.yLine > r[1]) {
				continue;
			}
            return r[2] * this.yLine + r[3];
        }
        this.lineStatus = LINE_STATUS_NOLINE;
        return 0;
    }

    /**
     * Finds the intersection between the <CODE>yLine</CODE> and the two
     * column bounds. It will set the <CODE>lineStatus</CODE> appropriately.
     *
     * @return a <CODE>float[2]</CODE>with the x coordinates of the intersection
     */
    private float[] findLimitsOneLine() {
        final float x1 = findLimitsPoint(this.leftWall);
        if (this.lineStatus == LINE_STATUS_OFFLIMITS || this.lineStatus == LINE_STATUS_NOLINE) {
			return null;
		}
        final float x2 = findLimitsPoint(this.rightWall);
        if (this.lineStatus == LINE_STATUS_NOLINE) {
			return null;
		}
        return new float[]{x1, x2};
    }

    /**
     * Finds the intersection between the <CODE>yLine</CODE>,
     * the <CODE>yLine-leading</CODE>and the two column bounds.
     * It will set the <CODE>lineStatus</CODE> appropriately.
     *
     * @return a <CODE>float[4]</CODE>with the x coordinates of the intersection
     */
    private float[] findLimitsTwoLines() {
        boolean repeat = false;
        for (;;) {
            if (repeat && this.currentLeading == 0) {
				return null;
			}
            repeat = true;
            final float x1[] = findLimitsOneLine();
            if (this.lineStatus == LINE_STATUS_OFFLIMITS) {
				return null;
			}
            this.yLine -= this.currentLeading;
            if (this.lineStatus == LINE_STATUS_NOLINE) {
                continue;
            }
            final float x2[] = findLimitsOneLine();
            if (this.lineStatus == LINE_STATUS_OFFLIMITS) {
				return null;
			}
            if (this.lineStatus == LINE_STATUS_NOLINE) {
                this.yLine -= this.currentLeading;
                continue;
            }
            if (x1[0] >= x2[1] || x2[0] >= x1[1]) {
				continue;
			}
            return new float[]{x1[0], x1[1], x2[0], x2[1]};
        }
    }



    /**
     * Simplified method for rectangular columns.
     *
     * @param phrase a <CODE>Phrase</CODE>
     * @param llx the lower left x corner
     * @param lly the lower left y corner
     * @param urx the upper right x corner
     * @param ury the upper right y corner
     * @param leading the leading
     * @param alignment the column alignment
     */
    void setSimpleColumn(final Phrase phrase, final float llx, final float lly, final float urx, final float ury, final float leading, final int alignment) {
        addText(phrase);
        setSimpleColumn(llx, lly, urx, ury, leading, alignment);
    }

    /**
     * Simplified method for rectangular columns.
     *
     * @param llx the lower left x corner
     * @param lly the lower left y corner
     * @param urx the upper right x corner
     * @param ury the upper right y corner
     * @param leading the leading
     * @param alignment the column alignment
     */
    void setSimpleColumn(final float llx, final float lly, final float urx, final float ury, final float leading, final int alignment) {
        setLeading(leading);
        this.alignment = alignment;
        setSimpleColumn(llx, lly, urx, ury);
    }

    /**
     * Simplified method for rectangular columns.
     *
     * @param llx
     * @param lly
     * @param urx
     * @param ury
     */
    void setSimpleColumn(final float llx, final float lly, final float urx, final float ury) {
        this.leftX = Math.min(llx, urx);
        this.maxY = Math.max(lly, ury);
        this.minY = Math.min(lly, ury);
        this.rightX = Math.max(llx, urx);
        this.yLine = this.maxY;
        this.rectangularWidth = this.rightX - this.leftX;
        if (this.rectangularWidth < 0) {
			this.rectangularWidth = 0;
		}
        this.rectangularMode = true;
    }

    /**
     * Sets the leading to fixed.
     *
     * @param leading the leading
     */
    public void setLeading(final float leading) {
        this.fixedLeading = leading;
        this.multipliedLeading = 0;
    }

    /**
     * Sets the leading fixed and variable. The resultant leading will be
     * fixedLeading+multipliedLeading*maxFontSize where maxFontSize is the
     * size of the biggest font in the line.
     *
     * @param fixedLeading the fixed leading
     * @param multipliedLeading the variable leading
     */
    void setLeading(final float fixedLeading, final float multipliedLeading) {
        this.fixedLeading = fixedLeading;
        this.multipliedLeading = multipliedLeading;
    }

    /**
     * Gets the fixed leading.
     *
     * @return the leading
     */
    public float getLeading() {
        return this.fixedLeading;
    }

    /**
     * Gets the variable leading.
     *
     * @return the leading
     */
    public float getMultipliedLeading() {
        return this.multipliedLeading;
    }

    /**
     * Sets the yLine. The line will be written to yLine-leading.
     *
     * @param yLine the yLine
     */
    public void setYLine(final float yLine) {
        this.yLine = yLine;
    }

    /**
     * Gets the yLine.
     *
     * @return the yLine
     */
    public float getYLine() {
        return this.yLine;
    }

    /**
     * Sets the alignment.
     *
     * @param alignment the alignment
     */
    public void setAlignment(final int alignment) {
        this.alignment = alignment;
    }

    /**
     * Gets the alignment.
     *
     * @return the alignment
     */
    public int getAlignment() {
        return this.alignment;
    }

    /**
     * Sets the first paragraph line indent.
     *
     * @param indent the indent
     */
    public void setIndent(final float indent) {
        this.indent = indent;
        this.lastWasNewline = true;
    }

    /**
     * Gets the first paragraph line indent.
     *
     * @return the indent
     */
    public float getIndent() {
        return this.indent;
    }

    /**
     * Sets the following paragraph lines indent.
     *
     * @param indent the indent
     */
    public void setFollowingIndent(final float indent) {
        this.followingIndent = indent;
        this.lastWasNewline = true;
    }

    /**
     * Gets the following paragraph lines indent.
     *
     * @return the indent
     */
    public float getFollowingIndent() {
        return this.followingIndent;
    }

    /**
     * Sets the right paragraph lines indent.
     *
     * @param indent the indent
     */
    public void setRightIndent(final float indent) {
        this.rightIndent = indent;
        this.lastWasNewline = true;
    }

    /**
     * Gets the right paragraph lines indent.
     *
     * @return the indent
     */
    public float getRightIndent() {
        return this.rightIndent;
    }

    /**
     * Outputs the lines to the document. It is equivalent to <CODE>go(false)</CODE>.
     *
     * @return returns the result of the operation. It can be <CODE>NO_MORE_TEXT</CODE>
     * and/or <CODE>NO_MORE_COLUMN</CODE>
     * @throws DocumentException on error
     */
    int go() throws DocumentException {
        return go(false);
    }

    /**
     * Outputs the lines to the document. The output can be simulated.
     * @param simulate <CODE>true</CODE> to simulate the writing to the document
     * @return returns the result of the operation. It can be <CODE>NO_MORE_TEXT</CODE>
     * and/or <CODE>NO_MORE_COLUMN</CODE>
     * @throws DocumentException on error
     */
    int go(final boolean simulate) throws DocumentException {
        if (this.composite) {
			return goComposite(simulate);
		}
        addWaitingPhrase();
        if (this.bidiLine == null) {
			return NO_MORE_TEXT;
		}
        this.descender = 0;
        this.linesWritten = 0;
        boolean dirty = false;
        float ratio = this.spaceCharRatio;
        final Object currentValues[] = new Object[2];
        PdfFont currentFont = null;
        final Float lastBaseFactor = new Float(0);
        currentValues[1] = lastBaseFactor;
        PdfDocument pdf = null;
        PdfContentByte graphics = null;
        PdfContentByte text = null;
        this.firstLineY = Float.NaN;
        int localRunDirection = PdfWriter.RUN_DIRECTION_NO_BIDI;
        if (this.runDirection != PdfWriter.RUN_DIRECTION_DEFAULT) {
			localRunDirection = this.runDirection;
		}
        if (this.canvas != null) {
            graphics = this.canvas;
            pdf = this.canvas.getPdfDocument();
            text = this.canvas.getDuplicate();
        }
        else if (!simulate) {
			throw new NullPointerException("ColumnText.go with simulate==false and text==null.");
		}
        if (!simulate) {
            if (ratio == GLOBAL_SPACE_CHAR_RATIO) {
				ratio = text.getPdfWriter().getSpaceCharRatio();
			} else if (ratio < 0.001f) {
				ratio = 0.001f;
			}
        }
        float firstIndent = 0;
        PdfLine line;
        float x1;
        int status = 0;
        while(true) {
        	firstIndent = this.lastWasNewline ? this.indent : this.followingIndent; //
        	if (this.rectangularMode) {
        		if (this.rectangularWidth <= firstIndent + this.rightIndent) {
        			status = NO_MORE_COLUMN;
        			if (this.bidiLine.isEmpty()) {
						status |= NO_MORE_TEXT;
					}
        			break;
        		}
        		if (this.bidiLine.isEmpty()) {
        			status = NO_MORE_TEXT;
        			break;
        		}
                line = this.bidiLine.processLine(this.leftX, this.rectangularWidth - firstIndent - this.rightIndent, this.alignment, localRunDirection, this.arabicOptions);
                if (line == null) {
                	status = NO_MORE_TEXT;
                	break;
                }
                final float[] maxSize = line.getMaxSize();
                if (isUseAscender() && Float.isNaN(this.firstLineY)) {
					this.currentLeading = line.getAscender();
				} else {
					this.currentLeading = Math.max(this.fixedLeading + maxSize[0] * this.multipliedLeading, maxSize[1]);
				}
                if (this.yLine > this.maxY || this.yLine - this.currentLeading < this.minY ) {
                	status = NO_MORE_COLUMN;
                	this.bidiLine.restore();
                	break;
                }
                this.yLine -= this.currentLeading;
                if (!simulate && !dirty) {
                	text.beginText();
                	dirty = true;
                }
                if (Float.isNaN(this.firstLineY)) {
					this.firstLineY = this.yLine;
				}
                updateFilledWidth(this.rectangularWidth - line.widthLeft());
                x1 = this.leftX;
        	}
            else {
               	final float yTemp = this.yLine;
               	final float xx[] = findLimitsTwoLines();
               	if (xx == null) {
               		status = NO_MORE_COLUMN;
               		if (this.bidiLine.isEmpty()) {
						status |= NO_MORE_TEXT;
					}
               		this.yLine = yTemp;
               		break;
               	}
               	if (this.bidiLine.isEmpty()) {
               		status = NO_MORE_TEXT;
               		this.yLine = yTemp;
               		break;
               	}
               	x1 = Math.max(xx[0], xx[2]);
                    final float x2 = Math.min(xx[1], xx[3]);
                    if (x2 - x1 <= firstIndent + this.rightIndent) {
						continue;
					}
                    if (!simulate && !dirty) {
                        text.beginText();
                        dirty = true;
                    }
                    line = this.bidiLine.processLine(x1, x2 - x1 - firstIndent - this.rightIndent, this.alignment, localRunDirection, this.arabicOptions);
                    if (line == null) {
                        status = NO_MORE_TEXT;
                        this.yLine = yTemp;
                        break;
                    }
                }
                if (!simulate) {
                    currentValues[0] = currentFont;
                    text.setTextMatrix(x1 + (line.isRTL() ? this.rightIndent : firstIndent) + line.indentLeft(), this.yLine);
                    pdf.writeLineToContent(line, text, graphics, currentValues, ratio);
                    currentFont = (PdfFont)currentValues[0];
                }
                this.lastWasNewline = line.isNewlineSplit();
                this.yLine -= line.isNewlineSplit() ? this.extraParagraphSpace : 0;
                ++this.linesWritten;
                this.descender = line.getDescender();
            }
        if (dirty) {
            text.endText();
            this.canvas.add(text);
        }
        return status;
    }

    /**
     * Sets the extra space between paragraphs.
     *
     * @return the extra space between paragraphs
     */
    public float getExtraParagraphSpace() {
        return this.extraParagraphSpace;
    }

    /**
     * Sets the extra space between paragraphs.
     *
     * @param extraParagraphSpace the extra space between paragraphs
     */
    public void setExtraParagraphSpace(final float extraParagraphSpace) {
        this.extraParagraphSpace = extraParagraphSpace;
    }



    /**
     * Gets the space/character extra spacing ratio for fully justified text.
     *
     * @return the space/character extra spacing ratio
     */
    public float getSpaceCharRatio() {
        return this.spaceCharRatio;
    }

    /**
     * Sets the ratio between the extra word spacing and the extra character
     * spacing when the text is fully justified.
     * Extra word spacing will grow <CODE>spaceCharRatio</CODE> times more
     * than extra character spacing.
     * If the ratio is <CODE>PdfWriter.NO_SPACE_CHAR_RATIO</CODE> then the
     * extra character spacing will be zero.
     *
     * @param spaceCharRatio the ratio between the extra word spacing and the extra character spacing
     */
    public void setSpaceCharRatio(final float spaceCharRatio) {
        this.spaceCharRatio = spaceCharRatio;
    }

    /**
     * Sets the run direction.
     *
     * @param runDirection the run direction
     */
    public void setRunDirection(final int runDirection) {
        if (runDirection < PdfWriter.RUN_DIRECTION_DEFAULT || runDirection > PdfWriter.RUN_DIRECTION_RTL) {
			throw new RuntimeException("Invalid run direction: " + runDirection);
		}
        this.runDirection = runDirection;
    }

    /**
     * Gets the run direction.
     *
     * @return the run direction
     */
    public int getRunDirection() {
        return this.runDirection;
    }

    /**
     * Gets the number of lines written.
     *
     * @return the number of lines written
     */
    public int getLinesWritten() {
        return this.linesWritten;
    }

    /**
     * Gets the arabic shaping options.
     *
     * @return the arabic shaping options
     */
    public int getArabicOptions() {
        return this.arabicOptions;
    }

    /**
     * Sets the arabic shaping options. The option can be AR_NOVOWEL,
     * AR_COMPOSEDTASHKEEL and AR_LIG.
     *
     * @param arabicOptions the arabic shaping options
     */
    public void setArabicOptions(final int arabicOptions) {
        this.arabicOptions = arabicOptions;
    }

    /**
     * Gets the biggest descender value of the last line written.
     *
     * @return the biggest descender value of the last line written
     */
    public float getDescender() {
        return this.descender;
    }

    /**
     * Gets the width that the line will occupy after writing.
     * Only the width of the first line is returned.
     *
     * @param phrase the <CODE>Phrase</CODE> containing the line
     * @param runDirection the run direction
     * @param arabicOptions the options for the arabic shaping
     * @return the width of the line
     */
    static float getWidth(final Phrase phrase, final int runDirection, final int arabicOptions) {
        final ColumnText ct = new ColumnText(null);
        ct.addText(phrase);
        ct.addWaitingPhrase();
        final PdfLine line = ct.bidiLine.processLine(0, 20000, Element.ALIGN_LEFT, runDirection, arabicOptions);
        if (line == null) {
			return 0;
		} else {
			return 20000 - line.widthLeft();
		}
    }



    /**
     * Shows a line of text. Only the first line is written.
     *
     * @param canvas where the text is to be written to
     * @param alignment the alignment. It is not influenced by the run direction
     * @param phrase the <CODE>Phrase</CODE> with the text
     * @param x the x reference position
     * @param y the y reference position
     * @param rotation the rotation to be applied in degrees counterclockwise
     * @param runDirection the run direction
     * @param arabicOptions the options for the arabic shaping
     */
    static void showTextAligned(final PdfContentByte canvas, int alignment, final Phrase phrase, final float x, final float y, final float rotation, final int runDirection, final int arabicOptions) {
        if (alignment != Element.ALIGN_LEFT && alignment != Element.ALIGN_CENTER
            && alignment != Element.ALIGN_RIGHT) {
			alignment = Element.ALIGN_LEFT;
		}
        canvas.saveState();
        final ColumnText ct = new ColumnText(canvas);
        float lly = -1;
        float ury = 2;
        float llx;
        float urx;
        switch (alignment) {
        	case Element.ALIGN_LEFT:
        		llx = 0;
        		urx = 20000;
        		break;
        	case Element.ALIGN_RIGHT:
        		llx = -20000;
        		urx = 0;
        		break;
        	default:
        		llx = -20000;
        		urx = 20000;
        		break;
        }
        if (rotation == 0) {
        	llx += x;
        	lly += y;
        	urx += x;
        	ury += y;
        }
        else {
            final double alpha = rotation * Math.PI / 180.0;
            final float cos = (float)Math.cos(alpha);
            final float sin = (float)Math.sin(alpha);
            canvas.concatCTM(cos, sin, -sin, cos, x, y);
        }
        ct.setSimpleColumn(phrase, llx, lly, urx, ury, 2, alignment);
        if (runDirection == PdfWriter.RUN_DIRECTION_RTL) {
            if (alignment == Element.ALIGN_LEFT) {
				alignment = Element.ALIGN_RIGHT;
			} else if (alignment == Element.ALIGN_RIGHT) {
				alignment = Element.ALIGN_LEFT;
			}
        }
        ct.setAlignment(alignment);
        ct.setArabicOptions(arabicOptions);
        ct.setRunDirection(runDirection);
        try {
            ct.go();
        }
        catch (final DocumentException e) {
            throw new ExceptionConverter(e);
        }
        canvas.restoreState();
    }

    /**
     * Shows a line of text. Only the first line is written.
     *
     * @param canvas where the text is to be written to
     * @param alignment the alignment
     * @param phrase the <CODE>Phrase</CODE> with the text
     * @param x the x reference position
     * @param y the y reference position
     * @param rotation the rotation to be applied in degrees counterclockwise
     */
    static void showTextAligned(final PdfContentByte canvas, final int alignment, final Phrase phrase, final float x, final float y, final float rotation) {
        showTextAligned(canvas, alignment, phrase, x, y, rotation, PdfWriter.RUN_DIRECTION_NO_BIDI, 0);
    }

    private int goComposite(final boolean simulate) throws DocumentException {
        if (!this.rectangularMode) {
			throw new DocumentException("Irregular columns are not supported in composite mode.");
		}
        this.linesWritten = 0;
        this.descender = 0;
        boolean firstPass = this.adjustFirstLine;

        main_loop:
        while (true) {
            if (this.compositeElements.isEmpty()) {
				return NO_MORE_TEXT;
			}
            final Element element = (Element)this.compositeElements.getFirst();
            if (element.type() == Element.PARAGRAPH) {
                final Paragraph para = (Paragraph)element;
                int status = 0;
                for (int keep = 0; keep < 2; ++keep) {
                    final float lastY = this.yLine;
                    boolean createHere = false;
                    if (this.compositeColumn == null) {
                        this.compositeColumn = new ColumnText(this.canvas);
                        this.compositeColumn.setUseAscender(firstPass ? this.useAscender : false);
                        this.compositeColumn.setAlignment(para.getAlignment());
                        this.compositeColumn.setIndent(para.getIndentationLeft() + para.getFirstLineIndent());
                        this.compositeColumn.setExtraParagraphSpace(para.getExtraParagraphSpace());
                        this.compositeColumn.setFollowingIndent(para.getIndentationLeft());
                        this.compositeColumn.setRightIndent(para.getIndentationRight());
                        this.compositeColumn.setLeading(para.getLeading(), para.getMultipliedLeading());
                        this.compositeColumn.setRunDirection(this.runDirection);
                        this.compositeColumn.setArabicOptions(this.arabicOptions);
                        this.compositeColumn.setSpaceCharRatio(this.spaceCharRatio);
                        this.compositeColumn.addText(para);
                        if (!firstPass) {
                            this.yLine -= para.getSpacingBefore();
                        }
                        createHere = true;
                    }
                    this.compositeColumn.leftX = this.leftX;
                    this.compositeColumn.rightX = this.rightX;
                    this.compositeColumn.yLine = this.yLine;
                    this.compositeColumn.rectangularWidth = this.rectangularWidth;
                    this.compositeColumn.rectangularMode = this.rectangularMode;
                    this.compositeColumn.minY = this.minY;
                    this.compositeColumn.maxY = this.maxY;
                    final boolean keepCandidate = para.getKeepTogether() && createHere && !firstPass;
                    status = this.compositeColumn.go(simulate || keepCandidate && keep == 0);
                    updateFilledWidth(this.compositeColumn.filledWidth);
                    if ((status & NO_MORE_TEXT) == 0 && keepCandidate) {
                        this.compositeColumn = null;
                        this.yLine = lastY;
                        return NO_MORE_COLUMN;
                    }
                    if (simulate || !keepCandidate) {
						break;
					}
                    if (keep == 0) {
                        this.compositeColumn = null;
                        this.yLine = lastY;
                    }
                }
                firstPass = false;
                this.yLine = this.compositeColumn.yLine;
                this.linesWritten += this.compositeColumn.linesWritten;
                this.descender = this.compositeColumn.descender;
                if ((status & NO_MORE_TEXT) != 0) {
                    this.compositeColumn = null;
                    this.compositeElements.removeFirst();
                    this.yLine -= para.getSpacingAfter();
                }
                if ((status & NO_MORE_COLUMN) != 0) {
                    return NO_MORE_COLUMN;
                }
            }
            else if (element.type() == Element.LIST) {
                com.lowagie.text.List list = (com.lowagie.text.List)element;
                ArrayList items = list.getItems();
                ListItem item = null;
                float listIndentation = list.getIndentationLeft();
                int count = 0;
                final Stack stack = new Stack();
                for (int k = 0; k < items.size(); ++k) {
                    final Object obj = items.get(k);
                    if (obj instanceof ListItem) {
                        if (count == this.listIdx) {
                            item = (ListItem)obj;
                            break;
                        } else {
							++count;
						}
                    }
                    else if (obj instanceof com.lowagie.text.List) {
                        stack.push(new Object[]{list, new Integer(k), new Float(listIndentation)});
                        list = (com.lowagie.text.List)obj;
                        items = list.getItems();
                        listIndentation += list.getIndentationLeft();
                        k = -1;
                        continue;
                    }
                    if (k == items.size() - 1) {
                        if (!stack.isEmpty()) {
                            final Object objs[] = (Object[])stack.pop();
                            list = (com.lowagie.text.List)objs[0];
                            items = list.getItems();
                            k = ((Integer)objs[1]).intValue();
                            listIndentation = ((Float)objs[2]).floatValue();
                        }
                    }
                }
                int status = 0;
                for (int keep = 0; keep < 2; ++keep) {
                    final float lastY = this.yLine;
                    boolean createHere = false;
                    if (this.compositeColumn == null) {
                        if (item == null) {
                            this.listIdx = 0;
                            this.compositeElements.removeFirst();
                            continue main_loop;
                        }
                        this.compositeColumn = new ColumnText(this.canvas);
                        this.compositeColumn.setUseAscender(firstPass ? this.useAscender : false);
                        this.compositeColumn.setAlignment(item.getAlignment());
                        this.compositeColumn.setIndent(item.getIndentationLeft() + listIndentation + item.getFirstLineIndent());
                        this.compositeColumn.setExtraParagraphSpace(item.getExtraParagraphSpace());
                        this.compositeColumn.setFollowingIndent(this.compositeColumn.getIndent());
                        this.compositeColumn.setRightIndent(item.getIndentationRight() + list.getIndentationRight());
                        this.compositeColumn.setLeading(item.getLeading(), item.getMultipliedLeading());
                        this.compositeColumn.setRunDirection(this.runDirection);
                        this.compositeColumn.setArabicOptions(this.arabicOptions);
                        this.compositeColumn.setSpaceCharRatio(this.spaceCharRatio);
                        this.compositeColumn.addText(item);
                        if (!firstPass) {
                            this.yLine -= item.getSpacingBefore();
                        }
                        createHere = true;
                    }
                    this.compositeColumn.leftX = this.leftX;
                    this.compositeColumn.rightX = this.rightX;
                    this.compositeColumn.yLine = this.yLine;
                    this.compositeColumn.rectangularWidth = this.rectangularWidth;
                    this.compositeColumn.rectangularMode = this.rectangularMode;
                    this.compositeColumn.minY = this.minY;
                    this.compositeColumn.maxY = this.maxY;
                    final boolean keepCandidate = item.getKeepTogether() && createHere && !firstPass;
                    status = this.compositeColumn.go(simulate || keepCandidate && keep == 0);
                    updateFilledWidth(this.compositeColumn.filledWidth);
                    if ((status & NO_MORE_TEXT) == 0 && keepCandidate) {
                        this.compositeColumn = null;
                        this.yLine = lastY;
                        return NO_MORE_COLUMN;
                    }
                    if (simulate || !keepCandidate) {
						break;
					}
                    if (keep == 0) {
                        this.compositeColumn = null;
                        this.yLine = lastY;
                    }
                }
                firstPass = false;
                this.yLine = this.compositeColumn.yLine;
                this.linesWritten += this.compositeColumn.linesWritten;
                this.descender = this.compositeColumn.descender;
                if (!Float.isNaN(this.compositeColumn.firstLineY) && !this.compositeColumn.firstLineYDone) {
                    if (!simulate) {
						showTextAligned(this.canvas, Element.ALIGN_LEFT, new Phrase(item.getListSymbol()), this.compositeColumn.leftX + listIndentation, this.compositeColumn.firstLineY, 0);
					}
                    this.compositeColumn.firstLineYDone = true;
                }
                if ((status & NO_MORE_TEXT) != 0) {
                    this.compositeColumn = null;
                    ++this.listIdx;
                    this.yLine -= item.getSpacingAfter();
                }
                if ((status & NO_MORE_COLUMN) != 0) {
					return NO_MORE_COLUMN;
				}
            }
            else if (element.type() == Element.PTABLE) {
            	// don't write anything in the current column if there's no more space available
                if (this.yLine < this.minY || this.yLine > this.maxY) {
					return NO_MORE_COLUMN;
				}

                // get the PdfPTable element
                PdfPTable table = (PdfPTable)element;
                // we ignore tables without a body
                if (table.size() <= table.getHeaderRows()) {
                    this.compositeElements.removeFirst();
                    continue;
                }

                // offsets
                float yTemp = this.yLine;
                if (!firstPass && this.listIdx == 0) {
					yTemp -= table.spacingBefore();
				}
                final float yLineWrite = yTemp;

                // don't write anything in the current column if there's no more space available
                if (yTemp < this.minY || yTemp > this.maxY) {
					return NO_MORE_COLUMN;
				}

                // coordinates
                this.currentLeading = 0;
                float x1 = this.leftX;
                float tableWidth;
                if (table.isLockedWidth()) {
                    tableWidth = table.getTotalWidth();
                    updateFilledWidth(tableWidth);
                }
                else {
                    tableWidth = this.rectangularWidth * table.getWidthPercentage() / 100f;
                    table.setTotalWidth(tableWidth);
                }

                // how many header rows are real header rows; how many are footer rows?
                final int headerRows = table.getHeaderRows();
                int footerRows = table.getFooterRows();
                if (footerRows > headerRows) {
					footerRows = headerRows;
				}
                final int realHeaderRows = headerRows - footerRows;
                final float headerHeight = table.getHeaderHeight();
                final float footerHeight = table.getFooterHeight();

                // make sure the header and footer fit on the page
                final boolean skipHeader = !firstPass && table.isSkipFirstHeader() && this.listIdx <= headerRows;
                if (!skipHeader) {
                    yTemp -= headerHeight;
                    if (yTemp < this.minY || yTemp > this.maxY) {
                        if (firstPass) {
                            this.compositeElements.removeFirst();
                            continue;
                        }
                        return NO_MORE_COLUMN;
                    }
                }

                // how many real rows (not header or footer rows) fit on a page?
                int k;
                if (this.listIdx < headerRows) {
					this.listIdx = headerRows;
				}
                if (!table.isComplete()) {
					yTemp -= footerHeight;
				}
                for (k = this.listIdx; k < table.size(); ++k) {
                    final float rowHeight = table.getRowHeight(k);
                    if (yTemp - rowHeight < this.minY) {
						break;
					}
                    yTemp -= rowHeight;
                }
                if (!table.isComplete()) {
					yTemp += footerHeight;
				}
                // either k is the first row that doesn't fit on the page (break);
                if (k < table.size()) {
                	if (table.isSplitRows() && (!table.isSplitLate() || k == this.listIdx && firstPass)) {
                		if (!this.splittedRow) {
                            this.splittedRow = true;
                            table = new PdfPTable(table);
                            this.compositeElements.set(0, table);
                            final ArrayList rows = table.getRows();
                            for (int i = headerRows; i < this.listIdx; ++i) {
								rows.set(i, null);
							}
                        }
                        final float h = yTemp - this.minY;
                        final PdfPRow newRow = table.getRow(k).splitRow(table, k, h);
                        if (newRow == null) {
                            if (k == this.listIdx) {
								return NO_MORE_COLUMN;
							}
                        }
                        else {
                            yTemp = this.minY;
                            table.getRows().add(++k, newRow);
                        }
                    }
                    else if (!table.isSplitRows() && k == this.listIdx && firstPass) {
                        this.compositeElements.removeFirst();
                        this.splittedRow = false;
                        continue;
                    }
                    else if (k == this.listIdx && !firstPass && (!table.isSplitRows() || table.isSplitLate()) && (table.getFooterRows() == 0 || table.isComplete())) {
						return NO_MORE_COLUMN;
					}
                }
                // or k is the number of rows in the table (for loop was done).
                firstPass = false;
                // we draw the table (for real now)
                if (!simulate) {
                	// set the alignment
                    switch (table.getHorizontalAlignment()) {
                        case Element.ALIGN_LEFT:
                            break;
                        case Element.ALIGN_RIGHT:
                            x1 += this.rectangularWidth - tableWidth;
                            break;
                        default:
                            x1 += (this.rectangularWidth - tableWidth) / 2f;
                    }
                    // copy the rows that fit on the page in a new table nt
                    final PdfPTable nt = PdfPTable.shallowCopy(table);
                    final ArrayList sub = nt.getRows();

                    // first we add the real header rows (if necessary)
                    if (!skipHeader) {
                        for (int j = 0; j < realHeaderRows; ++j) {
                        	final PdfPRow headerRow = table.getRow(j);
                            sub.add(headerRow);
                        }
                    } else {
						nt.setHeaderRows(footerRows);
					}
                    // then we add the real content
                    sub.addAll(table.getRows(this.listIdx, k));
                    // if k < table.size(), we must indicate that the new table is complete;
                    // otherwise no footers will be added (because iText thinks the table continues on the same page)
                    boolean showFooter = !table.isSkipLastFooter();
                    if (k < table.size()) {
                    	nt.setComplete(true);
                    	showFooter = true;
                    }
                    // we add the footer rows if necessary (not for incomplete tables)
                    for (int j = 0; j < footerRows && nt.isComplete() && showFooter; ++j) {
						sub.add(table.getRow(j + realHeaderRows));
					}

                    // we need a correction if the last row needs to be extended
                    float rowHeight = 0;
                    final PdfPRow last = (PdfPRow)sub.get(sub.size() - 1 - footerRows);
                    if (table.isExtendLastRow()) {
                        rowHeight = last.getMaxHeights();
                        last.setMaxHeights(yTemp - this.minY + rowHeight);
                        yTemp = this.minY;
                    }

                    // now we render the rows of the new table
                    if (this.canvases != null) {
						nt.writeSelectedRows(0, -1, x1, yLineWrite, this.canvases);
					} else {
						nt.writeSelectedRows(0, -1, x1, yLineWrite, this.canvas);
					}
                    if (table.isExtendLastRow()) {
                        last.setMaxHeights(rowHeight);
                    }
                }
                else if (table.isExtendLastRow() && this.minY > PdfPRow.BOTTOM_LIMIT) {
					yTemp = this.minY;
				}
                this.yLine = yTemp;
                if (!(skipHeader || table.isComplete())) {
					this.yLine += footerHeight;
				}
                if (k >= table.size()) {
                    this.yLine -= table.spacingAfter();
                    this.compositeElements.removeFirst();
                    this.splittedRow = false;
                    this.listIdx = 0;
                }
                else {
                    if (this.splittedRow) {
                        final ArrayList rows = table.getRows();
                        for (int i = this.listIdx; i < k; ++i) {
							rows.set(i, null);
						}
                    }
                    this.listIdx = k;
                    return NO_MORE_COLUMN;
                }
            }
            else if (element.type() == Element.YMARK) {
                if (!simulate) {
                    final DrawInterface zh = (DrawInterface)element;
                    zh.draw(this.canvas, this.leftX, this.minY, this.rightX, this.maxY, this.yLine);
                }
                this.compositeElements.removeFirst();
            } else {
				this.compositeElements.removeFirst();
			}
        }
    }

    /**
     * Gets the canvas.
     * If a set of four canvases exists, the TEXTCANVAS is returned.
     *
     * @return a PdfContentByte.
     */
    public PdfContentByte getCanvas() {
        return this.canvas;
    }

    /**
     * Sets the canvas.
     * If before a set of four canvases was set, it is being unset.
     *
     * @param canvas
     */
    public void setCanvas(final PdfContentByte canvas) {
        this.canvas = canvas;
        this.canvases = null;
        if (this.compositeColumn != null) {
			this.compositeColumn.setCanvas(canvas);
		}
    }

    /**
     * Sets the canvases.
     *
     * @param canvases
     */
    public void setCanvases(final PdfContentByte[] canvases) {
        this.canvases = canvases;
        this.canvas = canvases[PdfPTable.TEXTCANVAS];
        if (this.compositeColumn != null) {
			this.compositeColumn.setCanvases(canvases);
		}
    }

    /**
     * Gets the canvases.
     *
     * @return an array of PdfContentByte
     */
    public PdfContentByte[] getCanvases() {
        return this.canvases;
    }

    /**
     * Checks if the element has a height of 0.
     *
     * @return true or false
     * @since 2.1.2
     */
    boolean zeroHeightElement() {
        return this.composite && !this.compositeElements.isEmpty() && ((Element)this.compositeElements.getFirst()).type() == Element.YMARK;
    }

    /**
     * Checks if UseAscender is enabled/disabled.
     *
     * @return true is the adjustment of the first line height is based on max ascender.
     */
    public boolean isUseAscender() {
        return this.useAscender;
    }

    /**
     * Enables/Disables adjustment of first line height based on max ascender.
     *
     * @param useAscender	enable adjustment if true
     */
    public void setUseAscender(final boolean useAscender) {
        this.useAscender = useAscender;
    }



    /**
     * Gets the real width used by the largest line.
     *
     * @return the real width used by the largest line
     */
    public float getFilledWidth() {
        return this.filledWidth;
    }

    /**
     * Sets the real width used by the largest line.
     * Only used to set it to zero to start another measurement.
     *
     * @param filledWidth the real width used by the largest line
     */
    public void setFilledWidth(final float filledWidth) {
        this.filledWidth = filledWidth;
    }

    /**
     * Replaces the <CODE>filledWidth</CODE> if greater than the existing one.
     *
     * @param w the new <CODE>filledWidth</CODE> if greater than the existing one
     */
    private void updateFilledWidth(final float w) {
        if (w > this.filledWidth) {
			this.filledWidth = w;
		}
    }


    /**
     * Gets the first line adjustment property.
     *
     * @return the first line adjustment property.
     */
    public boolean isAdjustFirstLine() {
        return this.adjustFirstLine;
    }

    /**
     * Sets the first line adjustment.
     * Some objects have properties, like spacing before, that behave
     * differently if the object is the first to be written after go() or not.
     * The first line adjustment is <CODE>true</CODE> by default but can be
     * changed if several objects are to be placed one after the other in the
     * same column calling go() several times.
     *
     * @param adjustFirstLine <CODE>true</CODE> to adjust the first line, <CODE>false</CODE> otherwise
     */
    public void setAdjustFirstLine(final boolean adjustFirstLine) {
        this.adjustFirstLine = adjustFirstLine;
    }
}
