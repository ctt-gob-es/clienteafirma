/*
 * $Id: SAXiTextHandler.java 3427 2008-05-24 18:32:31Z xlv $
 *
 * Copyright 2001, 2002 by Bruno Lowagie.
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

package com.lowagie.text.xml;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EmptyStackException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Properties;
import java.util.Stack;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import com.lowagie.text.Anchor;
import com.lowagie.text.Annotation;
import com.lowagie.text.BadElementException;
import com.lowagie.text.Cell;
import com.lowagie.text.Chapter;
import com.lowagie.text.Chunk;
import com.lowagie.text.DocListener;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ElementTags;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.List;
import com.lowagie.text.ListItem;
import com.lowagie.text.Meta;
import com.lowagie.text.PageSize;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Rectangle;
import com.lowagie.text.Section;
import com.lowagie.text.Table;
import com.lowagie.text.TextElementArray;
import com.lowagie.text.factories.ElementFactory;
import com.lowagie.text.html.HtmlTagMap;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.pdf.draw.LineSeparator;
import com.lowagie.text.xml.simpleparser.EntitiesToSymbol;

/**
 * This class is a Handler that controls the iText XML to PDF conversion.
 * Subclass it, if you want to change the way iText translates XML to PDF.
 */

class SAXiTextHandler extends DefaultHandler {

    /** This is the resulting document. */
    private final DocListener document;

    /**
     * This is a <CODE>Stack</CODE> of objects, waiting to be added to the
     * document.
     */
    private final Stack stack;



    /** This is the current chunk to which characters can be added. */
    private Chunk currentChunk = null;

    /** This is the current chunk to which characters can be added. */
    private boolean ignore = false;

    /**
     * This is a flag that can be set, if you want to open and close the
     * Document-object yourself.
     */
    private boolean controlOpenClose = true;

    /** current margin of a page. */
    private float topMargin = 36;

    /** current margin of a page. */
    private float rightMargin = 36;

    /** current margin of a page. */
    private float leftMargin = 36;

    /** current margin of a page. */
    private float bottomMargin = 36;

    /**
     * @param document
     */
    SAXiTextHandler(final DocListener document) {
        super();
        this.document = document;
        this.stack = new Stack();
    }

    /** This hashmap contains all the custom keys and peers. */
    protected HashMap myTags;

    /**
     * @param document
     * @param myTags
     */
    private SAXiTextHandler(final DocListener document, final HtmlTagMap myTags) {
        this(document);
        this.myTags = myTags;
    }



    /**
     * @param document
     * @param myTags
     */
    SAXiTextHandler(final DocListener document, final HashMap myTags) {
        this(document);
        this.myTags = myTags;
    }

    /**
     * Sets the parameter that allows you to enable/disable the control over the
     * Document.open() and Document.close() method.
     * <P>
     * If you set this parameter to true (= default), the parser will open the
     * Document object when the start-root-tag is encountered and close it when
     * the end-root-tag is met. If you set it to false, you have to open and
     * close the Document object yourself.
     *
     * @param controlOpenClose
     *            set this to false if you plan to open/close the Document
     *            yourself
     */

    public void setControlOpenClose(final boolean controlOpenClose) {
        this.controlOpenClose = controlOpenClose;
    }

    /**
     * This method gets called when a start tag is encountered.
     *
     * @param uri
     *            the Uniform Resource Identifier
     * @param lname
     *            the local name (without prefix), or the empty string if
     *            Namespace processing is not being performed.
     * @param name
     *            the name of the tag that is encountered
     * @param attrs
     *            the list of attributes
     */

    @Override
	public void startElement(final String uri, final String lname, final String name,
            final Attributes attrs) {

        final Properties attributes = new Properties();
        if (attrs != null) {
            for (int i = 0; i < attrs.getLength(); i++) {
                final String attribute = attrs.getQName(i);
                attributes.setProperty(attribute, attrs.getValue(i));
            }
        }
        handleStartingTags(name, attributes);
    }

    /**
     * This method deals with the starting tags.
     *
     * @param name
     *            the name of the tag
     * @param attributes
     *            the list of attributes
     */

    void handleStartingTags(final String name, final Properties attributes) {
        // System.err.println("Start: " + name);
        if (this.ignore || ElementTags.IGNORE.equals(name)) {
            this.ignore = true;
            return;
        }

        // maybe there is some meaningful data that wasn't between tags
        if (this.currentChunk != null) {
            TextElementArray current;
            try {
                current = (TextElementArray) this.stack.pop();
            } catch (final EmptyStackException ese) {
            	if (this.bf == null) {
            		current = new Paragraph("", new Font());
            	}
            	else {
            		current = new Paragraph("", new Font(this.bf));
            	}
            }
            current.add(this.currentChunk);
            this.stack.push(current);
            this.currentChunk = null;
        }

        // chunks
        if (ElementTags.CHUNK.equals(name)) {
            this.currentChunk = ElementFactory.getChunk(attributes);
            if (this.bf != null) {
            	this.currentChunk.setFont(new Font(this.bf));
            }
            return;
        }

        // symbols
        if (ElementTags.ENTITY.equals(name)) {
            Font f = new Font();
            if (this.currentChunk != null) {
                handleEndingTags(ElementTags.CHUNK);
                f = this.currentChunk.getFont();
            }
            this.currentChunk = EntitiesToSymbol.get(attributes.getProperty(ElementTags.ID),
                    f);
            return;
        }

        // phrases
        if (ElementTags.PHRASE.equals(name)) {
            this.stack.push(ElementFactory.getPhrase(attributes));
            return;
        }

        // anchors
        if (ElementTags.ANCHOR.equals(name)) {
            this.stack.push(ElementFactory.getAnchor(attributes));
            return;
        }

        // paragraphs and titles
        if (ElementTags.PARAGRAPH.equals(name) || ElementTags.TITLE.equals(name)) {
            this.stack.push(ElementFactory.getParagraph(attributes));
            return;
        }

        // lists
        if (ElementTags.LIST.equals(name)) {
            this.stack.push(ElementFactory.getList(attributes));
            return;
        }

        // listitems
        if (ElementTags.LISTITEM.equals(name)) {
            this.stack.push(ElementFactory.getListItem(attributes));
            return;
        }

        // cells
        if (ElementTags.CELL.equals(name)) {
            this.stack.push(ElementFactory.getCell(attributes));
            return;
        }

        // tables
        if (ElementTags.TABLE.equals(name)) {
            final Table table = ElementFactory.getTable(attributes);
            final float widths[] = table.getProportionalWidths();
            for (int i = 0; i < widths.length; i++) {
                if (widths[i] == 0) {
                    widths[i] = 100.0f / widths.length;
                }
            }
            try {
                table.setWidths(widths);
            } catch (final BadElementException bee) {
                // this shouldn't happen
                throw new ExceptionConverter(bee);
            }
            this.stack.push(table);
            return;
        }

        // sections
        if (ElementTags.SECTION.equals(name)) {
            final Element previous = (Element) this.stack.pop();
            Section section;
            try {
                section = ElementFactory.getSection((Section) previous, attributes);
            } catch (final ClassCastException cce) {
                throw new ExceptionConverter(cce);
            }
            this.stack.push(previous);
            this.stack.push(section);
            return;
        }

        // chapters
        if (ElementTags.CHAPTER.equals(name)) {
            this.stack.push(ElementFactory.getChapter(attributes));
            return;
        }

        // images
        if (ElementTags.IMAGE.equals(name)) {
            try {
                final Image img = ElementFactory.getImage(attributes);
                try {
                	addImage(img);
                    return;
                } catch (final EmptyStackException ese) {
                    // if there is no element on the stack, the Image is added
                    // to the document
                    try {
                        this.document.add(img);
                    } catch (final DocumentException de) {
                        throw new ExceptionConverter(de);
                    }
                    return;
                }
            } catch (final Exception e) {
                throw new ExceptionConverter(e);
            }
        }

        // annotations
        if (ElementTags.ANNOTATION.equals(name)) {
            final Annotation annotation = ElementFactory.getAnnotation(attributes);
            TextElementArray current;
            try {
                try {
                    current = (TextElementArray) this.stack.pop();
                    try {
                        current.add(annotation);
                    } catch (final Exception e) {
                        this.document.add(annotation);
                    }
                    this.stack.push(current);
                } catch (final EmptyStackException ese) {
                    this.document.add(annotation);
                }
                return;
            } catch (final DocumentException de) {
                throw new ExceptionConverter(de);
            }
        }

        // newlines
        if (isNewline(name)) {
            TextElementArray current;
            try {
                current = (TextElementArray) this.stack.pop();
                current.add(Chunk.NEWLINE);
                this.stack.push(current);
            } catch (final EmptyStackException ese) {
                if (this.currentChunk == null) {
                    try {
                        this.document.add(Chunk.NEWLINE);
                    } catch (final DocumentException de) {
                        throw new ExceptionConverter(de);
                    }
                } else {
                    this.currentChunk.append("\n");
                }
            }
            return;
        }

        // newpage
        if (isNewpage(name)) {
            TextElementArray current;
            try {
                current = (TextElementArray) this.stack.pop();
                final Chunk newPage = new Chunk("");
                newPage.setNewPage();
                if (this.bf != null) {
                	newPage.setFont(new Font(this.bf));
                }
                current.add(newPage);
                this.stack.push(current);
            } catch (final EmptyStackException ese) {
                this.document.newPage();
            }
            return;
        }

        if (ElementTags.HORIZONTALRULE.equals(name)) {
            TextElementArray current;
            final LineSeparator hr = new LineSeparator(1.0f, 100.0f, null, Element.ALIGN_CENTER, 0);
            try {
                current = (TextElementArray) this.stack.pop();
                current.add(hr);
                this.stack.push(current);
            } catch (final EmptyStackException ese) {
                try {
                    this.document.add(hr);
                } catch (final DocumentException de) {
                    throw new ExceptionConverter(de);
                }
            }
            return;
        }

        // documentroot
        if (isDocumentRoot(name)) {
            String key;
            String value;
            // pagesize and orientation specific code suggested by Samuel Gabriel
            // Updated by Ricardo Coutinho. Only use if set in html!
			Rectangle pageSize = null;
			String orientation = null;
            for (final Object element : attributes.keySet()) {
                key = (String) element;
                value = attributes.getProperty(key);
                try {
                    // margin specific code suggested by Reza Nasiri
                    if (ElementTags.LEFT.equalsIgnoreCase(key)) {
						this.leftMargin = Float.parseFloat(value + "f");
					}
                    if (ElementTags.RIGHT.equalsIgnoreCase(key)) {
						this.rightMargin = Float.parseFloat(value + "f");
					}
                    if (ElementTags.TOP.equalsIgnoreCase(key)) {
						this.topMargin = Float.parseFloat(value + "f");
					}
                    if (ElementTags.BOTTOM.equalsIgnoreCase(key)) {
						this.bottomMargin = Float.parseFloat(value + "f");
					}
                } catch (final Exception ex) {
                    throw new ExceptionConverter(ex);
                }
                if (ElementTags.PAGE_SIZE.equals(key)) {
                    try {
                        final String pageSizeName = value;
                        final Field pageSizeField = PageSize.class
                                .getField(pageSizeName);
                        pageSize = (Rectangle) pageSizeField.get(null);
                    } catch (final Exception ex) {
                        throw new ExceptionConverter(ex);
                    }
                } else if (ElementTags.ORIENTATION.equals(key)) {
                    try {
                        if ("landscape".equals(value)) {
                            orientation = "landscape";
                        }
                    } catch (final Exception ex) {
                        throw new ExceptionConverter(ex);
                    }
                } else {
                    try {
                        this.document.add(new Meta(key, value));
                    } catch (final DocumentException de) {
                        throw new ExceptionConverter(de);
                    }
                }
            }
            if(pageSize != null) {
            	if ("landscape".equals(orientation)) {
            		pageSize = pageSize.rotate();
            	}
            	this.document.setPageSize(pageSize);
            }
            this.document.setMargins(this.leftMargin, this.rightMargin, this.topMargin,
                    this.bottomMargin);

            if (this.controlOpenClose) {
				this.document.open();
			}
        }

    }

    private void addImage(final Image img) throws EmptyStackException {
        // if there is an element on the stack...
        Object current = this.stack.pop();
        // ...and it's a Chapter or a Section, the Image can be
        // added directly
        if (current instanceof Chapter
                || current instanceof Section
                || current instanceof Cell) {
            ((TextElementArray) current).add(img);
            this.stack.push(current);
            return;
        }
        // ...if not, we need to to a lot of stuff
        else {
            final Stack newStack = new Stack();
            while (!(current instanceof Chapter
                    || current instanceof Section || current instanceof Cell)) {
                newStack.push(current);
                if (current instanceof Anchor) {
                    img.setAnnotation(new Annotation(0, 0, 0,
                            0, ((Anchor) current).getReference()));
                }
                current = this.stack.pop();
            }
            ((TextElementArray) current).add(img);
            this.stack.push(current);
            while (!newStack.empty()) {
                this.stack.push(newStack.pop());
            }
            return;
        }
    }

    /**
     * This method gets called when ignorable white space encountered.
     *
     * @param ch
     *            an array of characters
     * @param start
     *            the start position in the array
     * @param length
     *            the number of characters to read from the array
     */

    @Override
	public void ignorableWhitespace(final char[] ch, final int start, final int length) {
        characters(ch, start, length);
    }

    /**
     * This method gets called when characters are encountered.
     *
     * @param ch
     *            an array of characters
     * @param start
     *            the start position in the array
     * @param length
     *            the number of characters to read from the array
     */

    @Override
	public void characters(final char[] ch, final int start, final int length) {

        if (this.ignore) {
			return;
		}

        final String content = new String(ch, start, length);
        // System.err.println("'" + content + "'");

        if (content.trim().length() == 0 && content.indexOf(' ') < 0) {
            return;
        }

        final StringBuffer buf = new StringBuffer();
        final int len = content.length();
        char character;
        boolean newline = false;
        for (int i = 0; i < len; i++) {
            switch (character = content.charAt(i)) {
            case ' ':
                if (!newline) {
                    buf.append(character);
                }
                break;
            case '\n':
                if (i > 0) {
                    newline = true;
                    buf.append(' ');
                }
                break;
            case '\r':
                break;
            case '\t':
                break;
            default:
                newline = false;
                buf.append(character);
            }
        }
        if (this.currentChunk == null) {
        	if (this.bf == null) {
        		this.currentChunk = new Chunk(buf.toString());
        	}
        	else {
        		this.currentChunk = new Chunk(buf.toString(), new Font(this.bf));
        	}
        } else {
            this.currentChunk.append(buf.toString());
        }
    }

    private BaseFont bf = null;

    /**
     * Sets the font that has to be used.
     * @param bf
     */
    public void setBaseFont(final BaseFont bf) {
    	this.bf = bf;
    }

    /**
     * This method gets called when an end tag is encountered.
     *
     * @param uri
     *            the Uniform Resource Identifier
     * @param lname
     *            the local name (without prefix), or the empty string if
     *            Namespace processing is not being performed.
     * @param name
     *            the name of the tag that ends
     */

    @Override
	public void endElement(final String uri, final String lname, final String name) {
        handleEndingTags(name);
    }

    /**
     * This method deals with the starting tags.
     *
     * @param name
     *            the name of the tag
     */

    void handleEndingTags(final String name) {

        // System.err.println("Stop: " + name);

        if (ElementTags.IGNORE.equals(name)) {
            this.ignore = false;
            return;
        }
        if (this.ignore) {
			return;
		}
        // tags that don't have any content
        if (isNewpage(name) || ElementTags.ANNOTATION.equals(name) || ElementTags.IMAGE.equals(name)
                || isNewline(name)) {
            return;
        }

        try {
            // titles of sections and chapters
            if (ElementTags.TITLE.equals(name)) {
                final Paragraph current = (Paragraph) this.stack.pop();
                if (this.currentChunk != null) {
                    current.add(this.currentChunk);
                    this.currentChunk = null;
                }
                final Section previous = (Section) this.stack.pop();
                previous.setTitle(current);
                this.stack.push(previous);
                return;
            }

            // all other endtags
            if (this.currentChunk != null) {
                TextElementArray current;
                try {
                    current = (TextElementArray) this.stack.pop();
                } catch (final EmptyStackException ese) {
                    current = new Paragraph();
                }
                current.add(this.currentChunk);
                this.stack.push(current);
                this.currentChunk = null;
            }

            // chunks
            if (ElementTags.CHUNK.equals(name)) {
                return;
            }

            // phrases, anchors, lists, tables
            if (ElementTags.PHRASE.equals(name) || ElementTags.ANCHOR.equals(name) || ElementTags.LIST.equals(name)
                    || ElementTags.PARAGRAPH.equals(name)) {
                final Element current = (Element) this.stack.pop();
                try {
                    final TextElementArray previous = (TextElementArray) this.stack.pop();
                    previous.add(current);
                    this.stack.push(previous);
                } catch (final EmptyStackException ese) {
                    this.document.add(current);
                }
                return;
            }

            // listitems
            if (ElementTags.LISTITEM.equals(name)) {
                final ListItem listItem = (ListItem) this.stack.pop();
                final List list = (List) this.stack.pop();
                list.add(listItem);
                this.stack.push(list);
            }

            // tables
            if (ElementTags.TABLE.equals(name)) {
                final Table table = (Table) this.stack.pop();
                try {
                    final TextElementArray previous = (TextElementArray) this.stack.pop();
                    previous.add(table);
                    this.stack.push(previous);
                } catch (final EmptyStackException ese) {
                    this.document.add(table);
                }
                return;
            }

            // rows
            if (ElementTags.ROW.equals(name)) {
                final ArrayList cells = new ArrayList();
                int columns = 0;
                Table table;
                Cell cell;
                while (true) {
                    final Element element = (Element) this.stack.pop();
                    if (element.type() == Element.CELL) {
                        cell = (Cell) element;
                        columns += cell.getColspan();
                        cells.add(cell);
                    } else {
                        table = (Table) element;
                        break;
                    }
                }
                if (table.getColumns() < columns) {
                    table.addColumns(columns - table.getColumns());
                }
                Collections.reverse(cells);
                String width;
                final float[] cellWidths = new float[columns];
                final boolean[] cellNulls = new boolean[columns];
                for (int i = 0; i < columns; i++) {
                    cellWidths[i] = 0;
                    cellNulls[i] = true;
                }
                float total = 0;
                int j = 0;
                for (final Iterator i = cells.iterator(); i.hasNext();) {
                    cell = (Cell) i.next();
                    width = cell.getWidthAsString();
                    if (cell.getWidth() == 0) {
                        if (cell.getColspan() == 1 && cellWidths[j] == 0) {
                            try {
                                cellWidths[j] = 100f / columns;
                                total += cellWidths[j];
                            } catch (final Exception e) {
                                // empty on purpose
                            }
                        } else if (cell.getColspan() == 1) {
                            cellNulls[j] = false;
                        }
                    } else if (cell.getColspan() == 1 && width.endsWith("%")) {
                        try {
                            cellWidths[j] = Float.parseFloat(
                                    width.substring(0, width.length() - 1)
                                            + "f");
                            total += cellWidths[j];
                        } catch (final Exception e) {
                            // empty on purpose
                        }
                    }
                    j += cell.getColspan();
                    table.addCell(cell);
                }
                final float widths[] = table.getProportionalWidths();
                if (widths.length == columns) {
                    float left = 0.0f;
                    for (int i = 0; i < columns; i++) {
                        if (cellNulls[i] && widths[i] != 0) {
                            left += widths[i];
                            cellWidths[i] = widths[i];
                        }
                    }
                    if (100.0 >= total) {
                        for (int i = 0; i < widths.length; i++) {
                            if (cellWidths[i] == 0 && widths[i] != 0) {
                                cellWidths[i] = widths[i] / left
                                        * (100.0f - total);
                            }
                        }
                    }
                    table.setWidths(cellWidths);
                }
                this.stack.push(table);
            }

            // cells
            if (ElementTags.CELL.equals(name)) {
                return;
            }

            // sections
            if (ElementTags.SECTION.equals(name)) {
                this.stack.pop();
                return;
            }

            // chapters
            if (ElementTags.CHAPTER.equals(name)) {
                this.document.add((Element) this.stack.pop());
                return;
            }

            // the documentroot
            if (isDocumentRoot(name)) {
                try {
                    while (true) {
                        final Element element = (Element) this.stack.pop();
                        try {
                            final TextElementArray previous = (TextElementArray) this.stack
                                    .pop();
                            previous.add(element);
                            this.stack.push(previous);
                        } catch (final EmptyStackException es) {
                            this.document.add(element);
                        }
                    }
                } catch (final EmptyStackException ese) {
                    // empty on purpose
                }
                if (this.controlOpenClose) {
					this.document.close();
				}
                return;
            }
        } catch (final DocumentException de) {
            throw new ExceptionConverter(de);
        }
    }

    /**
     * Checks if a certain tag corresponds with the newpage-tag.
     *
     * @param tag
     *            a presumed tagname
     * @return <CODE>true</CODE> or <CODE>false</CODE>
     */

    private boolean isNewpage(final String tag) {
        return ElementTags.NEWPAGE.equals(tag);
    }

    /**
     * Checks if a certain tag corresponds with the newpage-tag.
     *
     * @param tag
     *            a presumed tagname
     * @return <CODE>true</CODE> or <CODE>false</CODE>
     */

    private boolean isNewline(final String tag) {
        return ElementTags.NEWLINE.equals(tag);
    }

    /**
     * Checks if a certain tag corresponds with the roottag.
     *
     * @param tag
     *            a presumed tagname
     * @return <CODE>true</CODE> if <VAR>tag </VAR> equals <CODE>itext
     *         </CODE>,<CODE>false</CODE> otherwise.
     */

    private boolean isDocumentRoot(final String tag) {
        return ElementTags.ITEXT.equals(tag);
    }
}