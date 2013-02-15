/*
 * $Id: Document.java 4007 2009-07-07 09:43:40Z blowagie $
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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;

/**
 * A generic Document class.
 * <P>
 * All kinds of Text-elements can be added to a <CODE>HTMLDocument</CODE>.
 * The <CODE>Document</CODE> signals all the listeners when an element has
 * been added.
 * <P>
 * Remark:
 * <OL>
 *     <LI>Once a document is created you can add some meta information.
 *     <LI>You can also set the headers/footers.
 *     <LI>You have to open the document before you can write content.
 * <LI>You can only write content (no more meta-formation!) once a document is
 * opened.
 * <LI>When you change the header/footer on a certain page, this will be
 * effective starting on the next page.
 * <LI>After closing the document, every listener (as well as its <CODE>
 * OutputStream</CODE>) is closed too.
 * </OL>
 * Example: <BLOCKQUOTE>
 *
 * <PRE>// creation of the document with a certain size and certain margins
 * <STRONG>Document document = new Document(PageSize.A4, 50, 50, 50, 50);
 * </STRONG> try {
 *   // creation of the different writers
 *   HtmlWriter.getInstance(<STRONG>document </STRONG>, System.out);
 *   PdfWriter.getInstance(<STRONG>document </STRONG>, new FileOutputStream("text.pdf"));
 *   // we add some meta information to the document
 *   <STRONG>document.addAuthor("Bruno Lowagie"); </STRONG>
 *   <STRONG>document.addSubject("This is the result of a Test."); </STRONG>
 *   // we open the document for writing
 *   <STRONG>document.open(); </STRONG>
 *   <STRONG>document.add(new Paragraph("Hello world"));</STRONG>
 *  } catch(DocumentException de) {
 *   System.err.println(de.getMessage());
 *  }
 *  <STRONG>document.close();</STRONG>
 * </PRE>
 *
 * </BLOCKQUOTE>
 */

public class Document implements DocListener {

    // membervariables
    /**
     * This constant may only be changed by Paulo Soares and/or Bruno Lowagie.
     * @since	2.1.6
     */
	private static final String ITEXT = "iText";
    /**
     * This constant may only be changed by Paulo Soares and/or Bruno Lowagie.
     * @since	2.1.6
     */
	private static final String RELEASE = "2.1.7";
	/** This constant may only be changed by Paulo Soares and/or Bruno Lowagie. */
	private static final String ITEXT_VERSION = ITEXT + " " + RELEASE + " by 1T3XT";

	/**
	 * Allows the pdf documents to be produced without compression for debugging
	 * purposes.
	 */
    public static boolean compress = true;

	/**
	 * When true the file access is not done through a memory mapped file. Use it if the file
     * is too big to be mapped in your address space.
	 */
    public static boolean plainRandomAccess = false;

    /** Scales the WMF font size. The default value is 0.86. */
    public static float wmfFontCorrection = 0.86f;

	/** The DocListener. */
    private final ArrayList listeners = new ArrayList();

	/** Is the document open or not? */
    protected boolean open;

	/** Has the document already been closed? */
    protected boolean close;

    // membervariables concerning the layout

	/** The size of the page. */
    protected Rectangle pageSize;

	/** margin in x direction starting from the left */
    protected float marginLeft = 0;

	/** margin in x direction starting from the right */
    protected float marginRight = 0;

	/** margin in y direction starting from the top */
    protected float marginTop = 0;

	/** margin in y direction starting from the bottom */
    protected float marginBottom = 0;

    /** mirroring of the left/right margins */
    protected boolean marginMirroring = false;

    /**
     * mirroring of the top/bottom margins
     * @since	2.1.6
     */
    protected boolean marginMirroringTopBottom = false;

	/** Content of JavaScript onLoad function */
    private String javaScript_onLoad = null;

	/** Content of JavaScript onUnLoad function */
    private String javaScript_onUnLoad = null;

	/** Style class in HTML body tag */
    private String htmlStyleClass = null;

    // headers, footers

	/** Current pagenumber */
    protected int pageN = 0;

	/** This is the textual part of a Page; it can contain a header */
    protected HeaderFooter header = null;

	/** This is the textual part of the footer */
    protected HeaderFooter footer = null;

    /** This is a chapter number in case ChapterAutoNumber is used. */
    private int chapternumber = 0;

    // constructor

	/**
	 * Constructs a new <CODE>Document</CODE> -object.
 */

    public Document() {
        this(PageSize.A4);
    }

	/**
	 * Constructs a new <CODE>Document</CODE> -object.
 *
	 * @param pageSize
	 *            the pageSize
 */

    private Document(final Rectangle pageSize) {
        this(pageSize, 36, 36, 36, 36);
    }

	/**
	 * Constructs a new <CODE>Document</CODE> -object.
 *
	 * @param pageSize
	 *            the pageSize
	 * @param marginLeft
	 *            the margin on the left
	 * @param marginRight
	 *            the margin on the right
	 * @param marginTop
	 *            the margin on the top
	 * @param marginBottom
	 *            the margin on the bottom
 */

    private Document(final Rectangle pageSize, final float marginLeft, final float marginRight,
			final float marginTop, final float marginBottom) {
        this.pageSize = pageSize;
        this.marginLeft = marginLeft;
        this.marginRight = marginRight;
        this.marginTop = marginTop;
        this.marginBottom = marginBottom;
    }

    // listener methods

	/**
 * Adds a <CODE>DocListener</CODE> to the <CODE>Document</CODE>.
 *
	 * @param listener
	 *            the new DocListener.
 */

    public void addDocListener(final DocListener listener) {
        this.listeners.add(listener);
    }

    // methods implementing the DocListener interface

	/**
	 * Adds an <CODE>Element</CODE> to the <CODE>Document</CODE>.
 *
	 * @param element
	 *            the <CODE>Element</CODE> to add
	 * @return <CODE>true</CODE> if the element was added, <CODE>false
	 *         </CODE> if not
	 * @throws DocumentException
	 *             when a document isn't open yet, or has been closed
 */

    @Override
	public boolean add(final Element element) throws DocumentException {
        if (this.close) {
			throw new DocumentException(
				"The document has been closed. You can't add any Elements.");
        }
		if (!this.open && element.isContent()) {
			throw new DocumentException(
				"The document is not open yet; you can only add Meta information.");
        }
        boolean success = false;
        DocListener listener;
        if (element instanceof ChapterAutoNumber) {
        	this.chapternumber = ((ChapterAutoNumber)element).setAutomaticNumber(this.chapternumber);
        }
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            success |= listener.add(element);
        }
		if (element instanceof LargeElement) {
			final LargeElement e = (LargeElement)element;
			if (!e.isComplete()) {
				e.flushContent();
			}
		}
        return success;
    }

	/**
 * Opens the document.
 * <P>
	 * Once the document is opened, you can't write any Header- or
	 * Meta-information anymore. You have to open the document before you can
	 * begin to add content to the body of the document.
 */

    @Override
	public void open() {
		if (!this.close) {
            this.open = true;
        }
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            listener.setPageSize(this.pageSize);
			listener.setMargins(this.marginLeft, this.marginRight, this.marginTop,
					this.marginBottom);
            listener.open();
        }
    }

	/**
 * Sets the pagesize.
 *
	 * @param pageSize
	 *            the new pagesize
 * @return	a <CODE>boolean</CODE>
 */

    @Override
	public boolean setPageSize(final Rectangle pageSize) {
        this.pageSize = pageSize;
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            listener.setPageSize(pageSize);
        }
        return true;
    }

	/**
 * Sets the margins.
 *
	 * @param marginLeft
	 *            the margin on the left
	 * @param marginRight
	 *            the margin on the right
	 * @param marginTop
	 *            the margin on the top
	 * @param marginBottom
	 *            the margin on the bottom
 * @return	a <CODE>boolean</CODE>
 */

	@Override
	public boolean setMargins(final float marginLeft, final float marginRight,
			final float marginTop, final float marginBottom) {
        this.marginLeft = marginLeft;
        this.marginRight = marginRight;
        this.marginTop = marginTop;
        this.marginBottom = marginBottom;
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
			listener.setMargins(marginLeft, marginRight, marginTop,
					marginBottom);
        }
        return true;
    }

	/**
 * Signals that an new page has to be started.
 *
	 * @return <CODE>true</CODE> if the page was added, <CODE>false</CODE>
	 *         if not.
 */

    @Override
	public boolean newPage() {
        if (!this.open || this.close) {
            return false;
        }
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            listener.newPage();
        }
        return true;
    }

	/**
 * Changes the header of this document.
 *
	 * @param header
	 *            the new header
 */

    @Override
	public void setHeader(final HeaderFooter header) {
        this.header = header;
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            listener.setHeader(header);
        }
    }

	/**
 * Resets the header of this document.
 */

    @Override
	public void resetHeader() {
        this.header = null;
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            listener.resetHeader();
        }
    }

	/**
 * Changes the footer of this document.
 *
	 * @param footer
	 *            the new footer
 */

    @Override
	public void setFooter(final HeaderFooter footer) {
        this.footer = footer;
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            listener.setFooter(footer);
        }
    }

	/**
 * Resets the footer of this document.
 */

    @Override
	public void resetFooter() {
        this.footer = null;
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            listener.resetFooter();
        }
    }

	/**
 * Sets the page number to 0.
 */

    @Override
	public void resetPageCount() {
        this.pageN = 0;
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            listener.resetPageCount();
        }
    }

	/**
 * Sets the page number.
 *
	 * @param pageN
	 *            the new page number
 */

    @Override
	public void setPageCount(final int pageN) {
        this.pageN = pageN;
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            listener.setPageCount(pageN);
        }
    }

	/**
 * Returns the current page number.
 *
 * @return the current page number
 */

    public int getPageNumber() {
        return this.pageN;
    }

	/**
 * Closes the document.
 * <P>
	 * Once all the content has been written in the body, you have to close the
	 * body. After that nothing can be written to the body anymore.
 */

    @Override
	public void close() {
		if (!this.close) {
            this.open = false;
            this.close = true;
        }
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            listener.close();
        }
    }

    // methods concerning the header or some meta information

	/**
 * Adds the title to a Document.
 *
	 * @param title
	 *            the title
 * @return	<CODE>true</CODE> if successful, <CODE>false</CODE> otherwise
 */

    public boolean addTitle(final String title) {
        try {
            return add(new Meta(Element.TITLE, title));
		} catch (final DocumentException de) {
            throw new ExceptionConverter(de);
        }
    }



	/**
 * Adds the producer to a Document.
 *
 * @return	<CODE>true</CODE> if successful, <CODE>false</CODE> otherwise
 */

    public boolean addProducer() {
        try {
            return add(new Meta(Element.PRODUCER, getVersion()));
		} catch (final DocumentException de) {
            throw new ExceptionConverter(de);
        }
    }

	/**
 * Adds the current date and time to a Document.
	 * @param globalDate
 *
 * @return	<CODE>true</CODE> if successful, <CODE>false</CODE> otherwise
 */

    public boolean addCreationDate(final Calendar globalDate) {
        try {
			/* bugfix by 'taqua' (Thomas) */
			final SimpleDateFormat sdf = new SimpleDateFormat(
					"EEE MMM dd HH:mm:ss zzz yyyy");
			return add(new Meta(Element.CREATIONDATE, sdf.format(globalDate.getTime())));
		} catch (final DocumentException de) {
            throw new ExceptionConverter(de);
        }
    }

    // methods to get the layout of the document.






	/**
 * Returns the lower left x-coordinate.
 *
 * @return	the lower left x-coordinate
 */

    public float left() {
        return this.pageSize.getLeft(this.marginLeft);
    }

	/**
 * Returns the upper right x-coordinate.
 *
 * @return	the upper right x-coordinate
 */

    public float right() {
        return this.pageSize.getRight(this.marginRight);
    }

	/**
 * Returns the upper right y-coordinate.
 *
 * @return	the upper right y-coordinate
 */

    public float top() {
        return this.pageSize.getTop(this.marginTop);
    }

	/**
 * Returns the lower left y-coordinate.
 *
 * @return	the lower left y-coordinate
 */

    public float bottom() {
        return this.pageSize.getBottom(this.marginBottom);
    }

	/**
 * Returns the lower left x-coordinate considering a given margin.
 *
	 * @param margin
	 *            a margin
 * @return	the lower left x-coordinate
 */

    public float left(final float margin) {
        return this.pageSize.getLeft(this.marginLeft + margin);
    }

	/**
 * Returns the upper right x-coordinate, considering a given margin.
 *
	 * @param margin
	 *            a margin
 * @return	the upper right x-coordinate
 */

    public float right(final float margin) {
        return this.pageSize.getRight(this.marginRight + margin);
    }

	/**
 * Returns the upper right y-coordinate, considering a given margin.
 *
	 * @param margin
	 *            a margin
 * @return	the upper right y-coordinate
 */

    public float top(final float margin) {
        return this.pageSize.getTop(this.marginTop + margin);
    }

	/**
 * Returns the lower left y-coordinate, considering a given margin.
 *
	 * @param margin
	 *            a margin
 * @return	the lower left y-coordinate
 */

    public float bottom(final float margin) {
        return this.pageSize.getBottom(this.marginBottom + margin);
    }

	/**
 * Gets the pagesize.
	 *
 * @return the page size
 */

	public Rectangle getPageSize() {
        return this.pageSize;
    }

	/**
	 * Checks if the document is open.
	 *
     * @return <CODE>true</CODE> if the document is open
     */
    public boolean isOpen() {
        return this.open;
    }

	/**
	 * Gets the product name.
	 * This method may only be changed by Paulo Soares and/or Bruno Lowagie.
     * @return the product name
     * @since	2.1.6
     */
    public static final String getProduct() {
        return ITEXT;
    }

	/**
	 * Gets the release number.
	 * This method may only be changed by Paulo Soares and/or Bruno Lowagie.
     * @return the product name
     * @since	2.1.6
     */
    public static final String getRelease() {
        return RELEASE;
    }

	/**
	 * Gets the iText version.
	 * This method may only be changed by Paulo Soares and/or Bruno Lowagie.
     * @return iText version
     */
    public static final String getVersion() {
        return ITEXT_VERSION;
    }

	/**
 * Adds a JavaScript onLoad function to the HTML body tag
 *
	 * @param code
	 *            the JavaScript code to be executed on load of the HTML page
 */

    public void setJavaScript_onLoad(final String code) {
        this.javaScript_onLoad = code;
    }

	/**
 * Gets the JavaScript onLoad command.
	 *
 * @return the JavaScript onLoad command
 */

    public String getJavaScript_onLoad() {
        return this.javaScript_onLoad;
    }

	/**
 * Adds a JavaScript onUnLoad function to the HTML body tag
 *
	 * @param code
	 *            the JavaScript code to be executed on unload of the HTML page
 */

    public void setJavaScript_onUnLoad(final String code) {
        this.javaScript_onUnLoad = code;
    }

	/**
 * Gets the JavaScript onUnLoad command.
	 *
 * @return the JavaScript onUnLoad command
 */

    public String getJavaScript_onUnLoad() {
        return this.javaScript_onUnLoad;
    }

	/**
 * Adds a style class to the HTML body tag
 *
	 * @param htmlStyleClass
	 *            the style class for the HTML body tag
 */

    public void setHtmlStyleClass(final String htmlStyleClass) {
        this.htmlStyleClass = htmlStyleClass;
    }

	/**
 * Gets the style class of the HTML body tag
 *
 * @return		the style class of the HTML body tag
 */

    public String getHtmlStyleClass() {
        return this.htmlStyleClass;
    }

    /**
     * Set the margin mirroring. It will mirror right/left margins for odd/even pages.
     * <p>
     * Note: it will not work with {@link Table}.
	 *
	 * @param marginMirroring
	 *            <CODE>true</CODE> to mirror the margins
     * @return always <CODE>true</CODE>
     */
    @Override
	public boolean setMarginMirroring(final boolean marginMirroring) {
        this.marginMirroring = marginMirroring;
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            listener.setMarginMirroring(marginMirroring);
        }
        return true;
    }

    /**
     * Set the margin mirroring. It will mirror top/bottom margins for odd/even pages.
     * <p>
     * Note: it will not work with {@link Table}.
	 *
	 * @param marginMirroringTopBottom
	 *            <CODE>true</CODE> to mirror the margins
     * @return always <CODE>true</CODE>
     * @since	2.1.6
     */
    @Override
	public boolean setMarginMirroringTopBottom(final boolean marginMirroringTopBottom) {
        this.marginMirroringTopBottom = marginMirroringTopBottom;
        DocListener listener;
		for (final Iterator iterator = this.listeners.iterator(); iterator.hasNext();) {
            listener = (DocListener) iterator.next();
            listener.setMarginMirroringTopBottom(marginMirroringTopBottom);
        }
        return true;
    }

    /**
     * Gets the margin mirroring flag.
	 *
     * @return the margin mirroring flag
     */
    public boolean isMarginMirroring() {
        return this.marginMirroring;
    }
}
