/*
 * $Id: PdfDocument.java 3939 2009-05-27 13:09:45Z blowagie $
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

package com.lowagie.text.pdf;

import harmony.java.awt.Color;

import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.HeaderFooter;
import com.lowagie.text.Image;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.Table;
import com.lowagie.text.pdf.collection.PdfCollection;
import com.lowagie.text.pdf.draw.DrawInterface;
import com.lowagie.text.pdf.internal.PdfAnnotationsImp;
import com.lowagie.text.pdf.internal.PdfViewerPreferencesImp;

/**
 * <CODE>PdfDocument</CODE> is the class that is used by <CODE>PdfWriter</CODE>
 * to translate a <CODE>Document</CODE> into a PDF with different pages.
 * <P>
 * A <CODE>PdfDocument</CODE> always listens to a <CODE>Document</CODE>
 * and adds the Pdf representation of every <CODE>Element</CODE> that is
 * added to the <CODE>Document</CODE>.
 *
 * @see		com.lowagie.text.Document
 * @see		com.lowagie.text.DocListener
 * @see		PdfWriter
 * @since	2.0.8 (class was package-private before)
 */

class PdfDocument extends Document {

    /**
     * <CODE>PdfInfo</CODE> is the PDF InfoDictionary.
     * <P>
     * A document's trailer may contain a reference to an Info dictionary that provides information
     * about the document. This optional dictionary may contain one or more keys, whose values
     * should be strings.<BR>
     * This object is described in the 'Portable Document Format Reference Manual version 1.3'
     * section 6.10 (page 120-121)
     * @param globalDate
     * @since	2.0.8 (PdfDocument was package-private before)
     */

    private static class PdfInfo extends PdfDictionary {

        /**
         * Construct a <CODE>PdfInfo</CODE>-object.
         */

        private PdfInfo(final Calendar globalDate) {
            super();
            addProducer();
            addCreationDate(globalDate);
        }



        /**
         * Adds the title of the document.
         *
         * @param	title		the title of the document
         */

        private void addTitle(final String title) {
            put(PdfName.TITLE, new PdfString(title, PdfObject.TEXT_UNICODE));
        }









        /**
         * Adds the name of the producer to the document.
         */

        private void addProducer() {
            put(PdfName.PRODUCER, new PdfString(getVersion()));
        }

        /**
         * Adds the date of creation to the document.
         */

        private void addCreationDate(final Calendar globalDate) {
            final PdfString date = new PdfDate(globalDate);
            put(PdfName.CREATIONDATE, date);
            put(PdfName.MODDATE, date);
        }


    }

    /**
     * <CODE>PdfCatalog</CODE> is the PDF Catalog-object.
     * <P>
     * The Catalog is a dictionary that is the root node of the document. It contains a reference
     * to the tree of pages contained in the document, a reference to the tree of objects representing
     * the document's outline, a reference to the document's article threads, and the list of named
     * destinations. In addition, the Catalog indicates whether the document's outline or thumbnail
     * page images should be displayed automatically when the document is viewed and whether some location
     * other than the first page should be shown when the document is opened.<BR>
     * In this class however, only the reference to the tree of pages is implemented.<BR>
     * This object is described in the 'Portable Document Format Reference Manual version 1.3'
     * section 6.2 (page 67-71)
     */

    private static class PdfCatalog extends PdfDictionary {

    	/** The writer writing the PDF for which we are creating this catalog object. */
        private final PdfWriter writer;

        /**
         * Constructs a <CODE>PdfCatalog</CODE>.
         *
         * @param		pages		an indirect reference to the root of the document's Pages tree.
         * @param writer the writer the catalog applies to
         */

        private PdfCatalog(final PdfIndirectReference pages, final PdfWriter writer) {
            super(CATALOG);
            this.writer = writer;
            put(PdfName.PAGES, pages);
        }

        /**
         * Adds the names of the named destinations to the catalog.
         * @param localDestinations the local destinations
         * @param documentLevelJS the javascript used in the document
         * @param documentFileAttachment	the attached files
         * @param writer the writer the catalog applies to
         */
        private void addNames(final TreeMap localDestinations, final HashMap documentLevelJS, final HashMap documentFileAttachment, final PdfWriter writer) {
            if (localDestinations.isEmpty() && documentLevelJS.isEmpty() && documentFileAttachment.isEmpty()) {
				return;
			}
            try {
                final PdfDictionary names = new PdfDictionary();
                if (!localDestinations.isEmpty()) {
                    final PdfArray ar = new PdfArray();
                    for (final Iterator i = localDestinations.entrySet().iterator(); i.hasNext();) {
                        final Map.Entry entry = (Map.Entry) i.next();
                        final String name = (String) entry.getKey();
                        final Object obj[] = (Object[]) entry.getValue();
                        if (obj[2] == null) {
							continue;
						}
                        final PdfIndirectReference ref = (PdfIndirectReference)obj[1];
                        ar.add(new PdfString(name, null));
                        ar.add(ref);
                    }
                    if (ar.size() > 0) {
                        final PdfDictionary dests = new PdfDictionary();
                        dests.put(PdfName.NAMES, ar);
                        names.put(PdfName.DESTS, writer.addToBody(dests).getIndirectReference());
                    }
                }
                if (!documentLevelJS.isEmpty()) {
                    final PdfDictionary tree = PdfNameTree.writeTree(documentLevelJS, writer);
                    names.put(PdfName.JAVASCRIPT, writer.addToBody(tree).getIndirectReference());
                }
                if (!documentFileAttachment.isEmpty()) {
                    names.put(PdfName.EMBEDDEDFILES, writer.addToBody(PdfNameTree.writeTree(documentFileAttachment, writer)).getIndirectReference());
                }
                if (names.size() > 0) {
					put(PdfName.NAMES, writer.addToBody(names).getIndirectReference());
				}
            }
            catch (final IOException e) {
                throw new ExceptionConverter(e);
            }
        }

        /**
         * Adds an open action to the catalog.
         * @param	action	the action that will be triggered upon opening the document
         */
        private void setOpenAction(final PdfAction action) {
            put(PdfName.OPENACTION, action);
        }


        /**
         * Sets the document level additional actions.
         * @param actions   dictionary of actions
         */
        private void setAdditionalActions(final PdfDictionary actions) {
            try {
                put(PdfName.AA, this.writer.addToBody(actions).getIndirectReference());
            } catch (final Exception e) {
                throw new ExceptionConverter(e);
            }
        }
    }

// CONSTRUCTING A PdfDocument/PdfWriter INSTANCE

    /**
     * Constructs a new PDF document.
     * @param globalDate
     */
    PdfDocument(final Calendar globalDate) {
        super();
        addProducer();
        addCreationDate(globalDate);

        this.info = new PdfInfo(globalDate);
    }

    /** The <CODE>PdfWriter</CODE>. */
    private PdfWriter writer;

    /**
     * Adds a <CODE>PdfWriter</CODE> to the <CODE>PdfDocument</CODE>.
     *
     * @param writer the <CODE>PdfWriter</CODE> that writes everything
     *                     what is added to this document to an outputstream.
     * @throws DocumentException on error
     */
    void addWriter(final PdfWriter writer) throws DocumentException {
        if (this.writer == null) {
            this.writer = writer;
            this.annotationsImp = new PdfAnnotationsImp(writer);
            return;
        }
        throw new DocumentException("You can only add a writer to a PdfDocument once.");
    }

// LISTENER METHODS START

//	[L0] ElementListener interface

    /** This is the PdfContentByte object, containing the text. */
    private PdfContentByte text;

    /** This is the PdfContentByte object, containing the borders and other Graphics. */
    private PdfContentByte graphics;

    /** This represents the leading of the lines. */
    private float leading = 0;

    /**
     * Getter for the current leading.
     * @return	the current leading
     * @since	2.1.2
     */
    public float getLeading() {
    	return this.leading;
    }

    /**
     * Setter for the current leading.
     * @param	leading the current leading
     * @since	2.1.6
     */
    void setLeading(final float leading) {
    	this.leading = leading;
    }

    /** This represents the current alignment of the PDF Elements. */
    private int alignment = Element.ALIGN_LEFT;

    /** This is the current height of the document. */
    private float currentHeight = 0;


//	[L1] DocListener interface

    /**
     * Opens the document.
     * <P>
     * You have to open the document before you can begin to add content
     * to the body of the document.
     */
    @Override
	public void open() {
        if (!this.open) {
            super.open();
            this.writer.open();
            this.rootOutline = new PdfOutline(this.writer);
            this.currentOutline = this.rootOutline;
        }
        try {
            initPage();
        }
        catch(final DocumentException de) {
            throw new ExceptionConverter(de);
        }
    }

//	[L2] DocListener interface

    /**
     * Closes the document.
     * <B>
     * Once all the content has been written in the body, you have to close
     * the body. After that nothing can be written to the body anymore.
     */
    @Override
	public void close() {
        if (this.close) {
            return;
        }
        try {
        	final boolean wasImage = this.imageWait != null;
            newPage();
            if (this.imageWait != null || wasImage) {
				newPage();
			}
            if (this.annotationsImp.hasUnusedAnnotations()) {
				throw new RuntimeException("Not all annotations could be added to the document (the document doesn't have enough pages).");
			}
            final PdfPageEvent pageEvent = this.writer.getPageEvent();
            if (pageEvent != null) {
				pageEvent.onCloseDocument(this.writer, this);
			}
            super.close();

            this.writer.addLocalDestinations(this.localDestinations);
            calculateOutlineCount();
            writeOutlines();
        }
        catch(final Exception e) {
            throw ExceptionConverter.convertException(e);
        }

        this.writer.close();
    }

//	[L3] DocListener interface
    private int textEmptySize;

    // [C9] Metadata for the page
    /** XMP Metadata for the page. */
    private byte[] xmpMetadata = null;
	/**
	 * Use this method to set the XMP Metadata.
	 * @param xmpMetadata The xmpMetadata to set.
	 */
	public void setXmpMetadata(final byte[] xmpMetadata) {
		this.xmpMetadata = xmpMetadata;
	}

    /**
     * Makes a new page and sends it to the <CODE>PdfWriter</CODE>.
     *
     * @return a <CODE>boolean</CODE>
     */
    @Override
	public boolean newPage() {
        this.lastElementType = -1;
        if (this.writer == null || this.writer.getDirectContent().size() == 0 && this.writer.getDirectContentUnder().size() == 0 && (this.pageEmpty || this.writer.isPaused())) {
        	setNewPageSizeAndMargins();
            return false;
        }
    	if (!this.open || this.close) {
    		throw new RuntimeException("The document isn't open.");
    	}
        final PdfPageEvent pageEvent = this.writer.getPageEvent();
        if (pageEvent != null) {
			pageEvent.onEndPage(this.writer, this);
		}

        //Added to inform any listeners that we are moving to a new page (added by David Freels)
        super.newPage();

        // the following 2 lines were added by Pelikan Stephan
        this.indentation.imageIndentLeft = 0;
        this.indentation.imageIndentRight = 0;

        try {
            // we flush the arraylist with recently written lines
        	flushLines();

        	// we prepare the elements of the page dictionary

        	// [U1] page size and rotation
        	final int rotation = this.pageSize.getRotation();

        	// [C10]
        	if (this.writer.isPdfX()) {
        		if (this.thisBoxSize.containsKey("art") && this.thisBoxSize.containsKey("trim")) {
					throw new PdfXConformanceException("Only one of ArtBox or TrimBox can exist in the page.");
				}
        		if (!this.thisBoxSize.containsKey("art") && !this.thisBoxSize.containsKey("trim")) {
        			if (this.thisBoxSize.containsKey("crop")) {
						this.thisBoxSize.put("trim", this.thisBoxSize.get("crop"));
					} else {
						this.thisBoxSize.put("trim", new PdfRectangle(this.pageSize, this.pageSize.getRotation()));
					}
        		}
        	}

        	// [M1]
        	this.pageResources.addDefaultColorDiff(this.writer.getDefaultColorspace());
            if (this.writer.isRgbTransparencyBlending()) {
                final PdfDictionary dcs = new PdfDictionary();
                dcs.put(PdfName.CS, PdfName.DEVICERGB);
                this.pageResources.addDefaultColorDiff(dcs);
            }
        	final PdfDictionary resources = this.pageResources.getResources();

        	// we create the page dictionary

        	final PdfPage page = new PdfPage(new PdfRectangle(this.pageSize, rotation), this.thisBoxSize, resources, rotation);
        	page.put(PdfName.TABS, this.writer.getTabs());

            // we complete the page dictionary

            // [C9] if there is XMP data to add: add it
            if (this.xmpMetadata != null) {
            	final PdfStream xmp = new PdfStream(this.xmpMetadata);
            	xmp.put(PdfName.TYPE, PdfName.METADATA);
            	xmp.put(PdfName.SUBTYPE, PdfName.XML);
            	final PdfEncryption crypto = this.writer.getEncryption();
                if (crypto != null && !crypto.isMetadataEncrypted()) {
                    final PdfArray ar = new PdfArray();
                    ar.add(PdfName.CRYPT);
                    xmp.put(PdfName.FILTER, ar);
                }
            	page.put(PdfName.METADATA, this.writer.addToBody(xmp).getIndirectReference());
            }

        	// [U3] page actions: transition, duration, additional actions
        	if (this.transition!=null) {
        		page.put(PdfName.TRANS, this.transition.getTransitionDictionary());
        		this.transition = null;
        	}
        	if (this.duration>0) {
        		page.put(PdfName.DUR,new PdfNumber(this.duration));
        		this.duration = 0;
        	}
        	if (this.pageAA != null) {
        		page.put(PdfName.AA, this.writer.addToBody(this.pageAA).getIndirectReference());
        		this.pageAA = null;
        	}

        	// [U4] we add the thumbs
        	if (this.thumb != null) {
        		page.put(PdfName.THUMB, this.thumb);
        		this.thumb = null;
        	}

        	// [U8] we check if the userunit is defined
        	if (this.writer.getUserunit() > 0f) {
        		page.put(PdfName.USERUNIT, new PdfNumber(this.writer.getUserunit()));
        	}

        	// [C5] and [C8] we add the annotations
        	if (this.annotationsImp.hasUnusedAnnotations()) {
        		final PdfArray array = this.annotationsImp.rotateAnnotations(this.writer, this.pageSize);
        		if (array.size() != 0) {
					page.put(PdfName.ANNOTS, array);
				}
        	}

        	// [F12] we add tag info
        	if (this.writer.isTagged()) {
				page.put(PdfName.STRUCTPARENTS, new PdfNumber(this.writer.getCurrentPageNumber() - 1));
			}

            if (this.text.size() > this.textEmptySize) {
				this.text.endText();
			} else {
				this.text = null;
			}
        	this.writer.add(page, new PdfContents(this.writer.getDirectContentUnder(), this.graphics, this.text, this.writer.getDirectContent(), this.pageSize));
        	// we initialize the new page
        	initPage();
        }
        catch(final DocumentException de) {
        	// maybe this never happens, but it's better to check.
        	throw new ExceptionConverter(de);
        }
        catch (final IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
        return true;
    }

//	[L4] DocListener interface

    /**
     * Sets the pagesize.
     *
     * @param pageSize the new pagesize
     * @return <CODE>true</CODE> if the page size was set
     */
    @Override
	public boolean setPageSize(final Rectangle pageSize) {
        if (this.writer != null && this.writer.isPaused()) {
            return false;
        }
        this.nextPageSize = new Rectangle(pageSize);
        return true;
    }

//	[L5] DocListener interface

    /** margin in x direction starting from the left. Will be valid in the next page */
    private float nextMarginLeft;

    /** margin in x direction starting from the right. Will be valid in the next page */
    private float nextMarginRight;

    /** margin in y direction starting from the top. Will be valid in the next page */
    private float nextMarginTop;

    /** margin in y direction starting from the bottom. Will be valid in the next page */
    private float nextMarginBottom;

    /**
     * Sets the margins.
     *
     * @param	marginLeft		the margin on the left
     * @param	marginRight		the margin on the right
     * @param	marginTop		the margin on the top
     * @param	marginBottom	the margin on the bottom
     * @return	a <CODE>boolean</CODE>
     */
    @Override
	public boolean setMargins(final float marginLeft, final float marginRight, final float marginTop, final float marginBottom) {
        if (this.writer != null && this.writer.isPaused()) {
            return false;
        }
        this.nextMarginLeft = marginLeft;
        this.nextMarginRight = marginRight;
        this.nextMarginTop = marginTop;
        this.nextMarginBottom = marginBottom;
        return true;
    }

//	[L6] DocListener interface

    /**
     * @see com.lowagie.text.DocListener#setMarginMirroring(boolean)
     */
    @Override
	public boolean setMarginMirroring(final boolean MarginMirroring) {
        if (this.writer != null && this.writer.isPaused()) {
            return false;
        }
        return super.setMarginMirroring(MarginMirroring);
    }

    /**
     * @see com.lowagie.text.DocListener#setMarginMirroring(boolean)
     * @since	2.1.6
     */
    @Override
	public boolean setMarginMirroringTopBottom(final boolean MarginMirroringTopBottom) {
        if (this.writer != null && this.writer.isPaused()) {
            return false;
        }
        return super.setMarginMirroringTopBottom(MarginMirroringTopBottom);
    }

//	[L7] DocListener interface

    /**
     * Sets the page number.
     *
     * @param	pageN		the new page number
     */
    @Override
	public void setPageCount(final int pageN) {
        if (this.writer != null && this.writer.isPaused()) {
            return;
        }
        super.setPageCount(pageN);
    }

//	[L8] DocListener interface

    /**
     * Sets the page number to 0.
     */
    @Override
	public void resetPageCount() {
        if (this.writer != null && this.writer.isPaused()) {
            return;
        }
        super.resetPageCount();
    }

//	[L9] DocListener interface

    /**
     * Changes the header of this document.
     *
     * @param header the new header
     */
    @Override
	public void setHeader(final HeaderFooter header) {
        if (this.writer != null && this.writer.isPaused()) {
            return;
        }
        super.setHeader(header);
    }

//	[L10] DocListener interface

    /**
     * Resets the header of this document.
     */
    @Override
	public void resetHeader() {
        if (this.writer != null && this.writer.isPaused()) {
            return;
        }
        super.resetHeader();
    }

//	[L11] DocListener interface

    /**
     * Changes the footer of this document.
     *
     * @param	footer		the new footer
     */
    @Override
	public void setFooter(final HeaderFooter footer) {
        if (this.writer != null && this.writer.isPaused()) {
            return;
        }
        super.setFooter(footer);
    }

//	[L12] DocListener interface

    /**
     * Resets the footer of this document.
     */
    @Override
	public void resetFooter() {
        if (this.writer != null && this.writer.isPaused()) {
            return;
        }
        super.resetFooter();
    }

// DOCLISTENER METHODS END

    /** Signals that OnOpenDocument should be called. */
    private boolean firstPageEvent = true;

    /**
     * Initializes a page.
     * <P>
     * If the footer/header is set, it is printed.
     * @throws DocumentException on error
     */
    private void initPage() throws DocumentException {
        // the pagenumber is incremented
        this.pageN++;

        // initialization of some page objects
        this.annotationsImp.resetAnnotations();
        this.pageResources = new PageResources();

        this.writer.resetContent();
        this.graphics = new PdfContentByte(this.writer);
        this.text = new PdfContentByte(this.writer);
        this.text.reset();
        this.text.beginText();
        this.textEmptySize = this.text.size();

    	this.markPoint = 0;
        setNewPageSizeAndMargins();
        this.imageEnd = -1;
        this.indentation.imageIndentRight = 0;
        this.indentation.imageIndentLeft = 0;
        this.indentation.indentBottom = 0;
        this.indentation.indentTop = 0;
        this.currentHeight = 0;

        // backgroundcolors, etc...
        this.thisBoxSize = new HashMap(this.boxSize);
        if (this.pageSize.getBackgroundColor() != null
        || this.pageSize.hasBorders()
        || this.pageSize.getBorderColor() != null) {
            add(this.pageSize);
        }

        final float oldleading = this.leading;
        final int oldAlignment = this.alignment;
        // if there is a footer, the footer is added
        doFooter();
        // we move to the left/top position of the page
        this.text.moveText(left(), top());
        doHeader();
        this.pageEmpty = true;
        // if there is an image waiting to be drawn, draw it
        try {
            if (this.imageWait != null) {
                add(this.imageWait);
                this.imageWait = null;
            }
        }
        catch(final Exception e) {
            throw new ExceptionConverter(e);
        }
        this.leading = oldleading;
        this.alignment = oldAlignment;
        carriageReturn();

        final PdfPageEvent pageEvent = this.writer.getPageEvent();
        if (pageEvent != null) {
            if (this.firstPageEvent) {
                pageEvent.onOpenDocument(this.writer, this);
            }
            pageEvent.onStartPage(this.writer, this);
        }
        this.firstPageEvent = false;
    }

    /** The line that is currently being written. */
    private PdfLine line = null;
    /** The lines that are written until now. */
    private ArrayList lines = new ArrayList();

    /**
     * Adds the current line to the list of lines and also adds an empty line.
     * @throws DocumentException on error
     */
    private void newLine() throws DocumentException {
        this.lastElementType = -1;
        carriageReturn();
        if (this.lines != null && !this.lines.isEmpty()) {
            this.lines.add(this.line);
            this.currentHeight += this.line.height();
        }
        this.line = new PdfLine(indentLeft(), indentRight(), this.alignment, this.leading);
    }

    /**
     * If the current line is not empty or null, it is added to the arraylist
     * of lines and a new empty line is added.
     */
    private void carriageReturn() {
        // the arraylist with lines may not be null
        if (this.lines == null) {
            this.lines = new ArrayList();
        }
        // If the current line is not null
        if (this.line != null) {
            // we check if the end of the page is reached (bugfix by Francois Gravel)
            if (this.currentHeight + this.line.height() + this.leading < indentTop() - indentBottom()) {
                // if so nonempty lines are added and the height is augmented
                if (this.line.size() > 0) {
                    this.currentHeight += this.line.height();
                    this.lines.add(this.line);
                    this.pageEmpty = false;
                }
            }
            // if the end of the line is reached, we start a new page
            else {
                newPage();
            }
        }
        if (this.imageEnd > -1 && this.currentHeight > this.imageEnd) {
            this.imageEnd = -1;
            this.indentation.imageIndentRight = 0;
            this.indentation.imageIndentLeft = 0;
        }
        // a new current line is constructed
        this.line = new PdfLine(indentLeft(), indentRight(), this.alignment, this.leading);
    }



    /** Holds the type of the last element, that has been added to the document. */
    private int lastElementType = -1;

    /**
     * Ensures that a new line has been started.
     */
    private void ensureNewLine() {
      try {
        if (this.lastElementType == Element.PHRASE ||
            this.lastElementType == Element.CHUNK) {
          newLine();
          flushLines();
        }
      } catch (final DocumentException ex) {
        throw new ExceptionConverter(ex);
        }
    }

    /**
     * Writes all the lines to the text-object.
     *
     * @return the displacement that was caused
     * @throws DocumentException on error
     */
    private float flushLines() throws DocumentException {
        // checks if the ArrayList with the lines is not null
        if (this.lines == null) {
            return 0;
        }
        // checks if a new Line has to be made.
        if (this.line != null && this.line.size() > 0) {
            this.lines.add(this.line);
            this.line = new PdfLine(indentLeft(), indentRight(), this.alignment, this.leading);
        }

        // checks if the ArrayList with the lines is empty
        if (this.lines.isEmpty()) {
            return 0;
        }

        // initialization of some parameters
        final Object currentValues[] = new Object[2];
        PdfFont currentFont = null;
        float displacement = 0;
        PdfLine l;
        final Float lastBaseFactor = new Float(0);
        currentValues[1] = lastBaseFactor;
        // looping over all the lines
        for (final Iterator i = this.lines.iterator(); i.hasNext(); ) {

            // this is a line in the loop
            l = (PdfLine) i.next();

            final float moveTextX = l.indentLeft() - indentLeft() + this.indentation.indentLeft + this.indentation.listIndentLeft + this.indentation.sectionIndentLeft;
            this.text.moveText(moveTextX, -l.height());
            // is the line preceded by a symbol?
            if (l.listSymbol() != null) {
                ColumnText.showTextAligned(this.graphics, Element.ALIGN_LEFT, new Phrase(l.listSymbol()), this.text.getXTLM() - l.listIndent(), this.text.getYTLM(), 0);
            }

            currentValues[0] = currentFont;

            writeLineToContent(l, this.text, this.graphics, currentValues, this.writer.getSpaceCharRatio());

            currentFont = (PdfFont)currentValues[0];
            displacement += l.height();
            this.text.moveText(-moveTextX, 0);

        }
        this.lines = new ArrayList();
        return displacement;
    }

    /** The characters to be applied the hanging punctuation. */
    private static final String hangingPunctuation = ".,;:'";

    /**
     * Writes a text line to the document. It takes care of all the attributes.
     * <P>
     * Before entering the line position must have been established and the
     * <CODE>text</CODE> argument must be in text object scope (<CODE>beginText()</CODE>).
     * @param line the line to be written
     * @param text the <CODE>PdfContentByte</CODE> where the text will be written to
     * @param graphics the <CODE>PdfContentByte</CODE> where the graphics will be written to
     * @param currentValues the current font and extra spacing values
     * @param ratio
     * @throws DocumentException on error
     */
    void writeLineToContent(final PdfLine line, final PdfContentByte text, final PdfContentByte graphics, final Object currentValues[], final float ratio)  throws DocumentException {
        PdfFont currentFont = (PdfFont)currentValues[0];
        float lastBaseFactor = ((Float)currentValues[1]).floatValue();
        PdfChunk chunk;
        int numberOfSpaces;
        int lineLen;
        boolean isJustified;
        float hangingCorrection = 0;
        float hScale = 1;
        float lastHScale = Float.NaN;
        float baseWordSpacing = 0;
        float baseCharacterSpacing = 0;
        float glueWidth = 0;

        numberOfSpaces = line.numberOfSpaces();
        lineLen = line.GetLineLengthUtf32();
        // does the line need to be justified?
        isJustified = line.hasToBeJustified() && (numberOfSpaces != 0 || lineLen > 1);
        final int separatorCount = line.getSeparatorCount();
        if (separatorCount > 0) {
        	glueWidth = line.widthLeft() / separatorCount;
        }
        else if (isJustified) {
            if (line.isNewlineSplit() && line.widthLeft() >= lastBaseFactor * (ratio * numberOfSpaces + lineLen - 1)) {
                if (line.isRTL()) {
                    text.moveText(line.widthLeft() - lastBaseFactor * (ratio * numberOfSpaces + lineLen - 1), 0);
                }
                baseWordSpacing = ratio * lastBaseFactor;
                baseCharacterSpacing = lastBaseFactor;
            }
            else {
                float width = line.widthLeft();
                final PdfChunk last = line.getChunk(line.size() - 1);
                if (last != null) {
                    final String s = last.toString();
                    char c;
                    if (s.length() > 0 && hangingPunctuation.indexOf(c = s.charAt(s.length() - 1)) >= 0) {
                        final float oldWidth = width;
                        width += last.font().width(c) * 0.4f;
                        hangingCorrection = width - oldWidth;
                    }
                }
                final float baseFactor = width / (ratio * numberOfSpaces + lineLen - 1);
                baseWordSpacing = ratio * baseFactor;
                baseCharacterSpacing = baseFactor;
                lastBaseFactor = baseFactor;
            }
        }

        final int lastChunkStroke = line.getLastStrokeChunk();
        int chunkStrokeIdx = 0;
        float xMarker = text.getXTLM();
        final float baseXMarker = xMarker;
        final float yMarker = text.getYTLM();
        boolean adjustMatrix = false;
        float tabPosition = 0;

        // looping over all the chunks in 1 line
        for (final Iterator j = line.iterator(); j.hasNext(); ) {
            chunk = (PdfChunk) j.next();
            final Color color = chunk.color();
            hScale = 1;

            if (chunkStrokeIdx <= lastChunkStroke) {
                float width;
                if (isJustified) {
                    width = chunk.getWidthCorrected(baseCharacterSpacing, baseWordSpacing);
                }
                else {
                    width = chunk.width();
                }
                if (chunk.isStroked()) {
                    final PdfChunk nextChunk = line.getChunk(chunkStrokeIdx + 1);
                    if (chunk.isSeparator()) {
                    	width = glueWidth;
                    	final Object[] sep = (Object[])chunk.getAttribute(Chunk.SEPARATOR);
                        final DrawInterface di = (DrawInterface)sep[0];
                        final Boolean vertical = (Boolean)sep[1];
                        final float fontSize = chunk.font().size();
                        final float ascender = chunk.font().getFont().getFontDescriptor(BaseFont.ASCENT, fontSize);
                        final float descender = chunk.font().getFont().getFontDescriptor(BaseFont.DESCENT, fontSize);
                        if (vertical.booleanValue()) {
                        	di.draw(graphics, baseXMarker, yMarker + descender, baseXMarker + line.getOriginalWidth(), ascender - descender, yMarker);
                        }
                        else {
                        	di.draw(graphics, xMarker, yMarker + descender, xMarker + width, ascender - descender, yMarker);
                        }
                    }
                    if (chunk.isTab()) {
                    	final Object[] tab = (Object[])chunk.getAttribute(Chunk.TAB);
                        final DrawInterface di = (DrawInterface)tab[0];
                        tabPosition = ((Float)tab[1]).floatValue() + ((Float)tab[3]).floatValue();
                        final float fontSize = chunk.font().size();
                        final float ascender = chunk.font().getFont().getFontDescriptor(BaseFont.ASCENT, fontSize);
                        final float descender = chunk.font().getFont().getFontDescriptor(BaseFont.DESCENT, fontSize);
                        if (tabPosition > xMarker) {
                        	di.draw(graphics, xMarker, yMarker + descender, tabPosition, ascender - descender, yMarker);
                        }
                        final float tmp = xMarker;
                    	xMarker = tabPosition;
                    	tabPosition = tmp;
                    }
                    if (chunk.isAttribute(Chunk.BACKGROUND)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.BACKGROUND)) {
							subtract = 0;
						}
                        if (nextChunk == null) {
							subtract += hangingCorrection;
						}
                        final float fontSize = chunk.font().size();
                        final float ascender = chunk.font().getFont().getFontDescriptor(BaseFont.ASCENT, fontSize);
                        final float descender = chunk.font().getFont().getFontDescriptor(BaseFont.DESCENT, fontSize);
                        final Object bgr[] = (Object[])chunk.getAttribute(Chunk.BACKGROUND);
                        graphics.setColorFill((Color)bgr[0]);
                        final float extra[] = (float[])bgr[1];
                        graphics.rectangle(xMarker - extra[0],
                            yMarker + descender - extra[1] + chunk.getTextRise(),
                            width - subtract + extra[0] + extra[2],
                            ascender - descender + extra[1] + extra[3]);
                        graphics.fill();
                        graphics.setGrayFill(0);
                    }
                    if (chunk.isAttribute(Chunk.UNDERLINE)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.UNDERLINE)) {
							subtract = 0;
						}
                        if (nextChunk == null) {
							subtract += hangingCorrection;
						}
                        final Object unders[][] = (Object[][])chunk.getAttribute(Chunk.UNDERLINE);
                        Color scolor = null;
                        for (final Object[] obj : unders) {
                            scolor = (Color)obj[0];
                            final float ps[] = (float[])obj[1];
                            if (scolor == null) {
								scolor = color;
							}
                            if (scolor != null) {
								graphics.setColorStroke(scolor);
							}
                            final float fsize = chunk.font().size();
                            graphics.setLineWidth(ps[0] + fsize * ps[1]);
                            final float shift = ps[2] + fsize * ps[3];
                            final int cap2 = (int)ps[4];
                            if (cap2 != 0) {
								graphics.setLineCap(cap2);
							}
                            graphics.moveTo(xMarker, yMarker + shift);
                            graphics.lineTo(xMarker + width - subtract, yMarker + shift);
                            graphics.stroke();
                            if (scolor != null) {
								graphics.resetGrayStroke();
							}
                            if (cap2 != 0) {
								graphics.setLineCap(0);
							}
                        }
                        graphics.setLineWidth(1);
                    }
                    if (chunk.isAttribute(Chunk.ACTION)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.ACTION)) {
							subtract = 0;
						}
                        if (nextChunk == null) {
							subtract += hangingCorrection;
						}
                        text.addAnnotation(new PdfAnnotation(this.writer, xMarker, yMarker, xMarker + width - subtract, yMarker + chunk.font().size(), (PdfAction)chunk.getAttribute(Chunk.ACTION)));
                    }
                    if (chunk.isAttribute(Chunk.REMOTEGOTO)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.REMOTEGOTO)) {
							subtract = 0;
						}
                        if (nextChunk == null) {
							subtract += hangingCorrection;
						}
                        final Object obj[] = (Object[])chunk.getAttribute(Chunk.REMOTEGOTO);
                        final String filename = (String)obj[0];
                        if (obj[1] instanceof String) {
							remoteGoto(filename, (String)obj[1], xMarker, yMarker, xMarker + width - subtract, yMarker + chunk.font().size());
						} else {
							remoteGoto(filename, ((Integer)obj[1]).intValue(), xMarker, yMarker, xMarker + width - subtract, yMarker + chunk.font().size());
						}
                    }
                    if (chunk.isAttribute(Chunk.LOCALGOTO)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.LOCALGOTO)) {
							subtract = 0;
						}
                        if (nextChunk == null) {
							subtract += hangingCorrection;
						}
                        localGoto((String)chunk.getAttribute(Chunk.LOCALGOTO), xMarker, yMarker, xMarker + width - subtract, yMarker + chunk.font().size());
                    }
                    if (chunk.isAttribute(Chunk.LOCALDESTINATION)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.LOCALDESTINATION)) {
							subtract = 0;
						}
                        if (nextChunk == null) {
							subtract += hangingCorrection;
						}
                        localDestination((String)chunk.getAttribute(Chunk.LOCALDESTINATION), new PdfDestination(PdfDestination.XYZ, xMarker, yMarker + chunk.font().size(), 0));
                    }
                    if (chunk.isAttribute(Chunk.GENERICTAG)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.GENERICTAG)) {
							subtract = 0;
						}
                        if (nextChunk == null) {
							subtract += hangingCorrection;
						}
                        final Rectangle rect = new Rectangle(xMarker, yMarker, xMarker + width - subtract, yMarker + chunk.font().size());
                        final PdfPageEvent pev = this.writer.getPageEvent();
                        if (pev != null) {
							pev.onGenericTag(this.writer, this, rect, (String)chunk.getAttribute(Chunk.GENERICTAG));
						}
                    }
                    if (chunk.isAttribute(Chunk.PDFANNOTATION)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.PDFANNOTATION)) {
							subtract = 0;
						}
                        if (nextChunk == null) {
							subtract += hangingCorrection;
						}
                        final float fontSize = chunk.font().size();
                        final float ascender = chunk.font().getFont().getFontDescriptor(BaseFont.ASCENT, fontSize);
                        final float descender = chunk.font().getFont().getFontDescriptor(BaseFont.DESCENT, fontSize);
                        final PdfAnnotation annot = PdfFormField.shallowDuplicate((PdfAnnotation)chunk.getAttribute(Chunk.PDFANNOTATION));
                        annot.put(PdfName.RECT, new PdfRectangle(xMarker, yMarker + descender, xMarker + width - subtract, yMarker + ascender));
                        text.addAnnotation(annot);
                    }
                    final float params[] = (float[])chunk.getAttribute(Chunk.SKEW);
                    final Float hs = (Float)chunk.getAttribute(Chunk.HSCALE);
                    if (params != null || hs != null) {
                        float b = 0, c = 0;
                        if (params != null) {
                            b = params[0];
                            c = params[1];
                        }
                        if (hs != null) {
							hScale = hs.floatValue();
						}
                        text.setTextMatrix(hScale, b, c, 1, xMarker, yMarker);
                    }
                    if (chunk.isImage()) {
                        final Image image = chunk.getImage();
                        final float matrix[] = image.matrix();
                        matrix[Image.CX] = xMarker + chunk.getImageOffsetX() - matrix[Image.CX];
                        matrix[Image.CY] = yMarker + chunk.getImageOffsetY() - matrix[Image.CY];
                        graphics.addImage(image, matrix[0], matrix[1], matrix[2], matrix[3], matrix[4], matrix[5]);
                        text.moveText(xMarker + lastBaseFactor + image.getScaledWidth() - text.getXTLM(), 0);
                    }
                }
                xMarker += width;
                ++chunkStrokeIdx;
            }

            if (chunk.font().compareTo(currentFont) != 0) {
                currentFont = chunk.font();
                text.setFontAndSize(currentFont.getFont(), currentFont.size());
            }
            float rise = 0;
            final Object textRender[] = (Object[])chunk.getAttribute(Chunk.TEXTRENDERMODE);
            int tr = 0;
            float strokeWidth = 1;
            Color strokeColor = null;
            final Float fr = (Float)chunk.getAttribute(Chunk.SUBSUPSCRIPT);
            if (textRender != null) {
                tr = ((Integer)textRender[0]).intValue() & 3;
                if (tr != PdfContentByte.TEXT_RENDER_MODE_FILL) {
					text.setTextRenderingMode(tr);
				}
                if (tr == PdfContentByte.TEXT_RENDER_MODE_STROKE || tr == PdfContentByte.TEXT_RENDER_MODE_FILL_STROKE) {
                    strokeWidth = ((Float)textRender[1]).floatValue();
                    if (strokeWidth != 1) {
						text.setLineWidth(strokeWidth);
					}
                    strokeColor = (Color)textRender[2];
                    if (strokeColor == null) {
						strokeColor = color;
					}
                    if (strokeColor != null) {
						text.setColorStroke(strokeColor);
					}
                }
            }
            if (fr != null) {
				rise = fr.floatValue();
			}
            if (color != null) {
				text.setColorFill(color);
			}
            if (rise != 0) {
				text.setTextRise(rise);
			}
            if (chunk.isImage()) {
                adjustMatrix = true;
            }
            else if (chunk.isHorizontalSeparator()) {
            	final PdfTextArray array = new PdfTextArray();
            	array.add(-glueWidth * 1000f / chunk.font.size() / hScale);
            	text.showText(array);
            }
            else if (chunk.isTab()) {
            	final PdfTextArray array = new PdfTextArray();
            	array.add((tabPosition - xMarker) * 1000f / chunk.font.size() / hScale);
            	text.showText(array);
            }
            // If it is a CJK chunk or Unicode TTF we will have to simulate the
            // space adjustment.
            else if (isJustified && numberOfSpaces > 0 && chunk.isSpecialEncoding()) {
                if (hScale != lastHScale) {
                    lastHScale = hScale;
                    text.setWordSpacing(baseWordSpacing / hScale);
                    text.setCharacterSpacing(baseCharacterSpacing / hScale);
                }
                final String s = chunk.toString();
                int idx = s.indexOf(' ');
                if (idx < 0) {
					text.showText(s);
				} else {
                    final float spaceCorrection = - baseWordSpacing * 1000f / chunk.font.size() / hScale;
                    final PdfTextArray textArray = new PdfTextArray(s.substring(0, idx));
                    int lastIdx = idx;
                    while ((idx = s.indexOf(' ', lastIdx + 1)) >= 0) {
                        textArray.add(spaceCorrection);
                        textArray.add(s.substring(lastIdx, idx));
                        lastIdx = idx;
                    }
                    textArray.add(spaceCorrection);
                    textArray.add(s.substring(lastIdx));
                    text.showText(textArray);
                }
            }
            else {
                if (isJustified && hScale != lastHScale) {
                    lastHScale = hScale;
                    text.setWordSpacing(baseWordSpacing / hScale);
                    text.setCharacterSpacing(baseCharacterSpacing / hScale);
                }
                text.showText(chunk.toString());
            }

            if (rise != 0) {
				text.setTextRise(0);
			}
            if (color != null) {
				text.resetRGBColorFill();
			}
            if (tr != PdfContentByte.TEXT_RENDER_MODE_FILL) {
				text.setTextRenderingMode(PdfContentByte.TEXT_RENDER_MODE_FILL);
			}
            if (strokeColor != null) {
				text.resetRGBColorStroke();
			}
            if (strokeWidth != 1) {
				text.setLineWidth(1);
			}
            if (chunk.isAttribute(Chunk.SKEW) || chunk.isAttribute(Chunk.HSCALE)) {
                adjustMatrix = true;
                text.setTextMatrix(xMarker, yMarker);
            }
        }
        if (isJustified) {
            text.setWordSpacing(0);
            text.setCharacterSpacing(0);
            if (line.isNewlineSplit()) {
				lastBaseFactor = 0;
			}
        }
        if (adjustMatrix) {
			text.moveText(baseXMarker - text.getXTLM(), 0);
		}
        currentValues[0] = currentFont;
        currentValues[1] = new Float(lastBaseFactor);
    }

    private final Indentation indentation = new Indentation();

    /**
     * @since	2.0.8 (PdfDocument was package-private before)
     */
    private static class Indentation {

        /** This represents the current indentation of the PDF Elements on the left side. */
        private float indentLeft = 0;

        /** Indentation to the left caused by a section. */
        private final float sectionIndentLeft = 0;

        /** This represents the current indentation of the PDF Elements on the left side. */
        private float listIndentLeft = 0;

        /** This is the indentation caused by an image on the left. */
        private float imageIndentLeft = 0;

        /** This represents the current indentation of the PDF Elements on the right side. */
        private float indentRight = 0;

        /** Indentation to the right caused by a section. */
        private final float sectionIndentRight = 0;

        /** This is the indentation caused by an image on the right. */
        private float imageIndentRight = 0;

        /** This represents the current indentation of the PDF Elements on the top side. */
        private float indentTop = 0;

        /** This represents the current indentation of the PDF Elements on the bottom side. */
        private float indentBottom = 0;
    }

    /**
     * Gets the indentation on the left side.
     *
     * @return	a margin
     */

    private float indentLeft() {
        return left(this.indentation.indentLeft + this.indentation.listIndentLeft + this.indentation.imageIndentLeft + this.indentation.sectionIndentLeft);
    }

    /**
     * Gets the indentation on the right side.
     *
     * @return	a margin
     */

    private float indentRight() {
        return right(this.indentation.indentRight + this.indentation.sectionIndentRight + this.indentation.imageIndentRight);
    }

    /**
     * Gets the indentation on the top side.
     *
     * @return	a margin
     */

    private float indentTop() {
        return top(this.indentation.indentTop);
    }

    /**
     * Gets the indentation on the bottom side.
     *
     * @return	a margin
     */

    float indentBottom() {
        return bottom(this.indentation.indentBottom);
    }



//	Info Dictionary and Catalog

    /** some meta information about the Document. */
    private final PdfInfo info;

    /**
     * Gets the <CODE>PdfInfo</CODE>-object.
     *
     * @return	<CODE>PdfInfo</COPE>
     */

    PdfInfo getInfo() {
        return this.info;
    }

    /**
     * Gets the <CODE>PdfCatalog</CODE>-object.
     *
     * @param pages an indirect reference to this document pages
     * @return <CODE>PdfCatalog</CODE>
     */

    PdfCatalog getCatalog(final PdfIndirectReference pages) {
        final PdfCatalog catalog = new PdfCatalog(pages, this.writer);

        // [C1] outlines
        if (this.rootOutline.getKids().size() > 0) {
            catalog.put(PdfName.PAGEMODE, PdfName.USEOUTLINES);
            catalog.put(PdfName.OUTLINES, this.rootOutline.indirectReference());
        }

        // [C2] version
        this.writer.getPdfVersion().addToCatalog(catalog);

        // [C3] preferences
        this.viewerPreferences.addToCatalog(catalog);

        // [C4] pagelabels
        if (this.pageLabels != null) {
            catalog.put(PdfName.PAGELABELS, this.pageLabels.getDictionary(this.writer));
        }

        // [C5] named objects
        catalog.addNames(this.localDestinations, getDocumentLevelJS(), this.documentFileAttachment, this.writer);

        // [C6] actions
        if (this.openActionName != null) {
            final PdfAction action = getLocalGotoAction(this.openActionName);
            catalog.setOpenAction(action);
        }
        else if (this.openActionAction != null) {
			catalog.setOpenAction(this.openActionAction);
		}
        if (this.additionalActions != null)   {
            catalog.setAdditionalActions(this.additionalActions);
        }

        // [C7] portable collections
        if (this.collection != null) {
        	catalog.put(PdfName.COLLECTION, this.collection);
        }

        // [C8] AcroForm
        if (this.annotationsImp.hasValidAcroForm()) {
            try {
                catalog.put(PdfName.ACROFORM, this.writer.addToBody(this.annotationsImp.getAcroForm()).getIndirectReference());
            }
            catch (final IOException e) {
                throw new ExceptionConverter(e);
            }
        }

        return catalog;
    }

//	[C1] outlines

    /** This is the root outline of the document. */
    private PdfOutline rootOutline;

    /** This is the current <CODE>PdfOutline</CODE> in the hierarchy of outlines. */
    private PdfOutline currentOutline;



    /**
     * Gets the root outline. All the outlines must be created with a parent.
     * The first level is created with this outline.
     * @return the root outline
     */
    public PdfOutline getRootOutline() {
        return this.rootOutline;
    }


    /**
     * Updates the count in the outlines.
     */
    private void calculateOutlineCount() {
        if (this.rootOutline.getKids().size() == 0) {
			return;
		}
        traverseOutlineCount(this.rootOutline);
    }

    /**
     * Recursive method to update the count in the outlines.
     */
    private void traverseOutlineCount(final PdfOutline outline) {
        final ArrayList kids = outline.getKids();
        final PdfOutline parent = outline.parent();
        if (kids.isEmpty()) {
            if (parent != null) {
                parent.setCount(parent.getCount() + 1);
            }
        }
        else {
            for (int k = 0; k < kids.size(); ++k) {
                traverseOutlineCount((PdfOutline)kids.get(k));
            }
            if (parent != null) {
                if (outline.isOpen()) {
                    parent.setCount(outline.getCount() + parent.getCount() + 1);
                }
                else {
                    parent.setCount(parent.getCount() + 1);
                    outline.setCount(-outline.getCount());
                }
            }
        }
    }

    /**
     * Writes the outline tree to the body of the PDF document.
     */
    private void writeOutlines() throws IOException {
        if (this.rootOutline.getKids().size() == 0) {
			return;
		}
        outlineTree(this.rootOutline);
        this.writer.addToBody(this.rootOutline, this.rootOutline.indirectReference());
    }

    /**
     * Recursive method used to write outlines.
     */
    private void outlineTree(final PdfOutline outline) throws IOException {
        outline.setIndirectReference(this.writer.getPdfIndirectReference());
        if (outline.parent() != null) {
			outline.put(PdfName.PARENT, outline.parent().indirectReference());
		}
        final ArrayList kids = outline.getKids();
        final int size = kids.size();
        for (int k = 0; k < size; ++k) {
			outlineTree((PdfOutline)kids.get(k));
		}
        for (int k = 0; k < size; ++k) {
            if (k > 0) {
				((PdfOutline)kids.get(k)).put(PdfName.PREV, ((PdfOutline)kids.get(k - 1)).indirectReference());
			}
            if (k < size - 1) {
				((PdfOutline)kids.get(k)).put(PdfName.NEXT, ((PdfOutline)kids.get(k + 1)).indirectReference());
			}
        }
        if (size > 0) {
            outline.put(PdfName.FIRST, ((PdfOutline)kids.get(0)).indirectReference());
            outline.put(PdfName.LAST, ((PdfOutline)kids.get(size - 1)).indirectReference());
        }
        for (int k = 0; k < size; ++k) {
            final PdfOutline kid = (PdfOutline)kids.get(k);
            this.writer.addToBody(kid, kid.indirectReference());
        }
    }

//  [C3] PdfViewerPreferences interface

	/** Contains the Viewer preferences of this PDF document. */
    private final PdfViewerPreferencesImp viewerPreferences = new PdfViewerPreferencesImp();
    /** @see com.lowagie.text.pdf.interfaces.PdfViewerPreferences#setViewerPreferences(int) */
    void setViewerPreferences(final int preferences) {
        this.viewerPreferences.setViewerPreferences(preferences);
    }

    /** @see com.lowagie.text.pdf.interfaces.PdfViewerPreferences#addViewerPreference(com.lowagie.text.pdf.PdfName, com.lowagie.text.pdf.PdfObject) */
    void addViewerPreference(final PdfName key, final PdfObject value) {
    	this.viewerPreferences.addViewerPreference(key, value);
    }

//	[C4] Page labels

    protected PdfPageLabels pageLabels;
    /**
     * Sets the page labels
     * @param pageLabels the page labels
     */
    void setPageLabels(final PdfPageLabels pageLabels) {
        this.pageLabels = pageLabels;
    }

//	[C5] named objects: local destinations, javascript, embedded files

    /**
     * Implements a link to other part of the document. The jump will
     * be made to a local destination with the same name, that must exist.
     * @param name the name for this link
     * @param llx the lower left x corner of the activation area
     * @param lly the lower left y corner of the activation area
     * @param urx the upper right x corner of the activation area
     * @param ury the upper right y corner of the activation area
     */
    private void localGoto(final String name, final float llx, final float lly, final float urx, final float ury) {
        final PdfAction action = getLocalGotoAction(name);
        this.annotationsImp.addPlainAnnotation(new PdfAnnotation(this.writer, llx, lly, urx, ury, action));
    }

    /**
     * Implements a link to another document.
     * @param filename the filename for the remote document
     * @param name the name to jump to
     * @param llx the lower left x corner of the activation area
     * @param lly the lower left y corner of the activation area
     * @param urx the upper right x corner of the activation area
     * @param ury the upper right y corner of the activation area
     */
    private void remoteGoto(final String filename, final String name, final float llx, final float lly, final float urx, final float ury) {
        this.annotationsImp.addPlainAnnotation(new PdfAnnotation(this.writer, llx, lly, urx, ury, new PdfAction(filename, name)));
    }

    /**
     * Implements a link to another document.
     * @param filename the filename for the remote document
     * @param page the page to jump to
     * @param llx the lower left x corner of the activation area
     * @param lly the lower left y corner of the activation area
     * @param urx the upper right x corner of the activation area
     * @param ury the upper right y corner of the activation area
     */
    private void remoteGoto(final String filename, final int page, final float llx, final float lly, final float urx, final float ury) {
        addAnnotation(new PdfAnnotation(this.writer, llx, lly, urx, ury, new PdfAction(filename, page)));
    }

    /** Implements an action in an area.
     * @param action the <CODE>PdfAction</CODE>
     * @param llx the lower left x corner of the activation area
     * @param lly the lower left y corner of the activation area
     * @param urx the upper right x corner of the activation area
     * @param ury the upper right y corner of the activation area
     */
    void setAction(final PdfAction action, final float llx, final float lly, final float urx, final float ury) {
        addAnnotation(new PdfAnnotation(this.writer, llx, lly, urx, ury, action));
    }

    /**
     * Stores the destinations keyed by name. Value is
     * <CODE>Object[]{PdfAction,PdfIndirectReference,PdfDestintion}</CODE>.
     */
    private final TreeMap localDestinations = new TreeMap();

    private PdfAction getLocalGotoAction(final String name) {
        PdfAction action;
        Object obj[] = (Object[])this.localDestinations.get(name);
        if (obj == null) {
			obj = new Object[3];
		}
        if (obj[0] == null) {
            if (obj[1] == null) {
                obj[1] = this.writer.getPdfIndirectReference();
            }
            action = new PdfAction((PdfIndirectReference)obj[1]);
            obj[0] = action;
            this.localDestinations.put(name, obj);
        }
        else {
            action = (PdfAction)obj[0];
        }
        return action;
    }

    /**
     * The local destination to where a local goto with the same
     * name will jump to.
     * @param name the name of this local destination
     * @param destination the <CODE>PdfDestination</CODE> with the jump coordinates
     * @return <CODE>true</CODE> if the local destination was added,
     * <CODE>false</CODE> if a local destination with the same name
     * already existed
     */
    private boolean localDestination(final String name, final PdfDestination destination) {
        Object obj[] = (Object[])this.localDestinations.get(name);
        if (obj == null) {
			obj = new Object[3];
		}
        if (obj[2] != null) {
			return false;
		}
        obj[2] = destination;
        this.localDestinations.put(name, obj);
        destination.addPage(this.writer.getCurrentPage());
        return true;
    }

    /**
     * Stores a list of document level JavaScript actions.
     */
    private int jsCounter;
    private final HashMap documentLevelJS = new HashMap();
    private static final DecimalFormat SIXTEEN_DIGITS = new DecimalFormat("0000000000000000");
    void addJavaScript(final PdfAction js) {
        if (js.get(PdfName.JS) == null) {
			throw new RuntimeException("Only JavaScript actions are allowed.");
		}
        try {
            this.documentLevelJS.put(SIXTEEN_DIGITS.format(this.jsCounter++), this.writer.addToBody(js).getIndirectReference());
        }
        catch (final IOException e) {
            throw new ExceptionConverter(e);
        }
    }
    void addJavaScript(final String name, final PdfAction js) {
        if (js.get(PdfName.JS) == null) {
			throw new RuntimeException("Only JavaScript actions are allowed.");
		}
        try {
            this.documentLevelJS.put(name, this.writer.addToBody(js).getIndirectReference());
        }
        catch (final IOException e) {
            throw new ExceptionConverter(e);
        }
    }

    HashMap getDocumentLevelJS() {
    	return this.documentLevelJS;
    }

    private final HashMap documentFileAttachment = new HashMap();

    void addFileAttachment(String description, final PdfFileSpecification fs) throws IOException {
        if (description == null) {
        	final PdfString desc = (PdfString)fs.get(PdfName.DESC);
        	if (desc == null) {
        		description = "";
        	}
        	else {
        		description = PdfEncodings.convertToString(desc.getBytes(), null);
        	}
        }
        fs.addDescription(description, true);
        if (description.length() == 0) {
			description = "Unnamed";
		}
        String fn = PdfEncodings.convertToString(new PdfString(description, PdfObject.TEXT_UNICODE).getBytes(), null);
        int k = 0;
        while (this.documentFileAttachment.containsKey(fn)) {
            ++k;
            fn = PdfEncodings.convertToString(new PdfString(description + " " + k, PdfObject.TEXT_UNICODE).getBytes(), null);
        }
        this.documentFileAttachment.put(fn, fs.getReference());
    }

    HashMap getDocumentFileAttachment() {
        return this.documentFileAttachment;
    }

//	[C6] document level actions

    private String openActionName;

    void setOpenAction(final String name) {
        this.openActionName = name;
        this.openActionAction = null;
    }

    private PdfAction openActionAction;
    void setOpenAction(final PdfAction action) {
        this.openActionAction = action;
        this.openActionName = null;
    }

    private PdfDictionary additionalActions;
    void addAdditionalAction(final PdfName actionType, final PdfAction action)  {
        if (this.additionalActions == null)  {
            this.additionalActions = new PdfDictionary();
        }
        if (action == null) {
			this.additionalActions.remove(actionType);
		} else {
			this.additionalActions.put(actionType, action);
		}
        if (this.additionalActions.size() == 0) {
			this.additionalActions = null;
		}
    }

//	[C7] portable collections

    private PdfCollection collection;

    /**
     * Sets the collection dictionary.
     * @param collection a dictionary of type PdfCollection
     */
	public void setCollection(final PdfCollection collection) {
		this.collection = collection;
	}

//	[C8] AcroForm

	private PdfAnnotationsImp annotationsImp;

    /**
     * Gets the AcroForm object.
     * @return the PdfAcroform object of the PdfDocument
     */
    PdfAcroForm getAcroForm() {
        return this.annotationsImp.getAcroForm();
    }

    void setSigFlags(final int f) {
        this.annotationsImp.setSigFlags(f);
    }



    void addAnnotation(final PdfAnnotation annot) {
        this.pageEmpty = false;
        this.annotationsImp.addAnnotation(annot);
    }

//	[F12] tagged PDF

    private int markPoint;





//	[U1] page sizes

    /** This is the size of the next page. */
    private Rectangle nextPageSize = null;

    /** This is the size of the several boxes of the current Page. */
    private HashMap thisBoxSize = new HashMap();

    /** This is the size of the several boxes that will be used in
     * the next page. */
    private final HashMap boxSize = new HashMap();

    void setCropBoxSize(final Rectangle crop) {
        setBoxSize("crop", crop);
    }

    private void setBoxSize(final String boxName, final Rectangle size) {
        if (size == null) {
			this.boxSize.remove(boxName);
		} else {
			this.boxSize.put(boxName, new PdfRectangle(size));
		}
    }

    private void setNewPageSizeAndMargins() {
        this.pageSize = this.nextPageSize;
    	if (this.marginMirroring && (getPageNumber() & 1) == 0) {
            this.marginRight = this.nextMarginLeft;
            this.marginLeft = this.nextMarginRight;
        }
        else {
            this.marginLeft = this.nextMarginLeft;
            this.marginRight = this.nextMarginRight;
        }
    	if (this.marginMirroringTopBottom && (getPageNumber() & 1) == 0) {
    		this.marginTop = this.nextMarginBottom;
    		this.marginBottom = this.nextMarginTop;
    	}
    	else {
    		this.marginTop = this.nextMarginTop;
    		this.marginBottom = this.nextMarginBottom;
    	}
    }



//	[U2] empty pages

    /** This checks if the page is empty. */
    private boolean pageEmpty = true;

    void setPageEmpty(final boolean pageEmpty) {
        this.pageEmpty = pageEmpty;
    }

//	[U3] page actions

    /** The duration of the page */
    private int duration=-1; // negative values will indicate no duration

    /** The page transition */
    private PdfTransition transition=null;

    /**
     * Sets the display duration for the page (for presentations)
     * @param seconds   the number of seconds to display the page
     */
    void setDuration(final int seconds) {
        if (seconds > 0) {
			this.duration=seconds;
		} else {
			this.duration=-1;
		}
    }

    /**
     * Sets the transition for the page
     * @param transition   the PdfTransition object
     */
    void setTransition(final PdfTransition transition) {
        this.transition=transition;
    }

    private PdfDictionary pageAA = null;
    void setPageAction(final PdfName actionType, final PdfAction action) {
        if (this.pageAA == null) {
            this.pageAA = new PdfDictionary();
        }
        this.pageAA.put(actionType, action);
    }

//	[U8] thumbnail images

    private PdfIndirectReference thumb;
    void setThumbnail(final Image image) throws PdfException, DocumentException {
        this.thumb = this.writer.getImageReference(this.writer.addDirectImageSimple(image));
    }

//	[M0] Page resources contain references to fonts, extgstate, images,...

    /** This are the page resources of the current Page. */
    private PageResources pageResources;

    PageResources getPageResources() {
        return this.pageResources;
    }

//	[M3] Images

    /** Holds value of property strictImageSequence. */
    private boolean strictImageSequence = false;

    /** Getter for property strictImageSequence.
     * @return Value of property strictImageSequence.
     *
     */
    boolean isStrictImageSequence() {
        return this.strictImageSequence;
    }

    /** Setter for property strictImageSequence.
     * @param strictImageSequence New value of property strictImageSequence.
     *
     */
    void setStrictImageSequence(final boolean strictImageSequence) {
        this.strictImageSequence = strictImageSequence;
    }

    /** This is the position where the image ends. */
    private float imageEnd = -1;



    /** This is the image that could not be shown on a previous page. */
    private Image imageWait = null;

    /**
     * Adds an image to the document.
     * @param image the <CODE>Image</CODE> to add
     * @throws PdfException on error
     * @throws DocumentException on error
     */

    private void add(final Image image) throws PdfException, DocumentException {

        if (image.hasAbsoluteY()) {
            this.graphics.addImage(image);
            this.pageEmpty = false;
            return;
        }

        // if there isn't enough room for the image on this page, save it for the next page
        if (this.currentHeight != 0 && indentTop() - this.currentHeight - image.getScaledHeight() < indentBottom()) {
            if (!this.strictImageSequence && this.imageWait == null) {
                this.imageWait = image;
                return;
            }
            newPage();
            if (this.currentHeight != 0 && indentTop() - this.currentHeight - image.getScaledHeight() < indentBottom()) {
                this.imageWait = image;
                return;
            }
        }
        this.pageEmpty = false;
        // avoid endless loops
        if (image == this.imageWait) {
			this.imageWait = null;
		}
        final boolean textwrap = (image.getAlignment() & Image.TEXTWRAP) == Image.TEXTWRAP
        && !((image.getAlignment() & Image.MIDDLE) == Image.MIDDLE);
        final boolean underlying = (image.getAlignment() & Image.UNDERLYING) == Image.UNDERLYING;
        float diff = this.leading / 2;
        if (textwrap) {
            diff += this.leading;
        }
        final float lowerleft = indentTop() - this.currentHeight - image.getScaledHeight() -diff;
        final float mt[] = image.matrix();
        float startPosition = indentLeft() - mt[4];
        if ((image.getAlignment() & Image.RIGHT) == Image.RIGHT) {
			startPosition = indentRight() - image.getScaledWidth() - mt[4];
		}
        if ((image.getAlignment() & Image.MIDDLE) == Image.MIDDLE) {
			startPosition = indentLeft() + (indentRight() - indentLeft() - image.getScaledWidth()) / 2 - mt[4];
		}
        if (image.hasAbsoluteX()) {
			startPosition = image.getAbsoluteX();
		}
        if (textwrap) {
            if (this.imageEnd < 0 || this.imageEnd < this.currentHeight + image.getScaledHeight() + diff) {
                this.imageEnd = this.currentHeight + image.getScaledHeight() + diff;
            }
            if ((image.getAlignment() & Image.RIGHT) == Image.RIGHT) {
            	// indentation suggested by Pelikan Stephan
            	this.indentation.imageIndentRight += image.getScaledWidth() + image.getIndentationLeft();
            }
            else {
            	// indentation suggested by Pelikan Stephan
            	this.indentation.imageIndentLeft += image.getScaledWidth() + image.getIndentationRight();
            }
        }
        else {
        	if ((image.getAlignment() & Image.RIGHT) == Image.RIGHT) {
				startPosition -= image.getIndentationRight();
			} else if ((image.getAlignment() & Image.MIDDLE) == Image.MIDDLE) {
				startPosition += image.getIndentationLeft() - image.getIndentationRight();
			} else {
				startPosition += image.getIndentationLeft();
			}
        }
        this.graphics.addImage(image, mt[0], mt[1], mt[2], mt[3], startPosition, lowerleft - mt[5]);
        if (!(textwrap || underlying)) {
            this.currentHeight += image.getScaledHeight() + diff;
            flushLines();
            this.text.moveText(0, - (image.getScaledHeight() + diff));
            newLine();
        }
    }

//	[M4] Adding a PdfPTable



    /**
     * Checks if a <CODE>PdfPTable</CODE> fits the current page of the <CODE>PdfDocument</CODE>.
     *
     * @param	table	the table that has to be checked
     * @param	margin	a certain margin
     * @return	<CODE>true</CODE> if the <CODE>PdfPTable</CODE> fits the page, <CODE>false</CODE> otherwise.
     */

    private boolean fitsPage(final PdfPTable table, final float margin) {
    	if (!table.isLockedWidth()) {
    		final float totalWidth = (indentRight() - indentLeft()) * table.getWidthPercentage() / 100;
    		table.setTotalWidth(totalWidth);
    	}
        // ensuring that a new line has been started.
        ensureNewLine();
        return table.getTotalHeight() + (this.currentHeight > 0 ? table.spacingBefore() : 0f)
        	<= indentTop() - this.currentHeight - indentBottom() - margin;
    }

//	[M4'] Adding a Table

	/**
	 * This is a helper class for adding a Table to a document.
	 * @since	2.0.8 (PdfDocument was package-private before)
	 */
    private static class RenderingContext {
        private float pagetop = -1;
        private float oldHeight = -1;

        private PdfContentByte cellGraphics = null;

        private float lostTableBottom;

        private float maxCellBottom;


        private Map rowspanMap;
        private final Map pageMap = new HashMap();
        /**
         * A PdfPTable
         */
        private PdfTable table;

        /**
         * Consumes the rowspan
         * @param c
         * @return a rowspan.
         */
        private int consumeRowspan(final PdfCell c) {
            if (c.rowspan() == 1) {
                return 1;
            }

            Integer i = (Integer) this.rowspanMap.get(c);
            if (i == null) {
                i = new Integer(c.rowspan());
            }

            i = new Integer(i.intValue() - 1);
            this.rowspanMap.put(c, i);

            if (i.intValue() < 1) {
                return 1;
            }
            return i.intValue();
        }

        /**
         * Looks at the current rowspan.
         * @param c
         * @return the current rowspan
         */
        private int currentRowspan(final PdfCell c) {
            final Integer i = (Integer) this.rowspanMap.get(c);
            if (i == null) {
                return c.rowspan();
            } else {
                return i.intValue();
            }
        }

        private int cellRendered(final PdfCell cell, final int pageNumber) {
            Integer i = (Integer) this.pageMap.get(cell);
            if (i == null) {
                i = new Integer(1);
            } else {
                i = new Integer(i.intValue() + 1);
            }
            this.pageMap.put(cell, i);

            final Integer pageInteger = new Integer(pageNumber);
            Set set = (Set) this.pageMap.get(pageInteger);

            if (set == null) {
                set = new HashSet();
                this.pageMap.put(pageInteger, set);
            }

            set.add(cell);

            return i.intValue();
        }

        private int numCellRendered(final PdfCell cell) {
            Integer i = (Integer) this.pageMap.get(cell);
            if (i == null) {
                i = new Integer(0);
            }
            return i.intValue();
        }

        private boolean isCellRenderedOnPage(final PdfCell cell, final int pageNumber) {
            final Integer pageInteger = new Integer(pageNumber);
            final Set set = (Set) this.pageMap.get(pageInteger);

            if (set != null) {
                return set.contains(cell);
            }

            return false;
        }
    };

	/**
	 * Adds a new table to the document.
	 * @param t				Table to add.  Rendered rows will be deleted after processing.
	 * @throws DocumentException
	 * @since	iText 2.0.8
	 */
    private void addPdfTable(final Table t) throws DocumentException {
        // before every table, we flush all lines
        flushLines();

    	final PdfTable table = new PdfTable(t, indentLeft(), indentRight(), indentTop() - this.currentHeight);
        final RenderingContext ctx = new RenderingContext();
        ctx.pagetop = indentTop();
        ctx.oldHeight = this.currentHeight;
        ctx.cellGraphics = new PdfContentByte(this.writer);
        ctx.rowspanMap = new HashMap();
        ctx.table = table;

		// initialization of parameters
		PdfCell cell;

		// drawing the table
		final ArrayList headercells = table.getHeaderCells();
        final ArrayList cells = table.getCells();
        final ArrayList rows = extractRows(cells, ctx);
        boolean isContinue = false;
		while (!cells.isEmpty()) {
			// initialization of some extra parameters;
			ctx.lostTableBottom = 0;

			// loop over the cells
			boolean cellsShown = false;

            // draw the cells (line by line)
            Iterator iterator = rows.iterator();

            boolean atLeastOneFits = false;
            while (iterator.hasNext()) {
                final ArrayList row = (ArrayList) iterator.next();
                analyzeRow(rows, ctx);
                renderCells(ctx, row, table.hasToFitPageCells() & atLeastOneFits);

                if (!mayBeRemoved(row)) {
                    break;
                }
                consumeRowspan(row, ctx);
                iterator.remove();
                atLeastOneFits = true;
            }

//          compose cells array list for subsequent code
            cells.clear();
            final Set opt = new HashSet();
            iterator = rows.iterator();
            while (iterator.hasNext()) {
                final ArrayList row = (ArrayList) iterator.next();

                final Iterator cellIterator = row.iterator();
                while (cellIterator.hasNext()) {
                    cell = (PdfCell) cellIterator.next();

                    if (!opt.contains(cell)) {
                        cells.add(cell);
                        opt.add(cell);
                    }
                }
            }

			// we paint the graphics of the table after looping through all the cells
			Rectangle tablerec = new Rectangle(table);
			tablerec.setBorder(table.getBorder());
			tablerec.setBorderWidth(table.getBorderWidth());
			tablerec.setBorderColor(table.getBorderColor());
			tablerec.setBackgroundColor(table.getBackgroundColor());
			final PdfContentByte under = this.writer.getDirectContentUnder();
			under.rectangle(tablerec.rectangle(top(), indentBottom()));
			under.add(ctx.cellGraphics);
			// bugfix by Gerald Fehringer: now again add the border for the table
			// since it might have been covered by cell backgrounds
			tablerec.setBackgroundColor(null);
			tablerec = tablerec.rectangle(top(), indentBottom());
			tablerec.setBorder(table.getBorder());
			under.rectangle(tablerec);
			// end bugfix

            ctx.cellGraphics = new PdfContentByte(null);
			// if the table continues on the next page

			if (!rows.isEmpty()) {
				isContinue = true;
				this.graphics.setLineWidth(table.getBorderWidth());
				if (cellsShown && (table.getBorder() & Rectangle.BOTTOM) == Rectangle.BOTTOM) {
					// Draw the bottom line

					// the color is set to the color of the element
					final Color tColor = table.getBorderColor();
					if (tColor != null) {
						this.graphics.setColorStroke(tColor);
					}
					this.graphics.moveTo(table.getLeft(), Math.max(table.getBottom(), indentBottom()));
					this.graphics.lineTo(table.getRight(), Math.max(table.getBottom(), indentBottom()));
					this.graphics.stroke();
					if (tColor != null) {
						this.graphics.resetRGBColorStroke();
					}
				}

				// old page
				this.pageEmpty = false;
                float difference = ctx.lostTableBottom;

				// new page
				newPage();

				// G.F.: if something added in page event i.e. currentHeight > 0
				float heightCorrection = 0;
				boolean somethingAdded = false;
				if (this.currentHeight > 0) {
					heightCorrection = 6;
					this.currentHeight += heightCorrection;
					somethingAdded = true;
					newLine();
					flushLines();
					this.indentation.indentTop = this.currentHeight - this.leading;
					this.currentHeight = 0;
				}
				else {
                    flushLines();
				}

				// this part repeats the table headers (if any)
				int size = headercells.size();
				if (size > 0) {
					// this is the top of the headersection
					cell = (PdfCell) headercells.get(0);
					final float oldTop = cell.getTop(0);
					// loop over all the cells of the table header
					for (int i = 0; i < size; i++) {
						cell = (PdfCell) headercells.get(i);
						// calculation of the new cellpositions
						cell.setTop(indentTop() - oldTop + cell.getTop(0));
						cell.setBottom(indentTop() - oldTop + cell.getBottom(0));
						ctx.pagetop = cell.getBottom();
						// we paint the borders of the cell
						ctx.cellGraphics.rectangle(cell.rectangle(indentTop(), indentBottom()));
						// we write the text of the cell
						final ArrayList images = cell.getImages(indentTop(), indentBottom());
						for (final Iterator im = images.iterator(); im.hasNext();) {
							cellsShown = true;
							final Image image = (Image) im.next();
							this.graphics.addImage(image);
						}
						this.lines = cell.getLines(indentTop(), indentBottom());
						final float cellTop = cell.getTop(indentTop());
						this.text.moveText(0, cellTop-heightCorrection);
						final float cellDisplacement = flushLines() - cellTop+heightCorrection;
						this.text.moveText(0, cellDisplacement);
					}

					this.currentHeight = indentTop() - ctx.pagetop + table.cellspacing();
					this.text.moveText(0, ctx.pagetop - indentTop() - this.currentHeight);
				}
				else {
					if (somethingAdded) {
						ctx.pagetop = indentTop();
						this.text.moveText(0, -table.cellspacing());
					}
				}
				ctx.oldHeight = this.currentHeight - heightCorrection;

				// calculating the new positions of the table and the cells
				size = Math.min(cells.size(), table.columns());
				int i = 0;
				while (i < size) {
					cell = (PdfCell) cells.get(i);
					if (cell.getTop(-table.cellspacing()) > ctx.lostTableBottom) {
						final float newBottom = ctx.pagetop - difference + cell.getBottom();
						final float neededHeight = cell.remainingHeight();
						if (newBottom > ctx.pagetop - neededHeight) {
							difference += newBottom - (ctx.pagetop - neededHeight);
						}
					}
					i++;
				}
				size = cells.size();
				table.setTop(indentTop());
				table.setBottom(ctx.pagetop - difference + table.getBottom(table.cellspacing()));
				for (i = 0; i < size; i++) {
					cell = (PdfCell) cells.get(i);
					final float newBottom = ctx.pagetop - difference + cell.getBottom();
					float newTop = ctx.pagetop - difference + cell.getTop(-table.cellspacing());
					if (newTop > indentTop() - this.currentHeight) {
						newTop = indentTop() - this.currentHeight;
					}

					cell.setTop(newTop );
					cell.setBottom(newBottom );
				}
			}
		}

        final float tableHeight = table.getTop() - table.getBottom();
        // bugfix by Adauto Martins when have more than two tables and more than one page
        // If continuation of table in other page (bug report #1460051)
        if (isContinue) {
        	this.currentHeight = tableHeight;
        	this.text.moveText(0, -(tableHeight - ctx.oldHeight * 2));
        } else {
        	this.currentHeight = ctx.oldHeight + tableHeight;
        	this.text.moveText(0, -tableHeight);
        }
        // end bugfix
        this.pageEmpty = false;
    }

    private void analyzeRow(final ArrayList rows, final RenderingContext ctx) {
        ctx.maxCellBottom = indentBottom();

        // determine whether row(index) is in a rowspan
        int rowIndex = 0;

        ArrayList row = (ArrayList) rows.get(rowIndex);
        int maxRowspan = 1;
        Iterator iterator = row.iterator();
        while (iterator.hasNext()) {
            final PdfCell cell = (PdfCell) iterator.next();
            maxRowspan = Math.max(ctx.currentRowspan(cell), maxRowspan);
        }
        rowIndex += maxRowspan;

        boolean useTop = true;
        if (rowIndex == rows.size()) {
            rowIndex = rows.size() - 1;
            useTop = false;
        }

        if (rowIndex < 0 || rowIndex >= rows.size()) {
			return;
		}

        row = (ArrayList) rows.get(rowIndex);
        iterator = row.iterator();
        while (iterator.hasNext()) {
            final PdfCell cell = (PdfCell) iterator.next();
            final Rectangle cellRect = cell.rectangle(ctx.pagetop, indentBottom());
            if (useTop) {
                ctx.maxCellBottom = Math.max(ctx.maxCellBottom, cellRect.getTop());
            } else {
                if (ctx.currentRowspan(cell) == 1) {
                    ctx.maxCellBottom = Math.max(ctx.maxCellBottom, cellRect.getBottom());
                }
            }
        }
    }

    private boolean mayBeRemoved(final ArrayList row) {
        final Iterator iterator = row.iterator();
        boolean mayBeRemoved = true;
        while (iterator.hasNext()) {
            final PdfCell cell = (PdfCell) iterator.next();

            mayBeRemoved &= cell.mayBeRemoved();
        }
        return mayBeRemoved;
    }

    private void consumeRowspan(final ArrayList row, final RenderingContext ctx) {
        final Iterator iterator = row.iterator();
        while (iterator.hasNext()) {
            final PdfCell c = (PdfCell) iterator.next();
            ctx.consumeRowspan(c);
        }
    }

    private ArrayList extractRows(final ArrayList cells, final RenderingContext ctx) {
        PdfCell cell;
        PdfCell previousCell = null;
        final ArrayList rows = new ArrayList();
        java.util.List rowCells = new ArrayList();

        final Iterator iterator = cells.iterator();
        while (iterator.hasNext()) {
            cell = (PdfCell) iterator.next();

            boolean isAdded = false;

            boolean isEndOfRow = !iterator.hasNext();
            boolean isCurrentCellPartOfRow = !iterator.hasNext();

            if (previousCell != null) {
                if (cell.getLeft() <= previousCell.getLeft()) {
                    isEndOfRow = true;
                    isCurrentCellPartOfRow = false;
                }
            }

            if (isCurrentCellPartOfRow) {
                rowCells.add(cell);
                isAdded = true;
            }

            if (isEndOfRow) {
                if (!rowCells.isEmpty()) {
                    // add to rowlist
                    rows.add(rowCells);
                }

                // start a new list for next line
                rowCells = new ArrayList();
            }

            if (!isAdded) {
                rowCells.add(cell);
            }

            previousCell = cell;
        }

        if (!rowCells.isEmpty()) {
            rows.add(rowCells);
        }

        // fill row information with rowspan cells to get complete "scan lines"
        for (int i = rows.size() - 1; i >= 0; i--) {
            final ArrayList row = (ArrayList) rows.get(i);
            // iterator through row
            for (int j = 0; j < row.size(); j++) {
                final PdfCell c = (PdfCell) row.get(j);
                final int rowspan = c.rowspan();
                // fill in missing rowspan cells to complete "scan line"
                for (int k = 1; k < rowspan && rows.size() < i+k; k++) {
                    final ArrayList spannedRow = (ArrayList) rows.get(i + k);
                    if (spannedRow.size() > j) {
						spannedRow.add(j, c);
					}
                }
            }
        }

        return rows;
    }

    private void renderCells(final RenderingContext ctx, final java.util.List cells, final boolean hasToFit) throws DocumentException {
        PdfCell cell;
        Iterator iterator;
        if (hasToFit) {
            iterator = cells.iterator();
            while (iterator.hasNext()) {
            	cell = (PdfCell) iterator.next();
            	if (!cell.isHeader()) {
            		if (cell.getBottom() < indentBottom()) {
						return;
					}
            	}
            }
        }
        iterator = cells.iterator();

        while (iterator.hasNext()) {
            cell = (PdfCell) iterator.next();
            if (!ctx.isCellRenderedOnPage(cell, getPageNumber())) {

                float correction = 0;
                if (ctx.numCellRendered(cell) >= 1) {
                    correction = 1.0f;
                }

                this.lines = cell.getLines(ctx.pagetop, indentBottom() - correction);

                // if there is still text to render we render it
                if (this.lines != null && !this.lines.isEmpty()) {
                    // we write the text
                    final float cellTop = cell.getTop(ctx.pagetop - ctx.oldHeight);
                    this.text.moveText(0, cellTop);
                    final float cellDisplacement = flushLines() - cellTop;

                    this.text.moveText(0, cellDisplacement);
                    if (ctx.oldHeight + cellDisplacement > this.currentHeight) {
                        this.currentHeight = ctx.oldHeight + cellDisplacement;
                    }

                    ctx.cellRendered(cell, getPageNumber());
                }
                float indentBottom = Math.max(cell.getBottom(), indentBottom());
                final Rectangle tableRect = ctx.table.rectangle(ctx.pagetop, indentBottom());
                indentBottom = Math.max(tableRect.getBottom(), indentBottom);

                // we paint the borders of the cells
                final Rectangle cellRect = cell.rectangle(tableRect.getTop(), indentBottom);
 				//cellRect.setBottom(cellRect.bottom());
                if (cellRect.getHeight() > 0) {
                    ctx.lostTableBottom = indentBottom;
                    ctx.cellGraphics.rectangle(cellRect);
                }

                // and additional graphics
                final ArrayList images = cell.getImages(ctx.pagetop, indentBottom());
                for (final Iterator i = images.iterator(); i.hasNext();) {
                    final Image image = (Image) i.next();
                    this.graphics.addImage(image);
                }

            }
        }
    }

    /**
     * Returns the bottomvalue of a <CODE>Table</CODE> if it were added to this document.
     *
     * @param	table	the table that may or may not be added to this document
     * @return	a bottom value
     */
    float bottom(final Table table) {
        // constructing a PdfTable
        final PdfTable tmp = new PdfTable(table, indentLeft(), indentRight(), indentTop() - this.currentHeight);
        return tmp.getBottom();
    }

//	[M5] header/footer
    private void doFooter() throws DocumentException {
    	if (this.footer == null) {
			return;
		}
		// Begin added by Edgar Leonardo Prieto Perilla
    	// Avoid footer indentation
    	final float tmpIndentLeft = this.indentation.indentLeft;
    	final float tmpIndentRight = this.indentation.indentRight;
    	// Begin added: Bonf (Marc Schneider) 2003-07-29
        final float tmpListIndentLeft = this.indentation.listIndentLeft;
        final float tmpImageIndentLeft = this.indentation.imageIndentLeft;
        final float tmpImageIndentRight = this.indentation.imageIndentRight;
        // End added: Bonf (Marc Schneider) 2003-07-29

        this.indentation.indentLeft = this.indentation.indentRight = 0;
        // Begin added: Bonf (Marc Schneider) 2003-07-29
        this.indentation.listIndentLeft = 0;
        this.indentation.imageIndentLeft = 0;
        this.indentation.imageIndentRight = 0;
        // End added: Bonf (Marc Schneider) 2003-07-29
        // End Added by Edgar Leonardo Prieto Perilla
        this.footer.setPageNumber(this.pageN);
        this.leading = this.footer.paragraph().getTotalLeading();
        add(this.footer.paragraph());
        // adding the footer limits the height
        this.indentation.indentBottom = this.currentHeight;
        this.text.moveText(left(), indentBottom());
        flushLines();
        this.text.moveText(-left(), -bottom());
        this.footer.setTop(bottom(this.currentHeight));
        this.footer.setBottom(bottom() - 0.75f * this.leading);
        this.footer.setLeft(left());
        this.footer.setRight(right());
        this.graphics.rectangle(this.footer);
        this.indentation.indentBottom = this.currentHeight + this.leading * 2;
        this.currentHeight = 0;
        // Begin added by Edgar Leonardo Prieto Perilla
        this.indentation.indentLeft = tmpIndentLeft;
        this.indentation.indentRight = tmpIndentRight;
        // Begin added: Bonf (Marc Schneider) 2003-07-29
        this.indentation.listIndentLeft = tmpListIndentLeft;
        this.indentation.imageIndentLeft = tmpImageIndentLeft;
        this.indentation.imageIndentRight = tmpImageIndentRight;
        // End added: Bonf (Marc Schneider) 2003-07-29
        // End added by Edgar Leonardo Prieto Perilla
    }

    private void doHeader() throws DocumentException {
        // if there is a header, the header = added
        if (this.header == null) {
			return;
		}
		// Begin added by Edgar Leonardo Prieto Perilla
		// Avoid header indentation
		final float tmpIndentLeft = this.indentation.indentLeft;
		final float tmpIndentRight = this.indentation.indentRight;
        // Begin added: Bonf (Marc Schneider) 2003-07-29
        final float tmpListIndentLeft = this.indentation.listIndentLeft;
        final float tmpImageIndentLeft = this.indentation.imageIndentLeft;
        final float tmpImageIndentRight = this.indentation.imageIndentRight;
        // End added: Bonf (Marc Schneider) 2003-07-29
        this.indentation.indentLeft = this.indentation.indentRight = 0;
        //  Added: Bonf
        this.indentation.listIndentLeft = 0;
        this.indentation.imageIndentLeft = 0;
        this.indentation.imageIndentRight = 0;
        // End added: Bonf
        // Begin added by Edgar Leonardo Prieto Perilla
		this.header.setPageNumber(this.pageN);
        this.leading = this.header.paragraph().getTotalLeading();
        this.text.moveText(0, this.leading);
        add(this.header.paragraph());
        newLine();
        this.indentation.indentTop = this.currentHeight - this.leading;
        this.header.setTop(top() + this.leading);
        this.header.setBottom(indentTop() + this.leading * 2 / 3);
        this.header.setLeft(left());
        this.header.setRight(right());
        this.graphics.rectangle(this.header);
        flushLines();
        this.currentHeight = 0;
        // Begin added by Edgar Leonardo Prieto Perilla
        // Restore indentation
		this.indentation.indentLeft = tmpIndentLeft;
		this.indentation.indentRight = tmpIndentRight;
        // Begin added: Bonf (Marc Schneider) 2003-07-29
        this.indentation.listIndentLeft = tmpListIndentLeft;
        this.indentation.imageIndentLeft = tmpImageIndentLeft;
        this.indentation.imageIndentRight = tmpImageIndentRight;
        // End added: Bonf (Marc Schneider) 2003-07-29
		// End Added by Edgar Leonardo Prieto Perilla
    }
}