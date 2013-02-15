/*
 * $Id: DocWriter.java 3937 2009-05-27 12:56:48Z blowagie $
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

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.pdf.OutputStreamCounter;

/**
 * An abstract <CODE>Writer</CODE> class for documents.
 * <P>
 * <CODE>DocWriter</CODE> is the abstract class of several writers such
 * as <CODE>PdfWriter</CODE> and <CODE>HtmlWriter</CODE>.
 * A <CODE>DocWriter</CODE> can be added as a <CODE>DocListener</CODE>
 * to a certain <CODE>Document</CODE> by getting an instance (see method
 * <CODE>getInstance()</CODE> in the specific writer-classes).
 * Every <CODE>Element</CODE> added to the original <CODE>Document</CODE>
 * will be written to the <CODE>OutputStream</CODE> of the listening
 * <CODE>DocWriter</CODE>.
 *
 * @see   Document
 * @see   DocListener
 */

public abstract class DocWriter implements DocListener {

/** This is some byte that is often used. */
    private static final byte SPACE = (byte)' ';

/** This is some byte that is often used. */
    private static final byte EQUALS = (byte)'=';

/** This is some byte that is often used. */
    private static final byte QUOTE = (byte)'\"';

/** This is some byte that is often used. */
    private static final byte GT = (byte)'>';

/** This is some byte that is often used. */
    private static final byte FORWARD = (byte)'/';

    // membervariables





/** The outputstream of this writer. */
    protected OutputStreamCounter os;

/** Is the writer open for writing? */
    protected boolean open = false;

/** Do we have to pause all writing actions? */
    private final boolean pause = false;

/** Closes the stream on document close */
    private boolean closeStream = true;

    // constructor

    protected DocWriter()  {
    }

/**
 * Constructs a <CODE>DocWriter</CODE>.
 *
 * @param document  The <CODE>Document</CODE> that has to be written
 * @param os  The <CODE>OutputStream</CODE> the writer has to write to.
 */

    protected DocWriter(final Document document, final OutputStream os)  {
        this.os = new OutputStreamCounter(new BufferedOutputStream(os));
    }

    // implementation of the DocListener methods

/**
 * Signals that an <CODE>Element</CODE> was added to the <CODE>Document</CODE>.
 * <P>
 * This method should be overridden in the specific <CODE>DocWriter<CODE> classes
 * derived from this abstract class.
 *
 * @param element A high level object to add
 * @return  <CODE>false</CODE>
 * @throws  DocumentException when a document isn't open yet, or has been closed
 */

    @Override
	public boolean add(final Element element) throws DocumentException {
        return false;
    }

/**
 * Signals that the <CODE>Document</CODE> was opened.
 */

    @Override
	public void open() {
        this.open = true;
    }

/**
 * Sets the pagesize.
 *
 * @param pageSize  the new pagesize
 * @return  a <CODE>boolean</CODE>
 */

    @Override
	public boolean setPageSize(final Rectangle pageSize) {
        return true;
    }

/**
 * Sets the margins.
 * <P>
 * This does nothing. Has to be overridden if needed.
 *
 * @param marginLeft    the margin on the left
 * @param marginRight   the margin on the right
 * @param marginTop   the margin on the top
 * @param marginBottom  the margin on the bottom
 * @return  <CODE>false</CODE>
 */

    @Override
	public boolean setMargins(final float marginLeft, final float marginRight, final float marginTop, final float marginBottom) {
        return false;
    }

/**
 * Signals that an new page has to be started.
 * <P>
 * This does nothing. Has to be overridden if needed.
 *
 * @return  <CODE>true</CODE> if the page was added, <CODE>false</CODE> if not.
 */

    @Override
	public boolean newPage() {
        if (!this.open) {
            return false;
        }
        return true;
    }

/**
 * Changes the header of this document.
 * <P>
 * This method should be overridden in the specific <CODE>DocWriter<CODE> classes
 * derived from this abstract class if they actually support the use of
 * headers.
 *
 * @param header    the new header
 */

    @Override
	public void setHeader(final HeaderFooter header) {
    }

/**
 * Resets the header of this document.
 * <P>
 * This method should be overridden in the specific <CODE>DocWriter<CODE> classes
 * derived from this abstract class if they actually support the use of
 * headers.
 */

    @Override
	public void resetHeader() {
    }

/**
 * Changes the footer of this document.
 * <P>
 * This method should be overridden in the specific <CODE>DocWriter<CODE> classes
 * derived from this abstract class if they actually support the use of
 * footers.
 *
 * @param footer    the new footer
 */

    @Override
	public void setFooter(final HeaderFooter footer) {
    }

/**
 * Resets the footer of this document.
 * <P>
 * This method should be overridden in the specific <CODE>DocWriter<CODE> classes
 * derived from this abstract class if they actually support the use of
 * footers.
 */

    @Override
	public void resetFooter() {
    }

/**
 * Sets the page number to 0.
 * <P>
 * This method should be overridden in the specific <CODE>DocWriter<CODE> classes
 * derived from this abstract class if they actually support the use of
 * pagenumbers.
 */

    @Override
	public void resetPageCount() {
    }

/**
 * Sets the page number.
 * <P>
 * This method should be overridden in the specific <CODE>DocWriter<CODE> classes
 * derived from this abstract class if they actually support the use of
 * pagenumbers.
 *
 * @param pageN   the new page number
 */

    @Override
	public void setPageCount(final int pageN) {
    }

/**
 * Signals that the <CODE>Document</CODE> was closed and that no other
 * <CODE>Elements</CODE> will be added.
 */

    @Override
	public void close() {
        this.open = false;
        try {
            this.os.flush();
            if (this.closeStream) {
				this.os.close();
			}
        }
        catch(final IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }

    // methods

/** Converts a <CODE>String</CODE> into a <CODE>Byte</CODE> array
 * according to the ISO-8859-1 codepage.
 * @param text the text to be converted
 * @return the conversion result
 */

    public static final byte[] getISOBytes(final String text)
    {
        if (text == null) {
			return null;
		}
        final int len = text.length();
        final byte b[] = new byte[len];
        for (int k = 0; k < len; ++k) {
			b[k] = (byte)text.charAt(k);
		}
        return b;
    }


    /**
     * Checks if writing is paused.
     *
     * @return		<CODE>true</CODE> if writing temporarily has to be paused, <CODE>false</CODE> otherwise.
     */

    public boolean isPaused() {
        return this.pause;
    }

/**
 * Writes a <CODE>String</CODE> to the <CODE>OutputStream</CODE>.
 *
 * @param string    the <CODE>String</CODE> to write
 * @throws IOException
 */

    private void write(final String string) throws IOException {
        this.os.write(getISOBytes(string));
    }



/**
 * Writes a key-value pair to the outputstream.
 *
 * @param   key     the name of an attribute
 * @param   value   the value of an attribute
 * @throws IOException
 */

    private void write(final String key, final String value)
    throws IOException {
        this.os.write(SPACE);
        write(key);
        this.os.write(EQUALS);
        this.os.write(QUOTE);
        write(value);
        this.os.write(QUOTE);
    }

    /** Checks if the stream is to be closed on document close
     * @return true if the stream is closed on document close
     *
     */
    public boolean isCloseStream() {
        return this.closeStream;
    }

    /** Sets the close state of the stream after document close
     * @param closeStream true if the stream is closed on document close
     *
     */
    public void setCloseStream(final boolean closeStream) {
        this.closeStream = closeStream;
    }

    /**
     * @see com.lowagie.text.DocListener#setMarginMirroring(boolean)
     */
    @Override
	public boolean setMarginMirroring(final boolean MarginMirroring) {
        return false;
    }

    /**
     * @see com.lowagie.text.DocListener#setMarginMirroring(boolean)
     * @since	2.1.6
     */
    @Override
	public boolean setMarginMirroringTopBottom(final boolean MarginMirroring) {
        return false;
    }

}
