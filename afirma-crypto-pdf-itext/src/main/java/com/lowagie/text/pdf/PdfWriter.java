/*
 * $Id: PdfWriter.java 3948 2009-06-03 15:17:22Z blowagie $
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

import java.awt.Color;
import java.awt.color.ICC_Profile;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;

import com.lowagie.text.DocWriter;
import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.ImgJBIG2;
import com.lowagie.text.ImgWMF;
import com.lowagie.text.Rectangle;
import com.lowagie.text.Table;
import com.lowagie.text.pdf.collection.PdfCollection;
import com.lowagie.text.pdf.events.PdfPageEventForwarder;
import com.lowagie.text.pdf.interfaces.PdfAnnotations;
import com.lowagie.text.pdf.interfaces.PdfDocumentActions;
import com.lowagie.text.pdf.interfaces.PdfEncryptionSettings;
import com.lowagie.text.pdf.interfaces.PdfPageActions;
import com.lowagie.text.pdf.interfaces.PdfRunDirection;
import com.lowagie.text.pdf.interfaces.PdfVersion;
import com.lowagie.text.pdf.interfaces.PdfViewerPreferences;
import com.lowagie.text.pdf.interfaces.PdfXConformance;
import com.lowagie.text.pdf.internal.PdfVersionImp;
import com.lowagie.text.pdf.internal.PdfXConformanceImp;
import com.lowagie.text.xml.xmp.XmpWriter;

/**
 * A <CODE>DocWriter</CODE> class for PDF.
 * <P>
 * When this <CODE>PdfWriter</CODE> is added
 * to a certain <CODE>PdfDocument</CODE>, the PDF representation of every Element
 * added to this Document will be written to the outputstream.</P>
 */

public class PdfWriter extends DocWriter implements
	PdfViewerPreferences,
	PdfEncryptionSettings,
	PdfVersion,
	PdfDocumentActions,
	PdfPageActions,
	PdfXConformance,
	PdfRunDirection,
	PdfAnnotations {

	/**
	 * The highest generation number possible.
	 * @since	iText 2.1.6
	 */
	static final int GENERATION_MAX = 65535;

// INNER CLASSES

    /**
     * This class generates the structure of a PDF document.
     * <P>
     * This class covers the third section of Chapter 5 in the 'Portable Document Format
     * Reference Manual version 1.3' (page 55-60). It contains the body of a PDF document
     * (section 5.14) and it can also generate a Cross-reference Table (section 5.15).
     *
     * @see		PdfWriter
     * @see		PdfObject
     * @see		PdfIndirectObject
     */

    static class PdfBody {

        // inner classes

        /**
         * <CODE>PdfCrossReference</CODE> is an entry in the PDF Cross-Reference table.
         */

        private static class PdfCrossReference implements Comparable {

            // membervariables
            private final int type;

            /**	Byte offset in the PDF file. */
            private final int offset;

            private final int refnum;
            /**	generation of the object. */
            private final int generation;

            // constructors
            /**
             * Constructs a cross-reference element for a PdfIndirectObject.
             * @param refnum
             * @param	offset		byte offset of the object
             * @param	generation	generation number of the object
             */

            private PdfCrossReference(final int refnum, final int offset, final int generation) {
                this.type = 0;
                this.offset = offset;
                this.refnum = refnum;
                this.generation = generation;
            }

            /**
             * Constructs a cross-reference element for a PdfIndirectObject.
             * @param refnum
             * @param	offset		byte offset of the object
             */

            private PdfCrossReference(final int refnum, final int offset) {
                this.type = 1;
                this.offset = offset;
                this.refnum = refnum;
                this.generation = 0;
            }

            private PdfCrossReference(final int type, final int refnum, final int offset, final int generation) {
                this.type = type;
                this.offset = offset;
                this.refnum = refnum;
                this.generation = generation;
            }

            private int getRefnum() {
                return this.refnum;
            }

            /**
             * Returns the PDF representation of this <CODE>PdfObject</CODE>.
             * @param os
             * @throws IOException
             */

            private void toPdf(final OutputStream os) throws IOException {
                final StringBuffer off = new StringBuffer("0000000000").append(this.offset);
                off.delete(0, off.length() - 10);
                final StringBuffer gen = new StringBuffer("00000").append(this.generation);
                gen.delete(0, gen.length() - 5);

                off.append(' ').append(gen).append(this.generation == GENERATION_MAX ? " f \n" : " n \n");
                os.write(getISOBytes(off.toString()));
            }

            /**
             * Writes PDF syntax to the OutputStream
             * @param midSize
             * @param os
             * @throws IOException
             */
            private void toPdf(int midSize, final OutputStream os) throws IOException {
                os.write((byte)this.type);
                while (--midSize >= 0) {
					os.write((byte)(this.offset >>> 8 * midSize & 0xff));
				}
                os.write((byte)(this.generation >>> 8 & 0xff));
                os.write((byte)(this.generation & 0xff));
            }

            /**
             * @see java.lang.Comparable#compareTo(java.lang.Object)
             */
            @Override
			public int compareTo(final Object o) {
                final PdfCrossReference other = (PdfCrossReference)o;
                return this.refnum < other.refnum ? -1 : this.refnum==other.refnum ? 0 : 1;
            }

            /**
             * @see java.lang.Object#equals(java.lang.Object)
             */
            @Override
			public boolean equals(final Object obj) {
                if (obj instanceof PdfCrossReference) {
                    final PdfCrossReference other = (PdfCrossReference)obj;
                    return this.refnum == other.refnum;
                } else {
					return false;
				}
            }

            /**
             * @see java.lang.Object#hashCode()
             */
            @Override
			public int hashCode() {
				return this.refnum;
			}

        }

        private static final int OBJSINSTREAM = 200;

        // membervariables

        /** array containing the cross-reference table of the normal objects. */
        private final TreeSet xrefs;
        private int refnum;
        /** the current byte position in the body. */
        private int position;
        private final PdfWriter writer;
        private ByteBuffer index;
        private ByteBuffer streamObjects;
        private int currentObjNum;
        private int numObj = 0;

        // constructors

        /**
         * Constructs a new <CODE>PdfBody</CODE>.
         * @param writer
         */
        private PdfBody(final PdfWriter writer) {
            this.xrefs = new TreeSet();
            this.xrefs.add(new PdfCrossReference(0, 0, GENERATION_MAX));
            this.position = writer.getOs().getCounter();
            this.refnum = 1;
            this.writer = writer;
        }

        // methods

        void setRefnum(final int refnum) {
            this.refnum = refnum;
        }

        private PdfWriter.PdfBody.PdfCrossReference addToObjStm(final PdfObject obj, final int nObj) throws IOException {
            if (this.numObj >= OBJSINSTREAM) {
				flushObjStm();
			}
            if (this.index == null) {
                this.index = new ByteBuffer();
                this.streamObjects = new ByteBuffer();
                this.currentObjNum = getIndirectReferenceNumber();
                this.numObj = 0;
            }
            final int p = this.streamObjects.size();
            final int idx = this.numObj++;
            final PdfEncryption enc = this.writer.crypto;
            this.writer.crypto = null;
            obj.toPdf(this.writer, this.streamObjects);
            this.writer.crypto = enc;
            this.streamObjects.append(' ');
            this.index.append(nObj).append(' ').append(p).append(' ');
            return new PdfWriter.PdfBody.PdfCrossReference(2, nObj, this.currentObjNum, idx);
        }

        private void flushObjStm() throws IOException {
            if (this.numObj == 0) {
				return;
			}
            final int first = this.index.size();
            this.index.append(this.streamObjects);
            final PdfStream stream = new PdfStream(this.index.toByteArray());
            stream.flateCompress(this.writer.getCompressionLevel());
            stream.put(PdfName.TYPE, PdfName.OBJSTM);
            stream.put(PdfName.N, new PdfNumber(this.numObj));
            stream.put(PdfName.FIRST, new PdfNumber(first));
            add(stream, this.currentObjNum);
            this.index = null;
            this.streamObjects = null;
            this.numObj = 0;
        }

        /**
         * Adds a <CODE>PdfObject</CODE> to the body.
         * <P>
         * This methods creates a <CODE>PdfIndirectObject</CODE> with a
         * certain number, containing the given <CODE>PdfObject</CODE>.
         * It also adds a <CODE>PdfCrossReference</CODE> for this object
         * to an <CODE>ArrayList</CODE> that will be used to build the
         * Cross-reference Table.
         *
         * @param		object			a <CODE>PdfObject</CODE>
         * @return		a <CODE>PdfIndirectObject</CODE>
         * @throws IOException
         */

        PdfIndirectObject add(final PdfObject object) throws IOException {
            return add(object, getIndirectReferenceNumber());
        }

        private PdfIndirectObject add(final PdfObject object, final boolean inObjStm) throws IOException {
            return add(object, getIndirectReferenceNumber(), inObjStm);
        }

        /**
         * Gets a PdfIndirectReference for an object that will be created in the future.
         * @return a PdfIndirectReference
         */

        PdfIndirectReference getPdfIndirectReference() {
            return new PdfIndirectReference(0, getIndirectReferenceNumber());
        }

        int getIndirectReferenceNumber() {
            final int n = this.refnum++;
            this.xrefs.add(new PdfCrossReference(n, 0, GENERATION_MAX));
            return n;
        }

        /**
         * Adds a <CODE>PdfObject</CODE> to the body given an already existing
         * PdfIndirectReference.
         * <P>
         * This methods creates a <CODE>PdfIndirectObject</CODE> with the number given by
         * <CODE>ref</CODE>, containing the given <CODE>PdfObject</CODE>.
         * It also adds a <CODE>PdfCrossReference</CODE> for this object
         * to an <CODE>ArrayList</CODE> that will be used to build the
         * Cross-reference Table.
         *
         * @param		object			a <CODE>PdfObject</CODE>
         * @param		ref		        a <CODE>PdfIndirectReference</CODE>
         * @return		a <CODE>PdfIndirectObject</CODE>
         * @throws IOException
         */

        PdfIndirectObject add(final PdfObject object, final PdfIndirectReference ref) throws IOException {
            return add(object, ref.getNumber());
        }

        private PdfIndirectObject add(final PdfObject object, final PdfIndirectReference ref, final boolean inObjStm) throws IOException {
            return add(object, ref.getNumber(), inObjStm);
        }

        private PdfIndirectObject add(final PdfObject object, final int refNumber) throws IOException {
            return add(object, refNumber, true); // to false
        }

        private PdfIndirectObject add(final PdfObject object, final int refNumber, final boolean inObjStm) throws IOException {
            if (inObjStm && object.canBeInObjStm() && this.writer.isFullCompression()) {
                final PdfCrossReference pxref = addToObjStm(object, refNumber);
                final PdfIndirectObject indirect = new PdfIndirectObject(refNumber, object, this.writer);
                if (!this.xrefs.add(pxref)) {
                    this.xrefs.remove(pxref);
                    this.xrefs.add(pxref);
                }
                return indirect;
            }
            else {
                final PdfIndirectObject indirect = new PdfIndirectObject(refNumber, object, this.writer);
                final PdfCrossReference pxref = new PdfCrossReference(refNumber, this.position);
                if (!this.xrefs.add(pxref)) {
                    this.xrefs.remove(pxref);
                    this.xrefs.add(pxref);
                }
                indirect.writeTo(this.writer.getOs());
                this.position = this.writer.getOs().getCounter();
                return indirect;
            }
        }

        /**
         * Returns the offset of the Cross-Reference table.
         *
         * @return		an offset
         */

        int offset() {
            return this.position;
        }

        /**
         * Returns the total number of objects contained in the CrossReferenceTable of this <CODE>Body</CODE>.
         *
         * @return	a number of objects
         */

        int size() {
            return Math.max(((PdfCrossReference)this.xrefs.last()).getRefnum() + 1, this.refnum);
        }

        /**
         * Returns the CrossReferenceTable of the <CODE>Body</CODE>.
         * @param os
         * @param root
         * @param info
         * @param encryption
         * @param fileID
         * @param prevxref
         * @throws IOException
         */

        void writeCrossReferenceTable(final OutputStream os, final PdfIndirectReference root, final PdfIndirectReference info, final PdfIndirectReference encryption, final PdfObject fileID, final int prevxref) throws IOException {
            int refNumber = 0;
            if (this.writer.isFullCompression()) {
                flushObjStm();
                refNumber = getIndirectReferenceNumber();
                this.xrefs.add(new PdfCrossReference(refNumber, this.position));
            }
            PdfCrossReference entry = (PdfCrossReference)this.xrefs.first();
            int first = entry.getRefnum();
            int len = 0;
            final ArrayList sections = new ArrayList();
            for (final Iterator i = this.xrefs.iterator(); i.hasNext(); ) {
                entry = (PdfCrossReference)i.next();
                if (first + len == entry.getRefnum()) {
					++len;
				} else {
                    sections.add(new Integer(first));
                    sections.add(new Integer(len));
                    first = entry.getRefnum();
                    len = 1;
                }
            }
            sections.add(new Integer(first));
            sections.add(new Integer(len));
            if (this.writer.isFullCompression()) {
                int mid = 4;
                int mask = 0xff000000;
                for (; mid > 1; --mid) {
                    if ((mask & this.position) != 0) {
						break;
					}
                    mask >>>= 8;
                }
                ByteBuffer buf = new ByteBuffer();

                for (final Iterator i = this.xrefs.iterator(); i.hasNext(); ) {
                    entry = (PdfCrossReference) i.next();
                    entry.toPdf(mid, buf);
                }
                final PdfStream xr = new PdfStream(buf.toByteArray());
                buf = null;
                xr.flateCompress(this.writer.getCompressionLevel());
                xr.put(PdfName.SIZE, new PdfNumber(size()));
                xr.put(PdfName.ROOT, root);
                if (info != null) {
                    xr.put(PdfName.INFO, info);
                }
                if (encryption != null) {
					xr.put(PdfName.ENCRYPT, encryption);
				}
                if (fileID != null) {
					xr.put(PdfName.ID, fileID);
				}
                xr.put(PdfName.W, new PdfArray(new int[]{1, mid, 2}));
                xr.put(PdfName.TYPE, PdfName.XREF);
                final PdfArray idx = new PdfArray();
                for (int k = 0; k < sections.size(); ++k) {
					idx.add(new PdfNumber(((Integer)sections.get(k)).intValue()));
				}
                xr.put(PdfName.INDEX, idx);
                if (prevxref > 0) {
					xr.put(PdfName.PREV, new PdfNumber(prevxref));
				}
                final PdfEncryption enc = this.writer.crypto;
                this.writer.crypto = null;
                final PdfIndirectObject indirect = new PdfIndirectObject(refNumber, xr, this.writer);
                indirect.writeTo(this.writer.getOs());
                this.writer.crypto = enc;
            }
            else {
                os.write(getISOBytes("xref\n"));
                final Iterator i = this.xrefs.iterator();
                for (int k = 0; k < sections.size(); k += 2) {
                    first = ((Integer)sections.get(k)).intValue();
                    len = ((Integer)sections.get(k + 1)).intValue();
                    os.write(getISOBytes(String.valueOf(first)));
                    os.write(getISOBytes(" "));
                    os.write(getISOBytes(String.valueOf(len)));
                    os.write('\n');
                    while (len-- > 0) {
                        entry = (PdfCrossReference) i.next();
                        entry.toPdf(os);
                    }
                }
            }
        }
    }

    /**
     * <CODE>PdfTrailer</CODE> is the PDF Trailer object.
     * <P>
     * This object is described in the 'Portable Document Format Reference Manual version 1.3'
     * section 5.16 (page 59-60).
     */

    static class PdfTrailer extends PdfDictionary {

        // membervariables

        private final int offset;

        // constructors

        /**
         * Constructs a PDF-Trailer.
         *
         * @param		size		the number of entries in the <CODE>PdfCrossReferenceTable</CODE>
         * @param		offset		offset of the <CODE>PdfCrossReferenceTable</CODE>
         * @param		root		an indirect reference to the root of the PDF document
         * @param		info		an indirect reference to the info object of the PDF document
         * @param encryption
         * @param fileID
         * @param prevxref
         */

        PdfTrailer(final int size, final int offset, final PdfIndirectReference root, final PdfIndirectReference info, final PdfIndirectReference encryption, final PdfObject fileID, final int prevxref) {
            this.offset = offset;
            put(PdfName.SIZE, new PdfNumber(size));
            put(PdfName.ROOT, root);
            if (info != null) {
                put(PdfName.INFO, info);
            }
            if (encryption != null) {
				put(PdfName.ENCRYPT, encryption);
			}
            if (fileID != null) {
				put(PdfName.ID, fileID);
			}
            if (prevxref > 0) {
				put(PdfName.PREV, new PdfNumber(prevxref));
			}
        }

        /**
         * Returns the PDF representation of this <CODE>PdfObject</CODE>.
         * @param writer
         * @param os
         * @throws IOException
         */
        @Override
		public void toPdf(final PdfWriter writer, final OutputStream os) throws IOException {
            os.write(getISOBytes("trailer\n"));
            super.toPdf(null, os);
            os.write(getISOBytes("\nstartxref\n"));
            os.write(getISOBytes(String.valueOf(this.offset)));
            os.write(getISOBytes("\n%%EOF\n"));
        }
    }

//	ESSENTIALS

//	Construct a PdfWriter instance

    /**
     * Constructs a <CODE>PdfWriter</CODE>.
     */
    protected PdfWriter() {
    }

    /**
     * Constructs a <CODE>PdfWriter</CODE>.
     * <P>
     * Remark: a PdfWriter can only be constructed by calling the method
     * <CODE>getInstance(Document document, OutputStream os)</CODE>.
     *
     * @param	document	The <CODE>PdfDocument</CODE> that has to be written
     * @param	os			The <CODE>OutputStream</CODE> the writer has to write to.
     */

    protected PdfWriter(final PdfDocument document, final OutputStream os) {
        super(document, os);
        this.pdf = document;
        this.directContent = new PdfContentByte(this);
        this.directContentUnder = new PdfContentByte(this);
    }





//	the PdfDocument instance

    /** the pdfdocument object. */
    protected PdfDocument pdf;

    /**
     * Gets the <CODE>PdfDocument</CODE> associated with this writer.
     * @return the <CODE>PdfDocument</CODE>
     */

    PdfDocument getPdfDocument() {
        return this.pdf;
    }

    /**
     * Use this method to get the info dictionary if you want to
     * change it directly (add keys and values to the info dictionary).
     * @return the info dictionary
     */
    public PdfDictionary getInfo() {
        return this.pdf.getInfo();
    }



    /**
     * Sets the initial leading for the PDF document.
     * This has to be done before the document is opened.
     * @param	leading	the initial leading
     * @since	2.1.6
     * @throws	DocumentException	if you try setting the leading after the document was opened.
     */
    public void setInitialLeading(final float leading) throws DocumentException {
    	if (this.open) {
			throw new DocumentException("You can't set the initial leading if the document is already open.");
		}
    	this.pdf.setLeading(leading);
    }

//	the PdfDirectContentByte instances

/*
 * You should see Direct Content as a canvas on which you can draw
 * graphics and text. One canvas goes on top of the page (getDirectContent),
 * the other goes underneath (getDirectContentUnder).
 * You can always the same object throughout your document,
 * even if you have moved to a new page. Whatever you add on
 * the canvas will be displayed on top or under the current page.
 */

    /** The direct content in this document. */
    private PdfContentByte directContent;

    /** The direct content under in this document. */
    private PdfContentByte directContentUnder;

    /**
     * Use this method to get the direct content for this document.
     * There is only one direct content, multiple calls to this method
     * will allways retrieve the same object.
     * @return the direct content
     */

    public PdfContentByte getDirectContent() {
        if (!this.open) {
			throw new RuntimeException("The document is not open.");
		}
        return this.directContent;
    }

    /**
     * Use this method to get the direct content under for this document.
     * There is only one direct content, multiple calls to this method
     * will always retrieve the same object.
     * @return the direct content
     */

    public PdfContentByte getDirectContentUnder() {
        if (!this.open) {
			throw new RuntimeException("The document is not open.");
		}
        return this.directContentUnder;
    }

    /**
     * Resets all the direct contents to empty.
     * This happens when a new page is started.
     */
    void resetContent() {
        this.directContent.reset();
        this.directContentUnder.reset();
    }

//	PDF body

/*
 * A PDF file has 4 parts: a header, a body, a cross-reference table, and a trailer.
 * The body contains all the PDF objects that make up the PDF document.
 * Each element gets a reference (a set of numbers) and the byte position of
 * every object is stored in the cross-reference table.
 * Use these methods only if you know what you're doing.
 */

    /** body of the PDF document */
    protected PdfBody body;

    /**
     * Adds the local destinations to the body of the document.
     * @param dest the <CODE>HashMap</CODE> containing the destinations
     * @throws IOException on error
     */

    void addLocalDestinations(final TreeMap dest) throws IOException {
        for (final Iterator i = dest.entrySet().iterator(); i.hasNext();) {
            final Map.Entry entry = (Map.Entry) i.next();
            final String name = (String) entry.getKey();
            final Object obj[] = (Object[]) entry.getValue();
            final PdfDestination destination = (PdfDestination)obj[2];
            if (obj[1] == null) {
				obj[1] = getPdfIndirectReference();
			}
            if (destination == null) {
				addToBody(new PdfString("invalid_" + name), (PdfIndirectReference)obj[1]);
			} else {
				addToBody(destination, (PdfIndirectReference)obj[1]);
			}
        }
    }

    /**
     * Use this method to add a PDF object to the PDF body.
     * Use this method only if you know what you're doing!
     * @param object
     * @return a PdfIndirectObject
     * @throws IOException
     */
    PdfIndirectObject addToBody(final PdfObject object) throws IOException {
        final PdfIndirectObject iobj = this.body.add(object);
        return iobj;
    }

    /**
     * Use this method to add a PDF object to the PDF body.
     * Use this method only if you know what you're doing!
     * @param object
     * @param inObjStm
     * @return a PdfIndirectObject
     * @throws IOException
     */
    PdfIndirectObject addToBody(final PdfObject object, final boolean inObjStm) throws IOException {
        final PdfIndirectObject iobj = this.body.add(object, inObjStm);
        return iobj;
    }

    /**
     * Use this method to add a PDF object to the PDF body.
     * Use this method only if you know what you're doing!
     * @param object
     * @param ref
     * @return a PdfIndirectObject
     * @throws IOException
     */
    public PdfIndirectObject addToBody(final PdfObject object, final PdfIndirectReference ref) throws IOException {
        final PdfIndirectObject iobj = this.body.add(object, ref);
        return iobj;
    }

    /**
     * Use this method to add a PDF object to the PDF body.
     * Use this method only if you know what you're doing!
     * @param object
     * @param ref
     * @param inObjStm
     * @return a PdfIndirectObject
     * @throws IOException
     */
    PdfIndirectObject addToBody(final PdfObject object, final PdfIndirectReference ref, final boolean inObjStm) throws IOException {
        final PdfIndirectObject iobj = this.body.add(object, ref, inObjStm);
        return iobj;
    }

    /**
     * Use this method to add a PDF object to the PDF body.
     * Use this method only if you know what you're doing!
     * @param object
     * @param refNumber
     * @return a PdfIndirectObject
     * @throws IOException
     */
    PdfIndirectObject addToBody(final PdfObject object, final int refNumber) throws IOException {
        final PdfIndirectObject iobj = this.body.add(object, refNumber);
        return iobj;
    }

    /**
     * Use this method to add a PDF object to the PDF body.
     * Use this method only if you know what you're doing!
     * @param object
     * @param refNumber
     * @param inObjStm
     * @return a PdfIndirectObject
     * @throws IOException
     */
    PdfIndirectObject addToBody(final PdfObject object, final int refNumber, final boolean inObjStm) throws IOException {
        final PdfIndirectObject iobj = this.body.add(object, refNumber, inObjStm);
        return iobj;
    }

    /**
     * Use this to get an <CODE>PdfIndirectReference</CODE> for an object that
     * will be created in the future.
     * Use this method only if you know what you're doing!
     * @return the <CODE>PdfIndirectReference</CODE>
     */

    public PdfIndirectReference getPdfIndirectReference() {
        return this.body.getPdfIndirectReference();
    }

    int getIndirectReferenceNumber() {
        return this.body.getIndirectReferenceNumber();
    }

    /**
     * Returns the outputStreamCounter.
     * @return the outputStreamCounter
     */
    private OutputStreamCounter getOs() {
        return this.os;
    }


//	PDF Catalog

/*
 * The Catalog is also called the root object of the document.
 * Whereas the Cross-Reference maps the objects number with the
 * byte offset so that the viewer can find the objects, the
 * Catalog tells the viewer the numbers of the objects needed
 * to render the document.
 */

    protected PdfDictionary getCatalog(final PdfIndirectReference rootObj)
    {
        final PdfDictionary catalog = this.pdf.getCatalog(rootObj);
        // [F12] tagged PDF
        if (this.tagged) {
            try {
                getStructureTreeRoot().buildTree();
            }
            catch (final Exception e) {
                throw new ExceptionConverter(e);
            }
            catalog.put(PdfName.STRUCTTREEROOT, this.structureTreeRoot.getReference());
            final PdfDictionary mi = new PdfDictionary();
            mi.put(PdfName.MARKED, PdfBoolean.PDFTRUE);
            if (this.userProperties) {
				mi.put(PdfName.USERPROPERTIES, PdfBoolean.PDFTRUE);
			}
            catalog.put(PdfName.MARKINFO, mi);
        }
        // [F13] OCG
        if (!this.documentOCG.isEmpty()) {
        	fillOCProperties(false);
        	catalog.put(PdfName.OCPROPERTIES, this.OCProperties);
        }
        return catalog;
    }

    /** Holds value of property extraCatalog this is used for Output Intents. */
    private PdfDictionary extraCatalog;

    /**
     * Sets extra keys to the catalog.
     * @return the catalog to change
     */
    public PdfDictionary getExtraCatalog() {
        if (this.extraCatalog == null) {
			this.extraCatalog = new PdfDictionary();
		}
        return this.extraCatalog;
    }

//	PdfPages

/*
 * The page root keeps the complete page tree of the document.
 * There's an entry in the Catalog that refers to the root
 * of the page tree, the page tree contains the references
 * to pages and other page trees.
 */

    /** The root of the page tree. */
    protected PdfPages root = new PdfPages(this);
    /** The PdfIndirectReference to the pages. */
    private final ArrayList pageReferences = new ArrayList();
    /** The current page number. */
    private int currentPageNumber = 1;
    /**
     * The value of the Tabs entry in the page dictionary.
     * @since	2.1.5
     */
    private PdfName tabs = null;





    /**
     * Use this method to get a reference to a page existing or not.
     * If the page does not exist yet the reference will be created
     * in advance. If on closing the document, a page number greater
     * than the total number of pages was requested, an exception
     * is thrown.
     * @param page the page number. The first page is 1
     * @return the reference to the page
     */
    public PdfIndirectReference getPageReference(int page) {
        --page;
        if (page < 0) {
			throw new IndexOutOfBoundsException("The page numbers start at 1.");
		}
        PdfIndirectReference ref;
        if (page < this.pageReferences.size()) {
            ref = (PdfIndirectReference)this.pageReferences.get(page);
            if (ref == null) {
                ref = this.body.getPdfIndirectReference();
                this.pageReferences.set(page, ref);
            }
        }
        else {
            final int empty = page - this.pageReferences.size();
            for (int k = 0; k < empty; ++k) {
				this.pageReferences.add(null);
			}
            ref = this.body.getPdfIndirectReference();
            this.pageReferences.add(ref);
        }
        return ref;
    }

    /**
     * Gets the pagenumber of this document.
     * This number can be different from the real pagenumber,
     * if you have (re)set the page number previously.
     * @return a page number
     */

    public int getPageNumber() {
        return this.pdf.getPageNumber();
    }

    PdfIndirectReference getCurrentPage() {
        return getPageReference(this.currentPageNumber);
    }

    public int getCurrentPageNumber() {
        return this.currentPageNumber;
    }

    /**
     * Sets the value for the Tabs entry in the page tree.
     * @param	tabs	Can be PdfName.R, PdfName.C or PdfName.S.
     * Since the Adobe Extensions Level 3, it can also be PdfName.A
     * or PdfName.W
     * @since	2.1.5
     */
    public void setTabs(final PdfName tabs) {
    	this.tabs = tabs;
    }

    /**
     * Returns the value to be used for the Tabs entry in the page tree.
     * @since	2.1.5
     */
    public PdfName getTabs() {
    	return this.tabs;
    }

    /**
     * Adds some <CODE>PdfContents</CODE> to this Writer.
     * <P>
     * The document has to be open before you can begin to add content
     * to the body of the document.
     *
     * @return a <CODE>PdfIndirectReference</CODE>
     * @param page the <CODE>PdfPage</CODE> to add
     * @param contents the <CODE>PdfContents</CODE> of the page
     * @throws PdfException on error
     */

    PdfIndirectReference add(final PdfPage page, final PdfContents contents) throws PdfException {
        if (!this.open) {
            throw new PdfException("The document isn't open.");
        }
        PdfIndirectObject object;
        try {
            object = addToBody(contents);
        }
        catch(final IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
        page.add(object.getIndirectReference());
        // [U5]
        if (this.group != null) {
            page.put(PdfName.GROUP, this.group);
            this.group = null;
        }
        else if (this.rgbTransparencyBlending) {
            final PdfDictionary pp = new PdfDictionary();
            pp.put(PdfName.TYPE, PdfName.GROUP);
            pp.put(PdfName.S, PdfName.TRANSPARENCY);
            pp.put(PdfName.CS, PdfName.DEVICERGB);
            page.put(PdfName.GROUP, pp);
        }
        this.root.addPage(page);
        this.currentPageNumber++;
        return null;
    }

//	page events

/*
 * Page events are specific for iText, not for PDF.
 * Upon specific events (for instance when a page starts
 * or ends), the corresponding method in the page event
 * implementation that is added to the writer is invoked.
 */

    /** The <CODE>PdfPageEvent</CODE> for this document. */
    private PdfPageEvent pageEvent;

    /**
     * Sets the <CODE>PdfPageEvent</CODE> for this document.
     * @param event the <CODE>PdfPageEvent</CODE> for this document
     */

    public void setPageEvent(final PdfPageEvent event) {
    	if (event == null) {
			this.pageEvent = null;
		} else if (this.pageEvent == null) {
			this.pageEvent = event;
		} else if (this.pageEvent instanceof PdfPageEventForwarder) {
			((PdfPageEventForwarder)this.pageEvent).addPageEvent(event);
		} else {
    		final PdfPageEventForwarder forward = new PdfPageEventForwarder();
    		forward.addPageEvent(this.pageEvent);
    		forward.addPageEvent(event);
    		this.pageEvent = forward;
    	}
    }

    /**
     * Gets the <CODE>PdfPageEvent</CODE> for this document or <CODE>null</CODE>
     * if none is set.
     * @return the <CODE>PdfPageEvent</CODE> for this document or <CODE>null</CODE>
     * if none is set
     */

    public PdfPageEvent getPageEvent() {
        return this.pageEvent;
    }

//	Open and Close methods + method that create the PDF

    /** A number referring to the previous Cross-Reference Table. */
    protected int prevxref = 0;

    /**
     * Signals that the <CODE>Document</CODE> has been opened and that
     * <CODE>Elements</CODE> can be added.
     * <P>
     * When this method is called, the PDF-document header is
     * written to the outputstream.
     * @see com.lowagie.text.DocWriter#open()
     */
    @Override
	public void open() {
        super.open();
        try {
        	this.pdf_version.writeHeader(this.os);
            this.body = new PdfBody(this);
            if (this.pdfxConformance.isPdfX32002()) {
                final PdfDictionary sec = new PdfDictionary();
                sec.put(PdfName.GAMMA, new PdfArray(new float[]{2.2f,2.2f,2.2f}));
                sec.put(PdfName.MATRIX, new PdfArray(new float[]{0.4124f,0.2126f,0.0193f,0.3576f,0.7152f,0.1192f,0.1805f,0.0722f,0.9505f}));
                sec.put(PdfName.WHITEPOINT, new PdfArray(new float[]{0.9505f,1f,1.089f}));
                final PdfArray arr = new PdfArray(PdfName.CALRGB);
                arr.add(sec);
                setDefaultColorspace(PdfName.DEFAULTRGB, addToBody(arr).getIndirectReference());
            }
        }
        catch(final IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }

    /**
     * Signals that the <CODE>Document</CODE> was closed and that no other
     * <CODE>Elements</CODE> will be added.
     * <P>
     * The pages-tree is built and written to the outputstream.
     * A Catalog is constructed, as well as an Info-object,
     * the reference table is composed and everything is written
     * to the outputstream embedded in a Trailer.
     * @see com.lowagie.text.DocWriter#close()
     */
    @Override
	public void close() {
        if (this.open) {
            if (this.currentPageNumber - 1 != this.pageReferences.size()) {
				throw new RuntimeException("The page " + this.pageReferences.size() +
                " was requested but the document has only " + (this.currentPageNumber - 1) + " pages.");
			}
            this.pdf.close();
            try {
                addSharedObjectsToBody();
                // add the root to the body
                final PdfIndirectReference rootRef = this.root.writePageTree();
                // make the catalog-object and add it to the body
                final PdfDictionary catalog = getCatalog(rootRef);
                // [C9] if there is XMP data to add: add it
                if (this.xmpMetadata != null) {
                	final PdfStream xmp = new PdfStream(this.xmpMetadata);
                	xmp.put(PdfName.TYPE, PdfName.METADATA);
                	xmp.put(PdfName.SUBTYPE, PdfName.XML);
                    if (this.crypto != null && !this.crypto.isMetadataEncrypted()) {
                        final PdfArray ar = new PdfArray();
                        ar.add(PdfName.CRYPT);
                        xmp.put(PdfName.FILTER, ar);
                    }
                	catalog.put(PdfName.METADATA, this.body.add(xmp).getIndirectReference());
                }
                // [C10] make pdfx conformant
                if (isPdfX()) {
                    this.pdfxConformance.completeInfoDictionary(getInfo());
                    this.pdfxConformance.completeExtraCatalog(getExtraCatalog());
                }
                // [C11] Output Intents
                if (this.extraCatalog != null) {
                    catalog.mergeDifferent(this.extraCatalog);
                }

                writeOutlines(catalog, false);

                // add the Catalog to the body
                final PdfIndirectObject indirectCatalog = addToBody(catalog, false);
                // add the info-object to the body
                final PdfIndirectObject infoObj = addToBody(getInfo(), false);

                // [F1] encryption
                PdfIndirectReference encryption = null;
                PdfObject fileID = null;
                this.body.flushObjStm();
                if (this.crypto != null) {
                    final PdfIndirectObject encryptionObject = addToBody(this.crypto.getEncryptionDictionary(), false);
                    encryption = encryptionObject.getIndirectReference();
                    fileID = this.crypto.getFileID();
                } else {
					fileID = PdfEncryption.createInfoId(PdfEncryption.createDocumentId());
				}

                // write the cross-reference table of the body
                this.body.writeCrossReferenceTable(this.os, indirectCatalog.getIndirectReference(),
                    infoObj.getIndirectReference(), encryption,  fileID, this.prevxref);

                // make the trailer
                // [F2] full compression
                if (this.fullCompression) {
                    this.os.write(getISOBytes("startxref\n"));
                    this.os.write(getISOBytes(String.valueOf(this.body.offset())));
                    this.os.write(getISOBytes("\n%%EOF\n"));
                }
                else {
                    final PdfTrailer trailer = new PdfTrailer(this.body.size(),
                    this.body.offset(),
                    indirectCatalog.getIndirectReference(),
                    infoObj.getIndirectReference(),
                    encryption,
                    fileID, this.prevxref);
                    trailer.toPdf(this, this.os);
                }
                super.close();
            }
            catch(final IOException ioe) {
                throw new ExceptionConverter(ioe);
            }
        }
    }

    protected void addSharedObjectsToBody() throws IOException {
        // [F3] add the fonts
        for (final Iterator it = this.documentFonts.values().iterator(); it.hasNext();) {
            final FontDetails details = (FontDetails)it.next();
            details.writeFont(this);
        }
        // [F4] add the form XObjects
        for (final Iterator it = this.formXObjects.values().iterator(); it.hasNext();) {
            final Object objs[] = (Object[])it.next();
            final PdfTemplate template = (PdfTemplate)objs[1];
            if (template != null && template.getIndirectReference() instanceof PRIndirectReference) {
				continue;
			}
            if (template != null && template.getType() == PdfTemplate.TYPE_TEMPLATE) {
                addToBody(template.getFormXObject(this.compressionLevel), template.getIndirectReference());
            }
        }
        // [F5] add all the dependencies in the imported pages
        for (final Iterator it = this.importedPages.values().iterator(); it.hasNext();) {
            this.currentPdfReaderInstance = (PdfReaderInstance)it.next();
            this.currentPdfReaderInstance.writeAllPages();
        }
        this.currentPdfReaderInstance = null;
        // [F6] add the spotcolors
        for (final Iterator it = this.documentColors.values().iterator(); it.hasNext();) {
            final ColorDetails color = (ColorDetails)it.next();
            addToBody(color.getSpotColor(this), color.getIndirectReference());
        }
        // [F7] add the pattern
        for (final Iterator it = this.documentPatterns.keySet().iterator(); it.hasNext();) {
            final PdfPatternPainter pat = (PdfPatternPainter)it.next();
            addToBody(pat.getPattern(this.compressionLevel), pat.getIndirectReference());
        }
        // [F8] add the shading patterns
        for (final Iterator it = this.documentShadingPatterns.keySet().iterator(); it.hasNext();) {
            final PdfShadingPattern shadingPattern = (PdfShadingPattern)it.next();
            shadingPattern.addToBody();
        }
        // [F9] add the shadings
        for (final Iterator it = this.documentShadings.keySet().iterator(); it.hasNext();) {
            final PdfShading shading = (PdfShading)it.next();
            shading.addToBody();
        }
        // [F10] add the extgstate
        for (final Iterator it = this.documentExtGState.entrySet().iterator(); it.hasNext();) {
            final Map.Entry entry = (Map.Entry) it.next();
            final PdfDictionary gstate = (PdfDictionary) entry.getKey();
            final PdfObject obj[] = (PdfObject[]) entry.getValue();
            addToBody(gstate, (PdfIndirectReference)obj[1]);
        }
        // [F11] add the properties
        for (final Iterator it = this.documentProperties.entrySet().iterator(); it.hasNext();) {
            final Map.Entry entry = (Map.Entry) it.next();
            final Object prop = entry.getKey();
            final PdfObject[] obj = (PdfObject[]) entry.getValue();
            if (prop instanceof PdfLayerMembership){
                final PdfLayerMembership layer = (PdfLayerMembership)prop;
                addToBody(layer.getPdfObject(), layer.getRef());
            }
            else if (prop instanceof PdfDictionary && !(prop instanceof PdfLayer)){
                addToBody((PdfDictionary)prop, (PdfIndirectReference)obj[1]);
            }
        }
        // [F13] add the OCG layers
        for (final Iterator it = this.documentOCG.iterator(); it.hasNext();) {
            final PdfOCG layer = (PdfOCG)it.next();
            addToBody(layer.getPdfObject(), layer.getRef());
        }
    }

// Root data for the PDF document (used when composing the Catalog)

//  [C1] Outlines (bookmarks)

     /**
      * Use this method to get the root outline
      * and construct bookmarks.
      * @return the root outline
      */

     public PdfOutline getRootOutline() {
         return this.directContent.getRootOutline();
     }

     protected java.util.List newBookmarks;

    /**
     * Sets the bookmarks. The list structure is defined in
     * {@link SimpleBookmark}.
     * @param outlines the bookmarks or <CODE>null</CODE> to remove any
     */
    public void setOutlines(final java.util.List outlines) {
        this.newBookmarks = outlines;
    }

    protected void writeOutlines(final PdfDictionary catalog, final boolean namedAsNames) throws IOException {
        if (this.newBookmarks == null || this.newBookmarks.isEmpty()) {
			return;
		}
        final PdfDictionary top = new PdfDictionary();
        final PdfIndirectReference topRef = getPdfIndirectReference();
        final Object kids[] = SimpleBookmark.iterateOutlines(this, topRef, this.newBookmarks, namedAsNames);
        top.put(PdfName.FIRST, (PdfIndirectReference)kids[0]);
        top.put(PdfName.LAST, (PdfIndirectReference)kids[1]);
        top.put(PdfName.COUNT, new PdfNumber(((Integer)kids[2]).intValue()));
        addToBody(top, topRef);
        catalog.put(PdfName.OUTLINES, topRef);
    }

//	[C2] PdfVersion interface
     /** possible PDF version (header) */
     public static final char VERSION_1_2 = '2';
     /** possible PDF version (header) */
     public static final char VERSION_1_3 = '3';
     /** possible PDF version (header) */
     public static final char VERSION_1_4 = '4';
     /** possible PDF version (header) */
     public static final char VERSION_1_5 = '5';
     /** possible PDF version (header) */
     public static final char VERSION_1_6 = '6';
     /** possible PDF version (header) */
     public static final char VERSION_1_7 = '7';

     /** possible PDF version (catalog) */
     public static final PdfName PDF_VERSION_1_2 = new PdfName("1.2");
     /** possible PDF version (catalog) */
     public static final PdfName PDF_VERSION_1_3 = new PdfName("1.3");
     /** possible PDF version (catalog) */
     public static final PdfName PDF_VERSION_1_4 = new PdfName("1.4");
     /** possible PDF version (catalog) */
     public static final PdfName PDF_VERSION_1_5 = new PdfName("1.5");
     /** possible PDF version (catalog) */
     public static final PdfName PDF_VERSION_1_6 = new PdfName("1.6");
     /** possible PDF version (catalog) */
     public static final PdfName PDF_VERSION_1_7 = new PdfName("1.7");

    /** Stores the version information for the header and the catalog. */
    protected PdfVersionImp pdf_version = new PdfVersionImp();

    /** @see com.lowagie.text.pdf.interfaces.PdfVersion#setPdfVersion(char) */
    @Override
	public void setPdfVersion(final char version) {
        this.pdf_version.setPdfVersion(version);
    }

    /** @see com.lowagie.text.pdf.interfaces.PdfVersion#setAtLeastPdfVersion(char) */
    @Override
	public void setAtLeastPdfVersion(final char version) {
    	this.pdf_version.setAtLeastPdfVersion(version);
    }

	/** @see com.lowagie.text.pdf.interfaces.PdfVersion#setPdfVersion(com.lowagie.text.pdf.PdfName) */
	@Override
	public void setPdfVersion(final PdfName version) {
		this.pdf_version.setPdfVersion(version);
	}

	/**
	 * @see com.lowagie.text.pdf.interfaces.PdfVersion#addDeveloperExtension(com.lowagie.text.pdf.PdfDeveloperExtension)
	 * @since	2.1.6
	 */
	@Override
	public void addDeveloperExtension(final PdfDeveloperExtension de) {
		this.pdf_version.addDeveloperExtension(de);
	}

	/**
	 * Returns the version information.
	 */
	PdfVersionImp getPdfVersion() {
		return this.pdf_version;
	}

//  [C3] PdfViewerPreferences interface

	// page layout (section 13.1.1 of "iText in Action")

	/** A viewer preference */
	public static final int PageLayoutSinglePage = 1;
	/** A viewer preference */
	public static final int PageLayoutOneColumn = 2;
	/** A viewer preference */
	public static final int PageLayoutTwoColumnLeft = 4;
	/** A viewer preference */
	public static final int PageLayoutTwoColumnRight = 8;
	/** A viewer preference */
	public static final int PageLayoutTwoPageLeft = 16;
	/** A viewer preference */
	public static final int PageLayoutTwoPageRight = 32;

    // page mode (section 13.1.2 of "iText in Action")

    /** A viewer preference */
    public static final int PageModeUseNone = 64;
    /** A viewer preference */
    public static final int PageModeUseOutlines = 128;
    /** A viewer preference */
    public static final int PageModeUseThumbs = 256;
    /** A viewer preference */
    public static final int PageModeFullScreen = 512;
    /** A viewer preference */
    public static final int PageModeUseOC = 1024;
    /** A viewer preference */
    public static final int PageModeUseAttachments = 2048;

    // values for setting viewer preferences in iText versions older than 2.x

    /** A viewer preference */
    public static final int HideToolbar = 1 << 12;
    /** A viewer preference */
    public static final int HideMenubar = 1 << 13;
    /** A viewer preference */
    public static final int HideWindowUI = 1 << 14;
    /** A viewer preference */
    public static final int FitWindow = 1 << 15;
    /** A viewer preference */
    public static final int CenterWindow = 1 << 16;
    /** A viewer preference */
    public static final int DisplayDocTitle = 1 << 17;

    /** A viewer preference */
    public static final int NonFullScreenPageModeUseNone = 1 << 18;
    /** A viewer preference */
    public static final int NonFullScreenPageModeUseOutlines = 1 << 19;
    /** A viewer preference */
    public static final int NonFullScreenPageModeUseThumbs = 1 << 20;
    /** A viewer preference */
    public static final int NonFullScreenPageModeUseOC = 1 << 21;

    /** A viewer preference */
    public static final int DirectionL2R = 1 << 22;
    /** A viewer preference */
    public static final int DirectionR2L = 1 << 23;

    /** A viewer preference */
    public static final int PrintScalingNone = 1 << 24;

    /** @see com.lowagie.text.pdf.interfaces.PdfViewerPreferences#setViewerPreferences(int) */
    @Override
	public void setViewerPreferences(final int preferences) {
        this.pdf.setViewerPreferences(preferences);
    }

    /** @see com.lowagie.text.pdf.interfaces.PdfViewerPreferences#addViewerPreference(com.lowagie.text.pdf.PdfName, com.lowagie.text.pdf.PdfObject) */
    @Override
	public void addViewerPreference(final PdfName key, final PdfObject value) {
    	this.pdf.addViewerPreference(key, value);
    }

//  [C4] Page labels

    /**
     * Use this method to add page labels
     * @param pageLabels the page labels
     */
    public void setPageLabels(final PdfPageLabels pageLabels) {
        this.pdf.setPageLabels(pageLabels);
    }

//  [C5] named objects: named destinations, javascript, embedded files

     /**
      * Use this method to add a JavaScript action at the document level.
      * When the document opens, all this JavaScript runs.
      * @param js The JavaScript action
      */
     private void addJavaScript(final PdfAction js) {
         this.pdf.addJavaScript(js);
     }

     /**
      * Use this method to add a JavaScript action at the document level.
      * When the document opens, all this JavaScript runs.
      * @param code the JavaScript code
      * @param unicode select JavaScript unicode. Note that the internal
      * Acrobat JavaScript engine does not support unicode,
      * so this may or may not work for you
      */
     void addJavaScript(final String code, final boolean unicode) {
         addJavaScript(PdfAction.javaScript(code, this, unicode));
     }


     /**
      * Use this method to add a JavaScript action at the document level.
      * When the document opens, all this JavaScript runs.
      * @param name	The name of the JS Action in the name tree
      * @param js The JavaScript action
      */
     private void addJavaScript(final String name, final PdfAction js) {
         this.pdf.addJavaScript(name, js);
     }

     /**
      * Use this method to add a JavaScript action at the document level.
      * When the document opens, all this JavaScript runs.
      * @param name	The name of the JS Action in the name tree
      * @param code the JavaScript code
      * @param unicode select JavaScript unicode. Note that the internal
      * Acrobat JavaScript engine does not support unicode,
      * so this may or may not work for you
      */
     private void addJavaScript(final String name, final String code, final boolean unicode) {
         addJavaScript(name, PdfAction.javaScript(code, this, unicode));
     }



     /**
      * Use this method to add a file attachment at the document level.
      * @param description the file description
      * @param fileStore an array with the file. If it's <CODE>null</CODE>
      * the file will be read from the disk
      * @param file the path to the file. It will only be used if
      * <CODE>fileStore</CODE> is not <CODE>null</CODE>
      * @param fileDisplay the actual file name stored in the pdf
      * @throws IOException on error
      */
     public void addFileAttachment(final String description, final byte fileStore[], final String file, final String fileDisplay) throws IOException {
         addFileAttachment(description, PdfFileSpecification.fileEmbedded(this, file, fileDisplay, fileStore));
     }

     /**
      * Use this method to add a file attachment at the document level.
      * @param description the file description
      * @param fs the file specification
      */
     void addFileAttachment(final String description, final PdfFileSpecification fs) throws IOException {
         this.pdf.addFileAttachment(description, fs);
     }



// [C6] Actions (open and additional)

     /** action value */
     static final PdfName DOCUMENT_CLOSE = PdfName.WC;
     /** action value */
     static final PdfName WILL_SAVE = PdfName.WS;
     /** action value */
     static final PdfName DID_SAVE = PdfName.DS;
     /** action value */
     static final PdfName WILL_PRINT = PdfName.WP;
     /** action value */
     static final PdfName DID_PRINT = PdfName.DP;

    /** @see com.lowagie.text.pdf.interfaces.PdfDocumentActions#setOpenAction(java.lang.String) */
    @Override
	public void setOpenAction(final String name) {
         this.pdf.setOpenAction(name);
     }

    /** @see com.lowagie.text.pdf.interfaces.PdfDocumentActions#setOpenAction(com.lowagie.text.pdf.PdfAction) */
    @Override
	public void setOpenAction(final PdfAction action) {
         this.pdf.setOpenAction(action);
     }

    /** @see com.lowagie.text.pdf.interfaces.PdfDocumentActions#setAdditionalAction(com.lowagie.text.pdf.PdfName, com.lowagie.text.pdf.PdfAction) */
	public void setAdditionalAction(final PdfName actionType, final PdfAction action) throws DocumentException {
         if (!(actionType.equals(DOCUMENT_CLOSE) ||
         actionType.equals(WILL_SAVE) ||
         actionType.equals(DID_SAVE) ||
         actionType.equals(WILL_PRINT) ||
         actionType.equals(DID_PRINT))) {
             throw new DocumentException("Invalid additional action type: " + actionType.toString());
         }
         this.pdf.addAdditionalAction(actionType, action);
     }

//  [C7] portable collections

    /**
     * Use this method to add the Collection dictionary.
     * @param collection a dictionary of type PdfCollection
     */
    public void setCollection(final PdfCollection collection) {
        setAtLeastPdfVersion(VERSION_1_7);
        this.pdf.setCollection(collection);
    }

//  [C8] AcroForm




    /** @see com.lowagie.text.pdf.interfaces.PdfAnnotations#getAcroForm() */
    @Override
	public PdfAcroForm getAcroForm() {
        return this.pdf.getAcroForm();
    }

    /** @see com.lowagie.text.pdf.interfaces.PdfAnnotations#addAnnotation(com.lowagie.text.pdf.PdfAnnotation) */
    @Override
	public void addAnnotation(final PdfAnnotation annot) {
        this.pdf.addAnnotation(annot);
    }

    void addAnnotation(final PdfAnnotation annot, final int page) {
        addAnnotation(annot);
    }



    /** @see com.lowagie.text.pdf.interfaces.PdfAnnotations#setSigFlags(int) */
    @Override
	public void setSigFlags(final int f) {
        this.pdf.setSigFlags(f);
    }

//  [C9] Metadata

    /** XMP Metadata for the document. */
    protected byte[] xmpMetadata = null;

    /**
     * Use this method to set the XMP Metadata.
     * @param xmpMetadata The xmpMetadata to set.
     */
    public void setXmpMetadata(final byte[] xmpMetadata) {
        this.xmpMetadata = xmpMetadata;
    }

    /**
     * Use this method to set the XMP Metadata for each page.
     * @param xmpMetadata The xmpMetadata to set.
     */
    public void setPageXmpMetadata(final byte[] xmpMetadata) {
        this.pdf.setXmpMetadata(xmpMetadata);
    }



    /**
     * @return an XmpMetadata byte array
     */
    private byte[] createXmpMetadataBytes() {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            final XmpWriter xmp = new XmpWriter(baos, this.pdf.getInfo(), this.pdfxConformance.getPDFXConformance());
            xmp.close();
        }
        catch (final IOException ioe) {
            ioe.printStackTrace();
        }
        return baos.toByteArray();
    }

//  [C10] PDFX Conformance
    /** A PDF/X level. */
    public static final int PDFXNONE = 0;
    /** A PDF/X level. */
    public static final int PDFX1A2001 = 1;
    /** A PDF/X level. */
    public static final int PDFX32002 = 2;
    /** PDFA-1A level. */
    public static final int PDFA1A = 3;
    /** PDFA-1B level. */
    public static final int PDFA1B = 4;

    /** Stores the PDF/X level. */
    private final PdfXConformanceImp pdfxConformance = new PdfXConformanceImp();

    /** @see com.lowagie.text.pdf.interfaces.PdfXConformance#setPDFXConformance(int) */
    @Override
	public void setPDFXConformance(final int pdfx) {
        if (this.pdfxConformance.getPDFXConformance() == pdfx) {
			return;
		}
        if (this.pdf.isOpen()) {
			throw new PdfXConformanceException("PDFX conformance can only be set before opening the document.");
		}
        if (this.crypto != null) {
			throw new PdfXConformanceException("A PDFX conforming document cannot be encrypted.");
		}
        if (pdfx == PDFA1A || pdfx == PDFA1B) {
			setPdfVersion(VERSION_1_4);
		} else if (pdfx != PDFXNONE) {
			setPdfVersion(VERSION_1_3);
		}
        this.pdfxConformance.setPDFXConformance(pdfx);
    }

    /** @see com.lowagie.text.pdf.interfaces.PdfXConformance#getPDFXConformance() */
    @Override
	public int getPDFXConformance() {
        return this.pdfxConformance.getPDFXConformance();
    }

    /** @see com.lowagie.text.pdf.interfaces.PdfXConformance#isPdfX() */
    @Override
	public boolean isPdfX() {
        return this.pdfxConformance.isPdfX();
    }

//  [C11] Output intents
    /**
     * Sets the values of the output intent dictionary. Null values are allowed to
     * suppress any key.
     *
     * @param outputConditionIdentifier a value
     * @param outputCondition           a value, "PDFA/A" to force GTS_PDFA1, otherwise cued by pdfxConformance.
     * @param registryName              a value
     * @param info                      a value
     * @param colorProfile              a value
     * @since 2.1.5
     * @throws IOException on error
     */
    private void setOutputIntents(final String outputConditionIdentifier, final String outputCondition, final String registryName, final String info, final ICC_Profile colorProfile) throws IOException {
        getExtraCatalog();
        final PdfDictionary out = new PdfDictionary(PdfName.OUTPUTINTENT);
        if (outputCondition != null) {
			out.put(PdfName.OUTPUTCONDITION, new PdfString(outputCondition, PdfObject.TEXT_UNICODE));
		}
        if (outputConditionIdentifier != null) {
			out.put(PdfName.OUTPUTCONDITIONIDENTIFIER, new PdfString(outputConditionIdentifier, PdfObject.TEXT_UNICODE));
		}
        if (registryName != null) {
			out.put(PdfName.REGISTRYNAME, new PdfString(registryName, PdfObject.TEXT_UNICODE));
		}
        if (info != null) {
			out.put(PdfName.INFO, new PdfString(info, PdfObject.TEXT_UNICODE));
		}
        if (colorProfile != null) {
            final PdfStream stream = new PdfICCBased(colorProfile, this.compressionLevel);
            out.put(PdfName.DESTOUTPUTPROFILE, addToBody(stream).getIndirectReference());
        }

        PdfName intentSubtype;
        if (this.pdfxConformance.isPdfA1() || "PDFA/1".equals(outputCondition)) {
            intentSubtype = PdfName.GTS_PDFA1;
        }
        else {
            intentSubtype = PdfName.GTS_PDFX;
        }

        out.put(PdfName.S, intentSubtype);

        this.extraCatalog.put(PdfName.OUTPUTINTENTS, new PdfArray(out));
    }

   /**
     * Sets the values of the output intent dictionary. Null values are allowed to
     * suppress any key.
     *
     * Prefer the <CODE>ICC_Profile</CODE>-based version of this method.
     * @param outputConditionIdentifier a value
     * @param outputCondition           a value, "PDFA/A" to force GTS_PDFA1, otherwise cued by pdfxConformance.
     * @param registryName              a value
     * @param info                      a value
     * @param destOutputProfile         a value
     * @since 1.x
     *
     * @throws IOException
     */
    private void setOutputIntents(final String outputConditionIdentifier, final String outputCondition, final String registryName, final String info, final byte destOutputProfile[]) throws IOException {
        final ICC_Profile colorProfile = destOutputProfile == null ? null : ICC_Profile.getInstance(destOutputProfile);
        setOutputIntents(outputConditionIdentifier, outputCondition, registryName, info, colorProfile);
    }




    private static String getNameString(final PdfDictionary dic, final PdfName key) {
        final PdfObject obj = PdfReader.getPdfObject(dic.get(key));
        if (obj == null || !obj.isString()) {
			return null;
		}
        return ((PdfString)obj).toUnicodeString();
    }

// PDF Objects that have an impact on the PDF body

//  [F1] PdfEncryptionSettings interface

    // types of encryption

    /** Type of encryption */
    static final int STANDARD_ENCRYPTION_40 = 0;
    /** Type of encryption */
    static final int STANDARD_ENCRYPTION_128 = 1;
    /** Type of encryption */
    static final int ENCRYPTION_AES_128 = 2;
    /** Mask to separate the encryption type from the encryption mode. */
    static final int ENCRYPTION_MASK = 7;
    /** Add this to the mode to keep the metadata in clear text */
    static final int DO_NOT_ENCRYPT_METADATA = 8;
    /**
     * Add this to the mode to keep encrypt only the embedded files.
     * @since 2.1.3
     */
    static final int EMBEDDED_FILES_ONLY = 24;

    // permissions

    /** The operation permitted when the document is opened with the user password
     *
     * @since 2.0.7
     */
    static final int ALLOW_PRINTING = 4 + 2048;

    /** The operation permitted when the document is opened with the user password
     *
     * @since 2.0.7
     */
    static final int ALLOW_MODIFY_CONTENTS = 8;

    /** The operation permitted when the document is opened with the user password
     *
     * @since 2.0.7
     */
    static final int ALLOW_COPY = 16;

    /** The operation permitted when the document is opened with the user password
     *
     * @since 2.0.7
     */
    static final int ALLOW_MODIFY_ANNOTATIONS = 32;

    /** The operation permitted when the document is opened with the user password
     *
     * @since 2.0.7
     */
    static final int ALLOW_FILL_IN = 256;

    /** The operation permitted when the document is opened with the user password
     *
     * @since 2.0.7
     */
    static final int ALLOW_SCREENREADERS = 512;

    /** The operation permitted when the document is opened with the user password
     *
     * @since 2.0.7
     */
    static final int ALLOW_ASSEMBLY = 1024;

    /** The operation permitted when the document is opened with the user password
     *
     * @since 2.0.7
     */
    static final int ALLOW_DEGRADED_PRINTING = 4;



    /** Contains the business logic for cryptography. */
    protected PdfEncryption crypto;
    PdfEncryption getEncryption() {
        return this.crypto;
    }

    /** @see com.lowagie.text.pdf.interfaces.PdfEncryptionSettings#setEncryption(byte[], byte[], int, int) */
    @Override
	public void setEncryption(final byte userPassword[], final byte ownerPassword[], final int permissions, final int encryptionType) throws DocumentException {
        if (this.pdf.isOpen()) {
			throw new DocumentException("Encryption can only be added before opening the document.");
		}
        this.crypto = new PdfEncryption();
        this.crypto.setCryptoMode(encryptionType, 0);
        this.crypto.setupAllKeys(userPassword, ownerPassword, permissions);
    }

    /** @see com.lowagie.text.pdf.interfaces.PdfEncryptionSettings#setEncryption(java.security.cert.Certificate[], int[], int) */
    @Override
	public void setEncryption(final Certificate[] certs, final int[] permissions, final int encryptionType) throws DocumentException {
        if (this.pdf.isOpen()) {
			throw new DocumentException("Encryption can only be added before opening the document.");
		}
        this.crypto = new PdfEncryption();
        if (certs != null) {
            for (int i=0; i < certs.length; i++) {
                this.crypto.addRecipient(certs[i], permissions[i]);
            }
        }
        this.crypto.setCryptoMode(encryptionType, 0);
        this.crypto.getEncryptionDictionary();
    }







//  [F2] compression

    /** Holds value of property fullCompression. */
    protected boolean fullCompression = false;

    /**
     * Use this method to find out if 1.5 compression is on.
     * @return the 1.5 compression status
     */
    public boolean isFullCompression() {
        return this.fullCompression;
    }

    /**
     * Use this method to set the document's compression to the
     * PDF 1.5 mode with object streams and xref streams.
     * It can be set at any time but once set it can't be unset.
     * <p>
     * If set before opening the document it will also set the pdf version to 1.5.
     */
    void setFullCompression() {
        this.fullCompression = true;
        setAtLeastPdfVersion(VERSION_1_5);
    }

    /**
     * The compression level of the content streams.
     * @since 2.1.3
     */
    protected int compressionLevel = PdfStream.DEFAULT_COMPRESSION;

    /**
     * Returns the compression level used for streams written by this writer.
     * @return the compression level (0 = best speed, 9 = best compression, -1 is default)
     * @since 2.1.3
     */
    public int getCompressionLevel() {
        return this.compressionLevel;
    }

    /**
     * Sets the compression level to be used for streams written by this writer.
     * @param compressionLevel a value between 0 (best speed) and 9 (best compression)
     * @since 2.1.3
     */
    public void setCompressionLevel(final int compressionLevel) {
        if (compressionLevel < PdfStream.NO_COMPRESSION || compressionLevel > PdfStream.BEST_COMPRESSION) {
			this.compressionLevel = PdfStream.DEFAULT_COMPRESSION;
		} else {
			this.compressionLevel = compressionLevel;
		}
    }

//  [F3] adding fonts

    /** The fonts of this document */
    private final LinkedHashMap documentFonts = new LinkedHashMap();

    /** The font number counter for the fonts in the document. */
    private int fontNumber = 1;

    /**
     * Adds a <CODE>BaseFont</CODE> to the document but not to the page resources.
     * It is used for templates.
     * @param bf the <CODE>BaseFont</CODE> to add
     * @return an <CODE>Object[]</CODE> where position 0 is a <CODE>PdfName</CODE>
     * and position 1 is an <CODE>PdfIndirectReference</CODE>
     */

    FontDetails addSimple(final BaseFont bf) {
        if (bf.getFontType() == BaseFont.FONT_TYPE_DOCUMENT) {
            return new FontDetails(new PdfName("F" + this.fontNumber++), ((DocumentFont)bf).getIndirectReference(), bf);
        }
        FontDetails ret = (FontDetails)this.documentFonts.get(bf);
        if (ret == null) {
            PdfXConformanceImp.checkPDFXConformance(this, PdfXConformanceImp.PDFXKEY_FONT, bf);
            ret = new FontDetails(new PdfName("F" + this.fontNumber++), this.body.getPdfIndirectReference(), bf);
            this.documentFonts.put(bf, ret);
        }
        return ret;
    }

    void eliminateFontSubset(final PdfDictionary fonts) {
        for (final Iterator it = this.documentFonts.values().iterator(); it.hasNext();) {
            final FontDetails ft = (FontDetails)it.next();
            if (fonts.get(ft.getFontName()) != null) {
				ft.setSubset(false);
			}
        }
    }

//  [F4] adding (and releasing) form XObjects

    /** The form XObjects in this document. The key is the xref and the value
        is Object[]{PdfName, template}.*/
    private final HashMap formXObjects = new HashMap();

    /** The name counter for the form XObjects name. */
    private int formXObjectsCounter = 1;

    /**
     * Adds a template to the document but not to the page resources.
     * @param template the template to add
     * @param forcedName the template name, rather than a generated one. Can be null
     * @return the <CODE>PdfName</CODE> for this template
     */

    PdfName addDirectTemplateSimple(PdfTemplate template, final PdfName forcedName) {
        final PdfIndirectReference ref = template.getIndirectReference();
        final Object obj[] = (Object[])this.formXObjects.get(ref);
        PdfName name = null;
        try {
            if (obj == null) {
                if (forcedName == null) {
                    name = new PdfName("Xf" + this.formXObjectsCounter);
                    ++this.formXObjectsCounter;
                } else {
					name = forcedName;
				}
                if (template.getType() == PdfTemplate.TYPE_IMPORTED) {
                    // If we got here from PdfCopy we'll have to fill importedPages
                    final PdfImportedPage ip = (PdfImportedPage)template;
                    final PdfReader r = ip.getPdfReaderInstance().getReader();
                    if (!this.importedPages.containsKey(r)) {
                        this.importedPages.put(r, ip.getPdfReaderInstance());
                    }
                    template = null;
                }
                this.formXObjects.put(ref, new Object[]{name, template});
            } else {
				name = (PdfName)obj[0];
			}
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
        return name;
    }

    /**
     * Use this method to releases the memory used by a template.
     * This method writes the template to the output.
     * The template can still be added to any content
     * but changes to the template itself won't have any effect.
     * @param tp the template to release
     * @throws IOException on error
     */
    void releaseTemplate(final PdfTemplate tp) throws IOException {
        final PdfIndirectReference ref = tp.getIndirectReference();
        final Object[] objs = (Object[])this.formXObjects.get(ref);
        if (objs == null || objs[1] == null) {
			return;
		}
        final PdfTemplate template = (PdfTemplate)objs[1];
        if (template.getIndirectReference() instanceof PRIndirectReference) {
			return;
		}
        if (template.getType() == PdfTemplate.TYPE_TEMPLATE) {
            addToBody(template.getFormXObject(this.compressionLevel), template.getIndirectReference());
            objs[1] = null;
        }
    }

//  [F5] adding pages imported form other PDF documents

    private final HashMap importedPages = new HashMap();

    /**
     * Use this method to get a page from other PDF document.
     * The page can be used as any other PdfTemplate.
     * Note that calling this method more than once with the same parameters
     * will retrieve the same object.
     * @param reader the PDF document where the page is
     * @param pageNumber the page number. The first page is 1
     * @return the template representing the imported page
     */
    public PdfImportedPage getImportedPage(final PdfReader reader, final int pageNumber) {
        PdfReaderInstance inst = (PdfReaderInstance)this.importedPages.get(reader);
        if (inst == null) {
            inst = reader.getPdfReaderInstance(this);
            this.importedPages.put(reader, inst);
        }
        return inst.getImportedPage(pageNumber);
    }

    /**
     * Use this method to writes the reader to the document
     * and free the memory used by it.
     * The main use is when concatenating multiple documents
     * to keep the memory usage restricted to the current
     * appending document.
     * @param reader the <CODE>PdfReader</CODE> to free
     * @throws IOException on error
     */
    public void freeReader(final PdfReader reader) throws IOException {
        this.currentPdfReaderInstance = (PdfReaderInstance)this.importedPages.get(reader);
        if (this.currentPdfReaderInstance == null) {
			return;
		}
        this.currentPdfReaderInstance.writeAllPages();
        this.currentPdfReaderInstance = null;
        this.importedPages.remove(reader);
    }

    /**
     * Use this method to gets the current document size.
     * This size only includes the data already written
     * to the output stream, it does not include templates or fonts.
     * It is useful if used with <CODE>freeReader()</CODE>
     * when concatenating many documents and an idea of
     * the current size is needed.
     * @return the approximate size without fonts or templates
     */
    public int getCurrentDocumentSize() {
        return this.body.offset() + this.body.size() * 20 + 0x48;
    }

    protected PdfReaderInstance currentPdfReaderInstance;

    protected int getNewObjectNumber(final PdfReader reader, final int number, final int generation) {
        return this.currentPdfReaderInstance.getNewObjectNumber(number, generation);
    }

    RandomAccessFileOrArray getReaderFile(final PdfReader reader) {
        return this.currentPdfReaderInstance.getReaderFile();
    }

//  [F6] spot colors

    /** The colors of this document */
    private final HashMap documentColors = new HashMap();

    /** The color number counter for the colors in the document. */
    private int colorNumber = 1;

    PdfName getColorspaceName() {
        return new PdfName("CS" + this.colorNumber++);
    }

    /**
     * Adds a <CODE>SpotColor</CODE> to the document but not to the page resources.
     * @param spc the <CODE>SpotColor</CODE> to add
     * @return an <CODE>Object[]</CODE> where position 0 is a <CODE>PdfName</CODE>
     * and position 1 is an <CODE>PdfIndirectReference</CODE>
     */
    ColorDetails addSimple(final PdfSpotColor spc) {
        ColorDetails ret = (ColorDetails)this.documentColors.get(spc);
        if (ret == null) {
            ret = new ColorDetails(getColorspaceName(), this.body.getPdfIndirectReference(), spc);
            this.documentColors.put(spc, ret);
        }
        return ret;
    }

//  [F7] document patterns

    /** The patterns of this document */
    private final HashMap documentPatterns = new HashMap();

    /** The pattern number counter for the colors in the document. */
    private int patternNumber = 1;

    PdfName addSimplePattern(final PdfPatternPainter painter) {
        PdfName name = (PdfName)this.documentPatterns.get(painter);
        try {
            if ( name == null ) {
                name = new PdfName("P" + this.patternNumber);
                ++this.patternNumber;
                this.documentPatterns.put(painter, name);
            }
        } catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
        return name;
    }

//  [F8] shading patterns

    private final HashMap documentShadingPatterns = new HashMap();

    void addSimpleShadingPattern(final PdfShadingPattern shading) {
        if (!this.documentShadingPatterns.containsKey(shading)) {
            shading.setName(this.patternNumber);
            ++this.patternNumber;
            this.documentShadingPatterns.put(shading, null);
            addSimpleShading(shading.getShading());
        }
    }

//  [F9] document shadings

    private final HashMap documentShadings = new HashMap();

    void addSimpleShading(final PdfShading shading) {
        if (!this.documentShadings.containsKey(shading)) {
            this.documentShadings.put(shading, null);
            shading.setName(this.documentShadings.size());
        }
    }

// [F10] extended graphics state (for instance for transparency)

    private final HashMap documentExtGState = new HashMap();

    PdfObject[] addSimpleExtGState(final PdfDictionary gstate) {
        if (!this.documentExtGState.containsKey(gstate)) {
            PdfXConformanceImp.checkPDFXConformance(this, PdfXConformanceImp.PDFXKEY_GSTATE, gstate);
            this.documentExtGState.put(gstate, new PdfObject[]{new PdfName("GS" + (this.documentExtGState.size() + 1)), getPdfIndirectReference()});
        }
        return (PdfObject[])this.documentExtGState.get(gstate);
    }

//  [F11] adding properties (OCG, marked content)

    private final HashMap documentProperties = new HashMap();
    PdfObject[] addSimpleProperty(final Object prop, final PdfIndirectReference refi) {
        if (!this.documentProperties.containsKey(prop)) {
            if (prop instanceof PdfOCG) {
				PdfXConformanceImp.checkPDFXConformance(this, PdfXConformanceImp.PDFXKEY_LAYER, null);
			}
            this.documentProperties.put(prop, new PdfObject[]{new PdfName("Pr" + (this.documentProperties.size() + 1)), refi});
        }
        return (PdfObject[])this.documentProperties.get(prop);
    }

    boolean propertyExists(final Object prop) {
        return this.documentProperties.containsKey(prop);
    }

//  [F12] tagged PDF

    private final boolean tagged = false;
    private PdfStructureTreeRoot structureTreeRoot;



    /**
     * Check if the document is marked for tagging.
     * @return <CODE>true</CODE> if the document is marked for tagging
     */
    public boolean isTagged() {
        return this.tagged;
    }

    /**
     * Gets the structure tree root. If the document is not marked for tagging it will return <CODE>null</CODE>.
     * @return the structure tree root
     */
    public PdfStructureTreeRoot getStructureTreeRoot() {
        if (this.tagged && this.structureTreeRoot == null) {
			this.structureTreeRoot = new PdfStructureTreeRoot(this);
		}
        return this.structureTreeRoot;
    }

//  [F13] Optional Content Groups
    /** A hashSet containing all the PdfLayer objects. */
    protected HashSet documentOCG = new HashSet();
    /** An array list used to define the order of an OCG tree. */
    private final ArrayList documentOCGorder = new ArrayList();
    /** The OCProperties in a catalog dictionary. */
    protected PdfOCProperties OCProperties;
    /** The RBGroups array in an OCG dictionary */
    protected PdfArray OCGRadioGroup = new PdfArray();
    /**
     * The locked array in an OCG dictionary
     * @since   2.1.2
     */
    protected PdfArray OCGLocked = new PdfArray();

    /**
     * Use this method to get the <B>Optional Content Properties Dictionary</B>.
     * Each call fills the dictionary with the current layer state.
     * It's advisable to only call this method right before close
     * and do any modifications at that time.
     * @return the Optional Content Properties Dictionary
     */
    public PdfOCProperties getOCProperties() {
        fillOCProperties(true);
        return this.OCProperties;
    }





    private static void getOCGOrder(final PdfArray order, final PdfLayer layer) {
        if (!layer.isOnPanel()) {
			return;
		}
        if (layer.getTitle() == null) {
			order.add(layer.getRef());
		}
        final ArrayList children = layer.getChildren();
        if (children == null) {
			return;
		}
        final PdfArray kids = new PdfArray();
        if (layer.getTitle() != null) {
			kids.add(new PdfString(layer.getTitle(), PdfObject.TEXT_UNICODE));
		}
        for (int k = 0; k < children.size(); ++k) {
            getOCGOrder(kids, (PdfLayer)children.get(k));
        }
        if (kids.size() > 0) {
			order.add(kids);
		}
    }

    private void addASEvent(final PdfName event, final PdfName category) {
        final PdfArray arr = new PdfArray();
        for (final Iterator it = this.documentOCG.iterator(); it.hasNext();) {
            final PdfLayer layer = (PdfLayer)it.next();
            final PdfDictionary usage = (PdfDictionary)layer.get(PdfName.USAGE);
            if (usage != null && usage.get(category) != null) {
				arr.add(layer.getRef());
			}
        }
        if (arr.size() == 0) {
			return;
		}
        final PdfDictionary d = (PdfDictionary)this.OCProperties.get(PdfName.D);
        PdfArray arras = (PdfArray)d.get(PdfName.AS);
        if (arras == null) {
            arras = new PdfArray();
            d.put(PdfName.AS, arras);
        }
        final PdfDictionary as = new PdfDictionary();
        as.put(PdfName.EVENT, event);
        as.put(PdfName.CATEGORY, new PdfArray(category));
        as.put(PdfName.OCGS, arr);
        arras.add(as);
    }

    /**
     * @since 2.1.2
     */
    protected void fillOCProperties(final boolean erase) {
        if (this.OCProperties == null) {
			this.OCProperties = new PdfOCProperties();
		}
        if (erase) {
            this.OCProperties.remove(PdfName.OCGS);
            this.OCProperties.remove(PdfName.D);
        }
        if (this.OCProperties.get(PdfName.OCGS) == null) {
            final PdfArray gr = new PdfArray();
            for (final Iterator it = this.documentOCG.iterator(); it.hasNext();) {
                final PdfLayer layer = (PdfLayer)it.next();
                gr.add(layer.getRef());
            }
            this.OCProperties.put(PdfName.OCGS, gr);
        }
        if (this.OCProperties.get(PdfName.D) != null) {
			return;
		}
        final ArrayList docOrder = new ArrayList(this.documentOCGorder);
        for (final Iterator it = docOrder.iterator(); it.hasNext();) {
            final PdfLayer layer = (PdfLayer)it.next();
            if (layer.getParent() != null) {
				it.remove();
			}
        }
        final PdfArray order = new PdfArray();
        for (final Iterator it = docOrder.iterator(); it.hasNext();) {
            final PdfLayer layer = (PdfLayer)it.next();
            getOCGOrder(order, layer);
        }
        final PdfDictionary d = new PdfDictionary();
        this.OCProperties.put(PdfName.D, d);
        d.put(PdfName.ORDER, order);
        final PdfArray gr = new PdfArray();
        for (final Iterator it = this.documentOCG.iterator(); it.hasNext();) {
            final PdfLayer layer = (PdfLayer)it.next();
            if (!layer.isOn()) {
				gr.add(layer.getRef());
			}
        }
        if (gr.size() > 0) {
			d.put(PdfName.OFF, gr);
		}
        if (this.OCGRadioGroup.size() > 0) {
			d.put(PdfName.RBGROUPS, this.OCGRadioGroup);
		}
        if (this.OCGLocked.size() > 0) {
			d.put(PdfName.LOCKED, this.OCGLocked);
		}
        addASEvent(PdfName.VIEW, PdfName.ZOOM);
        addASEvent(PdfName.VIEW, PdfName.VIEW);
        addASEvent(PdfName.PRINT, PdfName.PRINT);
        addASEvent(PdfName.EXPORT, PdfName.EXPORT);
        d.put(PdfName.LISTMODE, PdfName.VISIBLEPAGES);
    }

    void registerLayer(final PdfOCG layer) {
        PdfXConformanceImp.checkPDFXConformance(this, PdfXConformanceImp.PDFXKEY_LAYER, null);
        if (layer instanceof PdfLayer) {
            final PdfLayer la = (PdfLayer)layer;
            if (la.getTitle() == null) {
                if (!this.documentOCG.contains(layer)) {
                    this.documentOCG.add(layer);
                    this.documentOCGorder.add(layer);
                }
            }
            else {
                this.documentOCGorder.add(layer);
            }
        } else {
			throw new IllegalArgumentException("Only PdfLayer is accepted.");
		}
    }

//  User methods to change aspects of the page

//  [U1] page size

    /**
     * Use this method to get the size of the media box.
     * @return a Rectangle
     */
    public Rectangle getPageSize() {
        return this.pdf.getPageSize();
    }

    /**
     * Use this method to set the crop box.
     * The crop box should not be rotated even if the page is rotated.
     * This change only takes effect in the next page.
     * @param crop the crop box
     */
    public void setCropBoxSize(final Rectangle crop) {
        this.pdf.setCropBoxSize(crop);
    }





//  [U2] take care of empty pages

    /**
     * Use this method to make sure a page is added,
     * even if it's empty. If you use setPageEmpty(false),
     * invoking newPage() after a blank page will add a newPage.
     * @param pageEmpty the state
     */
    public void setPageEmpty(final boolean pageEmpty) {
        this.pdf.setPageEmpty(pageEmpty);
    }

//  [U3] page actions (open and close)

    /** action value */
    private static final PdfName PAGE_OPEN = PdfName.O;
    /** action value */
    private static final PdfName PAGE_CLOSE = PdfName.C;

    /** @see com.lowagie.text.pdf.interfaces.PdfPageActions#setPageAction(com.lowagie.text.pdf.PdfName, com.lowagie.text.pdf.PdfAction) */
	public void setPageAction(final PdfName actionType, final PdfAction action) throws DocumentException {
          if (!actionType.equals(PAGE_OPEN) && !actionType.equals(PAGE_CLOSE)) {
			throw new DocumentException("Invalid page additional action type: " + actionType.toString());
		}
          this.pdf.setPageAction(actionType, action);
      }

    /** @see com.lowagie.text.pdf.interfaces.PdfPageActions#setDuration(int) */
    @Override
	public void setDuration(final int seconds) {
         this.pdf.setDuration(seconds);
     }

    /** @see com.lowagie.text.pdf.interfaces.PdfPageActions#setTransition(com.lowagie.text.pdf.PdfTransition) */
    @Override
	public void setTransition(final PdfTransition transition) {
         this.pdf.setTransition(transition);
     }

//  [U4] Thumbnail image

    /**
     * Use this method to set the thumbnail image for the current page.
     * @param image the image
     * @throws PdfException on error
     * @throws DocumentException or error
     */
    public void setThumbnail(final Image image) throws PdfException, DocumentException {
        this.pdf.setThumbnail(image);
    }

//  [U5] Transparency groups

    /**
     * A group attributes dictionary specifying the attributes
     * of the page's page group for use in the transparent
     * imaging model
     */
    private PdfDictionary group;

    /**
     * Use this method to get the group dictionary.
     * @return Value of property group.
     */
    public PdfDictionary getGroup() {
        return this.group;
    }

    /**
     * Use this method to set the group dictionary.
     * @param group New value of property group.
     */
    public void setGroup(final PdfDictionary group) {
        this.group = group;
    }

//  [U6] space char ratio

    /** The default space-char ratio. */
    private static final float SPACE_CHAR_RATIO_DEFAULT = 2.5f;


    /**
     * The ratio between the extra word spacing and the extra character spacing.
     * Extra word spacing will grow <CODE>ratio</CODE> times more than extra character spacing.
     */
    private float spaceCharRatio = SPACE_CHAR_RATIO_DEFAULT;

    /**
     * Use this method to gets the space/character extra spacing ratio
     * for fully justified text.
     * @return the space/character extra spacing ratio
     */
    public float getSpaceCharRatio() {
        return this.spaceCharRatio;
    }

    /**
     * Use this method to set the ratio between the extra word spacing and
     * the extra character spacing when the text is fully justified.
     * Extra word spacing will grow <CODE>spaceCharRatio</CODE> times more
     * than extra character spacing. If the ratio is <CODE>PdfWriter.NO_SPACE_CHAR_RATIO</CODE>
     * then the extra character spacing will be zero.
     * @param spaceCharRatio the ratio between the extra word spacing and the extra character spacing
     */
    public void setSpaceCharRatio(final float spaceCharRatio) {
        if (spaceCharRatio < 0.001f) {
			this.spaceCharRatio = 0.001f;
		} else {
			this.spaceCharRatio = spaceCharRatio;
		}
    }

//  [U7] run direction (doesn't actually do anything)

    /** Use the default run direction. */
    static final int RUN_DIRECTION_DEFAULT = 0;
    /** Do not use bidirectional reordering. */
    static final int RUN_DIRECTION_NO_BIDI = 1;
    /** Use bidirectional reordering with left-to-right
     * preferential run direction.
     */
    static final int RUN_DIRECTION_LTR = 2;
    /** Use bidirectional reordering with right-to-left
     * preferential run direction.
     */
    static final int RUN_DIRECTION_RTL = 3;

    private int runDirection = RUN_DIRECTION_NO_BIDI;

    /**
     * Use this method to set the run direction.
     * This is only used as a placeholder as it does not affect anything.
     * @param runDirection the run direction
     */
    @Override
	public void setRunDirection(final int runDirection) {
        if (runDirection < RUN_DIRECTION_NO_BIDI || runDirection > RUN_DIRECTION_RTL) {
			throw new RuntimeException("Invalid run direction: " + runDirection);
		}
        this.runDirection = runDirection;
    }

    /**
     * Use this method to set the run direction.
     * @return the run direction
     */
    @Override
	public int getRunDirection() {
        return this.runDirection;
    }

//  [U8] user units

     private float userunit = 0f;
    /**
     * Use this method to get the user unit.
     * A user unit is a value that defines the default user space unit.
     * The minimum UserUnit is 1 (1 unit = 1/72 inch).
     * The maximum UserUnit is 75,000.
     * Note that this userunit only works starting with PDF1.6!
     * @return Returns the userunit.
     */
    public float getUserunit() {
        return this.userunit;
    }
    /**
     * Use this method to set the user unit.
     * A UserUnit is a value that defines the default user space unit.
     * The minimum UserUnit is 1 (1 unit = 1/72 inch).
     * The maximum UserUnit is 75,000.
     * Note that this userunit only works starting with PDF1.6!
     * @param userunit The userunit to set.
     * @throws DocumentException on error
     */
     public void setUserunit(final float userunit) throws DocumentException {
 		if (userunit < 1f || userunit > 75000f) {
			throw new DocumentException("UserUnit should be a value between 1 and 75000.");
		}
         this.userunit = userunit;
         setAtLeastPdfVersion(VERSION_1_6);
     }

// Miscellaneous topics

//  [M1] Color settings

    private final PdfDictionary defaultColorspace = new PdfDictionary();
    /**
     * Use this method to get the default colorspaces.
     * @return the default colorspaces
     */
    public PdfDictionary getDefaultColorspace() {
        return this.defaultColorspace;
    }

    /**
     * Use this method to sets the default colorspace that will be applied
     * to all the document. The colorspace is only applied if another colorspace
     * with the same name is not present in the content.
     * <p>
     * The colorspace is applied immediately when creating templates and
     * at the page end for the main document content.
     * @param key the name of the colorspace. It can be <CODE>PdfName.DEFAULTGRAY</CODE>, <CODE>PdfName.DEFAULTRGB</CODE>
     * or <CODE>PdfName.DEFAULTCMYK</CODE>
     * @param cs the colorspace. A <CODE>null</CODE> or <CODE>PdfNull</CODE> removes any colorspace with the same name
     */
    private void setDefaultColorspace(final PdfName key, final PdfObject cs) {
        if (cs == null || cs.isNull()) {
			this.defaultColorspace.remove(key);
		}
        this.defaultColorspace.put(key, cs);
    }

//  [M2] spot patterns

    private final HashMap documentSpotPatterns = new HashMap();
    private ColorDetails patternColorspaceRGB;
    private ColorDetails patternColorspaceGRAY;
    private ColorDetails patternColorspaceCMYK;

    ColorDetails addSimplePatternColorspace(final Color color) {
        final int type = ExtendedColor.getType(color);
        if (type == ExtendedColor.TYPE_PATTERN || type == ExtendedColor.TYPE_SHADING) {
			throw new RuntimeException("An uncolored tile pattern can not have another pattern or shading as color.");
		}
        try {
            switch (type) {
                case ExtendedColor.TYPE_RGB:
                    if (this.patternColorspaceRGB == null) {
                        this.patternColorspaceRGB = new ColorDetails(getColorspaceName(), this.body.getPdfIndirectReference(), null);
                        final PdfArray array = new PdfArray(PdfName.PATTERN);
                        array.add(PdfName.DEVICERGB);
                        addToBody(array, this.patternColorspaceRGB.getIndirectReference());
                    }
                    return this.patternColorspaceRGB;
                case ExtendedColor.TYPE_CMYK:
                    if (this.patternColorspaceCMYK == null) {
                        this.patternColorspaceCMYK = new ColorDetails(getColorspaceName(), this.body.getPdfIndirectReference(), null);
                        final PdfArray array = new PdfArray(PdfName.PATTERN);
                        array.add(PdfName.DEVICECMYK);
                        addToBody(array, this.patternColorspaceCMYK.getIndirectReference());
                    }
                    return this.patternColorspaceCMYK;
                case ExtendedColor.TYPE_GRAY:
                    if (this.patternColorspaceGRAY == null) {
                        this.patternColorspaceGRAY = new ColorDetails(getColorspaceName(), this.body.getPdfIndirectReference(), null);
                        final PdfArray array = new PdfArray(PdfName.PATTERN);
                        array.add(PdfName.DEVICEGRAY);
                        addToBody(array, this.patternColorspaceGRAY.getIndirectReference());
                    }
                    return this.patternColorspaceGRAY;
                case ExtendedColor.TYPE_SEPARATION: {
                    final ColorDetails details = addSimple(((SpotColor)color).getPdfSpotColor());
                    ColorDetails patternDetails = (ColorDetails)this.documentSpotPatterns.get(details);
                    if (patternDetails == null) {
                        patternDetails = new ColorDetails(getColorspaceName(), this.body.getPdfIndirectReference(), null);
                        final PdfArray array = new PdfArray(PdfName.PATTERN);
                        array.add(details.getIndirectReference());
                        addToBody(array, patternDetails.getIndirectReference());
                        this.documentSpotPatterns.put(details, patternDetails);
                    }
                    return patternDetails;
                }
                default:
                    throw new RuntimeException("Invalid color type in PdfWriter.addSimplePatternColorspace().");
            }
        }
        catch (final Exception e) {
            throw new RuntimeException(e.getMessage());
        }
    }

//  [M3] Images

    /**
     * Use this method to get the strictImageSequence status.
     * @return value of property strictImageSequence
     */
    public boolean isStrictImageSequence() {
        return this.pdf.isStrictImageSequence();
    }

    /**
     * Use this method to set the image sequence, so that it follows
     * the text in strict order (or not).
     * @param strictImageSequence new value of property strictImageSequence
     *
     */
    public void setStrictImageSequence(final boolean strictImageSequence) {
        this.pdf.setStrictImageSequence(strictImageSequence);
    }



    /** Dictionary, containing all the images of the PDF document */
    private final PdfDictionary imageDictionary = new PdfDictionary();

    /** This is the list with all the images in the document. */
    private final HashMap images = new HashMap();

    /**
     * Use this method to adds an image to the document
     * but not to the page resources. It is used with
     * templates and <CODE>Document.add(Image)</CODE>.
     * Use this method only if you know what you're doing!
     * @param image the <CODE>Image</CODE> to add
     * @return the name of the image added
     * @throws PdfException on error
     * @throws DocumentException on error
     */
    PdfName addDirectImageSimple(final Image image) throws PdfException, DocumentException {
        return addDirectImageSimple(image, null);
    }

    /**
     * Adds an image to the document but not to the page resources.
     * It is used with templates and <CODE>Document.add(Image)</CODE>.
     * Use this method only if you know what you're doing!
     * @param image the <CODE>Image</CODE> to add
     * @param fixedRef the reference to used. It may be <CODE>null</CODE>,
     * a <CODE>PdfIndirectReference</CODE> or a <CODE>PRIndirectReference</CODE>.
     * @return the name of the image added
     * @throws PdfException on error
     * @throws DocumentException on error
     */
    private PdfName addDirectImageSimple(final Image image, final PdfIndirectReference fixedRef) throws PdfException, DocumentException {
        PdfName name;
        // if the images is already added, just retrieve the name
        if (this.images.containsKey(image.getMySerialId())) {
            name = (PdfName) this.images.get(image.getMySerialId());
        }
        // if it's a new image, add it to the document
        else {
            if (image.isImgTemplate()) {
                name = new PdfName("img" + this.images.size());
                if(image instanceof ImgWMF){
                    try {
                        final ImgWMF wmf = (ImgWMF)image;
                        wmf.readWMF(PdfTemplate.createTemplate(this, 0, 0));
                    }
                    catch (final Exception e) {
                        throw new DocumentException(e);
                    }
                }
            }
            else {
                final PdfIndirectReference dref = image.getDirectReference();
                if (dref != null) {
                    final PdfName rname = new PdfName("img" + this.images.size());
                    this.images.put(image.getMySerialId(), rname);
                    this.imageDictionary.put(rname, dref);
                    return rname;
                }
                final Image maskImage = image.getImageMask();
                PdfIndirectReference maskRef = null;
                if (maskImage != null) {
                    final PdfName mname = (PdfName)this.images.get(maskImage.getMySerialId());
                    maskRef = getImageReference(mname);
                }
                final PdfImage i = new PdfImage(image, "img" + this.images.size(), maskRef);
                if (image instanceof ImgJBIG2) {
                    final byte[] globals = ((ImgJBIG2) image).getGlobalBytes();
                    if (globals != null) {
                        final PdfDictionary decodeparms = new PdfDictionary();
                        decodeparms.put(PdfName.JBIG2GLOBALS, getReferenceJBIG2Globals(globals));
                        i.put(PdfName.DECODEPARMS, decodeparms);
                    }
                }
                if (image.hasICCProfile()) {
                    final PdfICCBased icc = new PdfICCBased(image.getICCProfile(), image.getCompressionLevel());
                    final PdfIndirectReference iccRef = add(icc);
                    final PdfArray iccArray = new PdfArray();
                    iccArray.add(PdfName.ICCBASED);
                    iccArray.add(iccRef);
                    final PdfArray colorspace = i.getAsArray(PdfName.COLORSPACE);
                    if (colorspace != null) {
                        if (colorspace.size() > 1 && PdfName.INDEXED.equals(colorspace.getPdfObject(0))) {
							colorspace.set(1, iccArray);
						} else {
							i.put(PdfName.COLORSPACE, iccArray);
						}
                    } else {
						i.put(PdfName.COLORSPACE, iccArray);
					}
                }
                add(i, fixedRef);
                name = i.name();
            }
            this.images.put(image.getMySerialId(), name);
        }
        return name;
    }

    /**
     * Writes a <CODE>PdfImage</CODE> to the outputstream.
     *
     * @param pdfImage the image to be added
     * @return a <CODE>PdfIndirectReference</CODE> to the encapsulated image
     * @throws PdfException when a document isn't open yet, or has been closed
     */

    private PdfIndirectReference add(final PdfImage pdfImage, PdfIndirectReference fixedRef) throws PdfException {
        if (! this.imageDictionary.contains(pdfImage.name())) {
            PdfXConformanceImp.checkPDFXConformance(this, PdfXConformanceImp.PDFXKEY_IMAGE, pdfImage);
            if (fixedRef instanceof PRIndirectReference) {
                final PRIndirectReference r2 = (PRIndirectReference)fixedRef;
                fixedRef = new PdfIndirectReference(0, getNewObjectNumber(r2.getReader(), r2.getNumber(), r2.getGeneration()));
            }
            try {
                if (fixedRef == null) {
					fixedRef = addToBody(pdfImage).getIndirectReference();
				} else {
					addToBody(pdfImage, fixedRef);
				}
            }
            catch(final IOException ioe) {
                throw new ExceptionConverter(ioe);
            }
            this.imageDictionary.put(pdfImage.name(), fixedRef);
            return fixedRef;
        }
        return (PdfIndirectReference) this.imageDictionary.get(pdfImage.name());
    }

    /**
     * return the <CODE>PdfIndirectReference</CODE> to the image with a given name.
     *
     * @param name the name of the image
     * @return a <CODE>PdfIndirectReference</CODE>
     */

    PdfIndirectReference getImageReference(final PdfName name) {
        return (PdfIndirectReference) this.imageDictionary.get(name);
    }

    private PdfIndirectReference add(final PdfICCBased icc) {
        PdfIndirectObject object;
        try {
            object = addToBody(icc);
        }
        catch(final IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
        return object.getIndirectReference();
    }

    /**
     * A HashSet with Stream objects containing JBIG2 Globals
     * @since 2.1.5
     */
    private final HashMap JBIG2Globals = new HashMap();
    /**
     * Gets an indirect reference to a JBIG2 Globals stream.
     * Adds the stream if it hasn't already been added to the writer.
	 * @param	content a byte array that may already been added to the writer inside a stream object.
     * @since  2.1.5
     */
    protected PdfIndirectReference getReferenceJBIG2Globals(final byte[] content) {
        if (content == null) {
			return null;
		}
        PdfStream stream;
        for (final Iterator i = this.JBIG2Globals.keySet().iterator(); i.hasNext(); ) {
            stream = (PdfStream) i.next();
            if (Arrays.equals(content, stream.getBytes())) {
                return (PdfIndirectReference) this.JBIG2Globals.get(stream);
            }
        }
        stream = new PdfStream(content);
        PdfIndirectObject ref;
        try {
            ref = addToBody(stream);
        } catch (final IOException e) {
            return null;
        }
        this.JBIG2Globals.put(stream, ref.getIndirectReference());
        return ref.getIndirectReference();
    }

//  [M4] Old table functionality; do we still need it?

    /**
     * Checks if a <CODE>Table</CODE> fits the current page of the <CODE>PdfDocument</CODE>.
     *
     * @param   table   the table that has to be checked
     * @param   margin  a certain margin
     * @return  <CODE>true</CODE> if the <CODE>Table</CODE> fits the page, <CODE>false</CODE> otherwise.
     */

    private boolean fitsPage(final Table table, final float margin) {
        return this.pdf.bottom(table) > this.pdf.indentBottom() + margin;
    }


//  [F12] tagged PDF
    /**
     * A flag indicating the presence of structure elements that contain user properties attributes.
     */
    private boolean userProperties;

    /**
     * Gets the flag indicating the presence of structure elements that contain user properties attributes.
     * @return the user properties flag
     */
    public boolean isUserProperties() {
        return this.userProperties;
    }

    /**
     * Sets the flag indicating the presence of structure elements that contain user properties attributes.
     * @param userProperties the user properties flag
     */
    public void setUserProperties(final boolean userProperties) {
        this.userProperties = userProperties;
    }

    /**
     * Holds value of property RGBTranparency.
     */
    private boolean rgbTransparencyBlending;

    /**
     * Gets the transparency blending colorspace.
     * @return <code>true</code> if the transparency blending colorspace is RGB, <code>false</code>
     * if it is the default blending colorspace
     * @since 2.1.0
     */
    public boolean isRgbTransparencyBlending() {
        return this.rgbTransparencyBlending;
    }

    /**
     * Sets the transparency blending colorspace to RGB. The default blending colorspace is
     * CMYK and will result in faded colors in the screen and in printing. Calling this method
     * will return the RGB colors to what is expected. The RGB blending will be applied to all subsequent pages
     * until other value is set.
     * Note that this is a generic solution that may not work in all cases.
     * @param rgbTransparencyBlending <code>true</code> to set the transparency blending colorspace to RGB, <code>false</code>
     * to use the default blending colorspace
     * @since 2.1.0
     */
    public void setRgbTransparencyBlending(final boolean rgbTransparencyBlending) {
        this.rgbTransparencyBlending = rgbTransparencyBlending;
    }
}
