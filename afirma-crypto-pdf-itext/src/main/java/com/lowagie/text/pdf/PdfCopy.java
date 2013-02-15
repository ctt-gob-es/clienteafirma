/*
 * $Id: PdfCopy.java 3912 2009-04-26 08:38:15Z blowagie $
 *
 * Copyright (C) 2002 Mark Thompson
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

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Rectangle;

/**
 * Make copies of PDF documents. Documents can be edited after reading and
 * before writing them out.
 * @author Mark Thompson
 */

class PdfCopy extends PdfWriter {
    /**
     * This class holds information about indirect references, since they are
     * renumbered by iText.
     */
    private static class IndirectReferences {
        private final PdfIndirectReference theRef;
        private boolean hasCopied;
        private IndirectReferences(final PdfIndirectReference ref) {
            this.theRef = ref;
            this.hasCopied = false;
        }
        private void setCopied() { this.hasCopied = true; }
        private boolean getCopied() { return this.hasCopied; }
        private PdfIndirectReference getRef() { return this.theRef; }
    };
    private HashMap indirects;
    private final HashMap indirectMap;

    private PdfReader reader;
    private PdfIndirectReference acroForm;
    private final int[] namePtr = {0};
    /** Holds value of property rotateContents. */
    private boolean rotateContents = true;
    private PdfArray fieldArray;
    private HashMap fieldTemplates;

    /**
     * A key to allow us to hash indirect references
     */
    private static class RefKey {
        private final int num;
        private final int gen;


        private RefKey(final PRIndirectReference ref) {
            this.num = ref.getNumber();
            this.gen = ref.getGeneration();
        }
        @Override
		public int hashCode() {
            return (this.gen<<16)+this.num;
        }
        @Override
		public boolean equals(final Object o) {
            if (!(o instanceof RefKey)) {
				return false;
			}
            final RefKey other = (RefKey)o;
            return this.gen == other.gen && this.num == other.num;
        }
        @Override
		public String toString() {
            return Integer.toString(this.num) + ' ' + this.gen;
        }
    }

    /**
     * Constructor
     * @param document
     * @param os outputstream
     * @param globalDate
     */
    PdfCopy(final Document document, final OutputStream os, final Calendar globalDate) throws DocumentException {
        super(new PdfDocument(globalDate), os);
        document.addDocListener(this.pdf);
        this.pdf.addWriter(this);
        this.indirectMap = new HashMap();
    }

    /** Getter for property rotateContents.
     * @return Value of property rotateContents.
     *
     */
    public boolean isRotateContents() {
        return this.rotateContents;
    }

    /** Setter for property rotateContents.
     * @param rotateContents New value of property rotateContents.
     *
     */
    public void setRotateContents(final boolean rotateContents) {
        this.rotateContents = rotateContents;
    }

    /**
     * Grabs a page from the input document
     * @param reader the reader of the document
     * @param pageNumber which page to get
     * @return the page
     */
    @Override
	public PdfImportedPage getImportedPage(final PdfReader reader, final int pageNumber) {
        if (this.currentPdfReaderInstance != null) {
            if (this.currentPdfReaderInstance.getReader() != reader) {
                try {
                    this.currentPdfReaderInstance.getReader().close();
                    this.currentPdfReaderInstance.getReaderFile().close();
                }
                catch (final IOException ioe) {
                    // empty on purpose
                }
                this.currentPdfReaderInstance = reader.getPdfReaderInstance(this);
            }
        }
        else {
            this.currentPdfReaderInstance = reader.getPdfReaderInstance(this);
        }
        return this.currentPdfReaderInstance.getImportedPage(pageNumber);
    }


    /**
     * Translate a PRIndirectReference to a PdfIndirectReference
     * In addition, translates the object numbers, and copies the
     * referenced object to the output file.
     * NB: PRIndirectReferences (and PRIndirectObjects) really need to know what
     * file they came from, because each file has its own namespace. The translation
     * we do from their namespace to ours is *at best* heuristic, and guaranteed to
     * fail under some circumstances.
     */
    private PdfIndirectReference copyIndirect(final PRIndirectReference in) throws IOException, BadPdfFormatException {
        PdfIndirectReference theRef;
        final RefKey key = new RefKey(in);
        IndirectReferences iRef = (IndirectReferences)this.indirects.get(key);
        if (iRef != null) {
            theRef = iRef.getRef();
            if (iRef.getCopied()) {
                return theRef;
            }
        }
        else {
            theRef = this.body.getPdfIndirectReference();
            iRef = new IndirectReferences(theRef);
            this.indirects.put(key, iRef);
        }
        PdfObject obj = PdfReader.getPdfObjectRelease(in);
        if (obj != null && obj.isDictionary()) {
            final PdfObject type = PdfReader.getPdfObjectRelease(((PdfDictionary)obj).get(PdfName.TYPE));
            if (type != null && PdfName.PAGE.equals(type)) {
                return theRef;
            }
        }
        iRef.setCopied();
        obj = copyObject(obj);
        addToBody(obj, theRef);
        return theRef;
    }

    /**
     * Translate a PRDictionary to a PdfDictionary. Also translate all of the
     * objects contained in it.
     */
    private PdfDictionary copyDictionary(final PdfDictionary in)
    throws IOException, BadPdfFormatException {
        final PdfDictionary out = new PdfDictionary();
        final PdfObject type = PdfReader.getPdfObjectRelease(in.get(PdfName.TYPE));

        for (final Object element : in.getKeys()) {
            final PdfName key = (PdfName)element;
            final PdfObject value = in.get(key);
            //	    System.out.println("Copy " + key);
            if (type != null && PdfName.PAGE.equals(type)) {
                if (!key.equals(PdfName.B) && !key.equals(PdfName.PARENT)) {
					out.put(key, copyObject(value));
				}
            } else {
				out.put(key, copyObject(value));
			}
        }
        return out;
    }

    /**
     * Translate a PRStream to a PdfStream. The data part copies itself.
     */
    private PdfStream copyStream(final PRStream in) throws IOException, BadPdfFormatException {
        final PRStream out = new PRStream(in, null);

        for (final Object element : in.getKeys()) {
            final PdfName key = (PdfName) element;
            final PdfObject value = in.get(key);
            out.put(key, copyObject(value));
        }

        return out;
    }


    /**
     * Translate a PRArray to a PdfArray. Also translate all of the objects contained
     * in it
     */
    private PdfArray copyArray(final PdfArray in) throws IOException, BadPdfFormatException {
        final PdfArray out = new PdfArray();

        for (final Iterator i = in.listIterator(); i.hasNext();) {
            final PdfObject value = (PdfObject)i.next();
            out.add(copyObject(value));
        }
        return out;
    }

    /**
     * Translate a PR-object to a Pdf-object
     */
    private PdfObject copyObject(final PdfObject in) throws IOException,BadPdfFormatException {
        if (in == null) {
			return PdfNull.PDFNULL;
		}
        switch (in.type) {
            case PdfObject.DICTIONARY:
                //	        System.out.println("Dictionary: " + in.toString());
                return copyDictionary((PdfDictionary)in);
            case PdfObject.INDIRECT:
                return copyIndirect((PRIndirectReference)in);
            case PdfObject.ARRAY:
                return copyArray((PdfArray)in);
            case PdfObject.NUMBER:
            case PdfObject.NAME:
            case PdfObject.STRING:
            case PdfObject.NULL:
            case PdfObject.BOOLEAN:
            case 0:
                return in;
            case PdfObject.STREAM:
                return copyStream((PRStream)in);
                //                return in;
            default:
                if (in.type < 0) {
                    final String lit = ((PdfLiteral)in).toString();
                    if (lit.equals("true") || lit.equals("false")) {
                        return new PdfBoolean(lit);
                    }
                    return new PdfLiteral(lit);
                }
                System.out.println("CANNOT COPY type " + in.type);
                return null;
        }
    }

    /**
     * convenience method. Given an imported page, set our "globals"
     */
    private int setFromIPage(final PdfImportedPage iPage) {
        final int pageNum = iPage.getPageNumber();
        final PdfReaderInstance inst = this.currentPdfReaderInstance = iPage.getPdfReaderInstance();
        this.reader = inst.getReader();
        setFromReader(this.reader);
        return pageNum;
    }

    /**
     * convenience method. Given a reader, set our "globals"
     */
    private void setFromReader(final PdfReader reader) {
        this.reader = reader;
        this.indirects = (HashMap)this.indirectMap.get(reader);
        if (this.indirects == null) {
            this.indirects = new HashMap();
            this.indirectMap.put(reader,this.indirects);
            final PdfDictionary catalog = reader.getCatalog();
            PRIndirectReference ref = null;
            final PdfObject o = catalog.get(PdfName.ACROFORM);
            if (o == null || o.type() != PdfObject.INDIRECT) {
				return;
			}
            ref = (PRIndirectReference)o;
            if (this.acroForm == null) {
				this.acroForm = this.body.getPdfIndirectReference();
			}
            this.indirects.put(new RefKey(ref), new IndirectReferences(this.acroForm));
        }
    }






    /*
     * the getCatalog method is part of PdfWriter.
     * we wrap this so that we can extend it
     */
    @Override
	protected PdfDictionary getCatalog(final PdfIndirectReference rootObj) {
        try {
            final PdfDictionary theCat = this.pdf.getCatalog(rootObj);
            if (this.fieldArray == null) {
                if (this.acroForm != null) {
					theCat.put(PdfName.ACROFORM, this.acroForm);
				}
            } else {
				addFieldResources(theCat);
			}
            return theCat;
        }
        catch (final IOException e) {
            throw new ExceptionConverter(e);
        }
    }

    private void addFieldResources(final PdfDictionary catalog) throws IOException {
        if (this.fieldArray == null) {
			return;
		}
        final PdfDictionary acroForm = new PdfDictionary();
        catalog.put(PdfName.ACROFORM, acroForm);
        acroForm.put(PdfName.FIELDS, this.fieldArray);
        acroForm.put(PdfName.DA, new PdfString("/Helv 0 Tf 0 g "));
        if (this.fieldTemplates.isEmpty()) {
			return;
		}
        final PdfDictionary dr = new PdfDictionary();
        acroForm.put(PdfName.DR, dr);
        for (final Iterator it = this.fieldTemplates.keySet().iterator(); it.hasNext();) {
            final PdfTemplate template = (PdfTemplate)it.next();
            PdfFormField.mergeResources(dr, (PdfDictionary)template.getResources());
        }
        // if (dr.get(PdfName.ENCODING) == null) dr.put(PdfName.ENCODING, PdfName.WIN_ANSI_ENCODING);
        PdfDictionary fonts = dr.getAsDict(PdfName.FONT);
        if (fonts == null) {
            fonts = new PdfDictionary();
            dr.put(PdfName.FONT, fonts);
        }
        if (!fonts.contains(PdfName.HELV)) {
            final PdfDictionary dic = new PdfDictionary(PdfName.FONT);
            dic.put(PdfName.BASEFONT, PdfName.HELVETICA);
            dic.put(PdfName.ENCODING, PdfName.WIN_ANSI_ENCODING);
            dic.put(PdfName.NAME, PdfName.HELV);
            dic.put(PdfName.SUBTYPE, PdfName.TYPE1);
            fonts.put(PdfName.HELV, addToBody(dic).getIndirectReference());
        }
        if (!fonts.contains(PdfName.ZADB)) {
            final PdfDictionary dic = new PdfDictionary(PdfName.FONT);
            dic.put(PdfName.BASEFONT, PdfName.ZAPFDINGBATS);
            dic.put(PdfName.NAME, PdfName.ZADB);
            dic.put(PdfName.SUBTYPE, PdfName.TYPE1);
            fonts.put(PdfName.ZADB, addToBody(dic).getIndirectReference());
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
     */

    @Override
	public void close() {
        if (this.open) {
            final PdfReaderInstance ri = this.currentPdfReaderInstance;
            this.pdf.close();
            super.close();
            if (ri != null) {
                try {
                    ri.getReader().close();
                    ri.getReaderFile().close();
                }
                catch (final IOException ioe) {
                    // empty on purpose
                }
            }
        }
    }

    @Override
	public void addAnnotation(final PdfAnnotation annot) {  }
    @Override
	PdfIndirectReference add(final PdfPage page, final PdfContents contents) throws PdfException { return null; }

    @Override
	public void freeReader(final PdfReader reader) throws IOException {
        this.indirectMap.remove(reader);
        if (this.currentPdfReaderInstance != null) {
            if (this.currentPdfReaderInstance.getReader() == reader) {
                try {
                    this.currentPdfReaderInstance.getReader().close();
                    this.currentPdfReaderInstance.getReaderFile().close();
                }
                catch (final IOException ioe) {
                    // empty on purpose
                }
                this.currentPdfReaderInstance = null;
            }
        }
    }



    private static class PageStamp {

        private final PdfDictionary pageN;
        private PdfCopy.StampContent under;
        private PdfCopy.StampContent over;
        private PageResources pageResources;
        private final PdfReader reader;
        private final PdfCopy cstp;

        private PageStamp(final PdfReader reader, final PdfDictionary pageN, final PdfCopy cstp) {
            this.pageN = pageN;
            this.reader = reader;
            this.cstp = cstp;
        }

        public PdfContentByte getUnderContent(){
            if (this.under == null) {
                if (this.pageResources == null) {
                    this.pageResources = new PageResources();
                    final PdfDictionary resources = this.pageN.getAsDict(PdfName.RESOURCES);
                    this.pageResources.setOriginalResources(resources, this.cstp.namePtr);
                }
                this.under = new PdfCopy.StampContent(this.cstp, this.pageResources);
            }
            return this.under;
        }

        public PdfContentByte getOverContent(){
            if (this.over == null) {
                if (this.pageResources == null) {
                    this.pageResources = new PageResources();
                    final PdfDictionary resources = this.pageN.getAsDict(PdfName.RESOURCES);
                    this.pageResources.setOriginalResources(resources, this.cstp.namePtr);
                }
                this.over = new PdfCopy.StampContent(this.cstp, this.pageResources);
            }
            return this.over;
        }



        private void applyRotation(final PdfDictionary pageN, final ByteBuffer out) {
            if (!this.cstp.rotateContents) {
				return;
			}
            final Rectangle page = this.reader.getPageSizeWithRotation(pageN);
            final int rotation = page.getRotation();
            switch (rotation) {
                case 90:
                    out.append(PdfContents.ROTATE90);
                    out.append(page.getTop());
                    out.append(' ').append('0').append(PdfContents.ROTATEFINAL);
                    break;
                case 180:
                    out.append(PdfContents.ROTATE180);
                    out.append(page.getRight());
                    out.append(' ');
                    out.append(page.getTop());
                    out.append(PdfContents.ROTATEFINAL);
                    break;
                case 270:
                    out.append(PdfContents.ROTATE270);
                    out.append('0').append(' ');
                    out.append(page.getRight());
                    out.append(PdfContents.ROTATEFINAL);
                    break;
            }
        }

        private void addDocumentField(final PdfIndirectReference ref) {
            if (this.cstp.fieldArray == null) {
				this.cstp.fieldArray = new PdfArray();
			}
            this.cstp.fieldArray.add(ref);
        }

        private void expandFields(final PdfFormField field, final ArrayList allAnnots) {
            allAnnots.add(field);
            final ArrayList kids = field.getKids();
            if (kids != null) {
                for (int k = 0; k < kids.size(); ++k) {
					expandFields((PdfFormField)kids.get(k), allAnnots);
				}
            }
        }

        public void addAnnotation(PdfAnnotation annot) {
            try {
                final ArrayList allAnnots = new ArrayList();
                if (annot.isForm()) {
                    final PdfFormField field = (PdfFormField)annot;
                    if (field.getParent() != null) {
						return;
					}
                    expandFields(field, allAnnots);
                    if (this.cstp.fieldTemplates == null) {
						this.cstp.fieldTemplates = new HashMap();
					}
                } else {
					allAnnots.add(annot);
				}
                for (int k = 0; k < allAnnots.size(); ++k) {
                    annot = (PdfAnnotation)allAnnots.get(k);
                    if (annot.isForm()) {
                        if (!annot.isUsed()) {
                            final HashMap templates = annot.getTemplates();
                            if (templates != null) {
								this.cstp.fieldTemplates.putAll(templates);
							}
                        }
                        final PdfFormField field = (PdfFormField)annot;
                        if (field.getParent() == null) {
							addDocumentField(field.getIndirectReference());
						}
                    }
                    if (annot.isAnnotation()) {
                        final PdfObject pdfobj = PdfReader.getPdfObject(this.pageN.get(PdfName.ANNOTS), this.pageN);
                        PdfArray annots = null;
                        if (pdfobj == null || !pdfobj.isArray()) {
                            annots = new PdfArray();
                            this.pageN.put(PdfName.ANNOTS, annots);
                        } else {
							annots = (PdfArray)pdfobj;
						}
                        annots.add(annot.getIndirectReference());
                        if (!annot.isUsed()) {
                            final PdfRectangle rect = (PdfRectangle)annot.get(PdfName.RECT);
                            if (rect != null && (rect.left() != 0 || rect.right() != 0 || rect.top() != 0 || rect.bottom() != 0)) {
                                final int rotation = this.reader.getPageRotation(this.pageN);
                                final Rectangle pageSize = this.reader.getPageSizeWithRotation(this.pageN);
                                switch (rotation) {
                                    case 90:
                                        annot.put(PdfName.RECT, new PdfRectangle(
                                        pageSize.getTop() - rect.bottom(),
                                        rect.left(),
                                        pageSize.getTop() - rect.top(),
                                        rect.right()));
                                        break;
                                    case 180:
                                        annot.put(PdfName.RECT, new PdfRectangle(
                                        pageSize.getRight() - rect.left(),
                                        pageSize.getTop() - rect.bottom(),
                                        pageSize.getRight() - rect.right(),
                                        pageSize.getTop() - rect.top()));
                                        break;
                                    case 270:
                                        annot.put(PdfName.RECT, new PdfRectangle(
                                        rect.bottom(),
                                        pageSize.getRight() - rect.left(),
                                        rect.top(),
                                        pageSize.getRight() - rect.right()));
                                        break;
                                }
                            }
                        }
                    }
                    if (!annot.isUsed()) {
                        annot.setUsed();
                        this.cstp.addToBody(annot, annot.getIndirectReference());
                    }
                }
            }
            catch (final IOException e) {
                throw new ExceptionConverter(e);
            }
        }
    }

    private static class StampContent extends PdfContentByte {
        private final PageResources pageResources;

        /** Creates a new instance of StampContent */
        private StampContent(final PdfWriter writer, final PageResources pageResources) {
            super(writer);
            this.pageResources = pageResources;
        }

        /**
         * Gets a duplicate of this <CODE>PdfContentByte</CODE>. All
         * the members are copied by reference but the buffer stays different.
         *
         * @return a copy of this <CODE>PdfContentByte</CODE>
         */
        @Override
		public PdfContentByte getDuplicate() {
            return new PdfCopy.StampContent(this.writer, this.pageResources);
        }

        @Override
		PageResources getPageResources() {
            return this.pageResources;
        }
    }
}