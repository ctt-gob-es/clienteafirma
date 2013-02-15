/*
 * Copyright 2003 by Paulo Soares.
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
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;

import org.xml.sax.SAXException;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Rectangle;
import com.lowagie.text.exceptions.BadPasswordException;
import com.lowagie.text.pdf.interfaces.PdfViewerPreferences;
import com.lowagie.text.pdf.internal.PdfViewerPreferencesImp;
import com.lowagie.text.xml.xmp.XmpReader;

public class PdfStamperImp extends PdfWriter {

    private PdfObject pdfFileID = null;

    private final HashMap readers2intrefs = new HashMap();
    private final HashMap readers2file = new HashMap();
    private final RandomAccessFileOrArray file;
    PdfReader reader;
    private final IntHashtable myXref = new IntHashtable();
    /** Integer(page number) -> PageStamp */
    private final HashMap pagesToContent = new HashMap();
    private boolean closed = false;
    /** Holds value of property rotateContents. */
    private boolean rotateContents = true;
    private AcroFields acroFields;
    private boolean flat = false;
    private boolean flatFreeText = false;
    private final int namePtr[] = {0};
    private final HashSet partialFlattening = new HashSet();
    private boolean useVp = false;
    private final PdfViewerPreferencesImp viewerPreferences = new PdfViewerPreferencesImp();
    private final HashMap fieldTemplates = new HashMap();
    private boolean fieldsAdded = false;
    private int sigFlags = 0;
    private final boolean append;
    private IntHashtable marked;
    private final int initialXrefSize;
    private PdfAction openAction;

    /** Creates new PdfStamperImp.
     * @param reader the read PDF
     * @param os the output destination
     * @param pdfVersion the new pdf version or '\0' to keep the same version as the original
     * document
     * @param append
     * @throws DocumentException on error
     * @throws IOException
     */
    PdfStamperImp(final PdfReader reader, final OutputStream os, final char pdfVersion, final boolean append, final Calendar globalDate) throws DocumentException, IOException {
        super(new PdfDocument(globalDate), os);
        if (!reader.isOpenedWithFullPermissions()) {
			throw new BadPasswordException("PdfReader not opened with owner password");
		}
        if (reader.isTampered()) {
			throw new DocumentException("The original document was reused. Read it again from file.");
		}
        reader.setTampered(true);
        this.reader = reader;
        this.file = reader.getSafeFile();
        this.append = append;
        if (append) {
            if (reader.isRebuilt()) {
				throw new DocumentException("Append mode requires a document without errors even if recovery was possible.");
			}
            if (reader.isEncrypted()) {
				this.crypto = new PdfEncryption(reader.getDecrypt());
			}
            this.pdf_version.setAppendmode(true);
            this.file.reOpen();
            final byte buf[] = new byte[8192];
            int n;
            while ((n = this.file.read(buf)) > 0) {
				this.os.write(buf, 0, n);
			}
            this.file.close();
            this.prevxref = reader.getLastXref();
            reader.setAppendable(true);
        }
        else {
            if (pdfVersion == 0) {
				super.setPdfVersion(reader.getPdfVersion());
			} else {
				super.setPdfVersion(pdfVersion);
			}
        }
        super.open();
        this.pdf.addWriter(this);
        if (append) {
            this.body.setRefnum(reader.getXrefSize());
            this.marked = new IntHashtable();
            if (reader.isNewXrefType()) {
				this.fullCompression = true;
			}
            if (reader.isHybridXref()) {
				this.fullCompression = false;
			}
        }
        this.initialXrefSize = reader.getXrefSize();
    }

    void close(final HashMap moreInfo, final Calendar globalDate) throws IOException {
        if (this.closed) {
			return;
		}
        if (this.useVp) {
            this.reader.setViewerPreferences(this.viewerPreferences);
            markUsed(this.reader.getTrailer().get(PdfName.ROOT));
        }
        if (this.flat) {
			flatFields();
		}
        if (this.flatFreeText) {
			flatFreeTextFields();
		}
        addFieldResources();
        final PdfDictionary catalog = this.reader.getCatalog();
        final PdfDictionary pages = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.PAGES));
        pages.put(PdfName.ITXT, new PdfString(Document.getRelease()));
        markUsed(pages);
        final PdfDictionary acroForm = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.ACROFORM), this.reader.getCatalog());
        if (this.acroFields != null && this.acroFields.getXfa().isChanged()) {
            markUsed(acroForm);
            if (!this.flat) {
				this.acroFields.getXfa().setXfa(this);
			}
        }
        if (this.sigFlags != 0) {
            if (acroForm != null) {
                acroForm.put(PdfName.SIGFLAGS, new PdfNumber(this.sigFlags));
                markUsed(acroForm);
                markUsed(catalog);
            }
        }
        this.closed = true;
        addSharedObjectsToBody();
        setOutlines();
        setJavaScript();
        addFileAttachments();
        if (this.openAction != null) {
            catalog.put(PdfName.OPENACTION, this.openAction);
        }
        if (this.pdf.pageLabels != null) {
			catalog.put(PdfName.PAGELABELS, this.pdf.pageLabels.getDictionary(this));
		}
        // OCG
        if (!this.documentOCG.isEmpty()) {
        	fillOCProperties(false);
        	final PdfDictionary ocdict = catalog.getAsDict(PdfName.OCPROPERTIES);
        	if (ocdict == null) {
        		this.reader.getCatalog().put(PdfName.OCPROPERTIES, this.OCProperties);
        	}
        	else {
        		ocdict.put(PdfName.OCGS, this.OCProperties.get(PdfName.OCGS));
        		PdfDictionary ddict = ocdict.getAsDict(PdfName.D);
        		if (ddict == null) {
        			ddict = new PdfDictionary();
        			ocdict.put(PdfName.D, ddict);
        		}
        		ddict.put(PdfName.ORDER, this.OCProperties.getAsDict(PdfName.D).get(PdfName.ORDER));
        		ddict.put(PdfName.RBGROUPS, this.OCProperties.getAsDict(PdfName.D).get(PdfName.RBGROUPS));
        		ddict.put(PdfName.OFF, this.OCProperties.getAsDict(PdfName.D).get(PdfName.OFF));
        		ddict.put(PdfName.AS, this.OCProperties.getAsDict(PdfName.D).get(PdfName.AS));
            }
        }
        // metadata
        int skipInfo = -1;
        final PRIndirectReference iInfo = (PRIndirectReference)this.reader.getTrailer().get(PdfName.INFO);
        final PdfDictionary oldInfo = (PdfDictionary)PdfReader.getPdfObject(iInfo);
        String producer = null;
        if (iInfo != null) {
			skipInfo = iInfo.getNumber();
		}
        if (oldInfo != null && oldInfo.get(PdfName.PRODUCER) != null) {
			producer = oldInfo.getAsString(PdfName.PRODUCER).toString();
		}
        if (producer == null) {
        	producer = Document.getVersion();
        }
        else if (producer.indexOf(Document.getProduct()) == -1) {
        	final StringBuffer buf = new StringBuffer(producer);
        	buf.append("; modified using ");
        	buf.append(Document.getVersion());
        	producer = buf.toString();
        }
        // XMP
        byte[] altMetadata = null;
        final PdfObject xmpo = PdfReader.getPdfObject(catalog.get(PdfName.METADATA));
        if (xmpo != null && xmpo.isStream()) {
            altMetadata = PdfReader.getStreamBytesRaw((PRStream)xmpo);
            PdfReader.killIndirect(catalog.get(PdfName.METADATA));
        }
        if (this.xmpMetadata != null) {
        	altMetadata = this.xmpMetadata;
        }
        // if there is XMP data to add: add it
        final PdfDate date = new PdfDate(globalDate);
        if (altMetadata != null) {
        	PdfStream xmp;
        	try {
        		final XmpReader xmpr = new XmpReader(altMetadata);
        		if (!xmpr.replace("http://ns.adobe.com/pdf/1.3/", "Producer", producer)) {
					xmpr.add("rdf:Description", "http://ns.adobe.com/pdf/1.3/", "pdf:Producer", producer);
				}
        		if (!xmpr.replace("http://ns.adobe.com/xap/1.0/", "ModifyDate", date.getW3CDate())) {
					xmpr.add("rdf:Description", "http://ns.adobe.com/xap/1.0/", "xmp:ModifyDate", date.getW3CDate());
				}
        		xmpr.replace("http://ns.adobe.com/xap/1.0/", "MetadataDate", date.getW3CDate());
            	xmp = new PdfStream(xmpr.serializeDoc());
        	}
        	catch(final SAXException e) {
        		xmp = new PdfStream(altMetadata);
        	}
        	catch(final IOException e) {
        		xmp = new PdfStream(altMetadata);
        	}
        	xmp.put(PdfName.TYPE, PdfName.METADATA);
        	xmp.put(PdfName.SUBTYPE, PdfName.XML);
        	if (this.crypto != null && !this.crypto.isMetadataEncrypted()) {
        		final PdfArray ar = new PdfArray();
        		ar.add(PdfName.CRYPT);
        		xmp.put(PdfName.FILTER, ar);
        	}
        	if (this.append && xmpo != null) {
        		this.body.add(xmp, xmpo.getIndRef());
        	}
        	else {
        		catalog.put(PdfName.METADATA, this.body.add(xmp).getIndirectReference());
        		markUsed(catalog);
        	}
        }
        try {
            this.file.reOpen();
            alterContents();
            final int rootN = ((PRIndirectReference)this.reader.trailer.get(PdfName.ROOT)).getNumber();
            if (this.append) {
                final int keys[] = this.marked.getKeys();
                for (final int j : keys) {
                    final PdfObject obj = this.reader.getPdfObjectRelease(j);
                    if (obj != null && skipInfo != j && j < this.initialXrefSize) {
                        addToBody(obj, j, j != rootN);
                    }
                }
                for (int k = this.initialXrefSize; k < this.reader.getXrefSize(); ++k) {
                    final PdfObject obj = this.reader.getPdfObject(k);
                    if (obj != null) {
                        addToBody(obj, getNewObjectNumber(this.reader, k, 0));
                    }
                }
            }
            else {
                for (int k = 1; k < this.reader.getXrefSize(); ++k) {
                    final PdfObject obj = this.reader.getPdfObjectRelease(k);
                    if (obj != null && skipInfo != k) {
                        addToBody(obj, getNewObjectNumber(this.reader, k, 0), k != rootN);
                    }
                }
            }
        }
        finally {
            try {
                this.file.close();
            }
            catch (final Exception e) {
                // empty on purpose
            }
        }
        PdfIndirectReference encryption = null;
        PdfObject fileID = null;
        if (this.crypto != null) {
            if (this.append) {
                encryption = this.reader.getCryptoRef();
            }
            else {
                final PdfIndirectObject encryptionObject = addToBody(this.crypto.getEncryptionDictionary(), false);
                encryption = encryptionObject.getIndirectReference();
            }
            fileID = this.crypto.getFileID();
        }
        else {
            fileID = PdfEncryption.createInfoId(PdfEncryption.createDocumentId());
        }

        this.pdfFileID = fileID;

        final PRIndirectReference iRoot = (PRIndirectReference)this.reader.trailer.get(PdfName.ROOT);
        final PdfIndirectReference root = new PdfIndirectReference(0, getNewObjectNumber(this.reader, iRoot.getNumber(), 0));
        PdfIndirectReference info = null;
        final PdfDictionary newInfo = new PdfDictionary();
        if (oldInfo != null) {
            for (final Object element : oldInfo.getKeys()) {
                final PdfName key = (PdfName)element;
                final PdfObject value = PdfReader.getPdfObject(oldInfo.get(key));
                newInfo.put(key, value);
            }
        }
        if (moreInfo != null) {
            for (final Iterator i = moreInfo.entrySet().iterator(); i.hasNext();) {
                final Map.Entry entry = (Map.Entry) i.next();
                final String key = (String) entry.getKey();
                final PdfName keyName = new PdfName(key);
                final String value = (String) entry.getValue();
                if (value == null) {
					newInfo.remove(keyName);
				} else {
					newInfo.put(keyName, new PdfString(value, PdfObject.TEXT_UNICODE));
				}
            }
        }
        newInfo.put(PdfName.MODDATE, date);
        newInfo.put(PdfName.PRODUCER, new PdfString(producer));
        if (this.append) {
            if (iInfo == null) {
				info = addToBody(newInfo, false).getIndirectReference();
			} else {
				info = addToBody(newInfo, iInfo.getNumber(), false).getIndirectReference();
			}
        }
        else {
            info = addToBody(newInfo, false).getIndirectReference();
        }
        // write the cross-reference table of the body
        this.body.writeCrossReferenceTable(this.os, root, info, encryption, fileID, this.prevxref);
        if (this.fullCompression) {
            this.os.write(getISOBytes("startxref\n"));
            this.os.write(getISOBytes(String.valueOf(this.body.offset())));
            this.os.write(getISOBytes("\n%%EOF\n"));
        }
        else {
            final PdfTrailer trailer = new PdfTrailer(this.body.size(),
            this.body.offset(),
            root,
            info,
            encryption,
            fileID, this.prevxref);
            trailer.toPdf(this, this.os);
        }
        this.os.flush();
        if (isCloseStream()) {
			this.os.close();
		}
        this.reader.close();
    }

    private void applyRotation(final PdfDictionary pageN, final ByteBuffer out) {
        if (!this.rotateContents) {
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

    private void alterContents() throws IOException {
        for (final Iterator i = this.pagesToContent.values().iterator(); i.hasNext();) {
            final PageStamp ps = (PageStamp)i.next();
            final PdfDictionary pageN = ps.pageN;
            markUsed(pageN);
            PdfArray ar = null;
            final PdfObject content = PdfReader.getPdfObject(pageN.get(PdfName.CONTENTS), pageN);
            if (content == null) {
                ar = new PdfArray();
                pageN.put(PdfName.CONTENTS, ar);
            }
            else if (content.isArray()) {
                ar = (PdfArray)content;
                markUsed(ar);
            }
            else if (content.isStream()) {
                ar = new PdfArray();
                ar.add(pageN.get(PdfName.CONTENTS));
                pageN.put(PdfName.CONTENTS, ar);
            }
            else {
                ar = new PdfArray();
                pageN.put(PdfName.CONTENTS, ar);
            }
            final ByteBuffer out = new ByteBuffer();
            if (ps.under != null) {
                out.append(PdfContents.SAVESTATE);
                applyRotation(pageN, out);
                out.append(ps.under.getInternalBuffer());
                out.append(PdfContents.RESTORESTATE);
            }
            if (ps.over != null) {
				out.append(PdfContents.SAVESTATE);
			}
            PdfStream stream = new PdfStream(out.toByteArray());
            stream.flateCompress(this.compressionLevel);
            ar.addFirst(addToBody(stream).getIndirectReference());
            out.reset();
            if (ps.over != null) {
                out.append(' ');
                out.append(PdfContents.RESTORESTATE);
                final ByteBuffer buf = ps.over.getInternalBuffer();
                out.append(buf.getBuffer(), 0, ps.replacePoint);
                out.append(PdfContents.SAVESTATE);
                applyRotation(pageN, out);
                out.append(buf.getBuffer(), ps.replacePoint, buf.size() - ps.replacePoint);
                out.append(PdfContents.RESTORESTATE);
                stream = new PdfStream(out.toByteArray());
                stream.flateCompress(this.compressionLevel);
                ar.add(addToBody(stream).getIndirectReference());
            }
            alterResources(ps);
        }
    }

    private void alterResources(final PageStamp ps) {
        ps.pageN.put(PdfName.RESOURCES, ps.pageResources.getResources());
    }

    @Override
	protected int getNewObjectNumber(final PdfReader reader, final int number, final int generation) {
        final IntHashtable ref = (IntHashtable)this.readers2intrefs.get(reader);
        if (ref != null) {
            int n = ref.get(number);
            if (n == 0) {
                n = getIndirectReferenceNumber();
                ref.put(number, n);
            }
            return n;
        }
        if (this.currentPdfReaderInstance == null) {
            if (this.append && number < this.initialXrefSize) {
				return number;
			}
            int n = this.myXref.get(number);
            if (n == 0) {
                n = getIndirectReferenceNumber();
                this.myXref.put(number, n);
            }
            return n;
        } else {
			return this.currentPdfReaderInstance.getNewObjectNumber(number, generation);
		}
    }

    @Override
	RandomAccessFileOrArray getReaderFile(final PdfReader reader) {
        if (this.readers2intrefs.containsKey(reader)) {
            final RandomAccessFileOrArray raf = (RandomAccessFileOrArray)this.readers2file.get(reader);
            if (raf != null) {
				return raf;
			}
            return reader.getSafeFile();
        }
        if (this.currentPdfReaderInstance == null) {
			return this.file;
		} else {
			return this.currentPdfReaderInstance.getReaderFile();
		}
    }

    /**
     * @param reader
     * @param openFile
     * @throws IOException
     */
    private void registerReader(final PdfReader reader, final boolean openFile) throws IOException {
        if (this.readers2intrefs.containsKey(reader)) {
			return;
		}
        this.readers2intrefs.put(reader, new IntHashtable());
        if (openFile) {
            final RandomAccessFileOrArray raf = reader.getSafeFile();
            this.readers2file.put(reader, raf);
            raf.reOpen();
        }
    }



    private static void findAllObjects(final PdfReader reader, final PdfObject obj, final IntHashtable hits) {
        if (obj == null) {
			return;
		}
        switch (obj.type()) {
            case PdfObject.INDIRECT:
                final PRIndirectReference iref = (PRIndirectReference)obj;
                if (reader != iref.getReader()) {
					return;
				}
                if (hits.containsKey(iref.getNumber())) {
					return;
				}
                hits.put(iref.getNumber(), 1);
                findAllObjects(reader, PdfReader.getPdfObject(obj), hits);
                return;
            case PdfObject.ARRAY:
                final PdfArray a = (PdfArray)obj;
                for (int k = 0; k < a.size(); ++k) {
                    findAllObjects(reader, a.getPdfObject(k), hits);
                }
                return;
            case PdfObject.DICTIONARY:
            case PdfObject.STREAM:
                final PdfDictionary dic = (PdfDictionary)obj;
			for (final Object element : dic.getKeys()) {
			    final PdfName name = (PdfName)element;
			    findAllObjects(reader, dic.get(name), hits);
			}
                return;
        }
    }



    private PageStamp getPageStamp(final int pageNum) {
        final PdfDictionary pageN = this.reader.getPageN(pageNum);
        PageStamp ps = (PageStamp)this.pagesToContent.get(pageN);
        if (ps == null) {
            ps = new PageStamp(this, this.reader, pageN);
            this.pagesToContent.put(pageN, ps);
        }
        return ps;
    }



    private PdfContentByte getOverContent(final int pageNum) {
        if (pageNum < 1 || pageNum > this.reader.getNumberOfPages()) {
			return null;
		}
        final PageStamp ps = getPageStamp(pageNum);
        if (ps.over == null) {
			ps.over = new StampContent(this, ps);
		}
        return ps.over;
    }

    private void correctAcroFieldPages(final int page) {
        if (this.acroFields == null) {
			return;
		}
        if (page > this.reader.getNumberOfPages()) {
			return;
		}
        final HashMap fields = this.acroFields.getFields();
        for (final Iterator it = fields.values().iterator(); it.hasNext();) {
            final AcroFields.Item item = (AcroFields.Item)it.next();
            for (int k = 0; k < item.size(); ++k) {
                final int p = item.getPage(k).intValue();
                if (p >= page) {
					item.forcePage(k, p + 1);
				}
            }
        }
    }

    private static void moveRectangle(final PdfDictionary dic2, final PdfReader r, final int pageImported, final PdfName key, final String name) {
        final Rectangle m = r.getBoxSize(pageImported, name);
        if (m == null) {
			dic2.remove(key);
		} else {
			dic2.put(key, new PdfRectangle(m));
		}
    }





    /** Getter for property rotateContents.
     * @return Value of property rotateContents.
     *
     */
    boolean isRotateContents() {
        return this.rotateContents;
    }

    /** Setter for property rotateContents.
     * @param rotateContents New value of property rotateContents.
     *
     */
    void setRotateContents(final boolean rotateContents) {
        this.rotateContents = rotateContents;
    }

    boolean isContentWritten() {
        return this.body.size() > 1;
    }

    AcroFields getAcroFields() {
        if (this.acroFields == null) {
            this.acroFields = new AcroFields(this.reader, this);
        }
        return this.acroFields;
    }

    void setFormFlattening(final boolean flat) {
        this.flat = flat;
    }

	void setFreeTextFlattening(final boolean flat) {
		this.flatFreeText = flat;
    }



    private void flatFields() {
        if (this.append) {
			throw new IllegalArgumentException("Field flattening is not supported in append mode.");
		}
        getAcroFields();
        final HashMap fields = this.acroFields.getFields();
        if (this.fieldsAdded && this.partialFlattening.isEmpty()) {
            for (final Iterator i = fields.keySet().iterator(); i.hasNext();) {
                this.partialFlattening.add(i.next());
            }
        }
        final PdfDictionary acroForm = this.reader.getCatalog().getAsDict(PdfName.ACROFORM);
        PdfArray acroFds = null;
        if (acroForm != null) {
            acroFds = (PdfArray)PdfReader.getPdfObject(acroForm.get(PdfName.FIELDS), acroForm);
        }
        for (final Iterator i = fields.entrySet().iterator(); i.hasNext();) {
            final Map.Entry entry = (Map.Entry) i.next();
            final String name = (String) entry.getKey();
            if (!this.partialFlattening.isEmpty() && !this.partialFlattening.contains(name)) {
				continue;
			}
            final AcroFields.Item item = (AcroFields.Item) entry.getValue();
            for (int k = 0; k < item.size(); ++k) {
                final PdfDictionary merged = item.getMerged(k);
                final PdfNumber ff = merged.getAsNumber(PdfName.F);
                int flags = 0;
                if (ff != null) {
					flags = ff.intValue();
				}
                final int page = item.getPage(k).intValue();
                final PdfDictionary appDic = merged.getAsDict(PdfName.AP);
                if (appDic != null && (flags & PdfFormField.FLAGS_PRINT) != 0 && (flags & PdfFormField.FLAGS_HIDDEN) == 0) {
                    final PdfObject obj = appDic.get(PdfName.N);
                    PdfAppearance app = null;
                    if (obj != null) {
                        PdfObject objReal = PdfReader.getPdfObject(obj);
                        if (obj instanceof PdfIndirectReference && !obj.isIndirect()) {
							app = new PdfAppearance((PdfIndirectReference)obj);
						} else if (objReal instanceof PdfStream) {
                            ((PdfDictionary)objReal).put(PdfName.SUBTYPE, PdfName.FORM);
                            app = new PdfAppearance((PdfIndirectReference)obj);
                        }
                        else {
                            if (objReal != null && objReal.isDictionary()) {
                                final PdfName as = merged.getAsName(PdfName.AS);
                                if (as != null) {
                                    final PdfIndirectReference iref = (PdfIndirectReference)((PdfDictionary)objReal).get(as);
                                    if (iref != null) {
                                        app = new PdfAppearance(iref);
                                        if (iref.isIndirect()) {
                                            objReal = PdfReader.getPdfObject(iref);
                                            ((PdfDictionary)objReal).put(PdfName.SUBTYPE, PdfName.FORM);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if (app != null) {
                        final Rectangle box = PdfReader.getNormalizedRectangle(merged.getAsArray(PdfName.RECT));
                        final PdfContentByte cb = getOverContent(page);
                        cb.setLiteral("Q ");
                        cb.addTemplate(app, box.getLeft(), box.getBottom());
                        cb.setLiteral("q ");
                    }
                }
                if (this.partialFlattening.isEmpty()) {
					continue;
				}
                final PdfDictionary pageDic = this.reader.getPageN(page);
                final PdfArray annots = pageDic.getAsArray(PdfName.ANNOTS);
                if (annots == null) {
					continue;
				}
                for (int idx = 0; idx < annots.size(); ++idx) {
                    final PdfObject ran = annots.getPdfObject(idx);
                    if (!ran.isIndirect()) {
						continue;
					}
                    final PdfObject ran2 = item.getWidgetRef(k);
                    if (!ran2.isIndirect()) {
						continue;
					}
                    if (((PRIndirectReference)ran).getNumber() == ((PRIndirectReference)ran2).getNumber()) {
                        annots.remove(idx--);
                        PRIndirectReference wdref = (PRIndirectReference)ran2;
                        while (true) {
                            final PdfDictionary wd = (PdfDictionary)PdfReader.getPdfObject(wdref);
                            final PRIndirectReference parentRef = (PRIndirectReference)wd.get(PdfName.PARENT);
                            PdfReader.killIndirect(wdref);
                            if (parentRef == null) { // reached AcroForm
                                for (int fr = 0; fr < acroFds.size(); ++fr) {
                                    final PdfObject h = acroFds.getPdfObject(fr);
                                    if (h.isIndirect() && ((PRIndirectReference)h).getNumber() == wdref.getNumber()) {
                                        acroFds.remove(fr);
                                        --fr;
                                    }
                                }
                                break;
                            }
                            final PdfDictionary parent = (PdfDictionary)PdfReader.getPdfObject(parentRef);
                            final PdfArray kids = parent.getAsArray(PdfName.KIDS);
                            for (int fr = 0; fr < kids.size(); ++fr) {
                                final PdfObject h = kids.getPdfObject(fr);
                                if (h.isIndirect() && ((PRIndirectReference)h).getNumber() == wdref.getNumber()) {
                                    kids.remove(fr);
                                    --fr;
                                }
                            }
                            if (!kids.isEmpty()) {
								break;
							}
                            wdref = parentRef;
                        }
                    }
                }
                if (annots.isEmpty()) {
                    PdfReader.killIndirect(pageDic.get(PdfName.ANNOTS));
                    pageDic.remove(PdfName.ANNOTS);
                }
            }
        }
        if (!this.fieldsAdded && this.partialFlattening.isEmpty()) {
            for (int page = 1; page <= this.reader.getNumberOfPages(); ++page) {
                final PdfDictionary pageDic = this.reader.getPageN(page);
                final PdfArray annots = pageDic.getAsArray(PdfName.ANNOTS);
                if (annots == null) {
					continue;
				}
                for (int idx = 0; idx < annots.size(); ++idx) {
                    final PdfObject annoto = annots.getDirectObject(idx);
                    if (annoto instanceof PdfIndirectReference && !annoto.isIndirect()) {
						continue;
					}
                    if (!annoto.isDictionary() || PdfName.WIDGET.equals(((PdfDictionary)annoto).get(PdfName.SUBTYPE))) {
                        annots.remove(idx);
                        --idx;
                    }
                }
                if (annots.isEmpty()) {
                    PdfReader.killIndirect(pageDic.get(PdfName.ANNOTS));
                    pageDic.remove(PdfName.ANNOTS);
                }
            }
            eliminateAcroformObjects();
        }
    }

    private void eliminateAcroformObjects() {
        final PdfObject acro = this.reader.getCatalog().get(PdfName.ACROFORM);
        if (acro == null) {
			return;
		}
        final PdfDictionary acrodic = (PdfDictionary)PdfReader.getPdfObject(acro);
        this.reader.killXref(acrodic.get(PdfName.XFA));
        acrodic.remove(PdfName.XFA);
        final PdfObject iFields = acrodic.get(PdfName.FIELDS);
        if (iFields != null) {
            final PdfDictionary kids = new PdfDictionary();
            kids.put(PdfName.KIDS, iFields);
            sweepKids(kids);
            PdfReader.killIndirect(iFields);
            acrodic.put(PdfName.FIELDS, new PdfArray());
        }
//        PdfReader.killIndirect(acro);
//        reader.getCatalog().remove(PdfName.ACROFORM);
    }

    private void sweepKids(final PdfObject obj) {
        final PdfObject oo = PdfReader.killIndirect(obj);
        if (oo == null || !oo.isDictionary()) {
			return;
		}
        final PdfDictionary dic = (PdfDictionary)oo;
        final PdfArray kids = (PdfArray)PdfReader.killIndirect(dic.get(PdfName.KIDS));
        if (kids == null) {
			return;
		}
        for (int k = 0; k < kids.size(); ++k) {
            sweepKids(kids.getPdfObject(k));
        }
    }

    private void flatFreeTextFields()
	{
		if (this.append) {
			throw new IllegalArgumentException("FreeText flattening is not supported in append mode.");
		}

		for (int page = 1; page <= this.reader.getNumberOfPages(); ++page)
		{
			final PdfDictionary pageDic = this.reader.getPageN(page);
			final PdfArray annots = pageDic.getAsArray(PdfName.ANNOTS);
			if (annots == null) {
				continue;
			}
			for (int idx = 0; idx < annots.size(); ++idx)
			{
				final PdfObject annoto = annots.getDirectObject(idx);
				if (annoto instanceof PdfIndirectReference && !annoto.isIndirect()) {
					continue;
				}

				final PdfDictionary annDic = (PdfDictionary)annoto;
 				if (!((PdfName)annDic.get(PdfName.SUBTYPE)).equals(PdfName.FREETEXT)) {
					continue;
				}
				final PdfNumber ff = annDic.getAsNumber(PdfName.F);
                final int flags = ff != null ? ff.intValue() : 0;

				if ( (flags & PdfFormField.FLAGS_PRINT) != 0 && (flags & PdfFormField.FLAGS_HIDDEN) == 0)
				{
					final PdfObject obj1 = annDic.get(PdfName.AP);
					if (obj1 == null) {
						continue;
					}
					final PdfDictionary appDic = obj1 instanceof PdfIndirectReference ?
							(PdfDictionary) PdfReader.getPdfObject(obj1) : (PdfDictionary) obj1;
					final PdfObject obj = appDic.get(PdfName.N);
					PdfAppearance app = null;
					PdfObject objReal = PdfReader.getPdfObject(obj);

					if (obj instanceof PdfIndirectReference && !obj.isIndirect()) {
						app = new PdfAppearance((PdfIndirectReference)obj);
					} else if (objReal instanceof PdfStream)
					{
						((PdfDictionary)objReal).put(PdfName.SUBTYPE, PdfName.FORM);
						app = new PdfAppearance((PdfIndirectReference)obj);
					}
					else
					{
						if (objReal.isDictionary())
						{
							final PdfName as_p = appDic.getAsName(PdfName.AS);
							if (as_p != null)
							{
								final PdfIndirectReference iref = (PdfIndirectReference)((PdfDictionary)objReal).get(as_p);
								if (iref != null)
								{
									app = new PdfAppearance(iref);
									if (iref.isIndirect())
									{
										objReal = PdfReader.getPdfObject(iref);
										((PdfDictionary)objReal).put(PdfName.SUBTYPE, PdfName.FORM);
									}
								}
							}
						}
					}
					if (app != null)
					{
						final Rectangle box = PdfReader.getNormalizedRectangle(annDic.getAsArray(PdfName.RECT));
						final PdfContentByte cb = getOverContent(page);
						cb.setLiteral("Q ");
						cb.addTemplate(app, box.getLeft(), box.getBottom());
						cb.setLiteral("q ");
					}
				}
			}
			for (int idx = 0; idx < annots.size(); ++idx)
			{
			    final PdfDictionary annot = annots.getAsDict(idx);
				if (annot != null)
				{
					if (PdfName.FREETEXT.equals(annot.get(PdfName.SUBTYPE)))
					{
					    annots.remove(idx);
						--idx;
					}
				}
			}
			if (annots.isEmpty())
			{
				PdfReader.killIndirect(pageDic.get(PdfName.ANNOTS));
				pageDic.remove(PdfName.ANNOTS);
			}
		}
	}

    /**
     * @see com.lowagie.text.pdf.PdfWriter#getPageReference(int)
     */
    @Override
	public PdfIndirectReference getPageReference(final int page) {
        final PdfIndirectReference ref = this.reader.getPageOrigRef(page);
        if (ref == null) {
			throw new IllegalArgumentException("Invalid page number " + page);
		}
        return ref;
    }

    /**
     * @see com.lowagie.text.pdf.PdfWriter#addAnnotation(com.lowagie.text.pdf.PdfAnnotation)
     */
    @Override
	public void addAnnotation(final PdfAnnotation annot) {
        throw new RuntimeException("Unsupported in this context. Use PdfStamper.addAnnotation()");
    }

    private void addDocumentField(final PdfIndirectReference ref) {
        final PdfDictionary catalog = this.reader.getCatalog();
        PdfDictionary acroForm = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.ACROFORM), catalog);
        if (acroForm == null) {
            acroForm = new PdfDictionary();
            catalog.put(PdfName.ACROFORM, acroForm);
            markUsed(catalog);
        }
        PdfArray fields = (PdfArray)PdfReader.getPdfObject(acroForm.get(PdfName.FIELDS), acroForm);
        if (fields == null) {
            fields = new PdfArray();
            acroForm.put(PdfName.FIELDS, fields);
            markUsed(acroForm);
        }
        if (!acroForm.contains(PdfName.DA)) {
            acroForm.put(PdfName.DA, new PdfString("/Helv 0 Tf 0 g "));
            markUsed(acroForm);
        }
        fields.add(ref);
        markUsed(fields);
    }

    private void addFieldResources() throws IOException {
        if (this.fieldTemplates.isEmpty()) {
			return;
		}
        final PdfDictionary catalog = this.reader.getCatalog();
        PdfDictionary acroForm = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.ACROFORM), catalog);
        if (acroForm == null) {
            acroForm = new PdfDictionary();
            catalog.put(PdfName.ACROFORM, acroForm);
            markUsed(catalog);
        }
        PdfDictionary dr = (PdfDictionary)PdfReader.getPdfObject(acroForm.get(PdfName.DR), acroForm);
        if (dr == null) {
            dr = new PdfDictionary();
            acroForm.put(PdfName.DR, dr);
            markUsed(acroForm);
        }
        markUsed(dr);
        for (final Iterator it = this.fieldTemplates.keySet().iterator(); it.hasNext();) {
            final PdfTemplate template = (PdfTemplate)it.next();
            PdfFormField.mergeResources(dr, (PdfDictionary)template.getResources(), this);
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
        if (acroForm.get(PdfName.DA) == null) {
            acroForm.put(PdfName.DA, new PdfString("/Helv 0 Tf 0 g "));
            markUsed(acroForm);
        }
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

    void addAnnotation(PdfAnnotation annot, PdfDictionary pageN) {
        try {
            final ArrayList allAnnots = new ArrayList();
            if (annot.isForm()) {
                this.fieldsAdded = true;
                getAcroFields();
                final PdfFormField field = (PdfFormField)annot;
                if (field.getParent() != null) {
					return;
				}
                expandFields(field, allAnnots);
            } else {
				allAnnots.add(annot);
			}
            for (int k = 0; k < allAnnots.size(); ++k) {
                annot = (PdfAnnotation)allAnnots.get(k);
                if (annot.getPlaceInPage() > 0) {
					pageN = this.reader.getPageN(annot.getPlaceInPage());
				}
                if (annot.isForm()) {
                    if (!annot.isUsed()) {
                        final HashMap templates = annot.getTemplates();
                        if (templates != null) {
							this.fieldTemplates.putAll(templates);
						}
                    }
                    final PdfFormField field = (PdfFormField)annot;
                    if (field.getParent() == null) {
						addDocumentField(field.getIndirectReference());
					}
                }
                if (annot.isAnnotation()) {
                    final PdfObject pdfobj = PdfReader.getPdfObject(pageN.get(PdfName.ANNOTS), pageN);
                    PdfArray annots = null;
                    if (pdfobj == null || !pdfobj.isArray()) {
                        annots = new PdfArray();
                        pageN.put(PdfName.ANNOTS, annots);
                        markUsed(pageN);
                    } else {
						annots = (PdfArray)pdfobj;
					}
                    annots.add(annot.getIndirectReference());
                    markUsed(annots);
                    if (!annot.isUsed()) {
                        final PdfRectangle rect = (PdfRectangle)annot.get(PdfName.RECT);
                        if (rect != null && (rect.left() != 0 || rect.right() != 0 || rect.top() != 0 || rect.bottom() != 0)) {
                            final int rotation = this.reader.getPageRotation(pageN);
                            final Rectangle pageSize = this.reader.getPageSizeWithRotation(pageN);
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
                    addToBody(annot, annot.getIndirectReference());
                }
            }
        }
        catch (final IOException e) {
            throw new ExceptionConverter(e);
        }
    }

    @Override
	void addAnnotation(final PdfAnnotation annot, final int page) {
    	annot.setPage(page);
        addAnnotation(annot, this.reader.getPageN(page));
    }

    private void outlineTravel(PRIndirectReference outline) {
        while (outline != null) {
            final PdfDictionary outlineR = (PdfDictionary)PdfReader.getPdfObjectRelease(outline);
            final PRIndirectReference first = (PRIndirectReference)outlineR.get(PdfName.FIRST);
            if (first != null) {
                outlineTravel(first);
            }
            PdfReader.killIndirect(outlineR.get(PdfName.DEST));
            PdfReader.killIndirect(outlineR.get(PdfName.A));
            PdfReader.killIndirect(outline);
            outline = (PRIndirectReference)outlineR.get(PdfName.NEXT);
        }
    }

    private void deleteOutlines() {
        final PdfDictionary catalog = this.reader.getCatalog();
        final PRIndirectReference outlines = (PRIndirectReference)catalog.get(PdfName.OUTLINES);
        if (outlines == null) {
			return;
		}
        outlineTravel(outlines);
        PdfReader.killIndirect(outlines);
        catalog.remove(PdfName.OUTLINES);
        markUsed(catalog);
    }

    private void setJavaScript() throws IOException {
        final HashMap djs = this.pdf.getDocumentLevelJS();
        if (djs.isEmpty()) {
			return;
		}
        final PdfDictionary catalog = this.reader.getCatalog();
        PdfDictionary names = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.NAMES), catalog);
        if (names == null) {
            names = new PdfDictionary();
            catalog.put(PdfName.NAMES, names);
            markUsed(catalog);
        }
        markUsed(names);
        final PdfDictionary tree = PdfNameTree.writeTree(djs, this);
        names.put(PdfName.JAVASCRIPT, addToBody(tree).getIndirectReference());
    }

    private void addFileAttachments() throws IOException {
        final HashMap fs = this.pdf.getDocumentFileAttachment();
        if (fs.isEmpty()) {
			return;
		}
        final PdfDictionary catalog = this.reader.getCatalog();
        PdfDictionary names = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.NAMES), catalog);
        if (names == null) {
            names = new PdfDictionary();
            catalog.put(PdfName.NAMES, names);
            markUsed(catalog);
        }
        markUsed(names);
        final HashMap old = PdfNameTree.readTree((PdfDictionary)PdfReader.getPdfObjectRelease(names.get(PdfName.EMBEDDEDFILES)));
        for (final Iterator it = fs.entrySet().iterator(); it.hasNext();) {
            final Map.Entry entry = (Map.Entry) it.next();
            final String name = (String) entry.getKey();
            int k = 0;
            String nn = name;
            while (old.containsKey(nn)) {
                ++k;
                nn += " " + k;
            }
            old.put(nn, entry.getValue());
        }
        final PdfDictionary tree = PdfNameTree.writeTree(old, this);
        names.put(PdfName.EMBEDDEDFILES, addToBody(tree).getIndirectReference());
    }



    private void setOutlines() throws IOException {
        if (this.newBookmarks == null) {
			return;
		}
        deleteOutlines();
        if (this.newBookmarks.isEmpty()) {
			return;
		}
        final PdfDictionary catalog = this.reader.getCatalog();
        final boolean namedAsNames = catalog.get(PdfName.DESTS) != null;
        writeOutlines(catalog, namedAsNames);
        markUsed(catalog);
    }

    /**
     * Sets the viewer preferences.
     * @param preferences the viewer preferences
     * @see PdfWriter#setViewerPreferences(int)
     */
    @Override
	public void setViewerPreferences(final int preferences) {
        this.useVp = true;
        this.viewerPreferences.setViewerPreferences(preferences);
    }

    /** Adds a viewer preference
     * @param key a key for a viewer preference
     * @param value the value for the viewer preference
     * @see PdfViewerPreferences#addViewerPreference
     */
    @Override
	public void addViewerPreference(final PdfName key, final PdfObject value) {
    	this.useVp = true;
    	this.viewerPreferences.addViewerPreference(key, value);
    }

    /**
     * Set the signature flags.
     * @param f the flags. This flags are ORed with current ones
     */
    @Override
	public void setSigFlags(final int f) {
        this.sigFlags |= f;
    }

    /** Always throws an <code>UnsupportedOperationException</code>.
     * @param actionType ignore
     * @param action ignore
     * @throws PdfException ignore
     * @see PdfStamper#setPageAction(PdfName, PdfAction, int)
     */
    @Override
	public void setPageAction(final PdfName actionType, final PdfAction action) throws PdfException {
        throw new UnsupportedOperationException("Use setPageAction(PdfName actionType, PdfAction action, int page)");
    }



    /**
     * Always throws an <code>UnsupportedOperationException</code>.
     * @param seconds ignore
     */
    @Override
	public void setDuration(final int seconds) {
        throw new UnsupportedOperationException("Use setPageAction(PdfName actionType, PdfAction action, int page)");
    }

    /**
     * Always throws an <code>UnsupportedOperationException</code>.
     * @param transition ignore
     */
    @Override
	public void setTransition(final PdfTransition transition) {
        throw new UnsupportedOperationException("Use setPageAction(PdfName actionType, PdfAction action, int page)");
    }





    protected void markUsed(final PdfObject obj) {
        if (this.append && obj != null) {
            PRIndirectReference ref = null;
            if (obj.type() == PdfObject.INDIRECT) {
				ref = (PRIndirectReference)obj;
			} else {
				ref = obj.getIndRef();
			}
            if (ref != null) {
				this.marked.put(ref.getNumber(), 1);
			}
        }
    }



    /**
     * Getter for property append.
     * @return Value of property append.
     */
    boolean isAppend() {
        return this.append;
    }

    /** Additional-actions defining the actions to be taken in
     * response to various trigger events affecting the document
     * as a whole. The actions types allowed are: <CODE>DOCUMENT_CLOSE</CODE>,
     * <CODE>WILL_SAVE</CODE>, <CODE>DID_SAVE</CODE>, <CODE>WILL_PRINT</CODE>
     * and <CODE>DID_PRINT</CODE>.
     *
     * @param actionType the action type
     * @param action the action to execute in response to the trigger
     * @throws PdfException on invalid action type
     */
    @Override
	public void setAdditionalAction(final PdfName actionType, final PdfAction action) throws PdfException {
        if (!(actionType.equals(DOCUMENT_CLOSE) ||
        actionType.equals(WILL_SAVE) ||
        actionType.equals(DID_SAVE) ||
        actionType.equals(WILL_PRINT) ||
        actionType.equals(DID_PRINT))) {
            throw new PdfException("Invalid additional action type: " + actionType.toString());
        }
        PdfDictionary aa = this.reader.getCatalog().getAsDict(PdfName.AA);
        if (aa == null) {
            if (action == null) {
				return;
			}
            aa = new PdfDictionary();
            this.reader.getCatalog().put(PdfName.AA, aa);
        }
        markUsed(aa);
        if (action == null) {
			aa.remove(actionType);
		} else {
			aa.put(actionType, action);
		}
    }

    /**
     * @see com.lowagie.text.pdf.PdfWriter#setOpenAction(com.lowagie.text.pdf.PdfAction)
     */
    @Override
	public void setOpenAction(final PdfAction action) {
        this.openAction = action;
    }

    /**
     * @see com.lowagie.text.pdf.PdfWriter#setOpenAction(java.lang.String)
     */
    @Override
	public void setOpenAction(final String name) {
        throw new UnsupportedOperationException("Open actions by name are not supported.");
    }

    /**
     * @see com.lowagie.text.pdf.PdfWriter#setThumbnail(com.lowagie.text.Image)
     */
    @Override
	public void setThumbnail(final com.lowagie.text.Image image) {
        throw new UnsupportedOperationException("Use PdfStamper.setThumbnail().");
    }



    @Override
	public PdfContentByte getDirectContentUnder() {
        throw new UnsupportedOperationException("Use PdfStamper.getUnderContent() or PdfStamper.getOverContent()");
    }

    @Override
	public PdfContentByte getDirectContent() {
        throw new UnsupportedOperationException("Use PdfStamper.getUnderContent() or PdfStamper.getOverContent()");
    }

    /**
     * Reads the OCProperties dictionary from the catalog of the existing document
     * and fills the documentOCG, documentOCGorder and OCGRadioGroup variables in PdfWriter.
     * Note that the original OCProperties of the existing document can contain more information.
     * @since	2.1.2
     */
    private void readOCProperties() {
    	if (!this.documentOCG.isEmpty()) {
    		return;
    	}
    	final PdfDictionary dict = this.reader.getCatalog().getAsDict(PdfName.OCPROPERTIES);
    	if (dict == null) {
    		return;
    	}
    	final PdfArray ocgs = dict.getAsArray(PdfName.OCGS);
    	PdfIndirectReference ref;
    	PdfLayer layer;
    	final HashMap ocgmap = new HashMap();
    	for (final Iterator i = ocgs.listIterator(); i.hasNext(); ) {
    		ref = (PdfIndirectReference)i.next();
    		layer = new PdfLayer(null);
    		layer.setRef(ref);
    		layer.setOnPanel(false);
			layer.merge((PdfDictionary)PdfReader.getPdfObject(ref));
    		ocgmap.put(ref.toString(), layer);
    	}
    	final PdfDictionary d = dict.getAsDict(PdfName.D);
    	final PdfArray off = d.getAsArray(PdfName.OFF);
    	if (off != null) {
    		for (final Iterator i = off.listIterator(); i.hasNext(); ) {
    			ref = (PdfIndirectReference)i.next();
    			layer = (PdfLayer)ocgmap.get(ref.toString());
    			layer.setOn(false);
    		}
    	}
    	final PdfArray order = d.getAsArray(PdfName.ORDER);
    	if (order != null) {
    		addOrder(null, order, ocgmap);
    	}
    	this.documentOCG.addAll(ocgmap.values());
    	this.OCGRadioGroup = d.getAsArray(PdfName.RBGROUPS);
    	this.OCGLocked = d.getAsArray(PdfName.LOCKED);
    	if (this.OCGLocked == null) {
			this.OCGLocked = new PdfArray();
		}
    }

    /**
     * Recursive method to reconstruct the documentOCGorder variable in the writer.
     * @param	parent	a parent PdfLayer (can be null)
     * @param	arr		an array possibly containing children for the parent PdfLayer
     * @param	ocgmap	a HashMap with indirect reference Strings as keys and PdfLayer objects as values.
     * @since	2.1.2
     */
    private void addOrder(final PdfLayer parent, final PdfArray arr, final Map ocgmap) {
    	PdfObject obj;
    	PdfLayer layer;
    	for (int i = 0; i < arr.size(); i++) {
    		obj = arr.getPdfObject(i);
    		if (obj.isIndirect()) {
    			layer = (PdfLayer)ocgmap.get(obj.toString());
    			layer.setOnPanel(true);
    			registerLayer(layer);
    			if (parent != null) {
    				parent.addChild(layer);
    			}
    			if (arr.size() > i + 1 && arr.getPdfObject(i + 1).isArray()) {
    				i++;
    				addOrder(layer, (PdfArray)arr.getPdfObject(i), ocgmap);
    			}
    		}
    		else if (obj.isArray()) {
    		    final PdfArray sub = (PdfArray)obj;
    			if (sub.isEmpty()) {
					return;
				}
    			obj = sub.getPdfObject(0);
    			if (obj.isString()) {
    				layer = new PdfLayer(obj.toString());
    				layer.setOnPanel(true);
    				registerLayer(layer);
    				if (parent != null) {
    					parent.addChild(layer);
    				}
    				final PdfArray array = new PdfArray();
    				for (final Iterator j = sub.listIterator(); j.hasNext(); ) {
    					array.add((PdfObject)j.next());
    				}
    				addOrder(layer, array, ocgmap);
    			}
    			else {
    				addOrder(parent, (PdfArray)obj, ocgmap);
    			}
    		}
    	}
    }

    /**
     * Gets the PdfLayer objects in an existing document as a Map
     * with the names/titles of the layers as keys.
     * @return	a Map with all the PdfLayers in the document (and the name/title of the layer as key)
     * @since	2.1.2
     */
    public Map getPdfLayers() {
    	if (this.documentOCG.isEmpty()) {
    		readOCProperties();
    	}
    	final HashMap map = new HashMap();
    	PdfLayer layer;
    	String key;
    	for (final Iterator i = this.documentOCG.iterator(); i.hasNext(); ) {
    		layer = (PdfLayer)i.next();
    		if (layer.getTitle() == null) {
    			key = layer.getAsString(PdfName.NAME).toString();
    		}
    		else {
    			key = layer.getTitle();
    		}
    		if (map.containsKey(key)) {
    			int seq = 2;
    			String tmp = key + "(" + seq + ")";
    			while (map.containsKey(tmp)) {
    				seq++;
    				tmp = key + "(" + seq + ")";
    			}
    			key = tmp;
    		}
			map.put(key, layer);
    	}
    	return map;
    }

    static class PageStamp {

        PdfDictionary pageN;
        private StampContent under;
        private StampContent over;
        PageResources pageResources;
        private final int replacePoint = 0;

        private PageStamp(final PdfStamperImp stamper, final PdfReader reader, final PdfDictionary pageN) {
            this.pageN = pageN;
            this.pageResources = new PageResources();
            final PdfDictionary resources = pageN.getAsDict(PdfName.RESOURCES);
            this.pageResources.setOriginalResources(resources, stamper.namePtr);
        }
    }

    public PdfObject getFileID() {
        return this.pdfFileID;
    }
}
