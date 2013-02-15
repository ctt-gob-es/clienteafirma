/*
 * $Id: PdfReader.java 3948 2009-06-03 15:17:22Z blowagie $
 *
 * Copyright 2001, 2002 Paulo Soares
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.Key;
import java.security.MessageDigest;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.zip.InflaterInputStream;

import org.bouncycastle.cms.CMSEnvelopedData;
import org.bouncycastle.cms.RecipientInformation;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.PageSize;
import com.lowagie.text.Rectangle;
import com.lowagie.text.exceptions.BadPasswordException;
import com.lowagie.text.exceptions.InvalidPdfException;
import com.lowagie.text.exceptions.UnsupportedPdfException;
import com.lowagie.text.pdf.interfaces.PdfViewerPreferences;
import com.lowagie.text.pdf.internal.PdfViewerPreferencesImp;

/** Reads a PDF document.
 * @author Paulo Soares (psoares@consiste.pt)
 * @author Kazuya Ujihara
 */
public class PdfReader implements PdfViewerPreferences {

    private static final PdfName pageInhCandidates[] = {
        PdfName.MEDIABOX, PdfName.ROTATE, PdfName.RESOURCES, PdfName.CROPBOX
    };

    private static final byte endstream[] = PdfEncodings.convertToBytes("endstream", null);
    private static final byte endobj[] = PdfEncodings.convertToBytes("endobj", null);
    protected PRTokeniser tokens;
    // Each xref pair is a position
    // type 0 -> -1, 0
    // type 1 -> offset, 0
    // type 2 -> index, obj num
    private int xref[];
    private HashMap objStmMark;
    private IntHashtable objStmToOffset;
    private boolean newXrefType;
    private ArrayList xrefObj;
    private PdfDictionary rootPages;
    protected PdfDictionary trailer;
    protected PdfDictionary catalog;
    private PageRefs pageRefs;
    private PRAcroForm acroForm = null;
    private boolean acroFormParsed = false;
    private boolean encrypted = false;
    private boolean rebuilt = false;
    private int freeXref;
    private boolean tampered = false;
    private int lastXref;
    private int eofPos;
    private char pdfVersion;
    private PdfEncryption decrypt;
    private byte password[] = null; //added by ujihara for decryption
    private Key certificateKey = null; //added by Aiken Sam for certificate decryption
    private Certificate certificate = null; //added by Aiken Sam for certificate decryption
    private String certificateKeyProvider = null; //added by Aiken Sam for certificate decryption
    private boolean ownerPasswordUsed;
    private final ArrayList strings = new ArrayList();
    private boolean sharedStreams = true;
    private boolean consolidateNamedDestinations = false;
    private int rValue;
    private int pValue;
    private int objNum;
    private int objGen;
    private int fileLength;
    private boolean hybridXref;
    private int lastXrefPartial = -1;
    private boolean partial;

    private PRIndirectReference cryptoRef;
	private final PdfViewerPreferencesImp viewerPreferences = new PdfViewerPreferencesImp();
    private boolean encryptionError;

    /**
     * Holds value of property appendable.
     */
    private boolean appendable;

    protected PdfReader() {
    }

    /** Reads and parses a PDF document.
     * @param filename the file name of the document
     * @throws IOException on error
     */
    public PdfReader(final String filename) throws IOException {
        this(filename, null);
    }

    /** Reads and parses a PDF document.
     * @param filename the file name of the document
     * @param ownerPassword the password to read the document
     * @throws IOException on error
     */
    private PdfReader(final String filename, final byte ownerPassword[]) throws IOException {
        this.password = ownerPassword;
        this.tokens = new PRTokeniser(filename);
        readPdf();
    }

    /** Reads and parses a PDF document.
     * @param pdfIn the byte array with the document
     * @throws IOException on error
     */
    public PdfReader(final byte pdfIn[]) throws IOException {
        this(pdfIn, null);
    }

    /** Reads and parses a PDF document.
     * @param pdfIn the byte array with the document
     * @param ownerPassword the password to read the document
     * @throws IOException on error
     */
    public PdfReader(final byte pdfIn[], final byte ownerPassword[]) throws IOException {
        this.password = ownerPassword;
        this.tokens = new PRTokeniser(pdfIn);
        readPdf();
    }

    /** Reads and parses a PDF document.
     * @param filename the file name of the document
     * @param certificate the certificate to read the document
     * @param certificateKey the private key of the certificate
     * @param certificateKeyProvider the security provider for certificateKey
     * @throws IOException on error
     */
    public PdfReader(final String filename, final Certificate certificate, final Key certificateKey, final String certificateKeyProvider) throws IOException {
        this.certificate = certificate;
        this.certificateKey = certificateKey;
        this.certificateKeyProvider = certificateKeyProvider;
        this.tokens = new PRTokeniser(filename);
        readPdf();
    }



    /** Reads and parses a PDF document.
     * @param url the URL of the document
     * @param ownerPassword the password to read the document
     * @throws IOException on error
     */
    private PdfReader(final URL url, final byte ownerPassword[]) throws IOException {
        this.password = ownerPassword;
        this.tokens = new PRTokeniser(new RandomAccessFileOrArray(url));
        readPdf();
    }

    /**
     * Reads and parses a PDF document.
     * @param is the <CODE>InputStream</CODE> containing the document. The stream is read to the
     * end but is not closed
     * @param ownerPassword the password to read the document
     * @throws IOException on error
     */
    private PdfReader(final InputStream is, final byte ownerPassword[]) throws IOException {
        this.password = ownerPassword;
        this.tokens = new PRTokeniser(new RandomAccessFileOrArray(is));
        readPdf();
    }





    /** Creates an independent duplicate.
     * @param reader the <CODE>PdfReader</CODE> to duplicate
     */
    PdfReader(final PdfReader reader) {
        this.appendable = reader.appendable;
        this.consolidateNamedDestinations = reader.consolidateNamedDestinations;
        this.encrypted = reader.encrypted;
        this.rebuilt = reader.rebuilt;
        this.sharedStreams = reader.sharedStreams;
        this.tampered = reader.tampered;
        this.password = reader.password;
        this.pdfVersion = reader.pdfVersion;
        this.eofPos = reader.eofPos;
        this.freeXref = reader.freeXref;
        this.lastXref = reader.lastXref;
        this.tokens = new PRTokeniser(reader.tokens.getSafeFile());
        if (reader.decrypt != null) {
			this.decrypt = new PdfEncryption(reader.decrypt);
		}
        this.pValue = reader.pValue;
        this.rValue = reader.rValue;
        this.xrefObj = new ArrayList(reader.xrefObj);
        for (int k = 0; k < reader.xrefObj.size(); ++k) {
            this.xrefObj.set(k, duplicatePdfObject((PdfObject)reader.xrefObj.get(k), this));
        }
        this.pageRefs = new PageRefs(reader.pageRefs, this);
        this.trailer = (PdfDictionary)duplicatePdfObject(reader.trailer, this);
        this.catalog = this.trailer.getAsDict(PdfName.ROOT);
        this.rootPages = this.catalog.getAsDict(PdfName.PAGES);
        this.fileLength = reader.fileLength;
        this.partial = reader.partial;
        this.hybridXref = reader.hybridXref;
        this.objStmToOffset = reader.objStmToOffset;
        this.xref = reader.xref;
        this.cryptoRef = (PRIndirectReference)duplicatePdfObject(reader.cryptoRef, this);
        this.ownerPasswordUsed = reader.ownerPasswordUsed;
    }

    /** Gets a new file instance of the original PDF
     * document.
     * @return a new file instance of the original PDF document
     */
    public RandomAccessFileOrArray getSafeFile() {
        return this.tokens.getSafeFile();
    }

    protected PdfReaderInstance getPdfReaderInstance(final PdfWriter writer) {
        return new PdfReaderInstance(this, writer);
    }

    /** Gets the number of pages in the document.
     * @return the number of pages in the document
     */
    public int getNumberOfPages() {
        return this.pageRefs.size();
    }

    /** Returns the document's catalog. This dictionary is not a copy,
     * any changes will be reflected in the catalog.
     * @return the document's catalog
     */
    public PdfDictionary getCatalog() {
        return this.catalog;
    }

    /** Returns the document's acroform, if it has one.
     * @return the document's acroform
     */
    public PRAcroForm getAcroForm() {
        if (!this.acroFormParsed) {
            this.acroFormParsed = true;
            final PdfObject form = this.catalog.get(PdfName.ACROFORM);
            if (form != null) {
                try {
                    this.acroForm = new PRAcroForm(this);
                    this.acroForm.readAcroForm((PdfDictionary)getPdfObject(form));
                }
                catch (final Exception e) {
                    this.acroForm = null;
                }
            }
        }
        return this.acroForm;
    }
    /**
     * Gets the page rotation. This value can be 0, 90, 180 or 270.
     * @param index the page number. The first page is 1
     * @return the page rotation
     */
    int getPageRotation(final int index) {
        return getPageRotation(this.pageRefs.getPageNRelease(index));
    }

    int getPageRotation(final PdfDictionary page) {
        final PdfNumber rotate = page.getAsNumber(PdfName.ROTATE);
        if (rotate == null) {
			return 0;
		} else {
            int n = rotate.intValue();
            n %= 360;
            return n < 0 ? n + 360 : n;
        }
    }
    /** Gets the page size, taking rotation into account. This
     * is a <CODE>Rectangle</CODE> with the value of the /MediaBox and the /Rotate key.
     * @param index the page number. The first page is 1
     * @return a <CODE>Rectangle</CODE>
     */
    Rectangle getPageSizeWithRotation(final int index) {
        return getPageSizeWithRotation(this.pageRefs.getPageNRelease(index));
    }

    /**
     * Gets the rotated page from a page dictionary.
     * @param page the page dictionary
     * @return the rotated page
     */
    Rectangle getPageSizeWithRotation(final PdfDictionary page) {
        Rectangle rect = getPageSize(page);
        int rotation = getPageRotation(page);
        while (rotation > 0) {
            rect = rect.rotate();
            rotation -= 90;
        }
        return rect;
    }

    /** Gets the page size without taking rotation into account. This
     * is the value of the /MediaBox key.
     * @param index the page number. The first page is 1
     * @return the page size
     */
    Rectangle getPageSize(final int index) {
        return getPageSize(this.pageRefs.getPageNRelease(index));
    }

    /**
     * Gets the page from a page dictionary
     * @param page the page dictionary
     * @return the page
     */
    private Rectangle getPageSize(final PdfDictionary page) {
        final PdfArray mediaBox = page.getAsArray(PdfName.MEDIABOX);
        return getNormalizedRectangle(mediaBox);
    }



    /** Gets the box size. Allowed names are: "crop", "trim", "art", "bleed" and "media".
     * @param index the page number. The first page is 1
     * @param boxName the box name
     * @return the box rectangle or null
     */
    Rectangle getBoxSize(final int index, final String boxName) {
        final PdfDictionary page = this.pageRefs.getPageNRelease(index);
        PdfArray box = null;
        if (boxName.equals("trim")) {
			box = (PdfArray)getPdfObjectRelease(page.get(PdfName.TRIMBOX));
		} else if (boxName.equals("art")) {
			box = (PdfArray)getPdfObjectRelease(page.get(PdfName.ARTBOX));
		} else if (boxName.equals("bleed")) {
			box = (PdfArray)getPdfObjectRelease(page.get(PdfName.BLEEDBOX));
		} else if (boxName.equals("crop")) {
			box = (PdfArray)getPdfObjectRelease(page.get(PdfName.CROPBOX));
		} else if (boxName.equals("media")) {
			box = (PdfArray)getPdfObjectRelease(page.get(PdfName.MEDIABOX));
		}
        if (box == null) {
			return null;
		}
        return getNormalizedRectangle(box);
    }

    /** Returns the content of the document information dictionary as a <CODE>HashMap</CODE>
     * of <CODE>String</CODE>.
     * @return content of the document information dictionary
     */
    public HashMap getInfo() {
        final HashMap map = new HashMap();
        final PdfDictionary info = this.trailer.getAsDict(PdfName.INFO);
        if (info == null) {
			return map;
		}
        for (final Object element : info.getKeys()) {
            final PdfName key = (PdfName)element;
            final PdfObject obj = getPdfObject(info.get(key));
            if (obj == null) {
				continue;
			}
            String value = obj.toString();
            switch (obj.type()) {
                case PdfObject.STRING: {
                    value = ((PdfString)obj).toUnicodeString();
                    break;
                }
                case PdfObject.NAME: {
                    value = PdfName.decodeName(value);
                    break;
                }
            }
            map.put(PdfName.decodeName(key.toString()), value);
        }
        return map;
    }

    /** Normalizes a <CODE>Rectangle</CODE> so that llx and lly are smaller than urx and ury.
     * @param box the original rectangle
     * @return a normalized <CODE>Rectangle</CODE>
     */
    static Rectangle getNormalizedRectangle(final PdfArray box) {
        final float llx = ((PdfNumber)getPdfObjectRelease(box.getPdfObject(0))).floatValue();
        final float lly = ((PdfNumber)getPdfObjectRelease(box.getPdfObject(1))).floatValue();
        final float urx = ((PdfNumber)getPdfObjectRelease(box.getPdfObject(2))).floatValue();
        final float ury = ((PdfNumber)getPdfObjectRelease(box.getPdfObject(3))).floatValue();
        return new Rectangle(Math.min(llx, urx), Math.min(lly, ury),
        Math.max(llx, urx), Math.max(lly, ury));
    }

    protected void readPdf() throws IOException {
        try {
            this.fileLength = this.tokens.getFile().length();
            this.pdfVersion = this.tokens.checkPdfHeader();
            try {
                readXref();
            }
            catch (final Exception e) {
                try {
                    this.rebuilt = true;
                    rebuildXref();
                    this.lastXref = -1;
                }
                catch (final Exception ne) {
                    throw new InvalidPdfException("Rebuild failed: " + ne.getMessage() + "; Original message: " + e.getMessage());
                }
            }
            try {
                readDocObj();
            }
            catch (final Exception e) {
            	if (e instanceof BadPasswordException) {
					throw new BadPasswordException(e.getMessage());
				}
                if (this.rebuilt || this.encryptionError) {
					throw new InvalidPdfException(e.getMessage());
				}
                this.rebuilt = true;
                this.encrypted = false;
                rebuildXref();
                this.lastXref = -1;
                readDocObj();
            }

            this.strings.clear();
            readPages();
            eliminateSharedStreams();
            removeUnusedObjects();
        }
        finally {
            try {
                this.tokens.close();
            }
            catch (final Exception e) {
                // empty on purpose
            }
        }
    }

    private void readPdfPartial() throws IOException {
        try {
            this.fileLength = this.tokens.getFile().length();
            this.pdfVersion = this.tokens.checkPdfHeader();
            try {
                readXref();
            }
            catch (final Exception e) {
                try {
                    this.rebuilt = true;
                    rebuildXref();
                    this.lastXref = -1;
                }
                catch (final Exception ne) {
                    throw new InvalidPdfException("Rebuild failed: " + ne.getMessage() + "; Original message: " + e.getMessage());
                }
            }
            readDocObjPartial();
            readPages();
        }
        catch (final IOException e) {
            try{this.tokens.close();}catch(final Exception ee){}
            throw e;
        }
    }

    private boolean equalsArray(final byte ar1[], final byte ar2[], final int size) {
        for (int k = 0; k < size; ++k) {
            if (ar1[k] != ar2[k]) {
				return false;
			}
        }
        return true;
    }

    /**
     * @throws IOException
     */
    private void readDecryptedDocObj() throws IOException {
        if (this.encrypted) {
			return;
		}
        final PdfObject encDic = this.trailer.get(PdfName.ENCRYPT);
        if (encDic == null || encDic.toString().equals("null")) {
			return;
		}
        this.encryptionError = true;
        byte[] encryptionKey = null;
        this.encrypted = true;
        final PdfDictionary enc = (PdfDictionary)getPdfObject(encDic);

        String s;
        PdfObject o;

        final PdfArray documentIDs = this.trailer.getAsArray(PdfName.ID);
        byte documentID[] = null;
        if (documentIDs != null) {
            o = documentIDs.getPdfObject(0);
            this.strings.remove(o);
            s = o.toString();
            documentID = com.lowagie.text.DocWriter.getISOBytes(s);
            if (documentIDs.size() > 1) {
				this.strings.remove(documentIDs.getPdfObject(1));
			}
        }
        // just in case we have a broken producer
        if (documentID == null) {
			documentID = new byte[0];
		}
        byte uValue[] = null;
        byte oValue[] = null;
        int cryptoMode = PdfWriter.STANDARD_ENCRYPTION_40;
        int lengthValue = 0;

        final PdfObject filter = getPdfObjectRelease(enc.get(PdfName.FILTER));

        if (filter.equals(PdfName.STANDARD)) {
            s = enc.get(PdfName.U).toString();
            this.strings.remove(enc.get(PdfName.U));
            uValue = com.lowagie.text.DocWriter.getISOBytes(s);
            s = enc.get(PdfName.O).toString();
            this.strings.remove(enc.get(PdfName.O));
            oValue = com.lowagie.text.DocWriter.getISOBytes(s);

            o = enc.get(PdfName.P);
            if (!o.isNumber()) {
				throw new InvalidPdfException("Illegal P value.");
			}
            this.pValue = ((PdfNumber)o).intValue();

            o = enc.get(PdfName.R);
            if (!o.isNumber()) {
				throw new InvalidPdfException("Illegal R value.");
			}
            this.rValue = ((PdfNumber)o).intValue();

            switch (this.rValue) {
            case 2:
            	cryptoMode = PdfWriter.STANDARD_ENCRYPTION_40;
            	break;
            case 3:
                o = enc.get(PdfName.LENGTH);
                if (!o.isNumber()) {
					throw new InvalidPdfException("Illegal Length value.");
				}
                lengthValue = ( (PdfNumber) o).intValue();
                if (lengthValue > 128 || lengthValue < 40 || lengthValue % 8 != 0) {
					throw new InvalidPdfException("Illegal Length value.");
				}
                cryptoMode = PdfWriter.STANDARD_ENCRYPTION_128;
                break;
            case 4:
                PdfDictionary dic = (PdfDictionary)enc.get(PdfName.CF);
                if (dic == null) {
					throw new InvalidPdfException("/CF not found (encryption)");
				}
                dic = (PdfDictionary)dic.get(PdfName.STDCF);
                if (dic == null) {
					throw new InvalidPdfException("/StdCF not found (encryption)");
				}
                if (PdfName.V2.equals(dic.get(PdfName.CFM))) {
					cryptoMode = PdfWriter.STANDARD_ENCRYPTION_128;
				} else if (PdfName.AESV2.equals(dic.get(PdfName.CFM))) {
					cryptoMode = PdfWriter.ENCRYPTION_AES_128;
				} else {
					throw new UnsupportedPdfException("No compatible encryption found");
				}
                final PdfObject em = enc.get(PdfName.ENCRYPTMETADATA);
                if (em != null && em.toString().equals("false")) {
					cryptoMode |= PdfWriter.DO_NOT_ENCRYPT_METADATA;
				}
                break;
            default:
            	throw new UnsupportedPdfException("Unknown encryption type R = " + this.rValue);
            }
        }
        else if (filter.equals(PdfName.PUBSEC)) {
            boolean foundRecipient = false;
            byte[] envelopedData = null;
            PdfArray recipients = null;

            o = enc.get(PdfName.V);
            if (!o.isNumber()) {
				throw new InvalidPdfException("Illegal V value.");
			}
            final int vValue = ((PdfNumber)o).intValue();
            switch(vValue) {
            case 1:
                cryptoMode = PdfWriter.STANDARD_ENCRYPTION_40;
                lengthValue = 40;
                recipients = (PdfArray)enc.get(PdfName.RECIPIENTS);
            	break;
            case 2:
                o = enc.get(PdfName.LENGTH);
                if (!o.isNumber()) {
					throw new InvalidPdfException("Illegal Length value.");
				}
                lengthValue = ( (PdfNumber) o).intValue();
                if (lengthValue > 128 || lengthValue < 40 || lengthValue % 8 != 0) {
					throw new InvalidPdfException("Illegal Length value.");
				}
                cryptoMode = PdfWriter.STANDARD_ENCRYPTION_128;
                recipients = (PdfArray)enc.get(PdfName.RECIPIENTS);
                break;
            case 4:
                PdfDictionary dic = (PdfDictionary)enc.get(PdfName.CF);
                if (dic == null) {
					throw new InvalidPdfException("/CF not found (encryption)");
				}
                dic = (PdfDictionary)dic.get(PdfName.DEFAULTCRYPTFILTER);
                if (dic == null) {
					throw new InvalidPdfException("/DefaultCryptFilter not found (encryption)");
				}
                if (PdfName.V2.equals(dic.get(PdfName.CFM))) {
                    cryptoMode = PdfWriter.STANDARD_ENCRYPTION_128;
                    lengthValue = 128;
                }
                else if (PdfName.AESV2.equals(dic.get(PdfName.CFM))) {
                    cryptoMode = PdfWriter.ENCRYPTION_AES_128;
                    lengthValue = 128;
                } else {
					throw new UnsupportedPdfException("No compatible encryption found");
				}
                final PdfObject em = dic.get(PdfName.ENCRYPTMETADATA);
                if (em != null && em.toString().equals("false")) {
					cryptoMode |= PdfWriter.DO_NOT_ENCRYPT_METADATA;
				}

                recipients = (PdfArray)dic.get(PdfName.RECIPIENTS);
                break;
            default:
            	throw new UnsupportedPdfException("Unknown encryption type V = " + this.rValue);
            }
            for (int i = 0; i<recipients.size(); i++) {
                final PdfObject recipient = recipients.getPdfObject(i);
                this.strings.remove(recipient);

                CMSEnvelopedData data = null;
                try {
                    data = new CMSEnvelopedData(recipient.getBytes());

                    final Iterator recipientCertificatesIt = data.getRecipientInfos().getRecipients().iterator();

                    while (recipientCertificatesIt.hasNext()) {
                        final RecipientInformation recipientInfo = (RecipientInformation)recipientCertificatesIt.next();

                        if (recipientInfo.getRID().match(this.certificate) && !foundRecipient) {
                         envelopedData = recipientInfo.getContent(this.certificateKey, this.certificateKeyProvider);
                         foundRecipient = true;
                        }
                    }
                }
                catch (final Exception f) {
                    throw new ExceptionConverter(f);
                }
            }

            if(!foundRecipient || envelopedData == null) {
                throw new UnsupportedPdfException("Bad certificate and key.");
            }

            MessageDigest md = null;

            try {
                md = MessageDigest.getInstance("SHA-1");
                md.update(envelopedData, 0, 20);
                for (int i = 0; i<recipients.size(); i++) {
                  final byte[] encodedRecipient = recipients.getPdfObject(i).getBytes();
                  md.update(encodedRecipient);
                }
                if ((cryptoMode & PdfWriter.DO_NOT_ENCRYPT_METADATA) != 0) {
					md.update(new byte[]{(byte)255, (byte)255, (byte)255, (byte)255});
				}
                encryptionKey = md.digest();
            }
            catch (final Exception f) {
                throw new ExceptionConverter(f);
            }
        }


        this.decrypt = new PdfEncryption();
        this.decrypt.setCryptoMode(cryptoMode, lengthValue);

        if (filter.equals(PdfName.STANDARD)) {
            //check by owner password
            this.decrypt.setupByOwnerPassword(documentID, this.password, uValue, oValue, this.pValue);
            if (!equalsArray(uValue, this.decrypt.userKey, this.rValue == 3 || this.rValue == 4 ? 16 : 32)) {
                //check by user password
                this.decrypt.setupByUserPassword(documentID, this.password, oValue, this.pValue);
                if (!equalsArray(uValue, this.decrypt.userKey, this.rValue == 3 || this.rValue == 4 ? 16 : 32)) {
                    throw new BadPasswordException("Bad user password");
                }
            } else {
				this.ownerPasswordUsed = true;
			}
        }
        else if (filter.equals(PdfName.PUBSEC)) {
            this.decrypt.setupByEncryptionKey(encryptionKey, lengthValue);
            this.ownerPasswordUsed = true;
        }

        for (int k = 0; k < this.strings.size(); ++k) {
            final PdfString str = (PdfString)this.strings.get(k);
            str.decrypt(this);
        }

        if (encDic.isIndirect()) {
            this.cryptoRef = (PRIndirectReference)encDic;
            this.xrefObj.set(this.cryptoRef.getNumber(), null);
        }
        this.encryptionError = false;
    }

    /**
     * @param obj
     * @return a PdfObject
     */
    public static PdfObject getPdfObjectRelease(final PdfObject obj) {
        final PdfObject obj2 = getPdfObject(obj);
        releaseLastXrefPartial(obj);
        return obj2;
    }


    /**
     * Reads a <CODE>PdfObject</CODE> resolving an indirect reference
     * if needed.
     * @param obj the <CODE>PdfObject</CODE> to read
     * @return the resolved <CODE>PdfObject</CODE>
     */
    public static PdfObject getPdfObject(PdfObject obj) {
        if (obj == null) {
			return null;
		}
        if (!obj.isIndirect()) {
			return obj;
		}
        try {
            final PRIndirectReference ref = (PRIndirectReference)obj;
            final int idx = ref.getNumber();
            final boolean appendable = ref.getReader().appendable;
            obj = ref.getReader().getPdfObject(idx);
            if (obj == null) {
                return null;
            }
            else {
                if (appendable) {
                    switch (obj.type()) {
                        case PdfObject.NULL:
                            obj = new PdfNull();
                            break;
                        case PdfObject.BOOLEAN:
                            obj = new PdfBoolean(((PdfBoolean)obj).booleanValue());
                            break;
                        case PdfObject.NAME:
                            obj = new PdfName(obj.getBytes());
                            break;
                    }
                    obj.setIndRef(ref);
                }
                return obj;
            }
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    /**
     * Reads a <CODE>PdfObject</CODE> resolving an indirect reference
     * if needed. If the reader was opened in partial mode the object will be released
     * to save memory.
     * @param obj the <CODE>PdfObject</CODE> to read
     * @param parent
     * @return a PdfObject
     */
    static PdfObject getPdfObjectRelease(final PdfObject obj, final PdfObject parent) {
        final PdfObject obj2 = getPdfObject(obj, parent);
        releaseLastXrefPartial(obj);
        return obj2;
    }

    /**
     * @param obj
     * @param parent
     * @return a PdfObject
     */
    static PdfObject getPdfObject(PdfObject obj, final PdfObject parent) {
        if (obj == null) {
			return null;
		}
        if (!obj.isIndirect()) {
            PRIndirectReference ref = null;
            if (parent != null && (ref = parent.getIndRef()) != null && ref.getReader().isAppendable()) {
                switch (obj.type()) {
                    case PdfObject.NULL:
                        obj = new PdfNull();
                        break;
                    case PdfObject.BOOLEAN:
                        obj = new PdfBoolean(((PdfBoolean)obj).booleanValue());
                        break;
                    case PdfObject.NAME:
                        obj = new PdfName(obj.getBytes());
                        break;
                }
                obj.setIndRef(ref);
            }
            return obj;
        }
        return getPdfObject(obj);
    }

    /**
     * @param idx
     * @return a PdfObject
     */
    PdfObject getPdfObjectRelease(final int idx) {
        final PdfObject obj = getPdfObject(idx);
        releaseLastXrefPartial();
        return obj;
    }

    /**
     * @param idx
     * @return aPdfObject
     */
    PdfObject getPdfObject(final int idx) {
        try {
            this.lastXrefPartial = -1;
            if (idx < 0 || idx >= this.xrefObj.size()) {
				return null;
			}
            PdfObject obj = (PdfObject)this.xrefObj.get(idx);
            if (!this.partial || obj != null) {
				return obj;
			}
            if (idx * 2 >= this.xref.length) {
				return null;
			}
            obj = readSingleObject(idx);
            this.lastXrefPartial = -1;
            if (obj != null) {
				this.lastXrefPartial = idx;
			}
            return obj;
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }



    /**
     *
     */
    private void releaseLastXrefPartial() {
        if (this.partial && this.lastXrefPartial != -1) {
            this.xrefObj.set(this.lastXrefPartial, null);
            this.lastXrefPartial = -1;
        }
    }

    /**
     * @param obj
     */
    static void releaseLastXrefPartial(final PdfObject obj) {
        if (obj == null) {
			return;
		}
        if (!obj.isIndirect()) {
			return;
		}
        if (!(obj instanceof PRIndirectReference)) {
			return;
		}

        final PRIndirectReference ref = (PRIndirectReference)obj;
        final PdfReader reader = ref.getReader();
        if (reader.partial && reader.lastXrefPartial != -1 && reader.lastXrefPartial == ref.getNumber()) {
            reader.xrefObj.set(reader.lastXrefPartial, null);
        }
        reader.lastXrefPartial = -1;
    }

    private void setXrefPartialObject(final int idx, final PdfObject obj) {
        if (!this.partial || idx < 0) {
			return;
		}
        this.xrefObj.set(idx, obj);
    }



    protected void readPages() throws IOException {
        this.catalog = this.trailer.getAsDict(PdfName.ROOT);
        this.rootPages = this.catalog.getAsDict(PdfName.PAGES);
        this.pageRefs = new PageRefs(this);
    }

    private void readDocObjPartial() throws IOException {
        this.xrefObj = new ArrayList(this.xref.length / 2);
        this.xrefObj.addAll(Collections.nCopies(this.xref.length / 2, null));
        readDecryptedDocObj();
        if (this.objStmToOffset != null) {
            final int keys[] = this.objStmToOffset.getKeys();
            for (final int key : keys) {
                final int n = key;
                this.objStmToOffset.put(n, this.xref[n * 2]);
                this.xref[n * 2] = -1;
            }
        }
    }

    private PdfObject readSingleObject(final int k) throws IOException {
        this.strings.clear();
        final int k2 = k * 2;
        int pos = this.xref[k2];
        if (pos < 0) {
			return null;
		}
        if (this.xref[k2 + 1] > 0) {
			pos = this.objStmToOffset.get(this.xref[k2 + 1]);
		}
        if (pos == 0) {
			return null;
		}
        this.tokens.seek(pos);
        this.tokens.nextValidToken();
        if (this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
			this.tokens.throwError("Invalid object number.");
		}
        this.objNum = this.tokens.intValue();
        this.tokens.nextValidToken();
        if (this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
			this.tokens.throwError("Invalid generation number.");
		}
        this.objGen = this.tokens.intValue();
        this.tokens.nextValidToken();
        if (!this.tokens.getStringValue().equals("obj")) {
			this.tokens.throwError("Token 'obj' expected.");
		}
        PdfObject obj;
        try {
            obj = readPRObject();
            for (int j = 0; j < this.strings.size(); ++j) {
                final PdfString str = (PdfString)this.strings.get(j);
                str.decrypt(this);
            }
            if (obj.isStream()) {
                checkPRStreamLength((PRStream)obj);
            }
        }
        catch (final Exception e) {
            obj = null;
        }
        if (this.xref[k2 + 1] > 0) {
            obj = readOneObjStm((PRStream)obj, this.xref[k2]);
        }
        this.xrefObj.set(k, obj);
        return obj;
    }

    private PdfObject readOneObjStm(final PRStream stream, int idx) throws IOException {
        final int first = stream.getAsNumber(PdfName.FIRST).intValue();
        final byte b[] = getStreamBytes(stream, this.tokens.getFile());
        final PRTokeniser saveTokens = this.tokens;
        this.tokens = new PRTokeniser(b);
        try {
            int address = 0;
            boolean ok = true;
            ++idx;
            for (int k = 0; k < idx; ++k) {
                ok = this.tokens.nextToken();
                if (!ok) {
					break;
				}
                if (this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
                    ok = false;
                    break;
                }
                ok = this.tokens.nextToken();
                if (!ok) {
					break;
				}
                if (this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
                    ok = false;
                    break;
                }
                address = this.tokens.intValue() + first;
            }
            if (!ok) {
				throw new InvalidPdfException("Error reading ObjStm");
			}
            this.tokens.seek(address);
            return readPRObject();
        }
        finally {
            this.tokens = saveTokens;
        }
    }



    protected void readDocObj() throws IOException {
        final ArrayList streams = new ArrayList();
        this.xrefObj = new ArrayList(this.xref.length / 2);
        this.xrefObj.addAll(Collections.nCopies(this.xref.length / 2, null));
        for (int k = 2; k < this.xref.length; k += 2) {
            final int pos = this.xref[k];
            if (pos <= 0 || this.xref[k + 1] > 0) {
				continue;
			}
            this.tokens.seek(pos);
            this.tokens.nextValidToken();
            if (this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
				this.tokens.throwError("Invalid object number.");
			}
            this.objNum = this.tokens.intValue();
            this.tokens.nextValidToken();
            if (this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
				this.tokens.throwError("Invalid generation number.");
			}
            this.objGen = this.tokens.intValue();
            this.tokens.nextValidToken();
            if (!this.tokens.getStringValue().equals("obj")) {
				this.tokens.throwError("Token 'obj' expected.");
			}
            PdfObject obj;
            try {
                obj = readPRObject();
                if (obj.isStream()) {
                    streams.add(obj);
                }
            }
            catch (final Exception e) {
                obj = null;
            }
            this.xrefObj.set(k / 2, obj);
        }
        for (int k = 0; k < streams.size(); ++k) {
            checkPRStreamLength((PRStream)streams.get(k));
        }
        readDecryptedDocObj();
        if (this.objStmMark != null) {
            for (final Iterator i = this.objStmMark.entrySet().iterator(); i.hasNext();) {
                final Map.Entry entry = (Map.Entry)i.next();
                final int n = ((Integer)entry.getKey()).intValue();
                final IntHashtable h = (IntHashtable)entry.getValue();
                readObjStm((PRStream)this.xrefObj.get(n), h);
                this.xrefObj.set(n, null);
            }
            this.objStmMark = null;
        }
        this.xref = null;
    }

    private void checkPRStreamLength(final PRStream stream) throws IOException {
        final int fileLength = this.tokens.length();
        final int start = stream.getOffset();
        boolean calc = false;
        int streamLength = 0;
        final PdfObject obj = getPdfObjectRelease(stream.get(PdfName.LENGTH));
        if (obj != null && obj.type() == PdfObject.NUMBER) {
            streamLength = ((PdfNumber)obj).intValue();
            if (streamLength + start > fileLength - 20) {
				calc = true;
			} else {
                this.tokens.seek(start + streamLength);
                final String line = this.tokens.readString(20);
                if (!line.startsWith("\nendstream") &&
                !line.startsWith("\r\nendstream") &&
                !line.startsWith("\rendstream") &&
                !line.startsWith("endstream")) {
					calc = true;
				}
            }
        } else {
			calc = true;
		}
        if (calc) {
            final byte tline[] = new byte[16];
            this.tokens.seek(start);
            while (true) {
                int pos = this.tokens.getFilePointer();
                if (!this.tokens.readLineSegment(tline)) {
					break;
				}
                if (equalsn(tline, endstream)) {
                    streamLength = pos - start;
                    break;
                }
                if (equalsn(tline, endobj)) {
                    this.tokens.seek(pos - 16);
                    final String s = this.tokens.readString(16);
                    final int index = s.indexOf("endstream");
                    if (index >= 0) {
						pos = pos - 16 + index;
					}
                    streamLength = pos - start;
                    break;
                }
            }
        }
        stream.setLength(streamLength);
    }

    private void readObjStm(final PRStream stream, final IntHashtable map) throws IOException {
        final int first = stream.getAsNumber(PdfName.FIRST).intValue();
        final int n = stream.getAsNumber(PdfName.N).intValue();
        final byte b[] = getStreamBytes(stream, this.tokens.getFile());
        final PRTokeniser saveTokens = this.tokens;
        this.tokens = new PRTokeniser(b);
        try {
            final int address[] = new int[n];
            final int objNumber[] = new int[n];
            boolean ok = true;
            for (int k = 0; k < n; ++k) {
                ok = this.tokens.nextToken();
                if (!ok) {
					break;
				}
                if (this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
                    ok = false;
                    break;
                }
                objNumber[k] = this.tokens.intValue();
                ok = this.tokens.nextToken();
                if (!ok) {
					break;
				}
                if (this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
                    ok = false;
                    break;
                }
                address[k] = this.tokens.intValue() + first;
            }
            if (!ok) {
				throw new InvalidPdfException("Error reading ObjStm");
			}
            for (int k = 0; k < n; ++k) {
                if (map.containsKey(k)) {
                    this.tokens.seek(address[k]);
                    final PdfObject obj = readPRObject();
                    this.xrefObj.set(objNumber[k], obj);
                }
            }
        }
        finally {
            this.tokens = saveTokens;
        }
    }

    /**
     * Eliminates the reference to the object freeing the memory used by it and clearing
     * the xref entry.
     * @param obj the object. If it's an indirect reference it will be eliminated
     * @return the object or the already erased dereferenced object
     */
    static PdfObject killIndirect(final PdfObject obj) {
        if (obj == null || obj.isNull()) {
			return null;
		}
        final PdfObject ret = getPdfObjectRelease(obj);
        if (obj.isIndirect()) {
            final PRIndirectReference ref = (PRIndirectReference)obj;
            final PdfReader reader = ref.getReader();
            final int n = ref.getNumber();
            reader.xrefObj.set(n, null);
            if (reader.partial) {
				reader.xref[n * 2] = -1;
			}
        }
        return ret;
    }

    private void ensureXrefSize(final int size) {
        if (size == 0) {
			return;
		}
        if (this.xref == null) {
			this.xref = new int[size];
		} else {
            if (this.xref.length < size) {
                final int xref2[] = new int[size];
                System.arraycopy(this.xref, 0, xref2, 0, this.xref.length);
                this.xref = xref2;
            }
        }
    }

    private void readXref() throws IOException {
        this.hybridXref = false;
        this.newXrefType = false;
        this.tokens.seek(this.tokens.getStartxref());
        this.tokens.nextToken();
        if (!this.tokens.getStringValue().equals("startxref")) {
			throw new InvalidPdfException("startxref not found.");
		}
        this.tokens.nextToken();
        if (this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
			throw new InvalidPdfException("startxref is not followed by a number.");
		}
        final int startxref = this.tokens.intValue();
        this.lastXref = startxref;
        this.eofPos = this.tokens.getFilePointer();
        try {
            if (readXRefStream(startxref)) {
                this.newXrefType = true;
                return;
            }
        }
        catch (final Exception e) {}
        this.xref = null;
        this.tokens.seek(startxref);
        this.trailer = readXrefSection();
        PdfDictionary trailer2 = this.trailer;
        while (true) {
            final PdfNumber prev = (PdfNumber)trailer2.get(PdfName.PREV);
            if (prev == null) {
				break;
			}
            this.tokens.seek(prev.intValue());
            trailer2 = readXrefSection();
        }
    }

    private PdfDictionary readXrefSection() throws IOException {
        this.tokens.nextValidToken();
        if (!this.tokens.getStringValue().equals("xref")) {
			this.tokens.throwError("xref subsection not found");
		}
        int start = 0;
        int end = 0;
        int pos = 0;
        int gen = 0;
        while (true) {
            this.tokens.nextValidToken();
            if (this.tokens.getStringValue().equals("trailer")) {
				break;
			}
            if (this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
				this.tokens.throwError("Object number of the first object in this xref subsection not found");
			}
            start = this.tokens.intValue();
            this.tokens.nextValidToken();
            if (this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
				this.tokens.throwError("Number of entries in this xref subsection not found");
			}
            end = this.tokens.intValue() + start;
            if (start == 1) { // fix incorrect start number
                final int back = this.tokens.getFilePointer();
                this.tokens.nextValidToken();
                pos = this.tokens.intValue();
                this.tokens.nextValidToken();
                gen = this.tokens.intValue();
                if (pos == 0 && gen == PdfWriter.GENERATION_MAX) {
                    --start;
                    --end;
                }
                this.tokens.seek(back);
            }
            ensureXrefSize(end * 2);
            for (int k = start; k < end; ++k) {
                this.tokens.nextValidToken();
                pos = this.tokens.intValue();
                this.tokens.nextValidToken();
                gen = this.tokens.intValue();
                this.tokens.nextValidToken();
                final int p = k * 2;
                if (this.tokens.getStringValue().equals("n")) {
                    if (this.xref[p] == 0 && this.xref[p + 1] == 0) {
//                        if (pos == 0)
//                            tokens.throwError("File position 0 cross-reference entry in this xref subsection");
                        this.xref[p] = pos;
                    }
                }
                else if (this.tokens.getStringValue().equals("f")) {
                    if (this.xref[p] == 0 && this.xref[p + 1] == 0) {
						this.xref[p] = -1;
					}
                } else {
					this.tokens.throwError("Invalid cross-reference entry in this xref subsection");
				}
            }
        }
        final PdfDictionary trailer = (PdfDictionary)readPRObject();
        final PdfNumber xrefSize = (PdfNumber)trailer.get(PdfName.SIZE);
        ensureXrefSize(xrefSize.intValue() * 2);
        final PdfObject xrs = trailer.get(PdfName.XREFSTM);
        if (xrs != null && xrs.isNumber()) {
            final int loc = ((PdfNumber)xrs).intValue();
            try {
                readXRefStream(loc);
                this.newXrefType = true;
                this.hybridXref = true;
            }
            catch (final IOException e) {
                this.xref = null;
                throw e;
            }
        }
        return trailer;
    }

    private boolean readXRefStream(final int ptr) throws IOException {
        this.tokens.seek(ptr);
        int thisStream = 0;
        if (!this.tokens.nextToken()) {
			return false;
		}
        if (this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
			return false;
		}
        thisStream = this.tokens.intValue();
        if (!this.tokens.nextToken() || this.tokens.getTokenType() != PRTokeniser.TK_NUMBER) {
			return false;
		}
        if (!this.tokens.nextToken() || !this.tokens.getStringValue().equals("obj")) {
			return false;
		}
        final PdfObject object = readPRObject();
        PRStream stm = null;
        if (object.isStream()) {
            stm = (PRStream)object;
            if (!PdfName.XREF.equals(stm.get(PdfName.TYPE))) {
				return false;
			}
        } else {
			return false;
		}
        if (this.trailer == null) {
            this.trailer = new PdfDictionary();
            this.trailer.putAll(stm);
        }
        stm.setLength(((PdfNumber)stm.get(PdfName.LENGTH)).intValue());
        final int size = ((PdfNumber)stm.get(PdfName.SIZE)).intValue();
        PdfArray index;
        PdfObject obj = stm.get(PdfName.INDEX);
        if (obj == null) {
            index = new PdfArray();
            index.add(new int[]{0, size});
        } else {
			index = (PdfArray)obj;
		}
        final PdfArray w = (PdfArray)stm.get(PdfName.W);
        int prev = -1;
        obj = stm.get(PdfName.PREV);
        if (obj != null) {
			prev = ((PdfNumber)obj).intValue();
		}
        // Each xref pair is a position
        // type 0 -> -1, 0
        // type 1 -> offset, 0
        // type 2 -> index, obj num
        ensureXrefSize(size * 2);
        if (this.objStmMark == null && !this.partial) {
			this.objStmMark = new HashMap();
		}
        if (this.objStmToOffset == null && this.partial) {
			this.objStmToOffset = new IntHashtable();
		}
        final byte b[] = getStreamBytes(stm, this.tokens.getFile());
        int bptr = 0;
        final int wc[] = new int[3];
        for (int k = 0; k < 3; ++k) {
			wc[k] = w.getAsNumber(k).intValue();
		}
        for (int idx = 0; idx < index.size(); idx += 2) {
            int start = index.getAsNumber(idx).intValue();
            int length = index.getAsNumber(idx + 1).intValue();
            ensureXrefSize((start + length) * 2);
            while (length-- > 0) {
                int type = 1;
                if (wc[0] > 0) {
                    type = 0;
                    for (int k = 0; k < wc[0]; ++k) {
						type = (type << 8) + (b[bptr++] & 0xff);
					}
                }
                int field2 = 0;
                for (int k = 0; k < wc[1]; ++k) {
					field2 = (field2 << 8) + (b[bptr++] & 0xff);
				}
                int field3 = 0;
                for (int k = 0; k < wc[2]; ++k) {
					field3 = (field3 << 8) + (b[bptr++] & 0xff);
				}
                final int base = start * 2;
                if (this.xref[base] == 0 && this.xref[base + 1] == 0) {
                    switch (type) {
                        case 0:
                            this.xref[base] = -1;
                            break;
                        case 1:
                            this.xref[base] = field2;
                            break;
                        case 2:
                            this.xref[base] = field3;
                            this.xref[base + 1] = field2;
                            if (this.partial) {
                                this.objStmToOffset.put(field2, 0);
                            }
                            else {
                                final Integer on = new Integer(field2);
                                IntHashtable seq = (IntHashtable)this.objStmMark.get(on);
                                if (seq == null) {
                                    seq = new IntHashtable();
                                    seq.put(field3, 1);
                                    this.objStmMark.put(on, seq);
                                } else {
									seq.put(field3, 1);
								}
                            }
                            break;
                    }
                }
                ++start;
            }
        }
        thisStream *= 2;
        if (thisStream < this.xref.length) {
			this.xref[thisStream] = -1;
		}

        if (prev == -1) {
			return true;
		}
        return readXRefStream(prev);
    }

    protected void rebuildXref() throws IOException {
        this.hybridXref = false;
        this.newXrefType = false;
        this.tokens.seek(0);
        int xr[][] = new int[1024][];
        int top = 0;
        this.trailer = null;
        final byte line[] = new byte[64];
        for (;;) {
            int pos = this.tokens.getFilePointer();
            if (!this.tokens.readLineSegment(line)) {
				break;
			}
            if (line[0] == 't') {
                if (!PdfEncodings.convertToString(line, null).startsWith("trailer")) {
					continue;
				}
                this.tokens.seek(pos);
                this.tokens.nextToken();
                pos = this.tokens.getFilePointer();
                try {
                    final PdfDictionary dic = (PdfDictionary)readPRObject();
                    if (dic.get(PdfName.ROOT) != null) {
						this.trailer = dic;
					} else {
						this.tokens.seek(pos);
					}
                }
                catch (final Exception e) {
                    this.tokens.seek(pos);
                }
            }
            else if (line[0] >= '0' && line[0] <= '9') {
                final int obj[] = PRTokeniser.checkObjectStart(line);
                if (obj == null) {
					continue;
				}
                final int num = obj[0];
                final int gen = obj[1];
                if (num >= xr.length) {
                    final int newLength = num * 2;
                    final int xr2[][] = new int[newLength][];
                    System.arraycopy(xr, 0, xr2, 0, top);
                    xr = xr2;
                }
                if (num >= top) {
					top = num + 1;
				}
                if (xr[num] == null || gen >= xr[num][1]) {
                    obj[0] = pos;
                    xr[num] = obj;
                }
            }
        }
        if (this.trailer == null) {
			throw new InvalidPdfException("trailer not found.");
		}
        this.xref = new int[top * 2];
        for (int k = 0; k < top; ++k) {
            final int obj[] = xr[k];
            if (obj != null) {
				this.xref[k * 2] = obj[0];
			}
        }
    }

    private PdfDictionary readDictionary() throws IOException {
        final PdfDictionary dic = new PdfDictionary();
        while (true) {
            this.tokens.nextValidToken();
            if (this.tokens.getTokenType() == PRTokeniser.TK_END_DIC) {
				break;
			}
            if (this.tokens.getTokenType() != PRTokeniser.TK_NAME) {
				this.tokens.throwError("Dictionary key is not a name.");
			}
            final PdfName name = new PdfName(this.tokens.getStringValue(), false);
            final PdfObject obj = readPRObject();
            final int type = obj.type();
            if (-type == PRTokeniser.TK_END_DIC) {
				this.tokens.throwError("Unexpected '>>'");
			}
            if (-type == PRTokeniser.TK_END_ARRAY) {
				this.tokens.throwError("Unexpected ']'");
			}
            dic.put(name, obj);
        }
        return dic;
    }

    private PdfArray readArray() throws IOException {
        final PdfArray array = new PdfArray();
        while (true) {
            final PdfObject obj = readPRObject();
            final int type = obj.type();
            if (-type == PRTokeniser.TK_END_ARRAY) {
				break;
			}
            if (-type == PRTokeniser.TK_END_DIC) {
				this.tokens.throwError("Unexpected '>>'");
			}
            array.add(obj);
        }
        return array;
    }

    // Track how deeply nested the current object is, so
    // we know when to return an individual null or boolean, or
    // reuse one of the static ones.
    private int readDepth = 0;

    private PdfObject readPRObject() throws IOException {
        this.tokens.nextValidToken();
        final int type = this.tokens.getTokenType();
        switch (type) {
            case PRTokeniser.TK_START_DIC: {
                ++this.readDepth;
                final PdfDictionary dic = readDictionary();
                --this.readDepth;
                final int pos = this.tokens.getFilePointer();
                // be careful in the trailer. May not be a "next" token.
                boolean hasNext;
                do {
                    hasNext = this.tokens.nextToken();
                } while (hasNext && this.tokens.getTokenType() == PRTokeniser.TK_COMMENT);

                if (hasNext && this.tokens.getStringValue().equals("stream")) {
                    //skip whitespaces
                    int ch;
                    do {
                        ch = this.tokens.read();
                    } while (ch == 32 || ch == 9 || ch == 0 || ch == 12);
                    if (ch != '\n') {
						ch = this.tokens.read();
					}
                    if (ch != '\n') {
						this.tokens.backOnePosition(ch);
					}
                    final PRStream stream = new PRStream(this, this.tokens.getFilePointer());
                    stream.putAll(dic);
                    // crypto handling
                    stream.setObjNum(this.objNum, this.objGen);

                    return stream;
                }
                else {
                    this.tokens.seek(pos);
                    return dic;
                }
            }
            case PRTokeniser.TK_START_ARRAY: {
                ++this.readDepth;
                final PdfArray arr = readArray();
                --this.readDepth;
                return arr;
            }
            case PRTokeniser.TK_NUMBER:
                return new PdfNumber(this.tokens.getStringValue());
            case PRTokeniser.TK_STRING:
                final PdfString str = new PdfString(this.tokens.getStringValue(), null).setHexWriting(this.tokens.isHexString());
                // crypto handling
                str.setObjNum(this.objNum, this.objGen);
                if (this.strings != null) {
					this.strings.add(str);
				}

                return str;
            case PRTokeniser.TK_NAME: {
                final PdfName cachedName = (PdfName)PdfName.staticNames.get( this.tokens.getStringValue() );
                if (this.readDepth > 0 && cachedName != null) {
                    return cachedName;
                } else {
                    // an indirect name (how odd...), or a non-standard one
                    return new PdfName(this.tokens.getStringValue(), false);
                }
            }
            case PRTokeniser.TK_REF:
                final int num = this.tokens.getReference();
                final PRIndirectReference ref = new PRIndirectReference(this, num, this.tokens.getGeneration());
                return ref;
            default:
                final String sv = this.tokens.getStringValue();
                if ("null".equals(sv)) {
                    if (this.readDepth == 0) {
                        return new PdfNull();
                    } //else
                    return PdfNull.PDFNULL;
                }
                else if ("true".equals(sv)) {
                    if (this.readDepth == 0) {
                        return new PdfBoolean( true );
                    } //else
                    return PdfBoolean.PDFTRUE;
                }
                else if ("false".equals(sv)) {
                    if (this.readDepth == 0) {
                        return new PdfBoolean( false );
                    } //else
                    return PdfBoolean.PDFFALSE;
                }
                return new PdfLiteral(-type, this.tokens.getStringValue());
        }
    }

    /** Decodes a stream that has the FlateDecode filter.
     * @param in the input data
     * @return the decoded data
     */
    private static byte[] FlateDecode(final byte in[]) {
        final byte b[] = FlateDecode(in, true);
        if (b == null) {
			return FlateDecode(in, false);
		}
        return b;
    }

    /**
     * @param in
     * @param dicPar
     * @return a byte array
     */
    private static byte[] decodePredictor(final byte in[], final PdfObject dicPar) {
        if (dicPar == null || !dicPar.isDictionary()) {
			return in;
		}
        final PdfDictionary dic = (PdfDictionary)dicPar;
        PdfObject obj = getPdfObject(dic.get(PdfName.PREDICTOR));
        if (obj == null || !obj.isNumber()) {
			return in;
		}
        final int predictor = ((PdfNumber)obj).intValue();
        if (predictor < 10) {
			return in;
		}
        int width = 1;
        obj = getPdfObject(dic.get(PdfName.COLUMNS));
        if (obj != null && obj.isNumber()) {
			width = ((PdfNumber)obj).intValue();
		}
        int colors = 1;
        obj = getPdfObject(dic.get(PdfName.COLORS));
        if (obj != null && obj.isNumber()) {
			colors = ((PdfNumber)obj).intValue();
		}
        int bpc = 8;
        obj = getPdfObject(dic.get(PdfName.BITSPERCOMPONENT));
        if (obj != null && obj.isNumber()) {
			bpc = ((PdfNumber)obj).intValue();
		}
        final DataInputStream dataStream = new DataInputStream(new ByteArrayInputStream(in));
        final ByteArrayOutputStream fout = new ByteArrayOutputStream(in.length);
        final int bytesPerPixel = colors * bpc / 8;
        final int bytesPerRow = (colors*width*bpc + 7)/8;
        byte[] curr = new byte[bytesPerRow];
        byte[] prior = new byte[bytesPerRow];

        // Decode the (sub)image row-by-row
        while (true) {
            // Read the filter type byte and a row of data
            int filter = 0;
            try {
                filter = dataStream.read();
                if (filter < 0) {
                    return fout.toByteArray();
                }
                dataStream.readFully(curr, 0, bytesPerRow);
            } catch (final Exception e) {
                return fout.toByteArray();
            }

            switch (filter) {
                case 0: //PNG_FILTER_NONE
                    break;
                case 1: //PNG_FILTER_SUB
                    for (int i = bytesPerPixel; i < bytesPerRow; i++) {
                        curr[i] += curr[i - bytesPerPixel];
                    }
                    break;
                case 2: //PNG_FILTER_UP
                    for (int i = 0; i < bytesPerRow; i++) {
                        curr[i] += prior[i];
                    }
                    break;
                case 3: //PNG_FILTER_AVERAGE
                    for (int i = 0; i < bytesPerPixel; i++) {
                        curr[i] += prior[i] / 2;
                    }
                    for (int i = bytesPerPixel; i < bytesPerRow; i++) {
                        curr[i] += ((curr[i - bytesPerPixel] & 0xff) + (prior[i] & 0xff))/2;
                    }
                    break;
                case 4: //PNG_FILTER_PAETH
                    for (int i = 0; i < bytesPerPixel; i++) {
                        curr[i] += prior[i];
                    }

                    for (int i = bytesPerPixel; i < bytesPerRow; i++) {
                        final int a = curr[i - bytesPerPixel] & 0xff;
                        final int b = prior[i] & 0xff;
                        final int c = prior[i - bytesPerPixel] & 0xff;

                        final int p = a + b - c;
                        final int pa = Math.abs(p - a);
                        final int pb = Math.abs(p - b);
                        final int pc = Math.abs(p - c);

                        int ret;

                        if (pa <= pb && pa <= pc) {
                            ret = a;
                        } else if (pb <= pc) {
                            ret = b;
                        } else {
                            ret = c;
                        }
                        curr[i] += (byte)ret;
                    }
                    break;
                default:
                    // Error -- unknown filter type
                    throw new RuntimeException("PNG filter unknown.");
            }
            try {
                fout.write(curr);
            }
            catch (final IOException ioe) {
                // Never happens
            }

            // Swap curr and prior
            final byte[] tmp = prior;
            prior = curr;
            curr = tmp;
        }
    }

    /** A helper to FlateDecode.
     * @param in the input data
     * @param strict <CODE>true</CODE> to read a correct stream. <CODE>false</CODE>
     * to try to read a corrupted stream
     * @return the decoded data
     */
    public static byte[] FlateDecode(final byte in[], final boolean strict) {
        final ByteArrayInputStream stream = new ByteArrayInputStream(in);
        final InflaterInputStream zip = new InflaterInputStream(stream);
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        final byte b[] = new byte[strict ? 4092 : 1];
        try {
            int n;
            while ((n = zip.read(b)) >= 0) {
                out.write(b, 0, n);
            }
            zip.close();
            out.close();
            return out.toByteArray();
        }
        catch (final Exception e) {
            if (strict) {
				return null;
			}
            return out.toByteArray();
        }
    }

    /** Decodes a stream that has the ASCIIHexDecode filter.
     * @param in the input data
     * @return the decoded data
     */
    private static byte[] ASCIIHexDecode(final byte in[]) {
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        boolean first = true;
        int n1 = 0;
        for (final byte element : in) {
            final int ch = element & 0xff;
            if (ch == '>') {
				break;
			}
            if (PRTokeniser.isWhitespace(ch)) {
				continue;
			}
            final int n = PRTokeniser.getHex(ch);
            if (n == -1) {
				throw new RuntimeException("Illegal character in ASCIIHexDecode.");
			}
            if (first) {
				n1 = n;
			} else {
				out.write((byte)((n1 << 4) + n));
			}
            first = !first;
        }
        if (!first) {
			out.write((byte)(n1 << 4));
		}
        return out.toByteArray();
    }

    /** Decodes a stream that has the ASCII85Decode filter.
     * @param in the input data
     * @return the decoded data
     */
    private static byte[] ASCII85Decode(final byte in[]) {
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        int state = 0;
        final int chn[] = new int[5];
        for (final byte element : in) {
            final int ch = element & 0xff;
            if (ch == '~') {
				break;
			}
            if (PRTokeniser.isWhitespace(ch)) {
				continue;
			}
            if (ch == 'z' && state == 0) {
                out.write(0);
                out.write(0);
                out.write(0);
                out.write(0);
                continue;
            }
            if (ch < '!' || ch > 'u') {
				throw new RuntimeException("Illegal character in ASCII85Decode.");
			}
            chn[state] = ch - '!';
            ++state;
            if (state == 5) {
                state = 0;
                int r = 0;
                for (int j = 0; j < 5; ++j) {
					r = r * 85 + chn[j];
				}
                out.write((byte)(r >> 24));
                out.write((byte)(r >> 16));
                out.write((byte)(r >> 8));
                out.write((byte)r);
            }
        }
        int r = 0;
        // We'll ignore the next two lines for the sake of perpetuating broken PDFs
//        if (state == 1)
//            throw new RuntimeException("Illegal length in ASCII85Decode.");
        if (state == 2) {
            r = chn[0] * 85 * 85 * 85 * 85 + chn[1] * 85 * 85 * 85 + 85 * 85 * 85  + 85 * 85 + 85;
            out.write((byte)(r >> 24));
        }
        else if (state == 3) {
            r = chn[0] * 85 * 85 * 85 * 85 + chn[1] * 85 * 85 * 85  + chn[2] * 85 * 85 + 85 * 85 + 85;
            out.write((byte)(r >> 24));
            out.write((byte)(r >> 16));
        }
        else if (state == 4) {
            r = chn[0] * 85 * 85 * 85 * 85 + chn[1] * 85 * 85 * 85  + chn[2] * 85 * 85  + chn[3] * 85 + 85;
            out.write((byte)(r >> 24));
            out.write((byte)(r >> 16));
            out.write((byte)(r >> 8));
        }
        return out.toByteArray();
    }

    /** Decodes a stream that has the LZWDecode filter.
     * @param in the input data
     * @return the decoded data
     */
    private static byte[] LZWDecode(final byte in[]) {
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        final LZWDecoder lzw = new LZWDecoder();
        lzw.decode(in, out);
        return out.toByteArray();
    }

    /** Checks if the document had errors and was rebuilt.
     * @return true if rebuilt.
     *
     */
    public boolean isRebuilt() {
        return this.rebuilt;
    }

    /** Gets the dictionary that represents a page.
     * @param pageNum the page number. 1 is the first
     * @return the page dictionary
     */
    public PdfDictionary getPageN(final int pageNum) {
        final PdfDictionary dic = this.pageRefs.getPageN(pageNum);
        if (dic == null) {
			return null;
		}
        if (this.appendable) {
			dic.setIndRef(this.pageRefs.getPageOrigRef(pageNum));
		}
        return dic;
    }

    /**
     * @param pageNum
     * @return a Dictionary object
     */
    public PdfDictionary getPageNRelease(final int pageNum) {
        final PdfDictionary dic = getPageN(pageNum);
        this.pageRefs.releasePage(pageNum);
        return dic;
    }

    /**
     * @param pageNum
     */
    public void releasePage(final int pageNum) {
        this.pageRefs.releasePage(pageNum);
    }

    /**
     *
     */
    public void resetReleasePage() {
        this.pageRefs.resetReleasePage();
    }

    /** Gets the page reference to this page.
     * @param pageNum the page number. 1 is the first
     * @return the page reference
     */
    public PRIndirectReference getPageOrigRef(final int pageNum) {
        return this.pageRefs.getPageOrigRef(pageNum);
    }

    /** Gets the contents of the page.
     * @param pageNum the page number. 1 is the first
     * @param file the location of the PDF document
     * @throws IOException on error
     * @return the content
     */
    public byte[] getPageContent(final int pageNum, final RandomAccessFileOrArray file) throws IOException{
        final PdfDictionary page = getPageNRelease(pageNum);
        if (page == null) {
			return null;
		}
        final PdfObject contents = getPdfObjectRelease(page.get(PdfName.CONTENTS));
        if (contents == null) {
			return new byte[0];
		}
        ByteArrayOutputStream bout = null;
        if (contents.isStream()) {
            return getStreamBytes((PRStream)contents, file);
        }
        else if (contents.isArray()) {
            final PdfArray array = (PdfArray)contents;
            bout = new ByteArrayOutputStream();
            for (int k = 0; k < array.size(); ++k) {
                final PdfObject item = getPdfObjectRelease(array.getPdfObject(k));
                if (item == null || !item.isStream()) {
					continue;
				}
                final byte[] b = getStreamBytes((PRStream)item, file);
                bout.write(b);
                if (k != array.size() - 1) {
					bout.write('\n');
				}
            }
            return bout.toByteArray();
        } else {
			return new byte[0];
		}
    }



    protected void killXref(PdfObject obj) {
        if (obj == null) {
			return;
		}
        if (obj instanceof PdfIndirectReference && !obj.isIndirect()) {
			return;
		}
        switch (obj.type()) {
            case PdfObject.INDIRECT: {
                final int xr = ((PRIndirectReference)obj).getNumber();
                obj = (PdfObject)this.xrefObj.get(xr);
                this.xrefObj.set(xr, null);
                this.freeXref = xr;
                killXref(obj);
                break;
            }
            case PdfObject.ARRAY: {
                final PdfArray t = (PdfArray)obj;
                for (int i = 0; i < t.size(); ++i) {
					killXref(t.getPdfObject(i));
				}
                break;
            }
            case PdfObject.STREAM:
            case PdfObject.DICTIONARY: {
                final PdfDictionary dic = (PdfDictionary)obj;
                for (final Object element : dic.getKeys()) {
                    killXref(dic.get((PdfName)element));
                }
                break;
            }
        }
    }


    /** Sets the contents of the page.
     * @param content the new page content
     * @param pageNum the page number. 1 is the first
     * @since	2.1.3	(the method already existed without param compressionLevel)
     */
    private void setPageContent(final int pageNum, final byte content[], final int compressionLevel) {
        final PdfDictionary page = getPageN(pageNum);
        if (page == null) {
			return;
		}
        final PdfObject contents = page.get(PdfName.CONTENTS);
        this.freeXref = -1;
        killXref(contents);
        if (this.freeXref == -1) {
            this.xrefObj.add(null);
            this.freeXref = this.xrefObj.size() - 1;
        }
        page.put(PdfName.CONTENTS, new PRIndirectReference(this, this.freeXref));
        this.xrefObj.set(this.freeXref, new PRStream(this, content, compressionLevel));
    }

    /** Get the content from a stream applying the required filters.
     * @param stream the stream
     * @param file the location where the stream is
     * @throws IOException on error
     * @return the stream content
     */
    private static byte[] getStreamBytes(final PRStream stream, final RandomAccessFileOrArray file) throws IOException {
        final PdfObject filter = getPdfObjectRelease(stream.get(PdfName.FILTER));
        byte[] b = getStreamBytesRaw(stream, file);
        ArrayList filters = new ArrayList();
        if (filter != null) {
            if (filter.isName()) {
				filters.add(filter);
			} else if (filter.isArray()) {
				filters = ((PdfArray)filter).getArrayList();
			}
        }
        ArrayList dp = new ArrayList();
        PdfObject dpo = getPdfObjectRelease(stream.get(PdfName.DECODEPARMS));
        if (dpo == null || !dpo.isDictionary() && !dpo.isArray()) {
			dpo = getPdfObjectRelease(stream.get(PdfName.DP));
		}
        if (dpo != null) {
            if (dpo.isDictionary()) {
				dp.add(dpo);
			} else if (dpo.isArray()) {
				dp = ((PdfArray)dpo).getArrayList();
			}
        }
        String name;
        for (int j = 0; j < filters.size(); ++j) {
            name = ((PdfName)getPdfObjectRelease((PdfObject)filters.get(j))).toString();
            if (name.equals("/FlateDecode") || name.equals("/Fl")) {
                b = FlateDecode(b);
                PdfObject dicParam = null;
                if (j < dp.size()) {
                    dicParam = (PdfObject)dp.get(j);
                    b = decodePredictor(b, dicParam);
                }
            }
            else if (name.equals("/ASCIIHexDecode") || name.equals("/AHx")) {
				b = ASCIIHexDecode(b);
			} else if (name.equals("/ASCII85Decode") || name.equals("/A85")) {
				b = ASCII85Decode(b);
			} else if (name.equals("/LZWDecode")) {
                b = LZWDecode(b);
                PdfObject dicParam = null;
                if (j < dp.size()) {
                    dicParam = (PdfObject)dp.get(j);
                    b = decodePredictor(b, dicParam);
                }
            }
            else if (name.equals("/Crypt")) {
            } else {
				throw new UnsupportedPdfException("The filter " + name + " is not supported.");
			}
        }
        return b;
    }

    /** Get the content from a stream applying the required filters.
     * @param stream the stream
     * @throws IOException on error
     * @return the stream content
     */
    public static byte[] getStreamBytes(final PRStream stream) throws IOException {
        final RandomAccessFileOrArray rf = stream.getReader().getSafeFile();
        try {
            rf.reOpen();
            return getStreamBytes(stream, rf);
        }
        finally {
            try{rf.close();}catch(final Exception e){}
        }
    }

    /** Get the content from a stream as it is without applying any filter.
     * @param stream the stream
     * @param file the location where the stream is
     * @throws IOException on error
     * @return the stream content
     */
    private static byte[] getStreamBytesRaw(final PRStream stream, final RandomAccessFileOrArray file) throws IOException {
        final PdfReader reader = stream.getReader();
        byte b[];
        if (stream.getOffset() < 0) {
			b = stream.getBytes();
		} else {
            b = new byte[stream.getLength()];
            file.seek(stream.getOffset());
            file.readFully(b);
            final PdfEncryption decrypt = reader.getDecrypt();
            if (decrypt != null) {
                final PdfObject filter = getPdfObjectRelease(stream.get(PdfName.FILTER));
                ArrayList filters = new ArrayList();
                if (filter != null) {
                    if (filter.isName()) {
						filters.add(filter);
					} else if (filter.isArray()) {
						filters = ((PdfArray)filter).getArrayList();
					}
                }
                boolean skip = false;
                for (int k = 0; k < filters.size(); ++k) {
                    final PdfObject obj = getPdfObjectRelease((PdfObject)filters.get(k));
                    if (obj != null && obj.toString().equals("/Crypt")) {
                        skip = true;
                        break;
                    }
                }
                if (!skip) {
                    decrypt.setHashKey(stream.getObjNum(), stream.getObjGen());
                    b = decrypt.decryptByteArray(b);
                }
            }
        }
        return b;
    }

    /** Get the content from a stream as it is without applying any filter.
     * @param stream the stream
     * @throws IOException on error
     * @return the stream content
     */
    static byte[] getStreamBytesRaw(final PRStream stream) throws IOException {
        final RandomAccessFileOrArray rf = stream.getReader().getSafeFile();
        try {
            rf.reOpen();
            return getStreamBytesRaw(stream, rf);
        }
        finally {
            try{rf.close();}catch(final Exception e){}
        }
    }

    /** Eliminates shared streams if they exist. */
    private void eliminateSharedStreams() {
        if (!this.sharedStreams) {
			return;
		}
        this.sharedStreams = false;
        if (this.pageRefs.size() == 1) {
			return;
		}
        final ArrayList newRefs = new ArrayList();
        final ArrayList newStreams = new ArrayList();
        final IntHashtable visited = new IntHashtable();
        for (int k = 1; k <= this.pageRefs.size(); ++k) {
            final PdfDictionary page = this.pageRefs.getPageN(k);
            if (page == null) {
				continue;
			}
            final PdfObject contents = getPdfObject(page.get(PdfName.CONTENTS));
            if (contents == null) {
				continue;
			}
            if (contents.isStream()) {
                final PRIndirectReference ref = (PRIndirectReference)page.get(PdfName.CONTENTS);
                if (visited.containsKey(ref.getNumber())) {
                    // need to duplicate
                    newRefs.add(ref);
                    newStreams.add(new PRStream((PRStream)contents, null));
                } else {
					visited.put(ref.getNumber(), 1);
				}
            }
            else if (contents.isArray()) {
                final PdfArray array = (PdfArray)contents;
                for (int j = 0; j < array.size(); ++j) {
                    final PRIndirectReference ref = (PRIndirectReference)array.getPdfObject(j);
                    if (visited.containsKey(ref.getNumber())) {
                        // need to duplicate
                        newRefs.add(ref);
                        newStreams.add(new PRStream((PRStream)getPdfObject(ref), null));
                    } else {
						visited.put(ref.getNumber(), 1);
					}
                }
            }
        }
        if (newStreams.isEmpty()) {
			return;
		}
        for (int k = 0; k < newStreams.size(); ++k) {
            this.xrefObj.add(newStreams.get(k));
            final PRIndirectReference ref = (PRIndirectReference)newRefs.get(k);
            ref.setNumber(this.xrefObj.size() - 1, 0);
        }
    }

    /** Checks if the document was changed.
     * @return <CODE>true</CODE> if the document was changed,
     * <CODE>false</CODE> otherwise
     */
    public boolean isTampered() {
        return this.tampered;
    }

    /**
     * Sets the tampered state. A tampered PdfReader cannot be reused in PdfStamper.
     * @param tampered the tampered state
     */
    public void setTampered(final boolean tampered) {
        this.tampered = tampered;
        this.pageRefs.keepPages();
    }

    /** Gets the XML metadata.
     * @throws IOException on error
     * @return the XML metadata
     */
    public byte[] getMetadata() throws IOException {
        final PdfObject obj = getPdfObject(this.catalog.get(PdfName.METADATA));
        if (!(obj instanceof PRStream)) {
			return null;
		}
        final RandomAccessFileOrArray rf = getSafeFile();
        byte b[] = null;
        try {
            rf.reOpen();
            b = getStreamBytes((PRStream)obj, rf);
        }
        finally {
            try {
                rf.close();
            }
            catch (final Exception e) {
                // empty on purpose
            }
        }
        return b;
    }

    /**
     * Gets the byte address of the last xref table.
     * @return the byte address of the last xref table
     */
    public int getLastXref() {
        return this.lastXref;
    }

    /**
     * Gets the number of xref objects.
     * @return the number of xref objects
     */
    public int getXrefSize() {
        return this.xrefObj.size();
    }

    /**
     * Gets the byte address of the %%EOF marker.
     * @return the byte address of the %%EOF marker
     */
    public int getEofPos() {
        return this.eofPos;
    }

    /**
     * Gets the PDF version. Only the last version char is returned. For example
     * version 1.4 is returned as '4'.
     * @return the PDF version
     */
    public char getPdfVersion() {
        return this.pdfVersion;
    }

    /**
     * Returns <CODE>true</CODE> if the PDF is encrypted.
     * @return <CODE>true</CODE> if the PDF is encrypted
     */
    public boolean isEncrypted() {
        return this.encrypted;
    }

    /**
     * Gets the encryption permissions. It can be used directly in
     * <CODE>PdfWriter.setEncryption()</CODE>.
     * @return the encryption permissions
     */
    public int getPermissions() {
        return this.pValue;
    }



    /**
     * Gets the trailer dictionary
     * @return the trailer dictionary
     */
    public PdfDictionary getTrailer() {
        return this.trailer;
    }

    PdfEncryption getDecrypt() {
        return this.decrypt;
    }

    private static boolean equalsn(final byte a1[], final byte a2[]) {
        final int length = a2.length;
        for (int k = 0; k < length; ++k) {
            if (a1[k] != a2[k]) {
				return false;
			}
        }
        return true;
    }

    private static boolean existsName(final PdfDictionary dic, final PdfName key, final PdfName value) {
        final PdfObject type = getPdfObjectRelease(dic.get(key));
        if (type == null || !type.isName()) {
			return false;
		}
        final PdfName name = (PdfName)type;
        return name.equals(value);
    }

    private static String getFontName(final PdfDictionary dic) {
        if (dic == null) {
			return null;
		}
        final PdfObject type = getPdfObjectRelease(dic.get(PdfName.BASEFONT));
        if (type == null || !type.isName()) {
			return null;
		}
        return PdfName.decodeName(type.toString());
    }

    private static String getSubsetPrefix(final PdfDictionary dic) {
        if (dic == null) {
			return null;
		}
        final String s = getFontName(dic);
        if (s == null) {
			return null;
		}
        if (s.length() < 8 || s.charAt(6) != '+') {
			return null;
		}
        for (int k = 0; k < 6; ++k) {
            final char c = s.charAt(k);
            if (c < 'A' || c > 'Z') {
				return null;
			}
        }
        return s;
    }

    /** Finds all the font subsets and changes the prefixes to some
     * random values.
     * @return the number of font subsets altered
     */
    int shuffleSubsetNames() {
        int total = 0;
        for (int k = 1; k < this.xrefObj.size(); ++k) {
            final PdfObject obj = getPdfObjectRelease(k);
            if (obj == null || !obj.isDictionary()) {
				continue;
			}
            final PdfDictionary dic = (PdfDictionary)obj;
            if (!existsName(dic, PdfName.TYPE, PdfName.FONT)) {
				continue;
			}
            if (existsName(dic, PdfName.SUBTYPE, PdfName.TYPE1)
                || existsName(dic, PdfName.SUBTYPE, PdfName.MMTYPE1)
                || existsName(dic, PdfName.SUBTYPE, PdfName.TRUETYPE)) {
                final String s = getSubsetPrefix(dic);
                if (s == null) {
					continue;
				}
                final String ns = BaseFont.createSubsetPrefix() + s.substring(7);
                final PdfName newName = new PdfName(ns);
                dic.put(PdfName.BASEFONT, newName);
                setXrefPartialObject(k, dic);
                ++total;
                final PdfDictionary fd = dic.getAsDict(PdfName.FONTDESCRIPTOR);
                if (fd == null) {
					continue;
				}
                fd.put(PdfName.FONTNAME, newName);
            }
            else if (existsName(dic, PdfName.SUBTYPE, PdfName.TYPE0)) {
                final String s = getSubsetPrefix(dic);
                final PdfArray arr = dic.getAsArray(PdfName.DESCENDANTFONTS);
                if (arr == null) {
					continue;
				}
                if (arr.isEmpty()) {
					continue;
				}
                final PdfDictionary desc = arr.getAsDict(0);
                final String sde = getSubsetPrefix(desc);
                if (sde == null) {
					continue;
				}
                final String ns = BaseFont.createSubsetPrefix();
                if (s != null) {
					dic.put(PdfName.BASEFONT, new PdfName(ns + s.substring(7)));
				}
                setXrefPartialObject(k, dic);
                final PdfName newName = new PdfName(ns + sde.substring(7));
                desc.put(PdfName.BASEFONT, newName);
                ++total;
                final PdfDictionary fd = desc.getAsDict(PdfName.FONTDESCRIPTOR);
                if (fd == null) {
					continue;
				}
                fd.put(PdfName.FONTNAME, newName);
            }
        }
        return total;
    }



    private static PdfArray getNameArray(PdfObject obj) {
        if (obj == null) {
			return null;
		}
        obj = getPdfObjectRelease(obj);
        if (obj == null) {
			return null;
		}
        if (obj.isArray()) {
			return (PdfArray)obj;
		} else if (obj.isDictionary()) {
            final PdfObject arr2 = getPdfObjectRelease(((PdfDictionary)obj).get(PdfName.D));
            if (arr2 != null && arr2.isArray()) {
				return (PdfArray)arr2;
			}
        }
        return null;
    }

    /**
     * Gets all the named destinations as an <CODE>HashMap</CODE>. The key is the name
     * and the value is the destinations array.
     * @return gets all the named destinations
     */
    public HashMap getNamedDestination() {
    	return getNamedDestination(false);
    }

    /**
     * Gets all the named destinations as an <CODE>HashMap</CODE>. The key is the name
     * and the value is the destinations array.
     * @param	keepNames	true if you want the keys to be real PdfNames instead of Strings
     * @return gets all the named destinations
     * @since	2.1.6
     */
    private HashMap getNamedDestination(final boolean keepNames) {
        final HashMap names = getNamedDestinationFromNames(keepNames);
        names.putAll(getNamedDestinationFromStrings());
        return names;
    }

    /**
     * Gets the named destinations from the /Dests key in the catalog as an <CODE>HashMap</CODE>. The key is the name
     * and the value is the destinations array.
     * @return gets the named destinations
     */
    public HashMap getNamedDestinationFromNames() {
    	return getNamedDestinationFromNames(false);
    }

    /**
     * Gets the named destinations from the /Dests key in the catalog as an <CODE>HashMap</CODE>. The key is the name
     * and the value is the destinations array.
     * @param	keepNames	true if you want the keys to be real PdfNames instead of Strings
     * @return gets the named destinations
     * @since	2.1.6
     */
    private HashMap getNamedDestinationFromNames(final boolean keepNames) {
        final HashMap names = new HashMap();
        if (this.catalog.get(PdfName.DESTS) != null) {
            final PdfDictionary dic = (PdfDictionary)getPdfObjectRelease(this.catalog.get(PdfName.DESTS));
            if (dic == null) {
				return names;
			}
            final Set keys = dic.getKeys();
            for (final Iterator it = keys.iterator(); it.hasNext();) {
                final PdfName key = (PdfName)it.next();
                final PdfArray arr = getNameArray(dic.get(key));
                if (arr == null) {
					continue;
				}
                if (keepNames) {
                	names.put(key, arr);
                }
                else {
                	final String name = PdfName.decodeName(key.toString());
                	names.put(name, arr);
                }
            }
        }
        return names;
    }

    /**
     * Gets the named destinations from the /Names key in the catalog as an <CODE>HashMap</CODE>. The key is the name
     * and the value is the destinations array.
     * @return gets the named destinations
     */
    public HashMap getNamedDestinationFromStrings() {
        if (this.catalog.get(PdfName.NAMES) != null) {
            PdfDictionary dic = (PdfDictionary)getPdfObjectRelease(this.catalog.get(PdfName.NAMES));
            if (dic != null) {
                dic = (PdfDictionary)getPdfObjectRelease(dic.get(PdfName.DESTS));
                if (dic != null) {
                    final HashMap names = PdfNameTree.readTree(dic);
                    for (final Iterator it = names.entrySet().iterator(); it.hasNext();) {
                        final Map.Entry entry = (Map.Entry)it.next();
                        final PdfArray arr = getNameArray((PdfObject)entry.getValue());
                        if (arr != null) {
							entry.setValue(arr);
						} else {
							it.remove();
						}
                    }
                    return names;
                }
            }
        }
        return new HashMap();
    }

    private boolean replaceNamedDestination(PdfObject obj, final HashMap names) {
        obj = getPdfObject(obj);
        final int objIdx = this.lastXrefPartial;
        releaseLastXrefPartial();
        if (obj != null && obj.isDictionary()) {
            PdfObject ob2 = getPdfObjectRelease(((PdfDictionary)obj).get(PdfName.DEST));
            Object name = null;
            if (ob2 != null) {
                if (ob2.isName()) {
					name = ob2;
				} else if (ob2.isString()) {
					name = ob2.toString();
				}
                final PdfArray dest = (PdfArray)names.get(name);
                if (dest != null) {
                    ((PdfDictionary)obj).put(PdfName.DEST, dest);
                    setXrefPartialObject(objIdx, obj);
                    return true;
                }
            }
            else if ((ob2 = getPdfObject(((PdfDictionary)obj).get(PdfName.A))) != null) {
                final int obj2Idx = this.lastXrefPartial;
                releaseLastXrefPartial();
                final PdfDictionary dic = (PdfDictionary)ob2;
                final PdfName type = (PdfName)getPdfObjectRelease(dic.get(PdfName.S));
                if (PdfName.GOTO.equals(type)) {
                    final PdfObject ob3 = getPdfObjectRelease(dic.get(PdfName.D));
                    if (ob3 != null) {
                        if (ob3.isName()) {
							name = ob3;
						} else if (ob3.isString()) {
							name = ob3.toString();
						}
                    }
                    final PdfArray dest = (PdfArray)names.get(name);
                    if (dest != null) {
                        dic.put(PdfName.D, dest);
                        setXrefPartialObject(obj2Idx, ob2);
                        setXrefPartialObject(objIdx, obj);
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Removes all the fields from the document.
     */
    void removeFields() {
        this.pageRefs.resetReleasePage();
        for (int k = 1; k <= this.pageRefs.size(); ++k) {
            final PdfDictionary page = this.pageRefs.getPageN(k);
            final PdfArray annots = page.getAsArray(PdfName.ANNOTS);
            if (annots == null) {
                this.pageRefs.releasePage(k);
                continue;
            }
            for (int j = 0; j < annots.size(); ++j) {
                final PdfObject obj = getPdfObjectRelease(annots.getPdfObject(j));
                if (obj == null || !obj.isDictionary()) {
					continue;
				}
                final PdfDictionary annot = (PdfDictionary)obj;
                if (PdfName.WIDGET.equals(annot.get(PdfName.SUBTYPE))) {
					annots.remove(j--);
				}
            }
            if (annots.isEmpty()) {
				page.remove(PdfName.ANNOTS);
			} else {
				this.pageRefs.releasePage(k);
			}
        }
        this.catalog.remove(PdfName.ACROFORM);
        this.pageRefs.resetReleasePage();
    }





    private void iterateBookmarks(PdfObject outlineRef, final HashMap names) {
        while (outlineRef != null) {
            replaceNamedDestination(outlineRef, names);
            final PdfDictionary outline = (PdfDictionary)getPdfObjectRelease(outlineRef);
            final PdfObject first = outline.get(PdfName.FIRST);
            if (first != null) {
                iterateBookmarks(first, names);
            }
            outlineRef = outline.get(PdfName.NEXT);
        }
    }

    /** Replaces all the local named links with the actual destinations. */
    void consolidateNamedDestinations() {
        if (this.consolidateNamedDestinations) {
			return;
		}
        this.consolidateNamedDestinations = true;
        final HashMap names = getNamedDestination(true);
        if (names.isEmpty()) {
			return;
		}
        for (int k = 1; k <= this.pageRefs.size(); ++k) {
            final PdfDictionary page = this.pageRefs.getPageN(k);
            PdfObject annotsRef;
            final PdfArray annots = (PdfArray)getPdfObject(annotsRef = page.get(PdfName.ANNOTS));
            final int annotIdx = this.lastXrefPartial;
            releaseLastXrefPartial();
            if (annots == null) {
                this.pageRefs.releasePage(k);
                continue;
            }
            boolean commitAnnots = false;
            for (int an = 0; an < annots.size(); ++an) {
                final PdfObject objRef = annots.getPdfObject(an);
                if (replaceNamedDestination(objRef, names) && !objRef.isIndirect()) {
					commitAnnots = true;
				}
            }
            if (commitAnnots) {
				setXrefPartialObject(annotIdx,  annots);
			}
            if (!commitAnnots || annotsRef.isIndirect()) {
				this.pageRefs.releasePage(k);
			}
        }
        final PdfDictionary outlines = (PdfDictionary)getPdfObjectRelease(this.catalog.get(PdfName.OUTLINES));
        if (outlines == null) {
			return;
		}
        iterateBookmarks(outlines.get(PdfName.FIRST), names);
    }

    private static PdfDictionary duplicatePdfDictionary(final PdfDictionary original, PdfDictionary copy, final PdfReader newReader) {
        if (copy == null) {
			copy = new PdfDictionary();
		}
        for (final Object element : original.getKeys()) {
            final PdfName key = (PdfName)element;
            copy.put(key, duplicatePdfObject(original.get(key), newReader));
        }
        return copy;
    }

    private static PdfObject duplicatePdfObject(final PdfObject original, final PdfReader newReader) {
        if (original == null) {
			return null;
		}
        switch (original.type()) {
            case PdfObject.DICTIONARY: {
                return duplicatePdfDictionary((PdfDictionary)original, null, newReader);
            }
            case PdfObject.STREAM: {
                final PRStream org = (PRStream)original;
                final PRStream stream = new PRStream(org, null, newReader);
                duplicatePdfDictionary(org, stream, newReader);
                return stream;
            }
            case PdfObject.ARRAY: {
                final PdfArray arr = new PdfArray();
                for (final Iterator it = ((PdfArray)original).listIterator(); it.hasNext();) {
                    arr.add(duplicatePdfObject((PdfObject)it.next(), newReader));
                }
                return arr;
            }
            case PdfObject.INDIRECT: {
                final PRIndirectReference org = (PRIndirectReference)original;
                return new PRIndirectReference(newReader, org.getNumber(), org.getGeneration());
            }
            default:
                return original;
        }
    }

    /**
     * Closes the reader
     */
    void close() {
        if (!this.partial) {
			return;
		}
        try {
            this.tokens.close();
        }
        catch (final IOException e) {
            throw new ExceptionConverter(e);
        }
    }

    private void removeUnusedNode(PdfObject obj, final boolean hits[]) {
        final Stack state = new Stack();
        state.push(obj);
        while (!state.empty()) {
            final Object current = state.pop();
            if (current == null) {
				continue;
			}
            ArrayList ar = null;
            PdfDictionary dic = null;
            PdfName[] keys = null;
            Object[] objs = null;
            int idx = 0;
            if (current instanceof PdfObject) {
                obj = (PdfObject)current;
                switch (obj.type()) {
                    case PdfObject.DICTIONARY:
                    case PdfObject.STREAM:
                        dic = (PdfDictionary)obj;
                        keys = new PdfName[dic.size()];
                        dic.getKeys().toArray(keys);
                        break;
                    case PdfObject.ARRAY:
                         ar = ((PdfArray)obj).getArrayList();
                         break;
                    case PdfObject.INDIRECT:
                        final PRIndirectReference ref = (PRIndirectReference)obj;
                        final int num = ref.getNumber();
                        if (!hits[num]) {
                            hits[num] = true;
                            state.push(getPdfObjectRelease(ref));
                        }
                        continue;
                    default:
                        continue;
                }
            }
            else {
                objs = (Object[])current;
                if (objs[0] instanceof ArrayList) {
                    ar = (ArrayList)objs[0];
                    idx = ((Integer)objs[1]).intValue();
                }
                else {
                    keys = (PdfName[])objs[0];
                    dic = (PdfDictionary)objs[1];
                    idx = ((Integer)objs[2]).intValue();
                }
            }
            if (ar != null) {
                for (int k = idx; k < ar.size(); ++k) {
                    final PdfObject v = (PdfObject)ar.get(k);
                    if (v.isIndirect()) {
                        final int num = ((PRIndirectReference)v).getNumber();
                        if (num >= this.xrefObj.size() || !this.partial && this.xrefObj.get(num) == null) {
                            ar.set(k, PdfNull.PDFNULL);
                            continue;
                        }
                    }
                    if (objs == null) {
						state.push(new Object[]{ar, new Integer(k + 1)});
					} else {
                        objs[1] = new Integer(k + 1);
                        state.push(objs);
                    }
                    state.push(v);
                    break;
                }
            }
            else {
                for (int k = idx; k < keys.length; ++k) {
                    final PdfName key = keys[k];
                    final PdfObject v = dic.get(key);
                    if (v.isIndirect()) {
                        final int num = ((PRIndirectReference)v).getNumber();
                        if (num >= this.xrefObj.size() || !this.partial && this.xrefObj.get(num) == null) {
                            dic.put(key, PdfNull.PDFNULL);
                            continue;
                        }
                    }
                    if (objs == null) {
						state.push(new Object[]{keys, dic, new Integer(k + 1)});
					} else {
                        objs[2] = new Integer(k + 1);
                        state.push(objs);
                    }
                    state.push(v);
                    break;
                }
            }
        }
    }

    /** Removes all the unreachable objects.
     * @return the number of indirect objects removed
     */
    private int removeUnusedObjects() {
        final boolean hits[] = new boolean[this.xrefObj.size()];
        removeUnusedNode(this.trailer, hits);
        int total = 0;
        if (this.partial) {
            for (int k = 1; k < hits.length; ++k) {
                if (!hits[k]) {
                    this.xref[k * 2] = -1;
                    this.xref[k * 2 + 1] = 0;
                    this.xrefObj.set(k, null);
                    ++total;
                }
            }
        }
        else {
            for (int k = 1; k < hits.length; ++k) {
                if (!hits[k]) {
                    this.xrefObj.set(k, null);
                    ++total;
                }
            }
        }
        return total;
    }

    /** Gets a read-only version of <CODE>AcroFields</CODE>.
     * @return a read-only version of <CODE>AcroFields</CODE>
     */
    public AcroFields getAcroFields() {
        return new AcroFields(this, null);
    }

    /**
     * Gets the global document JavaScript.
     * @param file the document file
     * @throws IOException on error
     * @return the global document JavaScript
     */
    private String getJavaScript(final RandomAccessFileOrArray file) throws IOException {
        final PdfDictionary names = (PdfDictionary)getPdfObjectRelease(this.catalog.get(PdfName.NAMES));
        if (names == null) {
			return null;
		}
        final PdfDictionary js = (PdfDictionary)getPdfObjectRelease(names.get(PdfName.JAVASCRIPT));
        if (js == null) {
			return null;
		}
        final HashMap jscript = PdfNameTree.readTree(js);
        String sortedNames[] = new String[jscript.size()];
        sortedNames = (String[])jscript.keySet().toArray(sortedNames);
        Arrays.sort(sortedNames);
        final StringBuffer buf = new StringBuffer();
        for (final String sortedName : sortedNames) {
            final PdfDictionary j = (PdfDictionary)getPdfObjectRelease((PdfIndirectReference)jscript.get(sortedName));
            if (j == null) {
				continue;
			}
            final PdfObject obj = getPdfObjectRelease(j.get(PdfName.JS));
            if (obj != null) {
                if (obj.isString()) {
					buf.append(((PdfString)obj).toUnicodeString()).append('\n');
				} else if (obj.isStream()) {
                    final byte bytes[] = getStreamBytes((PRStream)obj, file);
                    if (bytes.length >= 2 && bytes[0] == (byte)254 && bytes[1] == (byte)255) {
						buf.append(PdfEncodings.convertToString(bytes, PdfObject.TEXT_UNICODE));
					} else {
						buf.append(PdfEncodings.convertToString(bytes, PdfObject.TEXT_PDFDOCENCODING));
					}
                    buf.append('\n');
                }
            }
        }
        return buf.toString();
    }

    /**
     * Gets the global document JavaScript.
     * @throws IOException on error
     * @return the global document JavaScript
     */
    public String getJavaScript() throws IOException {
        final RandomAccessFileOrArray rf = getSafeFile();
        try {
            rf.reOpen();
            return getJavaScript(rf);
        }
        finally {
            try{rf.close();}catch(final Exception e){}
        }
    }



    /**
     * Selects the pages to keep in the document. The pages are described as a
     * <CODE>List</CODE> of <CODE>Integer</CODE>. The page ordering can be changed but
     * no page repetitions are allowed. Note that it may be very slow in partial mode.
     * @param pagesToKeep the pages to keep in the document
     */
    void selectPages(final List pagesToKeep) {
        this.pageRefs.selectPages(pagesToKeep);
        removeUnusedObjects();
    }

    /** Sets the viewer preferences as the sum of several constants.
     * @param preferences the viewer preferences
     * @see PdfViewerPreferences#setViewerPreferences
     */
    @Override
	public void setViewerPreferences(final int preferences) {
    	this.viewerPreferences.setViewerPreferences(preferences);
        setViewerPreferences(this.viewerPreferences);
    }

    /** Adds a viewer preference
     * @param key a key for a viewer preference
     * @param value	a value for the viewer preference
     * @see PdfViewerPreferences#addViewerPreference
     */
    @Override
	public void addViewerPreference(final PdfName key, final PdfObject value) {
    	this.viewerPreferences.addViewerPreference(key, value);
        setViewerPreferences(this.viewerPreferences);
    }

    void setViewerPreferences(final PdfViewerPreferencesImp vp) {
    	vp.addToCatalog(this.catalog);
    }

    /**
     * Returns a bitset representing the PageMode and PageLayout viewer preferences.
     * Doesn't return any information about the ViewerPreferences dictionary.
     * @return an int that contains the Viewer Preferences.
     */
    public int getSimpleViewerPreferences() {
    	return PdfViewerPreferencesImp.getViewerPreferences(this.catalog).getPageLayoutAndMode();
    }

    /**
     * Getter for property appendable.
     * @return Value of property appendable.
     */
    public boolean isAppendable() {
        return this.appendable;
    }

    /**
     * Setter for property appendable.
     * @param appendable New value of property appendable.
     */
    public void setAppendable(final boolean appendable) {
        this.appendable = appendable;
        if (appendable) {
			getPdfObject(this.trailer.get(PdfName.ROOT));
		}
    }

    /**
     * Getter for property newXrefType.
     * @return Value of property newXrefType.
     */
    public boolean isNewXrefType() {
        return this.newXrefType;
    }

    /**
     * Getter for property fileLength.
     * @return Value of property fileLength.
     */
    public int getFileLength() {
        return this.fileLength;
    }

    /**
     * Getter for property hybridXref.
     * @return Value of property hybridXref.
     */
    public boolean isHybridXref() {
        return this.hybridXref;
    }

    private static class PageRefs {
        private final PdfReader reader;
        private IntHashtable refsp;
        private ArrayList refsn;
        private ArrayList pageInh;
        private int lastPageRead = -1;
        private int sizep;
        private boolean keepPages;

        private PageRefs(final PdfReader reader) throws IOException {
            this.reader = reader;
            if (reader.partial) {
                this.refsp = new IntHashtable();
                final PdfNumber npages = (PdfNumber)PdfReader.getPdfObjectRelease(reader.rootPages.get(PdfName.COUNT));
                this.sizep = npages.intValue();
            }
            else {
                readPages();
            }
        }

        private PageRefs(final PageRefs other, final PdfReader reader) {
            this.reader = reader;
            this.sizep = other.sizep;
            if (other.refsn != null) {
                this.refsn = new ArrayList(other.refsn);
                for (int k = 0; k < this.refsn.size(); ++k) {
                    this.refsn.set(k, duplicatePdfObject((PdfObject)this.refsn.get(k), reader));
                }
            } else {
				this.refsp = (IntHashtable)other.refsp.clone();
			}
        }

        private int size() {
            if (this.refsn != null) {
				return this.refsn.size();
			} else {
				return this.sizep;
			}
        }

        void readPages() throws IOException {
            if (this.refsn != null) {
				return;
			}
            this.refsp = null;
            this.refsn = new ArrayList();
            this.pageInh = new ArrayList();
            iteratePages((PRIndirectReference)this.reader.catalog.get(PdfName.PAGES));
            this.pageInh = null;
            this.reader.rootPages.put(PdfName.COUNT, new PdfNumber(this.refsn.size()));
        }



        /** Gets the dictionary that represents a page.
         * @param pageNum the page number. 1 is the first
         * @return the page dictionary
         */
        public PdfDictionary getPageN(final int pageNum) {
            final PRIndirectReference ref = getPageOrigRef(pageNum);
            return (PdfDictionary)PdfReader.getPdfObject(ref);
        }

        /**
         * @param pageNum
         * @return a dictionary object
         */
        public PdfDictionary getPageNRelease(final int pageNum) {
            final PdfDictionary page = getPageN(pageNum);
            releasePage(pageNum);
            return page;
        }



        /** Gets the page reference to this page.
         * @param pageNum the page number. 1 is the first
         * @return the page reference
         */
        public PRIndirectReference getPageOrigRef(int pageNum) {
            try {
                --pageNum;
                if (pageNum < 0 || pageNum >= size()) {
					return null;
				}
                if (this.refsn != null) {
					return (PRIndirectReference)this.refsn.get(pageNum);
				} else {
                    final int n = this.refsp.get(pageNum);
                    if (n == 0) {
                        final PRIndirectReference ref = getSinglePage(pageNum);
                        if (this.reader.lastXrefPartial == -1) {
							this.lastPageRead = -1;
						} else {
							this.lastPageRead = pageNum;
						}
                        this.reader.lastXrefPartial = -1;
                        this.refsp.put(pageNum, ref.getNumber());
                        if (this.keepPages) {
							this.lastPageRead = -1;
						}
                        return ref;
                    }
                    else {
                        if (this.lastPageRead != pageNum) {
							this.lastPageRead = -1;
						}
                        if (this.keepPages) {
							this.lastPageRead = -1;
						}
                        return new PRIndirectReference(this.reader, n);
                    }
                }
            }
            catch (final Exception e) {
                throw new ExceptionConverter(e);
            }
        }

        private void keepPages() {
            if (this.refsp == null || this.keepPages) {
				return;
			}
            this.keepPages = true;
            this.refsp.clear();
        }

        /**
         * @param pageNum
         */
        public void releasePage(int pageNum) {
            if (this.refsp == null) {
				return;
			}
            --pageNum;
            if (pageNum < 0 || pageNum >= size()) {
				return;
			}
            if (pageNum != this.lastPageRead) {
				return;
			}
            this.lastPageRead = -1;
            this.reader.lastXrefPartial = this.refsp.get(pageNum);
            this.reader.releaseLastXrefPartial();
            this.refsp.remove(pageNum);
        }

        /**
         *
         */
        public void resetReleasePage() {
            if (this.refsp == null) {
				return;
			}
            this.lastPageRead = -1;
        }



        private void pushPageAttributes(final PdfDictionary nodePages) {
            final PdfDictionary dic = new PdfDictionary();
            if (!this.pageInh.isEmpty()) {
                dic.putAll((PdfDictionary)this.pageInh.get(this.pageInh.size() - 1));
            }
            for (final PdfName pageInhCandidate : pageInhCandidates) {
                final PdfObject obj = nodePages.get(pageInhCandidate);
                if (obj != null) {
					dic.put(pageInhCandidate, obj);
				}
            }
            this.pageInh.add(dic);
        }

        private void popPageAttributes() {
            this.pageInh.remove(this.pageInh.size() - 1);
        }

        private void iteratePages(final PRIndirectReference rpage) throws IOException {
            final PdfDictionary page = (PdfDictionary)getPdfObject(rpage);
            final PdfArray kidsPR = page.getAsArray(PdfName.KIDS);
            if (kidsPR == null) {
                page.put(PdfName.TYPE, PdfName.PAGE);
                final PdfDictionary dic = (PdfDictionary)this.pageInh.get(this.pageInh.size() - 1);
                PdfName key;
                for (final Object element : dic.getKeys()) {
                    key = (PdfName)element;
                    if (page.get(key) == null) {
						page.put(key, dic.get(key));
					}
                }
                if (page.get(PdfName.MEDIABOX) == null) {
                    final PdfArray arr = new PdfArray(new float[]{0,0,PageSize.LETTER.getRight(),PageSize.LETTER.getTop()});
                    page.put(PdfName.MEDIABOX, arr);
                }
                this.refsn.add(rpage);
            }
            else {
                page.put(PdfName.TYPE, PdfName.PAGES);
                pushPageAttributes(page);
                for (int k = 0; k < kidsPR.size(); ++k){
                    final PdfObject obj = kidsPR.getPdfObject(k);
                    if (!obj.isIndirect()) {
                        while (k < kidsPR.size()) {
							kidsPR.remove(k);
						}
                        break;
                    }
                    iteratePages((PRIndirectReference)obj);
                }
                popPageAttributes();
            }
        }

        private PRIndirectReference getSinglePage(final int n) {
            final PdfDictionary acc = new PdfDictionary();
            PdfDictionary top = this.reader.rootPages;
            int base = 0;
            while (true) {
                for (final PdfName pageInhCandidate : pageInhCandidates) {
                    final PdfObject obj = top.get(pageInhCandidate);
                    if (obj != null) {
						acc.put(pageInhCandidate, obj);
					}
                }
                final PdfArray kids = (PdfArray)PdfReader.getPdfObjectRelease(top.get(PdfName.KIDS));
                for (final Iterator it = kids.listIterator(); it.hasNext();) {
                    final PRIndirectReference ref = (PRIndirectReference)it.next();
                    final PdfDictionary dic = (PdfDictionary)getPdfObject(ref);
                    final int last = this.reader.lastXrefPartial;
                    final PdfObject count = getPdfObjectRelease(dic.get(PdfName.COUNT));
                    this.reader.lastXrefPartial = last;
                    int acn = 1;
                    if (count != null && count.type() == PdfObject.NUMBER) {
						acn = ((PdfNumber)count).intValue();
					}
                    if (n < base + acn) {
                        if (count == null) {
                            dic.mergeDifferent(acc);
                            return ref;
                        }
                        this.reader.releaseLastXrefPartial();
                        top = dic;
                        break;
                    }
                    this.reader.releaseLastXrefPartial();
                    base += acn;
                }
            }
        }

        private void selectPages(final List pagesToKeep) {
            final IntHashtable pg = new IntHashtable();
            final ArrayList finalPages = new ArrayList();
            final int psize = size();
            for (final Iterator it = pagesToKeep.iterator(); it.hasNext();) {
                final Integer pi = (Integer)it.next();
                final int p = pi.intValue();
                if (p >= 1 && p <= psize && pg.put(p, 1) == 0) {
					finalPages.add(pi);
				}
            }
            if (this.reader.partial) {
                for (int k = 1; k <= psize; ++k) {
                    getPageOrigRef(k);
                    resetReleasePage();
                }
            }
            final PRIndirectReference parent = (PRIndirectReference)this.reader.catalog.get(PdfName.PAGES);
            final PdfDictionary topPages = (PdfDictionary)PdfReader.getPdfObject(parent);
            final ArrayList newPageRefs = new ArrayList(finalPages.size());
            final PdfArray kids = new PdfArray();
            for (int k = 0; k < finalPages.size(); ++k) {
                final int p = ((Integer)finalPages.get(k)).intValue();
                final PRIndirectReference pref = getPageOrigRef(p);
                resetReleasePage();
                kids.add(pref);
                newPageRefs.add(pref);
                getPageN(p).put(PdfName.PARENT, parent);
            }
            final AcroFields af = this.reader.getAcroFields();
            final boolean removeFields = af.getFields().size() > 0;
            for (int k = 1; k <= psize; ++k) {
                if (!pg.containsKey(k)) {
                    if (removeFields) {
						af.removeFieldsFromPage(k);
					}
                    final PRIndirectReference pref = getPageOrigRef(k);
                    final int nref = pref.getNumber();
                    this.reader.xrefObj.set(nref, null);
                    if (this.reader.partial) {
                        this.reader.xref[nref * 2] = -1;
                        this.reader.xref[nref * 2 + 1] = 0;
                    }
                }
            }
            topPages.put(PdfName.COUNT, new PdfNumber(finalPages.size()));
            topPages.put(PdfName.KIDS, kids);
            this.refsp = null;
            this.refsn = newPageRefs;
        }
    }

    PdfIndirectReference getCryptoRef() {
        if (this.cryptoRef == null) {
			return null;
		}
        return new PdfIndirectReference(0, this.cryptoRef.getNumber(), this.cryptoRef.getGeneration());
    }

    /**
     * Removes any usage rights that this PDF may have. Only Adobe can grant usage rights
     * and any PDF modification with iText will invalidate them. Invalidated usage rights may
     * confuse Acrobat and it's advisable to remove them altogether.
     */
    public void removeUsageRights() {
        final PdfDictionary perms = this.catalog.getAsDict(PdfName.PERMS);
        if (perms == null) {
			return;
		}
        perms.remove(PdfName.UR);
        perms.remove(PdfName.UR3);
        if (perms.size() == 0) {
			this.catalog.remove(PdfName.PERMS);
		}
    }

    /**
     * Gets the certification level for this document. The return values can be <code>PdfSignatureAppearance.NOT_CERTIFIED</code>,
     * <code>PdfSignatureAppearance.CERTIFIED_NO_CHANGES_ALLOWED</code>,
     * <code>PdfSignatureAppearance.CERTIFIED_FORM_FILLING</code> and
     * <code>PdfSignatureAppearance.CERTIFIED_FORM_FILLING_AND_ANNOTATIONS</code>.
     * <p>
     * No signature validation is made, use the methods available for that in <CODE>AcroFields</CODE>.
     * </p>
     * @return gets the certification level for this document
     */
    public int getCertificationLevel() {
        PdfDictionary dic = this.catalog.getAsDict(PdfName.PERMS);
        if (dic == null) {
			return PdfSignatureAppearance.NOT_CERTIFIED;
		}
        dic = dic.getAsDict(PdfName.DOCMDP);
        if (dic == null) {
			return PdfSignatureAppearance.NOT_CERTIFIED;
		}
        final PdfArray arr = dic.getAsArray(PdfName.REFERENCE);
        if (arr == null || arr.size() == 0) {
			return PdfSignatureAppearance.NOT_CERTIFIED;
		}
        dic = arr.getAsDict(0);
        if (dic == null) {
			return PdfSignatureAppearance.NOT_CERTIFIED;
		}
        dic = dic.getAsDict(PdfName.TRANSFORMPARAMS);
        if (dic == null) {
			return PdfSignatureAppearance.NOT_CERTIFIED;
		}
        final PdfNumber p = dic.getAsNumber(PdfName.P);
        if (p == null) {
			return PdfSignatureAppearance.NOT_CERTIFIED;
		}
        return p.intValue();
    }

    /**
     * Checks if the document was opened with the owner password so that the end application
     * can decide what level of access restrictions to apply. If the document is not encrypted
     * it will return <CODE>true</CODE>.
     * @return <CODE>true</CODE> if the document was opened with the owner password or if it's not encrypted,
     * <CODE>false</CODE> if the document was opened with the user password
     */
    public final boolean isOpenedWithFullPermissions() {
        return !this.encrypted || this.ownerPasswordUsed;
    }

    public int getCryptoMode() {
    	if (this.decrypt == null) {
			return -1;
		} else {
			return this.decrypt.getCryptoMode();
		}
    }

    public boolean isMetadataEncrypted() {
    	if (this.decrypt == null) {
			return false;
		} else {
			return this.decrypt.isMetadataEncrypted();
		}
    }


}