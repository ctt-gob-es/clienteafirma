/*
 * Copyright 2003, 2004 by Paulo Soares.
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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.SignatureException;
import java.security.cert.Certificate;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.lowagie.text.DocWriter;
import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.pdf.interfaces.PdfEncryptionSettings;
import com.lowagie.text.pdf.interfaces.PdfViewerPreferences;

/** Applies extra content to the pages of a PDF document.
 * This extra content can be all the objects allowed in PdfContentByte
 * including pages from other Pdfs. The original PDF will keep
 * all the interactive elements including bookmarks, links and form fields.
 * <p>
 * It is also possible to change the field values and to
 * flatten them. New fields can be added but not flattened.
 * @author Paulo Soares (psoares@consiste.pt)
 */
public class PdfStamper
	implements PdfViewerPreferences, PdfEncryptionSettings {
    /**
     * The writer
     */
    private final PdfStamperImp stamper;
    private HashMap moreInfo;
    private boolean hasSignature;
    private PdfSignatureAppearance sigApp;

    /** Starts the process of adding extra content to an existing PDF
     * document.
     * @param reader the original document. It cannot be reused
     * @param os the output stream
     * @throws DocumentException on error
     * @throws IOException on error
     */
    PdfStamper(final PdfReader reader, final OutputStream os, final Calendar globalDate) throws DocumentException, IOException {
        this.stamper = new PdfStamperImp(reader, os, '\0', false, globalDate);
    }



    /**
     * Starts the process of adding extra content to an existing PDF
     * document, possibly as a new revision.
     * @param reader the original document. It cannot be reused
     * @param os the output stream
     * @param pdfVersion the new pdf version or '\0' to keep the same version as the original
     * document
     * @param append if <CODE>true</CODE> appends the document changes as a new revision. This is
     * only useful for multiple signatures as nothing is gained in speed or memory
     * @throws DocumentException on error
     * @throws IOException on error
     */
    private PdfStamper(final PdfReader reader, final OutputStream os, final char pdfVersion, final boolean append, final Calendar globalDate) throws DocumentException, IOException {
        this.stamper = new PdfStamperImp(reader, os, pdfVersion, append, globalDate);
    }

    /** Gets the optional <CODE>String</CODE> map to add or change values in
     * the info dictionary.
     * @return the map or <CODE>null</CODE>
     *
     */
    public HashMap getMoreInfo() {
        return this.moreInfo;
    }

    /** An optional <CODE>String</CODE> map to add or change values in
     * the info dictionary. Entries with <CODE>null</CODE>
     * values delete the key in the original info dictionary
     * @param moreInfo additional entries to the info dictionary
     *
     */
    public void setMoreInfo(final HashMap moreInfo) {
        this.moreInfo = moreInfo;
    }





    /**
     * Gets the signing instance. The appearances and other parameters can the be set.
     * @return the signing instance
     */
    public PdfSignatureAppearance getSignatureAppearance() {
        return this.sigApp;
    }

    /**
     * Closes the document. No more content can be written after the
     * document is closed.
     * <p>
     * If closing a signed document with an external signature the closing must be done
     * in the <CODE>PdfSignatureAppearance</CODE> instance.
     * @throws DocumentException on error
     * @throws IOException on error
     */
    void close(final Calendar globalDate) throws DocumentException, IOException {
        if (!this.hasSignature) {
            this.stamper.close(this.moreInfo, globalDate);
            return;
        }
        this.sigApp.preClose(globalDate);
        final PdfSigGenericPKCS sig = this.sigApp.getSigStandard();
        final PdfLiteral lit = (PdfLiteral)sig.get(PdfName.CONTENTS);
        final int totalBuf = (lit.getPosLength() - 2) / 2;
        byte buf[] = new byte[8192];
        int n;
        final InputStream inp = this.sigApp.getRangeStream();
        try {
            while ((n = inp.read(buf)) > 0) {
                sig.getSigner().update(buf, 0, n);
            }
        }
        catch (final SignatureException se) {
            throw new ExceptionConverter(se);
        }
        buf = new byte[totalBuf];
        final byte[] bsig = sig.getSignerContents();
        System.arraycopy(bsig, 0, buf, 0, bsig.length);
        final PdfString str = new PdfString(buf);
        str.setHexWriting(true);
        final PdfDictionary dic = new PdfDictionary();
        dic.put(PdfName.CONTENTS, str);
        this.sigApp.close(dic);
        this.stamper.reader.close();
    }





    /** Checks if the content is automatically adjusted to compensate
     * the original page rotation.
     * @return the auto-rotation status
     */
    public boolean isRotateContents() {
        return this.stamper.isRotateContents();
    }

    /** Flags the content to be automatically adjusted to compensate
     * the original page rotation. The default is <CODE>true</CODE>.
     * @param rotateContents <CODE>true</CODE> to set auto-rotation, <CODE>false</CODE>
     * otherwise
     */
    public void setRotateContents(final boolean rotateContents) {
        this.stamper.setRotateContents(rotateContents);
    }

    /** Sets the encryption options for this document. The userPassword and the
     *  ownerPassword can be null or have zero length. In this case the ownerPassword
     *  is replaced by a random string. The open permissions for the document can be
     *  AllowPrinting, AllowModifyContents, AllowCopy, AllowModifyAnnotations,
     *  AllowFillIn, AllowScreenReaders, AllowAssembly and AllowDegradedPrinting.
     *  The permissions can be combined by ORing them.
     * @param userPassword the user password. Can be null or empty
     * @param ownerPassword the owner password. Can be null or empty
     * @param permissions the user permissions
     * @param strength128Bits <code>true</code> for 128 bit key length, <code>false</code> for 40 bit key length
     * @throws DocumentException if anything was already written to the output
     */
    void setEncryption(final byte userPassword[], final byte ownerPassword[], final int permissions, final boolean strength128Bits) throws DocumentException {
        if (this.stamper.isAppend()) {
			throw new DocumentException("Append mode does not support changing the encryption status."); //$NON-NLS-1$
		}
        if (this.stamper.isContentWritten()) {
			throw new DocumentException("Content was already written to the output."); //$NON-NLS-1$
		}
        this.stamper.setEncryption(userPassword, ownerPassword, permissions, strength128Bits ? PdfWriter.STANDARD_ENCRYPTION_128 : PdfWriter.STANDARD_ENCRYPTION_40);
    }

    /** Sets the encryption options for this document. The userPassword and the
     *  ownerPassword can be null or have zero length. In this case the ownerPassword
     *  is replaced by a random string. The open permissions for the document can be
     *  AllowPrinting, AllowModifyContents, AllowCopy, AllowModifyAnnotations,
     *  AllowFillIn, AllowScreenReaders, AllowAssembly and AllowDegradedPrinting.
     *  The permissions can be combined by ORing them.
     * @param userPassword the user password. Can be null or empty
     * @param ownerPassword the owner password. Can be null or empty
     * @param permissions the user permissions
     * @param encryptionType the type of encryption. It can be one of STANDARD_ENCRYPTION_40, STANDARD_ENCRYPTION_128 or ENCRYPTION_AES128.
     * Optionally DO_NOT_ENCRYPT_METADATA can be ored to output the metadata in cleartext
     * @throws DocumentException if the document is already open
     */
    @Override
	public void setEncryption(final byte userPassword[], final byte ownerPassword[], final int permissions, final int encryptionType) throws DocumentException {
        if (this.stamper.isAppend()) {
			throw new DocumentException("Append mode does not support changing the encryption status."); //$NON-NLS-1$
		}
        if (this.stamper.isContentWritten()) {
			throw new DocumentException("Content was already written to the output."); //$NON-NLS-1$
		}
        this.stamper.setEncryption(userPassword, ownerPassword, permissions, encryptionType);
    }

    /**
     * Sets the encryption options for this document. The userPassword and the
     *  ownerPassword can be null or have zero length. In this case the ownerPassword
     *  is replaced by a random string. The open permissions for the document can be
     *  AllowPrinting, AllowModifyContents, AllowCopy, AllowModifyAnnotations,
     *  AllowFillIn, AllowScreenReaders, AllowAssembly and AllowDegradedPrinting.
     *  The permissions can be combined by ORing them.
     * @param strength <code>true</code> for 128 bit key length, <code>false</code> for 40 bit key length
     * @param userPassword the user password. Can be null or empty
     * @param ownerPassword the owner password. Can be null or empty
     * @param permissions the user permissions
     * @throws DocumentException if anything was already written to the output
     */
    void setEncryption(final boolean strength, final String userPassword, final String ownerPassword, final int permissions) throws DocumentException {
        setEncryption(DocWriter.getISOBytes(userPassword), DocWriter.getISOBytes(ownerPassword), permissions, strength);
    }

    /**
     * Sets the encryption options for this document. The userPassword and the
     *  ownerPassword can be null or have zero length. In this case the ownerPassword
     *  is replaced by a random string. The open permissions for the document can be
     *  AllowPrinting, AllowModifyContents, AllowCopy, AllowModifyAnnotations,
     *  AllowFillIn, AllowScreenReaders, AllowAssembly and AllowDegradedPrinting.
     *  The permissions can be combined by ORing them.
     * @param encryptionType the type of encryption. It can be one of STANDARD_ENCRYPTION_40, STANDARD_ENCRYPTION_128 or ENCRYPTION_AES128.
     * Optionally DO_NOT_ENCRYPT_METADATA can be ored to output the metadata in cleartext
     * @param userPassword the user password. Can be null or empty
     * @param ownerPassword the owner password. Can be null or empty
     * @param permissions the user permissions
     * @throws DocumentException if anything was already written to the output
     */
    void setEncryption(final int encryptionType, final String userPassword, final String ownerPassword, final int permissions) throws DocumentException {
        setEncryption(DocWriter.getISOBytes(userPassword), DocWriter.getISOBytes(ownerPassword), permissions, encryptionType);
    }

    /**
     * Sets the certificate encryption options for this document. An array of one or more public certificates
     * must be provided together with an array of the same size for the permissions for each certificate.
     *  The open permissions for the document can be
     *  AllowPrinting, AllowModifyContents, AllowCopy, AllowModifyAnnotations,
     *  AllowFillIn, AllowScreenReaders, AllowAssembly and AllowDegradedPrinting.
     *  The permissions can be combined by ORing them.
     * Optionally DO_NOT_ENCRYPT_METADATA can be ored to output the metadata in cleartext
     * @param certs the public certificates to be used for the encryption
     * @param permissions the user permissions for each of the certificates
     * @param encryptionType the type of encryption. It can be one of STANDARD_ENCRYPTION_40, STANDARD_ENCRYPTION_128 or ENCRYPTION_AES128.
     * @throws DocumentException if the encryption was set too late
     */
     @Override
	public void setEncryption(final Certificate[] certs, final int[] permissions, final int encryptionType) throws DocumentException {
        if (this.stamper.isAppend()) {
			throw new DocumentException("Append mode does not support changing the encryption status."); //$NON-NLS-1$
		}
        if (this.stamper.isContentWritten()) {
			throw new DocumentException("Content was already written to the output."); //$NON-NLS-1$
		}
        this.stamper.setEncryption(certs, permissions, encryptionType);
     }



    /** Gets the underlying PdfWriter.
     * @return the underlying PdfWriter
     */
    public PdfWriter getWriter() {
        return this.stamper;
    }

    /** Gets the underlying PdfReader.
     * @return the underlying PdfReader
     */
    public PdfReader getReader() {
        return this.stamper.reader;
    }

    /** Gets the <CODE>AcroFields</CODE> object that allows to get and set field values
     * and to merge FDF forms.
     * @return the <CODE>AcroFields</CODE> object
     */
    public AcroFields getAcroFields() {
        return this.stamper.getAcroFields();
    }

    /** Determines if the fields are flattened on close. The fields added with
     * {@link #addAnnotation(PdfAnnotation,int)} will never be flattened.
     * @param flat <CODE>true</CODE> to flatten the fields, <CODE>false</CODE>
     * to keep the fields
     */
    public void setFormFlattening(final boolean flat) {
        this.stamper.setFormFlattening(flat);
    }

    /** Determines if the FreeText annotations are flattened on close.
     * @param flat <CODE>true</CODE> to flatten the FreeText annotations, <CODE>false</CODE>
     * (the default) to keep the FreeText annotations as active content.
     */
    public void setFreeTextFlattening(final boolean flat) {
    	this.stamper.setFreeTextFlattening(flat);
	}

    /**
     * Adds an annotation of form field in a specific page. This page number
     * can be overridden with {@link PdfAnnotation#setPlaceInPage(int)}.
     * @param annot the annotation
     * @param page the page
     */
    private void addAnnotation(final PdfAnnotation annot, final int page) {
        this.stamper.addAnnotation(annot, page);
    }





    /**
     * Sets the bookmarks. The list structure is defined in
     * {@link SimpleBookmark}.
     * @param outlines the bookmarks or <CODE>null</CODE> to remove any
     */
    public void setOutlines(final List outlines) {
        this.stamper.setOutlines(outlines);
    }









    /** Adds a file attachment at the document level. Existing attachments will be kept.
     * @param description the file description
     * @param fs the file specification
     */
    private void addFileAttachment(final String description, final PdfFileSpecification fs) throws IOException {
        this.stamper.addFileAttachment(description, fs);
    }





    /**
     * Sets the viewer preferences.
     * @param preferences the viewer preferences
     * @see PdfViewerPreferences#setViewerPreferences(int)
     */
    @Override
	public void setViewerPreferences(final int preferences) {
        this.stamper.setViewerPreferences(preferences);
    }

    /** Adds a viewer preference
     * @param key a key for a viewer preference
     * @param value the value for the viewer preference
     * @see PdfViewerPreferences#addViewerPreference
     */

    @Override
	public void addViewerPreference(final PdfName key, final PdfObject value) {
    	this.stamper.addViewerPreference(key, value);
    }

    /**
     * Sets the XMP metadata.
     * @param xmp
     * @see PdfWriter#setXmpMetadata(byte[])
     */
    public void setXmpMetadata(final byte[] xmp) {
        this.stamper.setXmpMetadata(xmp);
    }

    /**
     * Gets the 1.5 compression status.
     * @return <code>true</code> if the 1.5 compression is on
     */
    public boolean isFullCompression() {
        return this.stamper.isFullCompression();
    }

    /**
     * Sets the document's compression to the new 1.5 mode with object streams and xref
     * streams. It can be set at any time but once set it can't be unset.
     */
    public void setFullCompression() {
        if (this.stamper.isAppend()) {
			return;
		}
        this.stamper.setFullCompression();
    }







    /**
     * Applies a digital signature to a document, possibly as a new revision, making
     * possible multiple signatures. The returned PdfStamper
     * can be used normally as the signature is only applied when closing.
     * <p>
     * A possible use for adding a signature without invalidating an existing one is:
     * <p>
     * <pre>
     * KeyStore ks = KeyStore.getInstance("pkcs12");
     * ks.load(new FileInputStream("my_private_key.pfx"), "my_password".toCharArray());
     * String alias = (String)ks.aliases().nextElement();
     * PrivateKey key = (PrivateKey)ks.getKey(alias, "my_password".toCharArray());
     * Certificate[] chain = ks.getCertificateChain(alias);
     * PdfReader reader = new PdfReader("original.pdf");
     * FileOutputStream fout = new FileOutputStream("signed.pdf");
     * PdfStamper stp = PdfStamper.createSignature(reader, fout, '\0', new
     * File("/temp"), true);
     * PdfSignatureAppearance sap = stp.getSignatureAppearance();
     * sap.setCrypto(key, chain, null, PdfSignatureAppearance.WINCER_SIGNED);
     * sap.setReason("I'm the author");
     * sap.setLocation("Lisbon");
     * // comment next line to have an invisible signature
     * sap.setVisibleSignature(new Rectangle(100, 100, 200, 200), 1, null);
     * stp.close();
     * </pre>
     * @param reader the original document
     * @param os the output stream or <CODE>null</CODE> to keep the document in the temporary file
     * @param pdfVersion the new pdf version or '\0' to keep the same version as the original
     * document
     * @param tempFile location of the temporary file. If it's a directory a temporary file will be created there.
     *     If it's a file it will be used directly. The file will be deleted on exit unless <CODE>os</CODE> is null.
     *     In that case the document can be retrieved directly from the temporary file. If it's <CODE>null</CODE>
     *     no temporary file will be created and memory will be used
     * @param append if <CODE>true</CODE> the signature and all the other content will be added as a
     * new revision thus not invalidating existing signatures
     * @return a <CODE>PdfStamper</CODE>
     * @throws DocumentException on error
     * @throws IOException on error
     */
    public static PdfStamper createSignature(final PdfReader reader,
    										 final OutputStream os,
    										 final char pdfVersion,
    										 final File tempFile,
    										 final boolean append) throws DocumentException, IOException {
    	return createSignature(reader, os, pdfVersion, tempFile, append, null);
    }

    /**
     * Applies a digital signature to a document, possibly as a new revision, making
     * possible multiple signatures. The returned PdfStamper
     * can be used normally as the signature is only applied when closing.
     * <p>
     * A possible use for adding a signature without invalidating an existing one is:
     * <p>
     * <pre>
     * KeyStore ks = KeyStore.getInstance("pkcs12");
     * ks.load(new FileInputStream("my_private_key.pfx"), "my_password".toCharArray());
     * String alias = (String)ks.aliases().nextElement();
     * PrivateKey key = (PrivateKey)ks.getKey(alias, "my_password".toCharArray());
     * Certificate[] chain = ks.getCertificateChain(alias);
     * PdfReader reader = new PdfReader("original.pdf");
     * FileOutputStream fout = new FileOutputStream("signed.pdf");
     * PdfStamper stp = PdfStamper.createSignature(reader, fout, '\0', new
     * File("/temp"), true);
     * PdfSignatureAppearance sap = stp.getSignatureAppearance();
     * sap.setCrypto(key, chain, null, PdfSignatureAppearance.WINCER_SIGNED);
     * sap.setReason("I'm the author");
     * sap.setLocation("Lisbon");
     * // comment next line to have an invisible signature
     * sap.setVisibleSignature(new Rectangle(100, 100, 200, 200), 1, null);
     * stp.close();
     * </pre>
     * @param reader the original document
     * @param os the output stream or <CODE>null</CODE> to keep the document in the temporary file
     * @param pdfVersion the new pdf version or '\0' to keep the same version as the original
     * document
     * @param tempFile location of the temporary file. If it's a directory a temporary file will be created there.
     *     If it's a file it will be used directly. The file will be deleted on exit unless <CODE>os</CODE> is null.
     *     In that case the document can be retrieved directly from the temporary file. If it's <CODE>null</CODE>
     *     no temporary file will be created and memory will be used
     * @param append if <CODE>true</CODE> the signature and all the other content will be added as a
     * new revision thus not invalidating existing signatures
     * @return a <CODE>PdfStamper</CODE>
     * @throws DocumentException on error
     * @throws IOException on error
     */
    public static PdfStamper createSignature(final PdfReader reader,
    										 final OutputStream os,
    										 final char pdfVersion,
    										 File tempFile,
    										 final boolean append,
    										 final Calendar globalDate) throws DocumentException, IOException {
    	final Calendar gDate = globalDate!=null ? globalDate : new GregorianCalendar();
        PdfStamper stp;
        if (tempFile == null) {
            final ByteBuffer bout = new ByteBuffer();
            stp = new PdfStamper(reader, bout, pdfVersion, append, gDate);
            stp.sigApp = new PdfSignatureAppearance(stp.stamper, gDate);
            stp.sigApp.setSigout(bout);
        }
        else {
            if (tempFile.isDirectory()) {
				tempFile = File.createTempFile("pdf", null, tempFile); //$NON-NLS-1$
			}
            final FileOutputStream fout = new FileOutputStream(tempFile);
            stp = new PdfStamper(reader, fout, pdfVersion, append, globalDate);
            stp.sigApp = new PdfSignatureAppearance(stp.stamper, globalDate);
            stp.sigApp.setTempFile(tempFile);
        }
        stp.sigApp.setOriginalout(os);
        stp.sigApp.setStamper(stp);
        stp.hasSignature = true;
        final PdfDictionary catalog = reader.getCatalog();
        final PdfDictionary acroForm = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.ACROFORM), catalog);
        if (acroForm != null) {
            acroForm.remove(PdfName.NEEDAPPEARANCES);
            stp.stamper.markUsed(acroForm);
        }
        return stp;
    }





    /**
     * Gets the PdfLayer objects in an existing document as a Map
     * with the names/titles of the layers as keys.
     * @return	a Map with all the PdfLayers in the document (and the name/title of the layer as key)
     * @since	2.1.2
     */
    public Map getPdfLayers() {
    	return this.stamper.getPdfLayers();
    }
}