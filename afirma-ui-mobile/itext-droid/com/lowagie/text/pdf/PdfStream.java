/*
 * $Id: PdfStream.java 3735 2009-02-26 01:44:03Z xlv $
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;

import com.lowagie.text.DocWriter;
import com.lowagie.text.Document;
import com.lowagie.text.ExceptionConverter;

/**
 * <CODE>PdfStream</CODE> is the Pdf stream object.
 * <P>
 * A stream, like a string, is a sequence of characters. However, an application can
 * read a small portion of a stream at a time, while a string must be read in its entirety.
 * For this reason, objects with potentially large amounts of data, such as images and
 * page descriptions, are represented as streams.<BR>
 * A stream consists of a dictionary that describes a sequence of characters, followed by
 * the keyword <B>stream</B>, followed by zero or more lines of characters, followed by
 * the keyword <B>endstream</B>.<BR>
 * All streams must be <CODE>PdfIndirectObject</CODE>s. The stream dictionary must be a direct
 * object. The keyword <B>stream</B> that follows the stream dictionary should be followed by
 * a carriage return and linefeed or just a linefeed.<BR>
 * Remark: In this version only the FLATEDECODE-filter is supported.<BR>
 * This object is described in the 'Portable Document Format Reference Manual version 1.7'
 * section 3.2.7 (page 60-63).<BR>
 *
 * @see		PdfObject
 * @see		PdfDictionary
 */

public class PdfStream extends PdfDictionary {

    // membervariables

	/**
	 * A possible compression level.
	 * @since	2.1.3
	 */
	public static final int DEFAULT_COMPRESSION = -1;
	/**
	 * A possible compression level.
	 * @since	2.1.3
	 */
	public static final int NO_COMPRESSION = 0;

	/**
	 * A possible compression level.
	 * @since	2.1.3
	 */
	public static final int BEST_COMPRESSION = 9;


/** is the stream compressed? */
    protected boolean compressed = false;
    /**
     * The level of compression.
     * @since	2.1.3
     */
    protected int compressionLevel = NO_COMPRESSION;

    protected ByteArrayOutputStream streamBytes = null;
    protected InputStream inputStream;
    private PdfIndirectReference ref;
    protected int inputStreamLength = -1;
    private PdfWriter writer;
    protected int rawLength;

    static final byte STARTSTREAM[] = DocWriter.getISOBytes("stream\n");
    static final byte ENDSTREAM[] = DocWriter.getISOBytes("\nendstream");


    // constructors

/**
 * Constructs a <CODE>PdfStream</CODE>-object.
 *
 * @param		bytes			content of the new <CODE>PdfObject</CODE> as an array of <CODE>byte</CODE>.
 */

    PdfStream(final byte[] bytes) {
        super();
        this.type = STREAM;
        this.bytes = bytes;
        this.rawLength = bytes.length;
        put(PdfName.LENGTH, new PdfNumber(bytes.length));
    }

    /**
     * Creates an efficient stream. No temporary array is ever created. The <CODE>InputStream</CODE>
     * is totally consumed but is not closed. The general usage is:
     * <p>
     * <pre>
     * InputStream in = ...;
     * PdfStream stream = new PdfStream(in, writer);
     * stream.flateCompress();
     * writer.addToBody(stream);
     * stream.writeLength();
     * in.close();
     * </pre>
     * @param inputStream the data to write to this stream
     * @param writer the <CODE>PdfWriter</CODE> for this stream
     */
    PdfStream(final InputStream inputStream, final PdfWriter writer) {
        super();
        this.type = STREAM;
        this.inputStream = inputStream;
        this.writer = writer;
        this.ref = writer.getPdfIndirectReference();
        put(PdfName.LENGTH, this.ref);
    }

/**
 * Constructs a <CODE>PdfStream</CODE>-object.
 */

    protected PdfStream() {
        super();
        this.type = STREAM;
    }

    /**
     * Writes the stream length to the <CODE>PdfWriter</CODE>.
     * <p>
     * This method must be called and can only be called if the constructor {@link #PdfStream(InputStream,PdfWriter)}
     * is used to create the stream.
     * @throws IOException on error
     * @see #PdfStream(InputStream,PdfWriter)
     */
    void writeLength() throws IOException {
        if (this.inputStream == null) {
			throw new UnsupportedOperationException("writeLength() can only be called in a contructed PdfStream(InputStream,PdfWriter).");
		}
        if (this.inputStreamLength == -1) {
			throw new IOException("writeLength() can only be called after output of the stream body.");
		}
        this.writer.addToBody(new PdfNumber(this.inputStreamLength), this.ref, false);
    }

    /**
     * Gets the raw length of the stream.
     * @return the raw length of the stream
     */
    public int getRawLength() {
        return this.rawLength;
    }



    /**
     * Compresses the stream.
	 * @param compressionLevel the compression level (0 = best speed, 9 = best compression, -1 is default)
	 * @since	2.1.3
     */
    void flateCompress(final int compressionLevel) {
        if (!Document.compress) {
			return;
		}
        // check if the flateCompress-method has already been
        if (this.compressed) {
            return;
        }
    	this.compressionLevel = compressionLevel;
        if (this.inputStream != null) {
            this.compressed = true;
            return;
        }
        // check if a filter already exists
        final PdfObject filter = PdfReader.getPdfObject(get(PdfName.FILTER));
        if (filter != null) {
            if (filter.isName()) {
                if (PdfName.FLATEDECODE.equals(filter)) {
					return;
				}
            }
            else if (filter.isArray()) {
                if (((PdfArray) filter).contains(PdfName.FLATEDECODE)) {
					return;
				}
            }
            else {
                throw new RuntimeException("Stream could not be compressed: filter is not a name or array.");
            }
        }
        try {
            // compress
            final ByteArrayOutputStream stream = new ByteArrayOutputStream();
            final Deflater deflater = new Deflater(compressionLevel);
            final DeflaterOutputStream zip = new DeflaterOutputStream(stream, deflater);
            if (this.streamBytes != null) {
				this.streamBytes.writeTo(zip);
			} else {
				zip.write(this.bytes);
			}
            zip.close();
            deflater.end();
            // update the object
            this.streamBytes = stream;
            this.bytes = null;
            put(PdfName.LENGTH, new PdfNumber(this.streamBytes.size()));
            if (filter == null) {
                put(PdfName.FILTER, PdfName.FLATEDECODE);
            }
            else {
                final PdfArray filters = new PdfArray(filter);
                filters.add(PdfName.FLATEDECODE);
                put(PdfName.FILTER, filters);
            }
            this.compressed = true;
        }
        catch(final IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }

//    public int getStreamLength(PdfWriter writer) {
//        if (dicBytes == null)
//            toPdf(writer);
//        if (streamBytes != null)
//            return streamBytes.size() + dicBytes.length + SIZESTREAM;
//        else
//            return bytes.length + dicBytes.length + SIZESTREAM;
//    }

    protected void superToPdf(final PdfWriter writer, final OutputStream os) throws IOException {
        super.toPdf(writer, os);
    }

    /**
     * @see com.lowagie.text.pdf.PdfDictionary#toPdf(com.lowagie.text.pdf.PdfWriter, java.io.OutputStream)
     */
    @Override
	public void toPdf(final PdfWriter writer, final OutputStream os) throws IOException {
        if (this.inputStream != null && this.compressed) {
			put(PdfName.FILTER, PdfName.FLATEDECODE);
		}
        PdfEncryption crypto = null;
        if (writer != null) {
			crypto = writer.getEncryption();
		}
        if (crypto != null) {
            final PdfObject filter = get(PdfName.FILTER);
            if (filter != null) {
                if (PdfName.CRYPT.equals(filter)) {
					crypto = null;
				} else if (filter.isArray()) {
                    final PdfArray a = (PdfArray)filter;
                    if (!a.isEmpty() && PdfName.CRYPT.equals(a.getPdfObject(0))) {
						crypto = null;
					}
                }
            }
        }
        final PdfObject nn = get(PdfName.LENGTH);
        if (crypto != null && nn != null && nn.isNumber()) {
            final int sz = ((PdfNumber)nn).intValue();
            put(PdfName.LENGTH, new PdfNumber(crypto.calculateStreamSize(sz)));
            superToPdf(writer, os);
            put(PdfName.LENGTH, nn);
        } else {
			superToPdf(writer, os);
		}
        os.write(STARTSTREAM);
        if (this.inputStream != null) {
            this.rawLength = 0;
            DeflaterOutputStream def = null;
            final OutputStreamCounter osc = new OutputStreamCounter(os);
            OutputStreamEncryption ose = null;
            OutputStream fout = osc;
            if (crypto != null && !crypto.isEmbeddedFilesOnly()) {
				fout = ose = crypto.getEncryptionStream(fout);
			}
            Deflater deflater = null;
            if (this.compressed) {
                deflater = new Deflater(this.compressionLevel);
                fout = def = new DeflaterOutputStream(fout, deflater, 0x8000);
            }

            final byte buf[] = new byte[4192];
            while (true) {
                final int n = this.inputStream.read(buf);
                if (n <= 0) {
					break;
				}
                fout.write(buf, 0, n);
                this.rawLength += n;
            }
            if (def != null) {
                def.finish();
                deflater.end();
            }
            if (ose != null) {
				ose.finish();
			}
            this.inputStreamLength = osc.getCounter();
        }
        else {
            if (crypto != null && !crypto.isEmbeddedFilesOnly()) {
                byte b[];
                if (this.streamBytes != null) {
                    b = crypto.encryptByteArray(this.streamBytes.toByteArray());
                }
                else {
                    b = crypto.encryptByteArray(this.bytes);
                }
                os.write(b);
            }
            else {
                if (this.streamBytes != null) {
					this.streamBytes.writeTo(os);
				} else {
					os.write(this.bytes);
				}
            }
        }
        os.write(ENDSTREAM);
    }

    /**
     * Writes the data content to an <CODE>OutputStream</CODE>.
     * @param os the destination to write to
     * @throws IOException on error
     */
    void writeContent(final OutputStream os) throws IOException {
        if (this.streamBytes != null) {
			this.streamBytes.writeTo(os);
		} else if (this.bytes != null) {
			os.write(this.bytes);
		}
    }

    /**
     * @see com.lowagie.text.pdf.PdfObject#toString()
     */
    @Override
	public String toString() {
    	if (get(PdfName.TYPE) == null) {
			return "Stream";
		}
    	return "Stream of type: " + get(PdfName.TYPE);
    }
}
