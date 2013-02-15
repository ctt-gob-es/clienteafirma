/*
 * $Id: PRStream.java 3633 2008-12-23 18:42:06Z xlv $
 *
 * Copyright 2001, 2002 by Paulo Soares.
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
import java.io.OutputStream;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;

import com.lowagie.text.Document;
import com.lowagie.text.ExceptionConverter;

public class PRStream extends PdfStream {

    private PdfReader reader;
    private int offset;
    private int length;

    //added by ujihara for decryption
    private int objNum = 0;
    private int objGen = 0;

    PRStream(final PRStream stream, final PdfDictionary newDic) {
        this.reader = stream.reader;
        this.offset = stream.offset;
        this.length = stream.length;
        this.compressed = stream.compressed;
        this.compressionLevel = stream.compressionLevel;
        this.streamBytes = stream.streamBytes;
        this.bytes = stream.bytes;
        this.objNum = stream.objNum;
        this.objGen = stream.objGen;
        if (newDic != null) {
			putAll(newDic);
		} else {
			this.hashMap.putAll(stream.hashMap);
		}
    }

    PRStream(final PRStream stream, final PdfDictionary newDic, final PdfReader reader) {
        this(stream, newDic);
        this.reader = reader;
    }

    PRStream(final PdfReader reader, final int offset) {
        this.reader = reader;
        this.offset = offset;
    }



    /**
     * Creates a new PDF stream object that will replace a stream
     * in a existing PDF file.
     * @param	reader	the reader that holds the existing PDF
     * @param	conts	the new content
     * @param	compressionLevel	the compression level for the content
     * @since	2.1.3 (replacing the existing constructor without param compressionLevel)
     */
    PRStream(final PdfReader reader, final byte[] conts, final int compressionLevel) {
        this.reader = reader;
        this.offset = -1;
        if (Document.compress) {
            try {
                final ByteArrayOutputStream stream = new ByteArrayOutputStream();
                final Deflater deflater = new Deflater(compressionLevel);
                final DeflaterOutputStream zip = new DeflaterOutputStream(stream, deflater);
                zip.write(conts);
                zip.close();
                deflater.end();
                this.bytes = stream.toByteArray();
            }
            catch (final IOException ioe) {
                throw new ExceptionConverter(ioe);
            }
            put(PdfName.FILTER, PdfName.FLATEDECODE);
        } else {
			this.bytes = conts;
		}
        setLength(this.bytes.length);
    }

    /**
     * Sets the data associated with the stream, either compressed or
     * uncompressed. Note that the data will never be compressed if
     * Document.compress is set to false.
     *
     * @param data raw data, decrypted and uncompressed.
     * @param compress true if you want the stream to be compressed.
     * @since	iText 2.1.1
     */
    private void setData(final byte[] data, final boolean compress) {
    	setData(data, compress, DEFAULT_COMPRESSION);
    }

    /**
     * Sets the data associated with the stream, either compressed or
     * uncompressed. Note that the data will never be compressed if
     * Document.compress is set to false.
     *
     * @param data raw data, decrypted and uncompressed.
     * @param compress true if you want the stream to be compressed.
     * @param compressionLevel	a value between -1 and 9 (ignored if compress == false)
     * @since	iText 2.1.3
     */
    private void setData(final byte[] data, final boolean compress, final int compressionLevel) {
        remove(PdfName.FILTER);
        this.offset = -1;
        if (Document.compress && compress) {
            try {
                final ByteArrayOutputStream stream = new ByteArrayOutputStream();
                final Deflater deflater = new Deflater(compressionLevel);
                final DeflaterOutputStream zip = new DeflaterOutputStream(stream, deflater);
                zip.write(data);
                zip.close();
                deflater.end();
                this.bytes = stream.toByteArray();
                this.compressionLevel = compressionLevel;
            }
            catch (final IOException ioe) {
                throw new ExceptionConverter(ioe);
            }
            put(PdfName.FILTER, PdfName.FLATEDECODE);
        } else {
			this.bytes = data;
		}
        setLength(this.bytes.length);
    }

    /**Sets the data associated with the stream
     * @param data raw data, decrypted and uncompressed.
     */
    public void setData(final byte[] data) {
        setData(data, true);
    }

    public void setLength(final int length) {
        this.length = length;
        put(PdfName.LENGTH, new PdfNumber(length));
    }

    public int getOffset() {
        return this.offset;
    }

    public int getLength() {
        return this.length;
    }

    public PdfReader getReader() {
        return this.reader;
    }

    @Override
	public byte[] getBytes() {
        return this.bytes;
    }

    void setObjNum(final int objNum, final int objGen) {
        this.objNum = objNum;
        this.objGen = objGen;
    }

    int getObjNum() {
        return this.objNum;
    }

    int getObjGen() {
        return this.objGen;
    }

    @Override
	public void toPdf(final PdfWriter writer, final OutputStream os) throws IOException {
        byte[] b = PdfReader.getStreamBytesRaw(this);
        PdfEncryption crypto = null;
        if (writer != null) {
			crypto = writer.getEncryption();
		}
        final PdfObject objLen = get(PdfName.LENGTH);
        int nn = b.length;
        if (crypto != null) {
			nn = crypto.calculateStreamSize(nn);
		}
        put(PdfName.LENGTH, new PdfNumber(nn));
        superToPdf(writer, os);
        put(PdfName.LENGTH, objLen);
        os.write(STARTSTREAM);
        if (this.length > 0) {
            if (crypto != null && !crypto.isEmbeddedFilesOnly()) {
				b = crypto.encryptByteArray(b);
			}
            os.write(b);
        }
        os.write(ENDSTREAM);
    }
}