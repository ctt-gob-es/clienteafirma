/*
 * $Id: Jpeg2000.java 3583 2008-08-12 00:00:09Z xlv $
 *
 * Copyright 2007 by Paulo Soares.
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

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

/**
 * An <CODE>Jpeg2000</CODE> is the representation of a graphic element (JPEG)
 * that has to be inserted into the document
 *
 * @see		Element
 * @see		Image
 */

class Jpeg2000 extends Image {

    // public static final membervariables

	private static final int JP2_JP = 0x6a502020;
    private static final int JP2_IHDR = 0x69686472;

    private static final int JP2_FTYP = 0x66747970;
    private static final int JP2_JP2H = 0x6a703268;
    private static final int JP2_JP2C = 0x6a703263;

    private InputStream inp;
    private int boxLength;
    private int boxType;

    // Constructors

    /**
     * Constructs a <CODE>Jpeg2000</CODE>-object, using an <VAR>url</VAR>.
     *
     * @param		url			the <CODE>URL</CODE> where the image can be found
     * @throws BadElementException
     * @throws IOException
     */
    Jpeg2000(final URL url) throws BadElementException, IOException {
        super(url);
        processParameters();
    }

    /**
     * Constructs a <CODE>Jpeg2000</CODE>-object from memory.
     *
     * @param		img		the memory image
     * @throws BadElementException
     * @throws IOException
     */

    Jpeg2000(final byte[] img) throws BadElementException, IOException {
        super((URL)null);
        this.rawData = img;
        this.originalData = img;
        processParameters();
    }

    private int cio_read(final int n) throws IOException {
        int v = 0;
        for (int i = n - 1; i >= 0; i--) {
            v += this.inp.read() << (i << 3);
        }
        return v;
    }

    private void jp2_read_boxhdr() throws IOException {
        this.boxLength = cio_read(4);
        this.boxType = cio_read(4);
        if (this.boxLength == 1) {
            if (cio_read(4) != 0) {
                throw new IOException("Cannot handle box sizes higher than 2^32");
            }
            this.boxLength = cio_read(4);
            if (this.boxLength == 0) {
				throw new IOException("Unsupported box size == 0");
			}
        }
        else if (this.boxLength == 0) {
            throw new IOException("Unsupported box size == 0");
        }
    }

    /**
     * This method checks if the image is a valid JPEG and processes some parameters.
     * @throws IOException
     */
    private void processParameters() throws IOException {
        this.type = JPEG2000;
        this.originalType = ORIGINAL_JPEG2000;
        this.inp = null;
        try {
            String errorID;
            if (this.rawData == null){
                this.inp = this.url.openStream();
                errorID = this.url.toString();
            }
            else{
                this.inp = new java.io.ByteArrayInputStream(this.rawData);
                errorID = "Byte array";
            }
            this.boxLength = cio_read(4);
            if (this.boxLength == 0x0000000c) {
                this.boxType = cio_read(4);
                if (JP2_JP != this.boxType) {
                    throw new IOException("Expected JP Marker");
                }
                if (0x0d0a870a != cio_read(4)) {
                    throw new IOException("Error with JP Marker");
                }

                jp2_read_boxhdr();
                if (JP2_FTYP != this.boxType) {
                    throw new IOException("Expected FTYP Marker");
                }
                Utilities.skip(this.inp, this.boxLength - 8);
                jp2_read_boxhdr();
                do {
                    if (JP2_JP2H != this.boxType) {
                        if (this.boxType == JP2_JP2C) {
                            throw new IOException("Expected JP2H Marker");
                        }
                        Utilities.skip(this.inp, this.boxLength - 8);
                        jp2_read_boxhdr();
                    }
                } while(JP2_JP2H != this.boxType);
                jp2_read_boxhdr();
                if (JP2_IHDR != this.boxType) {
                    throw new IOException("Expected IHDR Marker");
                }
                this.scaledHeight = cio_read(4);
                setTop(this.scaledHeight);
                this.scaledWidth = cio_read(4);
                setRight(this.scaledWidth);
                this.bpc = -1;
            }
            else if (this.boxLength == 0xff4fff51) {
                Utilities.skip(this.inp, 4);
                final int x1 = cio_read(4);
                final int y1 = cio_read(4);
                final int x0 = cio_read(4);
                final int y0 = cio_read(4);
                Utilities.skip(this.inp, 16);
                this.colorspace = cio_read(2);
                this.bpc = 8;
                this.scaledHeight = y1 - y0;
                setTop(this.scaledHeight);
                this.scaledWidth = x1 - x0;
                setRight(this.scaledWidth);
            }
            else {
                throw new IOException("Not a valid Jpeg2000 file");
            }
        }
        finally {
            if (this.inp != null) {
                try{this.inp.close();}catch(final Exception e){}
                this.inp = null;
            }
        }
        this.plainWidth = getWidth();
        this.plainHeight = getHeight();
    }
}
