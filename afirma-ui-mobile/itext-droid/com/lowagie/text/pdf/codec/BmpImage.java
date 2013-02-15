/*
 * Copyright 2003-2008 by Paulo Soares.
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
 *
 * This code was originally released in 2001 by SUN (see class
 * com.sun.media.imageioimpl.plugins.bmp.BMPImageReader.java)
 * using the BSD license in a specific wording. In a mail dating from
 * January 23, 2008, Brian Burkhalter (@sun.com) gave us permission
 * to use the code under the following version of the BSD license:
 *
 * Copyright (c) 2005 Sun Microsystems, Inc. All  Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * - Redistribution of source code must retain the above copyright
 *   notice, this  list of conditions and the following disclaimer.
 *
 * - Redistribution in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in
 *   the documentation and/or other materials provided with the
 *   distribution.
 *
 * Neither the name of Sun Microsystems, Inc. or the names of
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 *
 * This software is provided "AS IS," without a warranty of any
 * kind. ALL EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND
 * WARRANTIES, INCLUDING ANY IMPLIED WARRANTY OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT, ARE HEREBY
 * EXCLUDED. SUN MIDROSYSTEMS, INC. ("SUN") AND ITS LICENSORS SHALL
 * NOT BE LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF
 * USING, MODIFYING OR DISTRIBUTING THIS SOFTWARE OR ITS
 * DERIVATIVES. IN NO EVENT WILL SUN OR ITS LICENSORS BE LIABLE FOR
 * ANY LOST REVENUE, PROFIT OR DATA, OR FOR DIRECT, INDIRECT, SPECIAL,
 * CONSEQUENTIAL, INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER CAUSED AND
 * REGARDLESS OF THE THEORY OF LIABILITY, ARISING OUT OF THE USE OF OR
 * INABILITY TO USE THIS SOFTWARE, EVEN IF SUN HAS BEEN ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGES.
 *
 * You acknowledge that this software is not designed or intended for
 * use in the design, construction, operation or maintenance of any
 * nuclear facility.
 */
package com.lowagie.text.pdf.codec;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;

import com.lowagie.text.BadElementException;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.ImgRaw;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;
import com.lowagie.text.pdf.PdfString;

/** Reads a BMP image. All types of BMP can be read.
 * <p>
 * It is based in the JAI codec.
 *
 * @author  Paulo Soares (psoares@consiste.pt)
 */
public class BmpImage {

    // BMP variables
    private InputStream inputStream;
    private long bitmapFileSize;
    private long bitmapOffset;
    private long compression;
    private long imageSize;
    private byte palette[];
    private int imageType;
    private int numBands;
    private boolean isBottomUp;
    private int bitsPerPixel;
    private int redMask, greenMask, blueMask, alphaMask;
    private final HashMap properties = new HashMap();
    private long xPelsPerMeter;
    private long yPelsPerMeter;
    // BMP Image types
    private static final int VERSION_2_1_BIT = 0;
    private static final int VERSION_2_4_BIT = 1;
    private static final int VERSION_2_8_BIT = 2;
    private static final int VERSION_2_24_BIT = 3;

    private static final int VERSION_3_1_BIT = 4;
    private static final int VERSION_3_4_BIT = 5;
    private static final int VERSION_3_8_BIT = 6;
    private static final int VERSION_3_24_BIT = 7;

    private static final int VERSION_3_NT_16_BIT = 8;
    private static final int VERSION_3_NT_32_BIT = 9;

    private static final int VERSION_4_1_BIT = 10;
    private static final int VERSION_4_4_BIT = 11;
    private static final int VERSION_4_8_BIT = 12;
    private static final int VERSION_4_16_BIT = 13;
    private static final int VERSION_4_24_BIT = 14;
    private static final int VERSION_4_32_BIT = 15;

    // Color space types
    private static final int LCS_CALIBRATED_RGB = 0;
    private static final int LCS_sRGB = 1;
    private static final int LCS_CMYK = 2;

    // Compression Types
    private static final int BI_RGB = 0;
    private static final int BI_RLE8 = 1;
    private static final int BI_RLE4 = 2;
    private static final int BI_BITFIELDS = 3;

    private int width;
    private int height;

    private BmpImage(final InputStream is, final boolean noHeader, final int size) throws IOException {
        this.bitmapFileSize = size;
        this.bitmapOffset = 0;
        process(is, noHeader);
    }

    /** Reads a BMP from an url.
     * @param url the url
     * @throws IOException on error
     * @return the image
     */
    public static Image getImage(final URL url) throws IOException {
        InputStream is = null;
        try {
            is = url.openStream();
            final Image img = getImage(is);
            img.setUrl(url);
            return img;
        }
        finally {
            if (is != null) {
                is.close();
            }
        }
    }

    /** Reads a BMP from a stream. The stream is not closed.
     * @param is the stream
     * @throws IOException on error
     * @return the image
     */
    private static Image getImage(final InputStream is) throws IOException {
        return getImage(is, false, 0);
    }

    /** Reads a BMP from a stream. The stream is not closed.
     * The BMP may not have a header and be considered as a plain DIB.
     * @param is the stream
     * @param noHeader true to process a plain DIB
     * @param size the size of the DIB. Not used for a BMP
     * @throws IOException on error
     * @return the image
     */
    public static Image getImage(final InputStream is, final boolean noHeader, final int size) throws IOException {
        final BmpImage bmp = new BmpImage(is, noHeader, size);
        try {
            final Image img = bmp.getImage();
            img.setDpi((int)(bmp.xPelsPerMeter * 0.0254d + 0.5d), (int)(bmp.yPelsPerMeter * 0.0254d + 0.5d));
            img.setOriginalType(Image.ORIGINAL_BMP);
            return img;
        }
        catch (final BadElementException be) {
            throw new ExceptionConverter(be);
        }
    }



    /** Reads a BMP from a byte array.
     * @param data the byte array
     * @throws IOException on error
     * @return the image
     */
    public static Image getImage(final byte data[]) throws IOException {
        final ByteArrayInputStream is = new ByteArrayInputStream(data);
        final Image img = getImage(is);
        img.setOriginalData(data);
        return img;
    }


    private void process(final InputStream stream, final boolean noHeader) throws IOException {
        if (noHeader || stream instanceof BufferedInputStream) {
            this.inputStream = stream;
        } else {
            this.inputStream = new BufferedInputStream(stream);
        }
        if (!noHeader) {
            // Start File Header
            if (!(readUnsignedByte(this.inputStream) == 'B' &&
            readUnsignedByte(this.inputStream) == 'M')) {
                throw new
                RuntimeException("Invalid magic value for BMP file.");
            }

            // Read file size
            this.bitmapFileSize = readDWord(this.inputStream);

            // Read the two reserved fields
            readWord(this.inputStream);
            readWord(this.inputStream);

            // Offset to the bitmap from the beginning
            this.bitmapOffset = readDWord(this.inputStream);

            // End File Header
        }
        // Start BitmapCoreHeader
        final long size = readDWord(this.inputStream);

        if (size == 12) {
            this.width = readWord(this.inputStream);
            this.height = readWord(this.inputStream);
        } else {
            this.width = readLong(this.inputStream);
            this.height = readLong(this.inputStream);
        }

        final int planes = readWord(this.inputStream);
        this.bitsPerPixel = readWord(this.inputStream);

        this.properties.put("color_planes", new Integer(planes));
        this.properties.put("bits_per_pixel", new Integer(this.bitsPerPixel));

        // As BMP always has 3 rgb bands, except for Version 5,
        // which is bgra
        this.numBands = 3;
        if (this.bitmapOffset == 0) {
			this.bitmapOffset = size;
		}
        if (size == 12) {
            // Windows 2.x and OS/2 1.x
            this.properties.put("bmp_version", "BMP v. 2.x");

            // Classify the image type
            if (this.bitsPerPixel == 1) {
                this.imageType = VERSION_2_1_BIT;
            } else if (this.bitsPerPixel == 4) {
                this.imageType = VERSION_2_4_BIT;
            } else if (this.bitsPerPixel == 8) {
                this.imageType = VERSION_2_8_BIT;
            } else if (this.bitsPerPixel == 24) {
                this.imageType = VERSION_2_24_BIT;
            }

            // Read in the palette
            final int numberOfEntries = (int)((this.bitmapOffset-14-size) / 3);
            int sizeOfPalette = numberOfEntries*3;
            if (this.bitmapOffset == size) {
                switch (this.imageType) {
                    case VERSION_2_1_BIT:
                        sizeOfPalette = 2 * 3;
                        break;
                    case VERSION_2_4_BIT:
                        sizeOfPalette = 16 * 3;
                        break;
                    case VERSION_2_8_BIT:
                        sizeOfPalette = 256 * 3;
                        break;
                    case VERSION_2_24_BIT:
                        sizeOfPalette = 0;
                        break;
                }
                this.bitmapOffset = size + sizeOfPalette;
            }
            readPalette(sizeOfPalette);
        } else {

            this.compression = readDWord(this.inputStream);
            this.imageSize = readDWord(this.inputStream);
            this.xPelsPerMeter = readLong(this.inputStream);
            this.yPelsPerMeter = readLong(this.inputStream);
            final long colorsUsed = readDWord(this.inputStream);
            final long colorsImportant = readDWord(this.inputStream);

            switch((int)this.compression) {
                case BI_RGB:
                    this.properties.put("compression", "BI_RGB");
                    break;

                case BI_RLE8:
                    this.properties.put("compression", "BI_RLE8");
                    break;

                case BI_RLE4:
                    this.properties.put("compression", "BI_RLE4");
                    break;

                case BI_BITFIELDS:
                    this.properties.put("compression", "BI_BITFIELDS");
                    break;
            }

            this.properties.put("x_pixels_per_meter", new Long(this.xPelsPerMeter));
            this.properties.put("y_pixels_per_meter", new Long(this.yPelsPerMeter));
            this.properties.put("colors_used", new Long(colorsUsed));
            this.properties.put("colors_important", new Long(colorsImportant));

            if (size == 40) {
                // Windows 3.x and Windows NT
                switch((int)this.compression) {

                    case BI_RGB:  // No compression
                    case BI_RLE8:  // 8-bit RLE compression
                    case BI_RLE4:  // 4-bit RLE compression

                        if (this.bitsPerPixel == 1) {
                            this.imageType = VERSION_3_1_BIT;
                        } else if (this.bitsPerPixel == 4) {
                            this.imageType = VERSION_3_4_BIT;
                        } else if (this.bitsPerPixel == 8) {
                            this.imageType = VERSION_3_8_BIT;
                        } else if (this.bitsPerPixel == 24) {
                            this.imageType = VERSION_3_24_BIT;
                        } else if (this.bitsPerPixel == 16) {
                            this.imageType = VERSION_3_NT_16_BIT;
                            this.redMask = 0x7C00;
                            this.greenMask = 0x3E0;
                            this.blueMask = 0x1F;
                            this.properties.put("red_mask", new Integer(this.redMask));
                            this.properties.put("green_mask", new Integer(this.greenMask));
                            this.properties.put("blue_mask", new Integer(this.blueMask));
                        } else if (this.bitsPerPixel == 32) {
                            this.imageType = VERSION_3_NT_32_BIT;
                            this.redMask   = 0x00FF0000;
                            this.greenMask = 0x0000FF00;
                            this.blueMask  = 0x000000FF;
                            this.properties.put("red_mask", new Integer(this.redMask));
                            this.properties.put("green_mask", new Integer(this.greenMask));
                            this.properties.put("blue_mask", new Integer(this.blueMask));
                        }

                        // Read in the palette
                        final int numberOfEntries = (int)((this.bitmapOffset-14-size) / 4);
                        int sizeOfPalette = numberOfEntries*4;
                        if (this.bitmapOffset == size) {
                            switch (this.imageType) {
                                case VERSION_3_1_BIT:
                                    sizeOfPalette = (int)(colorsUsed == 0 ? 2 : colorsUsed) * 4;
                                    break;
                                case VERSION_3_4_BIT:
                                    sizeOfPalette = (int)(colorsUsed == 0 ? 16 : colorsUsed) * 4;
                                    break;
                                case VERSION_3_8_BIT:
                                    sizeOfPalette = (int)(colorsUsed == 0 ? 256 : colorsUsed) * 4;
                                    break;
                                default:
                                    sizeOfPalette = 0;
                                    break;
                            }
                            this.bitmapOffset = size + sizeOfPalette;
                        }
                        readPalette(sizeOfPalette);

                        this.properties.put("bmp_version", "BMP v. 3.x");
                        break;

                    case BI_BITFIELDS:

                        if (this.bitsPerPixel == 16) {
                            this.imageType = VERSION_3_NT_16_BIT;
                        } else if (this.bitsPerPixel == 32) {
                            this.imageType = VERSION_3_NT_32_BIT;
                        }

                        // BitsField encoding
                        this.redMask = (int)readDWord(this.inputStream);
                        this.greenMask = (int)readDWord(this.inputStream);
                        this.blueMask = (int)readDWord(this.inputStream);

                        this.properties.put("red_mask", new Integer(this.redMask));
                        this.properties.put("green_mask", new Integer(this.greenMask));
                        this.properties.put("blue_mask", new Integer(this.blueMask));

                        if (colorsUsed != 0) {
                            // there is a palette
                            sizeOfPalette = (int)colorsUsed*4;
                            readPalette(sizeOfPalette);
                        }

                        this.properties.put("bmp_version", "BMP v. 3.x NT");
                        break;

                    default:
                        throw new
                        RuntimeException("Invalid compression specified in BMP file.");
                }
            } else if (size == 108) {
                // Windows 4.x BMP

                this.properties.put("bmp_version", "BMP v. 4.x");

                // rgb masks, valid only if comp is BI_BITFIELDS
                this.redMask = (int)readDWord(this.inputStream);
                this.greenMask = (int)readDWord(this.inputStream);
                this.blueMask = (int)readDWord(this.inputStream);
                // Only supported for 32bpp BI_RGB argb
                this.alphaMask = (int)readDWord(this.inputStream);
                final long csType = readDWord(this.inputStream);
                final int redX = readLong(this.inputStream);
                final int redY = readLong(this.inputStream);
                final int redZ = readLong(this.inputStream);
                final int greenX = readLong(this.inputStream);
                final int greenY = readLong(this.inputStream);
                final int greenZ = readLong(this.inputStream);
                final int blueX = readLong(this.inputStream);
                final int blueY = readLong(this.inputStream);
                final int blueZ = readLong(this.inputStream);
                final long gammaRed = readDWord(this.inputStream);
                final long gammaGreen = readDWord(this.inputStream);
                final long gammaBlue = readDWord(this.inputStream);

                if (this.bitsPerPixel == 1) {
                    this.imageType = VERSION_4_1_BIT;
                } else if (this.bitsPerPixel == 4) {
                    this.imageType = VERSION_4_4_BIT;
                } else if (this.bitsPerPixel == 8) {
                    this.imageType = VERSION_4_8_BIT;
                } else if (this.bitsPerPixel == 16) {
                    this.imageType = VERSION_4_16_BIT;
                    if ((int)this.compression == BI_RGB) {
                        this.redMask = 0x7C00;
                        this.greenMask = 0x3E0;
                        this.blueMask = 0x1F;
                    }
                } else if (this.bitsPerPixel == 24) {
                    this.imageType = VERSION_4_24_BIT;
                } else if (this.bitsPerPixel == 32) {
                    this.imageType = VERSION_4_32_BIT;
                    if ((int)this.compression == BI_RGB) {
                        this.redMask   = 0x00FF0000;
                        this.greenMask = 0x0000FF00;
                        this.blueMask  = 0x000000FF;
                    }
                }

                this.properties.put("red_mask", new Integer(this.redMask));
                this.properties.put("green_mask", new Integer(this.greenMask));
                this.properties.put("blue_mask", new Integer(this.blueMask));
                this.properties.put("alpha_mask", new Integer(this.alphaMask));

                // Read in the palette
                final int numberOfEntries = (int)((this.bitmapOffset-14-size) / 4);
                int sizeOfPalette = numberOfEntries*4;
                if (this.bitmapOffset == size) {
                    switch (this.imageType) {
                        case VERSION_4_1_BIT:
                            sizeOfPalette = (int)(colorsUsed == 0 ? 2 : colorsUsed) * 4;
                            break;
                        case VERSION_4_4_BIT:
                            sizeOfPalette = (int)(colorsUsed == 0 ? 16 : colorsUsed) * 4;
                            break;
                        case VERSION_4_8_BIT:
                            sizeOfPalette = (int)(colorsUsed == 0 ? 256 : colorsUsed) * 4;
                            break;
                        default:
                            sizeOfPalette = 0;
                            break;
                    }
                    this.bitmapOffset = size + sizeOfPalette;
                }
                readPalette(sizeOfPalette);

                switch((int)csType) {
                    case LCS_CALIBRATED_RGB:
                        // All the new fields are valid only for this case
                        this.properties.put("color_space", "LCS_CALIBRATED_RGB");
                        this.properties.put("redX", new Integer(redX));
                        this.properties.put("redY", new Integer(redY));
                        this.properties.put("redZ", new Integer(redZ));
                        this.properties.put("greenX", new Integer(greenX));
                        this.properties.put("greenY", new Integer(greenY));
                        this.properties.put("greenZ", new Integer(greenZ));
                        this.properties.put("blueX", new Integer(blueX));
                        this.properties.put("blueY", new Integer(blueY));
                        this.properties.put("blueZ", new Integer(blueZ));
                        this.properties.put("gamma_red", new Long(gammaRed));
                        this.properties.put("gamma_green", new Long(gammaGreen));
                        this.properties.put("gamma_blue", new Long(gammaBlue));

                        // break;
                        throw new
                        RuntimeException("Not implemented yet.");

                    case LCS_sRGB:
                        // Default Windows color space
                        this.properties.put("color_space", "LCS_sRGB");
                        break;

                    case LCS_CMYK:
                        this.properties.put("color_space", "LCS_CMYK");
                        //		    break;
                        throw new
                        RuntimeException("Not implemented yet.");
                }

            } else {
                this.properties.put("bmp_version", "BMP v. 5.x");
                throw new
                RuntimeException("BMP version 5 not implemented yet.");
            }
        }

        if (this.height > 0) {
            // bottom up image
            this.isBottomUp = true;
        } else {
            // top down image
            this.isBottomUp = false;
            this.height = Math.abs(this.height);
        }
        // When number of bitsPerPixel is <= 8, we use IndexColorModel.
        if (this.bitsPerPixel == 1 || this.bitsPerPixel == 4 || this.bitsPerPixel == 8) {

            this.numBands = 1;


            // Create IndexColorModel from the palette.
            byte r[], g[], b[];
            int sizep;
            if (this.imageType == VERSION_2_1_BIT ||
            this.imageType == VERSION_2_4_BIT ||
            this.imageType == VERSION_2_8_BIT) {

                sizep = this.palette.length/3;

                if (sizep > 256) {
                    sizep = 256;
                }

                int off;
                r = new byte[sizep];
                g = new byte[sizep];
                b = new byte[sizep];
                for (int i=0; i<sizep; i++) {
                    off = 3 * i;
                    b[i] = this.palette[off];
                    g[i] = this.palette[off+1];
                    r[i] = this.palette[off+2];
                }
            } else {
                sizep = this.palette.length/4;

                if (sizep > 256) {
                    sizep = 256;
                }

                int off;
                r = new byte[sizep];
                g = new byte[sizep];
                b = new byte[sizep];
                for (int i=0; i<sizep; i++) {
                    off = 4 * i;
                    b[i] = this.palette[off];
                    g[i] = this.palette[off+1];
                    r[i] = this.palette[off+2];
                }
            }

        } else if (this.bitsPerPixel == 16) {
            this.numBands = 3;
        } else if (this.bitsPerPixel == 32) {
            this.numBands = this.alphaMask == 0 ? 3 : 4;

            // The number of bands in the SampleModel is determined by
            // the length of the mask array passed in.
        } else {
            this.numBands = 3;
        }
    }

    private byte[] getPalette(final int group) {
        if (this.palette == null) {
			return null;
		}
        final byte np[] = new byte[this.palette.length / group * 3];
        final int e = this.palette.length / group;
        for (int k = 0; k < e; ++k) {
            int src = k * group;
            final int dest = k * 3;
            np[dest + 2] = this.palette[src++];
            np[dest + 1] = this.palette[src++];
            np[dest] = this.palette[src];
        }
        return np;
    }

    private Image getImage() throws IOException, BadElementException {
        byte bdata[] = null; // buffer for byte data

        //	if (sampleModel.getDataType() == DataBuffer.TYPE_BYTE)
        //	    bdata = (byte[])((DataBufferByte)tile.getDataBuffer()).getData();
        //	else if (sampleModel.getDataType() == DataBuffer.TYPE_USHORT)
        //	    sdata = (short[])((DataBufferUShort)tile.getDataBuffer()).getData();
        //	else if (sampleModel.getDataType() == DataBuffer.TYPE_INT)
        //	    idata = (int[])((DataBufferInt)tile.getDataBuffer()).getData();

        // There should only be one tile.
        switch(this.imageType) {

            case VERSION_2_1_BIT:
                // no compression
                return read1Bit(3);

            case VERSION_2_4_BIT:
                // no compression
                return read4Bit(3);

            case VERSION_2_8_BIT:
                // no compression
                return read8Bit(3);

            case VERSION_2_24_BIT:
                // no compression
                bdata = new byte[this.width * this.height * 3];
                read24Bit(bdata);
                return new ImgRaw(this.width, this.height, 3, 8, bdata);

            case VERSION_3_1_BIT:
                // 1-bit images cannot be compressed.
                return read1Bit(4);

            case VERSION_3_4_BIT:
                switch((int)this.compression) {
                    case BI_RGB:
                        return read4Bit(4);

                    case BI_RLE4:
                        return readRLE4();

                    default:
                        throw new
                        RuntimeException("Invalid compression specified for BMP file.");
                }

            case VERSION_3_8_BIT:
                switch((int)this.compression) {
                    case BI_RGB:
                        return read8Bit(4);

                    case BI_RLE8:
                        return readRLE8();

                    default:
                        throw new
                        RuntimeException("Invalid compression specified for BMP file.");
                }

            case VERSION_3_24_BIT:
                // 24-bit images are not compressed
                bdata = new byte[this.width * this.height * 3];
                read24Bit(bdata);
                return new ImgRaw(this.width, this.height, 3, 8, bdata);

            case VERSION_3_NT_16_BIT:
                return read1632Bit(false);

            case VERSION_3_NT_32_BIT:
                return read1632Bit(true);

            case VERSION_4_1_BIT:
                return read1Bit(4);

            case VERSION_4_4_BIT:
                switch((int)this.compression) {

                    case BI_RGB:
                        return read4Bit(4);

                    case BI_RLE4:
                        return readRLE4();

                    default:
                        throw new
                        RuntimeException("Invalid compression specified for BMP file.");
                }

            case VERSION_4_8_BIT:
                switch((int)this.compression) {

                    case BI_RGB:
                        return read8Bit(4);

                    case BI_RLE8:
                        return readRLE8();

                    default:
                        throw new
                        RuntimeException("Invalid compression specified for BMP file.");
                }

            case VERSION_4_16_BIT:
                return read1632Bit(false);

            case VERSION_4_24_BIT:
                bdata = new byte[this.width * this.height * 3];
                read24Bit(bdata);
                return new ImgRaw(this.width, this.height, 3, 8, bdata);

            case VERSION_4_32_BIT:
                return read1632Bit(true);
        }
        return null;
    }

    private Image indexedModel(final byte bdata[], final int bpc, final int paletteEntries) throws BadElementException {
        final Image img = new ImgRaw(this.width, this.height, 1, bpc, bdata);
        final PdfArray colorspace = new PdfArray();
        colorspace.add(PdfName.INDEXED);
        colorspace.add(PdfName.DEVICERGB);
        final byte np[] = getPalette(paletteEntries);
        final int len = np.length;
        colorspace.add(new PdfNumber(len / 3 - 1));
        colorspace.add(new PdfString(np));
        final PdfDictionary ad = new PdfDictionary();
        ad.put(PdfName.COLORSPACE, colorspace);
        img.setAdditional(ad);
        return img;
    }

    private void readPalette(final int sizeOfPalette) throws IOException {
        if (sizeOfPalette == 0) {
            return;
        }

        this.palette = new byte[sizeOfPalette];
        int bytesRead = 0;
        while (bytesRead < sizeOfPalette) {
            final int r = this.inputStream.read(this.palette, bytesRead, sizeOfPalette - bytesRead);
            if (r < 0) {
                throw new RuntimeException("incomplete palette");
            }
            bytesRead += r;
        }
        this.properties.put("palette", this.palette);
    }

    // Deal with 1 Bit images using IndexColorModels
    private Image read1Bit(final int paletteEntries) throws IOException, BadElementException {
        final byte bdata[] = new byte[(this.width + 7) / 8 * this.height];
        int padding = 0;
        final int bytesPerScanline = (int)Math.ceil(this.width/8.0d);

        final int remainder = bytesPerScanline % 4;
        if (remainder != 0) {
            padding = 4 - remainder;
        }

        final int imSize = (bytesPerScanline + padding) * this.height;

        // Read till we have the whole image
        final byte values[] = new byte[imSize];
        int bytesRead = 0;
        while (bytesRead < imSize) {
            bytesRead += this.inputStream.read(values, bytesRead,
            imSize - bytesRead);
        }

        if (this.isBottomUp) {

            // Convert the bottom up image to a top down format by copying
            // one scanline from the bottom to the top at a time.

            for (int i=0; i<this.height; i++) {
                System.arraycopy(values,
                imSize - (i+1)*(bytesPerScanline + padding),
                bdata,
                i*bytesPerScanline, bytesPerScanline);
            }
        } else {

            for (int i=0; i<this.height; i++) {
                System.arraycopy(values,
                i * (bytesPerScanline + padding),
                bdata,
                i * bytesPerScanline,
                bytesPerScanline);
            }
        }
        return indexedModel(bdata, 1, paletteEntries);
    }

    // Method to read a 4 bit BMP image data
    private Image read4Bit(final int paletteEntries) throws IOException, BadElementException {
        final byte bdata[] = new byte[(this.width + 1) / 2 * this.height];

        // Padding bytes at the end of each scanline
        int padding = 0;

        final int bytesPerScanline = (int)Math.ceil(this.width/2.0d);
        final int remainder = bytesPerScanline % 4;
        if (remainder != 0) {
            padding = 4 - remainder;
        }

        final int imSize = (bytesPerScanline + padding) * this.height;

        // Read till we have the whole image
        final byte values[] = new byte[imSize];
        int bytesRead = 0;
        while (bytesRead < imSize) {
            bytesRead += this.inputStream.read(values, bytesRead,
            imSize - bytesRead);
        }

        if (this.isBottomUp) {

            // Convert the bottom up image to a top down format by copying
            // one scanline from the bottom to the top at a time.
            for (int i=0; i<this.height; i++) {
                System.arraycopy(values,
                imSize - (i+1)*(bytesPerScanline + padding),
                bdata,
                i*bytesPerScanline,
                bytesPerScanline);
            }
        } else {
            for (int i=0; i<this.height; i++) {
                System.arraycopy(values,
                i * (bytesPerScanline + padding),
                bdata,
                i * bytesPerScanline,
                bytesPerScanline);
            }
        }
        return indexedModel(bdata, 4, paletteEntries);
    }

    // Method to read 8 bit BMP image data
    private Image read8Bit(final int paletteEntries) throws IOException, BadElementException {
        final byte bdata[] = new byte[this.width * this.height];
        // Padding bytes at the end of each scanline
        int padding = 0;

        // width * bitsPerPixel should be divisible by 32
        final int bitsPerScanline = this.width * 8;
        if ( bitsPerScanline%32 != 0) {
            padding = (bitsPerScanline/32 + 1)*32 - bitsPerScanline;
            padding = (int)Math.ceil(padding/8.0);
        }

        final int imSize = (this.width + padding) * this.height;

        // Read till we have the whole image
        final byte values[] = new byte[imSize];
        int bytesRead = 0;
        while (bytesRead < imSize) {
            bytesRead += this.inputStream.read(values, bytesRead, imSize - bytesRead);
        }

        if (this.isBottomUp) {

            // Convert the bottom up image to a top down format by copying
            // one scanline from the bottom to the top at a time.
            for (int i=0; i<this.height; i++) {
                System.arraycopy(values,
                imSize - (i+1) * (this.width + padding),
                bdata,
                i * this.width,
                this.width);
            }
        } else {
            for (int i=0; i<this.height; i++) {
                System.arraycopy(values,
                i * (this.width + padding),
                bdata,
                i * this.width,
                this.width);
            }
        }
        return indexedModel(bdata, 8, paletteEntries);
    }

    // Method to read 24 bit BMP image data
    private void read24Bit(final byte[] bdata) {
        // Padding bytes at the end of each scanline
        int padding = 0;

        // width * bitsPerPixel should be divisible by 32
        final int bitsPerScanline = this.width * 24;
        if ( bitsPerScanline%32 != 0) {
            padding = (bitsPerScanline/32 + 1)*32 - bitsPerScanline;
            padding = (int)Math.ceil(padding/8.0);
        }


        final int imSize = (this.width * 3 + 3) / 4 * 4 * this.height;
        // Read till we have the whole image
        final byte values[] = new byte[imSize];
        try {
            int bytesRead = 0;
            while (bytesRead < imSize) {
                final int r = this.inputStream.read(values, bytesRead,
                imSize - bytesRead);
                if (r < 0) {
					break;
				}
                bytesRead += r;
            }
        } catch (final IOException ioe) {
            throw new ExceptionConverter(ioe);
        }

        int l=0, count;

        if (this.isBottomUp) {
            final int max = this.width*this.height*3-1;

            count = -padding;
            for (int i=0; i<this.height; i++) {
                l = max - (i+1)*this.width*3 + 1;
                count += padding;
                for (int j=0; j<this.width; j++) {
                    bdata[l + 2] = values[count++];
                    bdata[l + 1] = values[count++];
                    bdata[l] = values[count++];
                    l += 3;
                }
            }
        } else {
            count = -padding;
            for (int i=0; i<this.height; i++) {
                count += padding;
                for (int j=0; j<this.width; j++) {
                    bdata[l + 2] = values[count++];
                    bdata[l + 1] = values[count++];
                    bdata[l] = values[count++];
                    l += 3;
                }
            }
        }
    }

    private int findMask(int mask) {
        int k = 0;
        for (; k < 32; ++k) {
            if ((mask & 1) == 1) {
				break;
			}
            mask >>>= 1;
        }
        return mask;
    }

    private int findShift(int mask) {
        int k = 0;
        for (; k < 32; ++k) {
            if ((mask & 1) == 1) {
				break;
			}
            mask >>>= 1;
        }
        return k;
    }

    private Image read1632Bit(final boolean is32) throws IOException, BadElementException {

        final int red_mask = findMask(this.redMask);
        final int red_shift = findShift(this.redMask);
        final int red_factor = red_mask + 1;
        final int green_mask = findMask(this.greenMask);
        final int green_shift = findShift(this.greenMask);
        final int green_factor = green_mask + 1;
        final int blue_mask = findMask(this.blueMask);
        final int blue_shift = findShift(this.blueMask);
        final int blue_factor = blue_mask + 1;
        final byte bdata[] = new byte[this.width * this.height * 3];
        // Padding bytes at the end of each scanline
        int padding = 0;

        if (!is32) {
        // width * bitsPerPixel should be divisible by 32
            final int bitsPerScanline = this.width * 16;
            if ( bitsPerScanline%32 != 0) {
                padding = (bitsPerScanline/32 + 1)*32 - bitsPerScanline;
                padding = (int)Math.ceil(padding/8.0);
            }
        }

        int imSize = (int)this.imageSize;
        if (imSize == 0) {
            imSize = (int)(this.bitmapFileSize - this.bitmapOffset);
        }

        int l=0;
        int v;
        if (this.isBottomUp) {
            for (int i=this.height - 1; i >= 0; --i) {
                l = this.width * 3 * i;
                for (int j=0; j<this.width; j++) {
                    if (is32) {
						v = (int)readDWord(this.inputStream);
					} else {
						v = readWord(this.inputStream);
					}
                    bdata[l++] = (byte)((v >>> red_shift & red_mask) * 256 / red_factor);
                    bdata[l++] = (byte)((v >>> green_shift & green_mask) * 256 / green_factor);
                    bdata[l++] = (byte)((v >>> blue_shift & blue_mask) * 256 / blue_factor);
                }
                for (int m=0; m<padding; m++) {
                    this.inputStream.read();
                }
            }
        } else {
            for (int i=0; i<this.height; i++) {
                for (int j=0; j<this.width; j++) {
                    if (is32) {
						v = (int)readDWord(this.inputStream);
					} else {
						v = readWord(this.inputStream);
					}
                    bdata[l++] = (byte)((v >>> red_shift & red_mask) * 256 / red_factor);
                    bdata[l++] = (byte)((v >>> green_shift & green_mask) * 256 / green_factor);
                    bdata[l++] = (byte)((v >>> blue_shift & blue_mask) * 256 / blue_factor);
                }
                for (int m=0; m<padding; m++) {
                    this.inputStream.read();
                }
            }
        }
        return new ImgRaw(this.width, this.height, 3, 8, bdata);
    }

    private Image readRLE8() throws IOException, BadElementException {

        // If imageSize field is not provided, calculate it.
        int imSize = (int)this.imageSize;
        if (imSize == 0) {
            imSize = (int)(this.bitmapFileSize - this.bitmapOffset);
        }

        // Read till we have the whole image
        final byte values[] = new byte[imSize];
        int bytesRead = 0;
        while (bytesRead < imSize) {
            bytesRead += this.inputStream.read(values, bytesRead,
            imSize - bytesRead);
        }

        // Since data is compressed, decompress it
        byte val[] = decodeRLE(true, values);

        // Uncompressed data does not have any padding
        imSize = this.width * this.height;

        if (this.isBottomUp) {

            // Convert the bottom up image to a top down format by copying
            // one scanline from the bottom to the top at a time.
            // int bytesPerScanline = (int)Math.ceil((double)width/8.0);
            final byte temp[] = new byte[val.length];
            final int bytesPerScanline = this.width;
            for (int i=0; i<this.height; i++) {
                System.arraycopy(val,
                imSize - (i+1)*bytesPerScanline,
                temp,
                i*bytesPerScanline, bytesPerScanline);
            }
            val = temp;
        }
        return indexedModel(val, 8, 4);
    }

    private Image readRLE4() throws IOException, BadElementException {

        // If imageSize field is not specified, calculate it.
        int imSize = (int)this.imageSize;
        if (imSize == 0) {
            imSize = (int)(this.bitmapFileSize - this.bitmapOffset);
        }

        // Read till we have the whole image
        final byte values[] = new byte[imSize];
        int bytesRead = 0;
        while (bytesRead < imSize) {
            bytesRead += this.inputStream.read(values, bytesRead,
            imSize - bytesRead);
        }

        // Decompress the RLE4 compressed data.
        byte val[] = decodeRLE(false, values);

        // Invert it as it is bottom up format.
        if (this.isBottomUp) {

            final byte inverted[] = val;
            val = new byte[this.width * this.height];
            int l = 0, index, lineEnd;

            for (int i = this.height-1; i >= 0; i--) {
                index = i * this.width;
                lineEnd = l + this.width;
                while(l != lineEnd) {
                    val[l++] = inverted[index++];
                }
            }
        }
        final int stride = (this.width + 1) / 2;
        final byte bdata[] = new byte[stride * this.height];
        int ptr = 0;
        int sh = 0;
        for (int h = 0; h < this.height; ++h) {
            for (int w = 0; w < this.width; ++w) {
                if ((w & 1) == 0) {
					bdata[sh + w / 2] = (byte)(val[ptr++] << 4);
				} else {
					bdata[sh + w / 2] |= (byte)(val[ptr++] & 0x0f);
				}
            }
            sh += stride;
        }
        return indexedModel(bdata, 4, 4);
    }

    private byte[] decodeRLE(final boolean is8, final byte values[]) {
        final byte val[] = new byte[this.width * this.height];
        try {
            int ptr = 0;
            int x = 0;
            int q = 0;
            for (int y = 0; y < this.height && ptr < values.length;) {
                int count = values[ptr++] & 0xff;
                if (count != 0) {
                    // encoded mode
                    final int bt = values[ptr++] & 0xff;
                    if (is8) {
                        for (int i = count; i != 0; --i) {
                            val[q++] = (byte)bt;
                        }
                    }
                    else {
                        for (int i = 0; i < count; ++i) {
                            val[q++] = (byte)((i & 1) == 1 ? bt & 0x0f : bt >>> 4 & 0x0f);
                        }
                    }
                    x += count;
                }
                else {
                    // escape mode
                    count = values[ptr++] & 0xff;
                    if (count == 1) {
						break;
					}
                    switch (count) {
                        case 0:
                            x = 0;
                            ++y;
                            q = y * this.width;
                            break;
                        case 2:
                            // delta mode
                            x += values[ptr++] & 0xff;
                            y += values[ptr++] & 0xff;
                            q = y * this.width + x;
                            break;
                        default:
                            // absolute mode
                            if (is8) {
                                for (int i = count; i != 0; --i) {
									val[q++] = (byte)(values[ptr++] & 0xff);
								}
                            }
                            else {
                                int bt = 0;
                                for (int i = 0; i < count; ++i) {
                                    if ((i & 1) == 0) {
										bt = values[ptr++] & 0xff;
									}
                                    val[q++] = (byte)((i & 1) == 1 ? bt & 0x0f : bt >>> 4 & 0x0f);
                                }
                            }
                            x += count;
                            // read pad byte
                            if (is8) {
                                if ((count & 1) == 1) {
									++ptr;
								}
                            }
                            else {
                                if ((count & 3) == 1 || (count & 3) == 2) {
									++ptr;
								}
                            }
                            break;
                    }
                }
            }
        }
        catch (final RuntimeException e) {
            //empty on purpose
        }

        return val;
    }

    // Windows defined data type reading methods - everything is little endian

    // Unsigned 8 bits
    private int readUnsignedByte(final InputStream stream) throws IOException {
        return stream.read() & 0xff;
    }

    // Unsigned 2 bytes
    private int readUnsignedShort(final InputStream stream) throws IOException {
        final int b1 = readUnsignedByte(stream);
        final int b2 = readUnsignedByte(stream);
        return (b2 << 8 | b1) & 0xffff;
    }

    // Signed 16 bits
    private int readShort(final InputStream stream) throws IOException {
        final int b1 = readUnsignedByte(stream);
        final int b2 = readUnsignedByte(stream);
        return b2 << 8 | b1;
    }

    // Unsigned 16 bits
    private int readWord(final InputStream stream) throws IOException {
        return readUnsignedShort(stream);
    }

    // Unsigned 4 bytes
    private long readUnsignedInt(final InputStream stream) throws IOException {
        final int b1 = readUnsignedByte(stream);
        final int b2 = readUnsignedByte(stream);
        final int b3 = readUnsignedByte(stream);
        final int b4 = readUnsignedByte(stream);
        final long l = b4 << 24 | b3 << 16 | b2 << 8 | b1;
        return l & 0xffffffff;
    }

    // Signed 4 bytes
    private int readInt(final InputStream stream) throws IOException {
        final int b1 = readUnsignedByte(stream);
        final int b2 = readUnsignedByte(stream);
        final int b3 = readUnsignedByte(stream);
        final int b4 = readUnsignedByte(stream);
        return b4 << 24 | b3 << 16 | b2 << 8 | b1;
    }

    // Unsigned 4 bytes
    private long readDWord(final InputStream stream) throws IOException {
        return readUnsignedInt(stream);
    }

    // 32 bit signed value
    private int readLong(final InputStream stream) throws IOException {
        return readInt(stream);
    }
}
