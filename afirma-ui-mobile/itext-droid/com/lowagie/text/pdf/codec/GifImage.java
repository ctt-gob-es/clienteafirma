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
package com.lowagie.text.pdf.codec;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.ImgRaw;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;
import com.lowagie.text.pdf.PdfString;

/** Reads gif images of all types. All the images in a gif are read in the constructors
 * and can be retrieved with other methods.
 * @author Paulo Soares (psoares@consiste.pt)
 */
public class GifImage {

    private DataInputStream in;
    private int width;            // full image width
    private int height;           // full image height
    private boolean gctFlag;      // global color table used





    private boolean lctFlag;      // local color table flag
    private boolean interlace;    // interlace flag


    private int ix, iy, iw, ih;   // current image rectangle

    private final byte[] block = new byte[256];  // current data block
    private int blockSize = 0;    // block size

    // last graphic control extension info
    private int dispose = 0;   // 0=no action; 1=leave in place; 2=restore to bg; 3=restore to prev
    private boolean transparency = false;   // use transparent color

    private int transIndex;       // transparent color index

    private static final int MaxStackSize = 4096;   // max decoder pixel stack size

    // LZW decoder working arrays
    private short[] prefix;
    private byte[] suffix;
    private byte[] pixelStack;


    private byte m_out[];
    private int m_bpc;
    private int m_gbpc;
    private byte m_global_table[];

    private byte m_curr_table[];
    private int m_line_stride;
    private byte fromData[];
    private URL fromUrl;


    private final ArrayList frames = new ArrayList();     // frames read from current file

    /** Reads gif images from an URL.
     * @param url the URL
     * @throws IOException on error
     */
    public GifImage(final URL url) throws IOException {
        this.fromUrl = url;
        InputStream is = null;
        try {
            is = url.openStream();
            process(is);
        }
        finally {
            if (is != null) {
                is.close();
            }
        }
    }



    /** Reads gif images from a byte array.
     * @param data the byte array
     * @throws IOException on error
     */
    public GifImage(final byte data[]) throws IOException {
        this.fromData = data;
        InputStream is = null;
        try {
            is = new ByteArrayInputStream(data);
            process(is);
        }
        finally {
            if (is != null) {
                is.close();
            }
        }
    }



    /** Gets the number of frames the gif has.
     * @return the number of frames the gif has
     */
    public int getFrameCount() {
        return this.frames.size();
    }

    /** Gets the image from a frame. The first frame is 1.
     * @param frame the frame to get the image from
     * @return the image
     */
    public Image getImage(final int frame) {
        final GifFrame gf = (GifFrame)this.frames.get(frame - 1);
        return gf.image;
    }



    /** Gets the logical screen. The images may be smaller and placed
     * in some position in this screen to playback some animation.
     * No image will be be bigger that this.
     * @return the logical screen dimensions as [x,y]
     */
    public int[] getLogicalScreen() {
        return new int[]{this.width, this.height};
    }

    private void process(final InputStream is) throws IOException {
        this.in = new DataInputStream(new BufferedInputStream(is));
        readHeader();
        readContents();
        if (this.frames.isEmpty()) {
			throw new IOException("The file does not contain any valid image.");
		}
    }

    /**
     * Reads GIF file header information.
     */
    private void readHeader() throws IOException {
        String id = "";
        for (int i = 0; i < 6; i++) {
			id += (char)this.in.read();
		}
        if (!id.startsWith("GIF8")) {
            throw new IOException("Gif signature nor found.");
        }

        readLSD();
        if (this.gctFlag) {
            this.m_global_table = readColorTable(this.m_gbpc);
        }
    }

    /**
     * Reads Logical Screen Descriptor
     */
    private void readLSD() throws IOException {

        // logical screen size
        this.width = readShort();
        this.height = readShort();

        // packed fields
        final int packed = this.in.read();
        this.gctFlag = (packed & 0x80) != 0;      // 1   : global color table flag
        this.m_gbpc = (packed & 7) + 1;

    }

    /**
     * Reads next 16-bit value, LSB first
     */
    private int readShort() throws IOException {
        // read 16-bit value, LSB first
        return this.in.read() | this.in.read() << 8;
    }

    /**
     * Reads next variable length block from input.
     *
     * @return number of bytes stored in "buffer"
     */
    private int readBlock() throws IOException {
        this.blockSize = this.in.read();
        if (this.blockSize <= 0) {
			return this.blockSize = 0;
		}
        for (int k = 0; k < this.blockSize; ++k) {
            final int v = this.in.read();
            if (v < 0) {
                return this.blockSize = k;
            }
            this.block[k] = (byte)v;
        }
        return this.blockSize;
    }

    private byte[] readColorTable(int bpc) throws IOException {
        final int ncolors = 1 << bpc;
        final int nbytes = 3*ncolors;
        bpc = newBpc(bpc);
        final byte table[] = new byte[(1 << bpc) * 3];
        this.in.readFully(table, 0, nbytes);
        return table;
    }


    static private int newBpc(final int bpc) {
        switch (bpc) {
            case 1:
            case 2:
            case 4:
                break;
            case 3:
                return 4;
            default:
                return 8;
        }
        return bpc;
    }

    private void readContents() throws IOException {
        // read GIF file content blocks
        boolean done = false;
        while (!done) {
            int code = this.in.read();
            switch (code) {

                case 0x2C:    // image separator
                    readImage();
                    break;

                case 0x21:    // extension
                    code = this.in.read();
                    switch (code) {

                        case 0xf9:    // graphics control extension
                            readGraphicControlExt();
                            break;

                        case 0xff:    // application extension
                            readBlock();
                            skip();        // don't care
                            break;

                        default:    // uninteresting extension
                            skip();
                    }
                    break;

                default:
                    done = true;
                    break;
            }
        }
    }

    /**
     * Reads next frame image
     */
    private void readImage() throws IOException {
        this.ix = readShort();    // (sub)image position & size
        this.iy = readShort();
        this.iw = readShort();
        this.ih = readShort();

        final int packed = this.in.read();
        this.lctFlag = (packed & 0x80) != 0;     // 1 - local color table flag
        this.interlace = (packed & 0x40) != 0;   // 2 - interlace flag
        // 3 - sort flag
        // 4-5 - reserved

        this.m_bpc = newBpc(this.m_gbpc);
        if (this.lctFlag) {
            this.m_curr_table = readColorTable((packed & 7) + 1);   // read table
            this.m_bpc = newBpc((packed & 7) + 1);
        }
        else {
            this.m_curr_table = this.m_global_table;
        }
        if (this.transparency && this.transIndex >= this.m_curr_table.length / 3) {
			this.transparency = false;
		}
        if (this.transparency && this.m_bpc == 1) { // Acrobat 5.05 doesn't like this combination
            final byte tp[] = new byte[12];
            System.arraycopy(this.m_curr_table, 0, tp, 0, 6);
            this.m_curr_table = tp;
            this.m_bpc = 2;
        }
        final boolean skipZero = decodeImageData();   // decode pixel data
        if (!skipZero) {
			skip();
		}

        Image img = null;
        try {
            img = new ImgRaw(this.iw, this.ih, 1, this.m_bpc, this.m_out);
            final PdfArray colorspace = new PdfArray();
            colorspace.add(PdfName.INDEXED);
            colorspace.add(PdfName.DEVICERGB);
            final int len = this.m_curr_table.length;
            colorspace.add(new PdfNumber(len / 3 - 1));
            colorspace.add(new PdfString(this.m_curr_table));
            final PdfDictionary ad = new PdfDictionary();
            ad.put(PdfName.COLORSPACE, colorspace);
            img.setAdditional(ad);
            if (this.transparency) {
                img.setTransparency(new int[]{this.transIndex, this.transIndex});
            }
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
        img.setOriginalType(Image.ORIGINAL_GIF);
        img.setOriginalData(this.fromData);
        img.setUrl(this.fromUrl);
        final GifFrame gf = new GifFrame();
        gf.image = img;
        this.frames.add(gf);   // add image to frame list

        //resetFrame();

    }

    private boolean decodeImageData() throws IOException {
        final int NullCode = -1;
        final int npix = this.iw * this.ih;
        int available, clear, code_mask, code_size, end_of_information, in_code, old_code,
        bits, code, count, i, datum, data_size, first, top, bi;
        boolean skipZero = false;

        if (this.prefix == null) {
			this.prefix = new short[MaxStackSize];
		}
        if (this.suffix == null) {
			this.suffix = new byte[MaxStackSize];
		}
        if (this.pixelStack == null) {
			this.pixelStack = new byte[MaxStackSize+1];
		}

        this.m_line_stride = (this.iw * this.m_bpc + 7) / 8;
        this.m_out = new byte[this.m_line_stride * this.ih];
        int pass = 1;
        int inc = this.interlace ? 8 : 1;
        int line = 0;
        int xpos = 0;

        //  Initialize GIF data stream decoder.

        data_size = this.in.read();
        clear = 1 << data_size;
        end_of_information = clear + 1;
        available = clear + 2;
        old_code = NullCode;
        code_size = data_size + 1;
        code_mask = (1 << code_size) - 1;
        for (code = 0; code < clear; code++) {
            this.prefix[code] = 0;
            this.suffix[code] = (byte) code;
        }

        //  Decode GIF pixel stream.

        datum = bits = count = first = top = bi = 0;

        for (i = 0; i < npix; ) {
            if (top == 0) {
                if (bits < code_size) {
                    //  Load bytes until there are enough bits for a code.
                    if (count == 0) {
                        // Read a new data block.
                        count = readBlock();
                        if (count <= 0) {
                            skipZero = true;
                            break;
                        }
                        bi = 0;
                    }
                    datum += (this.block[bi] & 0xff) << bits;
                    bits += 8;
                    bi++;
                    count--;
                    continue;
                }

                //  Get the next code.

                code = datum & code_mask;
                datum >>= code_size;
                bits -= code_size;

                //  Interpret the code

                if (code > available || code == end_of_information) {
					break;
				}
                if (code == clear) {
                    //  Reset decoder.
                    code_size = data_size + 1;
                    code_mask = (1 << code_size) - 1;
                    available = clear + 2;
                    old_code = NullCode;
                    continue;
                }
                if (old_code == NullCode) {
                    this.pixelStack[top++] = this.suffix[code];
                    old_code = code;
                    first = code;
                    continue;
                }
                in_code = code;
                if (code == available) {
                    this.pixelStack[top++] = (byte) first;
                    code = old_code;
                }
                while (code > clear) {
                    this.pixelStack[top++] = this.suffix[code];
                    code = this.prefix[code];
                }
                first = this.suffix[code] & 0xff;

                //  Add a new string to the string table,

                if (available >= MaxStackSize) {
					break;
				}
                this.pixelStack[top++] = (byte) first;
                this.prefix[available] = (short) old_code;
                this.suffix[available] = (byte) first;
                available++;
                if ((available & code_mask) == 0 && available < MaxStackSize) {
                    code_size++;
                    code_mask += available;
                }
                old_code = in_code;
            }

            //  Pop a pixel off the pixel stack.

            top--;
            i++;

            setPixel(xpos, line, this.pixelStack[top]);
            ++xpos;
            if (xpos >= this.iw) {
                xpos = 0;
                line += inc;
                if (line >= this.ih) {
                    if (this.interlace) {
                        do {
                            pass++;
                            switch (pass) {
                                case 2:
                                    line = 4;
                                    break;
                                case 3:
                                    line = 2;
                                    inc = 4;
                                    break;
                                case 4:
                                    line = 1;
                                    inc = 2;
                                    break;
                                default: // this shouldn't happen
                                    line = this.ih - 1;
                                    inc = 0;
                            }
                        } while (line >= this.ih);
                    }
                    else {
                        line = this.ih - 1; // this shouldn't happen
                        inc = 0;
                    }
                }
            }
        }
        return skipZero;
    }


    private void setPixel(final int x, final int y, final int v) {
        if (this.m_bpc == 8) {
            final int pos = x + this.iw * y;
            this.m_out[pos] = (byte)v;
        }
        else {
            final int pos = this.m_line_stride * y + x / (8 / this.m_bpc);
            final int vout = v << 8 - this.m_bpc * (x % (8 / this.m_bpc))- this.m_bpc;
            this.m_out[pos] |= vout;
        }
    }



    /**
     * Reads Graphics Control Extension values
     */
    private void readGraphicControlExt() throws IOException {
        this.in.read();    // block size
        final int packed = this.in.read();   // packed fields
        this.dispose = (packed & 0x1c) >> 2;   // disposal method
        if (this.dispose == 0)
		 {
			this.dispose = 1;   // elect to keep old image if discretionary
		}
        this.transparency = (packed & 1) != 0;
        this.transIndex = this.in.read();        // transparent color index
        this.in.read();                     // block terminator
    }

    /**
     * Skips variable length blocks up to and including
     * next zero length block.
     */
    private void skip() throws IOException {
        do {
            readBlock();
        } while (this.blockSize > 0);
    }

    private static class GifFrame {
        private Image image;


    }
}
