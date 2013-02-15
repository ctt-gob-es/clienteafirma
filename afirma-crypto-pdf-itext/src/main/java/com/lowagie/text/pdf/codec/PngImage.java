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
 * This code is based on a series of source files originally released
 * by SUN in the context of the JAI project. The original code was released
 * under the BSD license in a specific wording. In a mail dating from
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

import java.awt.color.ICC_Profile;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.ImgRaw;
import com.lowagie.text.Utilities;
import com.lowagie.text.pdf.ByteBuffer;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfLiteral;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfString;

/** Reads a PNG image. All types of PNG can be read.
 * <p>
 * It is based in part in the JAI codec.
 *
 * @author  Paulo Soares (psoares@consiste.pt)
 */
public class PngImage {
/** Some PNG specific values. */
    public static final int[] PNGID = {137, 80, 78, 71, 13, 10, 26, 10};

/** A PNG marker. */
    private static final String IHDR = "IHDR";

/** A PNG marker. */
    private static final String PLTE = "PLTE";

/** A PNG marker. */
    private static final String IDAT = "IDAT";

/** A PNG marker. */
    private static final String IEND = "IEND";

/** A PNG marker. */
    private static final String tRNS = "tRNS";

/** A PNG marker. */
    private static final String pHYs = "pHYs";

/** A PNG marker. */
    private static final String gAMA = "gAMA";

/** A PNG marker. */
    private static final String cHRM = "cHRM";

/** A PNG marker. */
    private static final String sRGB = "sRGB";

/** A PNG marker. */
    private static final String iCCP = "iCCP";

    private static final int TRANSFERSIZE = 4096;
    private static final int PNG_FILTER_NONE = 0;
    private static final int PNG_FILTER_SUB = 1;
    private static final int PNG_FILTER_UP = 2;
    private static final int PNG_FILTER_AVERAGE = 3;
    private static final int PNG_FILTER_PAETH = 4;
    private static final PdfName intents[] = {PdfName.PERCEPTUAL,
        PdfName.RELATIVECOLORIMETRIC,PdfName.SATURATION,PdfName.ABSOLUTECOLORIMETRIC};

    private final InputStream is;
    private DataInputStream dataStream;
    private int width;
    private int height;
    private int bitDepth;
    private int colorType;
    private int compressionMethod;
    private int filterMethod;
    private int interlaceMethod;
    private final PdfDictionary additional = new PdfDictionary();
    private byte image[];
    private byte smask[];
    private byte trans[];
    private final NewByteArrayOutputStream idat = new NewByteArrayOutputStream();
    private int dpiX;
    private int dpiY;
    private float XYRatio;
    private boolean genBWMask;
    private boolean palShades;
    private int transRedGray = -1;
    private int transGreen = -1;
    private int transBlue = -1;
    private int inputBands;
    private int bytesPerPixel; // number of bytes per input pixel
    private byte colorTable[];
    private float gamma = 1f;
    private boolean hasCHRM = false;
    private float xW, yW, xR, yR, xG, yG, xB, yB;
    private PdfName intent;
    private ICC_Profile icc_profile;



    /** Creates a new instance of PngImage */
    private PngImage(final InputStream is) {
        this.is = is;
    }

    /** Reads a PNG from an url.
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

    /** Reads a PNG from a stream.
     * @param is the stream
     * @throws IOException on error
     * @return the image
     */
    private static Image getImage(final InputStream is) throws IOException {
        final PngImage png = new PngImage(is);
        return png.getImage();
    }



    /** Reads a PNG from a byte array.
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

    private boolean checkMarker(final String s) {
        if (s.length() != 4) {
			return false;
		}
        for (int k = 0; k < 4; ++k) {
            final char c = s.charAt(k);
            if ((c < 'a' || c > 'z') && (c < 'A' || c > 'Z')) {
				return false;
			}
        }
        return true;
    }

    private void readPng() throws IOException {
        for (final int element : PNGID) {
            if (element != this.is.read())	{
                throw new IOException("File is not a valid PNG.");
            }
        }
        final byte buffer[] = new byte[TRANSFERSIZE];
        while (true) {
            int len = getInt(this.is);
            final String marker = getString(this.is);
            if (len < 0 || !checkMarker(marker)) {
				throw new IOException("Corrupted PNG file.");
			}
            if (IDAT.equals(marker)) {
                int size;
                while (len != 0) {
                    size = this.is.read(buffer, 0, Math.min(len, TRANSFERSIZE));
                    if (size < 0) {
						return;
					}
                    this.idat.write(buffer, 0, size);
                    len -= size;
                }
            }
            else if (tRNS.equals(marker)) {
                switch (this.colorType) {
                    case 0:
                        if (len >= 2) {
                            len -= 2;
                            final int gray = getWord(this.is);
                            if (this.bitDepth == 16) {
								this.transRedGray = gray;
							} else {
								this.additional.put(PdfName.MASK, new PdfLiteral("["+gray+" "+gray+"]"));
							}
                        }
                        break;
                    case 2:
                        if (len >= 6) {
                            len -= 6;
                            final int red = getWord(this.is);
                            final int green = getWord(this.is);
                            final int blue = getWord(this.is);
                            if (this.bitDepth == 16) {
                                this.transRedGray = red;
                                this.transGreen = green;
                                this.transBlue = blue;
                            } else {
								this.additional.put(PdfName.MASK, new PdfLiteral("["+red+" "+red+" "+green+" "+green+" "+blue+" "+blue+"]"));
							}
                        }
                        break;
                    case 3:
                        if (len > 0) {
                            this.trans = new byte[len];
                            for (int k = 0; k < len; ++k) {
								this.trans[k] = (byte)this.is.read();
							}
                            len = 0;
                        }
                        break;
                }
                Utilities.skip(this.is, len);
            }
            else if (IHDR.equals(marker)) {
                this.width = getInt(this.is);
                this.height = getInt(this.is);

                this.bitDepth = this.is.read();
                this.colorType = this.is.read();
                this.compressionMethod = this.is.read();
                this.filterMethod = this.is.read();
                this.interlaceMethod = this.is.read();
            }
            else if (PLTE.equals(marker)) {
                if (this.colorType == 3) {
                    final PdfArray colorspace = new PdfArray();
                    colorspace.add(PdfName.INDEXED);
                    colorspace.add(getColorspace());
                    colorspace.add(new PdfNumber(len / 3 - 1));
                    final ByteBuffer colortable = new ByteBuffer();
                    while (len-- > 0) {
                        colortable.append_i(this.is.read());
                    }
                    colorspace.add(new PdfString(this.colorTable = colortable.toByteArray()));
                    this.additional.put(PdfName.COLORSPACE, colorspace);
                }
                else {
                    Utilities.skip(this.is, len);
                }
            }
            else if (pHYs.equals(marker)) {
                final int dx = getInt(this.is);
                final int dy = getInt(this.is);
                final int unit = this.is.read();
                if (unit == 1) {
                    this.dpiX = (int)(dx * 0.0254f + 0.5f);
                    this.dpiY = (int)(dy * 0.0254f + 0.5f);
                }
                else {
                    if (dy != 0) {
						this.XYRatio = (float)dx / (float)dy;
					}
                }
            }
            else if (cHRM.equals(marker)) {
                this.xW = getInt(this.is) / 100000f;
                this.yW = getInt(this.is) / 100000f;
                this.xR = getInt(this.is) / 100000f;
                this.yR = getInt(this.is) / 100000f;
                this.xG = getInt(this.is) / 100000f;
                this.yG = getInt(this.is) / 100000f;
                this.xB = getInt(this.is) / 100000f;
                this.yB = getInt(this.is) / 100000f;
                this.hasCHRM = !(Math.abs(this.xW)<0.0001f||Math.abs(this.yW)<0.0001f||Math.abs(this.xR)<0.0001f||Math.abs(this.yR)<0.0001f||Math.abs(this.xG)<0.0001f||Math.abs(this.yG)<0.0001f||Math.abs(this.xB)<0.0001f||Math.abs(this.yB)<0.0001f);
            }
            else if (sRGB.equals(marker)) {
                final int ri = this.is.read();
                this.intent = intents[ri];
                this.gamma = 2.2f;
                this.xW = 0.3127f;
                this.yW = 0.329f;
                this.xR = 0.64f;
                this.yR = 0.33f;
                this.xG = 0.3f;
                this.yG = 0.6f;
                this.xB = 0.15f;
                this.yB = 0.06f;
                this.hasCHRM = true;
            }
            else if (gAMA.equals(marker)) {
                final int gm = getInt(this.is);
                if (gm != 0) {
                    this.gamma = 100000f / gm;
                    if (!this.hasCHRM) {
                        this.xW = 0.3127f;
                        this.yW = 0.329f;
                        this.xR = 0.64f;
                        this.yR = 0.33f;
                        this.xG = 0.3f;
                        this.yG = 0.6f;
                        this.xB = 0.15f;
                        this.yB = 0.06f;
                        this.hasCHRM = true;
                    }
                }
            }
            else if (iCCP.equals(marker)) {
                do {
                    --len;
                } while (this.is.read() != 0);
                this.is.read();
                --len;
                byte icccom[] = new byte[len];
                int p = 0;
                while (len > 0) {
                    final int r = this.is.read(icccom, p, len);
                    if (r < 0) {
						throw new IOException("Premature end of file.");
					}
                    p += r;
                    len -= r;
                }
                final byte iccp[] = PdfReader.FlateDecode(icccom, true);
                icccom = null;
                try {
                    this.icc_profile = ICC_Profile.getInstance(iccp);
                }
                catch (final RuntimeException e) {
                    this.icc_profile = null;
                }
            }
            else if (IEND.equals(marker)) {
                break;
            }
            else {
                Utilities.skip(this.is, len);
            }
            Utilities.skip(this.is, 4);
        }
    }

    private PdfObject getColorspace() {
        if (this.icc_profile != null) {
            if ((this.colorType & 2) == 0) {
				return PdfName.DEVICEGRAY;
			} else {
				return PdfName.DEVICERGB;
			}
        }
        if (this.gamma == 1f && !this.hasCHRM) {
            if ((this.colorType & 2) == 0) {
				return PdfName.DEVICEGRAY;
			} else {
				return PdfName.DEVICERGB;
			}
        }
        else {
            final PdfArray array = new PdfArray();
            final PdfDictionary dic = new PdfDictionary();
            if ((this.colorType & 2) == 0) {
                if (this.gamma == 1f) {
					return PdfName.DEVICEGRAY;
				}
                array.add(PdfName.CALGRAY);
                dic.put(PdfName.GAMMA, new PdfNumber(this.gamma));
                dic.put(PdfName.WHITEPOINT, new PdfLiteral("[1 1 1]"));
                array.add(dic);
            }
            else {
                PdfObject wp = new PdfLiteral("[1 1 1]");
                array.add(PdfName.CALRGB);
                if (this.gamma != 1f) {
                    final PdfArray gm = new PdfArray();
                    final PdfNumber n = new PdfNumber(this.gamma);
                    gm.add(n);
                    gm.add(n);
                    gm.add(n);
                    dic.put(PdfName.GAMMA, gm);
                }
                if (this.hasCHRM) {
                    final float z = this.yW*((this.xG-this.xB)*this.yR-(this.xR-this.xB)*this.yG+(this.xR-this.xG)*this.yB);
                    final float YA = this.yR*((this.xG-this.xB)*this.yW-(this.xW-this.xB)*this.yG+(this.xW-this.xG)*this.yB)/z;
                    final float XA = YA*this.xR/this.yR;
                    final float ZA = YA*((1-this.xR)/this.yR-1);
                    final float YB = -this.yG*((this.xR-this.xB)*this.yW-(this.xW-this.xB)*this.yR+(this.xW-this.xR)*this.yB)/z;
                    final float XB = YB*this.xG/this.yG;
                    final float ZB = YB*((1-this.xG)/this.yG-1);
                    final float YC = this.yB*((this.xR-this.xG)*this.yW-(this.xW-this.xG)*this.yW+(this.xW-this.xR)*this.yG)/z;
                    final float XC = YC*this.xB/this.yB;
                    final float ZC = YC*((1-this.xB)/this.yB-1);
                    final float XW = XA+XB+XC;
                    final float YW = 1;//YA+YB+YC;
                    final float ZW = ZA+ZB+ZC;
                    final PdfArray wpa = new PdfArray();
                    wpa.add(new PdfNumber(XW));
                    wpa.add(new PdfNumber(YW));
                    wpa.add(new PdfNumber(ZW));
                    wp = wpa;
                    final PdfArray matrix = new PdfArray();
                    matrix.add(new PdfNumber(XA));
                    matrix.add(new PdfNumber(YA));
                    matrix.add(new PdfNumber(ZA));
                    matrix.add(new PdfNumber(XB));
                    matrix.add(new PdfNumber(YB));
                    matrix.add(new PdfNumber(ZB));
                    matrix.add(new PdfNumber(XC));
                    matrix.add(new PdfNumber(YC));
                    matrix.add(new PdfNumber(ZC));
                    dic.put(PdfName.MATRIX, matrix);
                }
                dic.put(PdfName.WHITEPOINT, wp);
                array.add(dic);
            }
            return array;
        }
    }

    private Image getImage() throws IOException {
        readPng();
        try {
            int pal0 = 0;
            int palIdx = 0;
            this.palShades = false;
            if (this.trans != null) {
                for (int k = 0; k < this.trans.length; ++k) {
                    final int n = this.trans[k] & 0xff;
                    if (n == 0) {
                        ++pal0;
                        palIdx = k;
                    }
                    if (n != 0 && n != 255) {
                        this.palShades = true;
                        break;
                    }
                }
            }
            if ((this.colorType & 4) != 0) {
				this.palShades = true;
			}
            this.genBWMask = !this.palShades && (pal0 > 1 || this.transRedGray >= 0);
            if (!this.palShades && !this.genBWMask && pal0 == 1) {
                this.additional.put(PdfName.MASK, new PdfLiteral("["+palIdx+" "+palIdx+"]"));
            }
            final boolean needDecode = this.interlaceMethod == 1 || this.bitDepth == 16 || (this.colorType & 4) != 0 || this.palShades || this.genBWMask;
            switch (this.colorType) {
                case 0:
                    this.inputBands = 1;
                    break;
                case 2:
                    this.inputBands = 3;
                    break;
                case 3:
                    this.inputBands = 1;
                    break;
                case 4:
                    this.inputBands = 2;
                    break;
                case 6:
                    this.inputBands = 4;
                    break;
            }
            if (needDecode) {
				decodeIdat();
			}
            int components = this.inputBands;
            if ((this.colorType & 4) != 0) {
				--components;
			}
            int bpc = this.bitDepth;
            if (bpc == 16) {
				bpc = 8;
			}
            Image img;
            if (this.image != null) {
                if (this.colorType == 3) {
					img = new ImgRaw(this.width, this.height, components, bpc, this.image);
				} else {
					img = Image.getInstance(this.width, this.height, components, bpc, this.image);
				}
            }
            else {
                img = new ImgRaw(this.width, this.height, components, bpc, this.idat.toByteArray());
                img.setDeflated(true);
                final PdfDictionary decodeparms = new PdfDictionary();
                decodeparms.put(PdfName.BITSPERCOMPONENT, new PdfNumber(this.bitDepth));
                decodeparms.put(PdfName.PREDICTOR, new PdfNumber(15));
                decodeparms.put(PdfName.COLUMNS, new PdfNumber(this.width));
                decodeparms.put(PdfName.COLORS, new PdfNumber(this.colorType == 3 || (this.colorType & 2) == 0 ? 1 : 3));
                this.additional.put(PdfName.DECODEPARMS, decodeparms);
            }
            if (this.additional.get(PdfName.COLORSPACE) == null) {
				this.additional.put(PdfName.COLORSPACE, getColorspace());
			}
            if (this.intent != null) {
				this.additional.put(PdfName.INTENT, this.intent);
			}
            if (this.additional.size() > 0) {
				img.setAdditional(this.additional);
			}
            if (this.icc_profile != null) {
				img.tagICC(this.icc_profile);
			}
            if (this.palShades) {
                final Image im2 = Image.getInstance(this.width, this.height, 1, 8, this.smask);
                im2.makeMask();
                img.setImageMask(im2);
            }
            if (this.genBWMask) {
                final Image im2 = Image.getInstance(this.width, this.height, 1, 1, this.smask);
                im2.makeMask();
                img.setImageMask(im2);
            }
            img.setDpi(this.dpiX, this.dpiY);
            img.setXYRatio(this.XYRatio);
            img.setOriginalType(Image.ORIGINAL_PNG);
            return img;
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    private void decodeIdat() {
        int nbitDepth = this.bitDepth;
        if (nbitDepth == 16) {
			nbitDepth = 8;
		}
        int size = -1;
        this.bytesPerPixel = this.bitDepth == 16 ? 2 : 1;
        switch (this.colorType) {
            case 0:
                size = (nbitDepth * this.width + 7) / 8 * this.height;
                break;
            case 2:
                size = this.width * 3 * this.height;
                this.bytesPerPixel *= 3;
                break;
            case 3:
                if (this.interlaceMethod == 1) {
					size = (nbitDepth * this.width + 7) / 8 * this.height;
				}
                this.bytesPerPixel = 1;
                break;
            case 4:
                size = this.width * this.height;
                this.bytesPerPixel *= 2;
                break;
            case 6:
                size = this.width * 3 * this.height;
                this.bytesPerPixel *= 4;
                break;
        }
        if (size >= 0) {
			this.image = new byte[size];
		}
        if (this.palShades) {
			this.smask = new byte[this.width * this.height];
		} else if (this.genBWMask) {
			this.smask = new byte[(this.width + 7) / 8 * this.height];
		}
        final ByteArrayInputStream bai = new ByteArrayInputStream(this.idat.getBuf(), 0, this.idat.size());
        final InputStream infStream = new InflaterInputStream(bai, new Inflater());
        this.dataStream = new DataInputStream(infStream);

        if (this.interlaceMethod != 1) {
            decodePass(0, 0, 1, 1, this.width, this.height);
        }
        else {
            decodePass(0, 0, 8, 8, (this.width + 7)/8, (this.height + 7)/8);
            decodePass(4, 0, 8, 8, (this.width + 3)/8, (this.height + 7)/8);
            decodePass(0, 4, 4, 8, (this.width + 3)/4, (this.height + 3)/8);
            decodePass(2, 0, 4, 4, (this.width + 1)/4, (this.height + 3)/4);
            decodePass(0, 2, 2, 4, (this.width + 1)/2, (this.height + 1)/4);
            decodePass(1, 0, 2, 2, this.width/2, (this.height + 1)/2);
            decodePass(0, 1, 1, 2, this.width, this.height/2);
        }

    }

    private void decodePass( final int xOffset, final int yOffset,
    final int xStep, final int yStep,
    final int passWidth, final int passHeight) {
        if (passWidth == 0 || passHeight == 0) {
            return;
        }

        final int bytesPerRow = (this.inputBands*passWidth*this.bitDepth + 7)/8;
        byte[] curr = new byte[bytesPerRow];
        byte[] prior = new byte[bytesPerRow];

        // Decode the (sub)image row-by-row
        int srcY, dstY;
        for (srcY = 0, dstY = yOffset;
        srcY < passHeight;
        srcY++, dstY += yStep) {
            // Read the filter type byte and a row of data
            int filter = 0;
            try {
                filter = this.dataStream.read();
                this.dataStream.readFully(curr, 0, bytesPerRow);
            } catch (final Exception e) {
                // empty on purpose
            }

            switch (filter) {
                case PNG_FILTER_NONE:
                    break;
                case PNG_FILTER_SUB:
                    decodeSubFilter(curr, bytesPerRow, this.bytesPerPixel);
                    break;
                case PNG_FILTER_UP:
                    decodeUpFilter(curr, prior, bytesPerRow);
                    break;
                case PNG_FILTER_AVERAGE:
                    decodeAverageFilter(curr, prior, bytesPerRow, this.bytesPerPixel);
                    break;
                case PNG_FILTER_PAETH:
                    decodePaethFilter(curr, prior, bytesPerRow, this.bytesPerPixel);
                    break;
                default:
                    // Error -- uknown filter type
                    throw new RuntimeException("PNG filter unknown.");
            }

            processPixels(curr, xOffset, xStep, dstY, passWidth);

            // Swap curr and prior
            final byte[] tmp = prior;
            prior = curr;
            curr = tmp;
        }
    }

    private void processPixels(final byte curr[], final int xOffset, final int step, final int y, final int width) {
        int srcX, dstX;

        final int out[] = getPixel(curr);
        int sizes = 0;
        switch (this.colorType) {
            case 0:
            case 3:
            case 4:
                sizes = 1;
                break;
            case 2:
            case 6:
                sizes = 3;
                break;
        }
        if (this.image != null) {
            dstX = xOffset;
            final int yStride = (sizes*this.width*(this.bitDepth == 16 ? 8 : this.bitDepth)+ 7)/8;
            for (srcX = 0; srcX < width; srcX++) {
                setPixel(this.image, out, this.inputBands * srcX, sizes, dstX, y, this.bitDepth, yStride);
                dstX += step;
            }
        }
        if (this.palShades) {
            if ((this.colorType & 4) != 0) {
                if (this.bitDepth == 16) {
                    for (int k = 0; k < width; ++k) {
						out[k * this.inputBands + sizes] >>>= 8;
					}
                }
                final int yStride = this.width;
                dstX = xOffset;
                for (srcX = 0; srcX < width; srcX++) {
                    setPixel(this.smask, out, this.inputBands * srcX + sizes, 1, dstX, y, 8, yStride);
                    dstX += step;
                }
            }
            else { //colorType 3
                final int yStride = this.width;
                final int v[] = new int[1];
                dstX = xOffset;
                for (srcX = 0; srcX < width; srcX++) {
                    final int idx = out[srcX];
                    if (idx < this.trans.length) {
						v[0] = this.trans[idx];
					}
					else {
						v[0] = 255; // Patrick Valsecchi
					}
                    setPixel(this.smask, v, 0, 1, dstX, y, 8, yStride);
                    dstX += step;
                }
            }
        }
        else if (this.genBWMask) {
            switch (this.colorType) {
                case 3: {
                    final int yStride = (this.width + 7) / 8;
                    final int v[] = new int[1];
                    dstX = xOffset;
                    for (srcX = 0; srcX < width; srcX++) {
                        final int idx = out[srcX];
                        v[0] = idx < this.trans.length && this.trans[idx] == 0 ? 1 : 0;
                        setPixel(this.smask, v, 0, 1, dstX, y, 1, yStride);
                        dstX += step;
                    }
                    break;
                }
                case 0: {
                    final int yStride = (this.width + 7) / 8;
                    final int v[] = new int[1];
                    dstX = xOffset;
                    for (srcX = 0; srcX < width; srcX++) {
                        final int g = out[srcX];
                        v[0] = g == this.transRedGray ? 1 : 0;
                        setPixel(this.smask, v, 0, 1, dstX, y, 1, yStride);
                        dstX += step;
                    }
                    break;
                }
                case 2: {
                    final int yStride = (this.width + 7) / 8;
                    final int v[] = new int[1];
                    dstX = xOffset;
                    for (srcX = 0; srcX < width; srcX++) {
                        final int markRed = this.inputBands * srcX;
                        v[0] = out[markRed] == this.transRedGray && out[markRed + 1] == this.transGreen
                            && out[markRed + 2] == this.transBlue ? 1 : 0;
                        setPixel(this.smask, v, 0, 1, dstX, y, 1, yStride);
                        dstX += step;
                    }
                    break;
                }
            }
        }
    }



    private static void setPixel(final byte image[], final int data[], final int offset, final int size, final int x, final int y, final int bitDepth, final int bytesPerRow) {
        if (bitDepth == 8) {
            final int pos = bytesPerRow * y + size * x;
            for (int k = 0; k < size; ++k) {
				image[pos + k] = (byte)data[k + offset];
			}
        }
        else if (bitDepth == 16) {
            final int pos = bytesPerRow * y + size * x;
            for (int k = 0; k < size; ++k) {
				image[pos + k] = (byte)(data[k + offset] >>> 8);
			}
        }
        else {
            final int pos = bytesPerRow * y + x / (8 / bitDepth);
            final int v = data[offset] << 8 - bitDepth * (x % (8 / bitDepth))- bitDepth;
            image[pos] |= v;
        }
    }

    private int[] getPixel(final byte curr[]) {
        switch (this.bitDepth) {
            case 8: {
                final int out[] = new int[curr.length];
                for (int k = 0; k < out.length; ++k) {
					out[k] = curr[k] & 0xff;
				}
                return out;
            }
            case 16: {
                final int out[] = new int[curr.length / 2];
                for (int k = 0; k < out.length; ++k) {
					out[k] = ((curr[k * 2] & 0xff) << 8) + (curr[k * 2 + 1] & 0xff);
				}
                return out;
            }
            default: {
                final int out[] = new int[curr.length * 8 / this.bitDepth];
                int idx = 0;
                final int passes = 8 / this.bitDepth;
                final int mask = (1 << this.bitDepth) - 1;
                for (final byte element : curr) {
                    for (int j = passes - 1; j >= 0; --j) {
                        out[idx++] = element >>> this.bitDepth * j & mask;
                    }
                }
                return out;
            }
        }
    }

    private static void decodeSubFilter(final byte[] curr, final int count, final int bpp) {
        for (int i = bpp; i < count; i++) {
            int val;

            val = curr[i] & 0xff;
            val += curr[i - bpp] & 0xff;

            curr[i] = (byte)val;
        }
    }

    private static void decodeUpFilter(final byte[] curr, final byte[] prev,
    final int count) {
        for (int i = 0; i < count; i++) {
            final int raw = curr[i] & 0xff;
            final int prior = prev[i] & 0xff;

            curr[i] = (byte)(raw + prior);
        }
    }

    private static void decodeAverageFilter(final byte[] curr, final byte[] prev,
    final int count, final int bpp) {
        int raw, priorPixel, priorRow;

        for (int i = 0; i < bpp; i++) {
            raw = curr[i] & 0xff;
            priorRow = prev[i] & 0xff;

            curr[i] = (byte)(raw + priorRow/2);
        }

        for (int i = bpp; i < count; i++) {
            raw = curr[i] & 0xff;
            priorPixel = curr[i - bpp] & 0xff;
            priorRow = prev[i] & 0xff;

            curr[i] = (byte)(raw + (priorPixel + priorRow)/2);
        }
    }

    private static int paethPredictor(final int a, final int b, final int c) {
        final int p = a + b - c;
        final int pa = Math.abs(p - a);
        final int pb = Math.abs(p - b);
        final int pc = Math.abs(p - c);

        if (pa <= pb && pa <= pc) {
            return a;
        } else if (pb <= pc) {
            return b;
        } else {
            return c;
        }
    }

    private static void decodePaethFilter(final byte[] curr, final byte[] prev,
    final int count, final int bpp) {
        int raw, priorPixel, priorRow, priorRowPixel;

        for (int i = 0; i < bpp; i++) {
            raw = curr[i] & 0xff;
            priorRow = prev[i] & 0xff;

            curr[i] = (byte)(raw + priorRow);
        }

        for (int i = bpp; i < count; i++) {
            raw = curr[i] & 0xff;
            priorPixel = curr[i - bpp] & 0xff;
            priorRow = prev[i] & 0xff;
            priorRowPixel = prev[i - bpp] & 0xff;

            curr[i] = (byte)(raw + paethPredictor(priorPixel,
            priorRow,
            priorRowPixel));
        }
    }

    private static class NewByteArrayOutputStream extends ByteArrayOutputStream {
        public byte[] getBuf() {
            return this.buf;
        }
    }

/**
 * Gets an <CODE>int</CODE> from an <CODE>InputStream</CODE>.
 *
 * @param		is      an <CODE>InputStream</CODE>
 * @return		the value of an <CODE>int</CODE>
 */

    private static final int getInt(final InputStream is) throws IOException {
        return (is.read() << 24) + (is.read() << 16) + (is.read() << 8) + is.read();
    }

/**
 * Gets a <CODE>word</CODE> from an <CODE>InputStream</CODE>.
 *
 * @param		is      an <CODE>InputStream</CODE>
 * @return		the value of an <CODE>int</CODE>
 */

    private static final int getWord(final InputStream is) throws IOException {
        return (is.read() << 8) + is.read();
    }

/**
 * Gets a <CODE>String</CODE> from an <CODE>InputStream</CODE>.
 *
 * @param		is      an <CODE>InputStream</CODE>
 * @return		the value of an <CODE>int</CODE>
 */

    private static final String getString(final InputStream is) throws IOException {
        final StringBuffer buf = new StringBuffer();
        for (int i = 0; i < 4; i++) {
            buf.append((char)is.read());
        }
        return buf.toString();
    }

}
