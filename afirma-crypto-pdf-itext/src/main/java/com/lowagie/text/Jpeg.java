/*
 * $Id: Jpeg.java 3970 2009-06-16 08:09:54Z blowagie $
 *
 * Copyright 1999, 2000, 2001, 2002 by Bruno Lowagie.
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

import java.awt.color.ICC_Profile;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

/**
 * An <CODE>Jpeg</CODE> is the representation of a graphic element (JPEG)
 * that has to be inserted into the document
 *
 * @see		Element
 * @see		Image
 */

public class Jpeg extends Image {

    // public static final membervariables

    /** This is a type of marker. */
    private static final int NOT_A_MARKER = -1;

    /** This is a type of marker. */
    private static final int VALID_MARKER = 0;

    /** Acceptable Jpeg markers. */
    private static final int[] VALID_MARKERS = {0xC0, 0xC1, 0xC2};

    /** This is a type of marker. */
    private static final int UNSUPPORTED_MARKER = 1;

    /** Unsupported Jpeg markers. */
    private static final int[] UNSUPPORTED_MARKERS = {0xC3, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCD, 0xCE, 0xCF};

    /** This is a type of marker. */
    private static final int NOPARAM_MARKER = 2;

    /** Jpeg markers without additional parameters. */
    private static final int[] NOPARAM_MARKERS = {0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0x01};

    /** Marker value */
    private static final int M_APP0 = 0xE0;
    /** Marker value */
    private static final int M_APP2 = 0xE2;
    /** Marker value */
    private static final int M_APPE = 0xEE;

    /** sequence that is used in all Jpeg files */
    private static final byte JFIF_ID[] = {0x4A, 0x46, 0x49, 0x46, 0x00};

    private byte[][] icc;
    // Constructors

    /**
     * Constructs a <CODE>Jpeg</CODE>-object, using an <VAR>url</VAR>.
     *
     * @param		url			the <CODE>URL</CODE> where the image can be found
     * @throws BadElementException
     * @throws IOException
     */
    Jpeg(final URL url) throws BadElementException, IOException {
        super(url);
        processParameters();
    }

    /**
     * Constructs a <CODE>Jpeg</CODE>-object from memory.
     *
     * @param		img		the memory image
     * @throws BadElementException
     * @throws IOException
     */

    public Jpeg(final byte[] img) throws BadElementException, IOException {
        super((URL)null);
        this.rawData = img;
        this.originalData = img;
        processParameters();
    }

    // private static methods

    /**
     * Reads a short from the <CODE>InputStream</CODE>.
     *
     * @param	is		the <CODE>InputStream</CODE>
     * @return	an int
     * @throws IOException
     */
    private static final int getShort(final InputStream is) throws IOException {
        return (is.read() << 8) + is.read();
    }

    /**
     * Returns a type of marker.
     *
     * @param	marker      an int
     * @return	a type: <VAR>VALID_MARKER</CODE>, <VAR>UNSUPPORTED_MARKER</VAR> or <VAR>NOPARAM_MARKER</VAR>
     */
    private static final int marker(final int marker) {
        for (final int element : VALID_MARKERS) {
            if (marker == element) {
                return VALID_MARKER;
            }
        }
        for (final int element : NOPARAM_MARKERS) {
            if (marker == element) {
                return NOPARAM_MARKER;
            }
        }
        for (final int element : UNSUPPORTED_MARKERS) {
            if (marker == element) {
                return UNSUPPORTED_MARKER;
            }
        }
        return NOT_A_MARKER;
    }

    // private methods

    /**
     * This method checks if the image is a valid JPEG and processes some parameters.
     * @throws BadElementException
     * @throws IOException
     */
    private void processParameters() throws BadElementException, IOException {
        this.type = JPEG;
        this.originalType = ORIGINAL_JPEG;
        InputStream is = null;
        try {
            String errorID;
            if (this.rawData == null){
                is = this.url.openStream();
                errorID = this.url.toString();
            }
            else{
                is = new java.io.ByteArrayInputStream(this.rawData);
                errorID = "Byte array";
            }
            if (is.read() != 0xFF || is.read() != 0xD8)	{
                throw new BadElementException(errorID + " is not a valid JPEG-file.");
            }
            boolean firstPass = true;
            int len;
            while (true) {
                final int v = is.read();
                if (v < 0) {
					throw new IOException("Premature EOF while reading JPG.");
				}
                if (v == 0xFF) {
                    final int marker = is.read();
                    if (firstPass && marker == M_APP0) {
                        firstPass = false;
                        len = getShort(is);
                        if (len < 16) {
                            Utilities.skip(is, len - 2);
                            continue;
                        }
                        final byte bcomp[] = new byte[JFIF_ID.length];
                        final int r = is.read(bcomp);
                        if (r != bcomp.length) {
							throw new BadElementException(errorID + " corrupted JFIF marker.");
						}
                        boolean found = true;
                        for (int k = 0; k < bcomp.length; ++k) {
                            if (bcomp[k] != JFIF_ID[k]) {
                                found = false;
                                break;
                            }
                        }
                        if (!found) {
                            Utilities.skip(is, len - 2 - bcomp.length);
                            continue;
                        }
                        Utilities.skip(is, 2);
                        final int units = is.read();
                        final int dx = getShort(is);
                        final int dy = getShort(is);
                        if (units == 1) {
                            this.dpiX = dx;
                            this.dpiY = dy;
                        }
                        else if (units == 2) {
                            this.dpiX = (int)(dx * 2.54f + 0.5f);
                            this.dpiY = (int)(dy * 2.54f + 0.5f);
                        }
                        Utilities.skip(is, len - 2 - bcomp.length - 7);
                        continue;
                    }
                    if (marker == M_APPE) {
                        len = getShort(is) - 2;
                        final byte[] byteappe = new byte[len];
                        for (int k = 0; k < len; ++k) {
                            byteappe[k] = (byte)is.read();
                        }
                        if (byteappe.length >= 12) {
                            final String appe = new String(byteappe, 0, 5, "ISO-8859-1");
                            if (appe.equals("Adobe")) {
                                this.invert = true;
                            }
                        }
                        continue;
                    }
                    if (marker == M_APP2) {
                        len = getShort(is) - 2;
                        final byte[] byteapp2 = new byte[len];
                        for (int k = 0; k < len; ++k) {
                            byteapp2[k] = (byte)is.read();
                        }
                        if (byteapp2.length >= 14) {
                            final String app2 = new String(byteapp2, 0, 11, "ISO-8859-1");
                            if (app2.equals("ICC_PROFILE")) {
                                final int order = byteapp2[12] & 0xff;
                                final int count = byteapp2[13] & 0xff;
                                if (this.icc == null) {
									this.icc = new byte[count][];
								}
                                this.icc[order - 1] = byteapp2;
                            }
                        }
                        continue;
                    }
                    firstPass = false;
                    final int markertype = marker(marker);
                    if (markertype == VALID_MARKER) {
                        Utilities.skip(is, 2);
                        if (is.read() != 0x08) {
                            throw new BadElementException(errorID + " must have 8 bits per component.");
                        }
                        this.scaledHeight = getShort(is);
                        setTop(this.scaledHeight);
                        this.scaledWidth = getShort(is);
                        setRight(this.scaledWidth);
                        this.colorspace = is.read();
                        this.bpc = 8;
                        break;
                    }
                    else if (markertype == UNSUPPORTED_MARKER) {
                        throw new BadElementException(errorID + ": unsupported JPEG marker: " + marker);
                    }
                    else if (markertype != NOPARAM_MARKER) {
                        Utilities.skip(is, getShort(is) - 2);
                    }
                }
            }
        }
        finally {
            if (is != null) {
                is.close();
            }
        }
        this.plainWidth = getWidth();
        this.plainHeight = getHeight();
        if (this.icc != null) {
            int total = 0;
            for (int k = 0; k < this.icc.length; ++k) {
                if (this.icc[k] == null) {
                    this.icc = null;
                    return;
                }
                total += this.icc[k].length - 14;
            }
            final byte[] ficc = new byte[total];
            total = 0;
            for (final byte[] element : this.icc) {
                System.arraycopy(element, 14, ficc, total, element.length - 14);
                total += element.length - 14;
            }
            try {
            	final ICC_Profile icc_prof = ICC_Profile.getInstance(ficc);
            	tagICC(icc_prof);
            }
            catch(final IllegalArgumentException e) {
            	// ignore ICC profile if it's invalid.
            }
            this.icc = null;
        }
    }
}
