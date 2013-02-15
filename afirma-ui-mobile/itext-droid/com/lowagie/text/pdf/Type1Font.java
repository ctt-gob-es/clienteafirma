/*
 * $Id: Type1Font.java 3718 2009-02-23 16:56:55Z blowagie $
 *
 * Copyright 2001-2006 Paulo Soares
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
import java.util.HashMap;
import java.util.StringTokenizer;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.fonts.FontsResourceAnchor;

/** Reads a Type1 font
 *
 * @author Paulo Soares (psoares@consiste.pt)
 */
class Type1Font extends BaseFont
{
    private static FontsResourceAnchor resourceAnchor;

    /** The PFB file if the input was made with a <CODE>byte</CODE> array.
     */
    private byte pfb[];
/** The Postscript font name.
 */
    private String FontName;
/** The full name of the font.
 */
    private String FullName;
/** The family name of the font.
 */
    private String FamilyName;
/** The weight of the font: normal, bold, etc.
 */
    private String Weight = "";
/** The italic angle of the font, usually 0.0 or negative.
 */
    private float ItalicAngle = 0.0f;
/** <CODE>true</CODE> if all the characters have the same
 *  width.
 */
    private boolean IsFixedPitch = false;
/** The character set of the font.
 */
    private String CharacterSet;
/** The llx of the FontBox.
 */
    private int llx = -50;
/** The lly of the FontBox.
 */
    private int lly = -200;
/** The lurx of the FontBox.
 */
    private int urx = 1000;
/** The ury of the FontBox.
 */
    private int ury = 900;
/** The underline position.
 */
    private int UnderlinePosition = -100;
/** The underline thickness.
 */
    private int UnderlineThickness = 50;
/** The font's encoding name. This encoding is 'StandardEncoding' or
 *  'AdobeStandardEncoding' for a font that can be totally encoded
 *  according to the characters names. For all other names the
 *  font is treated as symbolic.
 */
    private String EncodingScheme = "FontSpecific";
/** A variable.
 */
    private int CapHeight = 700;
/** A variable.
 */
    private int XHeight = 480;
/** A variable.
 */
    private int Ascender = 800;
/** A variable.
 */
    private int Descender = -200;
/** A variable.
 */
    private int StdHW;
/** A variable.
 */
    private int StdVW = 80;

/** Represents the section CharMetrics in the AFM file. Each
 *  value of this array contains a <CODE>Object[4]</CODE> with an
 *  Integer, Integer, String and int[]. This is the code, width, name and char bbox.
 *  The key is the name of the char and also an Integer with the char number.
 */
    private final HashMap CharMetrics = new HashMap();
/** Represents the section KernPairs in the AFM file. The key is
 *  the name of the first character and the value is a <CODE>Object[]</CODE>
 *  with 2 elements for each kern pair. Position 0 is the name of
 *  the second character and position 1 is the kerning distance. This is
 *  repeated for all the pairs.
 */
    private final HashMap KernPairs = new HashMap();
/** The file in use.
 */
    private final String fileName;
/** <CODE>true</CODE> if this font is one of the 14 built in fonts.
 */
    private boolean builtinFont = false;
/** Types of records in a PFB file. ASCII is 1 and BINARY is 2.
 *  They have to appear in the PFB file in this sequence.
 */
    private static final int PFB_TYPES[] = {1, 2, 1};

    /** Creates a new Type1 font.
     * @param ttfAfm the AFM file if the input is made with a <CODE>byte</CODE> array
     * @param pfb the PFB file if the input is made with a <CODE>byte</CODE> array
     * @param afmFile the name of one of the 14 built-in fonts or the location of an AFM file. The file must end in '.afm'
     * @param enc the encoding to be applied to this font
     * @param emb true if the font is to be embedded in the PDF
     * @throws DocumentException the AFM file is invalid
     * @throws IOException the AFM file could not be read
     * @since	2.1.5
     */
    Type1Font(final String afmFile, final String enc, final boolean emb, final byte ttfAfm[], final byte pfb[], final boolean forceRead)
    	throws DocumentException, IOException {
        if (emb && ttfAfm != null && pfb == null) {
			throw new DocumentException("Two byte arrays are needed if the Type1 font is embedded.");
		}
        if (emb && ttfAfm != null) {
			this.pfb = pfb;
		}
        this.encoding = enc;
        this.embedded = emb;
        this.fileName = afmFile;
        this.fontType = FONT_TYPE_T1;
        RandomAccessFileOrArray rf = null;
        InputStream is = null;
        if (BuiltinFonts14.containsKey(afmFile)) {
            this.embedded = false;
            this.builtinFont = true;
            byte buf[] = new byte[1024];
            try {
                if (resourceAnchor == null) {
					resourceAnchor = new FontsResourceAnchor();
				}
                is = getResourceStream(RESOURCE_PATH + afmFile + ".afm", resourceAnchor.getClass().getClassLoader());
                if (is == null) {
                    final String msg = afmFile + " not found as resource. (The *.afm files must exist as resources in the package com.lowagie.text.pdf.fonts)";
                    System.err.println(msg);
                    throw new DocumentException(msg);
                }
                final ByteArrayOutputStream out = new ByteArrayOutputStream();
                while (true) {
                    final int size = is.read(buf);
                    if (size < 0) {
						break;
					}
                    out.write(buf, 0, size);
                }
                buf = out.toByteArray();
            }
            finally {
                if (is != null) {
                    try {
                        is.close();
                    }
                    catch (final Exception e) {
                        // empty on purpose
                    }
                }
            }
            try {
                rf = new RandomAccessFileOrArray(buf);
                process(rf);
            }
            finally {
                if (rf != null) {
                    try {
                        rf.close();
                    }
                    catch (final Exception e) {
                        // empty on purpose
                    }
                }
            }
        }
        else if (afmFile.toLowerCase().endsWith(".afm")) {
            try {
                if (ttfAfm == null) {
					rf = new RandomAccessFileOrArray(afmFile, forceRead, Document.plainRandomAccess);
				} else {
					rf = new RandomAccessFileOrArray(ttfAfm);
				}
                process(rf);
            }
            finally {
                if (rf != null) {
                    try {
                        rf.close();
                    }
                    catch (final Exception e) {
                        // empty on purpose
                    }
                }
            }
        }
        else if (afmFile.toLowerCase().endsWith(".pfm")) {
            try {
                final ByteArrayOutputStream ba = new ByteArrayOutputStream();
                if (ttfAfm == null) {
					rf = new RandomAccessFileOrArray(afmFile, forceRead, Document.plainRandomAccess);
				} else {
					rf = new RandomAccessFileOrArray(ttfAfm);
				}
                Pfm2afm.convert(rf, ba);
                rf.close();
                rf = new RandomAccessFileOrArray(ba.toByteArray());
                process(rf);
            }
            finally {
                if (rf != null) {
                    try {
                        rf.close();
                    }
                    catch (final Exception e) {
                        // empty on purpose
                    }
                }
            }
        } else {
			throw new DocumentException(afmFile + " is not an AFM or PFM font file.");
		}

        this.EncodingScheme = this.EncodingScheme.trim();
        if (this.EncodingScheme.equals("AdobeStandardEncoding") || this.EncodingScheme.equals("StandardEncoding")) {
            this.fontSpecific = false;
        }
        if (!this.encoding.startsWith("#"))
		 {
			PdfEncodings.convertToBytes(" ", enc); // check if the encoding exists
		}
        createEncoding();
    }

/** Gets the width from the font according to the <CODE>name</CODE> or,
 * if the <CODE>name</CODE> is null, meaning it is a symbolic font,
 * the char <CODE>c</CODE>.
 * @param c the char if the font is symbolic
 * @param name the glyph name
 * @return the width of the char
 */
    @Override
	int getRawWidth(final int c, final String name) {
        Object metrics[];
        if (name == null) { // font specific
            metrics = (Object[])this.CharMetrics.get(new Integer(c));
        }
        else {
            if (name.equals(".notdef")) {
				return 0;
			}
            metrics = (Object[])this.CharMetrics.get(name);
        }
        if (metrics != null) {
			return ((Integer)metrics[1]).intValue();
		}
        return 0;
    }

/** Gets the kerning between two Unicode characters. The characters
 * are converted to names and this names are used to find the kerning
 * pairs in the <CODE>HashMap</CODE> <CODE>KernPairs</CODE>.
 * @param char1 the first char
 * @param char2 the second char
 * @return the kerning to be applied
 */
    @Override
	public int getKerning(final int char1, final int char2)
    {
        final String first = GlyphList.unicodeToName(char1);
        if (first == null) {
			return 0;
		}
        final String second = GlyphList.unicodeToName(char2);
        if (second == null) {
			return 0;
		}
        final Object obj[] = (Object[])this.KernPairs.get(first);
        if (obj == null) {
			return 0;
		}
        for (int k = 0; k < obj.length; k += 2) {
            if (second.equals(obj[k])) {
				return ((Integer)obj[k + 1]).intValue();
			}
        }
        return 0;
    }


    /** Reads the font metrics
     * @param rf the AFM file
     * @throws DocumentException the AFM file is invalid
     * @throws IOException the AFM file could not be read
     */
    private void process(final RandomAccessFileOrArray rf) throws DocumentException, IOException
    {
        String line;
        boolean isMetrics = false;
        while ((line = rf.readLine()) != null)
        {
            final StringTokenizer tok = new StringTokenizer(line, " ,\n\r\t\f");
            if (!tok.hasMoreTokens()) {
				continue;
			}
            final String ident = tok.nextToken();
            if (ident.equals("FontName")) {
				this.FontName = tok.nextToken("\u00ff").substring(1);
			} else if (ident.equals("FullName")) {
				this.FullName = tok.nextToken("\u00ff").substring(1);
			} else if (ident.equals("FamilyName")) {
				this.FamilyName = tok.nextToken("\u00ff").substring(1);
			} else if (ident.equals("Weight")) {
				this.Weight = tok.nextToken("\u00ff").substring(1);
			} else if (ident.equals("ItalicAngle")) {
				this.ItalicAngle = Float.parseFloat(tok.nextToken());
			} else if (ident.equals("IsFixedPitch")) {
				this.IsFixedPitch = tok.nextToken().equals("true");
			} else if (ident.equals("CharacterSet")) {
				this.CharacterSet = tok.nextToken("\u00ff").substring(1);
			} else if (ident.equals("FontBBox"))
            {
                this.llx = (int)Float.parseFloat(tok.nextToken());
                this.lly = (int)Float.parseFloat(tok.nextToken());
                this.urx = (int)Float.parseFloat(tok.nextToken());
                this.ury = (int)Float.parseFloat(tok.nextToken());
            }
            else if (ident.equals("UnderlinePosition")) {
				this.UnderlinePosition = (int)Float.parseFloat(tok.nextToken());
			} else if (ident.equals("UnderlineThickness")) {
				this.UnderlineThickness = (int)Float.parseFloat(tok.nextToken());
			} else if (ident.equals("EncodingScheme")) {
				this.EncodingScheme = tok.nextToken("\u00ff").substring(1);
			} else if (ident.equals("CapHeight")) {
				this.CapHeight = (int)Float.parseFloat(tok.nextToken());
			} else if (ident.equals("XHeight")) {
				this.XHeight = (int)Float.parseFloat(tok.nextToken());
			} else if (ident.equals("Ascender")) {
				this.Ascender = (int)Float.parseFloat(tok.nextToken());
			} else if (ident.equals("Descender")) {
				this.Descender = (int)Float.parseFloat(tok.nextToken());
			} else if (ident.equals("StdHW")) {
				this.StdHW = (int)Float.parseFloat(tok.nextToken());
			} else if (ident.equals("StdVW")) {
				this.StdVW = (int)Float.parseFloat(tok.nextToken());
			} else if (ident.equals("StartCharMetrics"))
            {
                isMetrics = true;
                break;
            }
        }
        if (!isMetrics) {
			throw new DocumentException("Missing StartCharMetrics in " + this.fileName);
		}
        while ((line = rf.readLine()) != null)
        {
            StringTokenizer tok = new StringTokenizer(line);
            if (!tok.hasMoreTokens()) {
				continue;
			}
            String ident = tok.nextToken();
            if (ident.equals("EndCharMetrics"))
            {
                isMetrics = false;
                break;
            }
            Integer C = new Integer(-1);
            Integer WX = new Integer(250);
            String N = "";
            int B[] = null;

            tok = new StringTokenizer(line, ";");
            while (tok.hasMoreTokens())
            {
                final StringTokenizer tokc = new StringTokenizer(tok.nextToken());
                if (!tokc.hasMoreTokens()) {
					continue;
				}
                ident = tokc.nextToken();
                if (ident.equals("C")) {
					C = Integer.valueOf(tokc.nextToken());
				} else if (ident.equals("WX")) {
					WX = new Integer((int)Float.parseFloat(tokc.nextToken()));
				} else if (ident.equals("N")) {
					N = tokc.nextToken();
				} else if (ident.equals("B")) {
                    B = new int[]{Integer.parseInt(tokc.nextToken()),
                                         Integer.parseInt(tokc.nextToken()),
                                         Integer.parseInt(tokc.nextToken()),
                                         Integer.parseInt(tokc.nextToken())};
                }
            }
            final Object metrics[] = new Object[]{C, WX, N, B};
            if (C.intValue() >= 0) {
				this.CharMetrics.put(C, metrics);
			}
            this.CharMetrics.put(N, metrics);
        }
        if (isMetrics) {
			throw new DocumentException("Missing EndCharMetrics in " + this.fileName);
		}
        if (!this.CharMetrics.containsKey("nonbreakingspace")) {
            final Object[] space = (Object[])this.CharMetrics.get("space");
            if (space != null) {
				this.CharMetrics.put("nonbreakingspace", space);
			}
        }
        while ((line = rf.readLine()) != null)
        {
            final StringTokenizer tok = new StringTokenizer(line);
            if (!tok.hasMoreTokens()) {
				continue;
			}
            final String ident = tok.nextToken();
            if (ident.equals("EndFontMetrics")) {
				return;
			}
            if (ident.equals("StartKernPairs"))
            {
                isMetrics = true;
                break;
            }
        }
        if (!isMetrics) {
			throw new DocumentException("Missing EndFontMetrics in " + this.fileName);
		}
        while ((line = rf.readLine()) != null)
        {
            final StringTokenizer tok = new StringTokenizer(line);
            if (!tok.hasMoreTokens()) {
				continue;
			}
            final String ident = tok.nextToken();
            if (ident.equals("KPX"))
            {
                final String first = tok.nextToken();
                final String second = tok.nextToken();
                final Integer width = new Integer((int)Float.parseFloat(tok.nextToken()));
                final Object relates[] = (Object[])this.KernPairs.get(first);
                if (relates == null) {
					this.KernPairs.put(first, new Object[]{second, width});
				} else
                {
                    final int n = relates.length;
                    final Object relates2[] = new Object[n + 2];
                    System.arraycopy(relates, 0, relates2, 0, n);
                    relates2[n] = second;
                    relates2[n + 1] = width;
                    this.KernPairs.put(first, relates2);
                }
            }
            else if (ident.equals("EndKernPairs"))
            {
                isMetrics = false;
                break;
            }
        }
        if (isMetrics) {
			throw new DocumentException("Missing EndKernPairs in " + this.fileName);
		}
        rf.close();
    }

/** If the embedded flag is <CODE>false</CODE> or if the font is
 *  one of the 14 built in types, it returns <CODE>null</CODE>,
 * otherwise the font is read and output in a PdfStream object.
 * @return the PdfStream containing the font or <CODE>null</CODE>
 * @throws DocumentException if there is an error reading the font
 * @since 2.1.3
 */
    @Override
	public PdfStream getFullFontStream() throws DocumentException
    {
        if (this.builtinFont || !this.embedded) {
			return null;
		}
        RandomAccessFileOrArray rf = null;
        try {
            final String filePfb = this.fileName.substring(0, this.fileName.length() - 3) + "pfb";
            if (this.pfb == null) {
				rf = new RandomAccessFileOrArray(filePfb, true, Document.plainRandomAccess);
			} else {
				rf = new RandomAccessFileOrArray(this.pfb);
			}
            final int fileLength = rf.length();
            final byte st[] = new byte[fileLength - 18];
            final int lengths[] = new int[3];
            int bytePtr = 0;
            for (int k = 0; k < 3; ++k) {
                if (rf.read() != 0x80) {
					throw new DocumentException("Start marker missing in " + filePfb);
				}
                if (rf.read() != PFB_TYPES[k]) {
					throw new DocumentException("Incorrect segment type in " + filePfb);
				}
                int size = rf.read();
                size += rf.read() << 8;
                size += rf.read() << 16;
                size += rf.read() << 24;
                lengths[k] = size;
                while (size != 0) {
                    final int got = rf.read(st, bytePtr, size);
                    if (got < 0) {
						throw new DocumentException("Premature end in " + filePfb);
					}
                    bytePtr += got;
                    size -= got;
                }
            }
            return new StreamFont(st, lengths, this.compressionLevel);
        }
        catch (final Exception e) {
            throw new DocumentException(e);
        }
        finally {
            if (rf != null) {
                try {
                    rf.close();
                }
                catch (final Exception e) {
                    // empty on purpose
                }
            }
        }
    }

/** Generates the font descriptor for this font or <CODE>null</CODE> if it is
 * one of the 14 built in fonts.
 * @param fontStream the indirect reference to a PdfStream containing the font or <CODE>null</CODE>
 * @return the PdfDictionary containing the font descriptor or <CODE>null</CODE>
 */
    private PdfDictionary getFontDescriptor(final PdfIndirectReference fontStream)
    {
        if (this.builtinFont) {
			return null;
		}
        final PdfDictionary dic = new PdfDictionary(PdfName.FONTDESCRIPTOR);
        dic.put(PdfName.ASCENT, new PdfNumber(this.Ascender));
        dic.put(PdfName.CAPHEIGHT, new PdfNumber(this.CapHeight));
        dic.put(PdfName.DESCENT, new PdfNumber(this.Descender));
        dic.put(PdfName.FONTBBOX, new PdfRectangle(this.llx, this.lly, this.urx, this.ury));
        dic.put(PdfName.FONTNAME, new PdfName(this.FontName));
        dic.put(PdfName.ITALICANGLE, new PdfNumber(this.ItalicAngle));
        dic.put(PdfName.STEMV, new PdfNumber(this.StdVW));
        if (fontStream != null) {
			dic.put(PdfName.FONTFILE, fontStream);
		}
        int flags = 0;
        if (this.IsFixedPitch) {
			flags |= 1;
		}
        flags |= this.fontSpecific ? 4 : 32;
        if (this.ItalicAngle < 0) {
			flags |= 64;
		}
        if (this.FontName.indexOf("Caps") >= 0 || this.FontName.endsWith("SC")) {
			flags |= 131072;
		}
        if (this.Weight.equals("Bold")) {
			flags |= 262144;
		}
        dic.put(PdfName.FLAGS, new PdfNumber(flags));

        return dic;
    }

    /** Generates the font dictionary for this font.
     * @return the PdfDictionary containing the font dictionary
     * @param firstChar the first valid character
     * @param lastChar the last valid character
     * @param shortTag a 256 bytes long <CODE>byte</CODE> array where each unused byte is represented by 0
     * @param fontDescriptor the indirect reference to a PdfDictionary containing the font descriptor or <CODE>null</CODE>
     */
    private PdfDictionary getFontBaseType(final PdfIndirectReference fontDescriptor, int firstChar, final int lastChar, final byte shortTag[])
    {
        final PdfDictionary dic = new PdfDictionary(PdfName.FONT);
        dic.put(PdfName.SUBTYPE, PdfName.TYPE1);
        dic.put(PdfName.BASEFONT, new PdfName(this.FontName));
        final boolean stdEncoding = this.encoding.equals("Cp1252") || this.encoding.equals("MacRoman");
        if (!this.fontSpecific || this.specialMap != null) {
            for (int k = firstChar; k <= lastChar; ++k) {
                if (!this.differences[k].equals(notdef)) {
                    firstChar = k;
                    break;
                }
            }
            if (stdEncoding) {
				dic.put(PdfName.ENCODING, this.encoding.equals("Cp1252") ? PdfName.WIN_ANSI_ENCODING : PdfName.MAC_ROMAN_ENCODING);
			} else {
                final PdfDictionary enc = new PdfDictionary(PdfName.ENCODING);
                final PdfArray dif = new PdfArray();
                boolean gap = true;
                for (int k = firstChar; k <= lastChar; ++k) {
                    if (shortTag[k] != 0) {
                        if (gap) {
                            dif.add(new PdfNumber(k));
                            gap = false;
                        }
                        dif.add(new PdfName(this.differences[k]));
                    } else {
						gap = true;
					}
                }
                enc.put(PdfName.DIFFERENCES, dif);
                dic.put(PdfName.ENCODING, enc);
            }
        }
        if (this.specialMap != null || this.forceWidthsOutput || !(this.builtinFont && (this.fontSpecific || stdEncoding))) {
            dic.put(PdfName.FIRSTCHAR, new PdfNumber(firstChar));
            dic.put(PdfName.LASTCHAR, new PdfNumber(lastChar));
            final PdfArray wd = new PdfArray();
            for (int k = firstChar; k <= lastChar; ++k) {
                if (shortTag[k] == 0) {
					wd.add(new PdfNumber(0));
				} else {
					wd.add(new PdfNumber(this.widths[k]));
				}
            }
            dic.put(PdfName.WIDTHS, wd);
        }
        if (!this.builtinFont && fontDescriptor != null) {
			dic.put(PdfName.FONTDESCRIPTOR, fontDescriptor);
		}
        return dic;
    }

    /** Outputs to the writer the font dictionaries and streams.
     * @param writer the writer for this document
     * @param ref the font indirect reference
     * @param params several parameters that depend on the font type
     * @throws IOException on error
     * @throws DocumentException error in generating the object
     */
    @Override
	void writeFont(final PdfWriter writer, final PdfIndirectReference ref, final Object params[]) throws DocumentException, IOException {
        int firstChar = ((Integer)params[0]).intValue();
        int lastChar = ((Integer)params[1]).intValue();
        final byte shortTag[] = (byte[])params[2];
        final boolean subsetp = ((Boolean)params[3]).booleanValue() && this.subset;
        if (!subsetp) {
            firstChar = 0;
            lastChar = shortTag.length - 1;
            for (int k = 0; k < shortTag.length; ++k) {
				shortTag[k] = 1;
			}
        }
        PdfIndirectReference ind_font = null;
        PdfObject pobj = null;
        PdfIndirectObject obj = null;
        pobj = getFullFontStream();
        if (pobj != null){
            obj = writer.addToBody(pobj);
            ind_font = obj.getIndirectReference();
        }
        pobj = getFontDescriptor(ind_font);
        if (pobj != null){
            obj = writer.addToBody(pobj);
            ind_font = obj.getIndirectReference();
        }
        pobj = getFontBaseType(ind_font, firstChar, lastChar, shortTag);
        writer.addToBody(pobj, ref);
    }

    /** Gets the font parameter identified by <CODE>key</CODE>. Valid values
     * for <CODE>key</CODE> are <CODE>ASCENT</CODE>, <CODE>CAPHEIGHT</CODE>, <CODE>DESCENT</CODE>,
     * <CODE>ITALICANGLE</CODE>, <CODE>BBOXLLX</CODE>, <CODE>BBOXLLY</CODE>, <CODE>BBOXURX</CODE>
     * and <CODE>BBOXURY</CODE>.
     * @param key the parameter to be extracted
     * @param fontSize the font size in points
     * @return the parameter in points
     */
    @Override
	public float getFontDescriptor(final int key, final float fontSize) {
        switch (key) {
            case AWT_ASCENT:
            case ASCENT:
                return this.Ascender * fontSize / 1000;
            case CAPHEIGHT:
                return this.CapHeight * fontSize / 1000;
            case AWT_DESCENT:
            case DESCENT:
                return this.Descender * fontSize / 1000;
            case ITALICANGLE:
                return this.ItalicAngle;
            case BBOXLLX:
                return this.llx * fontSize / 1000;
            case BBOXLLY:
                return this.lly * fontSize / 1000;
            case BBOXURX:
                return this.urx * fontSize / 1000;
            case BBOXURY:
                return this.ury * fontSize / 1000;
            case AWT_LEADING:
                return 0;
            case AWT_MAXADVANCE:
                return (this.urx - this.llx) * fontSize / 1000;
            case UNDERLINE_POSITION:
                return this.UnderlinePosition * fontSize / 1000;
            case UNDERLINE_THICKNESS:
                return this.UnderlineThickness * fontSize / 1000;
        }
        return 0;
    }

    /** Gets the postscript font name.
     * @return the postscript font name
     */
    @Override
	public String getPostscriptFontName() {
        return this.FontName;
    }

    /** Gets the full name of the font. If it is a True Type font
     * each array element will have {Platform ID, Platform Encoding ID,
     * Language ID, font name}. The interpretation of this values can be
     * found in the Open Type specification, chapter 2, in the 'name' table.<br>
     * For the other fonts the array has a single element with {"", "", "",
     * font name}.
     * @return the full name of the font
     */
    @Override
	public String[][] getFullFontName() {
        return new String[][]{{"", "", "", this.FullName}};
    }

    /** Gets all the entries of the names-table. If it is a True Type font
     * each array element will have {Name ID, Platform ID, Platform Encoding ID,
     * Language ID, font name}. The interpretation of this values can be
     * found in the Open Type specification, chapter 2, in the 'name' table.<br>
     * For the other fonts the array has a single element with {"4", "", "", "",
     * font name}.
     * @return the full name of the font
     */
    @Override
	public String[][] getAllNameEntries() {
        return new String[][]{{"4", "", "", "", this.FullName}};
    }

    /** Gets the family name of the font. If it is a True Type font
     * each array element will have {Platform ID, Platform Encoding ID,
     * Language ID, font name}. The interpretation of this values can be
     * found in the Open Type specification, chapter 2, in the 'name' table.<br>
     * For the other fonts the array has a single element with {"", "", "",
     * font name}.
     * @return the family name of the font
     */
    @Override
	public String[][] getFamilyFontName() {
        return new String[][]{{"", "", "", this.FamilyName}};
    }

    /** Checks if the font has any kerning pairs.
     * @return <CODE>true</CODE> if the font has any kerning pairs
     */
    @Override
	public boolean hasKernPairs() {
        return !this.KernPairs.isEmpty();
    }

    /**
     * Sets the font name that will appear in the pdf font dictionary.
     * Use with care as it can easily make a font unreadable if not embedded.
     * @param name the new font name
     */
    @Override
	public void setPostscriptFontName(final String name) {
        this.FontName = name;
    }

    /**
     * Sets the kerning between two Unicode chars.
     * @param char1 the first char
     * @param char2 the second char
     * @param kern the kerning to apply in normalized 1000 units
     * @return <code>true</code> if the kerning was applied, <code>false</code> otherwise
     */
    @Override
	public boolean setKerning(final int char1, final int char2, final int kern) {
        final String first = GlyphList.unicodeToName(char1);
        if (first == null) {
			return false;
		}
        final String second = GlyphList.unicodeToName(char2);
        if (second == null) {
			return false;
		}
        Object obj[] = (Object[])this.KernPairs.get(first);
        if (obj == null) {
            obj = new Object[]{second, new Integer(kern)};
            this.KernPairs.put(first, obj);
            return true;
        }
        for (int k = 0; k < obj.length; k += 2) {
            if (second.equals(obj[k])) {
                obj[k + 1] = new Integer(kern);
                return true;
            }
        }
        final int size = obj.length;
        final Object obj2[] = new Object[size + 2];
        System.arraycopy(obj, 0, obj2, 0, size);
        obj2[size] = second;
        obj2[size + 1] = new Integer(kern);
        this.KernPairs.put(first, obj2);
        return true;
    }

    @Override
	protected int[] getRawCharBBox(final int c, final String name) {
        Object metrics[];
        if (name == null) { // font specific
            metrics = (Object[])this.CharMetrics.get(new Integer(c));
        }
        else {
            if (name.equals(".notdef")) {
				return null;
			}
            metrics = (Object[])this.CharMetrics.get(name);
        }
        if (metrics != null) {
			return (int[])metrics[3];
		}
        return null;
    }

}
