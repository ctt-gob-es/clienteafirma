/*
 * $Id: TrueTypeFontUnicode.java 3869 2009-04-17 18:03:45Z blowagie $
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

import java.io.IOException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Utilities;

/** Represents a True Type font with Unicode encoding. All the character
 * in the font can be used directly by using the encoding Identity-H or
 * Identity-V. This is the only way to represent some character sets such
 * as Thai.
 * @author  Paulo Soares (psoares@consiste.pt)
 */
class TrueTypeFontUnicode extends TrueTypeFont implements Comparator{

    /**
     * <CODE>true</CODE> if the encoding is vertical.
     */
    private boolean vertical = false;

    /**
     * Creates a new TrueType font addressed by Unicode characters. The font
     * will always be embedded.
     * @param ttFile the location of the font on file. The file must end in '.ttf'.
     * The modifiers after the name are ignored.
     * @param enc the encoding to be applied to this font
     * @param emb true if the font is to be embedded in the PDF
     * @param ttfAfm the font as a <CODE>byte</CODE> array
     * @throws DocumentException the font is invalid
     * @throws IOException the font file could not be read
     */
    TrueTypeFontUnicode(final String ttFile, final String enc, final boolean emb, final byte ttfAfm[], final boolean forceRead) throws DocumentException, IOException {
        final String nameBase = getBaseName(ttFile);
        final String ttcName = getTTCName(nameBase);
        if (nameBase.length() < ttFile.length()) {
            this.style = ttFile.substring(nameBase.length());
        }
        this.encoding = enc;
        this.embedded = emb;
        this.fileName = ttcName;
        this.ttcIndex = "";
        if (ttcName.length() < nameBase.length()) {
			this.ttcIndex = nameBase.substring(ttcName.length() + 1);
		}
        this.fontType = FONT_TYPE_TTUNI;
        if ((this.fileName.toLowerCase().endsWith(".ttf") || this.fileName.toLowerCase().endsWith(".otf") || this.fileName.toLowerCase().endsWith(".ttc")) && (enc.equals(IDENTITY_H) || enc.equals(IDENTITY_V)) && emb) {
            process(ttfAfm, forceRead);
            if (this.os_2.fsType == 2) {
				throw new DocumentException(this.fileName + this.style + " cannot be embedded due to licensing restrictions.");
			}
            // Sivan
            if (this.cmap31 == null && !this.fontSpecific || this.cmap10 == null && this.fontSpecific) {
				this.directTextToByte=true;
			}
                //throw new DocumentException(fileName + " " + style + " does not contain an usable cmap.");
            if (this.fontSpecific) {
                this.fontSpecific = false;
                final String tempEncoding = this.encoding;
                this.encoding = "";
                createEncoding();
                this.encoding = tempEncoding;
                this.fontSpecific = true;
            }
        } else {
			throw new DocumentException(this.fileName + " " + this.style + " is not a TTF font file.");
		}
        this.vertical = enc.endsWith("V");
    }

    /**
     * Gets the width of a <CODE>char</CODE> in normalized 1000 units.
     * @param char1 the unicode <CODE>char</CODE> to get the width of
     * @return the width in normalized 1000 units
     */
    @Override
	public int getWidth(final int char1) {
        if (this.vertical) {
			return 1000;
		}
        if (this.fontSpecific) {
            if ((char1 & 0xff00) == 0 || (char1 & 0xff00) == 0xf000) {
				return getRawWidth(char1 & 0xff, null);
			} else {
				return 0;
			}
        }
        else {
            return getRawWidth(char1, this.encoding);
        }
    }

    /**
     * Gets the width of a <CODE>String</CODE> in normalized 1000 units.
     * @param text the <CODE>String</CODE> to get the width of
     * @return the width in normalized 1000 units
     */
    @Override
	public int getWidth(final String text) {
        if (this.vertical) {
			return text.length() * 1000;
		}
        int total = 0;
        if (this.fontSpecific) {
            final char cc[] = text.toCharArray();
            final int len = cc.length;
            for (int k = 0; k < len; ++k) {
                final char c = cc[k];
                if ((c & 0xff00) == 0 || (c & 0xff00) == 0xf000) {
					total += getRawWidth(c & 0xff, null);
				}
            }
        }
        else {
            final int len = text.length();
            for (int k = 0; k < len; ++k) {
                if (Utilities.isSurrogatePair(text, k)) {
                    total += getRawWidth(Utilities.convertToUtf32(text, k), this.encoding);
                    ++k;
                } else {
					total += getRawWidth(text.charAt(k), this.encoding);
				}
            }
        }
        return total;
    }

    /** Creates a ToUnicode CMap to allow copy and paste from Acrobat.
     * @param metrics metrics[0] contains the glyph index and metrics[2]
     * contains the Unicode code
     * @return the stream representing this CMap or <CODE>null</CODE>
     */
    private PdfStream getToUnicode(final Object metrics[]) {
        if (metrics.length == 0) {
			return null;
		}
        final StringBuffer buf = new StringBuffer(
        "/CIDInit /ProcSet findresource begin\n" +
        "12 dict begin\n" +
        "begincmap\n" +
        "/CIDSystemInfo\n" +
        "<< /Registry (TTX+0)\n" +
        "/Ordering (T42UV)\n" +
        "/Supplement 0\n" +
        ">> def\n" +
        "/CMapName /TTX+0 def\n" +
        "/CMapType 2 def\n" +
        "1 begincodespacerange\n" +
        "<0000><FFFF>\n" +
        "endcodespacerange\n");
        int size = 0;
        for (int k = 0; k < metrics.length; ++k) {
            if (size == 0) {
                if (k != 0) {
                    buf.append("endbfrange\n");
                }
                size = Math.min(100, metrics.length - k);
                buf.append(size).append(" beginbfrange\n");
            }
            --size;
            final int metric[] = (int[])metrics[k];
            final String fromTo = toHex(metric[0]);
            buf.append(fromTo).append(fromTo).append(toHex(metric[2])).append('\n');
        }
        buf.append(
        "endbfrange\n" +
        "endcmap\n" +
        "CMapName currentdict /CMap defineresource pop\n" +
        "end end\n");
        final String s = buf.toString();
        final PdfStream stream = new PdfStream(PdfEncodings.convertToBytes(s, null));
        stream.flateCompress(this.compressionLevel);
        return stream;
    }

    private static String toHex4(final int n) {
        final String s = "0000" + Integer.toHexString(n);
        return s.substring(s.length() - 4);
    }

    /** Gets an hex string in the format "&lt;HHHH&gt;".
     * @param n the number
     * @return the hex string
     */
    private static String toHex(int n) {
        if (n < 0x10000) {
			return "<" + toHex4(n) + ">";
		}
        n -= 0x10000;
        final int high = n / 0x400 + 0xd800;
        final int low = n % 0x400 + 0xdc00;
        return "[<" + toHex4(high) + toHex4(low) + ">]";
    }

    /** Generates the CIDFontTyte2 dictionary.
     * @param fontDescriptor the indirect reference to the font descriptor
     * @param subsetPrefix the subset prefix
     * @param metrics the horizontal width metrics
     * @return a stream
     */
    private PdfDictionary getCIDFontType2(final PdfIndirectReference fontDescriptor, final String subsetPrefix, final Object metrics[]) {
        final PdfDictionary dic = new PdfDictionary(PdfName.FONT);
        // sivan; cff
        if (this.cff) {
			dic.put(PdfName.SUBTYPE, PdfName.CIDFONTTYPE0);
            dic.put(PdfName.BASEFONT, new PdfName(subsetPrefix+this.fontName+"-"+this.encoding));
        }
		else {
			dic.put(PdfName.SUBTYPE, PdfName.CIDFONTTYPE2);
            dic.put(PdfName.BASEFONT, new PdfName(subsetPrefix + this.fontName));
        }
        dic.put(PdfName.FONTDESCRIPTOR, fontDescriptor);
        if (!this.cff) {
			dic.put(PdfName.CIDTOGIDMAP,PdfName.IDENTITY);
		}
        final PdfDictionary cdic = new PdfDictionary();
        cdic.put(PdfName.REGISTRY, new PdfString("Adobe"));
        cdic.put(PdfName.ORDERING, new PdfString("Identity"));
        cdic.put(PdfName.SUPPLEMENT, new PdfNumber(0));
        dic.put(PdfName.CIDSYSTEMINFO, cdic);
        if (!this.vertical) {
            dic.put(PdfName.DW, new PdfNumber(1000));
            final StringBuffer buf = new StringBuffer("[");
            int lastNumber = -10;
            boolean firstTime = true;
            for (final Object metric2 : metrics) {
                final int metric[] = (int[])metric2;
                if (metric[1] == 1000) {
					continue;
				}
                final int m = metric[0];
                if (m == lastNumber + 1) {
                    buf.append(' ').append(metric[1]);
                }
                else {
                    if (!firstTime) {
                        buf.append(']');
                    }
                    firstTime = false;
                    buf.append(m).append('[').append(metric[1]);
                }
                lastNumber = m;
            }
            if (buf.length() > 1) {
                buf.append("]]");
                dic.put(PdfName.W, new PdfLiteral(buf.toString()));
            }
        }
        return dic;
    }

    /** Generates the font dictionary.
     * @param descendant the descendant dictionary
     * @param subsetPrefix the subset prefix
     * @param toUnicode the ToUnicode stream
     * @return the stream
     */
    private PdfDictionary getFontBaseType(final PdfIndirectReference descendant, final String subsetPrefix, final PdfIndirectReference toUnicode) {
        final PdfDictionary dic = new PdfDictionary(PdfName.FONT);

        dic.put(PdfName.SUBTYPE, PdfName.TYPE0);
        // The PDF Reference manual advises to add -encoding to CID font names
		if (this.cff) {
			dic.put(PdfName.BASEFONT, new PdfName(subsetPrefix+this.fontName+"-"+this.encoding));
			  //dic.put(PdfName.BASEFONT, new PdfName(subsetPrefix+fontName));
		} else {
			dic.put(PdfName.BASEFONT, new PdfName(subsetPrefix + this.fontName));
		}
		  //dic.put(PdfName.BASEFONT, new PdfName(fontName));
        dic.put(PdfName.ENCODING, new PdfName(this.encoding));
        dic.put(PdfName.DESCENDANTFONTS, new PdfArray(descendant));
        if (toUnicode != null) {
			dic.put(PdfName.TOUNICODE, toUnicode);
		}
        return dic;
    }

    /** The method used to sort the metrics array.
     * @param o1 the first element
     * @param o2 the second element
     * @return the comparison
     */
    @Override
	public int compare(final Object o1, final Object o2) {
        final int m1 = ((int[])o1)[0];
        final int m2 = ((int[])o2)[0];
        if (m1 < m2) {
			return -1;
		}
        if (m1 == m2) {
			return 0;
		}
        return 1;
    }

    private static final byte[] rotbits = {(byte)0x80,(byte)0x40,(byte)0x20,(byte)0x10,(byte)0x08,(byte)0x04,(byte)0x02,(byte)0x01};

    /** Outputs to the writer the font dictionaries and streams.
     * @param writer the writer for this document
     * @param ref the font indirect reference
     * @param params several parameters that depend on the font type
     * @throws IOException on error
     * @throws DocumentException error in generating the object
     */
    @Override
	void writeFont(final PdfWriter writer, final PdfIndirectReference ref, final Object params[]) throws DocumentException, IOException {
        final HashMap longTag = (HashMap)params[0];
        addRangeUni(longTag, true, this.subset);
        final Object metrics[] = longTag.values().toArray();
        Arrays.sort(metrics, this);
        PdfIndirectReference ind_font = null;
        PdfObject pobj = null;
        PdfIndirectObject obj = null;
        PdfIndirectReference cidset = null;
        if (writer.getPDFXConformance() == PdfWriter.PDFA1A || writer.getPDFXConformance() == PdfWriter.PDFA1B) {
            PdfStream stream;
            if (metrics.length == 0) {
                stream = new PdfStream(new byte[]{(byte)0x80});
            }
            else {
                final int top = ((int[])metrics[metrics.length - 1])[0];
                final byte[] bt = new byte[top / 8 + 1];
                for (final Object metric : metrics) {
                    final int v = ((int[])metric)[0];
                    bt[v / 8] |= rotbits[v % 8];
                }
                stream = new PdfStream(bt);
                stream.flateCompress(this.compressionLevel);
            }
            cidset = writer.addToBody(stream).getIndirectReference();
        }
        // sivan: cff
        if (this.cff) {
			byte b[] = readCffFont();
            if (this.subset || this.subsetRanges != null) {
                final CFFFontSubset cff = new CFFFontSubset(new RandomAccessFileOrArray(b),longTag);
                b = cff.Process(cff.getNames()[0]);
            }
			pobj = new StreamFont(b, "CIDFontType0C", this.compressionLevel);
			obj = writer.addToBody(pobj);
			ind_font = obj.getIndirectReference();
        } else {
            byte[] b;
            if (this.subset || this.directoryOffset != 0) {
                final TrueTypeFontSubSet sb = new TrueTypeFontSubSet(this.fileName, new RandomAccessFileOrArray(this.rf), longTag, this.directoryOffset, false, false);
                b = sb.process();
            }
            else {
                b = getFullFont();
            }
            final int lengths[] = new int[]{b.length};
            pobj = new StreamFont(b, lengths, this.compressionLevel);
            obj = writer.addToBody(pobj);
            ind_font = obj.getIndirectReference();
        }
        String subsetPrefix = "";
        if (this.subset) {
			subsetPrefix = createSubsetPrefix();
		}
        final PdfDictionary dic = getFontDescriptor(ind_font, subsetPrefix, cidset);
        obj = writer.addToBody(dic);
        ind_font = obj.getIndirectReference();

        pobj = getCIDFontType2(ind_font, subsetPrefix, metrics);
        obj = writer.addToBody(pobj);
        ind_font = obj.getIndirectReference();

        pobj = getToUnicode(metrics);
        PdfIndirectReference toUnicodeRef = null;

        if (pobj != null) {
            obj = writer.addToBody(pobj);
            toUnicodeRef = obj.getIndirectReference();
        }

        pobj = getFontBaseType(ind_font, subsetPrefix, toUnicodeRef);
        writer.addToBody(pobj, ref);
    }

    /**
     * Returns a PdfStream object with the full font program.
     * @return	a PdfStream with the font program
     * @since	2.1.3
     */
    @Override
	public PdfStream getFullFontStream() throws IOException, DocumentException {
    	if (this.cff) {
			return new StreamFont(readCffFont(), "CIDFontType0C", this.compressionLevel);
        }
    	return super.getFullFontStream();
    }

    /** A forbidden operation. Will throw a null pointer exception.
     * @param text the text
     * @return always <CODE>null</CODE>
     */
    @Override
	byte[] convertToBytes(final String text) {
        return null;
    }

    @Override
	byte[] convertToBytes(final int char1) {
        return null;
    }

    /** Gets the glyph index and metrics for a character.
     * @param c the character
     * @return an <CODE>int</CODE> array with {glyph index, width}
     */
    @Override
	public int[] getMetricsTT(final int c) {
        if (this.cmapExt != null) {
			return (int[])this.cmapExt.get(new Integer(c));
		}
        HashMap map = null;
        if (this.fontSpecific) {
			map = this.cmap10;
		} else {
			map = this.cmap31;
		}
        if (map == null) {
			return null;
		}
        if (this.fontSpecific) {
            if ((c & 0xffffff00) == 0 || (c & 0xffffff00) == 0xf000) {
				return (int[])map.get(new Integer(c & 0xff));
			} else {
				return null;
			}
        } else {
			return (int[])map.get(new Integer(c));
		}
    }

    /**
     * Checks if a character exists in this font.
     * @param c the character to check
     * @return <CODE>true</CODE> if the character has a glyph,
     * <CODE>false</CODE> otherwise
     */
    @Override
	public boolean charExists(final int c) {
        return getMetricsTT(c) != null;
    }

    /**
     * Sets the character advance.
     * @param c the character
     * @param advance the character advance normalized to 1000 units
     * @return <CODE>true</CODE> if the advance was set,
     * <CODE>false</CODE> otherwise
     */
    @Override
	public boolean setCharAdvance(final int c, final int advance) {
        final int[] m = getMetricsTT(c);
        if (m == null) {
			return false;
		}
        m[1] = advance;
        return true;
    }

    @Override
	public int[] getCharBBox(final int c) {
        if (this.bboxes == null) {
			return null;
		}
        final int[] m = getMetricsTT(c);
        if (m == null) {
			return null;
		}
        return this.bboxes[m[0]];
    }
}
