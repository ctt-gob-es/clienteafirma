/*
 * $Id: TrueTypeFont.java 3854 2009-04-14 08:02:10Z blowagie $
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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;

/** Reads a Truetype font
 *
 * @author Paulo Soares (psoares@consiste.pt)
 */
class TrueTypeFont extends BaseFont {

    /** The code pages possible for a True Type font.
     */
    private static final String codePages[] = {
        "1252 Latin 1",
        "1250 Latin 2: Eastern Europe",
        "1251 Cyrillic",
        "1253 Greek",
        "1254 Turkish",
        "1255 Hebrew",
        "1256 Arabic",
        "1257 Windows Baltic",
        "1258 Vietnamese",
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        "874 Thai",
        "932 JIS/Japan",
        "936 Chinese: Simplified chars--PRC and Singapore",
        "949 Korean Wansung",
        "950 Chinese: Traditional chars--Taiwan and Hong Kong",
        "1361 Korean Johab",
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        "Macintosh Character Set (US Roman)",
        "OEM Character Set",
        "Symbol Character Set",
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        "869 IBM Greek",
        "866 MS-DOS Russian",
        "865 MS-DOS Nordic",
        "864 Arabic",
        "863 MS-DOS Canadian French",
        "862 Hebrew",
        "861 MS-DOS Icelandic",
        "860 MS-DOS Portuguese",
        "857 IBM Turkish",
        "855 IBM Cyrillic; primarily Russian",
        "852 Latin 2",
        "775 MS-DOS Baltic",
        "737 Greek; former 437 G",
        "708 Arabic; ASMO 708",
        "850 WE/Latin 1",
        "437 US"};

    private boolean justNames = false;
    /** Contains the location of the several tables. The key is the name of
     * the table and the value is an <CODE>int[2]</CODE> where position 0
     * is the offset from the start of the file and position 1 is the length
     * of the table.
     */
    protected HashMap tables;
    /** The file in use.
     */
    protected RandomAccessFileOrArray rf;
    /** The file name.
     */
    protected String fileName;

    protected boolean cff = false;

    private int cffOffset;

    private int cffLength;

    /** The offset from the start of the file to the table directory.
     * It is 0 for TTF and may vary for TTC depending on the chosen font.
     */
    protected int directoryOffset;
    /** The index for the TTC font. It is an empty <CODE>String</CODE> for a
     * TTF file.
     */
    protected String ttcIndex;
    /** The style modifier */
    protected String style = "";
    /** The content of table 'head'.
     */
    private final FontHeader head = new FontHeader();
    /** The content of table 'hhea'.
     */
    private final HorizontalHeader hhea = new HorizontalHeader();
    /** The content of table 'OS/2'.
     */
    protected WindowsMetrics os_2 = new WindowsMetrics();
    /** The width of the glyphs. This is essentially the content of table
     * 'hmtx' normalized to 1000 units.
     */
    private int GlyphWidths[];

    protected int bboxes[][];
    /** The map containing the code information for the table 'cmap', encoding 1.0.
     * The key is the code and the value is an <CODE>int[2]</CODE> where position 0
     * is the glyph number and position 1 is the glyph width normalized to 1000
     * units.
     */
    protected HashMap cmap10;
    /** The map containing the code information for the table 'cmap', encoding 3.1
     * in Unicode.
     * <P>
     * The key is the code and the value is an <CODE>int</CODE>[2] where position 0
     * is the glyph number and position 1 is the glyph width normalized to 1000
     * units.
     */
    protected HashMap cmap31;

    protected HashMap cmapExt;

    /** The map containing the kerning information. It represents the content of
     * table 'kern'. The key is an <CODE>Integer</CODE> where the top 16 bits
     * are the glyph number for the first character and the lower 16 bits are the
     * glyph number for the second character. The value is the amount of kerning in
     * normalized 1000 units as an <CODE>Integer</CODE>. This value is usually negative.
     */
    private final IntHashtable kerning = new IntHashtable();
    /**
     * The font name.
     * This name is usually extracted from the table 'name' with
     * the 'Name ID' 6.
     */
    protected String fontName;

    /** The full name of the font
     */
    private String fullName[][];

    /** All the names of the Names-Table
     */
    private String allNameEntries[][];

    /** The family name of the font
     */
    private String familyName[][];
    /** The italic angle. It is usually extracted from the 'post' table or in it's
     * absence with the code:
     * <P>
     * <PRE>
     * -Math.atan2(hhea.caretSlopeRun, hhea.caretSlopeRise) * 180 / Math.PI
     * </PRE>
     */
    private double italicAngle;
    /** <CODE>true</CODE> if all the glyphs have the same width.
     */
    private boolean isFixedPitch = false;

    private int underlinePosition;

    private int underlineThickness;

    /** The components of table 'head'.
     */
    private static class FontHeader {

        /** A variable. */
        private int unitsPerEm;
        /** A variable. */
        private short xMin;
        /** A variable. */
        private short yMin;
        /** A variable. */
        private short xMax;
        /** A variable. */
        private short yMax;
        /** A variable. */
        private int macStyle;
    }

    /** The components of table 'hhea'.
     */
    private static class HorizontalHeader {
        /** A variable. */
        private short Ascender;
        /** A variable. */
        private short Descender;
        /** A variable. */
        private short LineGap;
        /** A variable. */
        private int advanceWidthMax;



        /** A variable. */
        private short caretSlopeRise;
        /** A variable. */
        private short caretSlopeRun;
        /** A variable. */
        private int numberOfHMetrics;
    }

    /** The components of table 'OS/2'.
     */
    static class WindowsMetrics {
        /** A variable. */
        private short xAvgCharWidth;


        /** A variable. */
        short fsType;
        /** A variable. */
        private short ySubscriptXSize;
        /** A variable. */
        private short ySubscriptYSize;

        /** A variable. */
        private short ySubscriptYOffset;

        /** A variable. */
        private short ySuperscriptYSize;

        /** A variable. */
        private short ySuperscriptYOffset;
        /** A variable. */
        private short yStrikeoutSize;
        /** A variable. */
        private short yStrikeoutPosition;

        /** A variable. */
        private final byte panose[] = new byte[10];
        /** A variable. */
        private final byte achVendID[] = new byte[4];



        /** A variable. */
        private short sTypoAscender;
        /** A variable. */
        private short sTypoDescender;



        /** A variable. */
        private int ulCodePageRange1;
        /** A variable. */
        private int ulCodePageRange2;
        /** A variable. */
        private int sCapHeight;
    }

    /** This constructor is present to allow extending the class.
     */
    protected TrueTypeFont() {
    }

    /** Creates a new TrueType font.
     * @param ttFile the location of the font on file. The file must end in '.ttf' or
     * '.ttc' but can have modifiers after the name
     * @param enc the encoding to be applied to this font
     * @param emb true if the font is to be embedded in the PDF
     * @param ttfAfm the font as a <CODE>byte</CODE> array
     * @throws DocumentException the font is invalid
     * @throws IOException the font file could not be read
     * @since	2.1.5
     */
    TrueTypeFont(final String ttFile, final String enc, final boolean emb, final byte ttfAfm[], final boolean justNames, final boolean forceRead) throws DocumentException, IOException {
    	this.justNames = justNames;
        final String nameBase = getBaseName(ttFile);
        final String ttcName = getTTCName(nameBase);
        if (nameBase.length() < ttFile.length()) {
            this.style = ttFile.substring(nameBase.length());
        }
        this.encoding = enc;
        this.embedded = emb;
        this.fileName = ttcName;
        this.fontType = FONT_TYPE_TT;
        this.ttcIndex = "";
        if (ttcName.length() < nameBase.length()) {
			this.ttcIndex = nameBase.substring(ttcName.length() + 1);
		}
        if (this.fileName.toLowerCase().endsWith(".ttf") || this.fileName.toLowerCase().endsWith(".otf") || this.fileName.toLowerCase().endsWith(".ttc")) {
            process(ttfAfm, forceRead);
            if (!justNames && this.embedded && this.os_2.fsType == 2) {
				throw new DocumentException(this.fileName + this.style + " cannot be embedded due to licensing restrictions.");
			}
        } else {
			throw new DocumentException(this.fileName + this.style + " is not a TTF, OTF or TTC font file.");
		}
        if (!this.encoding.startsWith("#"))
		 {
			PdfEncodings.convertToBytes(" ", enc); // check if the encoding exists
		}
        createEncoding();
    }

    /** Gets the name from a composed TTC file name.
     * If I have for input "myfont.ttc,2" the return will
     * be "myfont.ttc".
     * @param name the full name
     * @return the simple file name
     */
    protected static String getTTCName(final String name) {
        final int idx = name.toLowerCase().indexOf(".ttc,");
        if (idx < 0) {
			return name;
		} else {
			return name.substring(0, idx + 4);
		}
    }


    /**
     * Reads the tables 'head', 'hhea', 'OS/2' and 'post' filling several variables.
     * @throws DocumentException the font is invalid
     * @throws IOException the font file could not be read
     */
    private void fillTables() throws DocumentException, IOException {
        int table_location[];
        table_location = (int[])this.tables.get("head");
        if (table_location == null) {
			throw new DocumentException("Table 'head' does not exist in " + this.fileName + this.style);
		}
        this.rf.seek(table_location[0] + 16);
        this.head.unitsPerEm = this.rf.readUnsignedShort();
        this.rf.skipBytes(16);
        this.head.xMin = this.rf.readShort();
        this.head.yMin = this.rf.readShort();
        this.head.xMax = this.rf.readShort();
        this.head.yMax = this.rf.readShort();
        this.head.macStyle = this.rf.readUnsignedShort();

        table_location = (int[])this.tables.get("hhea");
        if (table_location == null) {
			throw new DocumentException("Table 'hhea' does not exist " + this.fileName + this.style);
		}
        this.rf.seek(table_location[0] + 4);
        this.hhea.Ascender = this.rf.readShort();
        this.hhea.Descender = this.rf.readShort();
        this.hhea.LineGap = this.rf.readShort();
        this.hhea.advanceWidthMax = this.rf.readUnsignedShort();
        this.hhea.caretSlopeRise = this.rf.readShort();
        this.hhea.caretSlopeRun = this.rf.readShort();
        this.rf.skipBytes(12);
        this.hhea.numberOfHMetrics = this.rf.readUnsignedShort();

        table_location = (int[])this.tables.get("OS/2");
        if (table_location == null) {
			throw new DocumentException("Table 'OS/2' does not exist in " + this.fileName + this.style);
		}
        this.rf.seek(table_location[0]);
        final int version = this.rf.readUnsignedShort();
        this.os_2.xAvgCharWidth = this.rf.readShort();
        this.os_2.fsType = this.rf.readShort();
        this.os_2.ySubscriptXSize = this.rf.readShort();
        this.os_2.ySubscriptYSize = this.rf.readShort();
        this.os_2.ySubscriptYOffset = this.rf.readShort();
        this.os_2.ySuperscriptYSize = this.rf.readShort();
        this.os_2.ySuperscriptYOffset = this.rf.readShort();
        this.os_2.yStrikeoutSize = this.rf.readShort();
        this.os_2.yStrikeoutPosition = this.rf.readShort();
        this.rf.readFully(this.os_2.panose);
        this.rf.skipBytes(16);
        this.rf.readFully(this.os_2.achVendID);
        this.os_2.sTypoAscender = this.rf.readShort();
        this.os_2.sTypoDescender = this.rf.readShort();
        if (this.os_2.sTypoDescender > 0) {
			this.os_2.sTypoDescender = (short)-this.os_2.sTypoDescender;
		}
        this.os_2.ulCodePageRange1 = 0;
        this.os_2.ulCodePageRange2 = 0;
        if (version > 0) {
            this.os_2.ulCodePageRange1 = this.rf.readInt();
            this.os_2.ulCodePageRange2 = this.rf.readInt();
        }
        if (version > 1) {
            this.rf.skipBytes(2);
            this.os_2.sCapHeight = this.rf.readShort();
        } else {
			this.os_2.sCapHeight = (int)(0.7 * this.head.unitsPerEm);
		}

        table_location = (int[])this.tables.get("post");
        if (table_location == null) {
            this.italicAngle = -Math.atan2(this.hhea.caretSlopeRun, this.hhea.caretSlopeRise) * 180 / Math.PI;
            return;
        }
        this.rf.seek(table_location[0] + 4);
        final short mantissa = this.rf.readShort();
        final int fraction = this.rf.readUnsignedShort();
        this.italicAngle = mantissa + fraction / 16384.0d;
        this.underlinePosition = this.rf.readShort();
        this.underlineThickness = this.rf.readShort();
        this.isFixedPitch = this.rf.readInt() != 0;
    }

    /**
     * Gets the Postscript font name.
     * @throws DocumentException the font is invalid
     * @throws IOException the font file could not be read
     * @return the Postscript font name
     */
    String getBaseFont() throws DocumentException, IOException {
        int table_location[];
        table_location = (int[])this.tables.get("name");
        if (table_location == null) {
			throw new DocumentException("Table 'name' does not exist in " + this.fileName + this.style);
		}
        this.rf.seek(table_location[0] + 2);
        final int numRecords = this.rf.readUnsignedShort();
        final int startOfStorage = this.rf.readUnsignedShort();
        for (int k = 0; k < numRecords; ++k) {
            final int platformID = this.rf.readUnsignedShort();
            final int platformEncodingID = this.rf.readUnsignedShort();
            final int languageID = this.rf.readUnsignedShort();
            final int nameID = this.rf.readUnsignedShort();
            final int length = this.rf.readUnsignedShort();
            final int offset = this.rf.readUnsignedShort();
            if (nameID == 6) {
                this.rf.seek(table_location[0] + startOfStorage + offset);
                if (platformID == 0 || platformID == 3) {
					return readUnicodeString(length);
				} else {
					return readStandardString(length);
				}
            }
        }
        final File file = new File(this.fileName);
        return file.getName().replace(' ', '-');
    }

    /** Extracts the names of the font in all the languages available.
     * @param id the name id to retrieve
     * @throws DocumentException on error
     * @throws IOException on error
     */
    private String[][] getNames(final int id) throws DocumentException, IOException {
        int table_location[];
        table_location = (int[])this.tables.get("name");
        if (table_location == null) {
			throw new DocumentException("Table 'name' does not exist in " + this.fileName + this.style);
		}
        this.rf.seek(table_location[0] + 2);
        final int numRecords = this.rf.readUnsignedShort();
        final int startOfStorage = this.rf.readUnsignedShort();
        final ArrayList names = new ArrayList();
        for (int k = 0; k < numRecords; ++k) {
            final int platformID = this.rf.readUnsignedShort();
            final int platformEncodingID = this.rf.readUnsignedShort();
            final int languageID = this.rf.readUnsignedShort();
            final int nameID = this.rf.readUnsignedShort();
            final int length = this.rf.readUnsignedShort();
            final int offset = this.rf.readUnsignedShort();
            if (nameID == id) {
                final int pos = this.rf.getFilePointer();
                this.rf.seek(table_location[0] + startOfStorage + offset);
                String name;
                if (platformID == 0 || platformID == 3 || platformID == 2 && platformEncodingID == 1){
                    name = readUnicodeString(length);
                }
                else {
                    name = readStandardString(length);
                }
                names.add(new String[]{String.valueOf(platformID),
                    String.valueOf(platformEncodingID), String.valueOf(languageID), name});
                this.rf.seek(pos);
            }
        }
        final String thisName[][] = new String[names.size()][];
        for (int k = 0; k < names.size(); ++k) {
			thisName[k] = (String[])names.get(k);
		}
        return thisName;
    }

    /** Extracts all the names of the names-Table
     * @throws DocumentException on error
     * @throws IOException on error
     */
    private String[][] getAllNames() throws DocumentException, IOException {
        int table_location[];
        table_location = (int[])this.tables.get("name");
        if (table_location == null) {
			throw new DocumentException("Table 'name' does not exist in " + this.fileName + this.style);
		}
        this.rf.seek(table_location[0] + 2);
        final int numRecords = this.rf.readUnsignedShort();
        final int startOfStorage = this.rf.readUnsignedShort();
        final ArrayList names = new ArrayList();
        for (int k = 0; k < numRecords; ++k) {
            final int platformID = this.rf.readUnsignedShort();
            final int platformEncodingID = this.rf.readUnsignedShort();
            final int languageID = this.rf.readUnsignedShort();
            final int nameID = this.rf.readUnsignedShort();
            final int length = this.rf.readUnsignedShort();
            final int offset = this.rf.readUnsignedShort();
            final int pos = this.rf.getFilePointer();
            this.rf.seek(table_location[0] + startOfStorage + offset);
            String name;
            if (platformID == 0 || platformID == 3 || platformID == 2 && platformEncodingID == 1){
                name = readUnicodeString(length);
            }
            else {
                name = readStandardString(length);
            }
            names.add(new String[]{String.valueOf(nameID), String.valueOf(platformID),
                    String.valueOf(platformEncodingID), String.valueOf(languageID), name});
            this.rf.seek(pos);
        }
        final String thisName[][] = new String[names.size()][];
        for (int k = 0; k < names.size(); ++k) {
			thisName[k] = (String[])names.get(k);
		}
        return thisName;
    }

    private void checkCff() {
        int table_location[];
        table_location = (int[])this.tables.get("CFF ");
        if (table_location != null) {
            this.cff = true;
            this.cffOffset = table_location[0];
            this.cffLength = table_location[1];
        }
    }

    /** Reads the font data.
     * @param ttfAfm the font as a <CODE>byte</CODE> array, possibly <CODE>null</CODE>
     * @throws DocumentException the font is invalid
     * @throws IOException the font file could not be read
     * @since	2.1.5
     */
    void process(final byte ttfAfm[], final boolean preload) throws DocumentException, IOException {
        this.tables = new HashMap();

        try {
            if (ttfAfm == null) {
				this.rf = new RandomAccessFileOrArray(this.fileName, preload, Document.plainRandomAccess);
			} else {
				this.rf = new RandomAccessFileOrArray(ttfAfm);
			}
            if (this.ttcIndex.length() > 0) {
                final int dirIdx = Integer.parseInt(this.ttcIndex);
                if (dirIdx < 0) {
					throw new DocumentException("The font index for " + this.fileName + " must be positive.");
				}
                final String mainTag = readStandardString(4);
                if (!mainTag.equals("ttcf")) {
					throw new DocumentException(this.fileName + " is not a valid TTC file.");
				}
                this.rf.skipBytes(4);
                final int dirCount = this.rf.readInt();
                if (dirIdx >= dirCount) {
					throw new DocumentException("The font index for " + this.fileName + " must be between 0 and " + (dirCount - 1) + ". It was " + dirIdx + ".");
				}
                this.rf.skipBytes(dirIdx * 4);
                this.directoryOffset = this.rf.readInt();
            }
            this.rf.seek(this.directoryOffset);
            final int ttId = this.rf.readInt();
            if (ttId != 0x00010000 && ttId != 0x4F54544F) {
				throw new DocumentException(this.fileName + " is not a valid TTF or OTF file.");
			}
            final int num_tables = this.rf.readUnsignedShort();
            this.rf.skipBytes(6);
            for (int k = 0; k < num_tables; ++k) {
                final String tag = readStandardString(4);
                this.rf.skipBytes(4);
                final int table_location[] = new int[2];
                table_location[0] = this.rf.readInt();
                table_location[1] = this.rf.readInt();
                this.tables.put(tag, table_location);
            }
            checkCff();
            this.fontName = getBaseFont();
            this.fullName = getNames(4); //full name
            this.familyName = getNames(1); //family name
            this.allNameEntries = getAllNames();
            if (!this.justNames) {
                fillTables();
                readGlyphWidths();
                readCMaps();
                readKerning();
                readBbox();
                this.GlyphWidths = null;
            }
        }
        finally {
            if (this.rf != null) {
                this.rf.close();
                if (!this.embedded) {
					this.rf = null;
				}
            }
        }
    }

    /** Reads a <CODE>String</CODE> from the font file as bytes using the Cp1252
     *  encoding.
     * @param length the length of bytes to read
     * @return the <CODE>String</CODE> read
     * @throws IOException the font file could not be read
     */
    protected String readStandardString(final int length) throws IOException {
        final byte buf[] = new byte[length];
        this.rf.readFully(buf);
        try {
            return new String(buf, WINANSI);
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    /** Reads a Unicode <CODE>String</CODE> from the font file. Each character is
     *  represented by two bytes.
     * @param length the length of bytes to read. The <CODE>String</CODE> will have <CODE>length</CODE>/2
     * characters
     * @return the <CODE>String</CODE> read
     * @throws IOException the font file could not be read
     */
    private String readUnicodeString(int length) throws IOException {
        final StringBuffer buf = new StringBuffer();
        length /= 2;
        for (int k = 0; k < length; ++k) {
            buf.append(this.rf.readChar());
        }
        return buf.toString();
    }

    /** Reads the glyphs widths. The widths are extracted from the table 'hmtx'.
     *  The glyphs are normalized to 1000 units.
     * @throws DocumentException the font is invalid
     * @throws IOException the font file could not be read
     */
    private void readGlyphWidths() throws DocumentException, IOException {
        int table_location[];
        table_location = (int[])this.tables.get("hmtx");
        if (table_location == null) {
			throw new DocumentException("Table 'hmtx' does not exist in " + this.fileName + this.style);
		}
        this.rf.seek(table_location[0]);
        this.GlyphWidths = new int[this.hhea.numberOfHMetrics];
        for (int k = 0; k < this.hhea.numberOfHMetrics; ++k) {
            this.GlyphWidths[k] = this.rf.readUnsignedShort() * 1000 / this.head.unitsPerEm;
            this.rf.readUnsignedShort();
        }
    }

    /** Gets a glyph width.
     * @param glyph the glyph to get the width of
     * @return the width of the glyph in normalized 1000 units
     */
    private int getGlyphWidth(int glyph) {
        if (glyph >= this.GlyphWidths.length) {
			glyph = this.GlyphWidths.length - 1;
		}
        return this.GlyphWidths[glyph];
    }

    private void readBbox() throws DocumentException, IOException {
        int tableLocation[];
        tableLocation = (int[])this.tables.get("head");
        if (tableLocation == null) {
			throw new DocumentException("Table 'head' does not exist in " + this.fileName + this.style);
		}
        this.rf.seek(tableLocation[0] + TrueTypeFontSubSet.HEAD_LOCA_FORMAT_OFFSET);
        final boolean locaShortTable = this.rf.readUnsignedShort() == 0;
        tableLocation = (int[])this.tables.get("loca");
        if (tableLocation == null) {
			return;
		}
        this.rf.seek(tableLocation[0]);
        int locaTable[];
        if (locaShortTable) {
            final int entries = tableLocation[1] / 2;
            locaTable = new int[entries];
            for (int k = 0; k < entries; ++k) {
				locaTable[k] = this.rf.readUnsignedShort() * 2;
			}
        }
        else {
            final int entries = tableLocation[1] / 4;
            locaTable = new int[entries];
            for (int k = 0; k < entries; ++k) {
				locaTable[k] = this.rf.readInt();
			}
        }
        tableLocation = (int[])this.tables.get("glyf");
        if (tableLocation == null) {
			throw new DocumentException("Table 'glyf' does not exist in " + this.fileName + this.style);
		}
        final int tableGlyphOffset = tableLocation[0];
        this.bboxes = new int[locaTable.length - 1][];
        for (int glyph = 0; glyph < locaTable.length - 1; ++glyph) {
            final int start = locaTable[glyph];
            if (start != locaTable[glyph + 1]) {
                this.rf.seek(tableGlyphOffset + start + 2);
                this.bboxes[glyph] = new int[]{
                    this.rf.readShort() * 1000 / this.head.unitsPerEm,
                    this.rf.readShort() * 1000 / this.head.unitsPerEm,
                    this.rf.readShort() * 1000 / this.head.unitsPerEm,
                    this.rf.readShort() * 1000 / this.head.unitsPerEm};
            }
        }
    }

    /** Reads the several maps from the table 'cmap'. The maps of interest are 1.0 for symbolic
     *  fonts and 3.1 for all others. A symbolic font is defined as having the map 3.0.
     * @throws DocumentException the font is invalid
     * @throws IOException the font file could not be read
     */
    private void readCMaps() throws DocumentException, IOException {
        int table_location[];
        table_location = (int[])this.tables.get("cmap");
        if (table_location == null) {
			throw new DocumentException("Table 'cmap' does not exist in " + this.fileName + this.style);
		}
        this.rf.seek(table_location[0]);
        this.rf.skipBytes(2);
        final int num_tables = this.rf.readUnsignedShort();
        this.fontSpecific = false;
        int map10 = 0;
        int map31 = 0;
        int map30 = 0;
        int mapExt = 0;
        for (int k = 0; k < num_tables; ++k) {
            final int platId = this.rf.readUnsignedShort();
            final int platSpecId = this.rf.readUnsignedShort();
            final int offset = this.rf.readInt();
            if (platId == 3 && platSpecId == 0) {
                this.fontSpecific = true;
                map30 = offset;
            }
            else if (platId == 3 && platSpecId == 1) {
                map31 = offset;
            }
            else if (platId == 3 && platSpecId == 10) {
                mapExt = offset;
            }
            if (platId == 1 && platSpecId == 0) {
                map10 = offset;
            }
        }
        if (map10 > 0) {
            this.rf.seek(table_location[0] + map10);
            final int format = this.rf.readUnsignedShort();
            switch (format) {
                case 0:
                    this.cmap10 = readFormat0();
                    break;
                case 4:
                    this.cmap10 = readFormat4();
                    break;
                case 6:
                    this.cmap10 = readFormat6();
                    break;
            }
        }
        if (map31 > 0) {
            this.rf.seek(table_location[0] + map31);
            final int format = this.rf.readUnsignedShort();
            if (format == 4) {
                this.cmap31 = readFormat4();
            }
        }
        if (map30 > 0) {
            this.rf.seek(table_location[0] + map30);
            final int format = this.rf.readUnsignedShort();
            if (format == 4) {
                this.cmap10 = readFormat4();
            }
        }
        if (mapExt > 0) {
            this.rf.seek(table_location[0] + mapExt);
            final int format = this.rf.readUnsignedShort();
            switch (format) {
                case 0:
                    this.cmapExt = readFormat0();
                    break;
                case 4:
                    this.cmapExt = readFormat4();
                    break;
                case 6:
                    this.cmapExt = readFormat6();
                    break;
                case 12:
                    this.cmapExt = readFormat12();
                    break;
            }
        }
    }

    private HashMap readFormat12() throws IOException {
        final HashMap h = new HashMap();
        this.rf.skipBytes(2);
        final int table_lenght = this.rf.readInt();
        this.rf.skipBytes(4);
        final int nGroups = this.rf.readInt();
        for (int k = 0; k < nGroups; k++) {
            final int startCharCode = this.rf.readInt();
            final int endCharCode = this.rf.readInt();
            int startGlyphID = this.rf.readInt();
            for (int i = startCharCode; i <= endCharCode; i++) {
                final int[] r = new int[2];
                r[0] = startGlyphID;
                r[1] = getGlyphWidth(r[0]);
                h.put(new Integer(i), r);
                startGlyphID++;
            }
        }
        return h;
    }

    /** The information in the maps of the table 'cmap' is coded in several formats.
     *  Format 0 is the Apple standard character to glyph index mapping table.
     * @return a <CODE>HashMap</CODE> representing this map
     * @throws IOException the font file could not be read
     */
    private HashMap readFormat0() throws IOException {
        final HashMap h = new HashMap();
        this.rf.skipBytes(4);
        for (int k = 0; k < 256; ++k) {
            final int r[] = new int[2];
            r[0] = this.rf.readUnsignedByte();
            r[1] = getGlyphWidth(r[0]);
            h.put(new Integer(k), r);
        }
        return h;
    }

    /** The information in the maps of the table 'cmap' is coded in several formats.
     *  Format 4 is the Microsoft standard character to glyph index mapping table.
     * @return a <CODE>HashMap</CODE> representing this map
     * @throws IOException the font file could not be read
     */
    private HashMap readFormat4() throws IOException {
        final HashMap h = new HashMap();
        final int table_lenght = this.rf.readUnsignedShort();
        this.rf.skipBytes(2);
        final int segCount = this.rf.readUnsignedShort() / 2;
        this.rf.skipBytes(6);
        final int endCount[] = new int[segCount];
        for (int k = 0; k < segCount; ++k) {
            endCount[k] = this.rf.readUnsignedShort();
        }
        this.rf.skipBytes(2);
        final int startCount[] = new int[segCount];
        for (int k = 0; k < segCount; ++k) {
            startCount[k] = this.rf.readUnsignedShort();
        }
        final int idDelta[] = new int[segCount];
        for (int k = 0; k < segCount; ++k) {
            idDelta[k] = this.rf.readUnsignedShort();
        }
        final int idRO[] = new int[segCount];
        for (int k = 0; k < segCount; ++k) {
            idRO[k] = this.rf.readUnsignedShort();
        }
        final int glyphId[] = new int[table_lenght / 2 - 8 - segCount * 4];
        for (int k = 0; k < glyphId.length; ++k) {
            glyphId[k] = this.rf.readUnsignedShort();
        }
        for (int k = 0; k < segCount; ++k) {
            int glyph;
            for (int j = startCount[k]; j <= endCount[k] && j != 0xFFFF; ++j) {
                if (idRO[k] == 0) {
                    glyph = j + idDelta[k] & 0xFFFF;
                }
                else {
                    final int idx = k + idRO[k] / 2 - segCount + j - startCount[k];
                    if (idx >= glyphId.length) {
						continue;
					}
                    glyph = glyphId[idx] + idDelta[k] & 0xFFFF;
                }
                final int r[] = new int[2];
                r[0] = glyph;
                r[1] = getGlyphWidth(r[0]);
                h.put(new Integer(this.fontSpecific ? (j & 0xff00) == 0xf000 ? j & 0xff : j : j), r);
            }
        }
        return h;
    }

    /** The information in the maps of the table 'cmap' is coded in several formats.
     *  Format 6 is a trimmed table mapping. It is similar to format 0 but can have
     *  less than 256 entries.
     * @return a <CODE>HashMap</CODE> representing this map
     * @throws IOException the font file could not be read
     */
    private HashMap readFormat6() throws IOException {
        final HashMap h = new HashMap();
        this.rf.skipBytes(4);
        final int start_code = this.rf.readUnsignedShort();
        final int code_count = this.rf.readUnsignedShort();
        for (int k = 0; k < code_count; ++k) {
            final int r[] = new int[2];
            r[0] = this.rf.readUnsignedShort();
            r[1] = getGlyphWidth(r[0]);
            h.put(new Integer(k + start_code), r);
        }
        return h;
    }

    /** Reads the kerning information from the 'kern' table.
     * @throws IOException the font file could not be read
     */
    private void readKerning() throws IOException {
        int table_location[];
        table_location = (int[])this.tables.get("kern");
        if (table_location == null) {
			return;
		}
        this.rf.seek(table_location[0] + 2);
        final int nTables = this.rf.readUnsignedShort();
        int checkpoint = table_location[0] + 4;
        int length = 0;
        for (int k = 0; k < nTables; ++k) {
            checkpoint += length;
            this.rf.seek(checkpoint);
            this.rf.skipBytes(2);
            length = this.rf.readUnsignedShort();
            final int coverage = this.rf.readUnsignedShort();
            if ((coverage & 0xfff7) == 0x0001) {
                final int nPairs = this.rf.readUnsignedShort();
                this.rf.skipBytes(6);
                for (int j = 0; j < nPairs; ++j) {
                    final int pair = this.rf.readInt();
                    final int value = this.rf.readShort() * 1000 / this.head.unitsPerEm;
                    this.kerning.put(pair, value);
                }
            }
        }
    }

    /** Gets the kerning between two Unicode chars.
     * @param char1 the first char
     * @param char2 the second char
     * @return the kerning to be applied
     */
    @Override
	public int getKerning(final int char1, final int char2) {
        int metrics[] = getMetricsTT(char1);
        if (metrics == null) {
			return 0;
		}
        final int c1 = metrics[0];
        metrics = getMetricsTT(char2);
        if (metrics == null) {
			return 0;
		}
        final int c2 = metrics[0];
        return this.kerning.get((c1 << 16) + c2);
    }

    /** Gets the width from the font according to the unicode char <CODE>c</CODE>.
     * If the <CODE>name</CODE> is null it's a symbolic font.
     * @param c the unicode char
     * @param name the glyph name
     * @return the width of the char
     */
    @Override
	int getRawWidth(final int c, final String name) {
        final int[] metric = getMetricsTT(c);
        if (metric == null) {
			return 0;
		}
        return metric[1];
    }

    /** Generates the font descriptor for this font.
     * @return the PdfDictionary containing the font descriptor or <CODE>null</CODE>
     * @param subsetPrefix the subset prefix
     * @param fontStream the indirect reference to a PdfStream containing the font or <CODE>null</CODE>
     */
    protected PdfDictionary getFontDescriptor(final PdfIndirectReference fontStream, final String subsetPrefix, final PdfIndirectReference cidset) {
        final PdfDictionary dic = new PdfDictionary(PdfName.FONTDESCRIPTOR);
        dic.put(PdfName.ASCENT, new PdfNumber(this.os_2.sTypoAscender * 1000 / this.head.unitsPerEm));
        dic.put(PdfName.CAPHEIGHT, new PdfNumber(this.os_2.sCapHeight * 1000 / this.head.unitsPerEm));
        dic.put(PdfName.DESCENT, new PdfNumber(this.os_2.sTypoDescender * 1000 / this.head.unitsPerEm));
        dic.put(PdfName.FONTBBOX, new PdfRectangle(
        this.head.xMin * 1000 / this.head.unitsPerEm,
        this.head.yMin * 1000 / this.head.unitsPerEm,
        this.head.xMax * 1000 / this.head.unitsPerEm,
        this.head.yMax * 1000 / this.head.unitsPerEm));
        if (cidset != null) {
			dic.put(PdfName.CIDSET, cidset);
		}
        if (this.cff) {
            if (this.encoding.startsWith("Identity-")) {
				dic.put(PdfName.FONTNAME, new PdfName(subsetPrefix + this.fontName+"-"+this.encoding));
			} else {
				dic.put(PdfName.FONTNAME, new PdfName(subsetPrefix + this.fontName + this.style));
			}
        } else {
			dic.put(PdfName.FONTNAME, new PdfName(subsetPrefix + this.fontName + this.style));
		}
        dic.put(PdfName.ITALICANGLE, new PdfNumber(this.italicAngle));
        dic.put(PdfName.STEMV, new PdfNumber(80));
        if (fontStream != null) {
            if (this.cff) {
				dic.put(PdfName.FONTFILE3, fontStream);
			} else {
				dic.put(PdfName.FONTFILE2, fontStream);
			}
        }
        int flags = 0;
        if (this.isFixedPitch) {
			flags |= 1;
		}
        flags |= this.fontSpecific ? 4 : 32;
        if ((this.head.macStyle & 2) != 0) {
			flags |= 64;
		}
        if ((this.head.macStyle & 1) != 0) {
			flags |= 262144;
		}
        dic.put(PdfName.FLAGS, new PdfNumber(flags));

        return dic;
    }

    /** Generates the font dictionary for this font.
     * @return the PdfDictionary containing the font dictionary
     * @param subsetPrefix the subset prefix
     * @param firstChar the first valid character
     * @param lastChar the last valid character
     * @param shortTag a 256 bytes long <CODE>byte</CODE> array where each unused byte is represented by 0
     * @param fontDescriptor the indirect reference to a PdfDictionary containing the font descriptor or <CODE>null</CODE>
     */
    private PdfDictionary getFontBaseType(final PdfIndirectReference fontDescriptor, final String subsetPrefix, int firstChar, final int lastChar, final byte shortTag[]) {
        final PdfDictionary dic = new PdfDictionary(PdfName.FONT);
        if (this.cff) {
            dic.put(PdfName.SUBTYPE, PdfName.TYPE1);
            dic.put(PdfName.BASEFONT, new PdfName(this.fontName + this.style));
        }
        else {
            dic.put(PdfName.SUBTYPE, PdfName.TRUETYPE);
            dic.put(PdfName.BASEFONT, new PdfName(subsetPrefix + this.fontName + this.style));
        }
        dic.put(PdfName.BASEFONT, new PdfName(subsetPrefix + this.fontName + this.style));
        if (!this.fontSpecific) {
            for (int k = firstChar; k <= lastChar; ++k) {
                if (!this.differences[k].equals(notdef)) {
                    firstChar = k;
                    break;
                }
            }
        if (this.encoding.equals("Cp1252") || this.encoding.equals("MacRoman")) {
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
        if (fontDescriptor != null) {
			dic.put(PdfName.FONTDESCRIPTOR, fontDescriptor);
		}
        return dic;
    }

    protected byte[] getFullFont() throws IOException {
        RandomAccessFileOrArray rf2 = null;
        try {
            rf2 = new RandomAccessFileOrArray(this.rf);
            rf2.reOpen();
            final byte b[] = new byte[rf2.length()];
            rf2.readFully(b);
            return b;
        }
        finally {
            try {if (rf2 != null) {rf2.close();}} catch (final Exception e) {}
        }
    }

    private static int[] compactRanges(final ArrayList ranges) {
        final ArrayList simp = new ArrayList();
        for (int k = 0; k < ranges.size(); ++k) {
            final int[] r = (int[])ranges.get(k);
            for (int j = 0; j < r.length; j += 2) {
                simp.add(new int[]{Math.max(0, Math.min(r[j], r[j + 1])), Math.min(0xffff, Math.max(r[j], r[j + 1]))});
            }
        }
        for (int k1 = 0; k1 < simp.size() - 1; ++k1) {
            for (int k2 = k1 + 1; k2 < simp.size(); ++k2) {
                final int[] r1 = (int[])simp.get(k1);
                final int[] r2 = (int[])simp.get(k2);
                if (r1[0] >= r2[0] && r1[0] <= r2[1] || r1[1] >= r2[0] && r1[0] <= r2[1]) {
                    r1[0] = Math.min(r1[0], r2[0]);
                    r1[1] = Math.max(r1[1], r2[1]);
                    simp.remove(k2);
                    --k2;
                }
            }
        }
        final int[] s = new int[simp.size() * 2];
        for (int k = 0; k < simp.size(); ++k) {
            final int[] r = (int[])simp.get(k);
            s[k * 2] = r[0];
            s[k * 2 + 1] = r[1];
        }
        return s;
    }

    protected void addRangeUni(final HashMap longTag, final boolean includeMetrics, final boolean subsetp) {
        if (!subsetp && (this.subsetRanges != null || this.directoryOffset > 0)) {
            final int[] rg = this.subsetRanges == null && this.directoryOffset > 0 ? new int[]{0, 0xffff} : compactRanges(this.subsetRanges);
            HashMap usemap;
            if (!this.fontSpecific && this.cmap31 != null) {
				usemap = this.cmap31;
			} else if (this.fontSpecific && this.cmap10 != null) {
				usemap = this.cmap10;
			} else if (this.cmap31 != null) {
				usemap = this.cmap31;
			} else {
				usemap = this.cmap10;
			}
            for (final Iterator it = usemap.entrySet().iterator(); it.hasNext();) {
                final Map.Entry e = (Map.Entry)it.next();
                final int[] v = (int[])e.getValue();
                final Integer gi = new Integer(v[0]);
                if (longTag.containsKey(gi)) {
					continue;
				}
                final int c = ((Integer)e.getKey()).intValue();
                boolean skip = true;
                for (int k = 0; k < rg.length; k += 2) {
                    if (c >= rg[k] && c <= rg[k + 1]) {
                        skip = false;
                        break;
                    }
                }
                if (!skip) {
					longTag.put(gi, includeMetrics ? new int[]{v[0], v[1], c} : null);
				}
            }
        }
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
        String subsetPrefix = "";
        if (this.embedded) {
            if (this.cff) {
                pobj = new StreamFont(readCffFont(), "Type1C", this.compressionLevel);
                obj = writer.addToBody(pobj);
                ind_font = obj.getIndirectReference();
            }
            else {
                if (subsetp) {
					subsetPrefix = createSubsetPrefix();
				}
                final HashMap glyphs = new HashMap();
                for (int k = firstChar; k <= lastChar; ++k) {
                    if (shortTag[k] != 0) {
                        int[] metrics = null;
                        if (this.specialMap != null) {
                            final int[] cd = GlyphList.nameToUnicode(this.differences[k]);
                            if (cd != null) {
								metrics = getMetricsTT(cd[0]);
							}
                        }
                        else {
                            if (this.fontSpecific) {
								metrics = getMetricsTT(k);
							} else {
								metrics = getMetricsTT(this.unicodeDifferences[k]);
							}
                        }
                        if (metrics != null) {
							glyphs.put(new Integer(metrics[0]), null);
						}
                    }
                }
                addRangeUni(glyphs, false, subsetp);
                byte[] b = null;
                if (subsetp || this.directoryOffset != 0 || this.subsetRanges != null) {
                    final TrueTypeFontSubSet sb = new TrueTypeFontSubSet(this.fileName, new RandomAccessFileOrArray(this.rf), glyphs, this.directoryOffset, true, !subsetp);
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
        }
        pobj = getFontDescriptor(ind_font, subsetPrefix, null);
        if (pobj != null){
            obj = writer.addToBody(pobj);
            ind_font = obj.getIndirectReference();
        }
        pobj = getFontBaseType(ind_font, subsetPrefix, firstChar, lastChar, shortTag);
        writer.addToBody(pobj, ref);
    }

    /**
     * If this font file is using the Compact Font File Format, then this method
     * will return the raw bytes needed for the font stream. If this method is
     * ever made public: make sure to add a test if (cff == true).
     * @return	a byte array
     * @since	2.1.3
     */
    protected byte[] readCffFont() throws IOException {
        final RandomAccessFileOrArray rf2 = new RandomAccessFileOrArray(this.rf);
        final byte b[] = new byte[this.cffLength];
        try {
            rf2.reOpen();
            rf2.seek(this.cffOffset);
            rf2.readFully(b);
        }
        finally {
            try {
                rf2.close();
            }
            catch (final Exception e) {
                // empty on purpose
            }
        }
    	return b;
    }

    /**
     * Returns a PdfStream object with the full font program.
     * @return	a PdfStream with the font program
     * @since	2.1.3
     */
    @Override
	public PdfStream getFullFontStream() throws IOException, DocumentException {
        if (this.cff) {
            return new StreamFont(readCffFont(), "Type1C", this.compressionLevel);
        }
        else {
        	final byte[] b = getFullFont();
        	final int lengths[] = new int[]{b.length};
        	return new StreamFont(b, lengths, this.compressionLevel);
        }
    }

    /** Gets the font parameter identified by <CODE>key</CODE>. Valid values
     * for <CODE>key</CODE> are <CODE>ASCENT</CODE>, <CODE>CAPHEIGHT</CODE>, <CODE>DESCENT</CODE>
     * and <CODE>ITALICANGLE</CODE>.
     * @param key the parameter to be extracted
     * @param fontSize the font size in points
     * @return the parameter in points
     */
    @Override
	public float getFontDescriptor(final int key, final float fontSize) {
        switch (key) {
            case ASCENT:
                return this.os_2.sTypoAscender * fontSize / this.head.unitsPerEm;
            case CAPHEIGHT:
                return this.os_2.sCapHeight * fontSize / this.head.unitsPerEm;
            case DESCENT:
                return this.os_2.sTypoDescender * fontSize / this.head.unitsPerEm;
            case ITALICANGLE:
                return (float)this.italicAngle;
            case BBOXLLX:
                return fontSize * this.head.xMin / this.head.unitsPerEm;
            case BBOXLLY:
                return fontSize * this.head.yMin / this.head.unitsPerEm;
            case BBOXURX:
                return fontSize * this.head.xMax / this.head.unitsPerEm;
            case BBOXURY:
                return fontSize * this.head.yMax / this.head.unitsPerEm;
            case AWT_ASCENT:
                return fontSize * this.hhea.Ascender / this.head.unitsPerEm;
            case AWT_DESCENT:
                return fontSize * this.hhea.Descender / this.head.unitsPerEm;
            case AWT_LEADING:
                return fontSize * this.hhea.LineGap / this.head.unitsPerEm;
            case AWT_MAXADVANCE:
                return fontSize * this.hhea.advanceWidthMax / this.head.unitsPerEm;
            case UNDERLINE_POSITION:
                return (this.underlinePosition - this.underlineThickness / 2) * fontSize / this.head.unitsPerEm;
            case UNDERLINE_THICKNESS:
                return this.underlineThickness * fontSize / this.head.unitsPerEm;
            case STRIKETHROUGH_POSITION:
                return this.os_2.yStrikeoutPosition * fontSize / this.head.unitsPerEm;
            case STRIKETHROUGH_THICKNESS:
                return this.os_2.yStrikeoutSize * fontSize / this.head.unitsPerEm;
            case SUBSCRIPT_SIZE:
                return this.os_2.ySubscriptYSize * fontSize / this.head.unitsPerEm;
            case SUBSCRIPT_OFFSET:
                return -this.os_2.ySubscriptYOffset * fontSize / this.head.unitsPerEm;
            case SUPERSCRIPT_SIZE:
                return this.os_2.ySuperscriptYSize * fontSize / this.head.unitsPerEm;
            case SUPERSCRIPT_OFFSET:
                return this.os_2.ySuperscriptYOffset * fontSize / this.head.unitsPerEm;
        }
        return 0;
    }

    /** Gets the glyph index and metrics for a character.
     * @param c the character
     * @return an <CODE>int</CODE> array with {glyph index, width}
     */
    public int[] getMetricsTT(final int c) {
        if (this.cmapExt != null) {
			return (int[])this.cmapExt.get(new Integer(c));
		}
        if (!this.fontSpecific && this.cmap31 != null) {
			return (int[])this.cmap31.get(new Integer(c));
		}
        if (this.fontSpecific && this.cmap10 != null) {
			return (int[])this.cmap10.get(new Integer(c));
		}
        if (this.cmap31 != null) {
			return (int[])this.cmap31.get(new Integer(c));
		}
        if (this.cmap10 != null) {
			return (int[])this.cmap10.get(new Integer(c));
		}
        return null;
    }

    /** Gets the postscript font name.
     * @return the postscript font name
     */
    @Override
	public String getPostscriptFontName() {
        return this.fontName;
    }

    /** Gets the code pages supported by the font.
     * @return the code pages supported by the font
     */
    @Override
	public String[] getCodePagesSupported() {
        final long cp = ((long)this.os_2.ulCodePageRange2 << 32) + (this.os_2.ulCodePageRange1 & 0xffffffffL);
        int count = 0;
        long bit = 1;
        for (int k = 0; k < 64; ++k) {
            if ((cp & bit) != 0 && codePages[k] != null) {
				++count;
			}
            bit <<= 1;
        }
        final String ret[] = new String[count];
        count = 0;
        bit = 1;
        for (int k = 0; k < 64; ++k) {
            if ((cp & bit) != 0 && codePages[k] != null) {
				ret[count++] = codePages[k];
			}
            bit <<= 1;
        }
        return ret;
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
        return this.fullName;
    }

    /** Gets all the entries of the Names-Table. If it is a True Type font
     * each array element will have {Name ID, Platform ID, Platform Encoding ID,
     * Language ID, font name}. The interpretation of this values can be
     * found in the Open Type specification, chapter 2, in the 'name' table.<br>
     * For the other fonts the array has a single element with {"", "", "",
     * font name}.
     * @return the full name of the font
     */
    @Override
	public String[][] getAllNameEntries() {
        return this.allNameEntries;
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
        return this.familyName;
    }

    /** Checks if the font has any kerning pairs.
     * @return <CODE>true</CODE> if the font has any kerning pairs
     */
    @Override
	public boolean hasKernPairs() {
        return this.kerning.size() > 0;
    }

    /**
     * Sets the font name that will appear in the pdf font dictionary.
     * Use with care as it can easily make a font unreadable if not embedded.
     * @param name the new font name
     */
    @Override
	public void setPostscriptFontName(final String name) {
        this.fontName = name;
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
        int metrics[] = getMetricsTT(char1);
        if (metrics == null) {
			return false;
		}
        final int c1 = metrics[0];
        metrics = getMetricsTT(char2);
        if (metrics == null) {
			return false;
		}
        final int c2 = metrics[0];
        this.kerning.put((c1 << 16) + c2, kern);
        return true;
    }

    @Override
	protected int[] getRawCharBBox(final int c, final String name) {
        HashMap map = null;
        if (name == null || this.cmap31 == null) {
			map = this.cmap10;
		} else {
			map = this.cmap31;
		}
        if (map == null) {
			return null;
		}
        final int metric[] = (int[])map.get(new Integer(c));
        if (metric == null || this.bboxes == null) {
			return null;
		}
        return this.bboxes[metric[0]];
    }
}