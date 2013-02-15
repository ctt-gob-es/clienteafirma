/*
 * $Id: TrueTypeFontSubSet.java 3427 2008-05-24 18:32:31Z xlv $
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;

/** Subsets a True Type font by removing the unneeded glyphs from
 * the font.
 *
 * @author  Paulo Soares (psoares@consiste.pt)
 */
class TrueTypeFontSubSet {
    private static final String tableNamesSimple[] = {"cvt ", "fpgm", "glyf", "head",
        "hhea", "hmtx", "loca", "maxp", "prep"};
    private static final String tableNamesCmap[] = {"cmap", "cvt ", "fpgm", "glyf", "head",
        "hhea", "hmtx", "loca", "maxp", "prep"};
    private static final String tableNamesExtra[] = {"OS/2", "cmap", "cvt ", "fpgm", "glyf", "head",
        "hhea", "hmtx", "loca", "maxp", "name, prep"};
    private static final int entrySelectors[] = {0,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4};
    private static final int TABLE_CHECKSUM = 0;
    private static final int TABLE_OFFSET = 1;
    private static final int TABLE_LENGTH = 2;
    static final int HEAD_LOCA_FORMAT_OFFSET = 51;

    private static final int ARG_1_AND_2_ARE_WORDS = 1;
    private static final int WE_HAVE_A_SCALE = 8;
    private static final int MORE_COMPONENTS = 32;
    private static final int WE_HAVE_AN_X_AND_Y_SCALE = 64;
    private static final int WE_HAVE_A_TWO_BY_TWO = 128;


    /** Contains the location of the several tables. The key is the name of
     * the table and the value is an <CODE>int[3]</CODE> where position 0
     * is the checksum, position 1 is the offset from the start of the file
     * and position 2 is the length of the table.
     */
    private HashMap tableDirectory;
    /** The file in use.
     */
    private final RandomAccessFileOrArray rf;
    /** The file name.
     */
    private final String fileName;
    private final boolean includeCmap;
    private final boolean includeExtras;
    private boolean locaShortTable;
    private int locaTable[];
    private final HashMap glyphsUsed;
    private final ArrayList glyphsInList;
    private int tableGlyphOffset;
    private int newLocaTable[];
    private byte newLocaTableOut[];
    private byte newGlyfTable[];
    private int glyfTableRealSize;
    private int locaTableRealSize;
    private byte outFont[];
    private int fontPtr;
    private final int directoryOffset;

    /** Creates a new TrueTypeFontSubSet
     * @param directoryOffset The offset from the start of the file to the table directory
     * @param fileName the file name of the font
     * @param glyphsUsed the glyphs used
     * @param includeCmap <CODE>true</CODE> if the table cmap is to be included in the generated font
     */
    TrueTypeFontSubSet(final String fileName, final RandomAccessFileOrArray rf, final HashMap glyphsUsed, final int directoryOffset, final boolean includeCmap, final boolean includeExtras) {
        this.fileName = fileName;
        this.rf = rf;
        this.glyphsUsed = glyphsUsed;
        this.includeCmap = includeCmap;
        this.includeExtras = includeExtras;
        this.directoryOffset = directoryOffset;
        this.glyphsInList = new ArrayList(glyphsUsed.keySet());
    }

    /** Does the actual work of subsetting the font.
     * @throws IOException on error
     * @throws DocumentException on error
     * @return the subset font
     */
    byte[] process() throws IOException, DocumentException {
        try {
            this.rf.reOpen();
            createTableDirectory();
            readLoca();
            flatGlyphs();
            createNewGlyphTables();
            locaTobytes();
            assembleFont();
            return this.outFont;
        }
        finally {
            try {
                this.rf.close();
            }
            catch (final Exception e) {
                // empty on purpose
            }
        }
    }

    private void assembleFont() throws IOException {
        int tableLocation[];
        int fullFontSize = 0;
        String tableNames[];
        if (this.includeExtras) {
			tableNames = tableNamesExtra;
		} else {
            if (this.includeCmap) {
				tableNames = tableNamesCmap;
			} else {
				tableNames = tableNamesSimple;
			}
        }
        int tablesUsed = 2;
        int len = 0;
        for (final String name : tableNames) {
            if (name.equals("glyf") || name.equals("loca")) {
				continue;
			}
            tableLocation = (int[])this.tableDirectory.get(name);
            if (tableLocation == null) {
				continue;
			}
            ++tablesUsed;
            fullFontSize += tableLocation[TABLE_LENGTH] + 3 & ~3;
        }
        fullFontSize += this.newLocaTableOut.length;
        fullFontSize += this.newGlyfTable.length;
        int ref = 16 * tablesUsed + 12;
        fullFontSize += ref;
        this.outFont = new byte[fullFontSize];
        this.fontPtr = 0;
        writeFontInt(0x00010000);
        writeFontShort(tablesUsed);
        final int selector = entrySelectors[tablesUsed];
        writeFontShort((1 << selector) * 16);
        writeFontShort(selector);
        writeFontShort((tablesUsed - (1 << selector)) * 16);
        for (final String name : tableNames) {
            tableLocation = (int[])this.tableDirectory.get(name);
            if (tableLocation == null) {
				continue;
			}
            writeFontString(name);
            if (name.equals("glyf")) {
                writeFontInt(calculateChecksum(this.newGlyfTable));
                len = this.glyfTableRealSize;
            }
            else if (name.equals("loca")) {
                writeFontInt(calculateChecksum(this.newLocaTableOut));
                len = this.locaTableRealSize;
            }
            else {
                writeFontInt(tableLocation[TABLE_CHECKSUM]);
                len = tableLocation[TABLE_LENGTH];
            }
            writeFontInt(ref);
            writeFontInt(len);
            ref += len + 3 & ~3;
        }
        for (final String name : tableNames) {
            tableLocation = (int[])this.tableDirectory.get(name);
            if (tableLocation == null) {
				continue;
			}
            if (name.equals("glyf")) {
                System.arraycopy(this.newGlyfTable, 0, this.outFont, this.fontPtr, this.newGlyfTable.length);
                this.fontPtr += this.newGlyfTable.length;
                this.newGlyfTable = null;
            }
            else if (name.equals("loca")) {
                System.arraycopy(this.newLocaTableOut, 0, this.outFont, this.fontPtr, this.newLocaTableOut.length);
                this.fontPtr += this.newLocaTableOut.length;
                this.newLocaTableOut = null;
            }
            else {
                this.rf.seek(tableLocation[TABLE_OFFSET]);
                this.rf.readFully(this.outFont, this.fontPtr, tableLocation[TABLE_LENGTH]);
                this.fontPtr += tableLocation[TABLE_LENGTH] + 3 & ~3;
            }
        }
    }

    private void createTableDirectory() throws IOException, DocumentException {
        this.tableDirectory = new HashMap();
        this.rf.seek(this.directoryOffset);
        final int id = this.rf.readInt();
        if (id != 0x00010000) {
			throw new DocumentException(this.fileName + " is not a true type file.");
		}
        final int num_tables = this.rf.readUnsignedShort();
        this.rf.skipBytes(6);
        for (int k = 0; k < num_tables; ++k) {
            final String tag = readStandardString(4);
            final int tableLocation[] = new int[3];
            tableLocation[TABLE_CHECKSUM] = this.rf.readInt();
            tableLocation[TABLE_OFFSET] = this.rf.readInt();
            tableLocation[TABLE_LENGTH] = this.rf.readInt();
            this.tableDirectory.put(tag, tableLocation);
        }
    }

    private void readLoca() throws IOException, DocumentException {
        int tableLocation[];
        tableLocation = (int[])this.tableDirectory.get("head");
        if (tableLocation == null) {
			throw new DocumentException("Table 'head' does not exist in " + this.fileName);
		}
        this.rf.seek(tableLocation[TABLE_OFFSET] + HEAD_LOCA_FORMAT_OFFSET);
        this.locaShortTable = this.rf.readUnsignedShort() == 0;
        tableLocation = (int[])this.tableDirectory.get("loca");
        if (tableLocation == null) {
			throw new DocumentException("Table 'loca' does not exist in " + this.fileName);
		}
        this.rf.seek(tableLocation[TABLE_OFFSET]);
        if (this.locaShortTable) {
            final int entries = tableLocation[TABLE_LENGTH] / 2;
            this.locaTable = new int[entries];
            for (int k = 0; k < entries; ++k) {
				this.locaTable[k] = this.rf.readUnsignedShort() * 2;
			}
        }
        else {
            final int entries = tableLocation[TABLE_LENGTH] / 4;
            this.locaTable = new int[entries];
            for (int k = 0; k < entries; ++k) {
				this.locaTable[k] = this.rf.readInt();
			}
        }
    }

    private void createNewGlyphTables() throws IOException {
        this.newLocaTable = new int[this.locaTable.length];
        final int activeGlyphs[] = new int[this.glyphsInList.size()];
        for (int k = 0; k < activeGlyphs.length; ++k) {
			activeGlyphs[k] = ((Integer)this.glyphsInList.get(k)).intValue();
		}
        Arrays.sort(activeGlyphs);
        int glyfSize = 0;
        for (final int glyph : activeGlyphs) {
            glyfSize += this.locaTable[glyph + 1] - this.locaTable[glyph];
        }
        this.glyfTableRealSize = glyfSize;
        glyfSize = glyfSize + 3 & ~3;
        this.newGlyfTable = new byte[glyfSize];
        int glyfPtr = 0;
        int listGlyf = 0;
        for (int k = 0; k < this.newLocaTable.length; ++k) {
            this.newLocaTable[k] = glyfPtr;
            if (listGlyf < activeGlyphs.length && activeGlyphs[listGlyf] == k) {
                ++listGlyf;
                this.newLocaTable[k] = glyfPtr;
                final int start = this.locaTable[k];
                final int len = this.locaTable[k + 1] - start;
                if (len > 0) {
                    this.rf.seek(this.tableGlyphOffset + start);
                    this.rf.readFully(this.newGlyfTable, glyfPtr, len);
                    glyfPtr += len;
                }
            }
        }
    }

    private void locaTobytes() {
        if (this.locaShortTable) {
			this.locaTableRealSize = this.newLocaTable.length * 2;
		} else {
			this.locaTableRealSize = this.newLocaTable.length * 4;
		}
        this.newLocaTableOut = new byte[this.locaTableRealSize + 3 & ~3];
        this.outFont = this.newLocaTableOut;
        this.fontPtr = 0;
        for (final int element : this.newLocaTable) {
            if (this.locaShortTable) {
				writeFontShort(element / 2);
			} else {
				writeFontInt(element);
			}
        }

    }

    private void flatGlyphs() throws IOException, DocumentException {
        int tableLocation[];
        tableLocation = (int[])this.tableDirectory.get("glyf");
        if (tableLocation == null) {
			throw new DocumentException("Table 'glyf' does not exist in " + this.fileName);
		}
        final Integer glyph0 = new Integer(0);
        if (!this.glyphsUsed.containsKey(glyph0)) {
            this.glyphsUsed.put(glyph0, null);
            this.glyphsInList.add(glyph0);
        }
        this.tableGlyphOffset = tableLocation[TABLE_OFFSET];
        for (int k = 0; k < this.glyphsInList.size(); ++k) {
            final int glyph = ((Integer)this.glyphsInList.get(k)).intValue();
            checkGlyphComposite(glyph);
        }
    }

    private void checkGlyphComposite(final int glyph) throws IOException {
        final int start = this.locaTable[glyph];
        if (start == this.locaTable[glyph + 1]) {
			return;
		}
        this.rf.seek(this.tableGlyphOffset + start);
        final int numContours = this.rf.readShort();
        if (numContours >= 0) {
			return;
		}
        this.rf.skipBytes(8);
        for(;;) {
            final int flags = this.rf.readUnsignedShort();
            final Integer cGlyph = new Integer(this.rf.readUnsignedShort());
            if (!this.glyphsUsed.containsKey(cGlyph)) {
                this.glyphsUsed.put(cGlyph, null);
                this.glyphsInList.add(cGlyph);
            }
            if ((flags & MORE_COMPONENTS) == 0) {
				return;
			}
            int skip;
            if ((flags & ARG_1_AND_2_ARE_WORDS) != 0) {
				skip = 4;
			} else {
				skip = 2;
			}
            if ((flags & WE_HAVE_A_SCALE) != 0) {
				skip += 2;
			} else if ((flags & WE_HAVE_AN_X_AND_Y_SCALE) != 0) {
				skip += 4;
			}
            if ((flags & WE_HAVE_A_TWO_BY_TWO) != 0) {
				skip += 8;
			}
            this.rf.skipBytes(skip);
        }
    }

    /** Reads a <CODE>String</CODE> from the font file as bytes using the Cp1252
     *  encoding.
     * @param length the length of bytes to read
     * @return the <CODE>String</CODE> read
     * @throws IOException the font file could not be read
     */
    private String readStandardString(final int length) throws IOException {
        final byte buf[] = new byte[length];
        this.rf.readFully(buf);
        try {
            return new String(buf, BaseFont.WINANSI);
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    private void writeFontShort(final int n) {
        this.outFont[this.fontPtr++] = (byte)(n >> 8);
        this.outFont[this.fontPtr++] = (byte)n;
    }

    private void writeFontInt(final int n) {
        this.outFont[this.fontPtr++] = (byte)(n >> 24);
        this.outFont[this.fontPtr++] = (byte)(n >> 16);
        this.outFont[this.fontPtr++] = (byte)(n >> 8);
        this.outFont[this.fontPtr++] = (byte)n;
    }

    private void writeFontString(final String s) {
        final byte b[] = PdfEncodings.convertToBytes(s, BaseFont.WINANSI);
        System.arraycopy(b, 0, this.outFont, this.fontPtr, b.length);
        this.fontPtr += b.length;
    }

    private int calculateChecksum(final byte b[]) {
        final int len = b.length / 4;
        int v0 = 0;
        int v1 = 0;
        int v2 = 0;
        int v3 = 0;
        int ptr = 0;
        for (int k = 0; k < len; ++k) {
            v3 += b[ptr++] & 0xff;
            v2 += b[ptr++] & 0xff;
            v1 += b[ptr++] & 0xff;
            v0 += b[ptr++] & 0xff;
        }
        return v0 + (v1 << 8) + (v2 << 16) + (v3 << 24);
    }
}
