/*
 *
 * Copyright 2003 Sivan Toledo
 *
 * The contents of this file are subject to the Mozilla Public License Version 1.1
 * (the "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the License.
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
 */

/*
 * Comments by Sivan Toledo:
 * I created this class in order to add to iText the ability to utilize
 * OpenType fonts with CFF glyphs (these usually have an .otf extension).
 * The CFF font within the CFF table of the OT font might be either a CID
 * or a Type1 font. (CFF fonts may also contain multiple fonts; I do not
 * know if this is allowed in an OT table). The PDF spec, however, only
 * allow a CID font with an Identity-H or Identity-V encoding. Otherwise,
 * you are limited to an 8-bit encoding.
 * Adobe fonts come in both flavors. That is, the OTFs sometimes have
 * a CID CFF inside (for Japanese fonts), and sometimes a Type1 CFF
 * (virtually all the others, Latin/Greek/Cyrillic). So to easily use
 * all the glyphs in the latter, without creating multiple 8-bit encoding,
 * I wrote this class, whose main purpose is to convert a Type1 font inside
 * a CFF container (which might include other fonts) into a CID CFF font
 * that can be directly embeded in the PDF.
 *
 * Limitations of the current version:
 * 1. It does not extract a single CID font from a CFF that contains that
 *    particular CID along with other fonts. The Adobe Japanese OTF's that
 *    I have only have one font in the CFF table, so these can be
 *    embeded in the PDF as is.
 * 2. It does not yet subset fonts.
 * 3. It may or may not work on CFF fonts that are not within OTF's.
 *    I didn't try that. In any case, that would probably only be
 *    useful for subsetting CID fonts, not for CFF Type1 fonts (I don't
 *    think there are any available.
 * I plan to extend the class to support these three features at some
 * future time.
 */

package com.lowagie.text.pdf;

import com.lowagie.text.ExceptionConverter;

class CFFFont {

    private static final String operatorNames[] = {
        "version", "Notice", "FullName", "FamilyName",
        "Weight", "FontBBox", "BlueValues", "OtherBlues",
        "FamilyBlues", "FamilyOtherBlues", "StdHW", "StdVW",
        "UNKNOWN_12", "UniqueID", "XUID", "charset",
        "Encoding", "CharStrings", "Private", "Subrs",
        "defaultWidthX", "nominalWidthX", "UNKNOWN_22", "UNKNOWN_23",
        "UNKNOWN_24", "UNKNOWN_25", "UNKNOWN_26", "UNKNOWN_27",
        "UNKNOWN_28", "UNKNOWN_29", "UNKNOWN_30", "UNKNOWN_31",
        "Copyright", "isFixedPitch", "ItalicAngle", "UnderlinePosition",
        "UnderlineThickness", "PaintType", "CharstringType", "FontMatrix",
        "StrokeWidth", "BlueScale", "BlueShift", "BlueFuzz",
        "StemSnapH", "StemSnapV", "ForceBold", "UNKNOWN_12_15",
        "UNKNOWN_12_16", "LanguageGroup", "ExpansionFactor", "initialRandomSeed",
        "SyntheticBase", "PostScript", "BaseFontName", "BaseFontBlend",
        "UNKNOWN_12_24", "UNKNOWN_12_25", "UNKNOWN_12_26", "UNKNOWN_12_27",
        "UNKNOWN_12_28", "UNKNOWN_12_29", "ROS", "CIDFontVersion",
        "CIDFontRevision", "CIDFontType", "CIDCount", "UIDBase",
        "FDArray", "FDSelect", "FontName"
    };

    static final String standardStrings[] = {
        // Automatically generated from Appendix A of the CFF specification; do
        // not edit. Size should be 391.
        ".notdef", "space", "exclam", "quotedbl", "numbersign", "dollar",
        "percent", "ampersand", "quoteright", "parenleft", "parenright",
        "asterisk", "plus", "comma", "hyphen", "period", "slash", "zero", "one",
        "two", "three", "four", "five", "six", "seven", "eight", "nine", "colon",
        "semicolon", "less", "equal", "greater", "question", "at", "A", "B", "C",
        "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R",
        "S", "T", "U", "V", "W", "X", "Y", "Z", "bracketleft", "backslash",
        "bracketright", "asciicircum", "underscore", "quoteleft", "a", "b", "c",
        "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r",
        "s", "t", "u", "v", "w", "x", "y", "z", "braceleft", "bar", "braceright",
        "asciitilde", "exclamdown", "cent", "sterling", "fraction", "yen",
        "florin", "section", "currency", "quotesingle", "quotedblleft",
        "guillemotleft", "guilsinglleft", "guilsinglright", "fi", "fl", "endash",
        "dagger", "daggerdbl", "periodcentered", "paragraph", "bullet",
        "quotesinglbase", "quotedblbase", "quotedblright", "guillemotright",
        "ellipsis", "perthousand", "questiondown", "grave", "acute", "circumflex",
        "tilde", "macron", "breve", "dotaccent", "dieresis", "ring", "cedilla",
        "hungarumlaut", "ogonek", "caron", "emdash", "AE", "ordfeminine", "Lslash",
        "Oslash", "OE", "ordmasculine", "ae", "dotlessi", "lslash", "oslash", "oe",
        "germandbls", "onesuperior", "logicalnot", "mu", "trademark", "Eth",
        "onehalf", "plusminus", "Thorn", "onequarter", "divide", "brokenbar",
        "degree", "thorn", "threequarters", "twosuperior", "registered", "minus",
        "eth", "multiply", "threesuperior", "copyright", "Aacute", "Acircumflex",
        "Adieresis", "Agrave", "Aring", "Atilde", "Ccedilla", "Eacute",
        "Ecircumflex", "Edieresis", "Egrave", "Iacute", "Icircumflex", "Idieresis",
        "Igrave", "Ntilde", "Oacute", "Ocircumflex", "Odieresis", "Ograve",
        "Otilde", "Scaron", "Uacute", "Ucircumflex", "Udieresis", "Ugrave",
        "Yacute", "Ydieresis", "Zcaron", "aacute", "acircumflex", "adieresis",
        "agrave", "aring", "atilde", "ccedilla", "eacute", "ecircumflex",
        "edieresis", "egrave", "iacute", "icircumflex", "idieresis", "igrave",
        "ntilde", "oacute", "ocircumflex", "odieresis", "ograve", "otilde",
        "scaron", "uacute", "ucircumflex", "udieresis", "ugrave", "yacute",
        "ydieresis", "zcaron", "exclamsmall", "Hungarumlautsmall",
        "dollaroldstyle", "dollarsuperior", "ampersandsmall", "Acutesmall",
        "parenleftsuperior", "parenrightsuperior", "twodotenleader",
        "onedotenleader", "zerooldstyle", "oneoldstyle", "twooldstyle",
        "threeoldstyle", "fouroldstyle", "fiveoldstyle", "sixoldstyle",
        "sevenoldstyle", "eightoldstyle", "nineoldstyle", "commasuperior",
        "threequartersemdash", "periodsuperior", "questionsmall", "asuperior",
        "bsuperior", "centsuperior", "dsuperior", "esuperior", "isuperior",
        "lsuperior", "msuperior", "nsuperior", "osuperior", "rsuperior",
        "ssuperior", "tsuperior", "ff", "ffi", "ffl", "parenleftinferior",
        "parenrightinferior", "Circumflexsmall", "hyphensuperior", "Gravesmall",
        "Asmall", "Bsmall", "Csmall", "Dsmall", "Esmall", "Fsmall", "Gsmall",
        "Hsmall", "Ismall", "Jsmall", "Ksmall", "Lsmall", "Msmall", "Nsmall",
        "Osmall", "Psmall", "Qsmall", "Rsmall", "Ssmall", "Tsmall", "Usmall",
        "Vsmall", "Wsmall", "Xsmall", "Ysmall", "Zsmall", "colonmonetary",
        "onefitted", "rupiah", "Tildesmall", "exclamdownsmall", "centoldstyle",
        "Lslashsmall", "Scaronsmall", "Zcaronsmall", "Dieresissmall", "Brevesmall",
        "Caronsmall", "Dotaccentsmall", "Macronsmall", "figuredash",
        "hypheninferior", "Ogoneksmall", "Ringsmall", "Cedillasmall",
        "questiondownsmall", "oneeighth", "threeeighths", "fiveeighths",
        "seveneighths", "onethird", "twothirds", "zerosuperior", "foursuperior",
        "fivesuperior", "sixsuperior", "sevensuperior", "eightsuperior",
        "ninesuperior", "zeroinferior", "oneinferior", "twoinferior",
        "threeinferior", "fourinferior", "fiveinferior", "sixinferior",
        "seveninferior", "eightinferior", "nineinferior", "centinferior",
        "dollarinferior", "periodinferior", "commainferior", "Agravesmall",
        "Aacutesmall", "Acircumflexsmall", "Atildesmall", "Adieresissmall",
        "Aringsmall", "AEsmall", "Ccedillasmall", "Egravesmall", "Eacutesmall",
        "Ecircumflexsmall", "Edieresissmall", "Igravesmall", "Iacutesmall",
        "Icircumflexsmall", "Idieresissmall", "Ethsmall", "Ntildesmall",
        "Ogravesmall", "Oacutesmall", "Ocircumflexsmall", "Otildesmall",
        "Odieresissmall", "OEsmall", "Oslashsmall", "Ugravesmall", "Uacutesmall",
        "Ucircumflexsmall", "Udieresissmall", "Yacutesmall", "Thornsmall",
        "Ydieresissmall", "001.000", "001.001", "001.002", "001.003", "Black",
        "Bold", "Book", "Light", "Medium", "Regular", "Roman", "Semibold"
    };

    //private String[] strings;
    private String getString(final char sid) {
        if (sid < standardStrings.length) {
			return standardStrings[sid];
		}
        if (sid >= standardStrings.length+this.stringOffsets.length-1) {
			return null;
		}
        final int j = sid - standardStrings.length;
        //java.lang.System.err.println("going for "+j);
        final int p = getPosition();
        seek(this.stringOffsets[j]);
        final StringBuffer s = new StringBuffer();
        for (int k=this.stringOffsets[j]; k<this.stringOffsets[j+1]; k++) {
            s.append(getCard8());
        }
        seek(p);
        return s.toString();
    }

    char getCard8() {
        try {
            final byte i = this.buf.readByte();
            return (char)(i & 0xff);
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    char getCard16() {
        try {
            return this.buf.readChar();
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    private int getOffset(final int offSize) {
        int offset = 0;
        for (int i=0; i<offSize; i++) {
            offset *= 256;
            offset += getCard8();
        }
        return offset;
    }

    void seek(final int offset) {
        try {
            this.buf.seek(offset);
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    private short getShort() {
        try {
            return this.buf.readShort();
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    private int getInt() {
        try {
            return this.buf.readInt();
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    int getPosition() {
        try {
            return this.buf.getFilePointer();
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    // read the offsets in the next index
    // data structure, convert to global
    // offsets, and return them.
    // Sets the nextIndexOffset.
    int[] getIndex(int nextIndexOffset) {
        int count, indexOffSize;

        seek(nextIndexOffset);
        count = getCard16();
        final int[] offsets = new int[count+1];

        if (count==0) {
            offsets[0] = -1;
            nextIndexOffset += 2;
            return offsets;
        }

        indexOffSize = getCard8();

        for (int j=0; j<=count; j++) {
        	//nextIndexOffset = ofset to relative segment
            offsets[j] = nextIndexOffset
			//2-> count in the index header. 1->offset size in index header
            + 2+1
			//offset array size * offset size
            + (count+1)*indexOffSize
			//???zero <-> one base
            - 1
			// read object offset relative to object array base
            + getOffset(indexOffSize);
        }
        //nextIndexOffset = offsets[count];
        return offsets;
    }

    protected String   key;
    protected Object[] args      = new Object[48];
    protected int      arg_count = 0;

    protected void getDictItem() {
        for (int i=0; i<this.arg_count; i++) {
			this.args[i]=null;
		}
        this.arg_count = 0;
        this.key = null;
        boolean gotKey = false;

        while (!gotKey) {
            final char b0 = getCard8();
            if (b0 == 29) {
                final int item = getInt();
                this.args[this.arg_count] = new Integer(item);
                this.arg_count++;
                //System.err.println(item+" ");
                continue;
            }
            if (b0 == 28) {
                final short item = getShort();
                this.args[this.arg_count] = new Integer(item);
                this.arg_count++;
                //System.err.println(item+" ");
                continue;
            }
            if (b0 >= 32 && b0 <= 246) {
                final byte item = (byte) (b0-139);
                this.args[this.arg_count] = new Integer(item);
                this.arg_count++;
                //System.err.println(item+" ");
                continue;
            }
            if (b0 >= 247 && b0 <= 250) {
                final char b1 = getCard8();
                final short item = (short) ((b0-247)*256+b1+108);
                this.args[this.arg_count] = new Integer(item);
                this.arg_count++;
                //System.err.println(item+" ");
                continue;
            }
            if (b0 >= 251 && b0 <= 254) {
                final char b1 = getCard8();
                final short item = (short) (-(b0-251)*256-b1-108);
                this.args[this.arg_count] = new Integer(item);
                this.arg_count++;
                //System.err.println(item+" ");
                continue;
            }
            if (b0 == 30) {
                String item = "";
                boolean done = false;
                char buffer = 0;
                byte avail = 0;
                int  nibble = 0;
                while (!done) {
                    // get a nibble
                    if (avail==0) { buffer = getCard8(); avail=2; }
                    if (avail==1) { nibble = buffer / 16; avail--; }
                    if (avail==2) { nibble = buffer % 16; avail--; }
                    switch (nibble) {
                        case 0xa: item += "." ; break;
                        case 0xb: item += "E" ; break;
                        case 0xc: item += "E-"; break;
                        case 0xe: item += "-" ; break;
                        case 0xf: done=true   ; break;
                        default:
                            if (nibble >= 0 && nibble <= 9) {
								item += String.valueOf(nibble);
							} else {
                                item += "<NIBBLE ERROR: " + nibble + '>';
                                done = true;
                            }
                            break;
                    }
                }
                this.args[this.arg_count] = item;
                this.arg_count++;
                //System.err.println(" real=["+item+"]");
                continue;
            }
            if (b0 <= 21) {
                gotKey=true;
                if (b0 != 12) {
					this.key = operatorNames[b0];
				} else {
					this.key = operatorNames[32 + getCard8()];
				}
                //for (int i=0; i<arg_count; i++)
                //  System.err.print(args[i].toString()+" ");
                //System.err.println(key+" ;");
                continue;
            }
        }
    }

    /** List items for the linked list that builds the new CID font.
     */

    protected static abstract class Item {
        int myOffset = -1;
        /** remember the current offset and increment by item's size in bytes. */
        public void increment(final int[] currentOffset) {
            this.myOffset = currentOffset[0];
        }
        /** Emit the byte stream for this item. */
        public void emit(final byte[] buffer) {}
        /** Fix up cross references to this item (applies only to markers). */
        public void xref() {}
    }

    protected static abstract class OffsetItem extends Item {
        int value;
        /** set the value of an offset item that was initially unknown.
         * It will be fixed up latex by a call to xref on some marker.
         */
        private void set(final int offset) { this.value = offset; }
    }


    /** A range item.
     */

    protected static final class RangeItem extends Item {
        private final int offset, length;
        private final RandomAccessFileOrArray buf;
        RangeItem(final RandomAccessFileOrArray buf, final int offset, final int length) {
            this.offset = offset;
            this.length = length;
            this.buf = buf;
        }
        @Override
		public void increment(final int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += this.length;
        }
        @Override
		public void emit(final byte[] buffer) {
            //System.err.println("range emit offset "+offset+" size="+length);
            try {
                this.buf.seek(this.offset);
                for (int i=this.myOffset; i<this.myOffset+this.length; i++) {
					buffer[i] = this.buf.readByte();
				}
            }
            catch (final Exception e) {
                throw new ExceptionConverter(e);
            }
            //System.err.println("finished range emit");
        }
    }

    /** An index-offset item for the list.
     * The size denotes the required size in the CFF. A positive
     * value means that we need a specific size in bytes (for offset arrays)
     * and a negative value means that this is a dict item that uses a
     * variable-size representation.
     */
    static protected final class IndexOffsetItem extends OffsetItem {
        private final int size;
        IndexOffsetItem(final int size, final int value) {this.size=size; this.value=value;}
        IndexOffsetItem(final int size) {this.size=size; }

        @Override
		public void increment(final int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += this.size;
        }
        @Override
		public void emit(final byte[] buffer) {
            int i=0;
            switch (this.size) {
                case 4:
                    buffer[this.myOffset+i] = (byte) (this.value >>> 24 & 0xff);
                    i++;
                case 3:
                    buffer[this.myOffset+i] = (byte) (this.value >>> 16 & 0xff);
                    i++;
                case 2:
                    buffer[this.myOffset+i] = (byte) (this.value >>>  8 & 0xff);
                    i++;
                case 1:
                    buffer[this.myOffset+i] = (byte) (this.value >>>  0 & 0xff);
                    i++;
            }
            /*
            int mask = 0xff;
            for (int i=size-1; i>=0; i--) {
                buffer[myOffset+i] = (byte) (value & mask);
                mask <<= 8;
            }
             */
        }
    }

    static protected final class IndexBaseItem extends Item {
        public IndexBaseItem() {}
    }

    static protected final class IndexMarkerItem extends Item {
        private final OffsetItem offItem;
        private final IndexBaseItem indexBase;
        IndexMarkerItem(final OffsetItem offItem, final IndexBaseItem indexBase) {
            this.offItem   = offItem;
            this.indexBase = indexBase;
        }
        @Override
		public void xref() {
            //System.err.println("index marker item, base="+indexBase.myOffset+" my="+this.myOffset);
            this.offItem.set(this.myOffset-this.indexBase.myOffset+1);
        }
    }
    /**
     * TODO To change the template for this generated type comment go to
     * Window - Preferences - Java - Code Generation - Code and Comments
     */
    static protected final class SubrMarkerItem extends Item {
        private final OffsetItem offItem;
        private final IndexBaseItem indexBase;
        SubrMarkerItem(final OffsetItem offItem, final IndexBaseItem indexBase) {
            this.offItem   = offItem;
            this.indexBase = indexBase;
        }
        @Override
		public void xref() {
            //System.err.println("index marker item, base="+indexBase.myOffset+" my="+this.myOffset);
            this.offItem.set(this.myOffset-this.indexBase.myOffset);
        }
    }


    /** an unknown offset in a dictionary for the list.
     * We will fix up the offset later; for now, assume it's large.
     */
    static protected final class DictOffsetItem extends OffsetItem {
        private final int size;
        public DictOffsetItem() {this.size=5; }

        @Override
		public void increment(final int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += this.size;
        }
        // this is incomplete!
        @Override
		public void emit(final byte[] buffer) {
            if (this.size==5) {
                buffer[this.myOffset]   = 29;
                buffer[this.myOffset+1] = (byte) (this.value >>> 24 & 0xff);
                buffer[this.myOffset+2] = (byte) (this.value >>> 16 & 0xff);
                buffer[this.myOffset+3] = (byte) (this.value >>>  8 & 0xff);
                buffer[this.myOffset+4] = (byte) (this.value >>>  0 & 0xff);
            }
        }
    }

	/** Card24 item.
     */

    static protected final class UInt24Item extends Item {
        private final int value;
        UInt24Item(final int value) {this.value=value;}

        @Override
		public void increment(final int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += 3;
        }
        // this is incomplete!
        @Override
		public void emit(final byte[] buffer) {
        	buffer[this.myOffset+0] = (byte) (this.value >>> 16 & 0xff);
            buffer[this.myOffset+1] = (byte) (this.value >>> 8 & 0xff);
            buffer[this.myOffset+2] = (byte) (this.value >>> 0 & 0xff);
        }
    }

    /** Card32 item.
     */

    static protected final class UInt32Item extends Item {
        private final int value;
        UInt32Item(final int value) {this.value=value;}

        @Override
		public void increment(final int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += 4;
        }
        // this is incomplete!
        @Override
		public void emit(final byte[] buffer) {
        	buffer[this.myOffset+0] = (byte) (this.value >>> 24 & 0xff);
        	buffer[this.myOffset+1] = (byte) (this.value >>> 16 & 0xff);
            buffer[this.myOffset+2] = (byte) (this.value >>> 8 & 0xff);
            buffer[this.myOffset+3] = (byte) (this.value >>> 0 & 0xff);
        }
    }

    /** A SID or Card16 item.
     */

    static protected final class UInt16Item extends Item {
        private final char value;
        UInt16Item(final char value) {this.value=value;}

        @Override
		public void increment(final int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += 2;
        }
        // this is incomplete!
        @Override
		public void emit(final byte[] buffer) {
            buffer[this.myOffset+0] = (byte) (this.value >>> 8 & 0xff);
            buffer[this.myOffset+1] = (byte) (this.value >>> 0 & 0xff);
        }
    }

    /** A Card8 item.
     */

    static protected final class UInt8Item extends Item {
        private final char value;
        UInt8Item(final char value) {this.value=value;}

        @Override
		public void increment(final int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += 1;
        }
        // this is incomplete!
        @Override
		public void emit(final byte[] buffer) {
            buffer[this.myOffset+0] = (byte) (this.value >>> 0 & 0xff);
        }
    }

    static protected final class StringItem extends Item {
        private final String s;
        StringItem(final String s) {this.s=s;}

        @Override
		public void increment(final int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += this.s.length();
        }
        @Override
		public void emit(final byte[] buffer) {
            for (int i=0; i<this.s.length(); i++) {
				buffer[this.myOffset+i] = (byte) (this.s.charAt(i) & 0xff);
			}
        }
    }


    /** A dictionary number on the list.
     * This implementation is inefficient: it doesn't use the variable-length
     * representation.
     */

    static protected final class DictNumberItem extends Item {
        private final int value;
        private final int size = 5;
        DictNumberItem(final int value) {this.value=value;}
        @Override
		public void increment(final int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += this.size;
        }
        // this is incomplete!
        @Override
		public void emit(final byte[] buffer) {
            if (this.size==5) {
                buffer[this.myOffset]   = 29;
                buffer[this.myOffset+1] = (byte) (this.value >>> 24 & 0xff);
                buffer[this.myOffset+2] = (byte) (this.value >>> 16 & 0xff);
                buffer[this.myOffset+3] = (byte) (this.value >>>  8 & 0xff);
                buffer[this.myOffset+4] = (byte) (this.value >>>  0 & 0xff);
            }
        }
    }

    /** An offset-marker item for the list.
     * It is used to mark an offset and to set the offset list item.
     */

    static protected final class MarkerItem extends Item {
        private final OffsetItem p;
        MarkerItem(final OffsetItem pointerToMarker) {this.p=pointerToMarker;}
        @Override
		public void xref() {
            this.p.set(this.myOffset);
        }
    }

    /** a utility that creates a range item for an entire index
     *
     * @param indexOffset where the index is
     * @return a range item representing the entire index
     */

    protected RangeItem getEntireIndexRange(final int indexOffset) {
        seek(indexOffset);
        final int count = getCard16();
        if (count==0) {
            return new RangeItem(this.buf,indexOffset,2);
        } else {
            final int indexOffSize = getCard8();
            seek(indexOffset+2+1+count*indexOffSize);
            final int size = getOffset(indexOffSize)-1;
            return new RangeItem(this.buf,indexOffset,
            2+1+(count+1)*indexOffSize+size);
        }
    }










    public String[] getNames() {
        final String[] names = new String[ this.fonts.length ];
        for (int i=0; i<this.fonts.length; i++) {
			names[i] = this.fonts[i].name;
		}
        return names;
    }
    /**
     * A random Access File or an array
     */
    protected RandomAccessFileOrArray buf;
    private final int offSize;

    private final int nameIndexOffset;
    private final int topdictIndexOffset;
    protected int stringIndexOffset;
    protected int gsubrIndexOffset;
    private final int[] nameOffsets;
    protected int[] topdictOffsets;
    protected int[] stringOffsets;
    protected int[] gsubrOffsets;

    /**
     * TODO Changed from private to protected by Ygal&Oren
     */
    final class Font {
        String    name;

        boolean   isCID = false;
        int       privateOffset     = -1; // only if not CID
        int       privateLength     = -1; // only if not CID
        int       privateSubrs      = -1;
        int       charstringsOffset = -1;
        private int       encodingOffset    = -1;
        int       charsetOffset     = -1;
        int       fdarrayOffset     = -1; // only if CID
        int       fdselectOffset    = -1; // only if CID
        int[]     fdprivateOffsets;
        int[]     fdprivateLengths;


        // Added by Oren & Ygal
        int nglyphs;
        int nstrings;
        int CharsetLength;
        int[]    charstringsOffsets;

        int[] 	FDSelect;
        int FDSelectLength;
        int FDSelectFormat;
        int 		CharstringType = 2;
        int FDArrayCount;
        int FDArrayOffsize;
        int[] FDArrayOffsets;
        int[] PrivateSubrsOffset;
        int[][] PrivateSubrsOffsetsArray;
        int[]       SubrsOffsets;
    }
    // Changed from private to protected by Ygal&Oren
    protected Font[] fonts;

    public CFFFont(final RandomAccessFileOrArray inputbuffer) {

        //System.err.println("CFF: nStdString = "+standardStrings.length);
        this.buf = inputbuffer;
        seek(0);

        int major, minor;
        major = getCard8();
        minor = getCard8();

        //System.err.println("CFF Major-Minor = "+major+"-"+minor);

        final int hdrSize = getCard8();

        this.offSize = getCard8();

        //System.err.println("offSize = "+offSize);

        //int count, indexOffSize, indexOffset, nextOffset;

        this.nameIndexOffset    = hdrSize;
        this.nameOffsets        = getIndex(this.nameIndexOffset);
        this.topdictIndexOffset = this.nameOffsets[this.nameOffsets.length-1];
        this.topdictOffsets     = getIndex(this.topdictIndexOffset);
        this.stringIndexOffset  = this.topdictOffsets[this.topdictOffsets.length-1];
        this.stringOffsets      = getIndex(this.stringIndexOffset);
        this.gsubrIndexOffset   = this.stringOffsets[this.stringOffsets.length-1];
        this.gsubrOffsets       = getIndex(this.gsubrIndexOffset);

        this.fonts = new Font[this.nameOffsets.length-1];

        // now get the name index

        /*
        names             = new String[nfonts];
        privateOffset     = new int[nfonts];
        charsetOffset     = new int[nfonts];
        encodingOffset    = new int[nfonts];
        charstringsOffset = new int[nfonts];
        fdarrayOffset     = new int[nfonts];
        fdselectOffset    = new int[nfonts];
         */

        for (int j=0; j<this.nameOffsets.length-1; j++) {
            this.fonts[j] = new Font();
            seek(this.nameOffsets[j]);
            this.fonts[j].name = "";
            for (int k=this.nameOffsets[j]; k<this.nameOffsets[j+1]; k++) {
                this.fonts[j].name += getCard8();
            }
            //System.err.println("name["+j+"]=<"+fonts[j].name+">");
        }

        // string index

        //strings = new String[stringOffsets.length-1];
        /*
        System.err.println("std strings = "+standardStrings.length);
        System.err.println("fnt strings = "+(stringOffsets.length-1));
        for (char j=0; j<standardStrings.length+(stringOffsets.length-1); j++) {
            //seek(stringOffsets[j]);
            //strings[j] = "";
            //for (int k=stringOffsets[j]; k<stringOffsets[j+1]; k++) {
            //	strings[j] += (char)getCard8();
            //}
            System.err.println("j="+(int)j+" <? "+(standardStrings.length+(stringOffsets.length-1)));
            System.err.println("strings["+(int)j+"]=<"+getString(j)+">");
        }
         */

        // top dict

        for (int j=0; j<this.topdictOffsets.length-1; j++) {
            seek(this.topdictOffsets[j]);
            while (getPosition() < this.topdictOffsets[j+1]) {
                getDictItem();
                if (this.key=="ROS") {
					this.fonts[j].isCID = true;
				} else if (this.key=="Private") {
                    this.fonts[j].privateLength  = ((Integer)this.args[0]).intValue();
                    this.fonts[j].privateOffset  = ((Integer)this.args[1]).intValue();
                }
                else if (this.key=="charset"){
                    this.fonts[j].charsetOffset = ((Integer)this.args[0]).intValue();

                }
                else if (this.key=="Encoding"){
                    this.fonts[j].encodingOffset = ((Integer)this.args[0]).intValue();
                    ReadEncoding(this.fonts[j].encodingOffset);
                }
                else if (this.key=="CharStrings") {
                    this.fonts[j].charstringsOffset = ((Integer)this.args[0]).intValue();
                    //System.err.println("charstrings "+fonts[j].charstringsOffset);
                    // Added by Oren & Ygal
                    final int p = getPosition();
                    this.fonts[j].charstringsOffsets = getIndex(this.fonts[j].charstringsOffset);
                    seek(p);
                } else if (this.key=="FDArray") {
					this.fonts[j].fdarrayOffset = ((Integer)this.args[0]).intValue();
				} else if (this.key=="FDSelect") {
					this.fonts[j].fdselectOffset = ((Integer)this.args[0]).intValue();
				} else if (this.key=="CharstringType") {
					this.fonts[j].CharstringType = ((Integer)this.args[0]).intValue();
				}
            }

            // private dict
            if (this.fonts[j].privateOffset >= 0) {
                //System.err.println("PRIVATE::");
                seek(this.fonts[j].privateOffset);
                while (getPosition() < this.fonts[j].privateOffset+this.fonts[j].privateLength) {
                    getDictItem();
                    if (this.key=="Subrs") {
						//Add the private offset to the lsubrs since the offset is
                    	// relative to the beginning of the PrivateDict
                        this.fonts[j].privateSubrs = ((Integer)this.args[0]).intValue()+this.fonts[j].privateOffset;
					}
                }
            }

            // fdarray index
            if (this.fonts[j].fdarrayOffset >= 0) {
                final int[] fdarrayOffsets = getIndex(this.fonts[j].fdarrayOffset);

                this.fonts[j].fdprivateOffsets = new int[fdarrayOffsets.length-1];
                this.fonts[j].fdprivateLengths = new int[fdarrayOffsets.length-1];

                //System.err.println("FD Font::");

                for (int k=0; k<fdarrayOffsets.length-1; k++) {
                    seek(fdarrayOffsets[k]);
                    while (getPosition() < fdarrayOffsets[k+1]) {
						getDictItem();
					}
                    if (this.key=="Private") {
                        this.fonts[j].fdprivateLengths[k]  = ((Integer)this.args[0]).intValue();
                        this.fonts[j].fdprivateOffsets[k]  = ((Integer)this.args[1]).intValue();
                    }

                }
            }
        }
        //System.err.println("CFF: done");
    }

    // ADDED BY Oren & Ygal

    private void ReadEncoding(final int nextIndexOffset){
    	int format;
    	seek(nextIndexOffset);
    	format = getCard8();
    }
}