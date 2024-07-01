/*
 * Copyright (c) 1998, 2012, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package es.gob.afirma.core.signers.der;

import java.io.IOException;
import java.util.ArrayList;

/**
 * A package private utility class to convert indefinite length DER
 * encoded byte arrays to definite length DER encoded byte arrays.
 *
 * This assumes that the basic data structure is "tag, length, value"
 * triplet. In the case where the length is "indefinite", terminating
 * end-of-contents bytes are expected.
 *
 * @author Hemma Prafullchandra
 */
class DerIndefLenConverter {

    private static final int TAG_MASK            = 0x1f; // bits 5-1
    private static final int FORM_MASK           = 0x20; // bits 6
    private static final int CLASS_MASK          = 0xC0; // bits 8 and 7

    private static final int LEN_LONG            = 0x80; // bit 8 set
    private static final int LEN_MASK            = 0x7f; // bits 7 - 1
    private static final int SKIP_EOC_BYTES      = 2;

    private byte[] data, newData;
    private int newDataPos, dataPos, dataSize, index;
    private int unresolved = 0;

    private final ArrayList<Object> ndefsList = new ArrayList<>();

    private int numOfTotalLenBytes = 0;

    private boolean isEOC(final int tag) {
        return (tag & TAG_MASK) == 0x00 &&  // EOC
                (tag & FORM_MASK) == 0x00 && // primitive
                (tag & CLASS_MASK) == 0x00; // universal
    }

    // if bit 8 is set then it implies either indefinite length or long form
    static boolean isLongForm(final int lengthByte) {
        return (lengthByte & LEN_LONG) == LEN_LONG;
    }

    /*
     * Default package private constructor
     */
    DerIndefLenConverter() { }

    /**
     * Checks whether the given length byte is of the form
     * <em>Indefinite</em>.
     *
     * @param lengthByte the length byte from a DER encoded
     *        object.
     * @return true if the byte is of Indefinite form otherwise
     *         returns false.
     */
    static boolean isIndefinite(final int lengthByte) {
        return isLongForm(lengthByte) && (lengthByte & LEN_MASK) == 0;
    }

    /**
     * Parse the tag and if it is an end-of-contents tag then
     * add the current position to the <code>eocList</code> vector.
     */
    private void parseTag() throws IOException {
        if (this.dataPos == this.dataSize) {
			return;
		}
        try {
            if (isEOC(this.data[this.dataPos]) && this.data[this.dataPos + 1] == 0) {
                int numOfEncapsulatedLenBytes = 0;
                Object elem = null;
                int index;
                for (index = this.ndefsList.size()-1; index >= 0; index--) {
                    // Determine the first element in the vector that does not
                    // have a matching EOC
                    elem = this.ndefsList.get(index);
                    if (elem instanceof Integer) {
                        break;
                    } else {
                        numOfEncapsulatedLenBytes += ((byte[])elem).length - 3;
                    }
                }
                if (index < 0) {
                    throw new IOException("EOC does not have matching " +
                                          "indefinite-length tag");
                }
                final int sectionLen = this.dataPos - ((Integer)elem).intValue() +
                                 numOfEncapsulatedLenBytes;
                final byte[] sectionLenBytes = getLengthBytes(sectionLen);
                this.ndefsList.set(index, sectionLenBytes);
                this.unresolved--;

                // Add the number of bytes required to represent this section
                // to the total number of length bytes,
                // and subtract the indefinite-length tag (1 byte) and
                // EOC bytes (2 bytes) for this section
                this.numOfTotalLenBytes += sectionLenBytes.length - 3;
            }
            this.dataPos++;
        } catch (final IndexOutOfBoundsException iobe) {
            throw new IOException(iobe);
        }
    }

    /**
     * Write the tag and if it is an end-of-contents tag
     * then skip the tag and its 1 byte length of zero.
     */
    private void writeTag() {
        if (this.dataPos == this.dataSize) {
			return;
		}
        final int tag = this.data[this.dataPos++];
        if (isEOC(tag) && this.data[this.dataPos] == 0) {
            this.dataPos++;  // skip length
            writeTag();
        } else {
			this.newData[this.newDataPos++] = (byte)tag;
		}
    }

    /**
     * Parse the length and if it is an indefinite length then add
     * the current position to the <code>ndefsList</code> vector.
     */
    private int parseLength() throws IOException {
        int curLen = 0;
        if (this.dataPos == this.dataSize) {
			return curLen;
		}
        int lenByte = this.data[this.dataPos++] & 0xff;
        if (isIndefinite(lenByte)) {
            this.ndefsList.add(new Integer(this.dataPos));
            this.unresolved++;
            return curLen;
        }
        if (isLongForm(lenByte)) {
            lenByte &= LEN_MASK;
            if (lenByte > 4) {
                throw new IOException("Too much data");
            }
            if (this.dataSize - this.dataPos < lenByte + 1) {
                throw new IOException("Too little data");
            }
            for (int i = 0; i < lenByte; i++) {
                curLen = (curLen << 8) + (this.data[this.dataPos++] & 0xff);
            }
            if (curLen < 0) {
                throw new IOException("Invalid length bytes");
            }
        } else {
           curLen = lenByte & LEN_MASK;
        }
        return curLen;
    }

    /**
     * Write the length and if it is an indefinite length
     * then calculate the definite length from the positions
     * of the indefinite length and its matching EOC terminator.
     * Then, write the value.
     */
    private void writeLengthAndValue() throws IOException {
        if (this.dataPos == this.dataSize) {
			return;
		}
        int curLen = 0;
        int lenByte = this.data[this.dataPos++] & 0xff;
        if (isIndefinite(lenByte)) {
            final byte[] lenBytes = (byte[])this.ndefsList.get(this.index++);
            System.arraycopy(lenBytes, 0, this.newData, this.newDataPos,
                             lenBytes.length);
            this.newDataPos += lenBytes.length;
            return;
        }
        if (isLongForm(lenByte)) {
            lenByte &= LEN_MASK;
            for (int i = 0; i < lenByte; i++) {
                curLen = (curLen << 8) + (this.data[this.dataPos++] & 0xff);
            }
            if (curLen < 0) {
                throw new IOException("Invalid length bytes");
            }
        } else {
            curLen = lenByte & LEN_MASK;
        }
        writeLength(curLen);
        writeValue(curLen);
    }

    private void writeLength(final int curLen) {
        if (curLen < 128) {
            this.newData[this.newDataPos++] = (byte)curLen;

        } else if (curLen < 1 << 8) {
            this.newData[this.newDataPos++] = (byte)0x81;
            this.newData[this.newDataPos++] = (byte)curLen;

        } else if (curLen < 1 << 16) {
            this.newData[this.newDataPos++] = (byte)0x82;
            this.newData[this.newDataPos++] = (byte)(curLen >> 8);
            this.newData[this.newDataPos++] = (byte)curLen;

        } else if (curLen < 1 << 24) {
            this.newData[this.newDataPos++] = (byte)0x83;
            this.newData[this.newDataPos++] = (byte)(curLen >> 16);
            this.newData[this.newDataPos++] = (byte)(curLen >> 8);
            this.newData[this.newDataPos++] = (byte)curLen;

        } else {
            this.newData[this.newDataPos++] = (byte)0x84;
            this.newData[this.newDataPos++] = (byte)(curLen >> 24);
            this.newData[this.newDataPos++] = (byte)(curLen >> 16);
            this.newData[this.newDataPos++] = (byte)(curLen >> 8);
            this.newData[this.newDataPos++] = (byte)curLen;
        }
    }

    private byte[] getLengthBytes(final int curLen) {
        byte[] lenBytes;
        int index = 0;

        if (curLen < 128) {
            lenBytes = new byte[1];
            lenBytes[index++] = (byte)curLen;

        } else if (curLen < 1 << 8) {
            lenBytes = new byte[2];
            lenBytes[index++] = (byte)0x81;
            lenBytes[index++] = (byte)curLen;

        } else if (curLen < 1 << 16) {
            lenBytes = new byte[3];
            lenBytes[index++] = (byte)0x82;
            lenBytes[index++] = (byte)(curLen >> 8);
            lenBytes[index++] = (byte)curLen;

        } else if (curLen < 1 << 24) {
            lenBytes = new byte[4];
            lenBytes[index++] = (byte)0x83;
            lenBytes[index++] = (byte)(curLen >> 16);
            lenBytes[index++] = (byte)(curLen >> 8);
            lenBytes[index++] = (byte)curLen;

        } else {
            lenBytes = new byte[5];
            lenBytes[index++] = (byte)0x84;
            lenBytes[index++] = (byte)(curLen >> 24);
            lenBytes[index++] = (byte)(curLen >> 16);
            lenBytes[index++] = (byte)(curLen >> 8);
            lenBytes[index++] = (byte)curLen;
        }

        return lenBytes;
    }

    // Returns the number of bytes needed to represent the given length
    // in ASN.1 notation
    private int getNumOfLenBytes(final int len) {
        int numOfLenBytes = 0;

        if (len < 128) {
            numOfLenBytes = 1;
        } else if (len < 1 << 8) {
            numOfLenBytes = 2;
        } else if (len < 1 << 16) {
            numOfLenBytes = 3;
        } else if (len < 1 << 24) {
            numOfLenBytes = 4;
        } else {
            numOfLenBytes = 5;
        }
        return numOfLenBytes;
    }

    /**
     * Parse the value;
     */
    private void parseValue(final int curLen) {
        this.dataPos += curLen;
    }

    /**
     * Write the value;
     */
    private void writeValue(final int curLen) {
        for (int i=0; i < curLen; i++) {
			this.newData[this.newDataPos++] = this.data[this.dataPos++];
		}
    }

    /**
     * Converts a indefinite length DER encoded byte array to
     * a definte length DER encoding.
     *
     * @param indefData the byte array holding the indefinite
     *        length encoding.
     * @return the byte array containing the definite length
     *         DER encoding.
     * @exception IOException on parsing or re-writing errors.
     */
    byte[] convert(final byte[] indefData) throws IOException {
        this.data = indefData;
        this.dataPos=0; this.index=0;
        this.dataSize = this.data.length;
        int len=0;
        int unused = 0;

        // parse and set up the vectors of all the indefinite-lengths
        while (this.dataPos < this.dataSize) {
            parseTag();
            len = parseLength();
            parseValue(len);
            if (this.unresolved == 0) {
                unused = this.dataSize - this.dataPos;
                this.dataSize = this.dataPos;
                break;
            }
        }

        if (this.unresolved != 0) {
            throw new IOException("not all indef len BER resolved");
        }

        this.newData = new byte[this.dataSize + this.numOfTotalLenBytes + unused];
        this.dataPos=0; this.newDataPos=0; this.index=0;

        // write out the new byte array replacing all the indefinite-lengths
        // and EOCs
        while (this.dataPos < this.dataSize) {
           writeTag();
           writeLengthAndValue();
        }
        System.arraycopy(indefData, this.dataSize,
                         this.newData, this.dataSize + this.numOfTotalLenBytes, unused);

        return this.newData;
    }
}
