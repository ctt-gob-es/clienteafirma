/*
 * Copyright (c) 1997, 2006, Oracle and/or its affiliates. All rights reserved.
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

import java.io.ByteArrayOutputStream;
import java.util.Arrays;

/**
 * A packed array of booleans.
 *
 * @author Joshua Bloch
 * @author Douglas Hoover
 */

public class BitArray {

    private final byte[] repn;
    private final int length;

    private static final int BITS_PER_UNIT = 8;

    private static int subscript(final int idx) {
        return idx / BITS_PER_UNIT;
    }

    private static int position(final int idx) { // bits big-endian in each unit
        return 1 << BITS_PER_UNIT - 1 - idx % BITS_PER_UNIT;
    }

    /**
     * Creates a BitArray of the specified size, initialized to zeros.
     * @param length Array length.
     * @throws IllegalArgumentException When incorrect length is be used.
     */
    public BitArray(final int length) throws IllegalArgumentException {
        if (length < 0) {
            throw new IllegalArgumentException("Negative length for BitArray");
        }

        this.length = length;

        this.repn = new byte[(length + BITS_PER_UNIT - 1)/BITS_PER_UNIT];
    }


    /**
     * Creates a BitArray of the specified size, initialized from the
     * specified byte array.  The most significant bit of a[0] gets
     * index zero in the BitArray.  The array a must be large enough
     * to specify a value for every bit in the BitArray.  In other words,
     * 8*a.length &lt;= length.
     * @param length Array length.
     * @param a Initial content.
     * @throws IllegalArgumentException When incorrect length is be used.
     */
    public BitArray(final int length, final byte[] a) throws IllegalArgumentException {

        if (length < 0) {
            throw new IllegalArgumentException("Negative length for BitArray");
        }
        if (a.length * BITS_PER_UNIT < length) {
            throw new IllegalArgumentException("Byte array too short to represent " +
                                               "bit array of given length");
        }

        this.length = length;

        final int repLength = (length + BITS_PER_UNIT - 1)/BITS_PER_UNIT;
        final int unusedBits = repLength*BITS_PER_UNIT - length;
        final byte bitMask = (byte) (0xFF << unusedBits);

        /*
         normalize the representation:
          1. discard extra bytes
          2. zero out extra bits in the last byte
         */
        this.repn = new byte[repLength];
        System.arraycopy(a, 0, this.repn, 0, repLength);
        if (repLength > 0) {
            this.repn[repLength - 1] &= bitMask;
        }
    }

    /**
     * Create a BitArray whose bits are those of the given array
     * of Booleans.
     * @param bits Content.
     */
    public BitArray(final boolean[] bits) {
        this.length = bits.length;
        this.repn = new byte[(this.length + 7)/8];

        for (int i=0; i < this.length; i++) {
            set(i, bits[i]);
        }
    }


    /**
     *  Copy constructor (for cloning).
     */
    private BitArray(final BitArray ba) {
        this.length = ba.length;
        this.repn = ba.repn.clone();
    }

    /**
     *  Returns the indexed bit in this BitArray.
     *  @param index Array index.
     *  @return bit in this BitArray.
     *  @throws ArrayIndexOutOfBoundsException When Array index is out of bounds.
     */
    public boolean get(final int index) throws ArrayIndexOutOfBoundsException {
        if (index < 0 || index >= this.length) {
            throw new ArrayIndexOutOfBoundsException(Integer.toString(index));
        }

        return (this.repn[subscript(index)] & position(index)) != 0;
    }

    /**
     *  Sets the indexed bit in this BitArray.
     *  @param index Array index.
     *  @param value Value.
     */
    public void set(final int index, final boolean value)
    throws ArrayIndexOutOfBoundsException {
        if (index < 0 || index >= this.length) {
            throw new ArrayIndexOutOfBoundsException(Integer.toString(index));
        }
        final int idx = subscript(index);
        final int bit = position(index);

        if (value) {
            this.repn[idx] |= bit;
        } else {
            this.repn[idx] &= ~bit;
        }
    }

    /**
     * Returns the length of this BitArray.
     * @return Length.
     */
    public int length() {
        return this.length;
    }

    /**
     * Returns a Byte array containing the contents of this BitArray.
     * The bit stored at index zero in this BitArray will be copied
     * into the most significant bit of the zeroth element of the
     * returned byte array.  The last byte of the returned byte array
     * will be contain zeros in any bits that do not have corresponding
     * bits in the BitArray.  (This matters only if the BitArray's size
     * is not a multiple of 8.)
     * @return Content of this BitArray.
     */
    public byte[] toByteArray() {
        return this.repn.clone();
    }

    @Override
	public boolean equals(final Object obj) {
        if (obj == this) {
			return true;
		}
        if (obj == null || !(obj instanceof BitArray)) {
			return false;
		}

        final BitArray ba = (BitArray) obj;

        if (ba.length != this.length) {
			return false;
		}

        for (int i = 0; i < this.repn.length; i += 1) {
            if (this.repn[i] != ba.repn[i]) {
				return false;
			}
        }
        return true;
    }

    /**
     * Return a boolean array with the same bit values a this BitArray.
     * @return Content of this BitArray as booleans.
     */
    public boolean[] toBooleanArray() {
        final boolean[] bits = new boolean[this.length];

        for (int i=0; i < this.length; i++) {
            bits[i] = get(i);
        }
        return bits;
    }

    /**
     * Returns a hash code value for this bit array.
     *
     * @return  a hash code value for this bit array.
     */
    @Override
	public int hashCode() {
        int hashCode = 0;

        for (int i = 0; i < this.repn.length; i++) {
			hashCode = 31*hashCode + this.repn[i];
		}

        return hashCode ^ this.length;
    }


    @Override
	public Object clone() {
        return new BitArray(this);
    }


    private static final byte[][] NYBBLE = {
        { (byte)'0',(byte)'0',(byte)'0',(byte)'0'},
        { (byte)'0',(byte)'0',(byte)'0',(byte)'1'},
        { (byte)'0',(byte)'0',(byte)'1',(byte)'0'},
        { (byte)'0',(byte)'0',(byte)'1',(byte)'1'},
        { (byte)'0',(byte)'1',(byte)'0',(byte)'0'},
        { (byte)'0',(byte)'1',(byte)'0',(byte)'1'},
        { (byte)'0',(byte)'1',(byte)'1',(byte)'0'},
        { (byte)'0',(byte)'1',(byte)'1',(byte)'1'},
        { (byte)'1',(byte)'0',(byte)'0',(byte)'0'},
        { (byte)'1',(byte)'0',(byte)'0',(byte)'1'},
        { (byte)'1',(byte)'0',(byte)'1',(byte)'0'},
        { (byte)'1',(byte)'0',(byte)'1',(byte)'1'},
        { (byte)'1',(byte)'1',(byte)'0',(byte)'0'},
        { (byte)'1',(byte)'1',(byte)'0',(byte)'1'},
        { (byte)'1',(byte)'1',(byte)'1',(byte)'0'},
        { (byte)'1',(byte)'1',(byte)'1',(byte)'1'}
    };

    private static final int BYTES_PER_LINE = 8;

    /**
     *  Returns a string representation of this BitArray.
     */
    @Override
	public String toString() {
        final ByteArrayOutputStream out = new ByteArrayOutputStream();

        for (int i = 0; i < this.repn.length - 1; i++) {
            out.write(NYBBLE[this.repn[i] >> 4 & 0x0F], 0, 4);
            out.write(NYBBLE[this.repn[i] & 0x0F], 0, 4);

            if (i % BYTES_PER_LINE == BYTES_PER_LINE - 1) {
                out.write('\n');
            } else {
                out.write(' ');
            }
        }

        // in last byte of repn, use only the valid bits
        for (int i = BITS_PER_UNIT * (this.repn.length - 1); i < this.length; i++) {
            out.write(get(i) ? '1' : '0');
        }

        return new String(out.toByteArray());

    }

    public BitArray truncate() {
        for (int i=this.length-1; i>=0; i--) {
            if (get(i)) {
                return new BitArray(i+1, Arrays.copyOf(this.repn, (i + BITS_PER_UNIT)/BITS_PER_UNIT));
            }
        }
        return new BitArray(1);
    }

}
