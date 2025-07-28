/*
 * Copyright (c) 1996, 2017, Oracle and/or its affiliates. All rights reserved.
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

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.Vector;

/**
 * A DER input stream, used for parsing ASN.1 DER-encoded data such as
 * that found in X.509 certificates.  DER is a subset of BER/1, which has
 * the advantage that it allows only a single encoding of primitive data.
 * (High level data such as dates still support many encodings.)  That is,
 * it uses the "Definite" Encoding Rules (DER) not the "Basic" ones (BER).
 *
 * <P>Note that, like BER/1, DER streams are streams of explicitly
 * tagged data values.  Accordingly, this programming interface does
 * not expose any variant of the java.io.InputStream interface, since
 * that kind of input stream holds untagged data values and using that
 * I/O model could prevent correct parsing of the DER data.
 *
 * <P>At this time, this class supports only a subset of the types of DER
 * data encodings which are defined.  That subset is sufficient for parsing
 * most X.509 certificates.
 *
 *
 * @author David Brownell
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 */

public class DerInputStream {

    /*
     * This version only supports fully buffered DER.  This is easy to
     * work with, though if large objects are manipulated DER becomes
     * awkward to deal with.  That's where BER is useful, since BER
     * handles streaming data relatively well.
     */
    DerInputBuffer      buffer;

    /** The DER tag of the value; one of the tag_ constants. */
    public byte         tag;

    /**
     * Create a DER input stream from a data buffer.  The buffer is not
     * copied, it is shared.  Accordingly, the buffer should be treated
     * as read-only.
     *
     * @param data the buffer from which to create the string (CONSUMED)
     * @throws IOException When no it possible load data.
     */
    public DerInputStream(final byte[] data) throws IOException {
        init(data, 0, data.length, true);
    }

    /**
     * Create a DER input stream from part of a data buffer with
     * additional arg to control whether DER checks are enforced.
     * The buffer is not copied, it is shared.  Accordingly, the
     * buffer should be treated as read-only.
     *
     * @param data the buffer from which to create the string (CONSUMED)
     * @param offset the first index of <em>data</em> which will
     *          be read as DER input in the new stream
     * @param len how long a chunk of the buffer to use,
     *          starting at "offset"
     * @param allowBER whether to allow constructed indefinite-length
     *          encoding as well as tolerate leading 0s
     * @throws IOException When no it possible load data.
     */
    public DerInputStream(final byte[] data, final int offset, final int len,
        final boolean allowBER) throws IOException {
        init(data, offset, len, allowBER);
    }

    /**
     * Create a DER input stream from part of a data buffer.
     * The buffer is not copied, it is shared.  Accordingly, the
     * buffer should be treated as read-only.
     *
     * @param data the buffer from which to create the string (CONSUMED)
     * @param offset the first index of <em>data</em> which will
     *          be read as DER input in the new stream
     * @param len how long a chunk of the buffer to use,
     *          starting at "offset"
     * @throws IOException When no it possible load data.
     */
    public DerInputStream(final byte[] data, final int offset, final int len) throws IOException {
        init(data, offset, len, true);
    }

    /*
     * private helper routine
     */
    private void init(final byte[] data, final int offset, final int len, final boolean allowBER) throws IOException {
        if (offset+2 > data.length || offset+len > data.length) {
            throw new IOException("Encoding bytes too short");
        }
        // check for indefinite length encoding
        if (DerIndefLenConverter.isIndefinite(data[offset+1])) {
            if (!allowBER) {
                throw new IOException("Indefinite length BER encoding found");
            } else {
                final byte[] inData = new byte[len];
                System.arraycopy(data, offset, inData, 0, len);

                final DerIndefLenConverter derIn = new DerIndefLenConverter();
                this.buffer = new DerInputBuffer(derIn.convert(inData), allowBER);
            }
        } else {
            this.buffer = new DerInputBuffer(data, offset, len, allowBER);
        }
        this.buffer.mark(Integer.MAX_VALUE);
    }

    DerInputStream(final DerInputBuffer buf) {
        this.buffer = buf;
        this.buffer.mark(Integer.MAX_VALUE);
    }

    /**
     * Creates a new DER input stream from part of this input stream.
     *
     * @param len how long a chunk of the current input stream to use,
     *          starting at the current position.
     * @param do_skip true if the existing data in the input stream should
     *          be skipped.  If this value is false, the next data read
     *          on this stream and the newly created stream will be the
     *          same.
     * @return DER input stream.
     * @throws IOException When no it possible load data.
     */
    public DerInputStream subStream(final int len, final boolean do_skip)
    throws IOException {
        final DerInputBuffer newbuf = this.buffer.dup();

        newbuf.truncate(len);
        if (do_skip) {
            this.buffer.skip(len);
        }
        return new DerInputStream(newbuf);
    }

    /**
     * Return what has been written to this DerInputStream
     * as a byte array. Useful for debugging.
     * @return Input stream content.
     */
    public byte[] toByteArray() {
        return this.buffer.toByteArray();
    }

    /*
     * PRIMITIVES -- these are "universal" ASN.1 simple types.
     *
     *  INTEGER, ENUMERATED, BIT STRING, OCTET STRING, NULL
     *  OBJECT IDENTIFIER, SEQUENCE (OF), SET (OF)
     *  UTF8String, PrintableString, T61String, IA5String, UTCTime,
     *  GeneralizedTime, BMPString.
     * Note: UniversalString not supported till encoder is available.
     */

    /**
     * Get an integer from the input stream as an integer.
     *
     * @return the integer held in this DER input stream.
     * @throws IOException When no it possible load data.
     */
    public int getInteger() throws IOException {
        if (this.buffer.read() != DerValue.tag_Integer) {
            throw new IOException("DER input, Integer tag error");
        }
        return this.buffer.getInteger(getLength(this.buffer));
    }

    /**
     * Get a integer from the input stream as a BigInteger object.
     *
     * @return the integer held in this DER input stream.
     * @throws IOException When no it possible load data.
     */
    public BigInteger getBigInteger() throws IOException {
        if (this.buffer.read() != DerValue.tag_Integer) {
            throw new IOException("DER input, Integer tag error");
        }
        return this.buffer.getBigInteger(getLength(this.buffer), false);
    }

    /**
     * Returns an ASN.1 INTEGER value as a positive BigInteger.
     * This is just to deal with implementations that incorrectly encode
     * some values as negative.
     *
     * @return the integer held in this DER value as a BigInteger.
     * @throws IOException When no it possible load data.
     */
    public BigInteger getPositiveBigInteger() throws IOException {
        if (this.buffer.read() != DerValue.tag_Integer) {
            throw new IOException("DER input, Integer tag error");
        }
        return this.buffer.getBigInteger(getLength(this.buffer), true);
    }

    /**
     * Get an enumerated from the input stream.
     *
     * @return the integer held in this DER input stream.
     * @throws IOException When no it possible load data.
     */
    public int getEnumerated() throws IOException {
        if (this.buffer.read() != DerValue.tag_Enumerated) {
            throw new IOException("DER input, Enumerated tag error");
        }
        return this.buffer.getInteger(getLength(this.buffer));
    }

    /**
     * Get a bit string from the input stream. Padded bits (if any)
     * will be stripped off before the bit string is returned.
     * @return a bit string from the input stream.
     * @throws IOException When no it possible load data.
     */
    public byte[] getBitString() throws IOException {
        if (this.buffer.read() != DerValue.tag_BitString) {
			throw new IOException("DER input not an bit string");
		}

        return this.buffer.getBitString(getLength(this.buffer));
    }

    /**
     * Get a bit string from the input stream.  The bit string need
     * not be byte-aligned.
     * @return an BitArray.
     * @throws IOException When no it possible load data.
     */
    public BitArray getUnalignedBitString() throws IOException {
        if (this.buffer.read() != DerValue.tag_BitString) {
			throw new IOException("DER input not a bit string");
		}

        final int length = getLength(this.buffer) - 1;

        /*
         * First byte = number of excess bits in the last octet of the
         * representation.
         */
        final int excessBits = this.buffer.read();
        if (excessBits < 0) {
            throw new IOException("Unused bits of bit string invalid");
        }
        final int validBits = length*8 - excessBits;
        if (validBits < 0) {
            throw new IOException("Valid bits of bit string invalid");
        }

        final byte[] repn = new byte[length];

        if (length != 0 && this.buffer.read(repn) != length) {
            throw new IOException("Short read of DER bit string");
        }

        return new BitArray(validBits, repn);
    }

    /**
     * Returns an ASN.1 OCTET STRING from the input stream.
     * @return an ASN.1 OCTET STRING.
     * @throws IOException When no it possible load data.
     */
    public byte[] getOctetString() throws IOException {
        if (this.buffer.read() != DerValue.tag_OctetString) {
			throw new IOException("DER input not an octet string");
		}

        final int length = getLength(this.buffer);
        final byte[] retval = new byte[length];
        if (length != 0 && this.buffer.read(retval) != length) {
			throw new IOException("Short read of DER octet string");
		}

        return retval;
    }

    /**
     * Returns the asked number of bytes from the input stream.
     * @param val Buffer to store the bytes readed.
     * @throws IOException When no it possible load data.
     */
    public void getBytes(final byte[] val) throws IOException {
        if (val.length != 0 && this.buffer.read(val) != val.length) {
            throw new IOException("Short read of DER octet string");
        }
    }

    /**
     * Reads an encoded null value from the input stream.
     * @throws IOException When no it possible load data.
     */
    public void getNull() throws IOException {
        if (this.buffer.read() != DerValue.tag_Null || this.buffer.read() != 0) {
			throw new IOException("getNull, bad data");
		}
    }

    /**
     * Reads an X.200 style Object Identifier from the stream.
     * @return an X.200 style Object Identifier.
     * @throws IOException When no it possible load data.
     */
    public ObjectIdentifier getOID() throws IOException {
        return new ObjectIdentifier(this);
    }

    /**
     * Return a sequence of encoded entities.  ASN.1 sequences are
     * ordered, and they are often used, like a "struct" in C or C++,
     * to group data values.  They may have optional or context
     * specific values.
     *
     * @param startLen guess about how long the sequence will be
     *          (used to initialize an auto-growing data structure)
     * @return array of the values in the sequence
     * @throws IOException When no it possible load data.
     */
    public DerValue[] getSequence(final int startLen) throws IOException {
        this.tag = (byte)this.buffer.read();
        if (this.tag != DerValue.tag_Sequence) {
			throw new IOException("Sequence tag error");
		}
        return readVector(startLen);
    }

    /**
     * Return a set of encoded entities.  ASN.1 sets are unordered,
     * though DER may specify an order for some kinds of sets (such
     * as the attributes in an X.500 relative distinguished name)
     * to facilitate binary comparisons of encoded values.
     *
     * @param startLen guess about how large the set will be
     *          (used to initialize an auto-growing data structure)
     * @return array of the values in the sequence
     * @throws IOException When no it possible load data.
     */
    public DerValue[] getSet(final int startLen) throws IOException {
        this.tag = (byte)this.buffer.read();
        if (this.tag != DerValue.tag_Set) {
			throw new IOException("Set tag error");
		}
        return readVector(startLen);
    }

    /**
     * Return a set of encoded entities.  ASN.1 sets are unordered,
     * though DER may specify an order for some kinds of sets (such
     * as the attributes in an X.500 relative distinguished name)
     * to facilitate binary comparisons of encoded values.
     *
     * @param startLen guess about how large the set will be
     *          (used to initialize an auto-growing data structure)
     * @param implicit if true tag is assumed implicit.
     * @return array of the values in the sequence
     * @throws IOException When no it possible load data.
     */
    public DerValue[] getSet(final int startLen, final boolean implicit)
        throws IOException {
        this.tag = (byte)this.buffer.read();
        if (!implicit) {
            if (this.tag != DerValue.tag_Set) {
                throw new IOException("Set tag error");
            }
        }
        return readVector(startLen);
    }

    /*
     * Read a "vector" of values ... set or sequence have the
     * same encoding, except for the initial tag, so both use
     * this same helper routine.
     */
    protected DerValue[] readVector(final int startLen) throws IOException {
        DerInputStream  newstr;

        final byte lenByte = (byte)this.buffer.read();
        int len = getLength(lenByte, this.buffer);

        if (len == -1) {
           // indefinite length encoding found
           final int readLen = this.buffer.available();
           final int offset = 2;     // for tag and length bytes
           final byte[] indefData = new byte[readLen + offset];
           indefData[0] = this.tag;
           indefData[1] = lenByte;
           final DataInputStream dis = new DataInputStream(this.buffer);
           dis.readFully(indefData, offset, readLen);
           dis.close();
           final DerIndefLenConverter derIn = new DerIndefLenConverter();
           this.buffer = new DerInputBuffer(derIn.convert(indefData), this.buffer.allowBER);

           if (this.tag != this.buffer.read()) {
			throw new IOException("Indefinite length encoding" +
			        " not supported");
		}
           len = DerInputStream.getLength(this.buffer);
        }

        if (len == 0) {
			// return empty array instead of null, which should be
            // used only for missing optionals
            return new DerValue[0];
		}

        /*
         * Create a temporary stream from which to read the data,
         * unless it's not really needed.
         */
        if (this.buffer.available() == len) {
			newstr = this;
		} else {
			newstr = subStream(len, true);
		}

        /*
         * Pull values out of the stream.
         */
        final Vector<DerValue> vec = new Vector<>(startLen);
        DerValue value;

        do {
            value = new DerValue(newstr.buffer, this.buffer.allowBER);
            vec.addElement(value);
        } while (newstr.available() > 0);

        if (newstr.available() != 0) {
			throw new IOException("Extra data at end of vector");
		}

        /*
         * Now stick them into the array we're returning.
         */
        int             i;
		final int max = vec.size();
        final DerValue[]      retval = new DerValue[max];

        for (i = 0; i < max; i++) {
			retval[i] = vec.elementAt(i);
		}

        return retval;
    }

    /**
     * Get a single DER-encoded value from the input stream.
     * It can often be useful to pull a value from the stream
     * and defer parsing it.  For example, you can pull a nested
     * sequence out with one call, and only examine its elements
     * later when you really need to.
     * @return a single DER-encoded value.
     * @throws IOException When no it possible load data.
     */
    public DerValue getDerValue() throws IOException {
        return new DerValue(this.buffer);
    }

    /**
     * Read a string that was encoded as a UTF8String DER value.
     * @return a string that was encoded as a UTF8String DER value.
     * @throws IOException When no it possible load data.
     */
    public String getUTF8String() throws IOException {
        return readString(DerValue.tag_UTF8String, "UTF-8", "UTF8");
    }

    /**
     * Read a string that was encoded as a PrintableString DER value.
     * @return a string that was encoded as a PrintableString DER value.
     * @throws IOException When no it possible load data from InputStream.
     */
    public String getPrintableString() throws IOException {
        return readString(DerValue.tag_PrintableString, "Printable",
                          "ASCII");
    }

    /**
     * Read a string that was encoded as a T61String DER value.
     * @return a string that was encoded as a T61String DER value.
     * @throws IOException When no it possible load data.
     */
    public String getT61String() throws IOException {
        /*
         * Works for common characters between T61 and ASCII.
         */
        return readString(DerValue.tag_T61String, "T61", "ISO-8859-1");
    }

    /**
     * Read a string that was encoded as a IA5tring DER value.
     * @return a string that was encoded as a IA5tring DER value.
     * @throws IOException When no it possible load data.
     */
    public String getIA5String() throws IOException {
        return readString(DerValue.tag_IA5String, "IA5", "ASCII");
    }

    /**
     * Read a string that was encoded as a BMPString DER value.
     * @return a string that was encoded as a BMPString DER value.
     * @throws IOException When no it possible load data.
     */
    public String getBMPString() throws IOException {
        return readString(DerValue.tag_BMPString, "BMP",
                          "UnicodeBigUnmarked");
    }

    /**
     * Read a string that was encoded as a GeneralString DER value.
     * @return a string that was encoded as a GeneralString DER value.
     * @throws IOException When no it possible load data from InputStream.
     */
    public String getGeneralString() throws IOException {
        return readString(DerValue.tag_GeneralString, "General",
                          "ASCII");
    }

    /**
     * Private helper routine to read an encoded string from the input
     * stream.
     * @param stringTag the tag for the type of string to read
     * @param stringName a name to display in error messages
     * @param enc the encoder to use to interpret the data. Should
     * correspond to the stringTag above.
     * @throws IOException When no it possible load data.
     */
    private String readString(final byte stringTag, final String stringName,
                              final String enc) throws IOException {

        if (this.buffer.read() != stringTag) {
			throw new IOException("DER input not a " +
                                  stringName + " string");
		}

        final int length = getLength(this.buffer);
        final byte[] retval = new byte[length];
        if (length != 0 && this.buffer.read(retval) != length) {
			throw new IOException("Short read of DER " +
                                  stringName + " string");
		}

        return new String(retval, enc);
    }

//    /**
//     * Get a UTC encoded time value from the input stream.
//     */
//    public Date getUTCTime() throws IOException {
//        if (this.buffer.read() != DerValue.tag_UtcTime) {
//			throw new IOException("DER input, UTCtime tag invalid ");
//		}
//        return this.buffer.getUTCTime(getLength(this.buffer));
//    }
//
//    /**
//     * Get a Generalized encoded time value from the input stream.
//     */
//    public Date getGeneralizedTime() throws IOException {
//        if (this.buffer.read() != DerValue.tag_GeneralizedTime) {
//			throw new IOException("DER input, GeneralizedTime tag invalid ");
//		}
//        return this.buffer.getGeneralizedTime(getLength(this.buffer));
//    }

    /*
     * Get a byte from the input stream.
     */
    // package private
    int getByte() throws IOException {
        return 0x00ff & this.buffer.read();
    }

    public int peekByte() throws IOException {
        return this.buffer.peek();
    }

    // package private
    int getLength() throws IOException {
        return getLength(this.buffer);
    }

    /*
     * Get a length from the input stream, allowing for at most 32 bits of
     * encoding to be used.  (Not the same as getting a tagged integer!)
     *
     * @return the length or -1 if indefinite length found.
     * @exception IOException on parsing error or unsupported lengths.
     */
    static int getLength(final InputStream in) throws IOException {
        return getLength(in.read(), in);
    }

    /*
     * Get a length from the input stream, allowing for at most 32 bits of
     * encoding to be used.  (Not the same as getting a tagged integer!)
     *
     * @return the length or -1 if indefinite length found.
     * @exception IOException on parsing error or unsupported lengths.
     */
    static int getLength(final int lenByte, final InputStream in) throws IOException {
        int value, tmp;
        if (lenByte == -1) {
            throw new IOException("Short read of DER length");
        }

        final String mdName = "DerInputStream.getLength(): ";
        tmp = lenByte;
        if ((tmp & 0x080) == 0x00) { // short form, 1 byte datum
            value = tmp;
        } else {                     // long form or indefinite
            tmp &= 0x07f;

            /*
             * NOTE:  tmp == 0 indicates indefinite length encoded data.
             * tmp > 4 indicates more than 4Gb of data.
             */
            if (tmp == 0) {
				return -1;
			}
            if (tmp < 0 || tmp > 4) {
				throw new IOException(mdName + "lengthTag=" + tmp + ", "
                    + (tmp < 0 ? "incorrect DER encoding." : "too big."));
			}

            value = 0x0ff & in.read();
            tmp--;
            if (value == 0) {
                // DER requires length value be encoded in minimum number of bytes
                throw new IOException(mdName + "Redundant length bytes found");
            }
            while (tmp-- > 0) {
                value <<= 8;
                value += 0x0ff & in.read();
            }
            if (value < 0) {
                throw new IOException(mdName + "Invalid length bytes");
            } else if (value <= 127) {
                throw new IOException(mdName + "Should use short form for length");
            }
        }
        return value;
    }

    /**
     * Mark the current position in the buffer, so that
     * a later call to <code>reset</code> will return here.
     * @param value current position in the buffer.
     */
    public void mark(final int value) { this.buffer.mark(value); }


    /**
     * Return to the position of the last <code>mark</code>
     * call.  A mark is implicitly set at the beginning of
     * the stream when it is created.
     */
    public void reset() { this.buffer.reset(); }


    /**
     * Returns the number of bytes available for reading.
     * This is most useful for testing whether the stream is
     * empty.
     * @return the number of bytes available for reading.
     */
    public int available() { return this.buffer.available(); }
}
