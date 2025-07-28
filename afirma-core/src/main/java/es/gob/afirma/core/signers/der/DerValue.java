/**
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

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Represents a single DER-encoded value.  DER encoding rules are a subset
 * of the "Basic" Encoding Rules (BER), but they only support a single way
 * ("Definite" encoding) to encode any given value.
 *
 * <P>All DER-encoded data are triples <em>{type, length, data}</em>.  This
 * class represents such tagged values as they have been read (or constructed),
 * and provides structured access to the encoded data.
 *
 * <P>At this time, this class supports only a subset of the types of DER
 * data encodings which are defined.  That subset is sufficient for parsing
 * most X.509 certificates, and working with selected additional formats
 * (such as PKCS #10 certificate requests, and some kinds of PKCS #7 data).
 *
 * A note with respect to T61/Teletex strings: From RFC 1617, section 4.1.3
 * and RFC 5280, section 8, we assume that this kind of string will contain
 * ISO-8859-1 characters only.
 *
 *
 * @author David Brownell
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 */
public class DerValue {
    /** The tag class types */
    public static final byte TAG_UNIVERSAL = (byte)0x000;
    public static final byte TAG_APPLICATION = (byte)0x040;
    public static final byte TAG_CONTEXT = (byte)0x080;
    public static final byte TAG_PRIVATE = (byte)0x0c0;

    private static final int DEFAULT_BUFFER_SIZE = 8192;

    /**
     * The maximum size of array to allocate.
     * Some VMs reserve some header words in an array.
     * Attempts to allocate larger arrays may result in
     * OutOfMemoryError: Requested array size exceeds VM limit
     */
    private static final int MAX_BUFFER_SIZE = Integer.MAX_VALUE - 8;

    /** The DER tag of the value; one of the tag_ constants. */
    public byte                 tag;

    protected DerInputBuffer    buffer;

    /**
     * The DER-encoded data of the value, never null
     */
    public final DerInputStream data;

    private int                 length;

    /*
     * The type starts at the first byte of the encoding, and
     * is one of these tag_* values.  That may be all the type
     * data that is needed.
     */

    /*
     * These tags are the "universal" tags ... they mean the same
     * in all contexts.  (Mask with 0x1f -- five bits.)
     */

    /** Tag value indicating an ASN.1 "BOOLEAN" value. */
    public final static byte    tag_Boolean = 0x01;

    /** Tag value indicating an ASN.1 "INTEGER" value. */
    public final static byte    tag_Integer = 0x02;

    /** Tag value indicating an ASN.1 "BIT STRING" value. */
    public final static byte    tag_BitString = 0x03;

    /** Tag value indicating an ASN.1 "OCTET STRING" value. */
    public final static byte    tag_OctetString = 0x04;

    /** Tag value indicating an ASN.1 "NULL" value. */
    public final static byte    tag_Null = 0x05;

    /** Tag value indicating an ASN.1 "OBJECT IDENTIFIER" value. */
    public final static byte    tag_ObjectId = 0x06;

    /** Tag value including an ASN.1 "ENUMERATED" value */
    public final static byte    tag_Enumerated = 0x0A;

    /** Tag value indicating an ASN.1 "UTF8String" value. */
    public final static byte    tag_UTF8String = 0x0C;

    /** Tag value including a "printable" string */
    public final static byte    tag_PrintableString = 0x13;

    /** Tag value including a "teletype" string */
    public final static byte    tag_T61String = 0x14;

    /** Tag value including an ASCII string */
    public final static byte    tag_IA5String = 0x16;

    /** Tag value indicating an ASN.1 "UTCTime" value. */
    public final static byte    tag_UtcTime = 0x17;

    /** Tag value indicating an ASN.1 "GeneralizedTime" value. */
    public final static byte    tag_GeneralizedTime = 0x18;

    /** Tag value indicating an ASN.1 "GenerallString" value. */
    public final static byte    tag_GeneralString = 0x1B;

    /** Tag value indicating an ASN.1 "UniversalString" value. */
    public final static byte    tag_UniversalString = 0x1C;

    /** Tag value indicating an ASN.1 "BMPString" value. */
    public final static byte    tag_BMPString = 0x1E;

    // CONSTRUCTED seq/set

    /**
     * Tag value indicating an ASN.1
     * "SEQUENCE" (zero to N elements, order is significant).
     */
    public final static byte    tag_Sequence = 0x30;

    /**
     * Tag value indicating an ASN.1
     * "SEQUENCE OF" (one to N elements, order is significant).
     */
    public final static byte    tag_SequenceOf = 0x30;

    /**
     * Tag value indicating an ASN.1
     * "SET" (zero to N members, order does not matter).
     */
    public final static byte    tag_Set = 0x31;

    /**
     * Tag value indicating an ASN.1
     * "SET OF" (one to N members, order does not matter).
     */
    public final static byte    tag_SetOf = 0x31;

    /*
     * These values are the high order bits for the other kinds of tags.
     */

    /**
     * Returns true if the tag class is UNIVERSAL.
     * @return true if the tag class is UNIVERSAL.
     */
    public boolean isUniversal()      { return (this.tag & 0x0c0) == 0x000; }

    /**
     * Returns true if the tag class is APPLICATION.
     * @return true if the tag class is APPLICATION.
     */
    public boolean isApplication()    { return (this.tag & 0x0c0) == 0x040; }

    /**
     * Returns true if the CONTEXT SPECIFIC bit is set in the type tag.
     * This is associated with the ASN.1 "DEFINED BY" syntax.
     * @return true if the CONTEXT SPECIFIC bit is set in the type tag.
     */
    public boolean isContextSpecific() { return (this.tag & 0x0c0) == 0x080; }

    /**
     * Returns true if the CONTEXT SPECIFIC TAG matches the passed tag.
     * @param cntxtTag Content specific tag.
     * @return true if the CONTEXT SPECIFIC TAG matches the passed tag.
     */
    public boolean isContextSpecific(final byte cntxtTag) {
        if (!isContextSpecific()) {
            return false;
        }
        return (this.tag & 0x01f) == cntxtTag;
    }

    boolean isPrivate()        { return (this.tag & 0x0c0) == 0x0c0; }

    /**
     * Returns true if the CONSTRUCTED bit is set in the type tag.
     * @return true if the CONSTRUCTED bit is set in the type tag.
     */
    public boolean isConstructed()    { return (this.tag & 0x020) == 0x020; }

    /**
     * Returns true if the CONSTRUCTED TAG matches the passed tag.
     * @param constructedTag Constructed tag.
     * @return true if the CONSTRUCTED TAG matches the passed tag.
     */
    public boolean isConstructed(final byte constructedTag) {
        if (!isConstructed()) {
            return false;
        }
        return (this.tag & 0x01f) == constructedTag;
    }

    /**
     * Creates a PrintableString or UTF8string DER value from a string
     * @param value the String object to use for the DER value
     * @throws IOException When it is a invalid string.
     */
    public DerValue(final String value) throws IOException {
        boolean isPrintableString = true;
        for (int i = 0; i < value.length(); i++) {
            if (!isPrintableStringChar(value.charAt(i))) {
                isPrintableString = false;
                break;
            }
        }

        this.data = init(isPrintableString ? tag_PrintableString : tag_UTF8String, value);
    }

    /**
     * Creates a string type DER value from a String object
     * @param stringTag the tag for the DER value to create
     * @param value the String object to use for the DER value
     * @throws IOException When it is a invalid value.
     */
    public DerValue(final byte stringTag, final String value) throws IOException {
        this.data = init(stringTag, value);
    }

    // Creates a DerValue from a tag and some DER-encoded data w/ additional
    // arg to control whether DER checks are enforced.
    DerValue(final byte tag, final byte[] data, final boolean allowBER) {
        this.tag = tag;
        this.buffer = new DerInputBuffer(data.clone(), allowBER);
        this.length = data.length;
        this.data = new DerInputStream(this.buffer);
        this.data.mark(Integer.MAX_VALUE);
    }

    /**
     * Creates a DerValue from a tag and some DER-encoded data.
     *
     * @param tag the DER type tag
     * @param data the DER-encoded data
     */
    public DerValue(final byte tag, final byte[] data) {
        this(tag, data, true);
    }

    /*
     * package private
     */
    DerValue(final DerInputBuffer in) throws IOException {

        // XXX must also parse BER-encoded constructed
        // values such as sequences, sets...
        this.tag = (byte)in.read();
        final byte lenByte = (byte)in.read();
        this.length = DerInputStream.getLength(lenByte, in);
        if (this.length == -1) {  // indefinite length encoding found
            DerInputBuffer inbuf = in.dup();
            final int readLen = inbuf.available();
            final int offset = 2;     // for tag and length bytes
            final byte[] indefData = new byte[readLen + offset];
            indefData[0] = this.tag;
            indefData[1] = lenByte;
            final DataInputStream dis = new DataInputStream(inbuf);
            dis.readFully(indefData, offset, readLen);
            dis.close();
            final DerIndefLenConverter derIn = new DerIndefLenConverter();
            inbuf = new DerInputBuffer(derIn.convert(indefData), in.allowBER);
            if (this.tag != inbuf.read()) {
				throw new IOException
                        ("Indefinite length encoding not supported");
			}
            this.length = DerInputStream.getLength(inbuf);
            this.buffer = inbuf.dup();
            this.buffer.truncate(this.length);
            this.data = new DerInputStream(this.buffer);
            // indefinite form is encoded by sending a length field with a
            // length of 0. - i.e. [1000|0000].
            // the object is ended by sending two zero bytes.
            in.skip(this.length + offset);
        } else {

            this.buffer = in.dup();
            this.buffer.truncate(this.length);
            this.data = new DerInputStream(this.buffer);

            in.skip(this.length);
        }
    }

    // Get an ASN.1/DER encoded datum from a buffer w/ additional
    // arg to control whether DER checks are enforced.
    DerValue(final byte[] buf, final boolean allowBER) throws IOException {
        this.data = init(true, new ByteArrayInputStream(buf), allowBER);
    }

    /**
     * Get an ASN.1/DER encoded datum from a buffer.  The
     * entire buffer must hold exactly one datum, including
     * its tag and length.
     *
     * @param buf buffer holding a single DER-encoded datum.
     * @throws IOException When fail to read data.
     */
    public DerValue(final byte[] buf) throws IOException {
        this(buf, true);
    }

    // Get an ASN.1/DER encoded datum from part of a buffer w/ additional
    // arg to control whether DER checks are enforced.
    DerValue(final byte[] buf, final int offset, final int len, final boolean allowBER)
        throws IOException {
        this.data = init(true, new ByteArrayInputStream(buf, offset, len), allowBER);
    }

    /**
     * Get an ASN.1/DER encoded datum from part of a buffer.
     * That part of the buffer must hold exactly one datum, including
     * its tag and length.
     *
     * @param buf the buffer
     * @param offset start point of the single DER-encoded dataum
     * @param len how many bytes are in the encoded datum
     * @throws IOException When fail to read data.
     */
    public DerValue(final byte[] buf, final int offset, final int len) throws IOException {
        this(buf, offset, len, true);
    }

    // Get an ASN1/DER encoded datum from an input stream w/ additional
    // arg to control whether DER checks are enforced.
    DerValue(final InputStream in, final boolean allowBER) throws IOException {
        this.data = init(false, in, allowBER);
    }

    /**
     * Get an ASN1/DER encoded datum from an input stream.  The
     * stream may have additional data following the encoded datum.
     * In case of indefinite length encoded datum, the input stream
     * must hold only one datum.
     *
     * @param in the input stream holding a single DER datum,
     *  which may be followed by additional data
     * @throws IOException When fail to read data.
     */
    public DerValue(final InputStream in) throws IOException {
        this(in, true);
    }

    private DerInputStream init(final byte stringTag, final String value)
        throws IOException {
        String enc = null;

        this.tag = stringTag;

        switch (stringTag) {
        case tag_PrintableString:
        case tag_IA5String:
        case tag_GeneralString:
            enc = "ASCII";
            break;
        case tag_T61String:
            enc = "ISO-8859-1";
            break;
        case tag_BMPString:
            enc = "UnicodeBigUnmarked";
            break;
        case tag_UTF8String:
            enc = "UTF8";
            break;
            // TBD: Need encoder for UniversalString before it can
            // be handled.
        default:
            throw new IllegalArgumentException("Unsupported DER string type");
        }

        final byte[] buf = value.getBytes(enc);
        this.length = buf.length;
        this.buffer = new DerInputBuffer(buf, true);
        final DerInputStream result = new DerInputStream(this.buffer);
        result.mark(Integer.MAX_VALUE);
        return result;
    }

    /*
     * helper routine
     */
    private DerInputStream init(final boolean fullyBuffered, InputStream in,
        final boolean allowBER) throws IOException {

        this.tag = (byte)in.read();
        final byte lenByte = (byte)in.read();
        this.length = DerInputStream.getLength(lenByte, in);
        if (this.length == -1) { // indefinite length encoding found
            final int readLen = in.available();
            final int offset = 2;     // for tag and length bytes
            final byte[] indefData = new byte[readLen + offset];
            indefData[0] = this.tag;
            indefData[1] = lenByte;
            final DataInputStream dis = new DataInputStream(in);
            dis.readFully(indefData, offset, readLen);
            dis.close();
            final DerIndefLenConverter derIn = new DerIndefLenConverter();
            in = new ByteArrayInputStream(derIn.convert(indefData));
            if (this.tag != in.read()) {
				throw new IOException
                        ("Indefinite length encoding not supported");
			}
            this.length = DerInputStream.getLength(in);
        }

        if (fullyBuffered && in.available() != this.length) {
			throw new IOException("extra data given to DerValue constructor");
		}

        final byte[] bytes = readExactlyNBytes(in, this.length);

        this.buffer = new DerInputBuffer(bytes, allowBER);
        return new DerInputStream(this.buffer);
    }

    /**
     * Encode an ASN1/DER encoded datum onto a DER output stream.
     * @param out DER output stream
     * @throws IOException When fail to write data.
     */
    public void encode(final DerOutputStream out)
    throws IOException {
        out.write(this.tag);
        out.putLength(this.length);
        // XXX yeech, excess copies ... DerInputBuffer.write(OutStream)
        if (this.length > 0) {
            final byte[] value = new byte[this.length];
            // always synchronized on data
            synchronized (this.data) {
                this.buffer.reset();
                if (this.buffer.read(value) != this.length) {
                    throw new IOException("short DER value read (encode)");
                }
                out.write(value);
            }
        }
    }

    public final DerInputStream getData() {
        return this.data;
    }

    public final byte getTag() {
        return this.tag;
    }

    /**
     * Returns an ASN.1 BOOLEAN
     *
     * @return the boolean held in this DER value
     * @throws IOException When next data is not a boolean value.
     */
    public boolean getBoolean() throws IOException {
        if (this.tag != tag_Boolean) {
            throw new IOException("DerValue.getBoolean, not a BOOLEAN " + this.tag);
        }
        if (this.length != 1) {
            throw new IOException("DerValue.getBoolean, invalid length "
                                        + this.length);
        }
        if (this.buffer.read() != 0) {
            return true;
        }
        return false;
    }

    /**
     * Returns an ASN.1 OBJECT IDENTIFIER.
     *
     * @return the OID held in this DER value
     * @throws IOException When next data is not a OID value.
     */
    public ObjectIdentifier getOID() throws IOException {
        if (this.tag != tag_ObjectId) {
			throw new IOException("DerValue.getOID, not an OID " + this.tag);
		}
        return new ObjectIdentifier(this.buffer);
    }

    private byte[] append(final byte[] a, final byte[] b) {
        if (a == null) {
			return b;
		}

        final byte[] ret = new byte[a.length + b.length];
        System.arraycopy(a, 0, ret, 0, a.length);
        System.arraycopy(b, 0, ret, a.length, b.length);

        return ret;
    }

    /**
     * Returns an ASN.1 OCTET STRING
     *
     * @return the octet string held in this DER value
     * @throws IOException When next data is not a OctetString value.
     */
    public byte[] getOctetString() throws IOException {

        if (this.tag != tag_OctetString && !isConstructed(tag_OctetString)) {
            throw new IOException(
                "DerValue.getOctetString, not an Octet String: " + this.tag);
        }
        // Note: do not attempt to call buffer.read(bytes) at all. There's a
        // known bug that it returns -1 instead of 0.
        if (this.length == 0) {
            return new byte[0];
        }

        // Only allocate the array if there are enough bytes available.
        // This only works for ByteArrayInputStream.
        // The assignment below ensures that buffer has the required type.
        final ByteArrayInputStream arrayInput = this.buffer;
        if (arrayInput.available() < this.length) {
            throw new IOException("short read on DerValue buffer");
        }
        byte[] bytes = new byte[this.length];
        arrayInput.read(bytes);

        if (isConstructed()) {
            final DerInputStream in = new DerInputStream(bytes, 0, bytes.length,
                this.buffer.allowBER);
            bytes = null;
            while (in.available() != 0) {
                bytes = append(bytes, in.getOctetString());
            }
        }
        return bytes;
    }

    /**
     * Returns an ASN.1 INTEGER value as an integer.
     *
     * @return the integer held in this DER value.
     * @throws IOException When next data is not a Integer value.
     */
    public int getInteger() throws IOException {
        if (this.tag != tag_Integer) {
            throw new IOException("DerValue.getInteger, not an int " + this.tag);
        }
        return this.buffer.getInteger(this.data.available());
    }

    /**
     * Returns an ASN.1 INTEGER value as a BigInteger.
     *
     * @return the integer held in this DER value as a BigInteger.
     * @throws IOException When next data is not a BigInteger value.
     */
    public BigInteger getBigInteger() throws IOException {
        if (this.tag != tag_Integer) {
			throw new IOException("DerValue.getBigInteger, not an int " + this.tag);
		}
        return this.buffer.getBigInteger(this.data.available(), false);
    }

    /**
     * Returns an ASN.1 INTEGER value as a positive BigInteger.
     * This is just to deal with implementations that incorrectly encode
     * some values as negative.
     *
     * @return the integer held in this DER value as a BigInteger.
     * @throws IOException When next data is not a BigInteger value.
     */
    public BigInteger getPositiveBigInteger() throws IOException {
        if (this.tag != tag_Integer) {
			throw new IOException("DerValue.getBigInteger, not an int " + this.tag);
		}
        return this.buffer.getBigInteger(this.data.available(), true);
    }

    /**
     * Returns an ASN.1 ENUMERATED value.
     *
     * @return the integer held in this DER value.
     * @throws IOException When next data is not a Enumerated value.
     */
    public int getEnumerated() throws IOException {
        if (this.tag != tag_Enumerated) {
            throw new IOException("DerValue.getEnumerated, incorrect tag: "
                                  + this.tag);
        }
        return this.buffer.getInteger(this.data.available());
    }

    /**
     * Returns an ASN.1 BIT STRING value.  The bit string must be byte-aligned.
     *
     * @return the bit string held in this value
     * @throws IOException When next data is not a BitString value.
     */
    public byte[] getBitString() throws IOException {
        if (this.tag != tag_BitString) {
			throw new IOException(
                "DerValue.getBitString, not a bit string " + this.tag);
		}

        return this.buffer.getBitString();
    }

    /**
     * Returns an ASN.1 BIT STRING value that need not be byte-aligned.
     *
     * @return a BitArray representing the bit string held in this value
     * @throws IOException When next data is not a BitArray value.
     */
    public BitArray getUnalignedBitString() throws IOException {
        if (this.tag != tag_BitString) {
			throw new IOException(
                "DerValue.getBitString, not a bit string " + this.tag);
		}

        return this.buffer.getUnalignedBitString();
    }

    /**
     * Returns the name component as a Java string, regardless of its
     * encoding restrictions (ASCII, T61, Printable, IA5, BMP, UTF8).
     * @return Name component.
     * @throws IOException When no it possible load data.
     */
    // TBD: Need encoder for UniversalString before it can be handled.
    public String getAsString() throws IOException {
        if (this.tag == tag_UTF8String) {
			return getUTF8String();
		} else if (this.tag == tag_PrintableString) {
			return getPrintableString();
		} else if (this.tag == tag_T61String) {
			return getT61String();
		} else if (this.tag == tag_IA5String) {
			return getIA5String();
		} else if (this.tag == tag_BMPString) {
			return getBMPString();
		} else if (this.tag == tag_GeneralString) {
			return getGeneralString();
		} else {
			return null;
		}
    }

    /**
     * Returns an ASN.1 BIT STRING value, with the tag assumed implicit
     * based on the parameter.  The bit string must be byte-aligned.
     *
     * @param tagImplicit if true, the tag is assumed implicit.
     * @return the bit string held in this value
     * @throws IOException When the tag is incorrect.
     */
    public byte[] getBitString(final boolean tagImplicit) throws IOException {
        if (!tagImplicit) {
            if (this.tag != tag_BitString) {
				throw new IOException("DerValue.getBitString, not a bit string "
                                       + this.tag);
			}
            }
        return this.buffer.getBitString();
    }

    /**
     * Returns an ASN.1 BIT STRING value, with the tag assumed implicit
     * based on the parameter.  The bit string need not be byte-aligned.
     *
     * @param tagImplicit if true, the tag is assumed implicit.
     * @return the bit string held in this value
     * @throws IOException When the tag is incorrect.
     */
    public BitArray getUnalignedBitString(final boolean tagImplicit)
    throws IOException {
        if (!tagImplicit) {
            if (this.tag != tag_BitString) {
				throw new IOException("DerValue.getBitString, not a bit string "
                                       + this.tag);
			}
            }
        return this.buffer.getUnalignedBitString();
    }

    /**
     * Helper routine to return all the bytes contained in the
     * DerInputStream associated with this object.
     * @return Content.
     * @throws IOException When no it possible load data.
     */
    public byte[] getDataBytes() throws IOException {
        final byte[] retVal = new byte[this.length];
        synchronized (this.data) {
            this.data.reset();
            this.data.getBytes(retVal);
        }
        return retVal;
    }

    /**
     * Returns an ASN.1 STRING value
     *
     * @return the printable string held in this value
     * @throws IOException When no it possible load data.
     */
    public String getPrintableString()
    throws IOException {
        if (this.tag != tag_PrintableString) {
			throw new IOException(
                "DerValue.getPrintableString, not a string " + this.tag);
		}

        return new String(getDataBytes(), "ASCII");
    }

    /**
     * Returns an ASN.1 T61 (Teletype) STRING value
     *
     * @return the teletype string held in this value
     * @throws IOException When no it possible load data.
     */
    public String getT61String() throws IOException {
        if (this.tag != tag_T61String) {
			throw new IOException(
                "DerValue.getT61String, not T61 " + this.tag);
		}

        return new String(getDataBytes(), "ISO-8859-1");
    }

    /**
     * Returns an ASN.1 IA5 (ASCII) STRING value
     *
     * @return the ASCII string held in this value
     * @throws IOException When no it possible load data.
     */
    public String getIA5String() throws IOException {
        if (this.tag != tag_IA5String) {
			throw new IOException(
                "DerValue.getIA5String, not IA5 " + this.tag);
		}

        return new String(getDataBytes(), "ASCII");
    }

    /**
     * Returns the ASN.1 BMP (Unicode) STRING value as a Java string.
     *
     * @return a string corresponding to the encoded BMPString held in
     * this value
     * @throws IOException When no it possible load data.
     */
    public String getBMPString() throws IOException {
        if (this.tag != tag_BMPString) {
			throw new IOException(
                "DerValue.getBMPString, not BMP " + this.tag);
		}

        // BMPString is the same as Unicode in big endian, unmarked
        // format.
        return new String(getDataBytes(), "UnicodeBigUnmarked");
    }

    /**
     * Returns the ASN.1 UTF-8 STRING value as a Java String.
     *
     * @return a string corresponding to the encoded UTF8String held in
     * this value
     * @throws IOException When no it possible load data.
     */
    public String getUTF8String() throws IOException {
        if (this.tag != tag_UTF8String) {
			throw new IOException(
                "DerValue.getUTF8String, not UTF-8 " + this.tag);
		}

        return new String(getDataBytes(), "UTF8");
    }

    /**
     * Returns the ASN.1 GENERAL STRING value as a Java String.
     *
     * @return a string corresponding to the encoded GeneralString held in
     * this value
     * @throws IOException When no it possible load data.
     */
    public String getGeneralString() throws IOException {
        if (this.tag != tag_GeneralString) {
			throw new IOException(
                "DerValue.getGeneralString, not GeneralString " + this.tag);
		}

        return new String(getDataBytes(), "ASCII");
    }

//    /**
//     * Returns a Date if the DerValue is UtcTime.
//     *
//     * @return the Date held in this DER value
//     */
//    public Date getUTCTime() throws IOException {
//        if (this.tag != tag_UtcTime) {
//            throw new IOException("DerValue.getUTCTime, not a UtcTime: " + this.tag);
//        }
//        return this.buffer.getUTCTime(this.data.available());
//    }
//
//    /**
//     * Returns a Date if the DerValue is GeneralizedTime.
//     *
//     * @return the Date held in this DER value
//     */
//    public Date getGeneralizedTime() throws IOException {
//        if (this.tag != tag_GeneralizedTime) {
//            throw new IOException(
//                "DerValue.getGeneralizedTime, not a GeneralizedTime: " + this.tag);
//        }
//        return this.buffer.getGeneralizedTime(this.data.available());
//    }

    /**
     * Returns true iff the other object is a DER value which
     * is bitwise equal to this one.
     *
     * @param other the object being compared with this one
     */
    @Override
	public boolean equals(final Object other) {
        if (other instanceof DerValue) {
			return equals((DerValue)other);
		} else {
			return false;
		}
    }

    /**
     * Bitwise equality comparison.  DER encoded values have a single
     * encoding, so that bitwise equality of the encoded values is an
     * efficient way to establish equivalence of the unencoded values.
     *
     * @param other the object being compared with this one
     * @return {@code true} if the object is a DerValue with the same
     * content.
     */
    public boolean equals(final DerValue other) {
        if (this == other) {
            return true;
        }
        if (this.tag != other.tag) {
            return false;
        }
        if (this.data == other.data) {
            return true;
        }

        // make sure the order of lock is always consistent to avoid a deadlock
        return System.identityHashCode(this.data)
                > System.identityHashCode(other.data) ?
                doEquals(this, other):
                doEquals(other, this);
    }

    /**
     * Helper for public method equals()
     * @param d1 Value 1.
     * @param d2 Value 2.
     * @return {@code true} if the object is a DerValue with the same
     * content.
     */
    private static boolean doEquals(final DerValue d1, final DerValue d2) {
        synchronized (d1.data) {
            synchronized (d2.data) {
                d1.data.reset();
                d2.data.reset();
                return d1.buffer.equals(d2.buffer);
            }
        }
    }

    /**
     * Returns a printable representation of the value.
     *
     * @return printable representation of the value
     */
    @Override
	public String toString() {
        try {

            final String str = getAsString();
            if (str != null) {
				return "\"" + str + "\"";
			}
            if (this.tag == tag_Null) {
				return "[DerValue, null]";
			}
            if (this.tag == tag_ObjectId) {
				return "OID." + getOID();
			} else {
				return "[DerValue, tag = " + this.tag
                        + ", length = " + this.length + "]";
			}
        } catch (final IOException e) {
            throw new IllegalArgumentException("misformatted DER value");
        }
    }

    /**
     * Returns a DER-encoded value, such that if it's passed to the
     * DerValue constructor, a value equivalent to "this" is returned.
     *
     * @return DER-encoded value, including tag and length.
     * @throws IOException When no it possible load data.
     */
    public byte[] toByteArray() throws IOException {
        final DerOutputStream out = new DerOutputStream();

        encode(out);
        this.data.reset();
        return out.toByteArray();
    }

    /**
     * For "set" and "sequence" types, this function may be used
     * to return a DER stream of the members of the set or sequence.
     * This operation is not supported for primitive types such as
     * integers or bit strings.
     * @return DER stream of the members of the set or sequence.
     * @throws IOException When no it possible load data.
     */
    public DerInputStream toDerInputStream() throws IOException {
        if (this.tag == tag_Sequence || this.tag == tag_Set) {
			return new DerInputStream(this.buffer);
		}
        throw new IOException("toDerInputStream rejects tag type " + this.tag);
    }

    /**
     * Get the length of the encoded value.
     * @return Length.
     */
    public int length() {
        return this.length;
    }

    /**
     * Determine if a character is one of the permissible characters for
     * PrintableString:
     * A-Z, a-z, 0-9, space, apostrophe (39), left and right parentheses,
     * plus sign, comma, hyphen, period, slash, colon, equals sign,
     * and question mark.
     *
     * Characters that are *not* allowed in PrintableString include
     * exclamation point, quotation mark, number sign, dollar sign,
     * percent sign, ampersand, asterisk, semicolon, less than sign,
     * greater than sign, at sign, left and right square brackets,
     * backslash, circumflex (94), underscore, back quote (96),
     * left and right curly brackets, vertical line, tilde,
     * and the control codes (0-31 and 127).
     *
     * This list is based on X.680 (the ASN.1 spec).
     * @param ch Character.
     * @return {@code true} if the character is one of the permissible
     * characters for PrintableString.
     */
    public static boolean isPrintableStringChar(final char ch) {
        if (ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' ||
            ch >= '0' && ch <= '9') {
            return true;
        } else {
            switch (ch) {
                case ' ':       /* space */
                case '\'':      /* apostrophe */
                case '(':       /* left paren */
                case ')':       /* right paren */
                case '+':       /* plus */
                case ',':       /* comma */
                case '-':       /* hyphen */
                case '.':       /* period */
                case '/':       /* slash */
                case ':':       /* colon */
                case '=':       /* equals */
                case '?':       /* question mark */
                    return true;
                default:
                    return false;
            }
        }
    }

    /**
     * Create the tag of the attribute.
     *
     * @param tagClass the tag class type, one of UNIVERSAL, CONTEXT,
     *               APPLICATION or PRIVATE
     * @param form if true, the value is constructed, otherwise it
     * is primitive.
     * @param val the tag value
     * @return Tag value.
     */
    public static byte createTag(final byte tagClass, final boolean form, final byte val) {
        byte tag = (byte)(tagClass | val);
        if (form) {
            tag |= (byte)0x20;
        }
        return tag;
    }

    /**
     * Set the tag of the attribute. Commonly used to reset the
     * tag value used for IMPLICIT encodings.
     *
     * @param tag the tag value
     */
    public void resetTag(final byte tag) {
        this.tag = tag;
    }

    /**
     * Returns a hashcode for this DerValue.
     *
     * @return a hashcode for this DerValue.
     */
    @Override
	public int hashCode() {
        return toString().hashCode();
    }

    /**
     * Read exactly {@code length} of bytes from {@code in}.
     *
     * <p> Note that this method is safe to be called with unknown large
     * {@code length} argument. The memory used is proportional to the
     * actual bytes available. An exception is thrown if there are not
     * enough bytes in the stream.
     *
     * @param is input stream, must not be null
     * @param length number of bytes to read
     * @return bytes read
     * @throws EOFException if there are not enough bytes in the stream
     * @throws IOException if an I/O error occurs or {@code length} is negative
     * @throws OutOfMemoryError if an array of the required size cannot be
     *         allocated.
     */
    public static byte[] readExactlyNBytes(final InputStream is, final int length)
            throws IOException {
        if (length < 0) {
            throw new IOException("length cannot be negative: " + length);
        }
        final byte[] data = readNBytes(is, length);
        if (data.length < length) {
            throw new EOFException();
        }
        return data;
    }

    /**
     * Reads up to a specified number of bytes from the input stream. This
     * method blocks until the requested number of bytes have been read, end
     * of stream is detected, or an exception is thrown. This method does not
     * close the input stream.
     *
     * <p> The length of the returned array equals the number of bytes read
     * from the stream. If {@code len} is zero, then no bytes are read and
     * an empty byte array is returned. Otherwise, up to {@code len} bytes
     * are read from the stream. Fewer than {@code len} bytes may be read if
     * end of stream is encountered.
     *
     * <p> When this stream reaches end of stream, further invocations of this
     * method will return an empty byte array.
     *
     * <p> Note that this method is intended for simple cases where it is
     * convenient to read the specified number of bytes into a byte array. The
     * total amount of memory allocated by this method is proportional to the
     * number of bytes read from the stream which is bounded by {@code len}.
     * Therefore, the method may be safely called with very large values of
     * {@code len} provided sufficient memory is available.
     *
     * <p> The behavior for the case where the input stream is <i>asynchronously
     * closed</i>, or the thread interrupted during the read, is highly input
     * stream specific, and therefore not specified.
     *
     * <p> If an I/O error occurs reading from the input stream, then it may do
     * so after some, but not all, bytes have been read. Consequently the input
     * stream may not be at end of stream and may be in an inconsistent state.
     * It is strongly recommended that the stream be promptly closed if an I/O
     * error occurs.
     *
     * The number of bytes allocated to read data from this stream and return
     * the result is bounded by {@code 2*(long)len}, inclusive.
     *
     * @param is input stream, must not be null
     * @param len the maximum number of bytes to read
     * @return a byte array containing the bytes read from this input stream
     * @throws IllegalArgumentException if {@code length} is negative
     * @throws IOException if an I/O error occurs
     * @throws OutOfMemoryError if an array of the required size cannot be
     *         allocated.
     *
     * @since 11
     */
    public static byte[] readNBytes(final InputStream is, final int len) throws IOException {
        if (len < 0) {
            throw new IllegalArgumentException("len < 0");
        }

        List<byte[]> bufs = null;
        byte[] result = null;
        int total = 0;
        int remaining = len;
        int n;
        do {
            final byte[] buf = new byte[Math.min(remaining, DEFAULT_BUFFER_SIZE)];
            int nread = 0;

            // read to EOF which may read more or less than buffer size
            while ((n = is.read(buf, nread,
                    Math.min(buf.length - nread, remaining))) > 0) {
                nread += n;
                remaining -= n;
            }

            if (nread > 0) {
                if (MAX_BUFFER_SIZE - total < nread) {
                    throw new OutOfMemoryError("Required array size too large");
                }
                total += nread;
                if (result == null) {
                    result = buf;
                } else {
                    if (bufs == null) {
                        bufs = new ArrayList<>();
                        bufs.add(result);
                    }
                    bufs.add(buf);
                }
            }
            // if the last call to read returned -1 or the number of bytes
            // requested have been read then break
        } while (n >= 0 && remaining > 0);

        if (bufs == null) {
            if (result == null) {
                return new byte[0];
            }
            return result.length == total ?
                result : Arrays.copyOf(result, total);
        }

        result = new byte[total];
        int offset = 0;
        remaining = total;
        for (final byte[] b : bufs) {
            final int count = Math.min(b.length, remaining);
            System.arraycopy(b, 0, result, offset, count);
            offset += count;
            remaining -= count;
        }

        return result;
    }
}
