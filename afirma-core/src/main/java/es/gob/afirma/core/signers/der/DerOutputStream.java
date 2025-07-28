/*
 * Copyright (c) 1996, 2010, Oracle and/or its affiliates. All rights reserved.
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
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;


/**
 * Output stream marshaling DER-encoded data.  This is eventually provided
 * in the form of a byte array; there is no advance limit on the size of
 * that byte array.
 *
 * <P>At this time, this class supports only a subset of the types of
 * DER data encodings which are defined.  That subset is sufficient for
 * generating most X.509 certificates.
 *
 *
 * @author David Brownell
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 */
public class DerOutputStream
extends ByteArrayOutputStream implements DerEncoder {
    /**
     * Construct an DER output stream.
     *
     * @param size how large a buffer to preallocate.
     */
    public DerOutputStream(final int size) { super(size); }

    /**
     * Construct an DER output stream.
     */
    public DerOutputStream() { }

    /**
     * Writes tagged, pre-marshaled data.  This calcuates and encodes
     * the length, so that the output data is the standard triple of
     * { tag, length, data } used by all DER values.
     *
     * @param tag the DER value tag for the data, such as
     *          <em>DerValue.tag_Sequence</em>
     * @param buf buffered data, which must be DER-encoded
     * @throws IOException When no it possible write data.
     */
    public void write(final byte tag, final byte[] buf) throws IOException {
        write(tag);
        putLength(buf.length);
        write(buf, 0, buf.length);
    }

    /**
     * Writes tagged data using buffer-to-buffer copy.  As above,
     * this writes a standard DER record.  This is often used when
     * efficiently encapsulating values in sequences.
     *
     * @param tag the DER value tag for the data, such as
     *          <em>DerValue.tag_Sequence</em>
     * @param out buffered data
     * @throws IOException When no it possible write data.
     */
    public void write(final byte tag, final DerOutputStream out) throws IOException {
        write(tag);
        putLength(out.count);
        write(out.buf, 0, out.count);
    }

    /**
     * Writes implicitly tagged data using buffer-to-buffer copy.  As above,
     * this writes a standard DER record.  This is often used when
     * efficiently encapsulating implicitly tagged values.
     *
     * @param tag the DER value of the context-specific tag that replaces
     * original tag of the value in the output, such as in
     * <pre>
     *          <em> @lt;field&gt; [N] IMPLICIT @lt;type&gt;</em>
     * </pre>
     * For example, <em>FooLength [1] IMPLICIT INTEGER</em>, with value=4;
     * would be encoded as "81 01 04"  whereas in explicit
     * tagging it would be encoded as "A1 03 02 01 04".
     * Notice that the tag is A1 and not 81, this is because with
     * explicit tagging the form is always constructed.
     * @param value original value being implicitly tagged
     * @throws IOException When no it possible write data.
     */
    public void writeImplicit(final byte tag, final DerOutputStream value)
    throws IOException {
        write(tag);
        write(value.buf, 1, value.count-1);
    }

    /**
     * Marshals pre-encoded DER value onto the output stream.
     * @param val DER value.
     * @throws IOException When no it possible write data.
     */
    public void putDerValue(final DerValue val) throws IOException {
        val.encode(this);
    }

    /*
     * PRIMITIVES -- these are "universal" ASN.1 simple types.
     *
     *  BOOLEAN, INTEGER, BIT STRING, OCTET STRING, NULL
     *  OBJECT IDENTIFIER, SEQUENCE(OF), SET(OF)
     *  PrintableString, T61String, IA5String, UTCTime
     */

    /**
     * Marshals a DER boolean on the output stream.
     * @param val Boolean value.
     * @throws IOException When no it possible write data.
     */
    public void putBoolean(final boolean val) throws IOException {
        write(DerValue.tag_Boolean);
        putLength(1);
        if (val) {
            write(0xff);
        } else {
            write(0);
        }
    }

    /**
     * Marshals a DER enumerated on the output stream.
     * @param i the enumerated value.
     * @throws IOException When no it possible write data.
     */
    public void putEnumerated(final int i) throws IOException {
        write(DerValue.tag_Enumerated);
        putIntegerContents(i);
    }

    /**
     * Marshals a DER integer on the output stream.
     *
     * @param i the integer in the form of a BigInteger.
     * @throws IOException When no it possible write data.
     */
    public void putInteger(final BigInteger i) throws IOException {
        write(DerValue.tag_Integer);
        final byte[]    buf = i.toByteArray(); // least number  of bytes
        putLength(buf.length);
        write(buf, 0, buf.length);
    }

    /**
     * Marshals a DER integer on the output stream.
     * @param i the integer in the form of an Integer.
     * @throws IOException When no it possible write data.
     */
    public void putInteger(final Integer i) throws IOException {
        putInteger(i.intValue());
    }

    /**
     * Marshals a DER integer on the output stream.
     * @param i the integer.
     * @throws IOException When no it possible write data.
     */
    public void putInteger(final int i) throws IOException {
        write(DerValue.tag_Integer);
        putIntegerContents(i);
    }

    private void putIntegerContents(final int i) throws IOException {

        final byte[] bytes = new byte[4];
        int start = 0;

        // Obtain the four bytes of the int

        bytes[3] = (byte) (i & 0xff);
        bytes[2] = (byte)((i & 0xff00) >>> 8);
        bytes[1] = (byte)((i & 0xff0000) >>> 16);
        bytes[0] = (byte)((i & 0xff000000) >>> 24);

        // Reduce them to the least number of bytes needed to
        // represent this int

        if (bytes[0] == (byte)0xff) {

            // Eliminate redundant 0xff

            for (int j = 0; j < 3; j++) {
                if (bytes[j] == (byte)0xff &&
                    (bytes[j+1] & 0x80) == 0x80) {
					start++;
				} else {
					break;
				}
             }
         } else if (bytes[0] == 0x00) {

             // Eliminate redundant 0x00

            for (int j = 0; j < 3; j++) {
                if (bytes[j] == 0x00 &&
                    (bytes[j+1] & 0x80) == 0) {
					start++;
				} else {
					break;
				}
            }
        }

        putLength(4 - start);
        for (int k = start; k < 4; k++) {
			write(bytes[k]);
		}
    }

    /**
     * Marshals a DER bit string on the output stream. The bit
     * string must be byte-aligned.
     *
     * @param bits the bit string, MSB first
     * @throws IOException When no it possible write data.
     */
    public void putBitString(final byte[] bits) throws IOException {
        write(DerValue.tag_BitString);
        putLength(bits.length + 1);
        write(0);               // all of last octet is used
        write(bits);
    }

    /**
     * Marshals a DER bit string on the output stream.
     * The bit strings need not be byte-aligned.
     *
     * @param ba the bit array.
     * @throws IOException When no it possible write data.
     */
    public void putUnalignedBitString(final BitArray ba) throws IOException {
        final byte[] bits = ba.toByteArray();

        write(DerValue.tag_BitString);
        putLength(bits.length + 1);
        write(bits.length*8 - ba.length()); // excess bits in last octet
        write(bits);
    }

    /**
     * Marshals a truncated DER bit string on the output stream.
     * The bit strings need not be byte-aligned.
     *
     * @param ba the bit string, MSB first
     * @throws IOException When no it possible write data.
     */
    public void putTruncatedUnalignedBitString(final BitArray ba) throws IOException {
        putUnalignedBitString(ba.truncate());
    }

    /**
     * DER-encodes an ASN.1 OCTET STRING value on the output stream.
     *
     * @param octets the octet string
     * @throws IOException When no it possible write data.
     */
    public void putOctetString(final byte[] octets) throws IOException {
        write(DerValue.tag_OctetString, octets);
    }

    /**
     * Marshals a DER "null" value on the output stream.  These are
     * often used to indicate optional values which have been omitted.
     * @throws IOException When no it possible write data.
     */
    public void putNull() throws IOException {
        write(DerValue.tag_Null);
        putLength(0);
    }

    /**
     * Marshals an object identifier (OID) on the output stream.
     * Corresponds to the ASN.1 "OBJECT IDENTIFIER" construct.
     * @param oid object identifier.
     * @throws IOException When no it possible write data.
     */
    public void putOID(final ObjectIdentifier oid) throws IOException {
        oid.encode(this);
    }

    /**
     * Marshals a sequence on the output stream.  This supports both
     * the ASN.1 "SEQUENCE" (zero to N values) and "SEQUENCE OF"
     * (one to N values) constructs.
     * @param seq sequence.
     * @throws IOException When no it possible write data.
     */
    public void putSequence(final DerValue[] seq) throws IOException {
        final DerOutputStream bytes = new DerOutputStream();
        int i;

        for (i = 0; i < seq.length; i++) {
			seq[i].encode(bytes);
		}

        write(DerValue.tag_Sequence, bytes);
    }

    /**
     * Marshals the contents of a set on the output stream without
     * ordering the elements.  Ok for BER encoding, but not for DER
     * encoding.
     *
     * For DER encoding, use orderedPutSet() or orderedPutSetOf().
     * @param set DER value.
     * @throws IOException When no it possible write data.
     */
    public void putSet(final DerValue[] set) throws IOException {
        final DerOutputStream bytes = new DerOutputStream();
        int i;

        for (i = 0; i < set.length; i++) {
			set[i].encode(bytes);
		}

        write(DerValue.tag_Set, bytes);
    }

    /**
     * Marshals the contents of a set on the output stream.  Sets
     * are semantically unordered, but DER requires that encodings of
     * set elements be sorted into ascending lexicographical order
     * before being output.  Hence sets with the same tags and
     * elements have the same DER encoding.
     *
     * This method supports the ASN.1 "SET OF" construct, but not
     * "SET", which uses a different order.
     * @param tag DER tag value.
     * @param set DER encoded data.
     * @throws IOException When no it possible write data.
     */
    public void putOrderedSetOf(final byte tag, final DerEncoder[] set) throws IOException {
        putOrderedSet(tag, set, lexOrder);
    }

    /**
     * Marshals the contents of a set on the output stream.  Sets
     * are semantically unordered, but DER requires that encodings of
     * set elements be sorted into ascending tag order
     * before being output.  Hence sets with the same tags and
     * elements have the same DER encoding.
     *
     * This method supports the ASN.1 "SET" construct, but not
     * "SET OF", which uses a different order.
     * @param tag DER tag value.
     * @param set DER encoded data.
     * @throws IOException When no it possible write data.
     */
    public void putOrderedSet(final byte tag, final DerEncoder[] set) throws IOException {
        putOrderedSet(tag, set, tagOrder);
    }

    /**
     *  Lexicographical order comparison on byte arrays, for ordering
     *  elements of a SET OF objects in DER encoding.
     */
    private static ByteArrayLexOrder lexOrder = new ByteArrayLexOrder();

    /**
     *  Tag order comparison on byte arrays, for ordering elements of
     *  SET objects in DER encoding.
     */
    private static ByteArrayTagOrder tagOrder = new ByteArrayTagOrder();

    /**
     * Marshals a the contents of a set on the output stream with the
     * encodings of its sorted in increasing order.
     * @param tag DER tag value.
     * @param set DER encoded data.
     * @param order the order to use when sorting encodings of components.
     * @throws IOException When no it possible write data.
     */
    private void putOrderedSet(final byte tag, final DerEncoder[] set,
                               final Comparator<byte[]> order) throws IOException {
        final DerOutputStream[] streams = new DerOutputStream[set.length];

        for (int i = 0; i < set.length; i++) {
            streams[i] = new DerOutputStream();
            set[i].derEncode(streams[i]);
        }

        // order the element encodings
        final byte[][] bufs = new byte[streams.length][];
        for (int i = 0; i < streams.length; i++) {
            bufs[i] = streams[i].toByteArray();
        }
        Arrays.<byte[]>sort(bufs, order);

        final DerOutputStream bytes = new DerOutputStream();
        for (int i = 0; i < streams.length; i++) {
            bytes.write(bufs[i]);
        }
        write(tag, bytes);

    }

    /**
     * Marshals a string as a DER encoded UTF8String.
     * @param s String.
     * @throws IOException When no it possible write data.
     */
    public void putUTF8String(final String s) throws IOException {
        writeString(s, DerValue.tag_UTF8String, "UTF8");
    }

    /**
     * Marshals a string as a DER encoded PrintableString.
     * @param s String.
     * @throws IOException When no it possible write data.
     */
    public void putPrintableString(final String s) throws IOException {
        writeString(s, DerValue.tag_PrintableString, "ASCII");
    }

    /**
     * Marshals a string as a DER encoded T61String.
     * @param s String.
     * @throws IOException When no it possible write data.
     */
    public void putT61String(final String s) throws IOException {
        /*
         * Works for characters that are defined in both ASCII and
         * T61.
         */
        writeString(s, DerValue.tag_T61String, "ISO-8859-1");
    }

    /**
     * Marshals a string as a DER encoded IA5String.
     * @param s String.
     * @throws IOException When no it possible write data.
     */
    public void putIA5String(final String s) throws IOException {
        writeString(s, DerValue.tag_IA5String, "ASCII");
    }

    /**
     * Marshals a string as a DER encoded BMPString.
     * @param s String.
     * @throws IOException When no it possible write data.
     */
    public void putBMPString(final String s) throws IOException {
        writeString(s, DerValue.tag_BMPString, "UnicodeBigUnmarked");
    }

    /**
     * Marshals a string as a DER encoded GeneralString.
     * @param s String.
     * @throws IOException When no it possible write data.
     */
    public void putGeneralString(final String s) throws IOException {
        writeString(s, DerValue.tag_GeneralString, "ASCII");
    }

    /**
     * Private helper routine for writing DER encoded string values.
     * @param s the string to write
     * @param stringTag one of the DER string tags that indicate which
     * encoding should be used to write the string out.
     * @param enc the name of the encoder that should be used corresponding
     * to the above tag.
     * @throws IOException When no it possible write data.
     */
    private void writeString(final String s, final byte stringTag, final String enc)
        throws IOException {

        final byte[] data = s.getBytes(enc);
        write(stringTag);
        putLength(data.length);
        write(data);
    }

    /**
     * Marshals a DER UTC time/date value.
     *
     * <P>YYMMDDhhmmss{Z|+hhmm|-hhmm} ... emits only using Zulu time
     * and with seconds (even if seconds=0) as per RFC 5280.
     * @param d Date.
     * @throws IOException When no it possible write data.
     */
    public void putUTCTime(final Date d) throws IOException {
        putTime(d, DerValue.tag_UtcTime);
    }

    /**
     * Marshals a DER Generalized Time/date value.
     *
     * <P>YYYYMMDDhhmmss{Z|+hhmm|-hhmm} ... emits only using Zulu time
     * and with seconds (even if seconds=0) as per RFC 5280.
     * @param d Date.
     * @throws IOException When no it possible write data.
     */
    public void putGeneralizedTime(final Date d) throws IOException {
        putTime(d, DerValue.tag_GeneralizedTime);
    }

    /**
     * Private helper routine for marshalling a DER UTC/Generalized
     * time/date value. If the tag specified is not that for UTC Time
     * then it defaults to Generalized Time.
     * @param d the date to be marshalled
     * @param tag the tag for UTC Time or Generalized Time
     * @throws IOException When no it possible write data.
     */
    private void putTime(final Date d, byte tag) throws IOException {

        /*
         * Format the date.
         */

        final TimeZone tz = TimeZone.getTimeZone("GMT");
        String pattern = null;

        if (tag == DerValue.tag_UtcTime) {
            pattern = "yyMMddHHmmss'Z'";
        } else {
            tag = DerValue.tag_GeneralizedTime;
            pattern = "yyyyMMddHHmmss'Z'";
        }

        final SimpleDateFormat sdf = new SimpleDateFormat(pattern, Locale.US);
        sdf.setTimeZone(tz);
        final byte[] time = sdf.format(d).getBytes("ISO-8859-1");

        /*
         * Write the formatted date.
         */

        write(tag);
        putLength(time.length);
        write(time);
    }

    /**
     * Put the encoding of the length in the stream.
     *
     * @param len the length of the attribute.
     * @throws IOException When no it possible write data.
     */
    public void putLength(final int len) throws IOException {
        if (len < 128) {
            write((byte)len);

        } else if (len < 1 << 8) {
            write((byte)0x081);
            write((byte)len);

        } else if (len < 1 << 16) {
            write((byte)0x082);
            write((byte)(len >> 8));
            write((byte)len);

        } else if (len < 1 << 24) {
            write((byte)0x083);
            write((byte)(len >> 16));
            write((byte)(len >> 8));
            write((byte)len);

        } else {
            write((byte)0x084);
            write((byte)(len >> 24));
            write((byte)(len >> 16));
            write((byte)(len >> 8));
            write((byte)len);
        }
    }

    /**
     * Put the tag of the attribute in the stream.
     *
     * @param tagClass the tag class type, one of UNIVERSAL, CONTEXT,
     *                            APPLICATION or PRIVATE
     * @param form if true, the value is constructed, otherwise it is
     * primitive.
     * @param val the tag value
     */
    public void putTag(final byte tagClass, final boolean form, final byte val) {
        byte tag = (byte)(tagClass | val);
        if (form) {
            tag |= (byte)0x20;
        }
        write(tag);
    }

    /**
     *  Write the current contents of this <code>DerOutputStream</code>
     *  to an <code>OutputStream</code>.
     *	@param out OutputStream.
     * @throws IOException When no it possible write data.
     */
    @Override
	public void derEncode(final OutputStream out) throws IOException {
        out.write(toByteArray());
    }
}
