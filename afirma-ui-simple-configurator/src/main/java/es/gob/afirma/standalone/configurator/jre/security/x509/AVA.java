/*
 * Copyright (c) 1996, 2011, Oracle and/or its affiliates. All rights reserved.
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

package es.gob.afirma.standalone.configurator.jre.security.x509;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.security.AccessController;
import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import es.gob.afirma.standalone.configurator.jre.security.action.GetBooleanAction;
import es.gob.afirma.standalone.configurator.jre.security.pkcs.PKCS9Attribute;
import es.gob.afirma.standalone.configurator.jre.security.util.Debug;
import es.gob.afirma.standalone.configurator.jre.security.util.DerEncoder;
import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;


/**
 * X.500 Attribute-Value-Assertion (AVA):  an attribute, as identified by
 * some attribute ID, has some particular value.  Values are as a rule ASN.1
 * printable strings.  A conventional set of type IDs is recognized when
 * parsing (and generating) RFC 1779, 2253 or 4514 syntax strings.
 *
 * <P>AVAs are components of X.500 relative names.  Think of them as being
 * individual fields of a database record.  The attribute ID is how you
 * identify the field, and the value is part of a particular record.
 * <p>
 * Note that instances of this class are immutable.
 *
 * @see X500Name
 * @see RDN
 *
 *
 * @author David Brownell
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 */
public class AVA implements DerEncoder {

    private static final Debug debug = Debug.getInstance("x509", "\t[AVA]"); //$NON-NLS-1$ //$NON-NLS-2$
    // See CR 6391482: if enabled this flag preserves the old but incorrect
    // PrintableString encoding for DomainComponent. It may need to be set to
    // avoid breaking preexisting certificates generated with sun.security APIs.
    private static final boolean PRESERVE_OLD_DC_ENCODING =
        AccessController.doPrivileged(new GetBooleanAction
            ("com.sun.security.preserveOldDCEncoding")); //$NON-NLS-1$

    /**
     * DEFAULT format allows both RFC1779 and RFC2253 syntax and
     * additional keywords.
     */
    final static int DEFAULT = 1;
    /**
     * RFC1779 specifies format according to RFC1779.
     */
    final static int RFC1779 = 2;
    /**
     * RFC2253 specifies format according to RFC2253.
     */
    final static int RFC2253 = 3;

    // currently not private, accessed directly from RDN
    final ObjectIdentifier oid;
    final DerValue value;

    /*
     * If the value has any of these characters in it, it must be quoted.
     * Backslash and quote characters must also be individually escaped.
     * Leading and trailing spaces, also multiple internal spaces, also
     * call for quoting the whole string.
     */
    private static final String specialChars1779 = ",=\n+<>#;\\\""; //$NON-NLS-1$

    /*
     * In RFC2253, if the value has any of these characters in it, it
     * must be quoted by a preceding \.
     */
    private static final String specialChars2253 = ",=+<>#;\\\""; //$NON-NLS-1$

    /*
     * includes special chars from RFC1779 and RFC2253, as well as ' ' from
     * RFC 4514.
     */
    private static final String specialCharsDefault = ",=\n+<>#;\\\" "; //$NON-NLS-1$
    private static final String escapedDefault = ",+<>;\""; //$NON-NLS-1$

    /*
     * Values that aren't printable strings are emitted as BER-encoded
     * hex data.
     */
    private static final String hexDigits = "0123456789ABCDEF"; //$NON-NLS-1$

	public AVA(final ObjectIdentifier type, final DerValue val) {
        if ((type == null) || (val == null)) {
            throw new NullPointerException();
        }
        this.oid = type;
        this.value = val;
    }

    /**
     * Parse an RFC 1779, 2253 or 4514 style AVA string:  CN=fee fie foe fum
     * or perhaps with quotes.  Not all defined AVA tags are supported;
     * of current note are X.400 related ones (PRMD, ADMD, etc).
     *
     * This terminates at unescaped AVA separators ("+") or RDN
     * separators (",", ";"), and removes cosmetic whitespace at the end of
     * values.
     */
    AVA(final Reader in) throws IOException {
        this(in, DEFAULT);
    }

    /**
     * Parse an RFC 1779, 2253 or 4514 style AVA string:  CN=fee fie foe fum
     * or perhaps with quotes. Additional keywords can be specified in the
     * keyword/OID map.
     *
     * This terminates at unescaped AVA separators ("+") or RDN
     * separators (",", ";"), and removes cosmetic whitespace at the end of
     * values.
     */
    AVA(final Reader in, final Map<String, String> keywordMap) throws IOException {
        this(in, DEFAULT, keywordMap);
    }

    /**
     * Parse an AVA string formatted according to format.
     */
    AVA(final Reader in, final int format) throws IOException {
        this(in, format, Collections.<String, String>emptyMap());
    }

    /**
     * Parse an AVA string formatted according to format.
     *
     * @param in Reader containing AVA String
     * @param format parsing format
     * @param keywordMap a Map where a keyword String maps to a corresponding
     *   OID String. Each AVA keyword will be mapped to the corresponding OID.
     *   If an entry does not exist, it will fallback to the builtin
     *   keyword/OID mapping.
     * @throws IOException if the AVA String is not valid in the specified
     *   format or an OID String from the keywordMap is improperly formatted
     */
    AVA(final Reader in, final int format, final Map<String, String> keywordMap)
        throws IOException {
        // assume format is one of DEFAULT or RFC2253

        final StringBuilder   temp = new StringBuilder();
        int             c;

        /*
         * First get the keyword indicating the attribute's type,
         * and map it to the appropriate OID.
         */
        while (true) {
            c = readChar(in, "Incorrect AVA format"); //$NON-NLS-1$
            if (c == '=') {
                break;
            }
            temp.append((char)c);
        }

        this.oid = AVAKeyword.getOID(temp.toString(), format, keywordMap);

        /*
         * Now parse the value.  "#hex", a quoted string, or a string
         * terminated by "+", ",", ";".  Whitespace before or after
         * the value is stripped away unless format is RFC2253.
         */
        temp.setLength(0);
        if (format == RFC2253) {
            // read next character
            c = in.read();
            if (c == ' ') {
                throw new IOException("Incorrect AVA RFC2253 format - " + //$NON-NLS-1$
                                      "leading space must be escaped"); //$NON-NLS-1$
            }
        } else {
            // read next character skipping whitespace
            do {
                c = in.read();
            } while ((c == ' ') || (c == '\n'));
        }
        if (c == -1) {
            // empty value
            this.value = new DerValue(""); //$NON-NLS-1$
            return;
        }

        if (c == '#') {
            this.value = parseHexString(in, format);
        } else if ((c == '"') && (format != RFC2253)) {
            this.value = parseQuotedString(in, temp);
        } else {
            this.value = parseString(in, c, format, temp);
        }
    }

    /**
     * Get the ObjectIdentifier of this AVA.
     */
    public ObjectIdentifier getObjectIdentifier() {
        return this.oid;
    }

    /**
     * Get the value of this AVA as a DerValue.
     */
    public DerValue getDerValue() {
        return this.value;
    }

    /**
     * Get the value of this AVA as a String.
     *
     * @exception RuntimeException if we could not obtain the string form
     *    (should not occur)
     */
    public String getValueString() {
        try {
            final String s = this.value.getAsString();
            if (s == null) {
                throw new RuntimeException("AVA string is null"); //$NON-NLS-1$
            }
            return s;
        } catch (final IOException e) {
            // should not occur
            throw new RuntimeException("AVA error: " + e, e); //$NON-NLS-1$
        }
    }

    private static DerValue parseHexString
        (final Reader in, final int format) throws IOException {

        int c;
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte b = 0;
        int cNdx = 0;
        while (true) {
            c = in.read();

            if (isTerminator(c, format)) {
                break;
            }

            final int cVal = hexDigits.indexOf(Character.toUpperCase((char)c));

            if (cVal == -1) {
                throw new IOException("AVA parse, invalid hex " + //$NON-NLS-1$
                                              "digit: "+ (char)c); //$NON-NLS-1$
            }

            if ((cNdx % 2) == 1) {
                b = (byte)((b * 16) + (byte)(cVal));
                baos.write(b);
            } else {
                b = (byte)(cVal);
            }
            cNdx++;
        }

        // throw exception if no hex digits
        if (cNdx == 0) {
            throw new IOException("AVA parse, zero hex digits"); //$NON-NLS-1$
        }

        // throw exception if odd number of hex digits
        if (cNdx % 2 == 1) {
            throw new IOException("AVA parse, odd number of hex digits"); //$NON-NLS-1$
        }

        return new DerValue(baos.toByteArray());
    }

    private DerValue parseQuotedString
        (final Reader in, final StringBuilder temp) throws IOException {

        // RFC1779 specifies that an entire RDN may be enclosed in double
        // quotes. In this case the syntax is any sequence of
        // backslash-specialChar, backslash-backslash,
        // backslash-doublequote, or character other than backslash or
        // doublequote.
        int c = readChar(in, "Quoted string did not end in quote"); //$NON-NLS-1$

        final List<Byte> embeddedHex = new ArrayList<Byte>();
        boolean isPrintableString = true;
        while (c != '"') {
            if (c == '\\') {
                c = readChar(in, "Quoted string did not end in quote"); //$NON-NLS-1$

                // check for embedded hex pairs
                Byte hexByte = null;
                if ((hexByte = getEmbeddedHexPair(c, in)) != null) {

                    // always encode AVAs with embedded hex as UTF8
                    isPrintableString = false;

                    // append consecutive embedded hex
                    // as single string later
                    embeddedHex.add(hexByte);
                    c = in.read();
                    continue;
                }

                if (specialChars1779.indexOf((char)c) < 0) {
                    throw new IOException
                        ("Invalid escaped character in AVA: " + //$NON-NLS-1$
                        (char)c);
                }
            }

            // add embedded hex bytes before next char
            if (embeddedHex.size() > 0) {
                final String hexString = getEmbeddedHexString(embeddedHex);
                temp.append(hexString);
                embeddedHex.clear();
            }

            // check for non-PrintableString chars
            isPrintableString &= DerValue.isPrintableStringChar((char)c);
            temp.append((char)c);
            c = readChar(in, "Quoted string did not end in quote"); //$NON-NLS-1$
        }

        // add trailing embedded hex bytes
        if (embeddedHex.size() > 0) {
            final String hexString = getEmbeddedHexString(embeddedHex);
            temp.append(hexString);
            embeddedHex.clear();
        }

        do {
            c = in.read();
        } while ((c == '\n') || (c == ' '));
        if (c != -1) {
            throw new IOException("AVA had characters other than " //$NON-NLS-1$
                    + "whitespace after terminating quote"); //$NON-NLS-1$
        }

        // encode as PrintableString unless value contains
        // non-PrintableString chars
        if (this.oid.equals((Object)PKCS9Attribute.EMAIL_ADDRESS_OID) ||
            (this.oid.equals((Object)X500Name.DOMAIN_COMPONENT_OID) &&
                PRESERVE_OLD_DC_ENCODING == false)) {
            // EmailAddress and DomainComponent must be IA5String
            return new DerValue(DerValue.tag_IA5String,
                                        temp.toString().trim());
        } else if (isPrintableString) {
            return new DerValue(temp.toString().trim());
        } else {
            return new DerValue(DerValue.tag_UTF8String,
                                        temp.toString().trim());
        }
    }

    private DerValue parseString
        (final Reader in, int c, final int format, final StringBuilder temp) throws IOException {

        final List<Byte> embeddedHex = new ArrayList<>();
        boolean isPrintableString = true;
        boolean escape = false;
        boolean leadingChar = true;
        int spaceCount = 0;
        do {
            escape = false;
            if (c == '\\') {
                escape = true;
                c = readChar(in, "Invalid trailing backslash"); //$NON-NLS-1$

                // check for embedded hex pairs
                Byte hexByte = null;
                if ((hexByte = getEmbeddedHexPair(c, in)) != null) {

                    // always encode AVAs with embedded hex as UTF8
                    isPrintableString = false;

                    // append consecutive embedded hex
                    // as single string later
                    embeddedHex.add(hexByte);
                    c = in.read();
                    leadingChar = false;
                    continue;
                }

                // check if character was improperly escaped
                if (format == DEFAULT &&
                       specialCharsDefault.indexOf((char)c) == -1) {
                    throw new IOException
                        ("Invalid escaped character in AVA: '" + //$NON-NLS-1$
                        (char)c + "'"); //$NON-NLS-1$
                } else if (format == RFC2253) {
                    if (c == ' ') {
                        // only leading/trailing space can be escaped
                        if (!leadingChar && !trailingSpace(in)) {
                            throw new IOException
                                    ("Invalid escaped space character " + //$NON-NLS-1$
                                    "in AVA.  Only a leading or trailing " + //$NON-NLS-1$
                                    "space character can be escaped."); //$NON-NLS-1$
                        }
                    } else if (c == '#') {
                        // only leading '#' can be escaped
                        if (!leadingChar) {
                            throw new IOException
                                ("Invalid escaped '#' character in AVA.  " + //$NON-NLS-1$
                                "Only a leading '#' can be escaped."); //$NON-NLS-1$
                        }
                    } else if (specialChars2253.indexOf((char)c) == -1) {
                        throw new IOException
                                ("Invalid escaped character in AVA: '" + //$NON-NLS-1$
                                (char)c + "'"); //$NON-NLS-1$
                    }
                }
            } else {
                // check if character should have been escaped
                if (format == RFC2253) {
                    if (specialChars2253.indexOf((char)c) != -1) {
                        throw new IOException
                                ("Character '" + (char)c + //$NON-NLS-1$
                                 "' in AVA appears without escape"); //$NON-NLS-1$
                    }
                } else if (escapedDefault.indexOf((char)c) != -1) {
                    throw new IOException
                            ("Character '" + (char)c + //$NON-NLS-1$
                            "' in AVA appears without escape"); //$NON-NLS-1$
                }
            }

            // add embedded hex bytes before next char
            if (embeddedHex.size() > 0) {
                // add space(s) before embedded hex bytes
                for (int i = 0; i < spaceCount; i++) {
                    temp.append(" "); //$NON-NLS-1$
                }
                spaceCount = 0;

                final String hexString = getEmbeddedHexString(embeddedHex);
                temp.append(hexString);
                embeddedHex.clear();
            }

            // check for non-PrintableString chars
            isPrintableString &= DerValue.isPrintableStringChar((char)c);
            if (c == ' ' && escape == false) {
                // do not add non-escaped spaces yet
                // (non-escaped trailing spaces are ignored)
                spaceCount++;
            } else {
                // add space(s)
                for (int i = 0; i < spaceCount; i++) {
                    temp.append(" "); //$NON-NLS-1$
                }
                spaceCount = 0;
                temp.append((char)c);
            }
            c = in.read();
            leadingChar = false;
        } while (isTerminator(c, format) == false);

        if (format == RFC2253 && spaceCount > 0) {
            throw new IOException("Incorrect AVA RFC2253 format - " + //$NON-NLS-1$
                                        "trailing space must be escaped"); //$NON-NLS-1$
        }

        // add trailing embedded hex bytes
        if (embeddedHex.size() > 0) {
            final String hexString = getEmbeddedHexString(embeddedHex);
            temp.append(hexString);
            embeddedHex.clear();
        }

        // encode as PrintableString unless value contains
        // non-PrintableString chars
        if (this.oid.equals((Object)PKCS9Attribute.EMAIL_ADDRESS_OID) ||
            (this.oid.equals((Object)X500Name.DOMAIN_COMPONENT_OID) &&
                PRESERVE_OLD_DC_ENCODING == false)) {
            // EmailAddress and DomainComponent must be IA5String
            return new DerValue(DerValue.tag_IA5String, temp.toString());
        } else if (isPrintableString) {
            return new DerValue(temp.toString());
        } else {
            return new DerValue(DerValue.tag_UTF8String, temp.toString());
        }
    }

    private static Byte getEmbeddedHexPair(final int c1, final Reader in)
        throws IOException {

        if (hexDigits.indexOf(Character.toUpperCase((char)c1)) >= 0) {
            final int c2 = readChar(in, "unexpected EOF - " + //$NON-NLS-1$
                        "escaped hex value must include two valid digits"); //$NON-NLS-1$

            if (hexDigits.indexOf(Character.toUpperCase((char)c2)) >= 0) {
                final int hi = Character.digit((char)c1, 16);
                final int lo = Character.digit((char)c2, 16);
                return new Byte((byte)((hi<<4) + lo));
            } else {
                throw new IOException
                        ("escaped hex value must include two valid digits"); //$NON-NLS-1$
            }
        }
        return null;
    }

    private static String getEmbeddedHexString(final List<Byte> hexList)
                                                throws IOException {
        final int n = hexList.size();
        final byte[] hexBytes = new byte[n];
        for (int i = 0; i < n; i++) {
                hexBytes[i] = hexList.get(i).byteValue();
        }
        return new String(hexBytes, "UTF8"); //$NON-NLS-1$
    }

    private static boolean isTerminator(final int ch, final int format) {
        switch (ch) {
        case -1:
        case '+':
        case ',':
            return true;
        case ';':
            return format != RFC2253;
        default:
            return false;
        }
    }

    private static int readChar(final Reader in, final String errMsg) throws IOException {
        final int c = in.read();
        if (c == -1) {
            throw new IOException(errMsg);
        }
        return c;
    }

    private static boolean trailingSpace(final Reader in) throws IOException {

        boolean trailing = false;

        if (!in.markSupported()) {
            // oh well
            return true;
        }
		// make readAheadLimit huge -
		// in practice, AVA was passed a StringReader from X500Name,
		// and StringReader ignores readAheadLimit anyways
		in.mark(9999);
		while (true) {
		    final int nextChar = in.read();
		    if (nextChar == -1) {
		        trailing = true;
		        break;
		    } else if (nextChar == ' ') {
		        continue;
		    } else if (nextChar == '\\') {
		        final int followingChar = in.read();
		        if (followingChar != ' ') {
		            trailing = false;
		            break;
		        }
		    } else {
		        trailing = false;
		        break;
		    }
		}

		in.reset();
		return trailing;
    }

    AVA(final DerValue derval) throws IOException {
        // Individual attribute value assertions are SEQUENCE of two values.
        // That'd be a "struct" outside of ASN.1.
        if (derval.tag != DerValue.tag_Sequence) {
            throw new IOException("AVA not a sequence"); //$NON-NLS-1$
        }
        this.oid = X500Name.intern(derval.data.getOID());
        this.value = derval.data.getDerValue();

        if (derval.data.available() != 0) {
            throw new IOException("AVA, extra bytes = " //$NON-NLS-1$
                + derval.data.available());
        }
    }

    AVA(final DerInputStream in) throws IOException {
        this(in.getDerValue());
    }

    @Override
	public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof AVA == false) {
            return false;
        }
        final AVA other = (AVA)obj;
        return this.toRFC2253CanonicalString().equals
                                (other.toRFC2253CanonicalString());
    }

    /**
     * Returns a hashcode for this AVA.
     *
     * @return a hashcode for this AVA.
     */
    @Override
	public int hashCode() {
        return toRFC2253CanonicalString().hashCode();
    }

    /*
     * AVAs are encoded as a SEQUENCE of two elements.
     */
    public void encode(final DerOutputStream out) throws IOException {
        derEncode(out);
    }

    /**
     * DER encode this object onto an output stream.
     * Implements the <code>DerEncoder</code> interface.
     *
     * @param out
     * the output stream on which to write the DER encoding.
     *
     * @exception IOException on encoding error.
     */
    @Override
	public void derEncode(final OutputStream out) throws IOException {
        final DerOutputStream         tmp = new DerOutputStream();
        final DerOutputStream         tmp2 = new DerOutputStream();

        tmp.putOID(this.oid);
        this.value.encode(tmp);
        tmp2.write(DerValue.tag_Sequence, tmp);
        out.write(tmp2.toByteArray());
    }

    private String toKeyword(final int format, final Map<String, String> oidMap) {
        return AVAKeyword.getKeyword(this.oid, format, oidMap);
    }

    /**
     * Returns a printable form of this attribute, using RFC 1779
     * syntax for individual attribute/value assertions.
     */
    @Override
	public String toString() {
        return toKeywordValueString
            (toKeyword(DEFAULT, Collections.<String, String>emptyMap()));
    }

    /**
     * Returns a printable form of this attribute, using RFC 1779
     * syntax for individual attribute/value assertions. It only
     * emits standardised keywords.
     */
    public String toRFC1779String() {
        return toRFC1779String(Collections.<String, String>emptyMap());
    }

    /**
     * Returns a printable form of this attribute, using RFC 1779
     * syntax for individual attribute/value assertions. It
     * emits standardised keywords, as well as keywords contained in the
     * OID/keyword map.
     */
    public String toRFC1779String(final Map<String, String> oidMap) {
        return toKeywordValueString(toKeyword(RFC1779, oidMap));
    }

    /**
     * Returns a printable form of this attribute, using RFC 2253
     * syntax for individual attribute/value assertions. It only
     * emits standardised keywords.
     */
    public String toRFC2253String() {
        return toRFC2253String(Collections.<String, String>emptyMap());
    }

    /**
     * Returns a printable form of this attribute, using RFC 2253
     * syntax for individual attribute/value assertions. It
     * emits standardised keywords, as well as keywords contained in the
     * OID/keyword map.
     */
    public String toRFC2253String(final Map<String, String> oidMap) {
        /*
         * Section 2.3: The AttributeTypeAndValue is encoded as the string
         * representation of the AttributeType, followed by an equals character
         * ('=' ASCII 61), followed by the string representation of the
         * AttributeValue. The encoding of the AttributeValue is given in
         * section 2.4.
         */
        final StringBuilder typeAndValue = new StringBuilder(100);
        typeAndValue.append(toKeyword(RFC2253, oidMap));
        typeAndValue.append('=');

        /*
         * Section 2.4: Converting an AttributeValue from ASN.1 to a String.
         * If the AttributeValue is of a type which does not have a string
         * representation defined for it, then it is simply encoded as an
         * octothorpe character ('#' ASCII 35) followed by the hexadecimal
         * representation of each of the bytes of the BER encoding of the X.500
         * AttributeValue.  This form SHOULD be used if the AttributeType is of
         * the dotted-decimal form.
         */
        if ((typeAndValue.charAt(0) >= '0' && typeAndValue.charAt(0) <= '9') ||
            !isDerString(this.value, false))
        {
            byte[] data = null;
            try {
                data = this.value.toByteArray();
            } catch (final IOException ie) {
                throw new IllegalArgumentException("DER Value conversion"); //$NON-NLS-1$
            }
            typeAndValue.append('#');
            for (final byte b : data) {
                typeAndValue.append(Character.forDigit(0xF & (b >>> 4), 16));
                typeAndValue.append(Character.forDigit(0xF & b, 16));
            }
        } else {
            /*
             * 2.4 (cont): Otherwise, if the AttributeValue is of a type which
             * has a string representation, the value is converted first to a
             * UTF-8 string according to its syntax specification.
             *
             * NOTE: this implementation only emits DirectoryStrings of the
             * types returned by isDerString().
             */
            String valStr = null;
            try {
                valStr = new String(this.value.getDataBytes(), "UTF8"); //$NON-NLS-1$
            } catch (final IOException ie) {
                throw new IllegalArgumentException("DER Value conversion"); //$NON-NLS-1$
            }

            /*
             * 2.4 (cont): If the UTF-8 string does not have any of the
             * following characters which need escaping, then that string can be
             * used as the string representation of the value.
             *
             *   o   a space or "#" character occurring at the beginning of the
             *       string
             *   o   a space character occurring at the end of the string
             *   o   one of the characters ",", "+", """, "\", "<", ">" or ";"
             *
             * Implementations MAY escape other characters.
             *
             * NOTE: this implementation also recognizes "=" and "#" as
             * characters which need escaping, and null which is escaped as
             * '\00' (see RFC 4514).
             *
             * If a character to be escaped is one of the list shown above, then
             * it is prefixed by a backslash ('\' ASCII 92).
             *
             * Otherwise the character to be escaped is replaced by a backslash
             * and two hex digits, which form a single byte in the code of the
             * character.
             */
            final String escapees = ",=+<>#;\"\\"; //$NON-NLS-1$
            StringBuilder sbuffer = new StringBuilder();

            for (int i = 0; i < valStr.length(); i++) {
                final char c = valStr.charAt(i);
                if (DerValue.isPrintableStringChar(c) ||
                    escapees.indexOf(c) >= 0) {

                    // escape escapees
                    if (escapees.indexOf(c) >= 0) {
                        sbuffer.append('\\');
                    }

                    // append printable/escaped char
                    sbuffer.append(c);

                } else if (c == '\u0000') {
                    // escape null character
                    sbuffer.append("\\00"); //$NON-NLS-1$

                } else if (debug != null && Debug.isOn("ava")) { //$NON-NLS-1$

                    // embed non-printable/non-escaped char
                    // as escaped hex pairs for debugging
                    byte[] valueBytes = null;
                    try {
                        valueBytes = Character.toString(c).getBytes("UTF8"); //$NON-NLS-1$
                    } catch (final IOException ie) {
                        throw new IllegalArgumentException
                                        ("DER Value conversion"); //$NON-NLS-1$
                    }
                    for (final byte valueByte : valueBytes) {
                        sbuffer.append('\\');
                        char hexChar = Character.forDigit
                                (0xF & (valueByte >>> 4), 16);
                        sbuffer.append(Character.toUpperCase(hexChar));
                        hexChar = Character.forDigit
                                (0xF & (valueByte), 16);
                        sbuffer.append(Character.toUpperCase(hexChar));
                    }
                } else {

                    // append non-printable/non-escaped char
                    sbuffer.append(c);
                }
            }

            final char[] chars = sbuffer.toString().toCharArray();
            sbuffer = new StringBuilder();

            // Find leading and trailing whitespace.
            int lead;   // index of first char that is not leading whitespace
            for (lead = 0; lead < chars.length; lead++) {
                if (chars[lead] != ' ' && chars[lead] != '\r') {
                    break;
                }
            }
            int trail;  // index of last char that is not trailing whitespace
            for (trail = chars.length - 1; trail >= 0; trail--) {
                if (chars[trail] != ' ' && chars[trail] != '\r') {
                    break;
                }
            }

            // escape leading and trailing whitespace
            for (int i = 0; i < chars.length; i++) {
                final char c = chars[i];
                if (i < lead || i > trail) {
                    sbuffer.append('\\');
                }
                sbuffer.append(c);
            }
            typeAndValue.append(sbuffer.toString());
        }
        return typeAndValue.toString();
    }

    public String toRFC2253CanonicalString() {
        /*
         * Section 2.3: The AttributeTypeAndValue is encoded as the string
         * representation of the AttributeType, followed by an equals character
         * ('=' ASCII 61), followed by the string representation of the
         * AttributeValue. The encoding of the AttributeValue is given in
         * section 2.4.
         */
        final StringBuilder typeAndValue = new StringBuilder(40);
        typeAndValue.append
            (toKeyword(RFC2253, Collections.<String, String>emptyMap()));
        typeAndValue.append('=');

        /*
         * Section 2.4: Converting an AttributeValue from ASN.1 to a String.
         * If the AttributeValue is of a type which does not have a string
         * representation defined for it, then it is simply encoded as an
         * octothorpe character ('#' ASCII 35) followed by the hexadecimal
         * representation of each of the bytes of the BER encoding of the X.500
         * AttributeValue.  This form SHOULD be used if the AttributeType is of
         * the dotted-decimal form.
         */
        if ((typeAndValue.charAt(0) >= '0' && typeAndValue.charAt(0) <= '9') ||
            !isDerString(this.value, true))
        {
            byte[] data = null;
            try {
                data = this.value.toByteArray();
            } catch (final IOException ie) {
                throw new IllegalArgumentException("DER Value conversion"); //$NON-NLS-1$
            }
            typeAndValue.append('#');
            for (final byte b : data) {
                typeAndValue.append(Character.forDigit(0xF & (b >>> 4), 16));
                typeAndValue.append(Character.forDigit(0xF & b, 16));
            }
        } else {
            /*
             * 2.4 (cont): Otherwise, if the AttributeValue is of a type which
             * has a string representation, the value is converted first to a
             * UTF-8 string according to its syntax specification.
             *
             * NOTE: this implementation only emits DirectoryStrings of the
             * types returned by isDerString().
             */
            String valStr = null;
            try {
                valStr = new String(this.value.getDataBytes(), "UTF8"); //$NON-NLS-1$
            } catch (final IOException ie) {
                throw new IllegalArgumentException("DER Value conversion"); //$NON-NLS-1$
            }

            /*
             * 2.4 (cont): If the UTF-8 string does not have any of the
             * following characters which need escaping, then that string can be
             * used as the string representation of the value.
             *
             *   o   a space or "#" character occurring at the beginning of the
             *       string
             *   o   a space character occurring at the end of the string
             *
             *   o   one of the characters ",", "+", """, "\", "<", ">" or ";"
             *
             * If a character to be escaped is one of the list shown above, then
             * it is prefixed by a backslash ('\' ASCII 92).
             *
             * Otherwise the character to be escaped is replaced by a backslash
             * and two hex digits, which form a single byte in the code of the
             * character.
             */
            final String escapees = ",+<>;\"\\"; //$NON-NLS-1$
            final StringBuilder sbuffer = new StringBuilder();
            boolean previousWhite = false;

            for (int i = 0; i < valStr.length(); i++) {
                final char c = valStr.charAt(i);

                if (DerValue.isPrintableStringChar(c) ||
                    escapees.indexOf(c) >= 0 ||
                    (i == 0 && c == '#')) {

                    // escape leading '#' and escapees
                    if ((i == 0 && c == '#') || escapees.indexOf(c) >= 0) {
                        sbuffer.append('\\');
                    }

                    // convert multiple whitespace to single whitespace
                    if (!Character.isWhitespace(c)) {
                        previousWhite = false;
                        sbuffer.append(c);
                    } else {
                        if (previousWhite == false) {
                            // add single whitespace
                            previousWhite = true;
                            sbuffer.append(c);
                        } else {
                            // ignore subsequent consecutive whitespace
                            continue;
                        }
                    }

                } else if (debug != null && Debug.isOn("ava")) { //$NON-NLS-1$

                    // embed non-printable/non-escaped char
                    // as escaped hex pairs for debugging

                    previousWhite = false;

                    byte valueBytes[] = null;
                    try {
                        valueBytes = Character.toString(c).getBytes("UTF8"); //$NON-NLS-1$
                    } catch (final IOException ie) {
                        throw new IllegalArgumentException
                                        ("DER Value conversion"); //$NON-NLS-1$
                    }
                    for (final byte valueByte : valueBytes) {
                        sbuffer.append('\\');
                        sbuffer.append(Character.forDigit
                                        (0xF & (valueByte >>> 4), 16));
                        sbuffer.append(Character.forDigit
                                        (0xF & (valueByte), 16));
                    }
                } else {

                    // append non-printable/non-escaped char

                    previousWhite = false;
                    sbuffer.append(c);
                }
            }

            // remove leading and trailing whitespace from value
            typeAndValue.append(sbuffer.toString().trim());
        }

        String canon = typeAndValue.toString();
        canon = canon.toUpperCase(Locale.US).toLowerCase(Locale.US);
        return Normalizer.normalize(canon, Normalizer.Form.NFKD);
    }

    /*
     * Return true if DerValue can be represented as a String.
     */
    private static boolean isDerString(final DerValue value, final boolean canonical) {
        if (canonical) {
            switch (value.tag) {
                case DerValue.tag_PrintableString:
                case DerValue.tag_UTF8String:
                    return true;
                default:
                    return false;
            }
        } else {
            switch (value.tag) {
                case DerValue.tag_PrintableString:
                case DerValue.tag_T61String:
                case DerValue.tag_IA5String:
                case DerValue.tag_GeneralString:
                case DerValue.tag_BMPString:
                case DerValue.tag_UTF8String:
                    return true;
                default:
                    return false;
            }
        }
    }

    boolean hasRFC2253Keyword() {
        return AVAKeyword.hasKeyword(this.oid, RFC2253);
    }

    private String toKeywordValueString(final String keyword) {
        /*
         * Construct the value with as little copying and garbage
         * production as practical.  First the keyword (mandatory),
         * then the equals sign, finally the value.
         */
        final StringBuilder   retval = new StringBuilder(40);

        retval.append(keyword);
        retval.append("="); //$NON-NLS-1$

        try {
            final String valStr = this.value.getAsString();

            if (valStr == null) {

                // rfc1779 specifies that attribute values associated
                // with non-standard keyword attributes may be represented
                // using the hex format below.  This will be used only
                // when the value is not a string type

                final byte    data [] = this.value.toByteArray();

                retval.append('#');
                for (final byte element : data) {
                    retval.append(hexDigits.charAt((element >> 4) & 0x0f));
                    retval.append(hexDigits.charAt(element & 0x0f));
                }

            } else {

                boolean quoteNeeded = false;
                final StringBuilder sbuffer = new StringBuilder();
                boolean previousWhite = false;
                final String escapees = ",+=\n<>#;\\\""; //$NON-NLS-1$

                /*
                 * Special characters (e.g. AVA list separators) cause strings
                 * to need quoting, or at least escaping.  So do leading or
                 * trailing spaces, and multiple internal spaces.
                 */
                final int length = valStr.length();
                final boolean alreadyQuoted =
                    (length > 1 && valStr.charAt(0) == '\"'
                     && valStr.charAt(length - 1) == '\"');

                for (int i = 0; i < length; i++) {
                    final char c = valStr.charAt(i);
                    if (alreadyQuoted && (i == 0 || i == length - 1)) {
                        sbuffer.append(c);
                        continue;
                    }
                    if (DerValue.isPrintableStringChar(c) ||
                        escapees.indexOf(c) >= 0) {

                        // quote if leading whitespace or special chars
                        if (!quoteNeeded &&
                            ((i == 0 && (c == ' ' || c == '\n')) ||
                                escapees.indexOf(c) >= 0)) {
                            quoteNeeded = true;
                        }

                        // quote if multiple internal whitespace
                        if (!(c == ' ' || c == '\n')) {
                            // escape '"' and '\'
                            if (c == '"' || c == '\\') {
                                sbuffer.append('\\');
                            }
                            previousWhite = false;
                        } else {
                            if (!quoteNeeded && previousWhite) {
                                quoteNeeded = true;
                            }
                            previousWhite = true;
                        }

                        sbuffer.append(c);

                    } else if (debug != null && Debug.isOn("ava")) { //$NON-NLS-1$

                        // embed non-printable/non-escaped char
                        // as escaped hex pairs for debugging

                        previousWhite = false;

                        // embed escaped hex pairs
                        final byte[] valueBytes =
                                Character.toString(c).getBytes("UTF8"); //$NON-NLS-1$
                        for (final byte valueByte : valueBytes) {
                            sbuffer.append('\\');
                            char hexChar = Character.forDigit
                                        (0xF & (valueByte >>> 4), 16);
                            sbuffer.append(Character.toUpperCase(hexChar));
                            hexChar = Character.forDigit
                                        (0xF & (valueByte), 16);
                            sbuffer.append(Character.toUpperCase(hexChar));
                        }
                    } else {

                        // append non-printable/non-escaped char

                        previousWhite = false;
                        sbuffer.append(c);
                    }
                }

                // quote if trailing whitespace
                if (sbuffer.length() > 0) {
                    final char trailChar = sbuffer.charAt(sbuffer.length() - 1);
                    if (trailChar == ' ' || trailChar == '\n') {
                        quoteNeeded = true;
                    }
                }

                // Emit the string ... quote it if needed
                // if string is already quoted, don't re-quote
                if (!alreadyQuoted && quoteNeeded) {
                    retval.append("\"" + sbuffer.toString() + "\""); //$NON-NLS-1$ //$NON-NLS-2$
                } else {
                    retval.append(sbuffer.toString());
                }
            }
        } catch (final IOException e) {
            throw new IllegalArgumentException("DER Value conversion"); //$NON-NLS-1$
        }

        return retval.toString();
    }

}

/**
 * Helper class that allows conversion from String to ObjectIdentifier and
 * vice versa according to RFC1779, RFC2253, and an augmented version of
 * those standards.
 */
class AVAKeyword {

    private static final Map<ObjectIdentifier,AVAKeyword> oidMap;
    private static final Map<String,AVAKeyword> keywordMap;

    private final String keyword;
    private final ObjectIdentifier oid;
    private final boolean rfc1779Compliant, rfc2253Compliant;

    private AVAKeyword(final String keyword, final ObjectIdentifier oid,
               final boolean rfc1779Compliant, final boolean rfc2253Compliant) {
        this.keyword = keyword;
        this.oid = oid;
        this.rfc1779Compliant = rfc1779Compliant;
        this.rfc2253Compliant = rfc2253Compliant;

        // register it
        oidMap.put(oid, this);
        keywordMap.put(keyword, this);
    }

    private boolean isCompliant(final int standard) {
        switch (standard) {
        case AVA.RFC1779:
            return this.rfc1779Compliant;
        case AVA.RFC2253:
            return this.rfc2253Compliant;
        case AVA.DEFAULT:
            return true;
        default:
            // should not occur, internal error
            throw new IllegalArgumentException("Invalid standard " + standard); //$NON-NLS-1$
        }
    }

    /**
     * Get an object identifier representing the specified keyword (or
     * string encoded object identifier) in the given standard.
     *
     * @param keywordMap a Map where a keyword String maps to a corresponding
     *   OID String. Each AVA keyword will be mapped to the corresponding OID.
     *   If an entry does not exist, it will fallback to the builtin
     *   keyword/OID mapping.
     * @throws IOException If the keyword is not valid in the specified standard
     *   or the OID String to which a keyword maps to is improperly formatted.
     */
    static ObjectIdentifier getOID
        (String keyword, final int standard, final Map<String, String> extraKeywordMap)
            throws IOException {

        keyword = keyword.toUpperCase(Locale.ENGLISH);
        if (standard == AVA.RFC2253) {
            if (keyword.startsWith(" ") || keyword.endsWith(" ")) { //$NON-NLS-1$ //$NON-NLS-2$
                throw new IOException("Invalid leading or trailing space " + //$NON-NLS-1$
                        "in keyword \"" + keyword + "\""); //$NON-NLS-1$ //$NON-NLS-2$
            }
        } else {
            keyword = keyword.trim();
        }

        // check user-specified keyword map first, then fallback to built-in
        // map
        final String oidString = extraKeywordMap.get(keyword);
        if (oidString == null) {
            final AVAKeyword ak = keywordMap.get(keyword);
            if ((ak != null) && ak.isCompliant(standard)) {
                return ak.oid;
            }
        } else {
            return new ObjectIdentifier(oidString);
        }

        // no keyword found, check if OID string
        if (standard == AVA.DEFAULT && keyword.startsWith("OID.")) { //$NON-NLS-1$
            keyword = keyword.substring(4);
        }

        boolean number = false;
        if (keyword.length() != 0) {
            final char ch = keyword.charAt(0);
            if ((ch >= '0') && (ch <= '9')) {
                number = true;
            }
        }
        if (number == false) {
            throw new IOException("Invalid keyword \"" + keyword + "\""); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return new ObjectIdentifier(keyword);
    }

    /**
     * Get a keyword for the given ObjectIdentifier according to standard.
     * If no keyword is available, the ObjectIdentifier is encoded as a
     * String.
     */
    static String getKeyword(final ObjectIdentifier oid, final int standard) {
        return getKeyword
            (oid, standard, Collections.<String, String>emptyMap());
    }

    /**
     * Get a keyword for the given ObjectIdentifier according to standard.
     * Checks the extraOidMap for a keyword first, then falls back to the
     * builtin/default set. If no keyword is available, the ObjectIdentifier
     * is encoded as a String.
     */
    static String getKeyword
        (final ObjectIdentifier oid, final int standard, final Map<String, String> extraOidMap) {

        // check extraOidMap first, then fallback to built-in map
        final String oidString = oid.toString();
        String keywordString = extraOidMap.get(oidString);
        if (keywordString == null) {
            final AVAKeyword ak = oidMap.get(oid);
            if ((ak != null) && ak.isCompliant(standard)) {
                return ak.keyword;
            }
        } else {
            if (keywordString.length() == 0) {
                throw new IllegalArgumentException("keyword cannot be empty"); //$NON-NLS-1$
            }
            keywordString = keywordString.trim();
            char c = keywordString.charAt(0);
            if (c < 65 || c > 122 || (c > 90 && c < 97)) {
                throw new IllegalArgumentException
                    ("keyword does not start with letter"); //$NON-NLS-1$
            }
            for (int i=1; i<keywordString.length(); i++) {
                c = keywordString.charAt(i);
                if ((c < 65 || c > 122 || (c > 90 && c < 97)) &&
                    (c < 48 || c > 57) && c != '_') {
                    throw new IllegalArgumentException
                    ("keyword character is not a letter, digit, or underscore"); //$NON-NLS-1$
                }
            }
            return keywordString;
        }
        // no compliant keyword, use OID
        if (standard == AVA.RFC2253) {
            return oidString;
        } else {
            return "OID." + oidString; //$NON-NLS-1$
        }
    }

    /**
     * Test if oid has an associated keyword in standard.
     */
    static boolean hasKeyword(final ObjectIdentifier oid, final int standard) {
        final AVAKeyword ak = oidMap.get(oid);
        if (ak == null) {
            return false;
        }
        return ak.isCompliant(standard);
    }

    static {
        oidMap = new HashMap<ObjectIdentifier,AVAKeyword>();
        keywordMap = new HashMap<String,AVAKeyword>();

        // NOTE if multiple keywords are available for one OID, order
        // is significant!! Preferred *LAST*.
        new AVAKeyword("CN",           X500Name.commonName_oid,   true,  true); //$NON-NLS-1$
        new AVAKeyword("C",            X500Name.countryName_oid,  true,  true); //$NON-NLS-1$
        new AVAKeyword("L",            X500Name.localityName_oid, true,  true); //$NON-NLS-1$
        new AVAKeyword("S",            X500Name.stateName_oid,    false, false); //$NON-NLS-1$
        new AVAKeyword("ST",           X500Name.stateName_oid,    true,  true); //$NON-NLS-1$
        new AVAKeyword("O",            X500Name.orgName_oid,      true,  true); //$NON-NLS-1$
        new AVAKeyword("OU",           X500Name.orgUnitName_oid,  true,  true); //$NON-NLS-1$
        new AVAKeyword("T",            X500Name.title_oid,        false, false); //$NON-NLS-1$
        new AVAKeyword("IP",           X500Name.ipAddress_oid,    false, false); //$NON-NLS-1$
        new AVAKeyword("STREET",       X500Name.streetAddress_oid,true,  true); //$NON-NLS-1$
        new AVAKeyword("DC",           X500Name.DOMAIN_COMPONENT_OID, //$NON-NLS-1$
                                                                  false, true);
        new AVAKeyword("DNQUALIFIER",  X500Name.DNQUALIFIER_OID,  false, false); //$NON-NLS-1$
        new AVAKeyword("DNQ",          X500Name.DNQUALIFIER_OID,  false, false); //$NON-NLS-1$
        new AVAKeyword("SURNAME",      X500Name.SURNAME_OID,      false, false); //$NON-NLS-1$
        new AVAKeyword("GIVENNAME",    X500Name.GIVENNAME_OID,    false, false); //$NON-NLS-1$
        new AVAKeyword("INITIALS",     X500Name.INITIALS_OID,     false, false); //$NON-NLS-1$
        new AVAKeyword("GENERATION",   X500Name.GENERATIONQUALIFIER_OID, //$NON-NLS-1$
                                                                  false, false);
        new AVAKeyword("EMAIL", PKCS9Attribute.EMAIL_ADDRESS_OID, false, false); //$NON-NLS-1$
        new AVAKeyword("EMAILADDRESS", PKCS9Attribute.EMAIL_ADDRESS_OID, //$NON-NLS-1$
                                                                  false, false);
        new AVAKeyword("UID",          X500Name.userid_oid,       false, true); //$NON-NLS-1$
        new AVAKeyword("SERIALNUMBER", X500Name.SERIALNUMBER_OID, false, false); //$NON-NLS-1$
    }
}
