/*
 * Copyright (c) 2002, 2011, Oracle and/or its affiliates. All rights reserved.
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

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;

/**
 * RDNs are a set of {attribute = value} assertions.  Some of those
 * attributes are "distinguished" (unique w/in context).  Order is
 * never relevant.
 *
 * Some X.500 names include only a single distinguished attribute
 * per RDN.  This style is currently common.
 *
 * Note that DER-encoded RDNs sort AVAs by assertion OID ... so that
 * when we parse this data we don't have to worry about canonicalizing
 * it, but we'll need to sort them when we expose the RDN class more.
 * <p>
 * The ASN.1 for RDNs is:
 * <pre>
 * RelativeDistinguishedName ::=
 *   SET OF AttributeTypeAndValue
 *
 * AttributeTypeAndValue ::= SEQUENCE {
 *   type     AttributeType,
 *   value    AttributeValue }
 *
 * AttributeType ::= OBJECT IDENTIFIER
 *
 * AttributeValue ::= ANY DEFINED BY AttributeType
 * </pre>
 *
 * Note that instances of this class are immutable.
 *
 */
public class RDN {

    // currently not private, accessed directly from X500Name
    final AVA[] assertion;

    // cached immutable List of the AVAs
    private volatile List<AVA> avaList;

    // cache canonical String form
    private volatile String canonicalString;

    /**
     * Constructs an RDN from its printable representation.
     *
     * An RDN may consist of one or multiple Attribute Value Assertions (AVAs),
     * using '+' as a separator.
     * If the '+' should be considered part of an AVA value, it must be
     * preceded by '\'.
     *
     * @param name String form of RDN
     * @throws IOException on parsing error
     */
    public RDN(final String name) throws IOException {
        this(name, Collections.<String, String>emptyMap());
    }

    /**
     * Constructs an RDN from its printable representation.
     *
     * An RDN may consist of one or multiple Attribute Value Assertions (AVAs),
     * using '+' as a separator.
     * If the '+' should be considered part of an AVA value, it must be
     * preceded by '\'.
     *
     * @param name String form of RDN
     * @param keyword an additional mapping of keywords to OIDs
     * @throws IOException on parsing error
     */
    public RDN(final String name, final Map<String, String> keywordMap) throws IOException {
        int quoteCount = 0;
        int searchOffset = 0;
        int avaOffset = 0;
        final List<AVA> avaVec = new ArrayList<AVA>(3);
        int nextPlus = name.indexOf('+');
        while (nextPlus >= 0) {
            quoteCount += X500Name.countQuotes(name, searchOffset, nextPlus);
            /*
             * We have encountered an AVA delimiter (plus sign).
             * If the plus sign in the RDN under consideration is
             * preceded by a backslash (escape), or by a double quote, it
             * is part of the AVA. Otherwise, it is used as a separator, to
             * delimit the AVA under consideration from any subsequent AVAs.
             */
            if (nextPlus > 0 && name.charAt(nextPlus - 1) != '\\'
                && quoteCount != 1) {
                /*
                 * Plus sign is a separator
                 */
                final String avaString = name.substring(avaOffset, nextPlus);
                if (avaString.length() == 0) {
                    throw new IOException("empty AVA in RDN \"" + name + "\""); //$NON-NLS-1$ //$NON-NLS-2$
                }

                // Parse AVA, and store it in vector
                final AVA ava = new AVA(new StringReader(avaString), keywordMap);
                avaVec.add(ava);

                // Increase the offset
                avaOffset = nextPlus + 1;

                // Set quote counter back to zero
                quoteCount = 0;
            }
            searchOffset = nextPlus + 1;
            nextPlus = name.indexOf('+', searchOffset);
        }

        // parse last or only AVA
        final String avaString = name.substring(avaOffset);
        if (avaString.length() == 0) {
            throw new IOException("empty AVA in RDN \"" + name + "\""); //$NON-NLS-1$ //$NON-NLS-2$
        }
        final AVA ava = new AVA(new StringReader(avaString), keywordMap);
        avaVec.add(ava);

        this.assertion = avaVec.toArray(new AVA[avaVec.size()]);
    }

    /*
     * Constructs an RDN from its printable representation.
     *
     * An RDN may consist of one or multiple Attribute Value Assertions (AVAs),
     * using '+' as a separator.
     * If the '+' should be considered part of an AVA value, it must be
     * preceded by '\'.
     *
     * @param name String form of RDN
     * @throws IOException on parsing error
     */
    RDN(final String name, final String format) throws IOException {
        this(name, format, Collections.<String, String>emptyMap());
    }

    /*
     * Constructs an RDN from its printable representation.
     *
     * An RDN may consist of one or multiple Attribute Value Assertions (AVAs),
     * using '+' as a separator.
     * If the '+' should be considered part of an AVA value, it must be
     * preceded by '\'.
     *
     * @param name String form of RDN
     * @param keyword an additional mapping of keywords to OIDs
     * @throws IOException on parsing error
     */
    RDN(final String name, final String format, final Map<String, String> keywordMap)
        throws IOException {
        if (format.equalsIgnoreCase("RFC2253") == false) { //$NON-NLS-1$
            throw new IOException("Unsupported format " + format); //$NON-NLS-1$
        }
        int searchOffset = 0;
        int avaOffset = 0;
        final List<AVA> avaVec = new ArrayList<AVA>(3);
        int nextPlus = name.indexOf('+');
        while (nextPlus >= 0) {
            /*
             * We have encountered an AVA delimiter (plus sign).
             * If the plus sign in the RDN under consideration is
             * preceded by a backslash (escape), or by a double quote, it
             * is part of the AVA. Otherwise, it is used as a separator, to
             * delimit the AVA under consideration from any subsequent AVAs.
             */
            if (nextPlus > 0 && name.charAt(nextPlus - 1) != '\\' ) {
                /*
                 * Plus sign is a separator
                 */
                final String avaString = name.substring(avaOffset, nextPlus);
                if (avaString.length() == 0) {
                    throw new IOException("empty AVA in RDN \"" + name + "\""); //$NON-NLS-1$ //$NON-NLS-2$
                }

                // Parse AVA, and store it in vector
                final AVA ava = new AVA
                    (new StringReader(avaString), AVA.RFC2253, keywordMap);
                avaVec.add(ava);

                // Increase the offset
                avaOffset = nextPlus + 1;
            }
            searchOffset = nextPlus + 1;
            nextPlus = name.indexOf('+', searchOffset);
        }

        // parse last or only AVA
        final String avaString = name.substring(avaOffset);
        if (avaString.length() == 0) {
            throw new IOException("empty AVA in RDN \"" + name + "\""); //$NON-NLS-1$ //$NON-NLS-2$
        }
        final AVA ava = new AVA(new StringReader(avaString), AVA.RFC2253, keywordMap);
        avaVec.add(ava);

        this.assertion = avaVec.toArray(new AVA[avaVec.size()]);
    }

    /*
     * Constructs an RDN from an ASN.1 encoded value.  The encoding
     * of the name in the stream uses DER (a BER/1 subset).
     *
     * @param value a DER-encoded value holding an RDN.
     * @throws IOException on parsing error.
     */
    RDN(final DerValue rdn) throws IOException {
        if (rdn.tag != DerValue.tag_Set) {
            throw new IOException("X500 RDN"); //$NON-NLS-1$
        }
        final DerInputStream dis = new DerInputStream(rdn.toByteArray());
        final DerValue[] avaset = dis.getSet(5);

        this.assertion = new AVA[avaset.length];
        for (int i = 0; i < avaset.length; i++) {
            this.assertion[i] = new AVA(avaset[i]);
        }
    }

    /*
     * Creates an empty RDN with slots for specified
     * number of AVAs.
     *
     * @param i number of AVAs to be in RDN
     */
    RDN(final int i) { this.assertion = new AVA[i]; }

    public RDN(final AVA ava) {
        if (ava == null) {
            throw new NullPointerException();
        }
        this.assertion = new AVA[] { ava };
    }

    public RDN(final AVA[] avas) {
        this.assertion = avas.clone();
        for (final AVA element : this.assertion) {
            if (element == null) {
                throw new NullPointerException();
            }
        }
    }

    /**
     * Return an immutable List of the AVAs in this RDN.
     */
    public List<AVA> avas() {
        List<AVA> list = this.avaList;
        if (list == null) {
            list = Collections.unmodifiableList(Arrays.asList(this.assertion));
            this.avaList = list;
        }
        return list;
    }

    /**
     * Return the number of AVAs in this RDN.
     */
    public int size() {
        return this.assertion.length;
    }

    @Override
	public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof RDN == false) {
            return false;
        }
        final RDN other = (RDN)obj;
        if (this.assertion.length != other.assertion.length) {
            return false;
        }
        final String thisCanon = this.toRFC2253String(true);
        final String otherCanon = other.toRFC2253String(true);
        return thisCanon.equals(otherCanon);
    }

    /*
     * Calculates a hash code value for the object.  Objects
     * which are equal will also have the same hashcode.
     *
     * @returns int hashCode value
     */
    @Override
	public int hashCode() {
        return toRFC2253String(true).hashCode();
    }

    /*
     * return specified attribute value from RDN
     *
     * @params oid ObjectIdentifier of attribute to be found
     * @returns DerValue of attribute value; null if attribute does not exist
     */
    DerValue findAttribute(final ObjectIdentifier oid) {
        for (final AVA element : this.assertion) {
            if (element.oid.equals(oid)) {
                return element.value;
            }
        }
        return null;
    }

    /*
     * Encode the RDN in DER-encoded form.
     *
     * @param out DerOutputStream to which RDN is to be written
     * @throws IOException on error
     */
    void encode(final DerOutputStream out) throws IOException {
        out.putOrderedSetOf(DerValue.tag_Set, this.assertion);
    }

    /*
     * Returns a printable form of this RDN, using RFC 1779 style catenation
     * of attribute/value assertions, and emitting attribute type keywords
     * from RFCs 1779, 2253, and 3280.
     */
    @Override
	public String toString() {
        if (this.assertion.length == 1) {
            return this.assertion[0].toString();
        }

        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < this.assertion.length; i++) {
            if (i != 0) {
                sb.append(" + "); //$NON-NLS-1$
            }
            sb.append(this.assertion[i].toString());
        }
        return sb.toString();
    }

    /*
     * Returns a printable form of this RDN using the algorithm defined in
     * RFC 1779. Only RFC 1779 attribute type keywords are emitted.
     */
    public String toRFC1779String() {
        return toRFC1779String(Collections.<String, String>emptyMap());
    }

    /*
     * Returns a printable form of this RDN using the algorithm defined in
     * RFC 1779. RFC 1779 attribute type keywords are emitted, as well
     * as keywords contained in the OID/keyword map.
     */
    public String toRFC1779String(final Map<String, String> oidMap) {
        if (this.assertion.length == 1) {
            return this.assertion[0].toRFC1779String(oidMap);
        }

        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < this.assertion.length; i++) {
            if (i != 0) {
                sb.append(" + "); //$NON-NLS-1$
            }
            sb.append(this.assertion[i].toRFC1779String(oidMap));
        }
        return sb.toString();
    }

    /*
     * Returns a printable form of this RDN using the algorithm defined in
     * RFC 2253. Only RFC 2253 attribute type keywords are emitted.
     */
    public String toRFC2253String() {
        return toRFC2253StringInternal
            (false, Collections.<String, String>emptyMap());
    }

    /*
     * Returns a printable form of this RDN using the algorithm defined in
     * RFC 2253. RFC 2253 attribute type keywords are emitted, as well as
     * keywords contained in the OID/keyword map.
     */
    public String toRFC2253String(final Map<String, String> oidMap) {
        return toRFC2253StringInternal(false, oidMap);
    }

    /*
     * Returns a printable form of this RDN using the algorithm defined in
     * RFC 2253. Only RFC 2253 attribute type keywords are emitted.
     * If canonical is true, then additional canonicalizations
     * documented in X500Principal.getName are performed.
     */
    public String toRFC2253String(final boolean canonical) {
        if (canonical == false) {
            return toRFC2253StringInternal
                (false, Collections.<String, String>emptyMap());
        }
        String c = this.canonicalString;
        if (c == null) {
            c = toRFC2253StringInternal
                (true, Collections.<String, String>emptyMap());
            this.canonicalString = c;
        }
        return c;
    }

    private String toRFC2253StringInternal
        (final boolean canonical, final Map<String, String> oidMap) {
        /*
         * Section 2.2: When converting from an ASN.1 RelativeDistinguishedName
         * to a string, the output consists of the string encodings of each
         * AttributeTypeAndValue (according to 2.3), in any order.
         *
         * Where there is a multi-valued RDN, the outputs from adjoining
         * AttributeTypeAndValues are separated by a plus ('+' ASCII 43)
         * character.
         */

        // normally, an RDN only contains one AVA
        if (this.assertion.length == 1) {
            return canonical ? this.assertion[0].toRFC2253CanonicalString() :
                               this.assertion[0].toRFC2253String(oidMap);
        }

        final StringBuilder relname = new StringBuilder();
        if (!canonical) {
            for (int i = 0; i < this.assertion.length; i++) {
                if (i > 0) {
                    relname.append('+');
                }
                relname.append(this.assertion[i].toRFC2253String(oidMap));
            }
        } else {
            // order the string type AVA's alphabetically,
            // followed by the oid type AVA's numerically
            final List<AVA> avaList = new ArrayList<AVA>(this.assertion.length);
            for (final AVA element : this.assertion) {
                avaList.add(element);
            }
            java.util.Collections.sort(avaList, AVAComparator.getInstance());

            for (int i = 0; i < avaList.size(); i++) {
                if (i > 0) {
                    relname.append('+');
                }
                relname.append(avaList.get(i).toRFC2253CanonicalString());
            }
        }
        return relname.toString();
    }

}

class AVAComparator implements Comparator<AVA> {

    private static final Comparator<AVA> INSTANCE = new AVAComparator();

    private AVAComparator() {
        // empty
    }

    static Comparator<AVA> getInstance() {
        return INSTANCE;
    }

    /**
     * AVA's containing a standard keyword are ordered alphabetically,
     * followed by AVA's containing an OID keyword, ordered numerically
     */
    @Override
	public int compare(final AVA a1, final AVA a2) {
        final boolean a1Has2253 = a1.hasRFC2253Keyword();
        final boolean a2Has2253 = a2.hasRFC2253Keyword();

        if (a1Has2253 == a2Has2253) {
            return a1.toRFC2253CanonicalString().compareTo
                        (a2.toRFC2253CanonicalString());
        } else {
            if (a1Has2253) {
                return -1;
            } else {
                return 1;
            }
        }
    }

}
