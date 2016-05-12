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

package es.gob.afirma.standalone.configurator.jre.security.x509;

import java.io.IOException;
import java.util.Enumeration;

import es.gob.afirma.standalone.configurator.jre.security.util.BitArray;
import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;

/**
 * Represent the CRL Reason Flags.
 *
 * <p>This extension, if present, defines the identifies
 * the reason for the certificate revocation.
 * <p>The ASN.1 syntax for this is:
 * <pre>
 * ReasonFlags ::= BIT STRING {
 *    unused                  (0),
 *    keyCompromise           (1),
 *    cACompromise            (2),
 *    affiliationChanged      (3),
 *    superseded              (4),
 *    cessationOfOperation    (5),
 *    certificateHold         (6),
 *    privilegeWithdrawn      (7),
 *    aACompromise            (8) }
 * </pre>
 *
 * @author Hemma Prafullchandra
 */
public class ReasonFlags {

    /**
     * Reasons
     */
    public static final String UNUSED = "unused"; //$NON-NLS-1$
    public static final String KEY_COMPROMISE = "key_compromise"; //$NON-NLS-1$
    public static final String CA_COMPROMISE = "ca_compromise"; //$NON-NLS-1$
    public static final String AFFILIATION_CHANGED = "affiliation_changed"; //$NON-NLS-1$
    public static final String SUPERSEDED = "superseded"; //$NON-NLS-1$
    public static final String CESSATION_OF_OPERATION
                                   = "cessation_of_operation"; //$NON-NLS-1$
    public static final String CERTIFICATE_HOLD = "certificate_hold"; //$NON-NLS-1$
    public static final String PRIVILEGE_WITHDRAWN = "privilege_withdrawn"; //$NON-NLS-1$
    public static final String AA_COMPROMISE = "aa_compromise"; //$NON-NLS-1$

    private final static String[] NAMES = {
        UNUSED,
        KEY_COMPROMISE,
        CA_COMPROMISE,
        AFFILIATION_CHANGED,
        SUPERSEDED,
        CESSATION_OF_OPERATION,
        CERTIFICATE_HOLD,
        PRIVILEGE_WITHDRAWN,
        AA_COMPROMISE,
    };

    private static int name2Index(final String name) throws IOException {
        for( int i=0; i<NAMES.length; i++ ) {
            if( NAMES[i].equalsIgnoreCase(name) ) {
                return i;
            }
        }
        throw new IOException("Name not recognized by ReasonFlags"); //$NON-NLS-1$
    }

    // Private data members
    private boolean[] bitString;

    /**
     * Check if bit is set.
     *
     * @param position the position in the bit string to check.
     */
    private boolean isSet(final int position) {
        return this.bitString[position];
    }

    /**
     * Set the bit at the specified position.
     */
    private void set(final int position, final boolean val) {
        // enlarge bitString if necessary
        if (position >= this.bitString.length) {
            final boolean[] tmp = new boolean[position+1];
            System.arraycopy(this.bitString, 0, tmp, 0, this.bitString.length);
            this.bitString = tmp;
        }
        this.bitString[position] = val;
    }

    /**
     * Create a ReasonFlags with the passed bit settings.
     *
     * @param reasons the bits to be set for the ReasonFlags.
     */
    public ReasonFlags(final byte[] reasons) {
        this.bitString = new BitArray(reasons.length*8, reasons).toBooleanArray();
    }

    /**
     * Create a ReasonFlags with the passed bit settings.
     *
     * @param reasons the bits to be set for the ReasonFlags.
     */
    public ReasonFlags(final boolean[] reasons) {
        this.bitString = reasons;
    }

    /**
     * Create a ReasonFlags with the passed bit settings.
     *
     * @param reasons the bits to be set for the ReasonFlags.
     */
    public ReasonFlags(final BitArray reasons) {
        this.bitString = reasons.toBooleanArray();
    }

    /**
     * Create the object from the passed DER encoded value.
     *
     * @param in the DerInputStream to read the ReasonFlags from.
     * @exception IOException on decoding errors.
     */
    public ReasonFlags(final DerInputStream in) throws IOException {
        final DerValue derVal = in.getDerValue();
        this.bitString = derVal.getUnalignedBitString(true).toBooleanArray();
    }

    /**
     * Create the object from the passed DER encoded value.
     *
     * @param derVal the DerValue decoded from the stream.
     * @exception IOException on decoding errors.
     */
    public ReasonFlags(final DerValue derVal) throws IOException {
        this.bitString = derVal.getUnalignedBitString(true).toBooleanArray();
    }

    /**
     * Returns the reason flags as a boolean array.
     */
    public boolean[] getFlags() {
        return this.bitString;
    }

    /**
     * Set the attribute value.
     */
    public void set(final String name, final Object obj) throws IOException {
        if (!(obj instanceof Boolean)) {
            throw new IOException("Attribute must be of type Boolean."); //$NON-NLS-1$
        }
        final boolean val = ((Boolean)obj).booleanValue();
        set(name2Index(name), val);
    }

    /**
     * Get the attribute value.
     */
    public Object get(final String name) throws IOException {
        return Boolean.valueOf(isSet(name2Index(name)));
    }

    /**
     * Delete the attribute value.
     */
    public void delete(final String name) throws IOException {
        set(name, Boolean.FALSE);
    }

    /**
     * Returns a printable representation of the ReasonFlags.
     */
    @Override
	public String toString() {
        String s = "Reason Flags [\n"; //$NON-NLS-1$

        try {
            if (isSet(0)) {
				s += "  Unused\n"; //$NON-NLS-1$
			}
            if (isSet(1)) {
				s += "  Key Compromise\n"; //$NON-NLS-1$
			}
            if (isSet(2)) {
				s += "  CA Compromise\n"; //$NON-NLS-1$
			}
            if (isSet(3)) {
				s += "  Affiliation_Changed\n"; //$NON-NLS-1$
			}
            if (isSet(4)) {
				s += "  Superseded\n"; //$NON-NLS-1$
			}
            if (isSet(5)) {
				s += "  Cessation Of Operation\n"; //$NON-NLS-1$
			}
            if (isSet(6)) {
				s += "  Certificate Hold\n"; //$NON-NLS-1$
			}
            if (isSet(7)) {
				s += "  Privilege Withdrawn\n"; //$NON-NLS-1$
			}
            if (isSet(8)) {
				s += "  AA Compromise\n"; //$NON-NLS-1$
			}
        } catch (final ArrayIndexOutOfBoundsException ex) {}

        s += "]\n"; //$NON-NLS-1$

        return (s);
    }

    /**
     * Write the extension to the DerOutputStream.
     *
     * @param out the DerOutputStream to write the extension to.
     * @exception IOException on encoding errors.
     */
    public void encode(final DerOutputStream out) throws IOException {
        out.putTruncatedUnalignedBitString(new BitArray(this.bitString));
    }

    /**
     * Return an enumeration of names of attributes existing within this
     * attribute.
     */
    public Enumeration<String> getElements () {
        final AttributeNameEnumeration elements = new AttributeNameEnumeration();
        for (final String element : NAMES) {
            elements.addElement(element);
        }
        return (elements.elements());
    }
}
