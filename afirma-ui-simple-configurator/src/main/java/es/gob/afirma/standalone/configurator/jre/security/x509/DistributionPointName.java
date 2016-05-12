/*
 * Copyright (c) 2005, 2011, Oracle and/or its affiliates. All rights reserved.
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
import java.util.Objects;

import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;

/**
 * Represents the DistributionPointName ASN.1 type.
 *
 * It is used in the CRL Distribution Points Extension (OID = 2.5.29.31)
 * and the Issuing Distribution Point Extension (OID = 2.5.29.28).
 * <p>
 * Its ASN.1 definition is:
 * <pre>
 *
 *     DistributionPointName ::= CHOICE {
 *         fullName                  [0] GeneralNames,
 *         nameRelativeToCRLIssuer   [1] RelativeDistinguishedName }
 *
 *     GeneralNames ::= SEQUENCE SIZE (1..MAX) OF GeneralName
 *
 *     GeneralName ::= CHOICE {
 *         otherName                 [0] INSTANCE OF OTHER-NAME,
 *         rfc822Name                [1] IA5String,
 *         dNSName                   [2] IA5String,
 *         x400Address               [3] ORAddress,
 *         directoryName             [4] Name,
 *         ediPartyName              [5] EDIPartyName,
 *         uniformResourceIdentifier [6] IA5String,
 *         iPAddress                 [7] OCTET STRING,
 *         registeredID              [8] OBJECT IDENTIFIER }
 *
 *     RelativeDistinguishedName ::= SET OF AttributeTypeAndValue
 *
 *     AttributeTypeAndValue ::= SEQUENCE {
 *         type    AttributeType,
 *         value   AttributeValue }
 *
 *     AttributeType ::= OBJECT IDENTIFIER
 *
 *     AttributeValue ::= ANY DEFINED BY AttributeType
 *
 * </pre>
 * <p>
 * Instances of this class are designed to be immutable. However, since this
 * is an internal API we do not use defensive cloning for values for
 * performance reasons. It is the responsibility of the consumer to ensure
 * that no mutable elements are modified.
 *
 * @see CRLDistributionPointsExtension
 * @see IssuingDistributionPointExtension
 * @since 1.6
 */
public class DistributionPointName {

    // ASN.1 context specific tag values
    private static final byte TAG_FULL_NAME = 0;
    private static final byte TAG_RELATIVE_NAME = 1;

    // Only one of fullName and relativeName can be set
    private GeneralNames fullName = null;
    private RDN relativeName = null;

    // Cached hashCode value
    private volatile int hashCode;

    /**
     * Creates a distribution point name using a full name.
     *
     * @param fullName the name for the distribution point.
     * @exception IllegalArgumentException if <code>fullName</code> is null.
     */
    public DistributionPointName(final GeneralNames fullName) {

        if (fullName == null) {
            throw new IllegalArgumentException("fullName must not be null");
        }
        this.fullName = fullName;
    }

    /**
     * Creates a distribution point name using a relative name.
     *
     * @param relativeName the name of the distribution point relative to
     *        the name of the issuer of the CRL.
     * @exception IllegalArgumentException if <code>relativeName</code> is null.
     */
    public DistributionPointName(final RDN relativeName) {

        if (relativeName == null) {
            throw new IllegalArgumentException("relativeName must not be null");
        }
        this.relativeName = relativeName;
    }

    /**
     * Creates a distribution point name from its DER-encoded form.
     *
     * @param encoding the DER-encoded value.
     * @throws IOException on decoding error.
     */
    public DistributionPointName(final DerValue encoding) throws IOException {

        if (encoding.isContextSpecific(TAG_FULL_NAME) &&
            encoding.isConstructed()) {

            encoding.resetTag(DerValue.tag_Sequence);
            this.fullName = new GeneralNames(encoding);

        } else if (encoding.isContextSpecific(TAG_RELATIVE_NAME) &&
            encoding.isConstructed()) {

            encoding.resetTag(DerValue.tag_Set);
            this.relativeName = new RDN(encoding);

        } else {
            throw new IOException("Invalid encoding for DistributionPointName");
        }

    }

    /**
     * Returns the full name for the distribution point or null if not set.
     */
    public GeneralNames getFullName() {
        return this.fullName;
    }

    /**
     * Returns the relative name for the distribution point or null if not set.
     */
    public RDN getRelativeName() {
        return this.relativeName;
    }

    /**
     * Encodes the distribution point name and writes it to the DerOutputStream.
     *
     * @param out the output stream.
     * @exception IOException on encoding error.
     */
    public void encode(final DerOutputStream out) throws IOException {

        final DerOutputStream theChoice = new DerOutputStream();

        if (this.fullName != null) {
            this.fullName.encode(theChoice);
            out.writeImplicit(
                DerValue.createTag(DerValue.TAG_CONTEXT, true, TAG_FULL_NAME),
                theChoice);

        } else {
            this.relativeName.encode(theChoice);
            out.writeImplicit(
                DerValue.createTag(DerValue.TAG_CONTEXT, true,
                    TAG_RELATIVE_NAME),
                theChoice);
        }
    }

    /**
     * Compare an object to this distribution point name for equality.
     *
     * @param obj Object to be compared to this
     * @return true if objects match; false otherwise
     */
    @Override
	public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof DistributionPointName == false) {
            return false;
        }
        final DistributionPointName other = (DistributionPointName)obj;

        return Objects.equals(this.fullName, other.fullName) &&
               Objects.equals(this.relativeName, other.relativeName);
    }

    /**
     * Returns the hash code for this distribution point name.
     *
     * @return the hash code.
     */
    @Override
	public int hashCode() {
        int hash = this.hashCode;
        if (hash == 0) {
            hash = 1;
            if (this.fullName != null) {
                hash += this.fullName.hashCode();

            } else {
                hash += this.relativeName.hashCode();
            }
            this.hashCode = hash;
        }
        return hash;
    }

    /**
     * Returns a printable string of the distribution point name.
     */
    @Override
	public String toString() {
        final StringBuilder sb = new StringBuilder();
        if (this.fullName != null) {
            sb.append("DistributionPointName:\n     " + this.fullName + "\n");

        } else {
            sb.append("DistributionPointName:\n     " + this.relativeName + "\n");
        }

        return sb.toString();
    }
}
