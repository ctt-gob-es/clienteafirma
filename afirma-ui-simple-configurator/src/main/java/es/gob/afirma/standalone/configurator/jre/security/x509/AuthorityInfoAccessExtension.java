/*
 * Copyright (c) 2004, 2011, Oracle and/or its affiliates. All rights reserved.
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
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;

/**
 * The Authority Information Access Extension (OID = 1.3.6.1.5.5.7.1.1).
 * <p>
 * The AIA extension identifies how to access CA information and services
 * for the certificate in which it appears. It enables CAs to issue their
 * certificates pre-configured with the URLs appropriate for contacting
 * services relevant to those certificates. For example, a CA may issue a
 * certificate that identifies the specific OCSP Responder to use when
 * performing on-line validation of that certificate.
 * <p>
 * This extension is defined in <a href="http://www.ietf.org/rfc/rfc3280.txt">
 * Internet X.509 PKI Certificate and Certificate Revocation List
 * (CRL) Profile</a>. The profile permits
 * the extension to be included in end-entity or CA certificates,
 * and it must be marked as non-critical. Its ASN.1 definition is as follows:
 * <pre>
 *   id-pe-authorityInfoAccess OBJECT IDENTIFIER ::= { id-pe 1 }
 *
 *   AuthorityInfoAccessSyntax  ::=
 *         SEQUENCE SIZE (1..MAX) OF AccessDescription
 *
 *   AccessDescription  ::=  SEQUENCE {
 *         accessMethod          OBJECT IDENTIFIER,
 *         accessLocation        GeneralName  }
 * </pre>
 * <p>
 * @see Extension
 * @see CertAttrSet
 */

public class AuthorityInfoAccessExtension extends Extension
        implements CertAttrSet<String> {

    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT =
                                "x509.info.extensions.AuthorityInfoAccess";

    /**
     * Attribute name.
     */
    public static final String NAME = "AuthorityInfoAccess";
    public static final String DESCRIPTIONS = "descriptions";

    /**
     * The List of AccessDescription objects.
     */
    private List<AccessDescription> accessDescriptions;

    /**
     * Create an AuthorityInfoAccessExtension from a List of
     * AccessDescription; the criticality is set to false.
     *
     * @param accessDescriptions the List of AccessDescription
     * @throws IOException on error
     */
    public AuthorityInfoAccessExtension(
            final List<AccessDescription> accessDescriptions) throws IOException {
        this.extensionId = PKIXExtensions.AuthInfoAccess_Id;
        this.critical = false;
        this.accessDescriptions = accessDescriptions;
        encodeThis();
    }

    /**
     * Create the extension from the passed DER encoded value of the same.
     *
     * @param critical true if the extension is to be treated as critical.
     * @param value Array of DER encoded bytes of the actual value.
     * @exception IOException on error.
     */
    public AuthorityInfoAccessExtension(final Boolean critical, final Object value)
            throws IOException {
        this.extensionId = PKIXExtensions.AuthInfoAccess_Id;
        this.critical = critical.booleanValue();

        if (!(value instanceof byte[])) {
            throw new IOException("Illegal argument type");
        }

        this.extensionValue = (byte[])value;
        final DerValue val = new DerValue(this.extensionValue);
        if (val.tag != DerValue.tag_Sequence) {
            throw new IOException("Invalid encoding for " +
                                  "AuthorityInfoAccessExtension.");
        }
        this.accessDescriptions = new ArrayList<AccessDescription>();
        while (val.data.available() != 0) {
            final DerValue seq = val.data.getDerValue();
            final AccessDescription accessDescription = new AccessDescription(seq);
            this.accessDescriptions.add(accessDescription);
        }
    }

    /**
     * Return the list of AccessDescription objects.
     */
    public List<AccessDescription> getAccessDescriptions() {
        return this.accessDescriptions;
    }

    /**
     * Return the name of this attribute.
     */
    @Override
	public String getName() {
        return NAME;
    }

    /**
     * Write the extension to the DerOutputStream.
     *
     * @param out the DerOutputStream to write the extension to.
     * @exception IOException on encoding errors.
     */
    @Override
	public void encode(final OutputStream out) throws IOException {
        final DerOutputStream tmp = new DerOutputStream();
        if (this.extensionValue == null) {
            this.extensionId = PKIXExtensions.AuthInfoAccess_Id;
            this.critical = false;
            encodeThis();
        }
        super.encode(tmp);
        out.write(tmp.toByteArray());
    }

    /**
     * Set the attribute value.
     */
    @Override
	@SuppressWarnings("unchecked") // Checked with an instanceof check
    public void set(final String name, final Object obj) throws IOException {
        if (name.equalsIgnoreCase(DESCRIPTIONS)) {
            if (!(obj instanceof List)) {
                throw new IOException("Attribute value should be of type List.");
            }
            this.accessDescriptions = (List<AccessDescription>)obj;
        } else {
            throw new IOException("Attribute name [" + name +
                                "] not recognized by " +
                                "CertAttrSet:AuthorityInfoAccessExtension.");
        }
        encodeThis();
    }

    /**
     * Get the attribute value.
     */
    @Override
	public List<AccessDescription> get(final String name) throws IOException {
        if (name.equalsIgnoreCase(DESCRIPTIONS)) {
            return this.accessDescriptions;
        } else {
            throw new IOException("Attribute name [" + name +
                                "] not recognized by " +
                                "CertAttrSet:AuthorityInfoAccessExtension.");
        }
    }

    /**
     * Delete the attribute value.
     */
    @Override
	public void delete(final String name) throws IOException {
        if (name.equalsIgnoreCase(DESCRIPTIONS)) {
            this.accessDescriptions = new ArrayList<AccessDescription>();
        } else {
            throw new IOException("Attribute name [" + name +
                                "] not recognized by " +
                                "CertAttrSet:AuthorityInfoAccessExtension.");
        }
        encodeThis();
    }

    /**
     * Return an enumeration of names of attributes existing within this
     * attribute.
     */
    @Override
	public Enumeration<String> getElements() {
        final AttributeNameEnumeration elements = new AttributeNameEnumeration();
        elements.addElement(DESCRIPTIONS);
        return elements.elements();
    }

     // Encode this extension value
    private void encodeThis() throws IOException {
        if (this.accessDescriptions.isEmpty()) {
            this.extensionValue = null;
        } else {
            final DerOutputStream ads = new DerOutputStream();
            for (final AccessDescription accessDescription : this.accessDescriptions) {
                accessDescription.encode(ads);
            }
            final DerOutputStream seq = new DerOutputStream();
            seq.write(DerValue.tag_Sequence, ads);
            this.extensionValue = seq.toByteArray();
        }
    }

    /**
     * Return the extension as user readable string.
     */
    @Override
	public String toString() {
        return super.toString() + "AuthorityInfoAccess [\n  "
               + this.accessDescriptions + "\n]\n";
    }

}
