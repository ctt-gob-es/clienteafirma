/*
 * Copyright (c) 1997, 2009, Oracle and/or its affiliates. All rights reserved.
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
import java.util.Enumeration;

import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;

/**
 * This class represents the Authority Key Identifier Extension.
 *
 * <p>The authority key identifier extension provides a means of
 * identifying the particular public key used to sign a certificate.
 * This extension would be used where an issuer has multiple signing
 * keys (either due to multiple concurrent key pairs or due to
 * changeover).
 * <p>
 * The ASN.1 syntax for this is:
 * <pre>
 * AuthorityKeyIdentifier ::= SEQUENCE {
 *    keyIdentifier             [0] KeyIdentifier           OPTIONAL,
 *    authorityCertIssuer       [1] GeneralNames            OPTIONAL,
 *    authorityCertSerialNumber [2] CertificateSerialNumber OPTIONAL
 * }
 * KeyIdentifier ::= OCTET STRING
 * </pre>
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 * @see Extension
 * @see CertAttrSet
 */
public class AuthorityKeyIdentifierExtension extends Extension
implements CertAttrSet<String> {
    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT =
                         "x509.info.extensions.AuthorityKeyIdentifier";
    /**
     * Attribute names.
     */
    public static final String NAME = "AuthorityKeyIdentifier";
    public static final String KEY_ID = "key_id";
    public static final String AUTH_NAME = "auth_name";
    public static final String SERIAL_NUMBER = "serial_number";

    // Private data members
    private static final byte TAG_ID = 0;
    private static final byte TAG_NAMES = 1;
    private static final byte TAG_SERIAL_NUM = 2;

    private KeyIdentifier       id = null;
    private GeneralNames        names = null;
    private SerialNumber        serialNum = null;

    // Encode only the extension value
    private void encodeThis() throws IOException {
        if (this.id == null && this.names == null && this.serialNum == null) {
            this.extensionValue = null;
            return;
        }
        final DerOutputStream seq = new DerOutputStream();
        final DerOutputStream tmp = new DerOutputStream();
        if (this.id != null) {
            final DerOutputStream tmp1 = new DerOutputStream();
            this.id.encode(tmp1);
            tmp.writeImplicit(DerValue.createTag(DerValue.TAG_CONTEXT,
                              false, TAG_ID), tmp1);
        }
        try {
            if (this.names != null) {
                final DerOutputStream tmp1 = new DerOutputStream();
                this.names.encode(tmp1);
                tmp.writeImplicit(DerValue.createTag(DerValue.TAG_CONTEXT,
                                  true, TAG_NAMES), tmp1);
            }
        } catch (final Exception e) {
            throw new IOException(e.toString());
        }
        if (this.serialNum != null) {
            final DerOutputStream tmp1 = new DerOutputStream();
            this.serialNum.encode(tmp1);
            tmp.writeImplicit(DerValue.createTag(DerValue.TAG_CONTEXT,
                              false, TAG_SERIAL_NUM), tmp1);
        }
        seq.write(DerValue.tag_Sequence, tmp);
        this.extensionValue = seq.toByteArray();
    }

    /**
     * The default constructor for this extension.  Null parameters make
     * the element optional (not present).
     *
     * @param id the KeyIdentifier associated with this extension.
     * @param names the GeneralNames associated with this extension
     * @param serialNum the CertificateSerialNumber associated with
     *         this extension.
     * @exception IOException on error.
     */
    public AuthorityKeyIdentifierExtension(final KeyIdentifier kid, final GeneralNames name,
                                           final SerialNumber sn)
    throws IOException {
        this.id = kid;
        this.names = name;
        this.serialNum = sn;

        this.extensionId = PKIXExtensions.AuthorityKey_Id;
        this.critical = false;
        encodeThis();
    }

    /**
     * Create the extension from the passed DER encoded value of the same.
     *
     * @param critical true if the extension is to be treated as critical.
     * @param value an array of DER encoded bytes of the actual value.
     * @exception ClassCastException if value is not an array of bytes
     * @exception IOException on error.
     */
    public AuthorityKeyIdentifierExtension(final Boolean critical, final Object value)
    throws IOException {
        this.extensionId = PKIXExtensions.AuthorityKey_Id;
        this.critical = critical.booleanValue();

        this.extensionValue = (byte[]) value;
        final DerValue val = new DerValue(this.extensionValue);
        if (val.tag != DerValue.tag_Sequence) {
            throw new IOException("Invalid encoding for " +
                                  "AuthorityKeyIdentifierExtension.");
        }

        // Note that all the fields in AuthorityKeyIdentifier are defined as
        // being OPTIONAL, i.e., there could be an empty SEQUENCE, resulting
        // in val.data being null.
        while ((val.data != null) && (val.data.available() != 0)) {
            final DerValue opt = val.data.getDerValue();

            // NB. this is always encoded with the IMPLICIT tag
            // The checks only make sense if we assume implicit tagging,
            // with explicit tagging the form is always constructed.
            if (opt.isContextSpecific(TAG_ID) && !opt.isConstructed()) {
                if (this.id != null) {
					throw new IOException("Duplicate KeyIdentifier in " +
                                          "AuthorityKeyIdentifier.");
				}
                opt.resetTag(DerValue.tag_OctetString);
                this.id = new KeyIdentifier(opt);

            } else if (opt.isContextSpecific(TAG_NAMES) &&
                       opt.isConstructed()) {
                if (this.names != null) {
					throw new IOException("Duplicate GeneralNames in " +
                                          "AuthorityKeyIdentifier.");
				}
                opt.resetTag(DerValue.tag_Sequence);
                this.names = new GeneralNames(opt);

            } else if (opt.isContextSpecific(TAG_SERIAL_NUM) &&
                       !opt.isConstructed()) {
                if (this.serialNum != null) {
					throw new IOException("Duplicate SerialNumber in " +
                                          "AuthorityKeyIdentifier.");
				}
                opt.resetTag(DerValue.tag_Integer);
                this.serialNum = new SerialNumber(opt);
            } else {
				throw new IOException("Invalid encoding of " +
                                      "AuthorityKeyIdentifierExtension.");
			}
        }
    }

    /**
     * Return the object as a string.
     */
    @Override
	public String toString() {
        String s = super.toString() + "AuthorityKeyIdentifier [\n";
        if (this.id != null) {
            s += this.id.toString();     // id already has a newline
        }
        if (this.names != null) {
            s += this.names.toString() + "\n";
        }
        if (this.serialNum != null) {
            s += this.serialNum.toString() + "\n";
        }
        return (s + "]\n");
    }

    /**
     * Write the extension to the OutputStream.
     *
     * @param out the OutputStream to write the extension to.
     * @exception IOException on error.
     */
    @Override
	public void encode(final OutputStream out) throws IOException {
        final DerOutputStream tmp = new DerOutputStream();
        if (this.extensionValue == null) {
            this.extensionId = PKIXExtensions.AuthorityKey_Id;
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
	public void set(final String name, final Object obj) throws IOException {
        if (name.equalsIgnoreCase(KEY_ID)) {
            if (!(obj instanceof KeyIdentifier)) {
              throw new IOException("Attribute value should be of " +
                                    "type KeyIdentifier.");
            }
            this.id = (KeyIdentifier)obj;
        } else if (name.equalsIgnoreCase(AUTH_NAME)) {
            if (!(obj instanceof GeneralNames)) {
              throw new IOException("Attribute value should be of " +
                                    "type GeneralNames.");
            }
            this.names = (GeneralNames)obj;
        } else if (name.equalsIgnoreCase(SERIAL_NUMBER)) {
            if (!(obj instanceof SerialNumber)) {
              throw new IOException("Attribute value should be of " +
                                    "type SerialNumber.");
            }
            this.serialNum = (SerialNumber)obj;
        } else {
          throw new IOException("Attribute name not recognized by " +
                        "CertAttrSet:AuthorityKeyIdentifier.");
        }
        encodeThis();
    }

    /**
     * Get the attribute value.
     */
    @Override
	public Object get(final String name) throws IOException {
        if (name.equalsIgnoreCase(KEY_ID)) {
            return (this.id);
        } else if (name.equalsIgnoreCase(AUTH_NAME)) {
            return (this.names);
        } else if (name.equalsIgnoreCase(SERIAL_NUMBER)) {
            return (this.serialNum);
        } else {
          throw new IOException("Attribute name not recognized by " +
                        "CertAttrSet:AuthorityKeyIdentifier.");
        }
    }

    /**
     * Delete the attribute value.
     */
    @Override
	public void delete(final String name) throws IOException {
        if (name.equalsIgnoreCase(KEY_ID)) {
            this.id = null;
        } else if (name.equalsIgnoreCase(AUTH_NAME)) {
            this.names = null;
        } else if (name.equalsIgnoreCase(SERIAL_NUMBER)) {
            this.serialNum = null;
        } else {
          throw new IOException("Attribute name not recognized by " +
                        "CertAttrSet:AuthorityKeyIdentifier.");
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
        elements.addElement(KEY_ID);
        elements.addElement(AUTH_NAME);
        elements.addElement(SERIAL_NUMBER);

        return (elements.elements());
    }

    /**
     * Return the name of this attribute.
     */
    @Override
	public String getName() {
        return (NAME);
    }
}
