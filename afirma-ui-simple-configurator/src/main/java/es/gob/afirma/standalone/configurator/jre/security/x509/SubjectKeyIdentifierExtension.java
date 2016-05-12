/*
 * Copyright (c) 1997, 2011, Oracle and/or its affiliates. All rights reserved.
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
 * Represent the Subject Key Identifier Extension.
 *
 * This extension, if present, provides a means of identifying the particular
 * public key used in an application.  This extension by default is marked
 * non-critical.
 *
 * <p>Extensions are addiitonal attributes which can be inserted in a X509
 * v3 certificate. For example a "Driving License Certificate" could have
 * the driving license number as a extension.
 *
 * <p>Extensions are represented as a sequence of the extension identifier
 * (Object Identifier), a boolean flag stating whether the extension is to
 * be treated as being critical and the extension value itself (this is again
 * a DER encoding of the extension value).
 *
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 * @see Extension
 * @see CertAttrSet
 */
public class SubjectKeyIdentifierExtension extends Extension
implements CertAttrSet<String> {
    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT =
                         "x509.info.extensions.SubjectKeyIdentifier"; //$NON-NLS-1$
    /**
     * Attribute names.
     */
    public static final String NAME = "SubjectKeyIdentifier"; //$NON-NLS-1$
    public static final String KEY_ID = "key_id"; //$NON-NLS-1$

    // Private data member
    private KeyIdentifier id = null;

    // Encode this extension value
    private void encodeThis() throws IOException {
        if (this.id == null) {
            this.extensionValue = null;
            return;
        }
        final DerOutputStream os = new DerOutputStream();
        this.id.encode(os);
        this.extensionValue = os.toByteArray();
    }

    /**
     * Create a SubjectKeyIdentifierExtension with the passed octet string.
     * The criticality is set to False.
     * @param octetString the octet string identifying the key identifier.
     */
    public SubjectKeyIdentifierExtension(final byte[] octetString)
    throws IOException {
        this.id = new KeyIdentifier(octetString);

        this.extensionId = PKIXExtensions.SubjectKey_Id;
        this.critical = false;
        encodeThis();
    }

    /**
     * Create the extension from the passed DER encoded value.
     *
     * @param critical true if the extension is to be treated as critical.
     * @param value an array of DER encoded bytes of the actual value.
     * @exception ClassCastException if value is not an array of bytes
     * @exception IOException on error.
     */
    public SubjectKeyIdentifierExtension(final Boolean critical, final Object value)
    throws IOException {
        this.extensionId = PKIXExtensions.SubjectKey_Id;
        this.critical = critical.booleanValue();
        this.extensionValue = (byte[]) value;
        final DerValue val = new DerValue(this.extensionValue);
        this.id = new KeyIdentifier(val);
    }

    /**
     * Returns a printable representation.
     */
    @Override
	public String toString() {
        return super.toString() + "SubjectKeyIdentifier [\n" //$NON-NLS-1$
                + String.valueOf(this.id) + "]\n"; //$NON-NLS-1$
    }

    /**
     * Write the extension to the OutputStream.
     *
     * @param out the OutputStream to write the extension to.
     * @exception IOException on encoding errors.
     */
    @Override
	public void encode(final OutputStream out) throws IOException {
        final DerOutputStream tmp = new DerOutputStream();
        if (this.extensionValue == null) {
            this.extensionId = PKIXExtensions.SubjectKey_Id;
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
              throw new IOException("Attribute value should be of" + //$NON-NLS-1$
                                    " type KeyIdentifier."); //$NON-NLS-1$
            }
            this.id = (KeyIdentifier)obj;
        } else {
          throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                "CertAttrSet:SubjectKeyIdentifierExtension."); //$NON-NLS-1$
        }
        encodeThis();
    }

    /**
     * Get the attribute value.
     */
    @Override
	public KeyIdentifier get(final String name) throws IOException {
        if (name.equalsIgnoreCase(KEY_ID)) {
            return (this.id);
        } else {
          throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                "CertAttrSet:SubjectKeyIdentifierExtension."); //$NON-NLS-1$
        }
    }

    /**
     * Delete the attribute value.
     */
    @Override
	public void delete(final String name) throws IOException {
        if (name.equalsIgnoreCase(KEY_ID)) {
            this.id = null;
        } else {
          throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                "CertAttrSet:SubjectKeyIdentifierExtension."); //$NON-NLS-1$
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
