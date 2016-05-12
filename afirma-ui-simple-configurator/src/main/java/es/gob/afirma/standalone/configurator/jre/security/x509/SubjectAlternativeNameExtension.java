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
 * This represents the Subject Alternative Name Extension.
 *
 * This extension, if present, allows the subject to specify multiple
 * alternative names.
 *
 * <p>Extensions are represented as a sequence of the extension identifier
 * (Object Identifier), a boolean flag stating whether the extension is to
 * be treated as being critical and the extension value itself (this is again
 * a DER encoding of the extension value).
 * <p>
 * The ASN.1 syntax for this is:
 * <pre>
 * SubjectAltName ::= GeneralNames
 * GeneralNames ::= SEQUENCE SIZE (1..MAX) OF GeneralName
 * </pre>
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 * @see Extension
 * @see CertAttrSet
 */
public class SubjectAlternativeNameExtension extends Extension
implements CertAttrSet<String> {
    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT =
                         "x509.info.extensions.SubjectAlternativeName"; //$NON-NLS-1$
    /**
     * Attribute names.
     */
    public static final String NAME = "SubjectAlternativeName"; //$NON-NLS-1$
    public static final String SUBJECT_NAME = "subject_name"; //$NON-NLS-1$

    // private data members
    GeneralNames        names = null;

    // Encode this extension
    private void encodeThis() throws IOException {
        if (this.names == null || this.names.isEmpty()) {
            this.extensionValue = null;
            return;
        }
        final DerOutputStream os = new DerOutputStream();
        this.names.encode(os);
        this.extensionValue = os.toByteArray();
    }

    /**
     * Create a SubjectAlternativeNameExtension with the passed GeneralNames.
     * The extension is marked non-critical.
     *
     * @param names the GeneralNames for the subject.
     * @exception IOException on error.
     */
    public SubjectAlternativeNameExtension(final GeneralNames names)
    throws IOException {
        this(Boolean.FALSE, names);
    }

    /**
     * Create a SubjectAlternativeNameExtension with the specified
     * criticality and GeneralNames.
     *
     * @param critical true if the extension is to be treated as critical.
     * @param names the GeneralNames for the subject.
     * @exception IOException on error.
     */
    public SubjectAlternativeNameExtension(final Boolean critical, final GeneralNames names)
    throws IOException {
        this.names = names;
        this.extensionId = PKIXExtensions.SubjectAlternativeName_Id;
        this.critical = critical.booleanValue();
        encodeThis();
    }

    /**
     * Create a default SubjectAlternativeNameExtension. The extension
     * is marked non-critical.
     */
    public SubjectAlternativeNameExtension() {
        this.extensionId = PKIXExtensions.SubjectAlternativeName_Id;
        this.critical = false;
        this.names = new GeneralNames();
    }

    /**
     * Create the extension from the passed DER encoded value.
     *
     * @param critical true if the extension is to be treated as critical.
     * @param value an array of DER encoded bytes of the actual value.
     * @exception ClassCastException if value is not an array of bytes
     * @exception IOException on error.
     */
    public SubjectAlternativeNameExtension(final Boolean critical, final Object value)
    throws IOException {
        this.extensionId = PKIXExtensions.SubjectAlternativeName_Id;
        this.critical = critical.booleanValue();

        this.extensionValue = (byte[]) value;
        final DerValue val = new DerValue(this.extensionValue);
        if (val.data == null) {
            this.names = new GeneralNames();
            return;
        }

        this.names = new GeneralNames(val);
    }

    /**
     * Returns a printable representation of the SubjectAlternativeName.
     */
    @Override
	public String toString() {

        String result = super.toString() + "SubjectAlternativeName [\n"; //$NON-NLS-1$
        if(this.names == null) {
            result += "  null\n"; //$NON-NLS-1$
        } else {
            for(final GeneralName name: this.names.names()) {
                result += "  "+name+"\n"; //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        result += "]\n"; //$NON-NLS-1$
        return result;
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
            this.extensionId = PKIXExtensions.SubjectAlternativeName_Id;
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
        if (name.equalsIgnoreCase(SUBJECT_NAME)) {
            if (!(obj instanceof GeneralNames)) {
              throw new IOException("Attribute value should be of " + //$NON-NLS-1$
                                    "type GeneralNames."); //$NON-NLS-1$
            }
            this.names = (GeneralNames)obj;
        } else {
          throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                        "CertAttrSet:SubjectAlternativeName."); //$NON-NLS-1$
        }
        encodeThis();
    }

    /**
     * Get the attribute value.
     */
    @Override
	public GeneralNames get(final String name) throws IOException {
        if (name.equalsIgnoreCase(SUBJECT_NAME)) {
            return (this.names);
        } else {
          throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                        "CertAttrSet:SubjectAlternativeName."); //$NON-NLS-1$
        }
    }

    /**
     * Delete the attribute value.
     */
    @Override
	public void delete(final String name) throws IOException {
        if (name.equalsIgnoreCase(SUBJECT_NAME)) {
            this.names = null;
        } else {
          throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                        "CertAttrSet:SubjectAlternativeName."); //$NON-NLS-1$
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
        elements.addElement(SUBJECT_NAME);

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
