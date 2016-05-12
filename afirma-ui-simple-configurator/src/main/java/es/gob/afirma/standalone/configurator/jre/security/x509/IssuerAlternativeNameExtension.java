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
 * This represents the Issuer Alternative Name Extension.
 *
 * This extension, if present, allows the issuer to specify multiple
 * alternative names.
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
public class IssuerAlternativeNameExtension
extends Extension implements CertAttrSet<String> {
    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT =
                         "x509.info.extensions.IssuerAlternativeName";
    /**
     * Attribute names.
     */
    public static final String NAME = "IssuerAlternativeName";
    public static final String ISSUER_NAME = "issuer_name";

    // private data members
    GeneralNames names = null;

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
     * Create a IssuerAlternativeNameExtension with the passed GeneralNames.
     *
     * @param names the GeneralNames for the issuer.
     * @exception IOException on error.
     */
    public IssuerAlternativeNameExtension(final GeneralNames names)
    throws IOException {
        this.names = names;
        this.extensionId = PKIXExtensions.IssuerAlternativeName_Id;
        this.critical = false;
        encodeThis();
    }

    /**
     * Create a IssuerAlternativeNameExtension with the passed criticality
     * and GeneralNames.
     *
     * @param critical true if the extension is to be treated as critical.
     * @param names the GeneralNames for the issuer.
     * @exception IOException on error.
     */
    public IssuerAlternativeNameExtension(final Boolean critical, final GeneralNames names)
    throws IOException {
        this.names = names;
        this.extensionId = PKIXExtensions.IssuerAlternativeName_Id;
        this.critical = critical.booleanValue();
        encodeThis();
    }

    /**
     * Create a default IssuerAlternativeNameExtension.
     */
    public IssuerAlternativeNameExtension() {
        this.extensionId = PKIXExtensions.IssuerAlternativeName_Id;
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
    public IssuerAlternativeNameExtension(final Boolean critical, final Object value)
    throws IOException {
        this.extensionId = PKIXExtensions.IssuerAlternativeName_Id;
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
     * Returns a printable representation of the IssuerAlternativeName.
     */
    @Override
	public String toString() {

        String result = super.toString() + "IssuerAlternativeName [\n";
        if(this.names == null) {
            result += "  null\n";
        } else {
            for(final GeneralName name: this.names.names()) {
                result += "  "+name+"\n";
            }
        }
        result += "]\n";
        return result;
    }

    /**
     * Write the extension to the OutputStream.
     *
     * @param out the OutputStream to write the extension to.
     * @exception IOException on encoding error.
     */
    @Override
	public void encode(final OutputStream out) throws IOException {
        final DerOutputStream tmp = new DerOutputStream();
        if (this.extensionValue == null) {
            this.extensionId = PKIXExtensions.IssuerAlternativeName_Id;
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
        if (name.equalsIgnoreCase(ISSUER_NAME)) {
            if (!(obj instanceof GeneralNames)) {
              throw new IOException("Attribute value should be of" +
                                    " type GeneralNames.");
            }
            this.names = (GeneralNames)obj;
        } else {
          throw new IOException("Attribute name not recognized by " +
                        "CertAttrSet:IssuerAlternativeName.");
        }
        encodeThis();
    }

    /**
     * Get the attribute value.
     */
    @Override
	public GeneralNames get(final String name) throws IOException {
        if (name.equalsIgnoreCase(ISSUER_NAME)) {
            return (this.names);
        } else {
          throw new IOException("Attribute name not recognized by " +
                        "CertAttrSet:IssuerAlternativeName.");
        }
    }

    /**
     * Delete the attribute value.
     */
    @Override
	public void delete(final String name) throws IOException {
        if (name.equalsIgnoreCase(ISSUER_NAME)) {
            this.names = null;
        } else {
          throw new IOException("Attribute name not recognized by " +
                        "CertAttrSet:IssuerAlternativeName.");
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
        elements.addElement(ISSUER_NAME);

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
