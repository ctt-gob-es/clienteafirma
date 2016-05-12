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
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Enumeration;

import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;


/**
 * This class defines the version of the X509 Certificate.
 *
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 * @see CertAttrSet
 */
public class CertificateVersion implements CertAttrSet<String> {
    /**
     * X509Certificate Version 1
     */
    public static final int     V1 = 0;
    /**
     * X509Certificate Version 2
     */
    public static final int     V2 = 1;
    /**
     * X509Certificate Version 3
     */
    public static final int     V3 = 2;
    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT = "x509.info.version"; //$NON-NLS-1$
    /**
     * Sub attributes name for this CertAttrSet.
     */
    public static final String NAME = "version"; //$NON-NLS-1$
    public static final String VERSION = "number"; //$NON-NLS-1$

    // Private data members
    int version = V1;

    // Returns the version number.
    private int getVersion() {
        return(this.version);
    }

    // Construct the class from the passed DerValue
    private void construct(DerValue derVal) throws IOException {
        if (derVal.isConstructed() && derVal.isContextSpecific()) {
            derVal = derVal.data.getDerValue();
            this.version = derVal.getInteger();
            if (derVal.data.available() != 0) {
                throw new IOException("X.509 version, bad format"); //$NON-NLS-1$
            }
        }
    }

    /**
     * The default constructor for this class,
     *  sets the version to 0 (i.e. X.509 version 1).
     */
    public CertificateVersion() {
        this.version = V1;
    }

    /**
     * The constructor for this class for the required version.
     *
     * @param version the version for the certificate.
     * @exception IOException if the version is not valid.
     */
    public CertificateVersion(final int version) throws IOException {

        // check that it is a valid version
        if (version == V1 || version == V2 || version == V3) {
			this.version = version;
		} else {
            throw new IOException("X.509 Certificate version " + //$NON-NLS-1$
                                   version + " not supported.\n"); //$NON-NLS-1$
        }
    }

    /**
     * Create the object, decoding the values from the passed DER stream.
     *
     * @param in the DerInputStream to read the CertificateVersion from.
     * @exception IOException on decoding errors.
     */
    public CertificateVersion(final DerInputStream in) throws IOException {
        this.version = V1;
        final DerValue derVal = in.getDerValue();

        construct(derVal);
    }

    /**
     * Create the object, decoding the values from the passed stream.
     *
     * @param in the InputStream to read the CertificateVersion from.
     * @exception IOException on decoding errors.
     */
    public CertificateVersion(final InputStream in) throws IOException {
        this.version = V1;
        final DerValue derVal = new DerValue(in);

        construct(derVal);
    }

    /**
     * Create the object, decoding the values from the passed DerValue.
     *
     * @param val the Der encoded value.
     * @exception IOException on decoding errors.
     */
    public CertificateVersion(final DerValue val) throws IOException {
        this.version = V1;

        construct(val);
    }

    /**
     * Return the version number of the certificate.
     */
    @Override
	public String toString() {
        return("Version: V" + (this.version+1)); //$NON-NLS-1$
    }

    /**
     * Encode the CertificateVersion period in DER form to the stream.
     *
     * @param out the OutputStream to marshal the contents to.
     * @exception IOException on errors.
     */
    @Override
	public void encode(final OutputStream out) throws IOException {
        // Nothing for default
        if (this.version == V1) {
            return;
        }
        final DerOutputStream tmp = new DerOutputStream();
        tmp.putInteger(this.version);

        final DerOutputStream seq = new DerOutputStream();
        seq.write(DerValue.createTag(DerValue.TAG_CONTEXT, true, (byte)0),
                  tmp);

        out.write(seq.toByteArray());
    }

    /**
     * Set the attribute value.
     */
    @Override
	public void set(final String name, final Object obj) throws IOException {
        if (!(obj instanceof Integer)) {
            throw new IOException("Attribute must be of type Integer."); //$NON-NLS-1$
        }
        if (name.equalsIgnoreCase(VERSION)) {
            this.version = ((Integer)obj).intValue();
        } else {
            throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                                  "CertAttrSet: CertificateVersion."); //$NON-NLS-1$
        }
    }

    /**
     * Get the attribute value.
     */
    @Override
	public Integer get(final String name) throws IOException {
        if (name.equalsIgnoreCase(VERSION)) {
            return(new Integer(getVersion()));
        } else {
            throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                                  "CertAttrSet: CertificateVersion."); //$NON-NLS-1$
        }
    }

    /**
     * Delete the attribute value.
     */
    @Override
	public void delete(final String name) throws IOException {
        if (name.equalsIgnoreCase(VERSION)) {
            this.version = V1;
        } else {
            throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                                  "CertAttrSet: CertificateVersion."); //$NON-NLS-1$
        }
    }

    /**
     * Return an enumeration of names of attributes existing within this
     * attribute.
     */
    @Override
	public Enumeration<String> getElements() {
        final AttributeNameEnumeration elements = new AttributeNameEnumeration();
        elements.addElement(VERSION);

        return (elements.elements());
    }

    /**
     * Return the name of this attribute.
     */
    @Override
	public String getName() {
        return(NAME);
    }

    /**
     * Compare versions.
     */
    public int compare(final int vers) {
        return(this.version - vers);
    }
}
