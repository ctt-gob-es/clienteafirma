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
 * This class defines the AlgorithmId for the Certificate.
 *
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 */
public class CertificateAlgorithmId implements CertAttrSet<String> {
    private AlgorithmId algId;

    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT = "x509.info.algorithmID"; //$NON-NLS-1$
    /**
     * Sub attributes name for this CertAttrSet.
     */
    public static final String NAME = "algorithmID"; //$NON-NLS-1$

    /**
     * Identifier to be used with get, set, and delete methods. When
     * using this identifier the associated object being passed in or
     * returned is an instance of AlgorithmId.
     * @see es.gob.afirma.standalone.configurator.jre.security.x509.AlgorithmId
     */
    public static final String ALGORITHM = "algorithm"; //$NON-NLS-1$

    /**
     * Default constructor for the certificate attribute.
     *
     * @param algId the Algorithm identifier
     */
    public CertificateAlgorithmId(final AlgorithmId algId) {
        this.algId = algId;
    }

    /**
     * Create the object, decoding the values from the passed DER stream.
     *
     * @param in the DerInputStream to read the serial number from.
     * @exception IOException on decoding errors.
     */
    public CertificateAlgorithmId(final DerInputStream in) throws IOException {
        final DerValue val = in.getDerValue();
        this.algId = AlgorithmId.parse(val);
    }

    /**
     * Create the object, decoding the values from the passed stream.
     *
     * @param in the InputStream to read the serial number from.
     * @exception IOException on decoding errors.
     */
    public CertificateAlgorithmId(final InputStream in) throws IOException {
        final DerValue val = new DerValue(in);
        this.algId = AlgorithmId.parse(val);
    }

    /**
     * Return the algorithm identifier as user readable string.
     */
    @Override
	public String toString() {
        if (this.algId == null) {
			return ""; //$NON-NLS-1$
		}
        return (this.algId.toString() +
                ", OID = " + (this.algId.getOID()).toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * Encode the algorithm identifier in DER form to the stream.
     *
     * @param out the DerOutputStream to marshal the contents to.
     * @exception IOException on errors.
     */
    @Override
	public void encode(final OutputStream out) throws IOException {
        final DerOutputStream tmp = new DerOutputStream();
        this.algId.encode(tmp);

        out.write(tmp.toByteArray());
    }

    /**
     * Set the attribute value.
     */
    @Override
	public void set(final String name, final Object obj) throws IOException {
        if (!(obj instanceof AlgorithmId)) {
            throw new IOException("Attribute must be of type AlgorithmId."); //$NON-NLS-1$
        }
        if (name.equalsIgnoreCase(ALGORITHM)) {
            this.algId = (AlgorithmId)obj;
        } else {
            throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                              "CertAttrSet:CertificateAlgorithmId."); //$NON-NLS-1$
        }
    }

    /**
     * Get the attribute value.
     */
    @Override
	public AlgorithmId get(final String name) throws IOException {
        if (name.equalsIgnoreCase(ALGORITHM)) {
            return (this.algId);
        } else {
            throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                               "CertAttrSet:CertificateAlgorithmId."); //$NON-NLS-1$
        }
    }

    /**
     * Delete the attribute value.
     */
    @Override
	public void delete(final String name) throws IOException {
        if (name.equalsIgnoreCase(ALGORITHM)) {
            this.algId = null;
        } else {
            throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                               "CertAttrSet:CertificateAlgorithmId."); //$NON-NLS-1$
        }
    }

    /**
     * Return an enumeration of names of attributes existing within this
     * attribute.
     */
    @Override
	public Enumeration<String> getElements() {
        final AttributeNameEnumeration elements = new AttributeNameEnumeration();
        elements.addElement(ALGORITHM);
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
