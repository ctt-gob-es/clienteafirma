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
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateNotYetValidException;
import java.util.Date;
import java.util.Enumeration;

import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;

/**
 * This class defines the interval for which the certificate is valid.
 *
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 * @see CertAttrSet
 */
public class CertificateValidity implements CertAttrSet<String> {
    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT = "x509.info.validity"; //$NON-NLS-1$
    /**
     * Sub attributes name for this CertAttrSet.
     */
    public static final String NAME = "validity"; //$NON-NLS-1$
    public static final String NOT_BEFORE = "notBefore"; //$NON-NLS-1$
    public static final String NOT_AFTER = "notAfter"; //$NON-NLS-1$
    private static final long YR_2050 = 2524636800000L;

    // Private data members
    private Date        notBefore;
    private Date        notAfter;

    // Returns the first time the certificate is valid.
    private Date getNotBefore() {
        return (new Date(this.notBefore.getTime()));
    }

    // Returns the last time the certificate is valid.
    private Date getNotAfter() {
       return (new Date(this.notAfter.getTime()));
    }

    // Construct the class from the DerValue
    private void construct(final DerValue derVal) throws IOException {
        if (derVal.tag != DerValue.tag_Sequence) {
            throw new IOException("Invalid encoded CertificateValidity, " + //$NON-NLS-1$
                                  "starting sequence tag missing."); //$NON-NLS-1$
        }
        // check if UTCTime encoded or GeneralizedTime
        if (derVal.data.available() == 0) {
			throw new IOException("No data encoded for CertificateValidity"); //$NON-NLS-1$
		}

        final DerInputStream derIn = new DerInputStream(derVal.toByteArray());
        final DerValue[] seq = derIn.getSequence(2);
        if (seq.length != 2) {
			throw new IOException("Invalid encoding for CertificateValidity"); //$NON-NLS-1$
		}

        if (seq[0].tag == DerValue.tag_UtcTime) {
            this.notBefore = derVal.data.getUTCTime();
        } else if (seq[0].tag == DerValue.tag_GeneralizedTime) {
            this.notBefore = derVal.data.getGeneralizedTime();
        } else {
            throw new IOException("Invalid encoding for CertificateValidity"); //$NON-NLS-1$
        }

        if (seq[1].tag == DerValue.tag_UtcTime) {
            this.notAfter = derVal.data.getUTCTime();
        } else if (seq[1].tag == DerValue.tag_GeneralizedTime) {
            this.notAfter = derVal.data.getGeneralizedTime();
        } else {
            throw new IOException("Invalid encoding for CertificateValidity"); //$NON-NLS-1$
        }
    }

    /**
     * Default constructor for the class.
     */
    public CertificateValidity() { }

    /**
     * The default constructor for this class for the specified interval.
     *
     * @param notBefore the date and time before which the certificate
     *                   is not valid.
     * @param notAfter the date and time after which the certificate is
     *                  not valid.
     */
    public CertificateValidity(final Date notBefore, final Date notAfter) {
        this.notBefore = notBefore;
        this.notAfter = notAfter;
    }

    /**
     * Create the object, decoding the values from the passed DER stream.
     *
     * @param in the DerInputStream to read the CertificateValidity from.
     * @exception IOException on decoding errors.
     */
    public CertificateValidity(final DerInputStream in) throws IOException {
        final DerValue derVal = in.getDerValue();
        construct(derVal);
    }

    /**
     * Return the validity period as user readable string.
     */
    @Override
	public String toString() {
        if (this.notBefore == null || this.notAfter == null) {
			return ""; //$NON-NLS-1$
		}
        return ("Validity: [From: " + this.notBefore.toString() + //$NON-NLS-1$
             ",\n               To: " + this.notAfter.toString() + "]"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * Encode the CertificateValidity period in DER form to the stream.
     *
     * @param out the OutputStream to marshal the contents to.
     * @exception IOException on errors.
     */
    @Override
	public void encode(final OutputStream out) throws IOException {

        // in cases where default constructor is used check for
        // null values
        if (this.notBefore == null || this.notAfter == null) {
            throw new IOException("CertAttrSet:CertificateValidity:" + //$NON-NLS-1$
                                  " null values to encode.\n"); //$NON-NLS-1$
        }
        final DerOutputStream pair = new DerOutputStream();

        if (this.notBefore.getTime() < YR_2050) {
            pair.putUTCTime(this.notBefore);
        } else {
			pair.putGeneralizedTime(this.notBefore);
		}

        if (this.notAfter.getTime() < YR_2050) {
            pair.putUTCTime(this.notAfter);
        } else {
            pair.putGeneralizedTime(this.notAfter);
        }
        final DerOutputStream seq = new DerOutputStream();
        seq.write(DerValue.tag_Sequence, pair);

        out.write(seq.toByteArray());
    }

    /**
     * Set the attribute value.
     */
    @Override
	public void set(final String name, final Object obj) throws IOException {
        if (!(obj instanceof Date)) {
            throw new IOException("Attribute must be of type Date."); //$NON-NLS-1$
        }
        if (name.equalsIgnoreCase(NOT_BEFORE)) {
            this.notBefore = (Date)obj;
        } else if (name.equalsIgnoreCase(NOT_AFTER)) {
            this.notAfter = (Date)obj;
        } else {
            throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                            "CertAttrSet: CertificateValidity."); //$NON-NLS-1$
        }
    }

    /**
     * Get the attribute value.
     */
    @Override
	public Date get(final String name) throws IOException {
        if (name.equalsIgnoreCase(NOT_BEFORE)) {
            return (getNotBefore());
        } else if (name.equalsIgnoreCase(NOT_AFTER)) {
            return (getNotAfter());
        } else {
            throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                            "CertAttrSet: CertificateValidity."); //$NON-NLS-1$
        }
    }

    /**
     * Delete the attribute value.
     */
    @Override
	public void delete(final String name) throws IOException {
        if (name.equalsIgnoreCase(NOT_BEFORE)) {
            this.notBefore = null;
        } else if (name.equalsIgnoreCase(NOT_AFTER)) {
            this.notAfter = null;
        } else {
            throw new IOException("Attribute name not recognized by " + //$NON-NLS-1$
                            "CertAttrSet: CertificateValidity."); //$NON-NLS-1$
        }
    }

    /**
     * Return an enumeration of names of attributes existing within this
     * attribute.
     */
    @Override
	public Enumeration<String> getElements() {
        final AttributeNameEnumeration elements = new AttributeNameEnumeration();
        elements.addElement(NOT_BEFORE);
        elements.addElement(NOT_AFTER);

        return (elements.elements());
    }

    /**
     * Return the name of this attribute.
     */
    @Override
	public String getName() {
        return (NAME);
    }

    /**
     * Verify that the current time is within the validity period.
     *
     * @exception CertificateExpiredException if the certificate has expired.
     * @exception CertificateNotYetValidException if the certificate is not
     * yet valid.
     */
    public void valid()
    throws CertificateNotYetValidException, CertificateExpiredException {
        final Date now = new Date();
        valid(now);
    }

    /**
     * Verify that the passed time is within the validity period.
     * @param now the Date against which to compare the validity
     * period.
     *
     * @exception CertificateExpiredException if the certificate has expired
     * with respect to the <code>Date</code> supplied.
     * @exception CertificateNotYetValidException if the certificate is not
     * yet valid with respect to the <code>Date</code> supplied.
     *
     */
    public void valid(final Date now)
    throws CertificateNotYetValidException, CertificateExpiredException {
        /*
         * we use the internal Dates rather than the passed in Date
         * because someone could override the Date methods after()
         * and before() to do something entirely different.
         */
        if (this.notBefore.after(now)) {
            throw new CertificateNotYetValidException("NotBefore: " + //$NON-NLS-1$
                                                      this.notBefore.toString());
        }
        if (this.notAfter.before(now)) {
            throw new CertificateExpiredException("NotAfter: " + //$NON-NLS-1$
                                                  this.notAfter.toString());
        }
    }
}
