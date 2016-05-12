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
import java.security.cert.CertificateException;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.CertificateParsingException;
import java.util.Date;
import java.util.Enumeration;

import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;

/**
 * This class defines the Private Key Usage Extension.
 *
 * <p>The Private Key Usage Period extension allows the certificate issuer
 * to specify a different validity period for the private key than the
 * certificate. This extension is intended for use with digital
 * signature keys.  This extension consists of two optional components
 * notBefore and notAfter.  The private key associated with the
 * certificate should not be used to sign objects before or after the
 * times specified by the two components, respectively.
 *
 * <pre>
 * PrivateKeyUsagePeriod ::= SEQUENCE {
 *     notBefore  [0]  GeneralizedTime OPTIONAL,
 *     notAfter   [1]  GeneralizedTime OPTIONAL }
 * </pre>
 *
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 * @see Extension
 * @see CertAttrSet
 */
public class PrivateKeyUsageExtension extends Extension
implements CertAttrSet<String> {
    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT = "x509.info.extensions.PrivateKeyUsage"; //$NON-NLS-1$
    /**
     * Sub attributes name for this CertAttrSet.
     */
    public static final String NAME = "PrivateKeyUsage"; //$NON-NLS-1$
    public static final String NOT_BEFORE = "not_before"; //$NON-NLS-1$
    public static final String NOT_AFTER = "not_after"; //$NON-NLS-1$

    // Private data members
    private static final byte TAG_BEFORE = 0;
    private static final byte TAG_AFTER = 1;

    private Date        notBefore = null;
    private Date        notAfter = null;

    // Encode this extension value.
    private void encodeThis() throws IOException {
        if (this.notBefore == null && this.notAfter == null) {
            this.extensionValue = null;
            return;
        }
        final DerOutputStream seq = new DerOutputStream();

        final DerOutputStream tagged = new DerOutputStream();
        if (this.notBefore != null) {
            final DerOutputStream tmp = new DerOutputStream();
            tmp.putGeneralizedTime(this.notBefore);
            tagged.writeImplicit(DerValue.createTag(DerValue.TAG_CONTEXT,
                                 false, TAG_BEFORE), tmp);
        }
        if (this.notAfter != null) {
            final DerOutputStream tmp = new DerOutputStream();
            tmp.putGeneralizedTime(this.notAfter);
            tagged.writeImplicit(DerValue.createTag(DerValue.TAG_CONTEXT,
                                 false, TAG_AFTER), tmp);
        }
        seq.write(DerValue.tag_Sequence, tagged);
        this.extensionValue = seq.toByteArray();
    }

    /**
     * The default constructor for PrivateKeyUsageExtension.
     *
     * @param notBefore the date/time before which the private key
     *         should not be used.
     * @param notAfter the date/time after which the private key
     *         should not be used.
     */
    public PrivateKeyUsageExtension(final Date notBefore, final Date notAfter)
    throws IOException {
        this.notBefore = notBefore;
        this.notAfter = notAfter;

        this.extensionId = PKIXExtensions.PrivateKeyUsage_Id;
        this.critical = false;
        encodeThis();
    }

    /**
     * Create the extension from the passed DER encoded value.
     *
     * @param critical true if the extension is to be treated as critical.
     * @param value an array of DER encoded bytes of the actual value.
     * @exception ClassCastException if value is not an array of bytes
     * @exception CertificateException on certificate parsing errors.
     * @exception IOException on error.
     */
    public PrivateKeyUsageExtension(final Boolean critical, final Object value)
    throws CertificateException, IOException {
        this.extensionId = PKIXExtensions.PrivateKeyUsage_Id;
        this.critical = critical.booleanValue();

        this.extensionValue = (byte[]) value;
        DerInputStream str = new DerInputStream(this.extensionValue);
        final DerValue[] seq = str.getSequence(2);

        // NB. this is always encoded with the IMPLICIT tag
        // The checks only make sense if we assume implicit tagging,
        // with explicit tagging the form is always constructed.
        for (final DerValue element : seq) {
            final DerValue opt = element;

            if (opt.isContextSpecific(TAG_BEFORE) &&
                !opt.isConstructed()) {
                if (this.notBefore != null) {
                    throw new CertificateParsingException(
                        "Duplicate notBefore in PrivateKeyUsage."); //$NON-NLS-1$
                }
                opt.resetTag(DerValue.tag_GeneralizedTime);
                str = new DerInputStream(opt.toByteArray());
                this.notBefore = str.getGeneralizedTime();

            } else if (opt.isContextSpecific(TAG_AFTER) &&
                       !opt.isConstructed()) {
                if (this.notAfter != null) {
                    throw new CertificateParsingException(
                        "Duplicate notAfter in PrivateKeyUsage."); //$NON-NLS-1$
                }
                opt.resetTag(DerValue.tag_GeneralizedTime);
                str = new DerInputStream(opt.toByteArray());
                this.notAfter = str.getGeneralizedTime();
            } else {
				throw new IOException("Invalid encoding of " + //$NON-NLS-1$
                                      "PrivateKeyUsageExtension"); //$NON-NLS-1$
			}
        }
    }

    /**
     * Return the printable string.
     */
    @Override
	public String toString() {
        return(super.toString() +
                "PrivateKeyUsage: [\n" + //$NON-NLS-1$
                ((this.notBefore == null) ? "" : "From: " + this.notBefore.toString() + ", ") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                + ((this.notAfter == null) ? "" : "To: " + this.notAfter.toString()) //$NON-NLS-1$ //$NON-NLS-2$
                + "]\n"); //$NON-NLS-1$
    }

    /**
     * Verify that that the current time is within the validity period.
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
     * Verify that that the passed time is within the validity period.
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
            this.extensionId = PKIXExtensions.PrivateKeyUsage_Id;
            this.critical = false;
            encodeThis();
        }
        super.encode(tmp);
        out.write(tmp.toByteArray());
    }

    /**
     * Set the attribute value.
     * @exception CertificateException on attribute handling errors.
     */
    @Override
	public void set(final String name, final Object obj)
    throws CertificateException, IOException {
        if (!(obj instanceof Date)) {
            throw new CertificateException("Attribute must be of type Date."); //$NON-NLS-1$
        }
        if (name.equalsIgnoreCase(NOT_BEFORE)) {
            this.notBefore = (Date)obj;
        } else if (name.equalsIgnoreCase(NOT_AFTER)) {
            this.notAfter = (Date)obj;
        } else {
          throw new CertificateException("Attribute name not recognized by" //$NON-NLS-1$
                           + " CertAttrSet:PrivateKeyUsage."); //$NON-NLS-1$
        }
        encodeThis();
    }

    /**
     * Get the attribute value.
     * @exception CertificateException on attribute handling errors.
     */
    @Override
	public Date get(final String name) throws CertificateException {
      if (name.equalsIgnoreCase(NOT_BEFORE)) {
          return (new Date(this.notBefore.getTime()));
      } else if (name.equalsIgnoreCase(NOT_AFTER)) {
          return (new Date(this.notAfter.getTime()));
      } else {
          throw new CertificateException("Attribute name not recognized by" //$NON-NLS-1$
                           + " CertAttrSet:PrivateKeyUsage."); //$NON-NLS-1$
      }
  }

    /**
     * Delete the attribute value.
     * @exception CertificateException on attribute handling errors.
     */
    @Override
	public void delete(final String name) throws CertificateException, IOException {
        if (name.equalsIgnoreCase(NOT_BEFORE)) {
            this.notBefore = null;
        } else if (name.equalsIgnoreCase(NOT_AFTER)) {
            this.notAfter = null;
        } else {
          throw new CertificateException("Attribute name not recognized by" //$NON-NLS-1$
                           + " CertAttrSet:PrivateKeyUsage."); //$NON-NLS-1$
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
        elements.addElement(NOT_BEFORE);
        elements.addElement(NOT_AFTER);

        return(elements.elements());
    }

    /**
     * Return the name of this attribute.
     */
    @Override
	public String getName() {
      return(NAME);
    }
}
