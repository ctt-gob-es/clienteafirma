/*
 * Copyright (c) 1997, 2014, Oracle and/or its affiliates. All rights reserved.
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
import java.security.cert.CRLReason;
import java.util.Enumeration;

import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;


/**
 * The reasonCode is a non-critical CRL entry extension that identifies
 * the reason for the certificate revocation.
 * @author Hemma Prafullchandra
 * @see java.security.cert.CRLReason
 * @see Extension
 * @see CertAttrSet
 */
public class CRLReasonCodeExtension extends Extension
        implements CertAttrSet<String> {

    /**
     * Attribute name
     */
    public static final String NAME = "CRLReasonCode";
    public static final String REASON = "reason";

    private static CRLReason[] values = CRLReason.values();

    private int reasonCode = 0;

    private void encodeThis() throws IOException {
        if (this.reasonCode == 0) {
            this.extensionValue = null;
            return;
        }
        final DerOutputStream dos = new DerOutputStream();
        dos.putEnumerated(this.reasonCode);
        this.extensionValue = dos.toByteArray();
    }

    /**
     * Create a CRLReasonCodeExtension with the passed in reason.
     * Criticality automatically set to false.
     *
     * @param reason the enumerated value for the reason code.
     */
    public CRLReasonCodeExtension(final int reason) throws IOException {
        this(false, reason);
    }

    /**
     * Create a CRLReasonCodeExtension with the passed in reason.
     *
     * @param critical true if the extension is to be treated as critical.
     * @param reason the enumerated value for the reason code.
     */
    public CRLReasonCodeExtension(final boolean critical, final int reason)
    throws IOException {
        this.extensionId = PKIXExtensions.ReasonCode_Id;
        this.critical = critical;
        this.reasonCode = reason;
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
    public CRLReasonCodeExtension(final Boolean critical, final Object value)
    throws IOException {
        this.extensionId = PKIXExtensions.ReasonCode_Id;
        this.critical = critical.booleanValue();
        this.extensionValue = (byte[]) value;
        final DerValue val = new DerValue(this.extensionValue);
        this.reasonCode = val.getEnumerated();
    }

    /**
     * Set the attribute value.
     */
    @Override
	public void set(final String name, final Object obj) throws IOException {
        if (!(obj instanceof Integer)) {
            throw new IOException("Attribute must be of type Integer.");
        }
        if (name.equalsIgnoreCase(REASON)) {
            this.reasonCode = ((Integer)obj).intValue();
        } else {
            throw new IOException
                ("Name not supported by CRLReasonCodeExtension");
        }
        encodeThis();
    }

    /**
     * Get the attribute value.
     */
    @Override
	public Integer get(final String name) throws IOException {
        if (name.equalsIgnoreCase(REASON)) {
            return new Integer(this.reasonCode);
        } else {
            throw new IOException
                ("Name not supported by CRLReasonCodeExtension");
        }
    }

    /**
     * Delete the attribute value.
     */
    @Override
	public void delete(final String name) throws IOException {
        if (name.equalsIgnoreCase(REASON)) {
            this.reasonCode = 0;
        } else {
            throw new IOException
                ("Name not supported by CRLReasonCodeExtension");
        }
        encodeThis();
    }

    /**
     * Returns a printable representation of the Reason code.
     */
    @Override
	public String toString() {
        return super.toString() + "    Reason Code: " + getReasonCode();
    }

    /**
     * Write the extension to the DerOutputStream.
     *
     * @param out the DerOutputStream to write the extension to.
     * @exception IOException on encoding errors.
     */
    @Override
	public void encode(final OutputStream out) throws IOException {
        final DerOutputStream  tmp = new DerOutputStream();

        if (this.extensionValue == null) {
            this.extensionId = PKIXExtensions.ReasonCode_Id;
            this.critical = false;
            encodeThis();
        }
        super.encode(tmp);
        out.write(tmp.toByteArray());
    }

    /**
     * Return an enumeration of names of attributes existing within this
     * attribute.
     */
    @Override
	public Enumeration<String> getElements() {
        final AttributeNameEnumeration elements = new AttributeNameEnumeration();
        elements.addElement(REASON);

        return elements.elements();
    }

    /**
     * Return the name of this attribute.
     */
    @Override
	public String getName() {
        return NAME;
    }

    /**
     * Return the reason as a CRLReason enum.
     */
    public CRLReason getReasonCode() {
        // if out-of-range, return UNSPECIFIED
        if (this.reasonCode > 0 && this.reasonCode < values.length) {
            return values[this.reasonCode];
        } else {
            return CRLReason.UNSPECIFIED;
        }
    }
}
