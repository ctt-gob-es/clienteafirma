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
import java.math.BigInteger;
import java.util.Enumeration;

import es.gob.afirma.standalone.configurator.jre.security.util.Debug;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;

/**
 * Represent the CRL Number Extension.
 *
 * <p>This extension, if present, conveys a monotonically increasing
 * sequence number for each CRL issued by a given CA through a specific
 * CA X.500 Directory entry or CRL distribution point. This extension
 * allows users to easily determine when a particular CRL supersedes
 * another CRL.
 *
 * @author Hemma Prafullchandra
 * @see Extension
 * @see CertAttrSet
 */
public class CRLNumberExtension extends Extension
implements CertAttrSet<String> {

    /**
     * Attribute name.
     */
    public static final String NAME = "CRLNumber";
    public static final String NUMBER = "value";

    private static final String LABEL = "CRL Number";

    private BigInteger crlNumber = null;
    private final String extensionName;
    private final String extensionLabel;

    // Encode this extension value
    private void encodeThis() throws IOException {
        if (this.crlNumber == null) {
            this.extensionValue = null;
            return;
        }
        final DerOutputStream os = new DerOutputStream();
        os.putInteger(this.crlNumber);
        this.extensionValue = os.toByteArray();
    }

    /**
     * Create a CRLNumberExtension with the integer value .
     * The criticality is set to false.
     *
     * @param crlNum the value to be set for the extension.
     */
    public CRLNumberExtension(final int crlNum) throws IOException {
        this(PKIXExtensions.CRLNumber_Id, false, BigInteger.valueOf(crlNum),
        NAME, LABEL);
    }

    /**
     * Create a CRLNumberExtension with the BigInteger value .
     * The criticality is set to false.
     *
     * @param crlNum the value to be set for the extension.
     */
    public CRLNumberExtension(final BigInteger crlNum) throws IOException {
        this(PKIXExtensions.CRLNumber_Id, false, crlNum, NAME, LABEL);
    }

    /**
     * Creates the extension (also called by the subclass).
     */
    protected CRLNumberExtension(final ObjectIdentifier extensionId,
        final boolean isCritical, final BigInteger crlNum, final String extensionName,
        final String extensionLabel) throws IOException {

        this.extensionId = extensionId;
        this.critical = isCritical;
        this.crlNumber = crlNum;
        this.extensionName = extensionName;
        this.extensionLabel = extensionLabel;
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
    public CRLNumberExtension(final Boolean critical, final Object value)
    throws IOException {
        this(PKIXExtensions.CRLNumber_Id, critical, value, NAME, LABEL);
    }

    /**
     * Creates the extension (also called by the subclass).
     */
    protected CRLNumberExtension(final ObjectIdentifier extensionId,
        final Boolean critical, final Object value, final String extensionName,
        final String extensionLabel) throws IOException {

        this.extensionId = extensionId;
        this.critical = critical.booleanValue();
        this.extensionValue = (byte[]) value;
        final DerValue val = new DerValue(this.extensionValue);
        this.crlNumber = val.getBigInteger();
        this.extensionName = extensionName;
        this.extensionLabel = extensionLabel;
    }

    /**
     * Set the attribute value.
     */
    @Override
	public void set(final String name, final Object obj) throws IOException {
        if (name.equalsIgnoreCase(NUMBER)) {
            if (!(obj instanceof BigInteger)) {
                throw new IOException("Attribute must be of type BigInteger.");
            }
            this.crlNumber = (BigInteger)obj;
        } else {
          throw new IOException("Attribute name not recognized by"
                                + " CertAttrSet:" + this.extensionName + ".");
        }
        encodeThis();
    }

    /**
     * Get the attribute value.
     */
    @Override
	public BigInteger get(final String name) throws IOException {
        if (name.equalsIgnoreCase(NUMBER)) {
            if (this.crlNumber == null) {
				return null;
			} else {
				return this.crlNumber;
			}
        } else {
          throw new IOException("Attribute name not recognized by"
                                + " CertAttrSet:" + this.extensionName + ".");
        }
    }

    /**
     * Delete the attribute value.
     */
    @Override
	public void delete(final String name) throws IOException {
        if (name.equalsIgnoreCase(NUMBER)) {
            this.crlNumber = null;
        } else {
          throw new IOException("Attribute name not recognized by"
                                + " CertAttrSet:" + this.extensionName + ".");
        }
        encodeThis();
    }

    /**
     * Returns a printable representation of the CRLNumberExtension.
     */
    @Override
	public String toString() {
        final String s = super.toString() + this.extensionLabel + ": " +
                   ((this.crlNumber == null) ? "" : Debug.toHexString(this.crlNumber))
                   + "\n";
        return (s);
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
        encode(out, PKIXExtensions.CRLNumber_Id, true);
    }

    /**
     * Write the extension to the DerOutputStream.
     * (Also called by the subclass)
     */
    protected void encode(final OutputStream out, final ObjectIdentifier extensionId,
        final boolean isCritical) throws IOException {

       final DerOutputStream  tmp = new DerOutputStream();

       if (this.extensionValue == null) {
           this.extensionId = extensionId;
           this.critical = isCritical;
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
        elements.addElement(NUMBER);
        return (elements.elements());
    }

    /**
     * Return the name of this attribute.
     */
    @Override
	public String getName() {
        return (this.extensionName);
    }
}
