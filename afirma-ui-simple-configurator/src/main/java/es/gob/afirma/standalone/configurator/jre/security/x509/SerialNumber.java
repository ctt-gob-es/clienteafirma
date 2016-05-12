/*
 * Copyright (c) 1997, 2002, Oracle and/or its affiliates. All rights reserved.
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
import java.math.BigInteger;

import es.gob.afirma.standalone.configurator.jre.security.util.Debug;
import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;

/**
 * This class defines the SerialNumber class used by certificates.
 *
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 */
public class SerialNumber {
    private BigInteger  serialNum;

    // Construct the class from the DerValue
    private void construct(final DerValue derVal) throws IOException {
        this.serialNum = derVal.getBigInteger();
        if (derVal.data.available() != 0) {
            throw new IOException("Excess SerialNumber data"); //$NON-NLS-1$
        }
    }

    /**
     * The default constructor for this class using BigInteger.
     *
     * @param num the BigInteger number used to create the serial number.
     */
    public SerialNumber(final BigInteger num) {
        this.serialNum = num;
    }

    /**
     * The default constructor for this class using int.
     *
     * @param num the BigInteger number used to create the serial number.
     */
    public SerialNumber(final int num) {
        this.serialNum = BigInteger.valueOf(num);
    }

    /**
     * Create the object, decoding the values from the passed DER stream.
     *
     * @param in the DerInputStream to read the SerialNumber from.
     * @exception IOException on decoding errors.
     */
    public SerialNumber(final DerInputStream in) throws IOException {
        final DerValue derVal = in.getDerValue();
        construct(derVal);
    }

    /**
     * Create the object, decoding the values from the passed DerValue.
     *
     * @param val the DerValue to read the SerialNumber from.
     * @exception IOException on decoding errors.
     */
    public SerialNumber(final DerValue val) throws IOException {
        construct(val);
    }

    /**
     * Create the object, decoding the values from the passed stream.
     *
     * @param in the InputStream to read the SerialNumber from.
     * @exception IOException on decoding errors.
     */
    public SerialNumber(final InputStream in) throws IOException {
        final DerValue derVal = new DerValue(in);
        construct(derVal);
    }

    /**
     * Return the SerialNumber as user readable string.
     */
    @Override
	public String toString() {
        return ("SerialNumber: [" + Debug.toHexString(this.serialNum) + "]"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * Encode the SerialNumber in DER form to the stream.
     *
     * @param out the DerOutputStream to marshal the contents to.
     * @exception IOException on errors.
     */
    public void encode(final DerOutputStream out) throws IOException {
        out.putInteger(this.serialNum);
    }

    /**
     * Return the serial number.
     */
    public BigInteger getNumber() {
        return this.serialNum;
    }
}
