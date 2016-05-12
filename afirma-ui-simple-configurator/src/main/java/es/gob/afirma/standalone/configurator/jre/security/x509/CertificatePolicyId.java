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

import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;

/**
 * Represent the CertificatePolicyId ASN.1 object.
 *
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 */
public class CertificatePolicyId {
    private final ObjectIdentifier id;

    /**
     * Create a CertificatePolicyId with the ObjectIdentifier.
     *
     * @param id the ObjectIdentifier for the policy id.
     */
    public CertificatePolicyId(final ObjectIdentifier id) {
        this.id = id;
    }

    /**
     * Create the object from its Der encoded value.
     *
     * @param val the DER encoded value for the same.
     */
    public CertificatePolicyId(final DerValue val) throws IOException {
        this.id = val.getOID();
    }

    /**
     * Return the value of the CertificatePolicyId as an ObjectIdentifier.
     */
    public ObjectIdentifier getIdentifier() {
        return (this.id);
    }

    /**
     * Returns a printable representation of the CertificatePolicyId.
     */
    @Override
	public String toString() {
        final String s = "CertificatePolicyId: [" //$NON-NLS-1$
                 + this.id.toString()
                 + "]\n"; //$NON-NLS-1$

        return (s);
    }

    /**
     * Write the CertificatePolicyId to the DerOutputStream.
     *
     * @param out the DerOutputStream to write the object to.
     * @exception IOException on errors.
     */
    public void encode(final DerOutputStream out) throws IOException {
        out.putOID(this.id);
    }

    /**
     * Compares this CertificatePolicyId with another, for
     * equality. Uses ObjectIdentifier.equals() as test for
     * equality.
     *
     * @return true iff the ids are identical.
     */
    @Override
	public boolean equals(final Object other) {
        if (other instanceof CertificatePolicyId) {
			return this.id.equals((Object)
                    ((CertificatePolicyId) other).getIdentifier());
		} else {
			return false;
		}
    }

    /**
     * Returns a hash code value for this object.
     *
     * @return a hash code value
     */
    @Override
	public int hashCode() {
      return this.id.hashCode();
    }
}
