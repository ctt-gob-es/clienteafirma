/*
 * Copyright (c) 1997, 2004, Oracle and/or its affiliates. All rights reserved.
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

/**
 * Represent the GeneralSubtree ASN.1 object, whose syntax is:
 * <pre>
 * GeneralSubtree ::= SEQUENCE {
 *    base             GeneralName,
 *    minimum  [0]     BaseDistance DEFAULT 0,
 *    maximum  [1]     BaseDistance OPTIONAL
 * }
 * BaseDistance ::= INTEGER (0..MAX)
 * </pre>
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 */
public class GeneralSubtree {
    private static final byte TAG_MIN = 0;
    private static final byte TAG_MAX = 1;
    private static final int  MIN_DEFAULT = 0;

    private final GeneralName name;
    private int         minimum = MIN_DEFAULT;
    private int         maximum = -1;

    private int myhash = -1;

    /**
     * The default constructor for the class.
     *
     * @params name the GeneralName
     * @params min the minimum BaseDistance
     * @params max the maximum BaseDistance
     */
    public GeneralSubtree(final GeneralName name, final int min, final int max) {
        this.name = name;
        this.minimum = min;
        this.maximum = max;
    }

    /**
     * Create the object from its DER encoded form.
     *
     * @param val the DER encoded from of the same.
     */
    public GeneralSubtree(final DerValue val) throws IOException {
        if (val.tag != DerValue.tag_Sequence) {
            throw new IOException("Invalid encoding for GeneralSubtree."); //$NON-NLS-1$
        }
        this.name = new GeneralName(val.data.getDerValue(), true);

        // NB. this is always encoded with the IMPLICIT tag
        // The checks only make sense if we assume implicit tagging,
        // with explicit tagging the form is always constructed.
        while (val.data.available() != 0) {
            final DerValue opt = val.data.getDerValue();

            if (opt.isContextSpecific(TAG_MIN) && !opt.isConstructed()) {
                opt.resetTag(DerValue.tag_Integer);
                this.minimum = opt.getInteger();

            } else if (opt.isContextSpecific(TAG_MAX) && !opt.isConstructed()) {
                opt.resetTag(DerValue.tag_Integer);
                this.maximum = opt.getInteger();
            } else {
				throw new IOException("Invalid encoding of GeneralSubtree.");
			}
        }
    }

    /**
     * Return the GeneralName.
     *
     * @return the GeneralName
     */
    public GeneralName getName() {
        //XXXX May want to consider cloning this
        return this.name;
    }

    /**
     * Return the minimum BaseDistance.
     *
     * @return the minimum BaseDistance. Default is 0 if not set.
     */
    public int getMinimum() {
        return this.minimum;
    }

    /**
     * Return the maximum BaseDistance.
     *
     * @return the maximum BaseDistance, or -1 if not set.
     */
    public int getMaximum() {
        return this.maximum;
    }

    /**
     * Return a printable string of the GeneralSubtree.
     */
    @Override
	public String toString() {
        String s = "\n   GeneralSubtree: [\n" +
            "    GeneralName: " + ((this.name == null) ? "" : this.name.toString()) +
            "\n    Minimum: " + this.minimum;
            if (this.maximum == -1) {
                s += "\t    Maximum: undefined";
            } else {
				s += "\t    Maximum: " + this.maximum;
			}
            s += "    ]\n";
        return (s);
    }

    /**
     * Compare this GeneralSubtree with another
     *
     * @param other GeneralSubtree to compare to this
     * @returns true if match
     */
    @Override
	public boolean equals(final Object other) {
        if (!(other instanceof GeneralSubtree)) {
			return false;
		}
        final GeneralSubtree otherGS = (GeneralSubtree)other;
        if (this.name == null) {
            if (otherGS.name != null) {
                return false;
            }
        } else {
            if (!((this.name).equals(otherGS.name))) {
				return false;
			}
        }
        if (this.minimum != otherGS.minimum) {
			return false;
		}
        if (this.maximum != otherGS.maximum) {
			return false;
		}
        return true;
    }

    /**
     * Returns the hash code for this GeneralSubtree.
     *
     * @return a hash code value.
     */
    @Override
	public int hashCode() {
        if (this.myhash == -1) {
            this.myhash = 17;
            if (this.name != null) {
                this.myhash = 37 * this.myhash + this.name.hashCode();
            }
            if (this.minimum != MIN_DEFAULT) {
                this.myhash = 37 * this.myhash + this.minimum;
            }
            if (this.maximum != -1) {
                this.myhash = 37 * this.myhash + this.maximum;
            }
        }
        return this.myhash;
    }

    /**
     * Encode the GeneralSubtree.
     *
     * @params out the DerOutputStream to encode this object to.
     */
    public void encode(final DerOutputStream out) throws IOException {
        final DerOutputStream seq = new DerOutputStream();

        this.name.encode(seq);

        if (this.minimum != MIN_DEFAULT) {
            final DerOutputStream tmp = new DerOutputStream();
            tmp.putInteger(this.minimum);
            seq.writeImplicit(DerValue.createTag(DerValue.TAG_CONTEXT,
                              false, TAG_MIN), tmp);
        }
        if (this.maximum != -1) {
            final DerOutputStream tmp = new DerOutputStream();
            tmp.putInteger(this.maximum);
            seq.writeImplicit(DerValue.createTag(DerValue.TAG_CONTEXT,
                              false, TAG_MAX), tmp);
        }
        out.write(DerValue.tag_Sequence, seq);
    }
}
