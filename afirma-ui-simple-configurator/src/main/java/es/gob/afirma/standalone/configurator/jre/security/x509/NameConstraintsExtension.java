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
import java.security.cert.X509Certificate;
import java.util.Enumeration;

import javax.security.auth.x500.X500Principal;

import es.gob.afirma.standalone.configurator.jre.security.pkcs.PKCS9Attribute;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;

/**
 * This class defines the Name Constraints Extension.
 * <p>
 * The name constraints extension provides permitted and excluded
 * subtrees that place restrictions on names that may be included within
 * a certificate issued by a given CA.  Restrictions may apply to the
 * subject distinguished name or subject alternative names.  Any name
 * matching a restriction in the excluded subtrees field is invalid
 * regardless of information appearing in the permitted subtrees.
 * <p>
 * The ASN.1 syntax for this is:
 * <pre>
 * NameConstraints ::= SEQUENCE {
 *    permittedSubtrees [0]  GeneralSubtrees OPTIONAL,
 *    excludedSubtrees  [1]  GeneralSubtrees OPTIONAL
 * }
 * GeneralSubtrees ::= SEQUENCE SIZE (1..MAX) OF GeneralSubtree
 * </pre>
 *
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 * @see Extension
 * @see CertAttrSet
 */
public class NameConstraintsExtension extends Extension
implements CertAttrSet<String>, Cloneable {
    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT = "x509.info.extensions.NameConstraints";
    /**
     * Attribute names.
     */
    public static final String NAME = "NameConstraints";
    public static final String PERMITTED_SUBTREES = "permitted_subtrees";
    public static final String EXCLUDED_SUBTREES = "excluded_subtrees";

    // Private data members
    private static final byte TAG_PERMITTED = 0;
    private static final byte TAG_EXCLUDED = 1;

    private GeneralSubtrees     permitted = null;
    private GeneralSubtrees     excluded = null;

    private boolean hasMin;
    private boolean hasMax;
    private boolean minMaxValid = false;

    // Recalculate hasMin and hasMax flags.
    private void calcMinMax() throws IOException {
        this.hasMin = false;
        this.hasMax = false;
        if (this.excluded != null) {
            for (int i = 0; i < this.excluded.size(); i++) {
                final GeneralSubtree subtree = this.excluded.get(i);
                if (subtree.getMinimum() != 0) {
					this.hasMin = true;
				}
                if (subtree.getMaximum() != -1) {
					this.hasMax = true;
				}
            }
        }

        if (this.permitted != null) {
            for (int i = 0; i < this.permitted.size(); i++) {
                final GeneralSubtree subtree = this.permitted.get(i);
                if (subtree.getMinimum() != 0) {
					this.hasMin = true;
				}
                if (subtree.getMaximum() != -1) {
					this.hasMax = true;
				}
            }
        }
        this.minMaxValid = true;
    }

    // Encode this extension value.
    private void encodeThis() throws IOException {
        this.minMaxValid = false;
        if (this.permitted == null && this.excluded == null) {
            this.extensionValue = null;
            return;
        }
        final DerOutputStream seq = new DerOutputStream();

        final DerOutputStream tagged = new DerOutputStream();
        if (this.permitted != null) {
            final DerOutputStream tmp = new DerOutputStream();
            this.permitted.encode(tmp);
            tagged.writeImplicit(DerValue.createTag(DerValue.TAG_CONTEXT,
                                 true, TAG_PERMITTED), tmp);
        }
        if (this.excluded != null) {
            final DerOutputStream tmp = new DerOutputStream();
            this.excluded.encode(tmp);
            tagged.writeImplicit(DerValue.createTag(DerValue.TAG_CONTEXT,
                                 true, TAG_EXCLUDED), tmp);
        }
        seq.write(DerValue.tag_Sequence, tagged);
        this.extensionValue = seq.toByteArray();
    }

    /**
     * The default constructor for this class. Both parameters
     * are optional and can be set to null.  The extension criticality
     * is set to true.
     *
     * @param permitted the permitted GeneralSubtrees (null for optional).
     * @param excluded the excluded GeneralSubtrees (null for optional).
     */
    public NameConstraintsExtension(final GeneralSubtrees permitted,
                                    final GeneralSubtrees excluded)
    throws IOException {
        this.permitted = permitted;
        this.excluded = excluded;

        this.extensionId = PKIXExtensions.NameConstraints_Id;
        this.critical = true;
        encodeThis();
    }

    /**
     * Create the extension from the passed DER encoded value.
     *
     * @param critical true if the extension is to be treated as critical.
     * @param value an array of DER encoded bytes of the actual value.
     * @exception ClassCastException if value is not an array of bytes
     * @exception IOException on error.
     */
    public NameConstraintsExtension(final Boolean critical, final Object value)
    throws IOException {
        this.extensionId = PKIXExtensions.NameConstraints_Id;
        this.critical = critical.booleanValue();

        this.extensionValue = (byte[]) value;
        final DerValue val = new DerValue(this.extensionValue);
        if (val.tag != DerValue.tag_Sequence) {
            throw new IOException("Invalid encoding for" +
                                  " NameConstraintsExtension.");
        }

        // NB. this is always encoded with the IMPLICIT tag
        // The checks only make sense if we assume implicit tagging,
        // with explicit tagging the form is always constructed.
        // Note that all the fields in NameConstraints are defined as
        // being OPTIONAL, i.e., there could be an empty SEQUENCE, resulting
        // in val.data being null.
        if (val.data == null) {
			return;
		}
        while (val.data.available() != 0) {
            final DerValue opt = val.data.getDerValue();

            if (opt.isContextSpecific(TAG_PERMITTED) && opt.isConstructed()) {
                if (this.permitted != null) {
                    throw new IOException("Duplicate permitted " +
                         "GeneralSubtrees in NameConstraintsExtension.");
                }
                opt.resetTag(DerValue.tag_Sequence);
                this.permitted = new GeneralSubtrees(opt);

            } else if (opt.isContextSpecific(TAG_EXCLUDED) &&
                       opt.isConstructed()) {
                if (this.excluded != null) {
                    throw new IOException("Duplicate excluded " +
                             "GeneralSubtrees in NameConstraintsExtension.");
                }
                opt.resetTag(DerValue.tag_Sequence);
                this.excluded = new GeneralSubtrees(opt);
            } else {
				throw new IOException("Invalid encoding of " +
                                      "NameConstraintsExtension.");
			}
        }
        this.minMaxValid = false;
    }

    /**
     * Return the printable string.
     */
    @Override
	public String toString() {
        return (super.toString() + "NameConstraints: [" +
                ((this.permitted == null) ? "" :
                     ("\n    Permitted:" + this.permitted.toString())) +
                ((this.excluded == null) ? "" :
                     ("\n    Excluded:" + this.excluded.toString()))
                + "   ]\n");
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
            this.extensionId = PKIXExtensions.NameConstraints_Id;
            this.critical = true;
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
        if (name.equalsIgnoreCase(PERMITTED_SUBTREES)) {
            if (!(obj instanceof GeneralSubtrees)) {
                throw new IOException("Attribute value should be"
                                    + " of type GeneralSubtrees.");
            }
            this.permitted = (GeneralSubtrees)obj;
        } else if (name.equalsIgnoreCase(EXCLUDED_SUBTREES)) {
            if (!(obj instanceof GeneralSubtrees)) {
                throw new IOException("Attribute value should be "
                                    + "of type GeneralSubtrees.");
            }
            this.excluded = (GeneralSubtrees)obj;
        } else {
          throw new IOException("Attribute name not recognized by " +
                        "CertAttrSet:NameConstraintsExtension.");
        }
        encodeThis();
    }

    /**
     * Get the attribute value.
     */
    @Override
	public GeneralSubtrees get(final String name) throws IOException {
        if (name.equalsIgnoreCase(PERMITTED_SUBTREES)) {
            return (this.permitted);
        } else if (name.equalsIgnoreCase(EXCLUDED_SUBTREES)) {
            return (this.excluded);
        } else {
          throw new IOException("Attribute name not recognized by " +
                        "CertAttrSet:NameConstraintsExtension.");
        }
    }

    /**
     * Delete the attribute value.
     */
    @Override
	public void delete(final String name) throws IOException {
        if (name.equalsIgnoreCase(PERMITTED_SUBTREES)) {
            this.permitted = null;
        } else if (name.equalsIgnoreCase(EXCLUDED_SUBTREES)) {
            this.excluded = null;
        } else {
          throw new IOException("Attribute name not recognized by " +
                        "CertAttrSet:NameConstraintsExtension.");
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
        elements.addElement(PERMITTED_SUBTREES);
        elements.addElement(EXCLUDED_SUBTREES);

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
     * Merge additional name constraints with existing ones.
     * This function is used in certification path processing
     * to accumulate name constraints from successive certificates
     * in the path.  Note that NameConstraints can never be
     * expanded by a merge, just remain constant or become more
     * limiting.
     * <p>
     * IETF RFC2459 specifies the processing of Name Constraints as
     * follows:
     * <p>
     * (j)  If permittedSubtrees is present in the certificate, set the
     * constrained subtrees state variable to the intersection of its
     * previous value and the value indicated in the extension field.
     * <p>
     * (k)  If excludedSubtrees is present in the certificate, set the
     * excluded subtrees state variable to the union of its previous
     * value and the value indicated in the extension field.
     * <p>
     * @param newConstraints additional NameConstraints to be applied
     * @throws IOException on error
     */
    public void merge(final NameConstraintsExtension newConstraints)
            throws IOException {

        if (newConstraints == null) {
            // absence of any explicit constraints implies unconstrained
            return;
        }

        /*
         * If excludedSubtrees is present in the certificate, set the
         * excluded subtrees state variable to the union of its previous
         * value and the value indicated in the extension field.
         */

        GeneralSubtrees newExcluded = newConstraints.get(EXCLUDED_SUBTREES);
        if (this.excluded == null) {
            this.excluded = (newExcluded != null) ?
                        (GeneralSubtrees)newExcluded.clone() : null;
        } else {
            if (newExcluded != null) {
                // Merge new excluded with current excluded (union)
                this.excluded.union(newExcluded);
            }
        }

        /*
         * If permittedSubtrees is present in the certificate, set the
         * constrained subtrees state variable to the intersection of its
         * previous value and the value indicated in the extension field.
         */

        final GeneralSubtrees newPermitted = newConstraints.get(PERMITTED_SUBTREES);
        if (this.permitted == null) {
            this.permitted = (newPermitted != null) ?
                        (GeneralSubtrees)newPermitted.clone() : null;
        } else {
            if (newPermitted != null) {
                // Merge new permitted with current permitted (intersection)
                newExcluded = this.permitted.intersect(newPermitted);

                // Merge new excluded subtrees to current excluded (union)
                if (newExcluded != null) {
                    if (this.excluded != null) {
                        this.excluded.union(newExcluded);
                    } else {
                        this.excluded = (GeneralSubtrees)newExcluded.clone();
                    }
                }
            }
        }

        // Optional optimization: remove permitted subtrees that are excluded.
        // This is not necessary for algorithm correctness, but it makes
        // subsequent operations on the NameConstraints faster and require
        // less space.
        if (this.permitted != null) {
            this.permitted.reduce(this.excluded);
        }

        // The NameConstraints have been changed, so re-encode them.  Methods in
        // this class assume that the encodings have already been done.
        encodeThis();

    }

    /**
     * check whether a certificate conforms to these NameConstraints.
     * This involves verifying that the subject name and subjectAltName
     * extension (critical or noncritical) is consistent with the permitted
     * subtrees state variables.  Also verify that the subject name and
     * subjectAltName extension (critical or noncritical) is consistent with
     * the excluded subtrees state variables.
     *
     * @param cert X509Certificate to be verified
     * @returns true if certificate verifies successfully
     * @throws IOException on error
     */
    public boolean verify(final X509Certificate cert) throws IOException {

        if (cert == null) {
            throw new IOException("Certificate is null");
        }

        // Calculate hasMin and hasMax booleans (if necessary)
        if (!this.minMaxValid) {
            calcMinMax();
        }

        if (this.hasMin) {
            throw new IOException("Non-zero minimum BaseDistance in"
                                + " name constraints not supported");
        }

        if (this.hasMax) {
            throw new IOException("Maximum BaseDistance in"
                                + " name constraints not supported");
        }

        final X500Principal subjectPrincipal = cert.getSubjectX500Principal();
        final X500Name subject = X500Name.asX500Name(subjectPrincipal);

        if (subject.isEmpty() == false) {
            if (verify(subject) == false) {
                return false;
            }
        }

        GeneralNames altNames = null;
        // extract altNames
        try {
            // extract extensions, if any, from certInfo
            // following returns null if certificate contains no extensions
            final X509CertImpl certImpl = X509CertImpl.toImpl(cert);
            final SubjectAlternativeNameExtension altNameExt =
                certImpl.getSubjectAlternativeNameExtension();
            if (altNameExt != null) {
                // extract altNames from extension; this call does not
                // return an IOException on null altnames
                altNames = altNameExt.get(
                        SubjectAlternativeNameExtension.SUBJECT_NAME);
            }
        } catch (final CertificateException ce) {
            throw new IOException("Unable to extract extensions from " +
                        "certificate: " + ce.getMessage());
        }

        // If there are no subjectAlternativeNames, perform the special-case
        // check where if the subjectName contains any EMAILADDRESS
        // attributes, they must be checked against RFC822 constraints.
        // If that passes, we're fine.
        if (altNames == null) {
            return verifyRFC822SpecialCase(subject);
        }

        // verify each subjectAltName
        for (int i = 0; i < altNames.size(); i++) {
            final GeneralNameInterface altGNI = altNames.get(i).getName();
            if (!verify(altGNI)) {
                return false;
            }
        }

        // All tests passed.
        return true;
    }

    /**
     * check whether a name conforms to these NameConstraints.
     * This involves verifying that the name is consistent with the
     * permitted and excluded subtrees variables.
     *
     * @param name GeneralNameInterface name to be verified
     * @returns true if certificate verifies successfully
     * @throws IOException on error
     */
    public boolean verify(final GeneralNameInterface name) throws IOException {
        if (name == null) {
            throw new IOException("name is null");
        }

        // Verify that the name is consistent with the excluded subtrees
        if (this.excluded != null && this.excluded.size() > 0) {

            for (int i = 0; i < this.excluded.size(); i++) {
                final GeneralSubtree gs = this.excluded.get(i);
                if (gs == null) {
					continue;
				}
                final GeneralName gn = gs.getName();
                if (gn == null) {
					continue;
				}
                final GeneralNameInterface exName = gn.getName();
                if (exName == null) {
					continue;
				}

                // if name matches or narrows any excluded subtree,
                // return false
                switch (exName.constrains(name)) {
                case GeneralNameInterface.NAME_DIFF_TYPE:
                case GeneralNameInterface.NAME_WIDENS: // name widens excluded
                case GeneralNameInterface.NAME_SAME_TYPE:
                    break;
                case GeneralNameInterface.NAME_MATCH:
                case GeneralNameInterface.NAME_NARROWS: // subject name excluded
                    return false;
                }
            }
        }

        // Verify that the name is consistent with the permitted subtrees
        if (this.permitted != null && this.permitted.size() > 0) {

            boolean sameType = false;

            for (int i = 0; i < this.permitted.size(); i++) {
                final GeneralSubtree gs = this.permitted.get(i);
                if (gs == null) {
					continue;
				}
                final GeneralName gn = gs.getName();
                if (gn == null) {
					continue;
				}
                final GeneralNameInterface perName = gn.getName();
                if (perName == null) {
					continue;
				}

                // if Name matches any type in permitted,
                // and Name does not match or narrow some permitted subtree,
                // return false
                switch (perName.constrains(name)) {
                case GeneralNameInterface.NAME_DIFF_TYPE:
                    continue; // continue checking other permitted names
                case GeneralNameInterface.NAME_WIDENS: // name widens permitted
                case GeneralNameInterface.NAME_SAME_TYPE:
                    sameType = true;
                    continue; // continue to look for a match or narrow
                case GeneralNameInterface.NAME_MATCH:
                case GeneralNameInterface.NAME_NARROWS:
                    // name narrows permitted
                    return true; // name is definitely OK, so break out of loop
                }
            }
            if (sameType) {
                return false;
            }
        }
        return true;
    }

    /**
     * Perform the RFC 822 special case check. We have a certificate
     * that does not contain any subject alternative names. Check that
     * any EMAILADDRESS attributes in its subject name conform to these
     * NameConstraints.
     *
     * @param subject the certificate's subject name
     * @returns true if certificate verifies successfully
     * @throws IOException on error
     */
    public boolean verifyRFC822SpecialCase(final X500Name subject) throws IOException {
        for (final AVA ava : subject.allAvas()) {
            final ObjectIdentifier attrOID = ava.getObjectIdentifier();
            if (attrOID.equals((Object)PKCS9Attribute.EMAIL_ADDRESS_OID)) {
                final String attrValue = ava.getValueString();
                if (attrValue != null) {
                    RFC822Name emailName;
                    try {
                        emailName = new RFC822Name(attrValue);
                    } catch (final IOException ioe) {
                        continue;
                    }
                    if (!verify(emailName)) {
                        return(false);
                    }
                }
             }
        }
        return true;
    }

    /**
     * Clone all objects that may be modified during certificate validation.
     */
    @Override
	public Object clone() {
        try {
            final NameConstraintsExtension newNCE =
                (NameConstraintsExtension) super.clone();

            if (this.permitted != null) {
                newNCE.permitted = (GeneralSubtrees) this.permitted.clone();
            }
            if (this.excluded != null) {
                newNCE.excluded = (GeneralSubtrees) this.excluded.clone();
            }
            return newNCE;
        } catch (final CloneNotSupportedException cnsee) {
            throw new RuntimeException("CloneNotSupportedException while " +
                "cloning NameConstraintsException. This should never happen.");
        }
    }
}
