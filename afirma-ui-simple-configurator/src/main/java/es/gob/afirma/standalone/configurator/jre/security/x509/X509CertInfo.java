/*
 * Copyright (c) 1997, 2012, Oracle and/or its affiliates. All rights reserved.
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
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateParsingException;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import es.gob.afirma.standalone.configurator.jre.misc.HexDumpEncoder;
import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;

/**
 * The X509CertInfo class represents X.509 certificate information.
 *
 * <P>X.509 certificates have several base data elements, including:<UL>
 *
 * <LI>The <em>Subject Name</em>, an X.500 Distinguished Name for
 *      the entity (subject) for which the certificate was issued.
 *
 * <LI>The <em>Subject Public Key</em>, the public key of the subject.
 *      This is one of the most important parts of the certificate.
 *
 * <LI>The <em>Validity Period</em>, a time period (e.g. six months)
 *      within which the certificate is valid (unless revoked).
 *
 * <LI>The <em>Issuer Name</em>, an X.500 Distinguished Name for the
 *      Certificate Authority (CA) which issued the certificate.
 *
 * <LI>A <em>Serial Number</em> assigned by the CA, for use in
 *      certificate revocation and other applications.
 *
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 * @see CertAttrSet
 * @see X509CertImpl
 */
public class X509CertInfo implements CertAttrSet<String> {
    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT = "x509.info"; //$NON-NLS-1$
    // Certificate attribute names
    public static final String NAME = "info"; //$NON-NLS-1$
    public static final String DN_NAME = "dname"; //$NON-NLS-1$
    public static final String VERSION = CertificateVersion.NAME;
    public static final String SERIAL_NUMBER = CertificateSerialNumber.NAME;
    public static final String ALGORITHM_ID = CertificateAlgorithmId.NAME;
    public static final String ISSUER = "issuer"; //$NON-NLS-1$
    public static final String SUBJECT = "subject"; //$NON-NLS-1$
    public static final String VALIDITY = CertificateValidity.NAME;
    public static final String KEY = CertificateX509Key.NAME;
    public static final String ISSUER_ID = "issuerID"; //$NON-NLS-1$
    public static final String SUBJECT_ID = "subjectID"; //$NON-NLS-1$
    public static final String EXTENSIONS = CertificateExtensions.NAME;

    // X509.v1 data
    protected CertificateVersion version = new CertificateVersion();
    protected CertificateSerialNumber   serialNum = null;
    protected CertificateAlgorithmId    algId = null;
    protected X500Name                  issuer = null;
    protected X500Name                  subject = null;
    protected CertificateValidity       interval = null;
    protected CertificateX509Key        pubKey = null;

    // X509.v2 & v3 extensions
    protected UniqueIdentity   issuerUniqueId = null;
    protected UniqueIdentity  subjectUniqueId = null;

    // X509.v3 extensions
    protected CertificateExtensions     extensions = null;

    // Attribute numbers for internal manipulation
    private static final int ATTR_VERSION = 1;
    private static final int ATTR_SERIAL = 2;
    private static final int ATTR_ALGORITHM = 3;
    private static final int ATTR_ISSUER = 4;
    private static final int ATTR_VALIDITY = 5;
    private static final int ATTR_SUBJECT = 6;
    private static final int ATTR_KEY = 7;
    private static final int ATTR_ISSUER_ID = 8;
    private static final int ATTR_SUBJECT_ID = 9;
    private static final int ATTR_EXTENSIONS = 10;

    // DER encoded CertificateInfo data
    private byte[]      rawCertInfo = null;

    // The certificate attribute name to integer mapping stored here
    private static final Map<String,Integer> map = new HashMap<String,Integer>();
    static {
        map.put(VERSION, Integer.valueOf(ATTR_VERSION));
        map.put(SERIAL_NUMBER, Integer.valueOf(ATTR_SERIAL));
        map.put(ALGORITHM_ID, Integer.valueOf(ATTR_ALGORITHM));
        map.put(ISSUER, Integer.valueOf(ATTR_ISSUER));
        map.put(VALIDITY, Integer.valueOf(ATTR_VALIDITY));
        map.put(SUBJECT, Integer.valueOf(ATTR_SUBJECT));
        map.put(KEY, Integer.valueOf(ATTR_KEY));
        map.put(ISSUER_ID, Integer.valueOf(ATTR_ISSUER_ID));
        map.put(SUBJECT_ID, Integer.valueOf(ATTR_SUBJECT_ID));
        map.put(EXTENSIONS, Integer.valueOf(ATTR_EXTENSIONS));
    }

    /**
     * Construct an uninitialized X509CertInfo on which <a href="#decode">
     * decode</a> must later be called (or which may be deserialized).
     */
    public X509CertInfo() { }

    /**
     * Unmarshals a certificate from its encoded form, parsing the
     * encoded bytes.  This form of constructor is used by agents which
     * need to examine and use certificate contents.  That is, this is
     * one of the more commonly used constructors.  Note that the buffer
     * must include only a certificate, and no "garbage" may be left at
     * the end.  If you need to ignore data at the end of a certificate,
     * use another constructor.
     *
     * @param cert the encoded bytes, with no trailing data.
     * @exception CertificateParsingException on parsing errors.
     */
    public X509CertInfo(final byte[] cert) throws CertificateParsingException {
        try {
            final DerValue    in = new DerValue(cert);

            parse(in);
        } catch (final IOException e) {
            throw new CertificateParsingException(e);
        }
    }

    /**
     * Unmarshal a certificate from its encoded form, parsing a DER value.
     * This form of constructor is used by agents which need to examine
     * and use certificate contents.
     *
     * @param derVal the der value containing the encoded cert.
     * @exception CertificateParsingException on parsing errors.
     */
    public X509CertInfo(final DerValue derVal) throws CertificateParsingException {
        try {
            parse(derVal);
        } catch (final IOException e) {
            throw new CertificateParsingException(e);
        }
    }

    /**
     * Appends the certificate to an output stream.
     *
     * @param out an output stream to which the certificate is appended.
     * @exception CertificateException on encoding errors.
     * @exception IOException on other errors.
     */
    @Override
	public void encode(final OutputStream out)
    throws CertificateException, IOException {
        if (this.rawCertInfo == null) {
            final DerOutputStream tmp = new DerOutputStream();
            emit(tmp);
            this.rawCertInfo = tmp.toByteArray();
        }
        out.write(this.rawCertInfo.clone());
    }

    /**
     * Return an enumeration of names of attributes existing within this
     * attribute.
     */
    @Override
	public Enumeration<String> getElements() {
        final AttributeNameEnumeration elements = new AttributeNameEnumeration();
        elements.addElement(VERSION);
        elements.addElement(SERIAL_NUMBER);
        elements.addElement(ALGORITHM_ID);
        elements.addElement(ISSUER);
        elements.addElement(VALIDITY);
        elements.addElement(SUBJECT);
        elements.addElement(KEY);
        elements.addElement(ISSUER_ID);
        elements.addElement(SUBJECT_ID);
        elements.addElement(EXTENSIONS);

        return elements.elements();
    }

    /**
     * Return the name of this attribute.
     */
    @Override
	public String getName() {
        return(NAME);
    }

    /**
     * Returns the encoded certificate info.
     *
     * @exception CertificateEncodingException on encoding information errors.
     */
    public byte[] getEncodedInfo() throws CertificateEncodingException {
        try {
            if (this.rawCertInfo == null) {
                final DerOutputStream tmp = new DerOutputStream();
                emit(tmp);
                this.rawCertInfo = tmp.toByteArray();
            }
            return this.rawCertInfo.clone();
        } catch (final IOException e) {
            throw new CertificateEncodingException(e.toString());
        } catch (final CertificateException e) {
            throw new CertificateEncodingException(e.toString());
        }
    }

    /**
     * Compares two X509CertInfo objects.  This is false if the
     * certificates are not both X.509 certs, otherwise it
     * compares them as binary data.
     *
     * @param other the object being compared with this one
     * @return true iff the certificates are equivalent
     */
    @Override
	public boolean equals(final Object other) {
        if (other instanceof X509CertInfo) {
            return equals((X509CertInfo) other);
        } else {
            return false;
        }
    }

    /**
     * Compares two certificates, returning false if any data
     * differs between the two.
     *
     * @param other the object being compared with this one
     * @return true iff the certificates are equivalent
     */
    public boolean equals(final X509CertInfo other) {
        if (this == other) {
            return(true);
        } else if (this.rawCertInfo == null || other.rawCertInfo == null) {
            return(false);
        } else if (this.rawCertInfo.length != other.rawCertInfo.length) {
            return(false);
        }
        for (int i = 0; i < this.rawCertInfo.length; i++) {
            if (this.rawCertInfo[i] != other.rawCertInfo[i]) {
                return(false);
            }
        }
        return(true);
    }

    /**
     * Calculates a hash code value for the object.  Objects
     * which are equal will also have the same hashcode.
     */
    @Override
	public int hashCode() {
        int     retval = 0;

        for (int i = 1; i < this.rawCertInfo.length; i++) {
            retval += this.rawCertInfo[i] * i;
        }
        return(retval);
    }

    /**
     * Returns a printable representation of the certificate.
     */
    @Override
	public String toString() {

        if (this.subject == null || this.pubKey == null || this.interval == null
            || this.issuer == null || this.algId == null || this.serialNum == null) {
                throw new NullPointerException("X.509 cert is incomplete"); //$NON-NLS-1$
        }
        final StringBuilder sb = new StringBuilder();

        sb.append("[\n"); //$NON-NLS-1$
        sb.append("  " + this.version.toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
        sb.append("  Subject: " + this.subject.toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
        sb.append("  Signature Algorithm: " + this.algId.toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
        sb.append("  Key:  " + this.pubKey.toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
        sb.append("  " + this.interval.toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
        sb.append("  Issuer: " + this.issuer.toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
        sb.append("  " + this.serialNum.toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$

        // optional v2, v3 extras
        if (this.issuerUniqueId != null) {
            sb.append("  Issuer Id:\n" + this.issuerUniqueId.toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        if (this.subjectUniqueId != null) {
            sb.append("  Subject Id:\n" + this.subjectUniqueId.toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        if (this.extensions != null) {
            final Collection<Extension> allExts = this.extensions.getAllExtensions();
            final Extension[] exts = allExts.toArray(new Extension[0]);
            sb.append("\nCertificate Extensions: " + exts.length); //$NON-NLS-1$
            for (int i = 0; i < exts.length; i++) {
                sb.append("\n[" + (i+1) + "]: "); //$NON-NLS-1$ //$NON-NLS-2$
                final Extension ext = exts[i];
                try {
                    if (OIDMap.getClass(ext.getExtensionId()) == null) {
                        sb.append(ext.toString());
                        byte[] extValue = ext.getExtensionValue();
                        if (extValue != null) {
                            final DerOutputStream out = new DerOutputStream();
                            out.putOctetString(extValue);
                            extValue = out.toByteArray();
                            final HexDumpEncoder enc = new HexDumpEncoder();
                            sb.append("Extension unknown: " //$NON-NLS-1$
                                      + "DER encoded OCTET string =\n" //$NON-NLS-1$
                                      + enc.encodeBuffer(extValue) + "\n"); //$NON-NLS-1$
                        }
                    }
					else {
						sb.append(ext.toString()); //sub-class exists
					}
                } catch (final Exception e) {
                    sb.append(", Error parsing this extension"); //$NON-NLS-1$
                }
            }
            final Map<String,Extension> invalid = this.extensions.getUnparseableExtensions();
            if (invalid.isEmpty() == false) {
                sb.append("\nUnparseable certificate extensions: " + invalid.size()); //$NON-NLS-1$
                int i = 1;
                for (final Extension ext : invalid.values()) {
                    sb.append("\n[" + (i++) + "]: "); //$NON-NLS-1$ //$NON-NLS-2$
                    sb.append(ext);
                }
            }
        }
        sb.append("\n]"); //$NON-NLS-1$
        return sb.toString();
    }

    /**
     * Set the certificate attribute.
     *
     * @params name the name of the Certificate attribute.
     * @params val the value of the Certificate attribute.
     * @exception CertificateException on invalid attributes.
     * @exception IOException on other errors.
     */
    @Override
	public void set(final String name, final Object val)
    throws CertificateException, IOException {
        final X509AttributeName attrName = new X509AttributeName(name);

        final int attr = attributeMap(attrName.getPrefix());
        if (attr == 0) {
            throw new CertificateException("Attribute name not recognized: " //$NON-NLS-1$
                                           + name);
        }
        // set rawCertInfo to null, so that we are forced to re-encode
        this.rawCertInfo = null;
        final String suffix = attrName.getSuffix();

        switch (attr) {
        case ATTR_VERSION:
            if (suffix == null) {
                setVersion(val);
            } else {
                this.version.set(suffix, val);
            }
            break;

        case ATTR_SERIAL:
            if (suffix == null) {
                setSerialNumber(val);
            } else {
                this.serialNum.set(suffix, val);
            }
            break;

        case ATTR_ALGORITHM:
            if (suffix == null) {
                setAlgorithmId(val);
            } else {
                this.algId.set(suffix, val);
            }
            break;

        case ATTR_ISSUER:
            setIssuer(val);
            break;

        case ATTR_VALIDITY:
            if (suffix == null) {
                setValidity(val);
            } else {
                this.interval.set(suffix, val);
            }
            break;

        case ATTR_SUBJECT:
            setSubject(val);
            break;

        case ATTR_KEY:
            if (suffix == null) {
                setKey(val);
            } else {
                this.pubKey.set(suffix, val);
            }
            break;

        case ATTR_ISSUER_ID:
            setIssuerUniqueId(val);
            break;

        case ATTR_SUBJECT_ID:
            setSubjectUniqueId(val);
            break;

        case ATTR_EXTENSIONS:
            if (suffix == null) {
                setExtensions(val);
            } else {
                if (this.extensions == null) {
					this.extensions = new CertificateExtensions();
				}
                this.extensions.set(suffix, val);
            }
            break;
        }
    }

    /**
     * Delete the certificate attribute.
     *
     * @params name the name of the Certificate attribute.
     * @exception CertificateException on invalid attributes.
     * @exception IOException on other errors.
     */
    @Override
	public void delete(final String name)
    throws CertificateException, IOException {
        final X509AttributeName attrName = new X509AttributeName(name);

        final int attr = attributeMap(attrName.getPrefix());
        if (attr == 0) {
            throw new CertificateException("Attribute name not recognized: " //$NON-NLS-1$
                                           + name);
        }
        // set rawCertInfo to null, so that we are forced to re-encode
        this.rawCertInfo = null;
        final String suffix = attrName.getSuffix();

        switch (attr) {
        case ATTR_VERSION:
            if (suffix == null) {
                this.version = null;
            } else {
                this.version.delete(suffix);
            }
            break;
        case (ATTR_SERIAL):
            if (suffix == null) {
                this.serialNum = null;
            } else {
                this.serialNum.delete(suffix);
            }
            break;
        case (ATTR_ALGORITHM):
            if (suffix == null) {
                this.algId = null;
            } else {
                this.algId.delete(suffix);
            }
            break;
        case (ATTR_ISSUER):
            this.issuer = null;
            break;
        case (ATTR_VALIDITY):
            if (suffix == null) {
                this.interval = null;
            } else {
                this.interval.delete(suffix);
            }
            break;
        case (ATTR_SUBJECT):
            this.subject = null;
            break;
        case (ATTR_KEY):
            if (suffix == null) {
                this.pubKey = null;
            } else {
                this.pubKey.delete(suffix);
            }
            break;
        case (ATTR_ISSUER_ID):
            this.issuerUniqueId = null;
            break;
        case (ATTR_SUBJECT_ID):
            this.subjectUniqueId = null;
            break;
        case (ATTR_EXTENSIONS):
            if (suffix == null) {
                this.extensions = null;
            } else {
                if (this.extensions != null) {
					this.extensions.delete(suffix);
				}
            }
            break;
        }
    }

    /**
     * Get the certificate attribute.
     *
     * @params name the name of the Certificate attribute.
     *
     * @exception CertificateException on invalid attributes.
     * @exception IOException on other errors.
     */
    @Override
	public Object get(final String name)
    throws CertificateException, IOException {
        final X509AttributeName attrName = new X509AttributeName(name);

        final int attr = attributeMap(attrName.getPrefix());
        if (attr == 0) {
            throw new CertificateParsingException(
                          "Attribute name not recognized: " + name); //$NON-NLS-1$
        }
        final String suffix = attrName.getSuffix();

        switch (attr) { // frequently used attributes first
        case (ATTR_EXTENSIONS):
            if (suffix == null) {
                return(this.extensions);
            } else {
                if (this.extensions == null) {
                    return null;
                } else {
                    return(this.extensions.get(suffix));
                }
            }
        case (ATTR_SUBJECT):
            if (suffix == null) {
                return(this.subject);
            } else {
                return(getX500Name(suffix, false));
            }
        case (ATTR_ISSUER):
            if (suffix == null) {
                return(this.issuer);
            } else {
                return(getX500Name(suffix, true));
            }
        case (ATTR_KEY):
            if (suffix == null) {
                return(this.pubKey);
            } else {
                return(this.pubKey.get(suffix));
            }
        case (ATTR_ALGORITHM):
            if (suffix == null) {
                return(this.algId);
            } else {
                return(this.algId.get(suffix));
            }
        case (ATTR_VALIDITY):
            if (suffix == null) {
                return(this.interval);
            } else {
                return(this.interval.get(suffix));
            }
        case (ATTR_VERSION):
            if (suffix == null) {
                return(this.version);
            } else {
                return(this.version.get(suffix));
            }
        case (ATTR_SERIAL):
            if (suffix == null) {
                return(this.serialNum);
            } else {
                return(this.serialNum.get(suffix));
            }
        case (ATTR_ISSUER_ID):
            return(this.issuerUniqueId);
        case (ATTR_SUBJECT_ID):
            return(this.subjectUniqueId);
        }
        return null;
    }

    /*
     * Get the Issuer or Subject name
     */
    private Object getX500Name(final String name, final boolean getIssuer)
        throws IOException {
        if (name.equalsIgnoreCase(X509CertInfo.DN_NAME)) {
            return getIssuer ? this.issuer : this.subject;
        } else if (name.equalsIgnoreCase("x500principal")) { //$NON-NLS-1$
            return getIssuer ? this.issuer.asX500Principal()
                             : this.subject.asX500Principal();
        } else {
            throw new IOException("Attribute name not recognized."); //$NON-NLS-1$
        }
    }

    /*
     * This routine unmarshals the certificate information.
     */
    private void parse(final DerValue val)
    throws CertificateParsingException, IOException {
        DerInputStream  in;
        DerValue        tmp;

        if (val.tag != DerValue.tag_Sequence) {
            throw new CertificateParsingException("signed fields invalid"); //$NON-NLS-1$
        }
        this.rawCertInfo = val.toByteArray();

        in = val.data;

        // Version
        tmp = in.getDerValue();
        if (tmp.isContextSpecific((byte)0)) {
            this.version = new CertificateVersion(tmp);
            tmp = in.getDerValue();
        }

        // Serial number ... an integer
        this.serialNum = new CertificateSerialNumber(tmp);

        // Algorithm Identifier
        this.algId = new CertificateAlgorithmId(in);

        // Issuer name
        this.issuer = new X500Name(in);
        if (this.issuer.isEmpty()) {
            throw new CertificateParsingException(
                "Empty issuer DN not allowed in X509Certificates"); //$NON-NLS-1$
        }

        // validity:  SEQUENCE { start date, end date }
        this.interval = new CertificateValidity(in);

        // subject name
        this.subject = new X500Name(in);
        if ((this.version.compare(CertificateVersion.V1) == 0) &&
                this.subject.isEmpty()) {
            throw new CertificateParsingException(
                      "Empty subject DN not allowed in v1 certificate"); //$NON-NLS-1$
        }

        // public key
        this.pubKey = new CertificateX509Key(in);

        // If more data available, make sure version is not v1.
        if (in.available() != 0) {
            if (this.version.compare(CertificateVersion.V1) == 0) {
                throw new CertificateParsingException(
                          "no more data allowed for version 1 certificate"); //$NON-NLS-1$
            }
        } else {
            return;
        }

        // Get the issuerUniqueId if present
        tmp = in.getDerValue();
        if (tmp.isContextSpecific((byte)1)) {
            this.issuerUniqueId = new UniqueIdentity(tmp);
            if (in.available() == 0) {
				return;
			}
            tmp = in.getDerValue();
        }

        // Get the subjectUniqueId if present.
        if (tmp.isContextSpecific((byte)2)) {
            this.subjectUniqueId = new UniqueIdentity(tmp);
            if (in.available() == 0) {
				return;
			}
            tmp = in.getDerValue();
        }

        // Get the extensions.
        if (this.version.compare(CertificateVersion.V3) != 0) {
            throw new CertificateParsingException(
                      "Extensions not allowed in v2 certificate"); //$NON-NLS-1$
        }
        if (tmp.isConstructed() && tmp.isContextSpecific((byte)3)) {
            this.extensions = new CertificateExtensions(tmp.data);
        }

        // verify X.509 V3 Certificate
        verifyCert(this.subject, this.extensions);

    }

    /*
     * Verify if X.509 V3 Certificate is compliant with RFC 3280.
     */
    private void verifyCert(final X500Name subject,
        final CertificateExtensions extensions)
        throws CertificateParsingException, IOException {

        // if SubjectName is empty, check for SubjectAlternativeNameExtension
        if (subject.isEmpty()) {
            if (extensions == null) {
                throw new CertificateParsingException("X.509 Certificate is " + //$NON-NLS-1$
                        "incomplete: subject field is empty, and certificate " + //$NON-NLS-1$
                        "has no extensions"); //$NON-NLS-1$
            }
            SubjectAlternativeNameExtension subjectAltNameExt = null;
            final SubjectAlternativeNameExtension extValue = null;
            GeneralNames names = null;
            try {
                subjectAltNameExt = (SubjectAlternativeNameExtension)
                        extensions.get(SubjectAlternativeNameExtension.NAME);
                names = subjectAltNameExt.get(
                        SubjectAlternativeNameExtension.SUBJECT_NAME);
            } catch (final IOException e) {
                throw new CertificateParsingException("X.509 Certificate is " + //$NON-NLS-1$
                        "incomplete: subject field is empty, and " + //$NON-NLS-1$
                        "SubjectAlternativeName extension is absent"); //$NON-NLS-1$
            }

            // SubjectAlternativeName extension is empty or not marked critical
            if (names == null || names.isEmpty()) {
                throw new CertificateParsingException("X.509 Certificate is " + //$NON-NLS-1$
                        "incomplete: subject field is empty, and " + //$NON-NLS-1$
                        "SubjectAlternativeName extension is empty"); //$NON-NLS-1$
            } else if (subjectAltNameExt.isCritical() == false) {
                throw new CertificateParsingException("X.509 Certificate is " + //$NON-NLS-1$
                        "incomplete: SubjectAlternativeName extension MUST " + //$NON-NLS-1$
                        "be marked critical when subject field is empty"); //$NON-NLS-1$
            }
        }
    }

    /*
     * Marshal the contents of a "raw" certificate into a DER sequence.
     */
    private void emit(final DerOutputStream out)
    throws CertificateException, IOException {
        final DerOutputStream tmp = new DerOutputStream();

        // version number, iff not V1
        this.version.encode(tmp);

        // Encode serial number, issuer signing algorithm, issuer name
        // and validity
        this.serialNum.encode(tmp);
        this.algId.encode(tmp);

        if ((this.version.compare(CertificateVersion.V1) == 0) &&
            (this.issuer.toString() == null)) {
			throw new CertificateParsingException(
                      "Null issuer DN not allowed in v1 certificate"); //$NON-NLS-1$
		}

        this.issuer.encode(tmp);
        this.interval.encode(tmp);

        // Encode subject (principal) and associated key
        if ((this.version.compare(CertificateVersion.V1) == 0) &&
            (this.subject.toString() == null)) {
			throw new CertificateParsingException(
                      "Null subject DN not allowed in v1 certificate"); //$NON-NLS-1$
		}
        this.subject.encode(tmp);
        this.pubKey.encode(tmp);

        // Encode issuerUniqueId & subjectUniqueId.
        if (this.issuerUniqueId != null) {
            this.issuerUniqueId.encode(tmp, DerValue.createTag(DerValue.TAG_CONTEXT,
                                                          false,(byte)1));
        }
        if (this.subjectUniqueId != null) {
            this.subjectUniqueId.encode(tmp, DerValue.createTag(DerValue.TAG_CONTEXT,
                                                           false,(byte)2));
        }

        // Write all the extensions.
        if (this.extensions != null) {
            this.extensions.encode(tmp);
        }

        // Wrap the data; encoding of the "raw" cert is now complete.
        out.write(DerValue.tag_Sequence, tmp);
    }

    /**
     * Returns the integer attribute number for the passed attribute name.
     */
    private int attributeMap(final String name) {
        final Integer num = map.get(name);
        if (num == null) {
            return 0;
        }
        return num.intValue();
    }

    /**
     * Set the version number of the certificate.
     *
     * @params val the Object class value for the Extensions
     * @exception CertificateException on invalid data.
     */
    private void setVersion(final Object val) throws CertificateException {
        if (!(val instanceof CertificateVersion)) {
            throw new CertificateException("Version class type invalid."); //$NON-NLS-1$
        }
        this.version = (CertificateVersion)val;
    }

    /**
     * Set the serial number of the certificate.
     *
     * @params val the Object class value for the CertificateSerialNumber
     * @exception CertificateException on invalid data.
     */
    private void setSerialNumber(final Object val) throws CertificateException {
        if (!(val instanceof CertificateSerialNumber)) {
            throw new CertificateException("SerialNumber class type invalid."); //$NON-NLS-1$
        }
        this.serialNum = (CertificateSerialNumber)val;
    }

    /**
     * Set the algorithm id of the certificate.
     *
     * @params val the Object class value for the AlgorithmId
     * @exception CertificateException on invalid data.
     */
    private void setAlgorithmId(final Object val) throws CertificateException {
        if (!(val instanceof CertificateAlgorithmId)) {
            throw new CertificateException(
                             "AlgorithmId class type invalid."); //$NON-NLS-1$
        }
        this.algId = (CertificateAlgorithmId)val;
    }

    /**
     * Set the issuer name of the certificate.
     *
     * @params val the Object class value for the issuer
     * @exception CertificateException on invalid data.
     */
    private void setIssuer(final Object val) throws CertificateException {
        if (!(val instanceof X500Name)) {
            throw new CertificateException(
                             "Issuer class type invalid."); //$NON-NLS-1$
        }
        this.issuer = (X500Name)val;
    }

    /**
     * Set the validity interval of the certificate.
     *
     * @params val the Object class value for the CertificateValidity
     * @exception CertificateException on invalid data.
     */
    private void setValidity(final Object val) throws CertificateException {
        if (!(val instanceof CertificateValidity)) {
            throw new CertificateException(
                             "CertificateValidity class type invalid."); //$NON-NLS-1$
        }
        this.interval = (CertificateValidity)val;
    }

    /**
     * Set the subject name of the certificate.
     *
     * @params val the Object class value for the Subject
     * @exception CertificateException on invalid data.
     */
    private void setSubject(final Object val) throws CertificateException {
        if (!(val instanceof X500Name)) {
            throw new CertificateException(
                             "Subject class type invalid."); //$NON-NLS-1$
        }
        this.subject = (X500Name)val;
    }

    /**
     * Set the public key in the certificate.
     *
     * @params val the Object class value for the PublicKey
     * @exception CertificateException on invalid data.
     */
    private void setKey(final Object val) throws CertificateException {
        if (!(val instanceof CertificateX509Key)) {
            throw new CertificateException(
                             "Key class type invalid."); //$NON-NLS-1$
        }
        this.pubKey = (CertificateX509Key)val;
    }

    /**
     * Set the Issuer Unique Identity in the certificate.
     *
     * @params val the Object class value for the IssuerUniqueId
     * @exception CertificateException
     */
    private void setIssuerUniqueId(final Object val) throws CertificateException {
        if (this.version.compare(CertificateVersion.V2) < 0) {
            throw new CertificateException("Invalid version"); //$NON-NLS-1$
        }
        if (!(val instanceof UniqueIdentity)) {
            throw new CertificateException(
                             "IssuerUniqueId class type invalid."); //$NON-NLS-1$
        }
        this.issuerUniqueId = (UniqueIdentity)val;
    }

    /**
     * Set the Subject Unique Identity in the certificate.
     *
     * @params val the Object class value for the SubjectUniqueId
     * @exception CertificateException
     */
    private void setSubjectUniqueId(final Object val) throws CertificateException {
        if (this.version.compare(CertificateVersion.V2) < 0) {
            throw new CertificateException("Invalid version"); //$NON-NLS-1$
        }
        if (!(val instanceof UniqueIdentity)) {
            throw new CertificateException(
                             "SubjectUniqueId class type invalid."); //$NON-NLS-1$
        }
        this.subjectUniqueId = (UniqueIdentity)val;
    }

    /**
     * Set the extensions in the certificate.
     *
     * @params val the Object class value for the Extensions
     * @exception CertificateException
     */
    private void setExtensions(final Object val) throws CertificateException {
        if (this.version.compare(CertificateVersion.V3) < 0) {
            throw new CertificateException("Invalid version"); //$NON-NLS-1$
        }
        if (!(val instanceof CertificateExtensions)) {
          throw new CertificateException(
                             "Extensions class type invalid."); //$NON-NLS-1$
        }
        this.extensions = (CertificateExtensions)val;
    }
}
