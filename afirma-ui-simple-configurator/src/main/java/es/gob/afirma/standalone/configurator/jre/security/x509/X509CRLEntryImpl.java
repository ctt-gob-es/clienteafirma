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
import java.math.BigInteger;
import java.security.cert.CRLException;
import java.security.cert.CRLReason;
import java.security.cert.X509CRLEntry;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Enumeration;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.security.auth.x500.X500Principal;

import es.gob.afirma.standalone.configurator.jre.misc.HexDumpEncoder;
import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;

/**
 * <p>Abstract class for a revoked certificate in a CRL.
 * This class is for each entry in the <code>revokedCertificates</code>,
 * so it deals with the inner <em>SEQUENCE</em>.
 * The ASN.1 definition for this is:
 * <pre>
 * revokedCertificates    SEQUENCE OF SEQUENCE  {
 *     userCertificate    CertificateSerialNumber,
 *     revocationDate     ChoiceOfTime,
 *     crlEntryExtensions Extensions OPTIONAL
 *                        -- if present, must be v2
 * }  OPTIONAL
 *
 * CertificateSerialNumber  ::=  INTEGER
 *
 * Extensions  ::=  SEQUENCE SIZE (1..MAX) OF Extension
 *
 * Extension  ::=  SEQUENCE  {
 *     extnId        OBJECT IDENTIFIER,
 *     critical      BOOLEAN DEFAULT FALSE,
 *     extnValue     OCTET STRING
 *                   -- contains a DER encoding of a value
 *                   -- of the type registered for use with
 *                   -- the extnId object identifier value
 * }
 * </pre>
 *
 * @author Hemma Prafullchandra
 */

public class X509CRLEntryImpl extends X509CRLEntry
        implements Comparable<X509CRLEntryImpl> {

    private SerialNumber serialNumber = null;
    private Date revocationDate = null;
    private CRLExtensions extensions = null;
    private byte[] revokedCert = null;
    private X500Principal certIssuer;

    private final static boolean isExplicit = false;
    private static final long YR_2050 = 2524636800000L;

    /**
     * Constructs a revoked certificate entry using the given
     * serial number and revocation date.
     *
     * @param num the serial number of the revoked certificate.
     * @param date the Date on which revocation took place.
     */
    public X509CRLEntryImpl(final BigInteger num, final Date date) {
        this.serialNumber = new SerialNumber(num);
        this.revocationDate = date;
    }

    /**
     * Constructs a revoked certificate entry using the given
     * serial number, revocation date and the entry
     * extensions.
     *
     * @param num the serial number of the revoked certificate.
     * @param date the Date on which revocation took place.
     * @param crlEntryExts the extensions for this entry.
     */
    public X509CRLEntryImpl(final BigInteger num, final Date date,
                           final CRLExtensions crlEntryExts) {
        this.serialNumber = new SerialNumber(num);
        this.revocationDate = date;
        this.extensions = crlEntryExts;
    }

    /**
     * Unmarshals a revoked certificate from its encoded form.
     *
     * @param revokedCert the encoded bytes.
     * @exception CRLException on parsing errors.
     */
    public X509CRLEntryImpl(final byte[] revokedCert) throws CRLException {
        try {
            parse(new DerValue(revokedCert));
        } catch (final IOException e) {
            this.revokedCert = null;
            throw new CRLException("Parsing error: " + e.toString()); //$NON-NLS-1$
        }
    }

    /**
     * Unmarshals a revoked certificate from its encoded form.
     *
     * @param derVal the DER value containing the revoked certificate.
     * @exception CRLException on parsing errors.
     */
    public X509CRLEntryImpl(final DerValue derValue) throws CRLException {
        try {
            parse(derValue);
        } catch (final IOException e) {
            this.revokedCert = null;
            throw new CRLException("Parsing error: " + e.toString()); //$NON-NLS-1$
        }
    }

    /**
     * Returns true if this revoked certificate entry has
     * extensions, otherwise false.
     *
     * @return true if this CRL entry has extensions, otherwise
     * false.
     */
    @Override
	public boolean hasExtensions() {
        return (this.extensions != null);
    }

    /**
     * Encodes the revoked certificate to an output stream.
     *
     * @param outStrm an output stream to which the encoded revoked
     * certificate is written.
     * @exception CRLException on encoding errors.
     */
    public void encode(final DerOutputStream outStrm) throws CRLException {
        try {
            if (this.revokedCert == null) {
                final DerOutputStream tmp = new DerOutputStream();
                // sequence { serialNumber, revocationDate, extensions }
                this.serialNumber.encode(tmp);

                if (this.revocationDate.getTime() < YR_2050) {
                    tmp.putUTCTime(this.revocationDate);
                } else {
                    tmp.putGeneralizedTime(this.revocationDate);
                }

                if (this.extensions != null) {
					this.extensions.encode(tmp, isExplicit);
				}

                final DerOutputStream seq = new DerOutputStream();
                seq.write(DerValue.tag_Sequence, tmp);

                this.revokedCert = seq.toByteArray();
            }
            outStrm.write(this.revokedCert);
        } catch (final IOException e) {
             throw new CRLException("Encoding error: " + e.toString()); //$NON-NLS-1$
        }
    }

    /**
     * Returns the ASN.1 DER-encoded form of this CRL Entry,
     * which corresponds to the inner SEQUENCE.
     *
     * @exception CRLException if an encoding error occurs.
     */
    @Override
	public byte[] getEncoded() throws CRLException {
        return getEncoded0().clone();
    }

    // Called internally to avoid clone
    private byte[] getEncoded0() throws CRLException {
        if (this.revokedCert == null) {
			this.encode(new DerOutputStream());
		}
        return this.revokedCert;
    }

    @Override
    public X500Principal getCertificateIssuer() {
        return this.certIssuer;
    }

    void setCertificateIssuer(final X500Principal crlIssuer, final X500Principal certIssuer) {
        if (crlIssuer.equals(certIssuer)) {
            this.certIssuer = null;
        } else {
            this.certIssuer = certIssuer;
        }
    }

    /**
     * Gets the serial number from this X509CRLEntry,
     * i.e. the <em>userCertificate</em>.
     *
     * @return the serial number.
     */
    @Override
	public BigInteger getSerialNumber() {
        return this.serialNumber.getNumber();
    }

    /**
     * Gets the revocation date from this X509CRLEntry,
     * the <em>revocationDate</em>.
     *
     * @return the revocation date.
     */
    @Override
	public Date getRevocationDate() {
        return new Date(this.revocationDate.getTime());
    }

    /**
     * This method is the overridden implementation of the getRevocationReason
     * method in X509CRLEntry. It is better performance-wise since it returns
     * cached values.
     */
    @Override
    public CRLReason getRevocationReason() {
        final Extension ext = getExtension(PKIXExtensions.ReasonCode_Id);
        if (ext == null) {
            return null;
        }
        final CRLReasonCodeExtension rcExt = (CRLReasonCodeExtension) ext;
        return rcExt.getReasonCode();
    }

    /**
     * This static method is the default implementation of the
     * getRevocationReason method in X509CRLEntry.
     */
    public static CRLReason getRevocationReason(final X509CRLEntry crlEntry) {
        try {
            final byte[] ext = crlEntry.getExtensionValue("2.5.29.21"); //$NON-NLS-1$
            if (ext == null) {
                return null;
            }
            final DerValue val = new DerValue(ext);
            final byte[] data = val.getOctetString();

            final CRLReasonCodeExtension rcExt =
                new CRLReasonCodeExtension(Boolean.FALSE, data);
            return rcExt.getReasonCode();
        } catch (final IOException ioe) {
            return null;
        }
    }

    /**
     * get Reason Code from CRL entry.
     *
     * @returns Integer or null, if no such extension
     * @throws IOException on error
     */
    public Integer getReasonCode() throws IOException {
        final Object obj = getExtension(PKIXExtensions.ReasonCode_Id);
        if (obj == null) {
			return null;
		}
        final CRLReasonCodeExtension reasonCode = (CRLReasonCodeExtension)obj;
        return reasonCode.get(CRLReasonCodeExtension.REASON);
    }

    /**
     * Returns a printable string of this revoked certificate.
     *
     * @return value of this revoked certificate in a printable form.
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();

        sb.append(this.serialNumber.toString());
        sb.append("  On: " + this.revocationDate.toString()); //$NON-NLS-1$
        if (this.certIssuer != null) {
            sb.append("\n    Certificate issuer: " + this.certIssuer); //$NON-NLS-1$
        }
        if (this.extensions != null) {
            final Collection<Extension> allEntryExts = this.extensions.getAllExtensions();
            final Extension[] exts = allEntryExts.toArray(new Extension[0]);

            sb.append("\n    CRL Entry Extensions: " + exts.length); //$NON-NLS-1$
            for (int i = 0; i < exts.length; i++) {
                sb.append("\n    [" + (i+1) + "]: "); //$NON-NLS-1$ //$NON-NLS-2$
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
        }
        sb.append("\n"); //$NON-NLS-1$
        return sb.toString();
    }

    /**
     * Return true if a critical extension is found that is
     * not supported, otherwise return false.
     */
    @Override
	public boolean hasUnsupportedCriticalExtension() {
        if (this.extensions == null) {
			return false;
		}
        return this.extensions.hasUnsupportedCriticalExtension();
    }

    /**
     * Gets a Set of the extension(s) marked CRITICAL in this
     * X509CRLEntry.  In the returned set, each extension is
     * represented by its OID string.
     *
     * @return a set of the extension oid strings in the
     * Object that are marked critical.
     */
    @Override
	public Set<String> getCriticalExtensionOIDs() {
        if (this.extensions == null) {
            return null;
        }
        final Set<String> extSet = new TreeSet<>();
        for (final Extension ex : this.extensions.getAllExtensions()) {
            if (ex.isCritical()) {
                extSet.add(ex.getExtensionId().toString());
            }
        }
        return extSet;
    }

    /**
     * Gets a Set of the extension(s) marked NON-CRITICAL in this
     * X509CRLEntry. In the returned set, each extension is
     * represented by its OID string.
     *
     * @return a set of the extension oid strings in the
     * Object that are marked critical.
     */
    @Override
	public Set<String> getNonCriticalExtensionOIDs() {
        if (this.extensions == null) {
            return null;
        }
        final Set<String> extSet = new TreeSet<>();
        for (final Extension ex : this.extensions.getAllExtensions()) {
            if (!ex.isCritical()) {
                extSet.add(ex.getExtensionId().toString());
            }
        }
        return extSet;
    }

    /**
     * Gets the DER encoded OCTET string for the extension value
     * (<em>extnValue</em>) identified by the passed in oid String.
     * The <code>oid</code> string is
     * represented by a set of positive whole number separated
     * by ".", that means,<br>
     * &lt;positive whole number&gt;.&lt;positive whole number&gt;.&lt;positive
     * whole number&gt;.&lt;...&gt;
     *
     * @param oid the Object Identifier value for the extension.
     * @return the DER encoded octet string of the extension value.
     */
    @Override
	public byte[] getExtensionValue(final String oid) {
        if (this.extensions == null) {
			return null;
		}
        try {
            final String extAlias = OIDMap.getName(new ObjectIdentifier(oid));
            Extension crlExt = null;

            if (extAlias == null) { // may be unknown
                final ObjectIdentifier findOID = new ObjectIdentifier(oid);
                Extension ex = null;
                ObjectIdentifier inCertOID;
                for (final Enumeration<Extension> e = this.extensions.getElements();
                                                 e.hasMoreElements();) {
                    ex = e.nextElement();
                    inCertOID = ex.getExtensionId();
                    if (inCertOID.equals((Object)findOID)) {
                        crlExt = ex;
                        break;
                    }
                }
            } else {
				crlExt = this.extensions.get(extAlias);
			}
            if (crlExt == null) {
				return null;
			}
            final byte[] extData = crlExt.getExtensionValue();
            if (extData == null) {
				return null;
			}

            final DerOutputStream out = new DerOutputStream();
            out.putOctetString(extData);
            return out.toByteArray();
        } catch (final Exception e) {
            return null;
        }
    }

    /**
     * get an extension
     *
     * @param oid ObjectIdentifier of extension desired
     * @returns Extension of type <extension> or null, if not found
     */
    public Extension getExtension(final ObjectIdentifier oid) {
        if (this.extensions == null) {
			return null;
		}

        // following returns null if no such OID in map
        //XXX consider cloning this
        return this.extensions.get(OIDMap.getName(oid));
    }

    private void parse(final DerValue derVal)
    throws CRLException, IOException {

        if (derVal.tag != DerValue.tag_Sequence) {
            throw new CRLException("Invalid encoded RevokedCertificate, " + //$NON-NLS-1$
                                  "starting sequence tag missing."); //$NON-NLS-1$
        }
        if (derVal.data.available() == 0) {
			throw new CRLException("No data encoded for RevokedCertificates"); //$NON-NLS-1$
		}

        this.revokedCert = derVal.toByteArray();
        // serial number
        final DerInputStream in = derVal.toDerInputStream();
        final DerValue val = in.getDerValue();
        this.serialNumber = new SerialNumber(val);

        // revocationDate
        final int nextByte = derVal.data.peekByte();
        if ((byte)nextByte == DerValue.tag_UtcTime) {
            this.revocationDate = derVal.data.getUTCTime();
        } else if ((byte)nextByte == DerValue.tag_GeneralizedTime) {
            this.revocationDate = derVal.data.getGeneralizedTime();
        } else {
			throw new CRLException("Invalid encoding for revocation date"); //$NON-NLS-1$
		}

        if (derVal.data.available() == 0)
		 {
			return;  // no extensions
		}

        // crlEntryExtensions
        this.extensions = new CRLExtensions(derVal.toDerInputStream());
    }

    /**
     * Utility method to convert an arbitrary instance of X509CRLEntry
     * to a X509CRLEntryImpl. Does a cast if possible, otherwise reparses
     * the encoding.
     */
    public static X509CRLEntryImpl toImpl(final X509CRLEntry entry)
            throws CRLException {
        if (entry instanceof X509CRLEntryImpl) {
            return (X509CRLEntryImpl)entry;
        } else {
            return new X509CRLEntryImpl(entry.getEncoded());
        }
    }

    /**
     * Returns the CertificateIssuerExtension
     *
     * @return the CertificateIssuerExtension, or null if it does not exist
     */
    CertificateIssuerExtension getCertificateIssuerExtension() {
        return (CertificateIssuerExtension)
            getExtension(PKIXExtensions.CertificateIssuer_Id);
    }

    /**
     * Returns all extensions for this entry in a map
     * @return the extension map, can be empty, but not null
     */
    public Map<String, java.security.cert.Extension> getExtensions() {
        if (this.extensions == null) {
            return Collections.emptyMap();
        }
        final Collection<Extension> exts = this.extensions.getAllExtensions();
        final Map<String, java.security.cert.Extension> map = new TreeMap<>();
        for (final Extension ext : exts) {
            map.put(ext.getId(), ext);
        }
        return map;
    }

    @Override
    public int compareTo(final X509CRLEntryImpl that) {
        final int compSerial = getSerialNumber().compareTo(that.getSerialNumber());
        if (compSerial != 0) {
            return compSerial;
        }
        try {
            final byte[] thisEncoded = this.getEncoded0();
            final byte[] thatEncoded = that.getEncoded0();
            for (int i=0; i<thisEncoded.length && i<thatEncoded.length; i++) {
                final int a = thisEncoded[i] & 0xff;
                final int b = thatEncoded[i] & 0xff;
                if (a != b) {
					return a-b;
				}
            }
            return thisEncoded.length -thatEncoded.length;
        } catch (final CRLException ce) {
            return -1;
        }
    }
}
