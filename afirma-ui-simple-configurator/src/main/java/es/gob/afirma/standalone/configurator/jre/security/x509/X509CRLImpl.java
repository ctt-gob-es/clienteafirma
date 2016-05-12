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
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Principal;
import java.security.PrivateKey;
import java.security.Provider;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.CRLException;
import java.security.cert.Certificate;
import java.security.cert.X509CRL;
import java.security.cert.X509CRLEntry;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.Date;
import java.util.Enumeration;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.security.auth.x500.X500Principal;

import es.gob.afirma.standalone.configurator.jre.misc.HexDumpEncoder;
import es.gob.afirma.standalone.configurator.jre.security.provider.X509Factory;
import es.gob.afirma.standalone.configurator.jre.security.util.DerEncoder;
import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;

/**
 * <p>
 * An implementation for X509 CRL (Certificate Revocation List).
 * <p>
 * The X.509 v2 CRL format is described below in ASN.1:
 * <pre>
 * CertificateList  ::=  SEQUENCE  {
 *     tbsCertList          TBSCertList,
 *     signatureAlgorithm   AlgorithmIdentifier,
 *     signature            BIT STRING  }
 * </pre>
 * More information can be found in
 * <a href="http://www.ietf.org/rfc/rfc3280.txt">RFC 3280: Internet X.509
 * Public Key Infrastructure Certificate and CRL Profile</a>.
 * <p>
 * The ASN.1 definition of <code>tbsCertList</code> is:
 * <pre>
 * TBSCertList  ::=  SEQUENCE  {
 *     version                 Version OPTIONAL,
 *                             -- if present, must be v2
 *     signature               AlgorithmIdentifier,
 *     issuer                  Name,
 *     thisUpdate              ChoiceOfTime,
 *     nextUpdate              ChoiceOfTime OPTIONAL,
 *     revokedCertificates     SEQUENCE OF SEQUENCE  {
 *         userCertificate         CertificateSerialNumber,
 *         revocationDate          ChoiceOfTime,
 *         crlEntryExtensions      Extensions OPTIONAL
 *                                 -- if present, must be v2
 *         }  OPTIONAL,
 *     crlExtensions           [0]  EXPLICIT Extensions OPTIONAL
 *                                  -- if present, must be v2
 *     }
 * </pre>
 *
 * @author Hemma Prafullchandra
 * @see X509CRL
 */
public class X509CRLImpl extends X509CRL implements DerEncoder {

    // CRL data, and its envelope
    private byte[]      signedCRL = null; // DER encoded crl
    private byte[]      signature = null; // raw signature bits
    private byte[]      tbsCertList = null; // DER encoded "to-be-signed" CRL
    private AlgorithmId sigAlgId = null; // sig alg in CRL

    // crl information
    private int              version;
    private AlgorithmId      infoSigAlgId; // sig alg in "to-be-signed" crl
    private X500Name         issuer = null;
    private X500Principal    issuerPrincipal = null;
    private Date             thisUpdate = null;
    private Date             nextUpdate = null;
    private final Map<X509IssuerSerial,X509CRLEntry> revokedMap = new TreeMap<>();
    private final List<X509CRLEntry> revokedList = new LinkedList<>();
    private CRLExtensions    extensions = null;
    private final static boolean isExplicit = true;
    private static final long YR_2050 = 2524636800000L;

    private boolean readOnly = false;

    /**
     * PublicKey that has previously been used to successfully verify
     * the signature of this CRL. Null if the CRL has not
     * yet been verified (successfully).
     */
    private PublicKey verifiedPublicKey;
    /**
     * If verifiedPublicKey is not null, name of the provider used to
     * successfully verify the signature of this CRL, or the
     * empty String if no provider was explicitly specified.
     */
    private String verifiedProvider;

    /**
     * Not to be used. As it would lead to cases of uninitialized
     * CRL objects.
     */
    private X509CRLImpl() { }

    /**
     * Unmarshals an X.509 CRL from its encoded form, parsing the encoded
     * bytes.  This form of constructor is used by agents which
     * need to examine and use CRL contents. Note that the buffer
     * must include only one CRL, and no "garbage" may be left at
     * the end.
     *
     * @param crlData the encoded bytes, with no trailing padding.
     * @exception CRLException on parsing errors.
     */
    public X509CRLImpl(final byte[] crlData) throws CRLException {
        try {
            parse(new DerValue(crlData));
        } catch (final IOException e) {
            this.signedCRL = null;
            throw new CRLException("Parsing error: " + e.getMessage()); //$NON-NLS-1$
        }
    }

    /**
     * Unmarshals an X.509 CRL from an DER value.
     *
     * @param val a DER value holding at least one CRL
     * @exception CRLException on parsing errors.
     */
    public X509CRLImpl(final DerValue val) throws CRLException {
        try {
            parse(val);
        } catch (final IOException e) {
            this.signedCRL = null;
            throw new CRLException("Parsing error: " + e.getMessage()); //$NON-NLS-1$
        }
    }

    /**
     * Unmarshals an X.509 CRL from an input stream. Only one CRL
     * is expected at the end of the input stream.
     *
     * @param inStrm an input stream holding at least one CRL
     * @exception CRLException on parsing errors.
     */
    public X509CRLImpl(final InputStream inStrm) throws CRLException {
        try {
            parse(new DerValue(inStrm));
        } catch (final IOException e) {
            this.signedCRL = null;
            throw new CRLException("Parsing error: " + e.getMessage()); //$NON-NLS-1$
        }
    }

    /**
     * Initial CRL constructor, no revoked certs, and no extensions.
     *
     * @param issuer the name of the CA issuing this CRL.
     * @param thisUpdate the Date of this issue.
     * @param nextUpdate the Date of the next CRL.
     */
    public X509CRLImpl(final X500Name issuer, final Date thisDate, final Date nextDate) {
        this.issuer = issuer;
        this.thisUpdate = thisDate;
        this.nextUpdate = nextDate;
    }

    /**
     * CRL constructor, revoked certs, no extensions.
     *
     * @param issuer the name of the CA issuing this CRL.
     * @param thisUpdate the Date of this issue.
     * @param nextUpdate the Date of the next CRL.
     * @param badCerts the array of CRL entries.
     *
     * @exception CRLException on parsing/construction errors.
     */
    public X509CRLImpl(final X500Name issuer, final Date thisDate, final Date nextDate,
                       final X509CRLEntry[] badCerts)
        throws CRLException
    {
        this.issuer = issuer;
        this.thisUpdate = thisDate;
        this.nextUpdate = nextDate;
        if (badCerts != null) {
            final X500Principal crlIssuer = getIssuerX500Principal();
            X500Principal badCertIssuer = crlIssuer;
            for (final X509CRLEntry badCert2 : badCerts) {
                final X509CRLEntryImpl badCert = (X509CRLEntryImpl)badCert2;
                try {
                    badCertIssuer = getCertIssuer(badCert, badCertIssuer);
                } catch (final IOException ioe) {
                    throw new CRLException(ioe);
                }
                badCert.setCertificateIssuer(crlIssuer, badCertIssuer);
                final X509IssuerSerial issuerSerial = new X509IssuerSerial
                    (badCertIssuer, badCert.getSerialNumber());
                this.revokedMap.put(issuerSerial, badCert);
                this.revokedList.add(badCert);
                if (badCert.hasExtensions()) {
                    this.version = 1;
                }
            }
        }
    }

    /**
     * CRL constructor, revoked certs and extensions.
     *
     * @param issuer the name of the CA issuing this CRL.
     * @param thisUpdate the Date of this issue.
     * @param nextUpdate the Date of the next CRL.
     * @param badCerts the array of CRL entries.
     * @param crlExts the CRL extensions.
     *
     * @exception CRLException on parsing/construction errors.
     */
    public X509CRLImpl(final X500Name issuer, final Date thisDate, final Date nextDate,
               final X509CRLEntry[] badCerts, final CRLExtensions crlExts)
        throws CRLException
    {
        this(issuer, thisDate, nextDate, badCerts);
        if (crlExts != null) {
            this.extensions = crlExts;
            this.version = 1;
        }
    }

    /**
     * Returned the encoding as an uncloned byte array. Callers must
     * guarantee that they neither modify it nor expose it to untrusted
     * code.
     */
    public byte[] getEncodedInternal() throws CRLException {
        if (this.signedCRL == null) {
            throw new CRLException("Null CRL to encode"); //$NON-NLS-1$
        }
        return this.signedCRL;
    }

    /**
     * Returns the ASN.1 DER encoded form of this CRL.
     *
     * @exception CRLException if an encoding error occurs.
     */
    @Override
	public byte[] getEncoded() throws CRLException {
        return getEncodedInternal().clone();
    }

    /**
     * Encodes the "to-be-signed" CRL to the OutputStream.
     *
     * @param out the OutputStream to write to.
     * @exception CRLException on encoding errors.
     */
    public void encodeInfo(final OutputStream out) throws CRLException {
        try {
            final DerOutputStream tmp = new DerOutputStream();
            final DerOutputStream rCerts = new DerOutputStream();
            final DerOutputStream seq = new DerOutputStream();

            if (this.version != 0) {
				tmp.putInteger(this.version);
			}
            this.infoSigAlgId.encode(tmp);
            if ((this.version == 0) && (this.issuer.toString() == null)) {
				throw new CRLException("Null Issuer DN not allowed in v1 CRL"); //$NON-NLS-1$
			}
            this.issuer.encode(tmp);

            if (this.thisUpdate.getTime() < YR_2050) {
				tmp.putUTCTime(this.thisUpdate);
			} else {
				tmp.putGeneralizedTime(this.thisUpdate);
			}

            if (this.nextUpdate != null) {
                if (this.nextUpdate.getTime() < YR_2050) {
					tmp.putUTCTime(this.nextUpdate);
				} else {
					tmp.putGeneralizedTime(this.nextUpdate);
				}
            }

            if (!this.revokedList.isEmpty()) {
                for (final X509CRLEntry entry : this.revokedList) {
                    ((X509CRLEntryImpl)entry).encode(rCerts);
                }
                tmp.write(DerValue.tag_Sequence, rCerts);
            }

            if (this.extensions != null) {
				this.extensions.encode(tmp, isExplicit);
			}

            seq.write(DerValue.tag_Sequence, tmp);

            this.tbsCertList = seq.toByteArray();
            out.write(this.tbsCertList);
        } catch (final IOException e) {
             throw new CRLException("Encoding error: " + e.getMessage()); //$NON-NLS-1$
        }
    }

    /**
     * Verifies that this CRL was signed using the
     * private key that corresponds to the given public key.
     *
     * @param key the PublicKey used to carry out the verification.
     *
     * @exception NoSuchAlgorithmException on unsupported signature
     * algorithms.
     * @exception InvalidKeyException on incorrect key.
     * @exception NoSuchProviderException if there's no default provider.
     * @exception SignatureException on signature errors.
     * @exception CRLException on encoding errors.
     */
    @Override
	public void verify(final PublicKey key)
    throws CRLException, NoSuchAlgorithmException, InvalidKeyException,
           NoSuchProviderException, SignatureException {
        verify(key, ""); //$NON-NLS-1$
    }

    /**
     * Verifies that this CRL was signed using the
     * private key that corresponds to the given public key,
     * and that the signature verification was computed by
     * the given provider.
     *
     * @param key the PublicKey used to carry out the verification.
     * @param sigProvider the name of the signature provider.
     *
     * @exception NoSuchAlgorithmException on unsupported signature
     * algorithms.
     * @exception InvalidKeyException on incorrect key.
     * @exception NoSuchProviderException on incorrect provider.
     * @exception SignatureException on signature errors.
     * @exception CRLException on encoding errors.
     */
    @Override
	public synchronized void verify(final PublicKey key, String sigProvider)
            throws CRLException, NoSuchAlgorithmException, InvalidKeyException,
            NoSuchProviderException, SignatureException {

        if (sigProvider == null) {
            sigProvider = ""; //$NON-NLS-1$
        }
        if ((this.verifiedPublicKey != null) && this.verifiedPublicKey.equals(key)) {
            // this CRL has already been successfully verified using
            // this public key. Make sure providers match, too.
            if (sigProvider.equals(this.verifiedProvider)) {
                return;
            }
        }
        if (this.signedCRL == null) {
            throw new CRLException("Uninitialized CRL"); //$NON-NLS-1$
        }
        Signature   sigVerf = null;
        if (sigProvider.length() == 0) {
            sigVerf = Signature.getInstance(this.sigAlgId.getName());
        } else {
            sigVerf = Signature.getInstance(this.sigAlgId.getName(), sigProvider);
        }
        sigVerf.initVerify(key);

        if (this.tbsCertList == null) {
            throw new CRLException("Uninitialized CRL"); //$NON-NLS-1$
        }

        sigVerf.update(this.tbsCertList, 0, this.tbsCertList.length);

        if (!sigVerf.verify(this.signature)) {
            throw new SignatureException("Signature does not match."); //$NON-NLS-1$
        }
        this.verifiedPublicKey = key;
        this.verifiedProvider = sigProvider;
    }

    /**
     * Verifies that this CRL was signed using the
     * private key that corresponds to the given public key,
     * and that the signature verification was computed by
     * the given provider. Note that the specified Provider object
     * does not have to be registered in the provider list.
     *
     * @param key the PublicKey used to carry out the verification.
     * @param sigProvider the signature provider.
     *
     * @exception NoSuchAlgorithmException on unsupported signature
     * algorithms.
     * @exception InvalidKeyException on incorrect key.
     * @exception SignatureException on signature errors.
     * @exception CRLException on encoding errors.
     */
    @Override
	public synchronized void verify(final PublicKey key, final Provider sigProvider)
            throws CRLException, NoSuchAlgorithmException, InvalidKeyException,
            SignatureException {

        if (this.signedCRL == null) {
            throw new CRLException("Uninitialized CRL"); //$NON-NLS-1$
        }
        Signature sigVerf = null;
        if (sigProvider == null) {
            sigVerf = Signature.getInstance(this.sigAlgId.getName());
        } else {
            sigVerf = Signature.getInstance(this.sigAlgId.getName(), sigProvider);
        }
        sigVerf.initVerify(key);

        if (this.tbsCertList == null) {
            throw new CRLException("Uninitialized CRL"); //$NON-NLS-1$
        }

        sigVerf.update(this.tbsCertList, 0, this.tbsCertList.length);

        if (!sigVerf.verify(this.signature)) {
            throw new SignatureException("Signature does not match."); //$NON-NLS-1$
        }
        this.verifiedPublicKey = key;
    }

    /**
     * This static method is the default implementation of the
     * verify(PublicKey key, Provider sigProvider) method in X509CRL.
     * Called from java.security.cert.X509CRL.verify(PublicKey key,
     * Provider sigProvider)
     */
    public static void verify(final X509CRL crl, final PublicKey key,
            final Provider sigProvider) throws CRLException,
            NoSuchAlgorithmException, InvalidKeyException, SignatureException {
        crl.verify(key, sigProvider);
    }

    /**
     * Encodes an X.509 CRL, and signs it using the given key.
     *
     * @param key the private key used for signing.
     * @param algorithm the name of the signature algorithm used.
     *
     * @exception NoSuchAlgorithmException on unsupported signature
     * algorithms.
     * @exception InvalidKeyException on incorrect key.
     * @exception NoSuchProviderException on incorrect provider.
     * @exception SignatureException on signature errors.
     * @exception CRLException if any mandatory data was omitted.
     */
    public void sign(final PrivateKey key, final String algorithm)
    throws CRLException, NoSuchAlgorithmException, InvalidKeyException,
        NoSuchProviderException, SignatureException {
        sign(key, algorithm, null);
    }

    /**
     * Encodes an X.509 CRL, and signs it using the given key.
     *
     * @param key the private key used for signing.
     * @param algorithm the name of the signature algorithm used.
     * @param provider the name of the provider.
     *
     * @exception NoSuchAlgorithmException on unsupported signature
     * algorithms.
     * @exception InvalidKeyException on incorrect key.
     * @exception NoSuchProviderException on incorrect provider.
     * @exception SignatureException on signature errors.
     * @exception CRLException if any mandatory data was omitted.
     */
    public void sign(final PrivateKey key, final String algorithm, final String provider)
    throws CRLException, NoSuchAlgorithmException, InvalidKeyException,
        NoSuchProviderException, SignatureException {
        try {
            if (this.readOnly) {
				throw new CRLException("cannot over-write existing CRL"); //$NON-NLS-1$
			}
            Signature sigEngine = null;
            if ((provider == null) || (provider.length() == 0)) {
				sigEngine = Signature.getInstance(algorithm);
			} else {
				sigEngine = Signature.getInstance(algorithm, provider);
			}

            sigEngine.initSign(key);

                                // in case the name is reset
            this.sigAlgId = AlgorithmId.get(sigEngine.getAlgorithm());
            this.infoSigAlgId = this.sigAlgId;

            final DerOutputStream out = new DerOutputStream();
            final DerOutputStream tmp = new DerOutputStream();

            // encode crl info
            encodeInfo(tmp);

            // encode algorithm identifier
            this.sigAlgId.encode(tmp);

            // Create and encode the signature itself.
            sigEngine.update(this.tbsCertList, 0, this.tbsCertList.length);
            this.signature = sigEngine.sign();
            tmp.putBitString(this.signature);

            // Wrap the signed data in a SEQUENCE { data, algorithm, sig }
            out.write(DerValue.tag_Sequence, tmp);
            this.signedCRL = out.toByteArray();
            this.readOnly = true;

        } catch (final IOException e) {
            throw new CRLException("Error while encoding data: " + //$NON-NLS-1$
                                   e.getMessage());
        }
    }

    /**
     * Returns a printable string of this CRL.
     *
     * @return value of this CRL in a printable form.
     */
    @Override
	public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("X.509 CRL v" + (this.version+1) + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
        if (this.sigAlgId != null) {
			sb.append("Signature Algorithm: " + this.sigAlgId.toString() + //$NON-NLS-1$
                  ", OID=" + (this.sigAlgId.getOID()).toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
        if (this.issuer != null) {
			sb.append("Issuer: " + this.issuer.toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
        if (this.thisUpdate != null) {
			sb.append("\nThis Update: " + this.thisUpdate.toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
        if (this.nextUpdate != null) {
			sb.append("Next Update: " + this.nextUpdate.toString() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
        if (this.revokedList.isEmpty()) {
			sb.append("\nNO certificates have been revoked\n"); //$NON-NLS-1$
		} else {
            sb.append("\nRevoked Certificates: " + this.revokedList.size()); //$NON-NLS-1$
            int i = 1;
            for (final X509CRLEntry entry: this.revokedList) {
                sb.append("\n[" + i++ + "] " + entry.toString()); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        if (this.extensions != null) {
            final Collection<Extension> allExts = this.extensions.getAllExtensions();
            final Object[] objs = allExts.toArray();
            sb.append("\nCRL Extensions: " + objs.length); //$NON-NLS-1$
            for (int i = 0; i < objs.length; i++) {
                sb.append("\n[" + (i+1) + "]: "); //$NON-NLS-1$ //$NON-NLS-2$
                final Extension ext = (Extension)objs[i];
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
					sb.append(ext.toString()); // sub-class exists
				}
                } catch (final Exception e) {
                    sb.append(", Error parsing this extension"); //$NON-NLS-1$
                }
            }
        }
        if (this.signature != null) {
            final HexDumpEncoder encoder = new HexDumpEncoder();
            sb.append("\nSignature:\n" + encoder.encodeBuffer(this.signature) //$NON-NLS-1$
                      + "\n"); //$NON-NLS-1$
        } else {
			sb.append("NOT signed yet\n"); //$NON-NLS-1$
		}
        return sb.toString();
    }

    /**
     * Checks whether the given certificate is on this CRL.
     *
     * @param cert the certificate to check for.
     * @return true if the given certificate is on this CRL,
     * false otherwise.
     */
    @Override
	public boolean isRevoked(final Certificate cert) {
        if (this.revokedMap.isEmpty() || (!(cert instanceof X509Certificate))) {
            return false;
        }
        final X509Certificate xcert = (X509Certificate) cert;
        final X509IssuerSerial issuerSerial = new X509IssuerSerial(xcert);
        return this.revokedMap.containsKey(issuerSerial);
    }

    /**
     * Gets the version number from this CRL.
     * The ASN.1 definition for this is:
     * <pre>
     * Version  ::=  INTEGER  {  v1(0), v2(1), v3(2)  }
     *             -- v3 does not apply to CRLs but appears for consistency
     *             -- with definition of Version for certs
     * </pre>
     * @return the version number, i.e. 1 or 2.
     */
    @Override
	public int getVersion() {
        return this.version+1;
    }

    /**
     * Gets the issuer distinguished name from this CRL.
     * The issuer name identifies the entity who has signed (and
     * issued the CRL). The issuer name field contains an
     * X.500 distinguished name (DN).
     * The ASN.1 definition for this is:
     * <pre>
     * issuer    Name
     *
     * Name ::= CHOICE { RDNSequence }
     * RDNSequence ::= SEQUENCE OF RelativeDistinguishedName
     * RelativeDistinguishedName ::=
     *     SET OF AttributeValueAssertion
     *
     * AttributeValueAssertion ::= SEQUENCE {
     *                               AttributeType,
     *                               AttributeValue }
     * AttributeType ::= OBJECT IDENTIFIER
     * AttributeValue ::= ANY
     * </pre>
     * The Name describes a hierarchical name composed of attributes,
     * such as country name, and corresponding values, such as US.
     * The type of the component AttributeValue is determined by the
     * AttributeType; in general it will be a directoryString.
     * A directoryString is usually one of PrintableString,
     * TeletexString or UniversalString.
     * @return the issuer name.
     */
    @Override
	public Principal getIssuerDN() {
        return this.issuer;
    }

    /**
     * Return the issuer as X500Principal. Overrides method in X509CRL
     * to provide a slightly more efficient version.
     */
    @Override
	public X500Principal getIssuerX500Principal() {
        if (this.issuerPrincipal == null) {
            this.issuerPrincipal = this.issuer.asX500Principal();
        }
        return this.issuerPrincipal;
    }

    /**
     * Gets the thisUpdate date from the CRL.
     * The ASN.1 definition for this is:
     *
     * @return the thisUpdate date from the CRL.
     */
    @Override
	public Date getThisUpdate() {
        return (new Date(this.thisUpdate.getTime()));
    }

    /**
     * Gets the nextUpdate date from the CRL.
     *
     * @return the nextUpdate date from the CRL, or null if
     * not present.
     */
    @Override
	public Date getNextUpdate() {
        if (this.nextUpdate == null) {
			return null;
		}
        return (new Date(this.nextUpdate.getTime()));
    }

    /**
     * Gets the CRL entry with the given serial number from this CRL.
     *
     * @return the entry with the given serial number, or <code>null</code> if
     * no such entry exists in the CRL.
     * @see X509CRLEntry
     */
    @Override
	public X509CRLEntry getRevokedCertificate(final BigInteger serialNumber) {
        if (this.revokedMap.isEmpty()) {
            return null;
        }
        // assume this is a direct CRL entry (cert and CRL issuer are the same)
        final X509IssuerSerial issuerSerial = new X509IssuerSerial
            (getIssuerX500Principal(), serialNumber);
        return this.revokedMap.get(issuerSerial);
    }

    /**
     * Gets the CRL entry for the given certificate.
     */
    @Override
	public X509CRLEntry getRevokedCertificate(final X509Certificate cert) {
        if (this.revokedMap.isEmpty()) {
            return null;
        }
        final X509IssuerSerial issuerSerial = new X509IssuerSerial(cert);
        return this.revokedMap.get(issuerSerial);
    }

    /**
     * Gets all the revoked certificates from the CRL.
     * A Set of X509CRLEntry.
     *
     * @return all the revoked certificates or <code>null</code> if there are
     * none.
     * @see X509CRLEntry
     */
    @Override
	public Set<X509CRLEntry> getRevokedCertificates() {
        if (this.revokedList.isEmpty()) {
            return null;
        } else {
            return new TreeSet<X509CRLEntry>(this.revokedList);
        }
    }

    /**
     * Gets the DER encoded CRL information, the
     * <code>tbsCertList</code> from this CRL.
     * This can be used to verify the signature independently.
     *
     * @return the DER encoded CRL information.
     * @exception CRLException on encoding errors.
     */
    @Override
	public byte[] getTBSCertList() throws CRLException {
        if (this.tbsCertList == null) {
			throw new CRLException("Uninitialized CRL"); //$NON-NLS-1$
		}
        final byte[] dup = new byte[this.tbsCertList.length];
        System.arraycopy(this.tbsCertList, 0, dup, 0, dup.length);
        return dup;
    }

    /**
     * Gets the raw Signature bits from the CRL.
     *
     * @return the signature.
     */
    @Override
	public byte[] getSignature() {
        if (this.signature == null) {
			return null;
		}
        final byte[] dup = new byte[this.signature.length];
        System.arraycopy(this.signature, 0, dup, 0, dup.length);
        return dup;
    }

    /**
     * Gets the signature algorithm name for the CRL
     * signature algorithm. For example, the string "SHA1withDSA".
     * The ASN.1 definition for this is:
     * <pre>
     * AlgorithmIdentifier  ::=  SEQUENCE  {
     *     algorithm               OBJECT IDENTIFIER,
     *     parameters              ANY DEFINED BY algorithm OPTIONAL  }
     *                             -- contains a value of the type
     *                             -- registered for use with the
     *                             -- algorithm object identifier value
     * </pre>
     *
     * @return the signature algorithm name.
     */
    @Override
	public String getSigAlgName() {
        if (this.sigAlgId == null) {
			return null;
		}
        return this.sigAlgId.getName();
    }

    /**
     * Gets the signature algorithm OID string from the CRL.
     * An OID is represented by a set of positive whole number separated
     * by ".", that means,<br>
     * &lt;positive whole number&gt;.&lt;positive whole number&gt;.&lt;...&gt;
     * For example, the string "1.2.840.10040.4.3" identifies the SHA-1
     * with DSA signature algorithm defined in
     * <a href="http://www.ietf.org/rfc/rfc3279.txt">RFC 3279: Algorithms and
     * Identifiers for the Internet X.509 Public Key Infrastructure Certificate
     * and CRL Profile</a>.
     *
     * @return the signature algorithm oid string.
     */
    @Override
	public String getSigAlgOID() {
        if (this.sigAlgId == null) {
			return null;
		}
        final ObjectIdentifier oid = this.sigAlgId.getOID();
        return oid.toString();
    }

    /**
     * Gets the DER encoded signature algorithm parameters from this
     * CRL's signature algorithm. In most cases, the signature
     * algorithm parameters are null, the parameters are usually
     * supplied with the Public Key.
     *
     * @return the DER encoded signature algorithm parameters, or
     *         null if no parameters are present.
     */
    @Override
	public byte[] getSigAlgParams() {
        if (this.sigAlgId == null) {
			return null;
		}
        try {
            return this.sigAlgId.getEncodedParams();
        } catch (final IOException e) {
            return null;
        }
    }

    /**
     * Gets the signature AlgorithmId from the CRL.
     *
     * @return the signature AlgorithmId
     */
    public AlgorithmId getSigAlgId() {
        return this.sigAlgId;
    }

    /**
     * return the AuthorityKeyIdentifier, if any.
     *
     * @returns AuthorityKeyIdentifier or null
     *          (if no AuthorityKeyIdentifierExtension)
     * @throws IOException on error
     */
    public KeyIdentifier getAuthKeyId() throws IOException {
        final AuthorityKeyIdentifierExtension aki = getAuthKeyIdExtension();
        if (aki != null) {
            final KeyIdentifier keyId = (KeyIdentifier)aki.get(
                    AuthorityKeyIdentifierExtension.KEY_ID);
            return keyId;
        } else {
            return null;
        }
    }

    /**
     * return the AuthorityKeyIdentifierExtension, if any.
     *
     * @returns AuthorityKeyIdentifierExtension or null (if no such extension)
     * @throws IOException on error
     */
    public AuthorityKeyIdentifierExtension getAuthKeyIdExtension()
        throws IOException {
        final Object obj = getExtension(PKIXExtensions.AuthorityKey_Id);
        return (AuthorityKeyIdentifierExtension)obj;
    }

    /**
     * return the CRLNumberExtension, if any.
     *
     * @returns CRLNumberExtension or null (if no such extension)
     * @throws IOException on error
     */
    public CRLNumberExtension getCRLNumberExtension() throws IOException {
        final Object obj = getExtension(PKIXExtensions.CRLNumber_Id);
        return (CRLNumberExtension)obj;
    }

    /**
     * return the CRL number from the CRLNumberExtension, if any.
     *
     * @returns number or null (if no such extension)
     * @throws IOException on error
     */
    public BigInteger getCRLNumber() throws IOException {
        final CRLNumberExtension numExt = getCRLNumberExtension();
        if (numExt != null) {
            final BigInteger num = numExt.get(CRLNumberExtension.NUMBER);
            return num;
        } else {
            return null;
        }
    }

    /**
     * return the DeltaCRLIndicatorExtension, if any.
     *
     * @returns DeltaCRLIndicatorExtension or null (if no such extension)
     * @throws IOException on error
     */
    public DeltaCRLIndicatorExtension getDeltaCRLIndicatorExtension()
        throws IOException {

        final Object obj = getExtension(PKIXExtensions.DeltaCRLIndicator_Id);
        return (DeltaCRLIndicatorExtension)obj;
    }

    /**
     * return the base CRL number from the DeltaCRLIndicatorExtension, if any.
     *
     * @returns number or null (if no such extension)
     * @throws IOException on error
     */
    public BigInteger getBaseCRLNumber() throws IOException {
        final DeltaCRLIndicatorExtension dciExt = getDeltaCRLIndicatorExtension();
        if (dciExt != null) {
            final BigInteger num = dciExt.get(DeltaCRLIndicatorExtension.NUMBER);
            return num;
        } else {
            return null;
        }
    }

    /**
     * return the IssuerAlternativeNameExtension, if any.
     *
     * @returns IssuerAlternativeNameExtension or null (if no such extension)
     * @throws IOException on error
     */
    public IssuerAlternativeNameExtension getIssuerAltNameExtension()
        throws IOException {
        final Object obj = getExtension(PKIXExtensions.IssuerAlternativeName_Id);
        return (IssuerAlternativeNameExtension)obj;
    }

    /**
     * return the IssuingDistributionPointExtension, if any.
     *
     * @returns IssuingDistributionPointExtension or null
     *          (if no such extension)
     * @throws IOException on error
     */
    public IssuingDistributionPointExtension
        getIssuingDistributionPointExtension() throws IOException {

        final Object obj = getExtension(PKIXExtensions.IssuingDistributionPoint_Id);
        return (IssuingDistributionPointExtension) obj;
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
     * Gets a Set of the extension(s) marked CRITICAL in the
     * CRL. In the returned set, each extension is represented by
     * its OID string.
     *
     * @return a set of the extension oid strings in the
     * CRL that are marked critical.
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
     * Gets a Set of the extension(s) marked NON-CRITICAL in the
     * CRL. In the returned set, each extension is represented by
     * its OID string.
     *
     * @return a set of the extension oid strings in the
     * CRL that are NOT marked critical.
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
     * (<code>extnValue</code>) identified by the passed in oid String.
     * The <code>oid</code> string is
     * represented by a set of positive whole number separated
     * by ".", that means,<br>
     * &lt;positive whole number&gt;.&lt;positive whole number&gt;.&lt;...&gt;
     *
     * @param oid the Object Identifier value for the extension.
     * @return the der encoded octet string of the extension value.
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
     * @returns Object of type <extension> or null, if not found
     * @throws IOException on error
     */
    public Object getExtension(final ObjectIdentifier oid) {
        if (this.extensions == null) {
			return null;
		}

        // XXX Consider cloning this
        return this.extensions.get(OIDMap.getName(oid));
    }

    /*
     * Parses an X.509 CRL, should be used only by constructors.
     */
    private void parse(final DerValue val) throws CRLException, IOException {
        // check if can over write the certificate
        if (this.readOnly) {
			throw new CRLException("cannot over-write existing CRL"); //$NON-NLS-1$
		}

        if ( val.getData() == null || val.tag != DerValue.tag_Sequence) {
			throw new CRLException("Invalid DER-encoded CRL data"); //$NON-NLS-1$
		}

        this.signedCRL = val.toByteArray();
        final DerValue seq[] = new DerValue[3];

        seq[0] = val.data.getDerValue();
        seq[1] = val.data.getDerValue();
        seq[2] = val.data.getDerValue();

        if (val.data.available() != 0) {
			throw new CRLException("signed overrun, bytes = " //$NON-NLS-1$
                                     + val.data.available());
		}

        if (seq[0].tag != DerValue.tag_Sequence) {
			throw new CRLException("signed CRL fields invalid"); //$NON-NLS-1$
		}

        this.sigAlgId = AlgorithmId.parse(seq[1]);
        this.signature = seq[2].getBitString();

        if (seq[1].data.available() != 0) {
			throw new CRLException("AlgorithmId field overrun"); //$NON-NLS-1$
		}

        if (seq[2].data.available() != 0) {
			throw new CRLException("Signature field overrun"); //$NON-NLS-1$
		}

        // the tbsCertsList
        this.tbsCertList = seq[0].toByteArray();

        // parse the information
        final DerInputStream derStrm = seq[0].data;
        DerValue       tmp;
        byte           nextByte;

        // version (optional if v1)
        this.version = 0;   // by default, version = v1 == 0
        nextByte = (byte)derStrm.peekByte();
        if (nextByte == DerValue.tag_Integer) {
            this.version = derStrm.getInteger();
            if (this.version != 1) {
				throw new CRLException("Invalid version"); //$NON-NLS-1$
			}
        }
        tmp = derStrm.getDerValue();

        // signature
        final AlgorithmId tmpId = AlgorithmId.parse(tmp);

        // the "inner" and "outer" signature algorithms must match
        if (! tmpId.equals(this.sigAlgId)) {
			throw new CRLException("Signature algorithm mismatch"); //$NON-NLS-1$
		}
        this.infoSigAlgId = tmpId;

        // issuer
        this.issuer = new X500Name(derStrm);
        if (this.issuer.isEmpty()) {
            throw new CRLException("Empty issuer DN not allowed in X509CRLs"); //$NON-NLS-1$
        }

        // thisUpdate
        // check if UTCTime encoded or GeneralizedTime

        nextByte = (byte)derStrm.peekByte();
        if (nextByte == DerValue.tag_UtcTime) {
            this.thisUpdate = derStrm.getUTCTime();
        } else if (nextByte == DerValue.tag_GeneralizedTime) {
            this.thisUpdate = derStrm.getGeneralizedTime();
        } else {
            throw new CRLException("Invalid encoding for thisUpdate" //$NON-NLS-1$
                                   + " (tag=" + nextByte + ")"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        if (derStrm.available() == 0)
		 {
			return;     // done parsing no more optional fields present
		}

        // nextUpdate (optional)
        nextByte = (byte)derStrm.peekByte();
        if (nextByte == DerValue.tag_UtcTime) {
            this.nextUpdate = derStrm.getUTCTime();
        } else if (nextByte == DerValue.tag_GeneralizedTime) {
            this.nextUpdate = derStrm.getGeneralizedTime();
        } // else it is not present

        if (derStrm.available() == 0)
		 {
			return;     // done parsing no more optional fields present
		}

        // revokedCertificates (optional)
        nextByte = (byte)derStrm.peekByte();
        if ((nextByte == DerValue.tag_SequenceOf)
            && (! ((nextByte & 0x0c0) == 0x080))) {
            final DerValue[] badCerts = derStrm.getSequence(4);

            final X500Principal crlIssuer = getIssuerX500Principal();
            X500Principal badCertIssuer = crlIssuer;
            for (final DerValue badCert : badCerts) {
                final X509CRLEntryImpl entry = new X509CRLEntryImpl(badCert);
                badCertIssuer = getCertIssuer(entry, badCertIssuer);
                entry.setCertificateIssuer(crlIssuer, badCertIssuer);
                final X509IssuerSerial issuerSerial = new X509IssuerSerial
                    (badCertIssuer, entry.getSerialNumber());
                this.revokedMap.put(issuerSerial, entry);
                this.revokedList.add(entry);
            }
        }

        if (derStrm.available() == 0)
		 {
			return;     // done parsing no extensions
		}

        // crlExtensions (optional)
        tmp = derStrm.getDerValue();
        if (tmp.isConstructed() && tmp.isContextSpecific((byte)0)) {
            this.extensions = new CRLExtensions(tmp.data);
        }
        this.readOnly = true;
    }

    /**
     * Extract the issuer X500Principal from an X509CRL. Parses the encoded
     * form of the CRL to preserve the principal's ASN.1 encoding.
     *
     * Called by java.security.cert.X509CRL.getIssuerX500Principal().
     */
    public static X500Principal getIssuerX500Principal(final X509CRL crl) {
        try {
            final byte[] encoded = crl.getEncoded();
            final DerInputStream derIn = new DerInputStream(encoded);
            final DerValue tbsCert = derIn.getSequence(3)[0];
            final DerInputStream tbsIn = tbsCert.data;

            DerValue tmp;
            // skip version number if present
            final byte nextByte = (byte)tbsIn.peekByte();
            if (nextByte == DerValue.tag_Integer) {
                tmp = tbsIn.getDerValue();
            }

            tmp = tbsIn.getDerValue();  // skip signature
            tmp = tbsIn.getDerValue();  // issuer
            final byte[] principalBytes = tmp.toByteArray();
            return new X500Principal(principalBytes);
        } catch (final Exception e) {
            throw new RuntimeException("Could not parse issuer", e); //$NON-NLS-1$
        }
    }

    /**
     * Returned the encoding of the given certificate for internal use.
     * Callers must guarantee that they neither modify it nor expose it
     * to untrusted code. Uses getEncodedInternal() if the certificate
     * is instance of X509CertImpl, getEncoded() otherwise.
     */
    public static byte[] getEncodedInternal(final X509CRL crl) throws CRLException {
        if (crl instanceof X509CRLImpl) {
            return ((X509CRLImpl)crl).getEncodedInternal();
        } else {
            return crl.getEncoded();
        }
    }

    /**
     * Utility method to convert an arbitrary instance of X509CRL
     * to a X509CRLImpl. Does a cast if possible, otherwise reparses
     * the encoding.
     */
    public static X509CRLImpl toImpl(final X509CRL crl)
            throws CRLException {
        if (crl instanceof X509CRLImpl) {
            return (X509CRLImpl)crl;
        } else {
            return X509Factory.intern(crl);
        }
    }

    /**
     * Returns the X500 certificate issuer DN of a CRL entry.
     *
     * @param entry the entry to check
     * @param prevCertIssuer the previous entry's certificate issuer
     * @return the X500Principal in a CertificateIssuerExtension, or
     *   prevCertIssuer if it does not exist
     */
    private X500Principal getCertIssuer(final X509CRLEntryImpl entry,
        final X500Principal prevCertIssuer) throws IOException {

        final CertificateIssuerExtension ciExt =
            entry.getCertificateIssuerExtension();
        if (ciExt != null) {
            final GeneralNames names = ciExt.get(CertificateIssuerExtension.ISSUER);
            final X500Name issuerDN = (X500Name) names.get(0).getName();
            return issuerDN.asX500Principal();
        } else {
            return prevCertIssuer;
        }
    }

    @Override
    public void derEncode(final OutputStream out) throws IOException {
        if (this.signedCRL == null) {
			throw new IOException("Null CRL to encode"); //$NON-NLS-1$
		}
        out.write(this.signedCRL.clone());
    }

    /**
     * Immutable X.509 Certificate Issuer DN and serial number pair
     */
    private final static class X509IssuerSerial
            implements Comparable<X509IssuerSerial> {
        final X500Principal issuer;
        final BigInteger serial;
        volatile int hashcode = 0;

        /**
         * Create an X509IssuerSerial.
         *
         * @param issuer the issuer DN
         * @param serial the serial number
         */
        X509IssuerSerial(final X500Principal issuer, final BigInteger serial) {
            this.issuer = issuer;
            this.serial = serial;
        }

        /**
         * Construct an X509IssuerSerial from an X509Certificate.
         */
        X509IssuerSerial(final X509Certificate cert) {
            this(cert.getIssuerX500Principal(), cert.getSerialNumber());
        }

        /**
         * Returns the issuer.
         *
         * @return the issuer
         */
        X500Principal getIssuer() {
            return this.issuer;
        }

        /**
         * Returns the serial number.
         *
         * @return the serial number
         */
        BigInteger getSerial() {
            return this.serial;
        }

        /**
         * Compares this X509Serial with another and returns true if they
         * are equivalent.
         *
         * @param o the other object to compare with
         * @return true if equal, false otherwise
         */
        @Override
		public boolean equals(final Object o) {
            if (o == this) {
                return true;
            }

            if (!(o instanceof X509IssuerSerial)) {
                return false;
            }

            final X509IssuerSerial other = (X509IssuerSerial) o;
            if (this.serial.equals(other.getSerial()) &&
                this.issuer.equals(other.getIssuer())) {
                return true;
            }
            return false;
        }

        /**
         * Returns a hash code value for this X509IssuerSerial.
         *
         * @return the hash code value
         */
        @Override
		public int hashCode() {
            if (this.hashcode == 0) {
                int result = 17;
                result = 37*result + this.issuer.hashCode();
                result = 37*result + this.serial.hashCode();
                this.hashcode = result;
            }
            return this.hashcode;
        }

        @Override
        public int compareTo(final X509IssuerSerial another) {
            final int cissuer = this.issuer.toString()
                    .compareTo(another.issuer.toString());
            if (cissuer != 0) {
				return cissuer;
			}
            return this.serial.compareTo(another.serial);
        }
    }
}
