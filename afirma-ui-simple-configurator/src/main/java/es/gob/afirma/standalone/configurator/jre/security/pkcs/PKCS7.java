/*
 * Copyright (c) 1996, 2013, Oracle and/or its affiliates. All rights reserved.
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

package es.gob.afirma.standalone.configurator.jre.security.pkcs;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.net.URI;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Principal;
import java.security.SecureRandom;
import java.security.SignatureException;
import java.security.cert.CRLException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509CRL;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import es.gob.afirma.standalone.configurator.jre.security.timestamp.HttpTimestamper;
import es.gob.afirma.standalone.configurator.jre.security.timestamp.TSRequest;
import es.gob.afirma.standalone.configurator.jre.security.timestamp.TSResponse;
import es.gob.afirma.standalone.configurator.jre.security.timestamp.TimestampToken;
import es.gob.afirma.standalone.configurator.jre.security.timestamp.Timestamper;
import es.gob.afirma.standalone.configurator.jre.security.util.Debug;
import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;
import es.gob.afirma.standalone.configurator.jre.security.x509.AlgorithmId;
import es.gob.afirma.standalone.configurator.jre.security.x509.X500Name;
import es.gob.afirma.standalone.configurator.jre.security.x509.X509CRLImpl;
import es.gob.afirma.standalone.configurator.jre.security.x509.X509CertImpl;
import es.gob.afirma.standalone.configurator.jre.security.x509.X509CertInfo;


/**
 * PKCS7 as defined in RSA Laboratories PKCS7 Technical Note. Profile
 * Supports only <tt>SignedData</tt> ContentInfo
 * type, where to the type of data signed is plain Data.
 * For signedData, <tt>crls</tt>, <tt>attributes</tt> and
 * PKCS#6 Extended Certificates are not supported.
 *
 * @author Benjamin Renaud
 */
public class PKCS7 {

    private ObjectIdentifier contentType;

    // the ASN.1 members for a signedData (and other) contentTypes
    private BigInteger version = null;
    private AlgorithmId[] digestAlgorithmIds = null;
    private ContentInfo contentInfo = null;
    private X509Certificate[] certificates = null;
    private X509CRL[] crls = null;
    private SignerInfo[] signerInfos = null;

    private boolean oldStyle = false; // Is this JDK1.1.x-style?

    private Principal[] certIssuerNames;

    /*
     * Random number generator for creating nonce values
     * (Lazy initialization)
     */
    private static class SecureRandomHolder {
        static final SecureRandom RANDOM;
        static {
            SecureRandom tmp = null;
            try {
                tmp = SecureRandom.getInstance("SHA1PRNG");
            } catch (final NoSuchAlgorithmException e) {
                // should not happen
            }
            RANDOM = tmp;
        }
    }

    /*
     * Object identifier for the timestamping key purpose.
     */
    private static final String KP_TIMESTAMPING_OID = "1.3.6.1.5.5.7.3.8";

    /*
     * Object identifier for extendedKeyUsage extension
     */
    private static final String EXTENDED_KEY_USAGE_OID = "2.5.29.37";

    /**
     * Unmarshals a PKCS7 block from its encoded form, parsing the
     * encoded bytes from the InputStream.
     *
     * @param in an input stream holding at least one PKCS7 block.
     * @exception ParsingException on parsing errors.
     * @exception IOException on other errors.
     */
    public PKCS7(final InputStream in) throws ParsingException, IOException {
        final DataInputStream dis = new DataInputStream(in);
        final byte[] data = new byte[dis.available()];
        dis.readFully(data);

        parse(new DerInputStream(data));
    }

    /**
     * Unmarshals a PKCS7 block from its encoded form, parsing the
     * encoded bytes from the DerInputStream.
     *
     * @param derin a DerInputStream holding at least one PKCS7 block.
     * @exception ParsingException on parsing errors.
     */
    public PKCS7(final DerInputStream derin) throws ParsingException {
        parse(derin);
    }

    /**
     * Unmarshals a PKCS7 block from its encoded form, parsing the
     * encoded bytes.
     *
     * @param bytes the encoded bytes.
     * @exception ParsingException on parsing errors.
     */
    public PKCS7(final byte[] bytes) throws ParsingException {
        try {
            final DerInputStream derin = new DerInputStream(bytes);
            parse(derin);
        } catch (final IOException ioe1) {
            final ParsingException pe = new ParsingException(
                "Unable to parse the encoded bytes");
            pe.initCause(ioe1);
            throw pe;
        }
    }

    /*
     * Parses a PKCS#7 block.
     */
    private void parse(final DerInputStream derin)
        throws ParsingException
    {
        try {
            derin.mark(derin.available());
            // try new (i.e., JDK1.2) style
            parse(derin, false);
        } catch (final IOException ioe) {
            try {
                derin.reset();
                // try old (i.e., JDK1.1.x) style
                parse(derin, true);
                this.oldStyle = true;
            } catch (final IOException ioe1) {
                final ParsingException pe = new ParsingException(
                    ioe1.getMessage());
                pe.initCause(ioe);
                pe.addSuppressed(ioe1);
                throw pe;
            }
        }
    }

    /**
     * Parses a PKCS#7 block.
     *
     * @param derin the ASN.1 encoding of the PKCS#7 block.
     * @param oldStyle flag indicating whether or not the given PKCS#7 block
     * is encoded according to JDK1.1.x.
     */
    private void parse(final DerInputStream derin, final boolean oldStyle)
        throws IOException
    {
        this.contentInfo = new ContentInfo(derin, oldStyle);
        this.contentType = this.contentInfo.contentType;
        final DerValue content = this.contentInfo.getContent();

        if (this.contentType.equals((Object)ContentInfo.SIGNED_DATA_OID)) {
            parseSignedData(content);
        } else if (this.contentType.equals((Object)ContentInfo.OLD_SIGNED_DATA_OID)) {
            // This is for backwards compatibility with JDK 1.1.x
            parseOldSignedData(content);
        } else if (this.contentType.equals((Object)
                       ContentInfo.NETSCAPE_CERT_SEQUENCE_OID)){
            parseNetscapeCertChain(content);
        } else {
            throw new ParsingException("content type " + this.contentType +
                                       " not supported.");
        }
    }

    /**
     * Construct an initialized PKCS7 block.
     *
     * @param digestAlgorithmIds the message digest algorithm identifiers.
     * @param contentInfo the content information.
     * @param certificates an array of X.509 certificates.
     * @param crls an array of CRLs
     * @param signerInfos an array of signer information.
     */
    public PKCS7(final AlgorithmId[] digestAlgorithmIds,
                 final ContentInfo contentInfo,
                 final X509Certificate[] certificates,
                 final X509CRL[] crls,
                 final SignerInfo[] signerInfos) {

        this.version = BigInteger.ONE;
        this.digestAlgorithmIds = digestAlgorithmIds;
        this.contentInfo = contentInfo;
        this.certificates = certificates;
        this.crls = crls;
        this.signerInfos = signerInfos;
    }

    public PKCS7(final AlgorithmId[] digestAlgorithmIds,
                 final ContentInfo contentInfo,
                 final X509Certificate[] certificates,
                 final SignerInfo[] signerInfos) {
        this(digestAlgorithmIds, contentInfo, certificates, null, signerInfos);
    }

    private void parseNetscapeCertChain(final DerValue val)
    throws ParsingException, IOException {
        final DerInputStream dis = new DerInputStream(val.toByteArray());
        final DerValue[] contents = dis.getSequence(2);
        this.certificates = new X509Certificate[contents.length];

        CertificateFactory certfac = null;
        try {
            certfac = CertificateFactory.getInstance("X.509");
        } catch (final CertificateException ce) {
            // do nothing
        }

        for (int i=0; i < contents.length; i++) {
            ByteArrayInputStream bais = null;
            try {
                if (certfac == null) {
					this.certificates[i] = new X509CertImpl(contents[i]);
				} else {
                    final byte[] encoded = contents[i].toByteArray();
                    bais = new ByteArrayInputStream(encoded);
                    this.certificates[i] =
                        (X509Certificate)certfac.generateCertificate(bais);
                    bais.close();
                    bais = null;
                }
            } catch (final CertificateException ce) {
                final ParsingException pe = new ParsingException(ce.getMessage());
                pe.initCause(ce);
                throw pe;
            } catch (final IOException ioe) {
                final ParsingException pe = new ParsingException(ioe.getMessage());
                pe.initCause(ioe);
                throw pe;
            } finally {
                if (bais != null) {
					bais.close();
				}
            }
        }
    }

    private void parseSignedData(final DerValue val)
        throws ParsingException, IOException {

        final DerInputStream dis = val.toDerInputStream();

        // Version
        this.version = dis.getBigInteger();

        // digestAlgorithmIds
        final DerValue[] digestAlgorithmIdVals = dis.getSet(1);
        int len = digestAlgorithmIdVals.length;
        this.digestAlgorithmIds = new AlgorithmId[len];
        try {
            for (int i = 0; i < len; i++) {
                final DerValue oid = digestAlgorithmIdVals[i];
                this.digestAlgorithmIds[i] = AlgorithmId.parse(oid);
            }

        } catch (final IOException e) {
            final ParsingException pe =
                new ParsingException("Error parsing digest AlgorithmId IDs: " +
                                     e.getMessage());
            pe.initCause(e);
            throw pe;
        }
        // contentInfo
        this.contentInfo = new ContentInfo(dis);

        CertificateFactory certfac = null;
        try {
            certfac = CertificateFactory.getInstance("X.509");
        } catch (final CertificateException ce) {
            // do nothing
        }

        /*
         * check if certificates (implicit tag) are provided
         * (certificates are OPTIONAL)
         */
        if ((byte)(dis.peekByte()) == (byte)0xA0) {
            final DerValue[] certVals = dis.getSet(2, true);

            len = certVals.length;
            this.certificates = new X509Certificate[len];
            int count = 0;

            for (int i = 0; i < len; i++) {
                ByteArrayInputStream bais = null;
                try {
                    final byte tag = certVals[i].getTag();
                    // We only parse the normal certificate. Other types of
                    // CertificateChoices ignored.
                    if (tag == DerValue.tag_Sequence) {
                        if (certfac == null) {
                            this.certificates[count] = new X509CertImpl(certVals[i]);
                        } else {
                            final byte[] encoded = certVals[i].toByteArray();
                            bais = new ByteArrayInputStream(encoded);
                            this.certificates[count] =
                                (X509Certificate)certfac.generateCertificate(bais);
                            bais.close();
                            bais = null;
                        }
                        count++;
                    }
                } catch (final CertificateException ce) {
                    final ParsingException pe = new ParsingException(ce.getMessage());
                    pe.initCause(ce);
                    throw pe;
                } catch (final IOException ioe) {
                    final ParsingException pe = new ParsingException(ioe.getMessage());
                    pe.initCause(ioe);
                    throw pe;
                } finally {
                    if (bais != null) {
						bais.close();
					}
                }
            }
            if (count != len) {
                this.certificates = Arrays.copyOf(this.certificates, count);
            }
        }

        // check if crls (implicit tag) are provided (crls are OPTIONAL)
        if ((byte)(dis.peekByte()) == (byte)0xA1) {
            final DerValue[] crlVals = dis.getSet(1, true);

            len = crlVals.length;
            this.crls = new X509CRL[len];

            for (int i = 0; i < len; i++) {
                ByteArrayInputStream bais = null;
                try {
                    if (certfac == null) {
						this.crls[i] = new X509CRLImpl(crlVals[i]);
					} else {
                        final byte[] encoded = crlVals[i].toByteArray();
                        bais = new ByteArrayInputStream(encoded);
                        this.crls[i] = (X509CRL) certfac.generateCRL(bais);
                        bais.close();
                        bais = null;
                    }
                } catch (final CRLException e) {
                    final ParsingException pe =
                        new ParsingException(e.getMessage());
                    pe.initCause(e);
                    throw pe;
                } finally {
                    if (bais != null) {
						bais.close();
					}
                }
            }
        }

        // signerInfos
        final DerValue[] signerInfoVals = dis.getSet(1);

        len = signerInfoVals.length;
        this.signerInfos = new SignerInfo[len];

        for (int i = 0; i < len; i++) {
            final DerInputStream in = signerInfoVals[i].toDerInputStream();
            this.signerInfos[i] = new SignerInfo(in);
        }
    }

    /*
     * Parses an old-style SignedData encoding (for backwards
     * compatibility with JDK1.1.x).
     */
    private void parseOldSignedData(final DerValue val)
        throws ParsingException, IOException
    {
        final DerInputStream dis = val.toDerInputStream();

        // Version
        this.version = dis.getBigInteger();

        // digestAlgorithmIds
        final DerValue[] digestAlgorithmIdVals = dis.getSet(1);
        int len = digestAlgorithmIdVals.length;

        this.digestAlgorithmIds = new AlgorithmId[len];
        try {
            for (int i = 0; i < len; i++) {
                final DerValue oid = digestAlgorithmIdVals[i];
                this.digestAlgorithmIds[i] = AlgorithmId.parse(oid);
            }
        } catch (final IOException e) {
            throw new ParsingException("Error parsing digest AlgorithmId IDs");
        }

        // contentInfo
        this.contentInfo = new ContentInfo(dis, true);

        // certificates
        CertificateFactory certfac = null;
        try {
            certfac = CertificateFactory.getInstance("X.509");
        } catch (final CertificateException ce) {
            // do nothing
        }
        final DerValue[] certVals = dis.getSet(2);
        len = certVals.length;
        this.certificates = new X509Certificate[len];

        for (int i = 0; i < len; i++) {
            ByteArrayInputStream bais = null;
            try {
                if (certfac == null) {
					this.certificates[i] = new X509CertImpl(certVals[i]);
				} else {
                    final byte[] encoded = certVals[i].toByteArray();
                    bais = new ByteArrayInputStream(encoded);
                    this.certificates[i] =
                        (X509Certificate)certfac.generateCertificate(bais);
                    bais.close();
                    bais = null;
                }
            } catch (final CertificateException ce) {
                final ParsingException pe = new ParsingException(ce.getMessage());
                pe.initCause(ce);
                throw pe;
            } catch (final IOException ioe) {
                final ParsingException pe = new ParsingException(ioe.getMessage());
                pe.initCause(ioe);
                throw pe;
            } finally {
                if (bais != null) {
					bais.close();
				}
            }
        }

        // crls are ignored.
        dis.getSet(0);

        // signerInfos
        final DerValue[] signerInfoVals = dis.getSet(1);
        len = signerInfoVals.length;
        this.signerInfos = new SignerInfo[len];
        for (int i = 0; i < len; i++) {
            final DerInputStream in = signerInfoVals[i].toDerInputStream();
            this.signerInfos[i] = new SignerInfo(in, true);
        }
    }

    /**
     * Encodes the signed data to an output stream.
     *
     * @param out the output stream to write the encoded data to.
     * @exception IOException on encoding errors.
     */
    public void encodeSignedData(final OutputStream out) throws IOException {
        final DerOutputStream derout = new DerOutputStream();
        encodeSignedData(derout);
        out.write(derout.toByteArray());
    }

    /**
     * Encodes the signed data to a DerOutputStream.
     *
     * @param out the DerOutputStream to write the encoded data to.
     * @exception IOException on encoding errors.
     */
    public void encodeSignedData(final DerOutputStream out)
        throws IOException
    {
        final DerOutputStream signedData = new DerOutputStream();

        // version
        signedData.putInteger(this.version);

        // digestAlgorithmIds
        signedData.putOrderedSetOf(DerValue.tag_Set, this.digestAlgorithmIds);

        // contentInfo
        this.contentInfo.encode(signedData);

        // certificates (optional)
        if (this.certificates != null && this.certificates.length != 0) {
            // cast to X509CertImpl[] since X509CertImpl implements DerEncoder
            final X509CertImpl implCerts[] = new X509CertImpl[this.certificates.length];
            for (int i = 0; i < this.certificates.length; i++) {
                if (this.certificates[i] instanceof X509CertImpl) {
					implCerts[i] = (X509CertImpl) this.certificates[i];
				} else {
                    try {
                        final byte[] encoded = this.certificates[i].getEncoded();
                        implCerts[i] = new X509CertImpl(encoded);
                    } catch (final CertificateException ce) {
                        throw new IOException(ce);
                    }
                }
            }

            // Add the certificate set (tagged with [0] IMPLICIT)
            // to the signed data
            signedData.putOrderedSetOf((byte)0xA0, implCerts);
        }

        // CRLs (optional)
        if (this.crls != null && this.crls.length != 0) {
            // cast to X509CRLImpl[] since X509CRLImpl implements DerEncoder
            final Set<X509CRLImpl> implCRLs = new HashSet<X509CRLImpl>(this.crls.length);
            for (final X509CRL crl: this.crls) {
                if (crl instanceof X509CRLImpl) {
					implCRLs.add((X509CRLImpl) crl);
				} else {
                    try {
                        final byte[] encoded = crl.getEncoded();
                        implCRLs.add(new X509CRLImpl(encoded));
                    } catch (final CRLException ce) {
                        throw new IOException(ce);
                    }
                }
            }

            // Add the CRL set (tagged with [1] IMPLICIT)
            // to the signed data
            signedData.putOrderedSetOf((byte)0xA1,
                    implCRLs.toArray(new X509CRLImpl[implCRLs.size()]));
        }

        // signerInfos
        signedData.putOrderedSetOf(DerValue.tag_Set, this.signerInfos);

        // making it a signed data block
        final DerValue signedDataSeq = new DerValue(DerValue.tag_Sequence,
                                              signedData.toByteArray());

        // making it a content info sequence
        final ContentInfo block = new ContentInfo(ContentInfo.SIGNED_DATA_OID,
                                            signedDataSeq);

        // writing out the contentInfo sequence
        block.encode(out);
    }

    /**
     * This verifies a given SignerInfo.
     *
     * @param info the signer information.
     * @param bytes the DER encoded content information.
     *
     * @exception NoSuchAlgorithmException on unrecognized algorithms.
     * @exception SignatureException on signature handling errors.
     */
    public SignerInfo verify(final SignerInfo info, final byte[] bytes)
    throws NoSuchAlgorithmException, SignatureException {
        return info.verify(this, bytes);
    }

    /**
     * Returns all signerInfos which self-verify.
     *
     * @param bytes the DER encoded content information.
     *
     * @exception NoSuchAlgorithmException on unrecognized algorithms.
     * @exception SignatureException on signature handling errors.
     */
    public SignerInfo[] verify(final byte[] bytes)
    throws NoSuchAlgorithmException, SignatureException {

        final Vector<SignerInfo> intResult = new Vector<SignerInfo>();
        for (final SignerInfo signerInfo2 : this.signerInfos) {

            final SignerInfo signerInfo = verify(signerInfo2, bytes);
            if (signerInfo != null) {
                intResult.addElement(signerInfo);
            }
        }
        if (!intResult.isEmpty()) {

            final SignerInfo[] result = new SignerInfo[intResult.size()];
            intResult.copyInto(result);
            return result;
        }
        return null;
    }

    /**
     * Returns all signerInfos which self-verify.
     *
     * @exception NoSuchAlgorithmException on unrecognized algorithms.
     * @exception SignatureException on signature handling errors.
     */
    public SignerInfo[] verify()
    throws NoSuchAlgorithmException, SignatureException {
        return verify(null);
    }

    /**
     * Returns the version number of this PKCS7 block.
     * @return the version or null if version is not specified
     *         for the content type.
     */
    public  BigInteger getVersion() {
        return this.version;
    }

    /**
     * Returns the message digest algorithms specified in this PKCS7 block.
     * @return the array of Digest Algorithms or null if none are specified
     *         for the content type.
     */
    public AlgorithmId[] getDigestAlgorithmIds() {
        return  this.digestAlgorithmIds;
    }

    /**
     * Returns the content information specified in this PKCS7 block.
     */
    public ContentInfo getContentInfo() {
        return this.contentInfo;
    }

    /**
     * Returns the X.509 certificates listed in this PKCS7 block.
     * @return a clone of the array of X.509 certificates or null if
     *         none are specified for the content type.
     */
    public X509Certificate[] getCertificates() {
        if (this.certificates != null) {
			return this.certificates.clone();
		} else {
			return null;
		}
    }

    /**
     * Returns the X.509 crls listed in this PKCS7 block.
     * @return a clone of the array of X.509 crls or null if none
     *         are specified for the content type.
     */
    public X509CRL[] getCRLs() {
        if (this.crls != null) {
			return this.crls.clone();
		} else {
			return null;
		}
    }

    /**
     * Returns the signer's information specified in this PKCS7 block.
     * @return the array of Signer Infos or null if none are specified
     *         for the content type.
     */
    public SignerInfo[] getSignerInfos() {
        return this.signerInfos;
    }

    /**
     * Returns the X.509 certificate listed in this PKCS7 block
     * which has a matching serial number and Issuer name, or
     * null if one is not found.
     *
     * @param serial the serial number of the certificate to retrieve.
     * @param issuerName the Distinguished Name of the Issuer.
     */
    public X509Certificate getCertificate(final BigInteger serial, final X500Name issuerName) {
        if (this.certificates != null) {
            if (this.certIssuerNames == null) {
				populateCertIssuerNames();
			}
            for (int i = 0; i < this.certificates.length; i++) {
                final X509Certificate cert = this.certificates[i];
                final BigInteger thisSerial = cert.getSerialNumber();
                if (serial.equals(thisSerial)
                    && issuerName.equals(this.certIssuerNames[i]))
                {
                    return cert;
                }
            }
        }
        return null;
    }

    /**
     * Populate array of Issuer DNs from certificates and convert
     * each Principal to type X500Name if necessary.
     */
    private void populateCertIssuerNames() {
        if (this.certificates == null) {
			return;
		}

        this.certIssuerNames = new Principal[this.certificates.length];
        for (int i = 0; i < this.certificates.length; i++) {
            final X509Certificate cert = this.certificates[i];
            Principal certIssuerName = cert.getIssuerDN();
            if (!(certIssuerName instanceof X500Name)) {
                // must extract the original encoded form of DN for
                // subsequent name comparison checks (converting to a
                // String and back to an encoded DN could cause the
                // types of String attribute values to be changed)
                try {
                    final X509CertInfo tbsCert =
                        new X509CertInfo(cert.getTBSCertificate());
                    certIssuerName = (Principal)
                        tbsCert.get(X509CertInfo.ISSUER + "." +
                                    X509CertInfo.DN_NAME);
                } catch (final Exception e) {
                    // error generating X500Name object from the cert's
                    // issuer DN, leave name as is.
                }
            }
            this.certIssuerNames[i] = certIssuerName;
        }
    }

    /**
     * Returns the PKCS7 block in a printable string form.
     */
    @Override
	public String toString() {
        String out = "";

        out += this.contentInfo + "\n";
        if (this.version != null) {
			out += "PKCS7 :: version: " + Debug.toHexString(this.version) + "\n";
		}
        if (this.digestAlgorithmIds != null) {
            out += "PKCS7 :: digest AlgorithmIds: \n";
            for (final AlgorithmId digestAlgorithmId : this.digestAlgorithmIds) {
				out += "\t" + digestAlgorithmId + "\n";
			}
        }
        if (this.certificates != null) {
            out += "PKCS7 :: certificates: \n";
            for (int i = 0; i < this.certificates.length; i++) {
				out += "\t" + i + ".   " + this.certificates[i] + "\n";
			}
        }
        if (this.crls != null) {
            out += "PKCS7 :: crls: \n";
            for (int i = 0; i < this.crls.length; i++) {
				out += "\t" + i + ".   " + this.crls[i] + "\n";
			}
        }
        if (this.signerInfos != null) {
            out += "PKCS7 :: signer infos: \n";
            for (int i = 0; i < this.signerInfos.length; i++) {
				out += ("\t" + i + ".  " + this.signerInfos[i] + "\n");
			}
        }
        return out;
    }

    /**
     * Returns true if this is a JDK1.1.x-style PKCS#7 block, and false
     * otherwise.
     */
    public boolean isOldStyle() {
        return this.oldStyle;
    }

    /**
     * Assembles a PKCS #7 signed data message that optionally includes a
     * signature timestamp.
     *
     * @param signature the signature bytes
     * @param signerChain the signer's X.509 certificate chain
     * @param content the content that is signed; specify null to not include
     *        it in the PKCS7 data
     * @param signatureAlgorithm the name of the signature algorithm
     * @param tsaURI the URI of the Timestamping Authority; or null if no
     *         timestamp is requested
     * @param tSAPolicyID the TSAPolicyID of the Timestamping Authority as a
     *         numerical object identifier; or null if we leave the TSA server
     *         to choose one. This argument is only used when tsaURI is provided
     * @return the bytes of the encoded PKCS #7 signed data message
     * @throws NoSuchAlgorithmException The exception is thrown if the signature
     *         algorithm is unrecognised.
     * @throws CertificateException The exception is thrown if an error occurs
     *         while processing the signer's certificate or the TSA's
     *         certificate.
     * @throws IOException The exception is thrown if an error occurs while
     *         generating the signature timestamp or while generating the signed
     *         data message.
     */
    public static byte[] generateSignedData(final byte[] signature,
                                            final X509Certificate[] signerChain,
                                            final byte[] content,
                                            final String signatureAlgorithm,
                                            final URI tsaURI,
                                            final String tSAPolicyID)
        throws CertificateException, IOException, NoSuchAlgorithmException
    {

        // Generate the timestamp token
        PKCS9Attributes unauthAttrs = null;
        if (tsaURI != null) {
            // Timestamp the signature
            final HttpTimestamper tsa = new HttpTimestamper(tsaURI);
            final byte[] tsToken = generateTimestampToken(tsa, tSAPolicyID, signature);

            // Insert the timestamp token into the PKCS #7 signer info element
            // (as an unsigned attribute)
            unauthAttrs =
                new PKCS9Attributes(new PKCS9Attribute[]{
                    new PKCS9Attribute(
                        PKCS9Attribute.SIGNATURE_TIMESTAMP_TOKEN_STR,
                        tsToken)});
        }

        // Create the SignerInfo
        final X500Name issuerName =
            X500Name.asX500Name(signerChain[0].getIssuerX500Principal());
        final BigInteger serialNumber = signerChain[0].getSerialNumber();
        final String encAlg = AlgorithmId.getEncAlgFromSigAlg(signatureAlgorithm);
        final String digAlg = AlgorithmId.getDigAlgFromSigAlg(signatureAlgorithm);
        final SignerInfo signerInfo = new SignerInfo(issuerName, serialNumber,
                                               AlgorithmId.get(digAlg), null,
                                               AlgorithmId.get(encAlg),
                                               signature, unauthAttrs);

        // Create the PKCS #7 signed data message
        final SignerInfo[] signerInfos = {signerInfo};
        final AlgorithmId[] algorithms = {signerInfo.getDigestAlgorithmId()};
        // Include or exclude content
        final ContentInfo contentInfo = (content == null)
            ? new ContentInfo(ContentInfo.DATA_OID, null)
            : new ContentInfo(content);
        final PKCS7 pkcs7 = new PKCS7(algorithms, contentInfo,
                                signerChain, signerInfos);
        final ByteArrayOutputStream p7out = new ByteArrayOutputStream();
        pkcs7.encodeSignedData(p7out);

        return p7out.toByteArray();
    }

    /**
     * Requests, processes and validates a timestamp token from a TSA using
     * common defaults. Uses the following defaults in the timestamp request:
     * SHA-1 for the hash algorithm, a 64-bit nonce, and request certificate
     * set to true.
     *
     * @param tsa the timestamping authority to use
     * @param tSAPolicyID the TSAPolicyID of the Timestamping Authority as a
     *         numerical object identifier; or null if we leave the TSA server
     *         to choose one
     * @param toBeTimestamped the token that is to be timestamped
     * @return the encoded timestamp token
     * @throws IOException The exception is thrown if an error occurs while
     *                     communicating with the TSA, or a non-null
     *                     TSAPolicyID is specified in the request but it
     *                     does not match the one in the reply
     * @throws CertificateException The exception is thrown if the TSA's
     *                     certificate is not permitted for timestamping.
     */
    private static byte[] generateTimestampToken(final Timestamper tsa,
                                                 final String tSAPolicyID,
                                                 final byte[] toBeTimestamped)
        throws IOException, CertificateException
    {
        // Generate a timestamp
        MessageDigest messageDigest = null;
        TSRequest tsQuery = null;
        try {
            // SHA-1 is always used.
            messageDigest = MessageDigest.getInstance("SHA-1");
            tsQuery = new TSRequest(tSAPolicyID, toBeTimestamped, messageDigest);
        } catch (final NoSuchAlgorithmException e) {
            // ignore
        }

        // Generate a nonce
        BigInteger nonce = null;
        if (SecureRandomHolder.RANDOM != null) {
            nonce = new BigInteger(64, SecureRandomHolder.RANDOM);
            tsQuery.setNonce(nonce);
        }
        tsQuery.requestCertificate(true);

        final TSResponse tsReply = tsa.generateTimestamp(tsQuery);
        final int status = tsReply.getStatusCode();
        // Handle TSP error
        if (status != 0 && status != 1) {
            throw new IOException("Error generating timestamp: " +
                tsReply.getStatusCodeAsText() + " " +
                tsReply.getFailureCodeAsText());
        }

        if (tSAPolicyID != null &&
                !tSAPolicyID.equals(tsReply.getTimestampToken().getPolicyID())) {
            throw new IOException("TSAPolicyID changed in "
                    + "timestamp token");
        }
        final PKCS7 tsToken = tsReply.getToken();

        final TimestampToken tst = tsReply.getTimestampToken();
        if (!tst.getHashAlgorithm().getName().equals("SHA-1")) {
            throw new IOException("Digest algorithm not SHA-1 in "
                                  + "timestamp token");
        }
        if (!MessageDigest.isEqual(tst.getHashedMessage(),
                                   tsQuery.getHashedMessage())) {
            throw new IOException("Digest octets changed in timestamp token");
        }

        final BigInteger replyNonce = tst.getNonce();
        if (replyNonce == null && nonce != null) {
            throw new IOException("Nonce missing in timestamp token");
        }
        if (replyNonce != null && !replyNonce.equals(nonce)) {
            throw new IOException("Nonce changed in timestamp token");
        }

        // Examine the TSA's certificate (if present)
        for (final SignerInfo si: tsToken.getSignerInfos()) {
            final X509Certificate cert = si.getCertificate(tsToken);
            if (cert == null) {
                // Error, we've already set tsRequestCertificate = true
                throw new CertificateException(
                "Certificate not included in timestamp token");
            } else {
                if (!cert.getCriticalExtensionOIDs().contains(
                        EXTENDED_KEY_USAGE_OID)) {
                    throw new CertificateException(
                    "Certificate is not valid for timestamping");
                }
                final List<String> keyPurposes = cert.getExtendedKeyUsage();
                if (keyPurposes == null ||
                        !keyPurposes.contains(KP_TIMESTAMPING_OID)) {
                    throw new CertificateException(
                    "Certificate is not valid for timestamping");
                }
            }
        }
        return tsReply.getEncodedToken();
    }
}
