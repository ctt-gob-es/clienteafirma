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

import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Principal;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.security.Timestamp;
import java.security.cert.CertPath;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;

import es.gob.afirma.standalone.configurator.jre.misc.HexDumpEncoder;
import es.gob.afirma.standalone.configurator.jre.security.timestamp.TimestampToken;
import es.gob.afirma.standalone.configurator.jre.security.util.Debug;
import es.gob.afirma.standalone.configurator.jre.security.util.DerEncoder;
import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;
import es.gob.afirma.standalone.configurator.jre.security.x509.AlgorithmId;
import es.gob.afirma.standalone.configurator.jre.security.x509.KeyUsageExtension;
import es.gob.afirma.standalone.configurator.jre.security.x509.X500Name;


/**
 * A SignerInfo, as defined in PKCS#7's signedData type.
 *
 * @author Benjamin Renaud
 */
public class SignerInfo implements DerEncoder {

    BigInteger version;
    X500Name issuerName;
    BigInteger certificateSerialNumber;
    AlgorithmId digestAlgorithmId;
    AlgorithmId digestEncryptionAlgorithmId;
    byte[] encryptedDigest;
    Timestamp timestamp;
    private boolean hasTimestamp = true;
    private static final Debug debug = Debug.getInstance("jar");

    PKCS9Attributes authenticatedAttributes;
    PKCS9Attributes unauthenticatedAttributes;

    public SignerInfo(final X500Name  issuerName,
                      final BigInteger serial,
                      final AlgorithmId digestAlgorithmId,
                      final AlgorithmId digestEncryptionAlgorithmId,
                      final byte[] encryptedDigest) {
        this.version = BigInteger.ONE;
        this.issuerName = issuerName;
        this.certificateSerialNumber = serial;
        this.digestAlgorithmId = digestAlgorithmId;
        this.digestEncryptionAlgorithmId = digestEncryptionAlgorithmId;
        this.encryptedDigest = encryptedDigest;
    }

    public SignerInfo(final X500Name  issuerName,
                      final BigInteger serial,
                      final AlgorithmId digestAlgorithmId,
                      final PKCS9Attributes authenticatedAttributes,
                      final AlgorithmId digestEncryptionAlgorithmId,
                      final byte[] encryptedDigest,
                      final PKCS9Attributes unauthenticatedAttributes) {
        this.version = BigInteger.ONE;
        this.issuerName = issuerName;
        this.certificateSerialNumber = serial;
        this.digestAlgorithmId = digestAlgorithmId;
        this.authenticatedAttributes = authenticatedAttributes;
        this.digestEncryptionAlgorithmId = digestEncryptionAlgorithmId;
        this.encryptedDigest = encryptedDigest;
        this.unauthenticatedAttributes = unauthenticatedAttributes;
    }

    /**
     * Parses a PKCS#7 signer info.
     */
    public SignerInfo(final DerInputStream derin)
        throws IOException, ParsingException
    {
        this(derin, false);
    }

    /**
     * Parses a PKCS#7 signer info.
     *
     * <p>This constructor is used only for backwards compatibility with
     * PKCS#7 blocks that were generated using JDK1.1.x.
     *
     * @param derin the ASN.1 encoding of the signer info.
     * @param oldStyle flag indicating whether or not the given signer info
     * is encoded according to JDK1.1.x.
     */
    public SignerInfo(final DerInputStream derin, final boolean oldStyle)
        throws IOException, ParsingException
    {
        // version
        this.version = derin.getBigInteger();

        // issuerAndSerialNumber
        final DerValue[] issuerAndSerialNumber = derin.getSequence(2);
        final byte[] issuerBytes = issuerAndSerialNumber[0].toByteArray();
        this.issuerName = new X500Name(new DerValue(DerValue.tag_Sequence,
                                               issuerBytes));
        this.certificateSerialNumber = issuerAndSerialNumber[1].getBigInteger();

        // digestAlgorithmId
        DerValue tmp = derin.getDerValue();

        this.digestAlgorithmId = AlgorithmId.parse(tmp);

        // authenticatedAttributes
        if (oldStyle) {
            // In JDK1.1.x, the authenticatedAttributes are always present,
            // encoded as an empty Set (Set of length zero)
            derin.getSet(0);
        } else {
            // check if set of auth attributes (implicit tag) is provided
            // (auth attributes are OPTIONAL)
            if ((byte)(derin.peekByte()) == (byte)0xA0) {
                this.authenticatedAttributes = new PKCS9Attributes(derin);
            }
        }

        // digestEncryptionAlgorithmId - little RSA naming scheme -
        // signature == encryption...
        tmp = derin.getDerValue();

        this.digestEncryptionAlgorithmId = AlgorithmId.parse(tmp);

        // encryptedDigest
        this.encryptedDigest = derin.getOctetString();

        // unauthenticatedAttributes
        if (oldStyle) {
            // In JDK1.1.x, the unauthenticatedAttributes are always present,
            // encoded as an empty Set (Set of length zero)
            derin.getSet(0);
        } else {
            // check if set of unauth attributes (implicit tag) is provided
            // (unauth attributes are OPTIONAL)
            if (derin.available() != 0
                && (byte)(derin.peekByte()) == (byte)0xA1) {
                this.unauthenticatedAttributes =
                    new PKCS9Attributes(derin, true);// ignore unsupported attrs
            }
        }

        // all done
        if (derin.available() != 0) {
            throw new ParsingException("extra data at the end");
        }
    }

    public void encode(final DerOutputStream out) throws IOException {

        derEncode(out);
    }

    /**
     * DER encode this object onto an output stream.
     * Implements the <code>DerEncoder</code> interface.
     *
     * @param out
     * the output stream on which to write the DER encoding.
     *
     * @exception IOException on encoding error.
     */
    @Override
	public void derEncode(final OutputStream out) throws IOException {
        final DerOutputStream seq = new DerOutputStream();
        seq.putInteger(this.version);
        final DerOutputStream issuerAndSerialNumber = new DerOutputStream();
        this.issuerName.encode(issuerAndSerialNumber);
        issuerAndSerialNumber.putInteger(this.certificateSerialNumber);
        seq.write(DerValue.tag_Sequence, issuerAndSerialNumber);

        this.digestAlgorithmId.encode(seq);

        // encode authenticated attributes if there are any
        if (this.authenticatedAttributes != null) {
			this.authenticatedAttributes.encode((byte)0xA0, seq);
		}

        this.digestEncryptionAlgorithmId.encode(seq);

        seq.putOctetString(this.encryptedDigest);

        // encode unauthenticated attributes if there are any
        if (this.unauthenticatedAttributes != null) {
			this.unauthenticatedAttributes.encode((byte)0xA1, seq);
		}

        final DerOutputStream tmp = new DerOutputStream();
        tmp.write(DerValue.tag_Sequence, seq);

        out.write(tmp.toByteArray());
    }



    /*
     * Returns the (user) certificate pertaining to this SignerInfo.
     */
    public X509Certificate getCertificate(final PKCS7 block)
        throws IOException
    {
        return block.getCertificate(this.certificateSerialNumber, this.issuerName);
    }

    /*
     * Returns the certificate chain pertaining to this SignerInfo.
     */
    public ArrayList<X509Certificate> getCertificateChain(final PKCS7 block)
        throws IOException
    {
        X509Certificate userCert;
        userCert = block.getCertificate(this.certificateSerialNumber, this.issuerName);
        if (userCert == null) {
			return null;
		}

        final ArrayList<X509Certificate> certList = new ArrayList<X509Certificate>();
        certList.add(userCert);

        final X509Certificate[] pkcsCerts = block.getCertificates();
        if (pkcsCerts == null
            || userCert.getSubjectDN().equals(userCert.getIssuerDN())) {
            return certList;
        }

        Principal issuer = userCert.getIssuerDN();
        int start = 0;
        while (true) {
            boolean match = false;
            int i = start;
            while (i < pkcsCerts.length) {
                if (issuer.equals(pkcsCerts[i].getSubjectDN())) {
                    // next cert in chain found
                    certList.add(pkcsCerts[i]);
                    // if selected cert is self-signed, we're done
                    // constructing the chain
                    if (pkcsCerts[i].getSubjectDN().equals(
                                            pkcsCerts[i].getIssuerDN())) {
                        start = pkcsCerts.length;
                    } else {
                        issuer = pkcsCerts[i].getIssuerDN();
                        final X509Certificate tmpCert = pkcsCerts[start];
                        pkcsCerts[start] = pkcsCerts[i];
                        pkcsCerts[i] = tmpCert;
                        start++;
                    }
                    match = true;
                    break;
                } else {
                    i++;
                }
            }
            if (!match) {
				break;
			}
        }

        return certList;
    }

    /* Returns null if verify fails, this signerInfo if
       verify succeeds. */
    SignerInfo verify(final PKCS7 block, byte[] data)
    throws NoSuchAlgorithmException, SignatureException {

        try {

            final ContentInfo content = block.getContentInfo();
            if (data == null) {
                data = content.getContentBytes();
            }

            final String digestAlgname = getDigestAlgorithmId().getName();

            byte[] dataSigned;

            // if there are authenticate attributes, get the message
            // digest and compare it with the digest of data
            if (this.authenticatedAttributes == null) {
                dataSigned = data;
            } else {

                // first, check content type
                final ObjectIdentifier contentType = (ObjectIdentifier)
                       this.authenticatedAttributes.getAttributeValue(
                         PKCS9Attribute.CONTENT_TYPE_OID);
                if (contentType == null ||
                    !contentType.equals((Object)content.contentType))
				 {
					return null;  // contentType does not match, bad SignerInfo
				}

                // now, check message digest
                final byte[] messageDigest = (byte[])
                    this.authenticatedAttributes.getAttributeValue(
                         PKCS9Attribute.MESSAGE_DIGEST_OID);

                if (messageDigest == null) {
					return null;
				}

                final MessageDigest md = MessageDigest.getInstance(digestAlgname);
                final byte[] computedMessageDigest = md.digest(data);

                if (messageDigest.length != computedMessageDigest.length) {
					return null;
				}
                for (int i = 0; i < messageDigest.length; i++) {
                    if (messageDigest[i] != computedMessageDigest[i]) {
						return null;
					}
                }

                // message digest attribute matched
                // digest of original data

                // the data actually signed is the DER encoding of
                // the authenticated attributes (tagged with
                // the "SET OF" tag, not 0xA0).
                dataSigned = this.authenticatedAttributes.getDerEncoding();
            }

            // put together digest algorithm and encryption algorithm
            // to form signing algorithm
            String encryptionAlgname =
                getDigestEncryptionAlgorithmId().getName();

            // Workaround: sometimes the encryptionAlgname is actually
            // a signature name
            final String tmp = AlgorithmId.getEncAlgFromSigAlg(encryptionAlgname);
            if (tmp != null) {
				encryptionAlgname = tmp;
			}
            final String algname = AlgorithmId.makeSigAlg(
                    digestAlgname, encryptionAlgname);

            final Signature sig = Signature.getInstance(algname);
            final X509Certificate cert = getCertificate(block);

            if (cert == null) {
                return null;
            }
            if (cert.hasUnsupportedCriticalExtension()) {
                throw new SignatureException("Certificate has unsupported "
                                             + "critical extension(s)");
            }

            // Make sure that if the usage of the key in the certificate is
            // restricted, it can be used for digital signatures.
            // XXX We may want to check for additional extensions in the
            // future.
            final boolean[] keyUsageBits = cert.getKeyUsage();
            if (keyUsageBits != null) {
                KeyUsageExtension keyUsage;
                try {
                    // We don't care whether or not this extension was marked
                    // critical in the certificate.
                    // We're interested only in its value (i.e., the bits set)
                    // and treat the extension as critical.
                    keyUsage = new KeyUsageExtension(keyUsageBits);
                } catch (final IOException ioe) {
                    throw new SignatureException("Failed to parse keyUsage "
                                                 + "extension");
                }

                final boolean digSigAllowed = keyUsage.get(
                        KeyUsageExtension.DIGITAL_SIGNATURE).booleanValue();

                final boolean nonRepuAllowed = keyUsage.get(
                        KeyUsageExtension.NON_REPUDIATION).booleanValue();

                if (!digSigAllowed && !nonRepuAllowed) {
                    throw new SignatureException("Key usage restricted: "
                                                 + "cannot be used for "
                                                 + "digital signatures");
                }
            }

            final PublicKey key = cert.getPublicKey();
            sig.initVerify(key);

            sig.update(dataSigned);

            if (sig.verify(this.encryptedDigest)) {
                return this;
            }

        } catch (final IOException e) {
            throw new SignatureException("IO error verifying signature:\n" +
                                         e.getMessage());

        } catch (final InvalidKeyException e) {
            throw new SignatureException("InvalidKey: " + e.getMessage());

        }
        return null;
    }

    /* Verify the content of the pkcs7 block. */
    SignerInfo verify(final PKCS7 block)
    throws NoSuchAlgorithmException, SignatureException {
        return verify(block, null);
    }


    public BigInteger getVersion() {
            return this.version;
    }

    public X500Name getIssuerName() {
        return this.issuerName;
    }

    public BigInteger getCertificateSerialNumber() {
        return this.certificateSerialNumber;
    }

    public AlgorithmId getDigestAlgorithmId() {
        return this.digestAlgorithmId;
    }

    public PKCS9Attributes getAuthenticatedAttributes() {
        return this.authenticatedAttributes;
    }

    public AlgorithmId getDigestEncryptionAlgorithmId() {
        return this.digestEncryptionAlgorithmId;
    }

    public byte[] getEncryptedDigest() {
        return this.encryptedDigest;
    }

    public PKCS9Attributes getUnauthenticatedAttributes() {
        return this.unauthenticatedAttributes;
    }

    /*
     * Extracts a timestamp from a PKCS7 SignerInfo.
     *
     * Examines the signer's unsigned attributes for a
     * <tt>signatureTimestampToken</tt> attribute. If present,
     * then it is parsed to extract the date and time at which the
     * timestamp was generated.
     *
     * @param info A signer information element of a PKCS 7 block.
     *
     * @return A timestamp token or null if none is present.
     * @throws IOException if an error is encountered while parsing the
     *         PKCS7 data.
     * @throws NoSuchAlgorithmException if an error is encountered while
     *         verifying the PKCS7 object.
     * @throws SignatureException if an error is encountered while
     *         verifying the PKCS7 object.
     * @throws CertificateException if an error is encountered while generating
     *         the TSA's certpath.
     */
    public Timestamp getTimestamp()
        throws IOException, NoSuchAlgorithmException, SignatureException,
               CertificateException
    {
        if (this.timestamp != null || !this.hasTimestamp) {
			return this.timestamp;
		}

        if (this.unauthenticatedAttributes == null) {
            this.hasTimestamp = false;
            return null;
        }
        final PKCS9Attribute tsTokenAttr =
            this.unauthenticatedAttributes.getAttribute(
                PKCS9Attribute.SIGNATURE_TIMESTAMP_TOKEN_OID);
        if (tsTokenAttr == null) {
            this.hasTimestamp = false;
            return null;
        }

        final PKCS7 tsToken = new PKCS7((byte[])tsTokenAttr.getValue());
        // Extract the content (an encoded timestamp token info)
        final byte[] encTsTokenInfo = tsToken.getContentInfo().getData();
        // Extract the signer (the Timestamping Authority)
        // while verifying the content
        final SignerInfo[] tsa = tsToken.verify(encTsTokenInfo);
        // Expect only one signer
        final ArrayList<X509Certificate> chain = tsa[0].getCertificateChain(tsToken);
        final CertificateFactory cf = CertificateFactory.getInstance("X.509");
        final CertPath tsaChain = cf.generateCertPath(chain);
        // Create a timestamp token info object
        final TimestampToken tsTokenInfo = new TimestampToken(encTsTokenInfo);
        // Check that the signature timestamp applies to this signature
        verifyTimestamp(tsTokenInfo);
        // Create a timestamp object
        this.timestamp = new Timestamp(tsTokenInfo.getDate(), tsaChain);
        return this.timestamp;
    }

    /*
     * Check that the signature timestamp applies to this signature.
     * Match the hash present in the signature timestamp token against the hash
     * of this signature.
     */
    private void verifyTimestamp(final TimestampToken token)
        throws NoSuchAlgorithmException, SignatureException {

        final MessageDigest md =
            MessageDigest.getInstance(token.getHashAlgorithm().getName());

        if (!Arrays.equals(token.getHashedMessage(),
            md.digest(this.encryptedDigest))) {

            throw new SignatureException("Signature timestamp (#" +
                token.getSerialNumber() + ") generated on " + token.getDate() +
                " is inapplicable");
        }

        if (debug != null) {
            debug.println();
            debug.println("Detected signature timestamp (#" +
                token.getSerialNumber() + ") generated on " + token.getDate());
            debug.println();
        }
    }

    @Override
	public String toString() {
        final HexDumpEncoder hexDump = new HexDumpEncoder();

        String out = "";

        out += "Signer Info for (issuer): " + this.issuerName + "\n";
        out += "\tversion: " + Debug.toHexString(this.version) + "\n";
        out += "\tcertificateSerialNumber: " +
               Debug.toHexString(this.certificateSerialNumber) + "\n";
        out += "\tdigestAlgorithmId: " + this.digestAlgorithmId + "\n";
        if (this.authenticatedAttributes != null) {
            out += "\tauthenticatedAttributes: " + this.authenticatedAttributes +
                   "\n";
        }
        out += "\tdigestEncryptionAlgorithmId: " + this.digestEncryptionAlgorithmId +
            "\n";

        out += "\tencryptedDigest: " + "\n" +
            hexDump.encodeBuffer(this.encryptedDigest) + "\n";
        if (this.unauthenticatedAttributes != null) {
            out += "\tunauthenticatedAttributes: " +
                   this.unauthenticatedAttributes + "\n";
        }
        return out;
    }
}
