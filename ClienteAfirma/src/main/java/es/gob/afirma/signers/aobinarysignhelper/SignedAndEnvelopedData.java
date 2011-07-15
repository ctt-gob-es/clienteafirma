/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.BERSequence;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DERObject;
import org.bouncycastle.asn1.DERTaggedObject;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;

/** M&eacute;todo base para la generaci&oacute;n deun tipo Signed and Enveloped
 * Data del estandar pkcs#7 basado en la RFC-2315.
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un SignedAndEnvelopedData desarrollado en BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */
public final class SignedAndEnvelopedData extends ASN1Encodable {

    private final DERInteger version;
    private final ASN1Set recipientInfos;
    private final ASN1Set digestAlgorithms;
    private final EncryptedContentInfo encryptedContentInfo;
    private ASN1Set certificates;
    private ASN1Set crls;
    private final ASN1Set signerInfos;

    SignedAndEnvelopedData(final ASN1Set recipientInfos,
                           final ASN1Set digestAlgorithms,
                           final EncryptedContentInfo encryptedContentInfo,
                           final ASN1Set certificates,
                           final ASN1Set crls,
                           final ASN1Set signerInfos) {

        this.version = new DERInteger(1);// Always 1
        this.recipientInfos = recipientInfos;
        this.digestAlgorithms = digestAlgorithms;
        this.encryptedContentInfo = encryptedContentInfo;
        this.certificates = certificates;
        this.crls = crls;
        this.signerInfos = signerInfos;
    }

    SignedAndEnvelopedData(final ASN1Sequence seq) {
        int index = 0;
        this.version = (DERInteger) seq.getObjectAt(index++);
        this.recipientInfos = ASN1Set.getInstance(seq.getObjectAt(index++));
        this.digestAlgorithms = ASN1Set.getInstance(seq.getObjectAt(index++));
        this.encryptedContentInfo = EncryptedContentInfo.getInstance(seq.getObjectAt(index++));

        if (seq.size() > 5) {
            if (seq.size() == 6) {
                this.certificates = ASN1Set.getInstance((ASN1TaggedObject) seq.getObjectAt(index++), false);
            }
            else {
                this.certificates = ASN1Set.getInstance((ASN1TaggedObject) seq.getObjectAt(index++), false);
                this.crls = ASN1Set.getInstance((ASN1TaggedObject) seq.getObjectAt(index++), false);
            }

        }

        this.signerInfos = ASN1Set.getInstance(seq.getObjectAt(index++));

    }

    /** return an SignedAndEnvelopedData object from a tagged object.
     * @param obj
     *        the tagged object holding the object we want.
     * @param explicit
     *        true if the object is meant to be explicitly tagged false
     *        otherwise.
     * @exception IllegalArgumentException
     *            if the object held by the tagged object cannot be
     *            converted. */
    static SignedAndEnvelopedData getInstance(final ASN1TaggedObject obj, final boolean explicit) {
        return getInstance(ASN1Sequence.getInstance(obj, explicit));
    }

    /** return an SignedAndEnvelopedData object from the given object.
     * @param obj
     *        the object we want converted.
     * @exception IllegalArgumentException
     *            if the object cannot be converted. */
    static SignedAndEnvelopedData getInstance(final Object obj) {
        if (obj == null || obj instanceof SignedAndEnvelopedData) {
            return (SignedAndEnvelopedData) obj;
        }

        if (obj instanceof ASN1Sequence) {
            return new SignedAndEnvelopedData((ASN1Sequence) obj);
        }

        throw new IllegalArgumentException("Invalid EnvelopedData: " + obj.getClass().getName());
    }

    DERInteger getVersion() {
        return version;
    }

    ASN1Set getRecipientInfos() {
        return recipientInfos;
    }

    ASN1Set getDigestAlgorithms() {
        return digestAlgorithms;
    }

    EncryptedContentInfo getEncryptedContentInfo() {
        return encryptedContentInfo;
    }

    ASN1Set getCertificates() {
        return certificates;
    }

    ASN1Set getCrls() {
        return crls;
    }

    ASN1Set getSignerInfos() {
        return signerInfos;
    }

    /** Produce an object suitable for an ASN1OutputStream.
     *
     * <pre>
     *    SignedAndEnvelopedData ::= SEQUENCE {
     *    version Version,
     *    recipientInfos RecipientInfos,
     *    digestAlgorithms DigestAlgorithmIdentifiers,
     *    encryptedContentInfo EncryptedContentInfo,
     *    certificates
     *       [0] IMPLICIT ExtendedCertificatesAndCertificates
     *         OPTIONAL,
     *    crls
     *      [1] IMPLICIT CertificateRevocationLists OPTIONAL,
     *    signerInfos SignerInfos }
     *
     * </pre> */
    @Override
    public DERObject toASN1Object() {
        final ASN1EncodableVector v = new ASN1EncodableVector();

        v.add(version);
        v.add(recipientInfos);
        v.add(digestAlgorithms);
        v.add(encryptedContentInfo);

        if (certificates != null) {
            v.add(new DERTaggedObject(false, 1, certificates));
        }
        if (crls != null) {
            v.add(new DERTaggedObject(false, 1, crls));
        }

        v.add(signerInfos);

        return new BERSequence(v);
    }
}
