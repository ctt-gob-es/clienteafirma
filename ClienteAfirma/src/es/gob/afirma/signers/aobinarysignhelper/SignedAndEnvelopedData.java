/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
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

/**
 * M&eacute;todo base para la generaci&oacute;n deun tipo Signed and Enveloped Data
 * del estandar pkcs#7 basado en la RFC-2315.
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para crear un
 * SignedAndEnvelopedData desarrollado en BouncyCastle: <a href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 */
public final class SignedAndEnvelopedData extends ASN1Encodable {

    private DERInteger              version;
    private ASN1Set                 recipientInfos;
    private ASN1Set                 digestAlgorithms;
    private EncryptedContentInfo    encryptedContentInfo;
    private ASN1Set                 certificates;
    private ASN1Set                 crls;
    private ASN1Set                 signerInfos;

    SignedAndEnvelopedData(
        ASN1Set                 recipientInfos,
        ASN1Set                 digestAlgorithms,
        EncryptedContentInfo    encryptedContentInfo,
        ASN1Set                 certificates,
        ASN1Set                 crls,
        ASN1Set                 signerInfos)
    {

        this.version = new DERInteger(1);//Always 1
        this.recipientInfos = recipientInfos;
        this.digestAlgorithms = digestAlgorithms;
        this.encryptedContentInfo = encryptedContentInfo;
        this.certificates = certificates;
        this.crls = crls;
        this.signerInfos = signerInfos;
    }

    SignedAndEnvelopedData(
        ASN1Sequence seq)
    {
        int     index = 0;
        this.version = (DERInteger)seq.getObjectAt(index++);
        this.recipientInfos = ASN1Set.getInstance(seq.getObjectAt(index++));
        this.digestAlgorithms = ASN1Set.getInstance(seq.getObjectAt(index++));
        this.encryptedContentInfo = EncryptedContentInfo.getInstance(seq.getObjectAt(index++));

        if (seq.size()>5){
            if(seq.size()==6){
                this.certificates=ASN1Set.getInstance((ASN1TaggedObject)seq.getObjectAt(index++),false);
            }
            else{
                this.certificates=ASN1Set.getInstance((ASN1TaggedObject)seq.getObjectAt(index++),false);
                this.crls=ASN1Set.getInstance((ASN1TaggedObject)seq.getObjectAt(index++),false);
            }

        }
        
        this.signerInfos = ASN1Set.getInstance(seq.getObjectAt(index++));
        
        
    }

    /**
     * return an SignedAndEnvelopedData object from a tagged object.
     *
     * @param obj the tagged object holding the object we want.
     * @param explicit true if the object is meant to be explicitly
     *              tagged false otherwise.
     * @exception IllegalArgumentException if the object held by the
     *          tagged object cannot be converted.
     */
    static SignedAndEnvelopedData getInstance(
        ASN1TaggedObject obj,
        boolean explicit)
    {
        return getInstance(ASN1Sequence.getInstance(obj, explicit));
    }

    /**
     * return an SignedAndEnvelopedData object from the given object.
     *
     * @param obj the object we want converted.
     * @exception IllegalArgumentException if the object cannot be converted.
     */
    static SignedAndEnvelopedData getInstance(
        Object obj)
    {
        if (obj == null || obj instanceof SignedAndEnvelopedData)
        {
            return (SignedAndEnvelopedData)obj;
        }

        if (obj instanceof ASN1Sequence)
        {
            return new SignedAndEnvelopedData((ASN1Sequence)obj);
        }

        throw new IllegalArgumentException("Invalid EnvelopedData: " + obj.getClass().getName());
    }

    DERInteger getVersion()
    {
        return version;
    }

    ASN1Set getRecipientInfos()
    {
        return recipientInfos;
    }

    ASN1Set getDigestAlgorithms()
    {
    	return digestAlgorithms;
    }
    
    EncryptedContentInfo getEncryptedContentInfo()
    {
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

    

    /**
     * Produce an object suitable for an ASN1OutputStream.
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
     * </pre>
     */
    @Override
	public DERObject toASN1Object() {
        ASN1EncodableVector  v = new ASN1EncodableVector();

        v.add(version);
        v.add(recipientInfos);
        v.add(digestAlgorithms);
        v.add(encryptedContentInfo);

        if (certificates != null)
        {
            v.add(new DERTaggedObject(false, 1, certificates));
        }
        if (crls != null)
        {
            v.add(new DERTaggedObject(false, 1, crls));
        }

        v.add(signerInfos);

        return new BERSequence(v);
    }
}
