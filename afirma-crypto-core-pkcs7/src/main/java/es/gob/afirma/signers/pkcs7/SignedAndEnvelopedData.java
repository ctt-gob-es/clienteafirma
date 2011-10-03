/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.pkcs7;

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

    /** Crea un objecto CMS SignedAndEnvelopedData.
     * @param recipientInfos RecipientInfo
     * @param digestAlgorithms ALgoritmos de huella digital
     * @param encryptedContentInfo EncryptedContentInfo
     * @param certificates Certificados
     * @param crls Listas de revoaci&oacute;n de certificados
     * @param signerInfos SignerInfo
     */
    public SignedAndEnvelopedData(final ASN1Set recipientInfos,
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

    /** Crea un objecto CMS SignedAndEnvelopedData a partir de una Secuencia ASN.1.
     * @param seq Secuencia ASN.1 origen
     */
    public SignedAndEnvelopedData(final ASN1Sequence seq) {
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

    /** Obtiene un SignedAndEnvelopedData a partir de un objeto dado (que puede ser una secuencia ASN.1 o un mismo SignedAndEnvelopedData)
     * @param obj
     *        El objeto que queremos convertir en un SignedAndEnvelopedData
     * @return SignedAndEnvelopedData creado a partir del objeto proporcionado
     * @exception IllegalArgumentException
     *            si el objeto de entrada no puede ser convertido */
    public static SignedAndEnvelopedData getInstance(final Object obj) {
        if (obj == null || obj instanceof SignedAndEnvelopedData) {
            return (SignedAndEnvelopedData) obj;
        }

        if (obj instanceof ASN1Sequence) {
            return new SignedAndEnvelopedData((ASN1Sequence) obj);
        }

        throw new IllegalArgumentException("EnvelopedData invalido: " + obj.getClass().getName()); //$NON-NLS-1$
    }

    /** Obtiene la versi&oacute;n de la especificaci&oacute;n usada. 
     * @return La versi&oacute;n es siempre <code>1</code>*/
    public DERInteger getVersion() {
        return this.version;
    }

    /** Obtiene los RecipientInfo en forma de Set ASN.1.
     * @return RecipientInfos
     */    
    public ASN1Set getRecipientInfos() {
        return this.recipientInfos;
    }

    /** Obtiene los algoritmos de huella digital en forma de Set ASN.1.
     * @return Algoritmos de huella digital
     */
    public ASN1Set getDigestAlgorithms() {
        return this.digestAlgorithms;
    }

    /** Obtiene el EncryptedContentInfo.
     * @return EncryptedContentInfo
     */       
    public EncryptedContentInfo getEncryptedContentInfo() {
        return this.encryptedContentInfo;
    }

    /** Obtiene los Certificados en forma de Set ASN.1.
     * @return Certificados
     */
    public ASN1Set getCertificates() {
        return this.certificates;
    }

    /** Obtiene los SignerInfo en forma de Set ASN.1.
     * @return SignerInfos
     */
    public ASN1Set getSignerInfos() {
        return this.signerInfos;
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

        v.add(this.version);
        v.add(this.recipientInfos);
        v.add(this.digestAlgorithms);
        v.add(this.encryptedContentInfo);

        if (this.certificates != null) {
            v.add(new DERTaggedObject(false, 1, this.certificates));
        }
        if (this.crls != null) {
            v.add(new DERTaggedObject(false, 1, this.crls));
        }

        v.add(this.signerInfos);

        return new BERSequence(v);
    }
}
