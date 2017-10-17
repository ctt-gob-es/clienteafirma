/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pkcs7;

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1Integer;
import org.spongycastle.asn1.ASN1Object;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.ASN1TaggedObject;
import org.spongycastle.asn1.BERSequence;
import org.spongycastle.asn1.DERTaggedObject;
import org.spongycastle.asn1.cms.EncryptedContentInfo;

/** M&eacute;todo base para la generaci&oacute;n deun tipo Signed and Enveloped
 * Data del estandar pkcs#7 basado en la RFC-2315.
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un SignedAndEnvelopedData desarrollado en SpongyCastle.
 * <pre>
 *  SignedAndEnvelopedData ::= SEQUENCE {
 *     version                 Version,
 *     recipientInfos          RecipientInfos,
 *     digestAlgorithms        DigestAlgorithmIdentifiers,
 *     encryptedContentInfo    EncryptedContentInfo,
 *     certificates            [0] IMPLICIT Certificates OPTIONAL,
 *     crls                    [1] IMPLICIT CertificateRevocationLists OPTIONAL,
 *     signerInfos             SignerInfos }
 *
 *  RecipientInfos ::= SET OF RecipientInfo
 *
 *  DigestAlgorithmIdentifiers ::= CHOICE {
 *     daSet SET OF DigestAlgorithmIdentifier,
 *     daSequence SEQUENCE OF DigestAlgorithmIdentifier }
 *
 *  EncryptedContentInfo ::= SEQUENCE {
 *    contentType ContentType,
 *    contentEncryptionAlgorithm ContentEncryptionAlgorithmIdentifier,
 *    encryptedContent [0] IMPLICIT EncryptedContent OPTIONAL }
 *
 *  </pre> */
public final class SignedAndEnvelopedData extends ASN1Object {

    private final ASN1Integer version;
    private final ASN1Set recipientInfos;

    /** <code>DigestAlgorithmIdentifiers</code>, que puede ser tanto un <code>SET</code> como una <code>SEQUENCE</code>
     * ASN.1 de estructuras <code>DigestAlgorithmIdentifier</code>. */
    private ASN1Primitive digestAlgorithms;

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
                                  final ASN1Primitive digestAlgorithms,
                                  final EncryptedContentInfo encryptedContentInfo,
                                  final ASN1Set certificates,
                                  final ASN1Set crls,
                                  final ASN1Set signerInfos) {

        this.version = new ASN1Integer(1); // Siempre 1
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
        this.version = (ASN1Integer) seq.getObjectAt(index++);
        this.recipientInfos = ASN1Set.getInstance(seq.getObjectAt(index++));

        // Los DigestAlgorithmIdentifiers pueden ser SET o SEQUENCE, probamos ambos
        final ASN1Encodable dai = seq.getObjectAt(index++);
        try {
        	this.digestAlgorithms = ASN1Set.getInstance(dai);
        }
        catch(final IllegalArgumentException e) {
        	this.digestAlgorithms = ASN1Sequence.getInstance(dai);
        }

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
    public ASN1Integer getVersion() {
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
    public ASN1Primitive getDigestAlgorithms() {
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
	public ASN1Primitive toASN1Primitive() {
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
