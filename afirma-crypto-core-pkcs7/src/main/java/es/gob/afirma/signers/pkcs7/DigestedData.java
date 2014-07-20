/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.pkcs7;

import java.util.Enumeration;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Integer;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.BERSequence;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

/** Clase base para la implementaci&oacute;n del tipo DigestedData. La Estructura
 * del mensaje es la siguiente:<br>
 *
 * <pre>
 * <code>
 *  DigestedData ::= SEQUENCE {
 *        version CMSVersion,
 *        digestAlgorithm DigestAlgorithmIdentifier,
 *        encapContentInfo EncapsulatedContentInfo,
 *        digest Digest }
 *
 *  Digest ::= OCTET STRING
 * </code>
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje DigestedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */
public final class DigestedData implements ASN1Encodable {
    private final ASN1Integer version;
    private final AlgorithmIdentifier digestAlgorithm;
    private final ContentInfo contentInfo;
    private final ASN1OctetString digest;

    static DigestedData getInstance(final Object o) {
        if (o instanceof DigestedData) {
            return (DigestedData) o;
        }
        else if (o instanceof ASN1Sequence) {
            return new DigestedData((ASN1Sequence) o);
        }

        throw new IllegalArgumentException("Objeto desconocido: " + o.getClass().getName()); //$NON-NLS-1$
    }

    /** Crea un objeto CMS DigestedData.
     * @param digestAlgo ALgoritmo de huella digital
     * @param contentInfo ContentInfo
     * @param digest Valor de la huella digital
     */
    public DigestedData(final AlgorithmIdentifier digestAlgo, final ContentInfo contentInfo, final ASN1OctetString digest) {
        this.version = new ASN1Integer(0);
        this.digestAlgorithm = digestAlgo;
        this.contentInfo = contentInfo;
        this.digest = digest;
    }

    /** Crea un object CMS DigestedData a partir de una Secuencia ASN.1.
     * @param seq Secuencia origen
     */
    public DigestedData(final ASN1Sequence seq) {
        final Enumeration<?> e = seq.getObjects();

        this.version = (ASN1Integer) e.nextElement();
        this.digestAlgorithm = AlgorithmIdentifier.getInstance(e.nextElement());
        this.contentInfo = ContentInfo.getInstance(e.nextElement());
        this.digest = (ASN1OctetString) e.nextElement();

    }

    /**
     * Recupera la versi&oacute;n del sistema de empaquetado PKCS#7.
     * @return Versi&oacute;n del empaquetado.
     */
    public String getVersion() {
        return this.version.toString();
    }

    /**
     * Recupera el algoritmo de huella digital configurada para los empaquetados.
     * @return Algoritmo.
     */
    public String getDigestAlgorithm() {
        return this.digestAlgorithm.getAlgorithm().toString();
    }

    ASN1OctetString getDigest() {
        return this.digest;
    }

    /**
     * Recupera el tipo de contenido.
     * @return Tipo de contenido.
     */
    public String getContentType() {
        return this.contentInfo.getContentType().toString();
    }

    /** Produce an object suitable for an ASN1OutputStream.
     *
     * <pre>
     * DigestedData ::= SEQUENCE {
     *     version CMSVersion,
     *     digestAlgorithms DigestAlgorithmIdentifiers,
     *     encapContentInfo EncapsulatedContentInfo,
     *     digest  Digest
     *   }
     *
     * Digest ::= OCTET STRING
     * </pre> */
    @Override
	public ASN1Primitive toASN1Primitive() {
        final ASN1EncodableVector v = new ASN1EncodableVector();

        v.add(this.version);
        v.add(this.digestAlgorithm);
        v.add(this.contentInfo);
        v.add(this.digest);

        return new BERSequence(v);
    }
}
