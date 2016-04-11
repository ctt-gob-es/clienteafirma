/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.cades;

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.DERIA5String;
import org.spongycastle.asn1.DERSequence;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;

/** Tipo <i>AOSigPolicyQualifierInfo</i> de ASN.1 para CADES-EPES.
 *
 * <pre>
 *   AOSigPolicyQualifierInfo ::= SEQUENCE {
 *       sigPolicyQualifierId  sigPolicyQualifierId,
 *       sigQualifier          ANY DEFINED BY policyQualifierId }
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un Signature Policy qualifiers basandose en SpongyCastle de la clase
 * Policy qualifiers. */
final class AOSigPolicyQualifierInfo implements ASN1Encodable {

    private final ASN1ObjectIdentifier sigPolicyQualifierId;
    private final ASN1Encodable sigQualifier;

    /** Crea un nuevo <code>AOSigPolicyQualifierInfo</code> con su calificador
     * cPSuri.
     * @param cps
     *        El CPS (certification practice statement) uri como <code>String</code>. */
    AOSigPolicyQualifierInfo(final String cps) {
        this.sigPolicyQualifierId = PKCSObjectIdentifiers.id_spq_ets_uri;
        this.sigQualifier = new DERIA5String(cps);
    }

    /** Devuelve el identificador de la estancia.
     * @return El identificador. */
    ASN1ObjectIdentifier getSigPolicyQualifierId() {
        return this.sigPolicyQualifierId;
    }

    /** Devuelve el Calificador de la estancia.
     * @return el Calificador. */
    ASN1Encodable getSigQualifier() {
        return this.sigQualifier;
    }

    /** Devuelve una representaci&oacute;n DER-encodable the esta estancia.
     * @return un valor <code>DERObject</code>. */
    @Override
	public ASN1Primitive toASN1Primitive() {
        final ASN1EncodableVector dev = new ASN1EncodableVector();
        dev.add(this.sigPolicyQualifierId);
        dev.add(this.sigQualifier);
        return new DERSequence(dev);
    }
}
