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

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERIA5String;
import org.bouncycastle.asn1.DERObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

/** Tipo <i>SigPolicyQualifierInfo</i> de ASN.1 para CADES-EPES.
 *
 * <pre>
 *   SigPolicyQualifierInfo ::= SEQUENCE {
 *       sigPolicyQualifierId  sigPolicyQualifierId,
 *       sigQualifier          ANY DEFINED BY policyQualifierId }
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un Signature Policy qualifiers basandose en BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> de la clase
 * Policy qualifiers. */
final class SigPolicyQualifierInfo extends ASN1Encodable {

    private final DERObjectIdentifier sigPolicyQualifierId;
    private final DEREncodable sigQualifier;

    /** Crea un nuevo <code>SigPolicyQualifierInfo</code> con su calificador
     * cPSuri.
     * @param cps
     *        El CPS (certification practice statement) uri como <code>String</code>. */
    SigPolicyQualifierInfo(final String cps) {
        this.sigPolicyQualifierId = PKCSObjectIdentifiers.id_spq_ets_uri;
        this.sigQualifier = new DERIA5String(cps);
    }

    /** Devuelve el identificador de la estancia.
     * @return El identificador. */
    DERObjectIdentifier getSigPolicyQualifierId() {
        return this.sigPolicyQualifierId;
    }

    /** Devuelve el Cualificador de la estancia.
     * @return el Cualificador. */
    DEREncodable getSigQualifier() {
        return this.sigQualifier;
    }

    /** Devuelve una representaci&oacute;n DER-encodable the esta estancia.
     * @return un valor <code>DERObject</code>. */
    @Override
    public DERObject toASN1Object() {
        final ASN1EncodableVector dev = new ASN1EncodableVector();
        dev.add(this.sigPolicyQualifierId);
        dev.add(this.sigQualifier);
        return new DERSequence(dev);
    }
}
