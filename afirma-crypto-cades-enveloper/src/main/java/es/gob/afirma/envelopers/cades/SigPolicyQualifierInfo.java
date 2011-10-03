/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.envelopers.cades;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERIA5String;
import org.bouncycastle.asn1.DERObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

/** Signature Policy qualifiers, tipo especial para CADES-EPES
 *
 * <pre>
 *   SigPolicyQualifierInfo ::= SEQUENCE {
 *       SigPolicyQualifierId  SigPolicyQualifierId,
 *       SigQualifier          ANY DEFINED BY policyQualifierId }
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un Signature Policy qualifiers basandose en BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> de la clase
 * Policy qualifiers. */
final class SigPolicyQualifierInfo extends ASN1Encodable {

    private final DERObjectIdentifier SigPolicyQualifierId;
    private final DEREncodable SigQualifier;

    /** Crea un nuevo <code>SigPolicyQualifierInfo</code> con su calificador
     * cPSuri.
     * @param cps
     *        El CPS (certification practice statement) uri como <code>String</code>. */
    SigPolicyQualifierInfo(final String cps) {
        this.SigPolicyQualifierId = PKCSObjectIdentifiers.id_spq_ets_uri;
        this.SigQualifier = new DERIA5String(cps);
    }

    /** Devuelve el identificador de la estancia.
     * @return El identificador. */
    DERObjectIdentifier getSigPolicyQualifierId() {
        return this.SigPolicyQualifierId;
    }

    /** Devuelve el Cualificador de la estancia.
     * @return el Cualificador. */
    DEREncodable getSigQualifier() {
        return this.SigQualifier;
    }

    /** Devuelve una representaci&oacute;n DER-encodable the esta estancia.
     * @return un valor <code>DERObject</code>. */
    @Override
    public DERObject toASN1Object() {
        final ASN1EncodableVector dev = new ASN1EncodableVector();
        dev.add(this.SigPolicyQualifierId);
        dev.add(this.SigQualifier);
        return new DERSequence(dev);
    }
}
