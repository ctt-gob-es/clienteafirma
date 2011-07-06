/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Sequence;
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

    /** Creates a new <code>SigPolicyQualifierInfo</code> instance.
     * @param SigPolicyQualifierId
     *        Valor <code>PolicyQualifierId</code>
     * @param SigQualifier
     *        Es el calificador, definido en el campo de arriba. */
    public SigPolicyQualifierInfo(final DERObjectIdentifier SigPolicyQualifierId, final DEREncodable SigQualifier) {
        this.SigPolicyQualifierId = SigPolicyQualifierId;
        this.SigQualifier = SigQualifier;
    }

    /** Crea un nuevo <code>SigPolicyQualifierInfo</code> con su calificador
     * cPSuri.
     * @param cps
     *        El CPS (certification practice statement) uri como <code>String</code>. */
    public SigPolicyQualifierInfo(final String cps) {
        SigPolicyQualifierId = PKCSObjectIdentifiers.id_spq_ets_uri;
        SigQualifier = new DERIA5String(cps);
    }

    /** Crea una nueva estancia de <code>SigPolicyQualifierInfo</code> .
     * @param as
     *        Estructura X509 codificada como ASN1Sequence. */
    public SigPolicyQualifierInfo(final ASN1Sequence as) {
        if (as.size() != 2) {
            throw new IllegalArgumentException("Bad sequence size: " + as.size());
        }

        SigPolicyQualifierId = DERObjectIdentifier.getInstance(as.getObjectAt(0));
        SigQualifier = as.getObjectAt(1);
    }

    /** Devuelve la estancia a partir del objeto pasado por par&aacute;metro
     * @param as
     *        Objeto a estanciar.
     * @return Estancia a partir del objeto pasado por par&aacute;metro. */
    public static SigPolicyQualifierInfo getInstance(final Object as) {
        if (as instanceof SigPolicyQualifierInfo) {
            return (SigPolicyQualifierInfo) as;
        }
        else if (as instanceof ASN1Sequence) {
            return new SigPolicyQualifierInfo((ASN1Sequence) as);
        }
        throw new IllegalArgumentException("unknown object in getInstance.");
    }

    /** Devuelve el identificador de la estancia.
     * @return El identificador. */
    public DERObjectIdentifier getSigPolicyQualifierId() {
        return SigPolicyQualifierId;
    }

    /** Devuelve el Cualificador de la estancia.
     * @return el Cualificador. */
    public DEREncodable getSigQualifier() {
        return SigQualifier;
    }

    /** Devuelve una representaci&oacute;n DER-encodable the esta estancia.
     * @return un valor <code>DERObject</code>. */
    @Override
    public DERObject toASN1Object() {
        final ASN1EncodableVector dev = new ASN1EncodableVector();
        dev.add(SigPolicyQualifierId);
        dev.add(SigQualifier);
        return new DERSequence(dev);
    }
}
