/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.signers.cades;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;

/** Clase que encapsula los RecipientInfos y el EncryptedContentInfo para un sobre digital. */
final class Info {

    private ASN1EncodableVector recipientInfos = null;
    private EncryptedContentInfo encInfo = null;

    /** Obtiene los RecipientInfo.
     * @return RecipientInfo
     */
    ASN1EncodableVector getRecipientInfos() {
        return this.recipientInfos;
    }

    /** Establece los RecipientInfo.
     * @param recipientInfos RecipientInfo
     */
    void setRecipientInfos(final ASN1EncodableVector recipientInfos) {
        this.recipientInfos = recipientInfos;
    }

    /** Obtiene el EncryptedContentInfo.
     * @return EncryptedContentInfo
     */
    EncryptedContentInfo getEncInfo() {
        return this.encInfo;
    }

    /** Establece el EncryptedContentInfo.
     * @param encInfo EncryptedContentInfo
     */
    void setEncInfo(final EncryptedContentInfo encInfo) {
        this.encInfo = encInfo;
    }
}
