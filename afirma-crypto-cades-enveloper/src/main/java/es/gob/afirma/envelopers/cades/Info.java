/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */
package es.gob.afirma.envelopers.cades;

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
