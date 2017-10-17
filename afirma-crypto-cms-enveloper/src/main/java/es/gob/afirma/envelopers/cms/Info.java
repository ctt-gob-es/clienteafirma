/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.envelopers.cms;

import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.cms.EncryptedContentInfo;

/** Clase utilizada desde la clase Utils.java para manejar las variables de la
 * funcion initVariables */
final class Info {

    private ASN1EncodableVector recipientInfos = null;
    private EncryptedContentInfo encInfo = null;

    ASN1EncodableVector getRecipientInfos() {
        return this.recipientInfos;
    }

    void setRecipientInfos(final ASN1EncodableVector recipientInfos) {
        this.recipientInfos = recipientInfos;
    }

    EncryptedContentInfo getEncInfo() {
        return this.encInfo;
    }

    void setEncInfo(final EncryptedContentInfo encInfo) {
        this.encInfo = encInfo;
    }
}
