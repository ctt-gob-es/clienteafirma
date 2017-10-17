/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.envelopers.cms;

import org.spongycastle.asn1.x509.AlgorithmIdentifier;

/** Clase utilizada en Utils en la funcion fetchEncryptedKeys */
final class EncryptedKeyDatas {

    private byte[] encryptedKey = null;
    private AlgorithmIdentifier algEncryptedKey = null;

    byte[] getEncryptedKey() {
        return this.encryptedKey;
    }

    void setEncryptedKey(final byte[] encryptedKey) {
        this.encryptedKey = encryptedKey.clone();
    }

    AlgorithmIdentifier getAlgEncryptedKey() {
        return this.algEncryptedKey;
    }

    void setAlgEncryptedKey(final AlgorithmIdentifier algEncryptedKey) {
        this.algEncryptedKey = algEncryptedKey;
    }
}
