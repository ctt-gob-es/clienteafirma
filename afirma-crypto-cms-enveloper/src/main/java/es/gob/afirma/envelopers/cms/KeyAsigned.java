/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.envelopers.cms;

import javax.crypto.SecretKey;

import es.gob.afirma.core.ciphers.AOCipherConfig;


/** Clase utilizada en los signers CMS para contener la configuracion y la clave
 * de cifrado */
final class KeyAsigned {

    /** Clave de cifrado. La almacenamos internamente porque no hay forma de
     * mostrarla directamente al usuario. */
    private SecretKey cipherKey;

    private AOCipherConfig config;

    SecretKey getCipherKey() {
        return this.cipherKey;
    }

    void setCipherKey(final SecretKey cipherKey) {
        this.cipherKey = cipherKey;
    }

    AOCipherConfig getConfig() {
        return this.config;
    }

    void setConfig(final AOCipherConfig config) {
        this.config = config;
    }
}
