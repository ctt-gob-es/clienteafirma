/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

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
