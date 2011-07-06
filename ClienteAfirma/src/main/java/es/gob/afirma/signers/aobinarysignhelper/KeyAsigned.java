package es.gob.afirma.signers.aobinarysignhelper;

import javax.crypto.SecretKey;

import es.gob.afirma.ciphers.AOCipherConfig;

/** Clase utilizada en los signers CMS para contener la configuracion y la clave
 * de cifrado */
final class KeyAsigned {

    /** Clave de cifrado. La almacenamos internamente porque no hay forma de
     * mostrarla directamente al usuario. */
    private SecretKey cipherKey;

    private AOCipherConfig config;

    public SecretKey getCipherKey() {
        return cipherKey;
    }

    public void setCipherKey(final SecretKey cipherKey) {
        this.cipherKey = cipherKey;
    }

    public AOCipherConfig getConfig() {
        return config;
    }

    public void setConfig(final AOCipherConfig config) {
        this.config = config;
    }
}
