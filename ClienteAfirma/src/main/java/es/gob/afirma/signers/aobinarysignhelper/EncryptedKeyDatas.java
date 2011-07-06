package es.gob.afirma.signers.aobinarysignhelper;

import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

/** Clase utilizada en Utils en la funcion fetchEncryptedKeys */
final class EncryptedKeyDatas {

    private byte[] encryptedKey = null;
    private AlgorithmIdentifier algEncryptedKey = null;

    public byte[] getEncryptedKey() {
        return encryptedKey;
    }

    public void setEncryptedKey(final byte[] encryptedKey) {
        this.encryptedKey = encryptedKey;
    }

    public AlgorithmIdentifier getAlgEncryptedKey() {
        return algEncryptedKey;
    }

    public void setAlgEncryptedKey(final AlgorithmIdentifier algEncryptedKey) {
        this.algEncryptedKey = algEncryptedKey;
    }
}
