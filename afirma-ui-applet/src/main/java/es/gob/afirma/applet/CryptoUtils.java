package es.gob.afirma.applet;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/** Utilidades criptogr&aacute;ficas. */
final class CryptoUtils {

    static byte[] getMessageDigest(final byte[] data, final String algorithm) throws NoSuchAlgorithmException {
    	return MessageDigest.getInstance(algorithm).digest(data);
    }
}
