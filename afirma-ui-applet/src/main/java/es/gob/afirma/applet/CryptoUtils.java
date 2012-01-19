package es.gob.afirma.applet;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;

public final class CryptoUtils {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
    public static byte[] getMessageDigest(final byte[] data, final String algorithm) throws NoSuchAlgorithmException {
    	return MessageDigest.getInstance(algorithm).digest(data);
    }
}
