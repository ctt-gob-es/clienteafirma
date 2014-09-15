package es.gob.afirma.crypto.handwritten;

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.interfaces.RSAPublicKey;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;

final class RsaCipher {

	static byte[] encode(final RSAPublicKey key, final byte[] data) throws IllegalBlockSizeException,
	                                                                       BadPaddingException,
	                                                                       NoSuchAlgorithmException,
	                                                                       NoSuchPaddingException,
	                                                                       InvalidKeyException {
		if (data == null || data.length < 1) {
			throw new IllegalArgumentException(
				"Los datos a cifrar no pueden ser nulos ni vacios" //$NON-NLS-1$
			);
		}
		final Cipher cipher = Cipher.getInstance("RSA/ECB/PKCS1PADDING"); //$NON-NLS-1$
		cipher.init(Cipher.ENCRYPT_MODE, key);
		return cipher.doFinal(data);
	}

}
