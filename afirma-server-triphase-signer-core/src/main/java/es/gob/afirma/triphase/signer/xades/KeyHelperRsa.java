package es.gob.afirma.triphase.signer.xades;

import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.interfaces.RSAPublicKey;
import java.util.Dictionary;
import java.util.Hashtable;

final class KeyHelperRsa implements KeyHelper {

	private static final String RSA = "RSA"; //$NON-NLS-1$

	private static final Dictionary<Integer, PrivateKey> KEYS = new Hashtable<>();

	KeyHelperRsa() {
		// Vacio y 'package protected'
	}

	@Override
	public PrivateKey getPrivateKey(final PublicKey puK) throws NoSuchAlgorithmException {
		if (puK == null) {
			throw new IllegalArgumentException(
				"La clave publica no puede ser nula" //$NON-NLS-1$
			);
		}
		if (!(puK instanceof RSAPublicKey)) {
			throw new IllegalArgumentException(
				"Solo se admiten claves RSA, pero se ha recibido una de tipo: " + puK.getClass().getName() //$NON-NLS-1$
			);
		}
		return getPrivateKey(
			((RSAPublicKey)puK).getModulus().bitLength()
		);
	}

	private static PrivateKey getPrivateKey(final int keySize) throws NoSuchAlgorithmException {
		PrivateKey ret = KEYS.get(Integer.valueOf(keySize));
		if (ret == null) {
			final KeyPairGenerator keyGen = KeyPairGenerator.getInstance(RSA);
			keyGen.initialize(keySize);
			ret = keyGen.generateKeyPair().getPrivate();
			KEYS.put(Integer.valueOf(keySize), ret);
		}
		return ret;
	}

}
