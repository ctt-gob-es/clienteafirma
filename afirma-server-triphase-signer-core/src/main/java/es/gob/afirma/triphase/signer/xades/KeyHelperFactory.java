package es.gob.afirma.triphase.signer.xades;

import java.security.InvalidKeyException;
import java.security.Key;
import java.security.interfaces.RSAKey;

final class KeyHelperFactory {

	private KeyHelperFactory() {
		// No instanciable
	}

	static KeyHelper getKeyHelper(final Key key) throws InvalidKeyException {
		if (key == null) {
			throw new InvalidKeyException(
				"La clave no puede ser nula" //$NON-NLS-1$
			);
		}
		if (key instanceof RSAKey) {
			return new KeyHelperRsa();
		}
		throw new InvalidKeyException(
			"Tipo de clave no soportada: " + key.getClass().getName() //$NON-NLS-1$
		);
	}

}
