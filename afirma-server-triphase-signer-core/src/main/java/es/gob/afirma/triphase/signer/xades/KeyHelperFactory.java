package es.gob.afirma.triphase.signer.xades;

import java.security.InvalidKeyException;
import java.security.Key;
import java.security.interfaces.ECPublicKey;
import java.security.interfaces.RSAPublicKey;

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
		if (key instanceof RSAPublicKey) {
			return new KeyHelperRsa();
		}
		// La implementacion con curva eliptica para firma XAdES fallara en los entornos afectados
		// por el error de Java '8182580':
		// https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8182580
		if (key instanceof ECPublicKey || key instanceof org.spongycastle.jce.interfaces.ECPublicKey) {
			 return new KeyHelperEcdsa();
		}
		throw new InvalidKeyException(
			"Tipo de clave no soportada: " + key.getClass().getName() //$NON-NLS-1$
		);
	}

}
