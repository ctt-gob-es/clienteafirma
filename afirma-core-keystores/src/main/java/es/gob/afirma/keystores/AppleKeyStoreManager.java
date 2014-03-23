package es.gob.afirma.keystores;

import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.UnrecoverableEntryException;

import javax.security.auth.callback.PasswordCallback;

/** Gestor de claves del llavero de Apple OS X.
 * OS X necesita su propio gestor por la peculiaridades en la recuperaci&oacute;n de claves privadas
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class AppleKeyStoreManager extends AOKeyStoreManager {

	AppleKeyStoreManager() {
		setKeyStoreType(AOKeyStore.APPLE);
	}

	@Override
	public KeyStore.PrivateKeyEntry getKeyEntry(final String alias,
			                                    final PasswordCallback pssCallback) throws KeyStoreException,
			                                    										   NoSuchAlgorithmException,
			                                    										   UnrecoverableEntryException {
		if (getKeyStore() == null) {
			throw new IllegalStateException(
				"Se han pedido claves a un almacen no inicializado" //$NON-NLS-1$
			);
		}
		if (alias == null) {
			throw new IllegalArgumentException("El alias no puede ser nulo"); //$NON-NLS-1$
		}
		if (getKeyStore().containsAlias(alias)) {
            PrivateKey key = null;
            try {
                LOGGER.info("Llavero de Mac OS X, se tratan directamente las claves privadas"); //$NON-NLS-1$
                key = (PrivateKey) getKeyStore().getKey(alias, "dummy".toCharArray()); //$NON-NLS-1$
            }
            catch (final Exception e) {
            	LOGGER.warning("Error recuperando directamente la clave privada en Mac OS X: " + e); //$NON-NLS-1$
            }
            if (key == null) {
            	throw new UnsupportedOperationException("La entrada no tiene clave privada"); //$NON-NLS-1$
            }
            return new KeyStore.PrivateKeyEntry(key, getCertificateChain(alias));
		}
		LOGGER.warning("El almacen no contiene ninguna clave con el alias '" + alias + "', se devolvera null"); //$NON-NLS-1$ //$NON-NLS-2$
		return null;
	}

}
