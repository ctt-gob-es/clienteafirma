/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores;

import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.PrivateKey;

/** Gestor de claves del llavero de Apple OS X.
 * OS X necesita su propio gestor por la peculiaridades en la recuperaci&oacute;n de claves privadas
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class AppleKeyStoreManager extends AOKeyStoreManager {

	AppleKeyStoreManager() {
		setKeyStoreType(AOKeyStore.APPLE);
	}

	@Override
	public KeyStore.PrivateKeyEntry getKeyEntry(final String alias) throws KeyStoreException {
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
