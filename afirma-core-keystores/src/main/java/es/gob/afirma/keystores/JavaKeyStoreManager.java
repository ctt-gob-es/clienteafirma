/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.io.IOException;
import java.io.InputStream;

import javax.security.auth.callback.PasswordCallback;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves tipo JKS.
 * Contempla la posibilidad de que el almac&eacute;n y las claves tengan la misma o distintas contrase&ntilde;as.
 */
public final class JavaKeyStoreManager extends FileKeyStoreManager {

	JavaKeyStoreManager() {
		setKeyStoreType(AOKeyStore.JAVA);
	}

	/** {@inheritDoc} */
	@Override
	public void init(final AOKeyStore type,
                     final InputStream store,
                     final PasswordCallback pssCallBack,
                     final Object[] params,
                     final boolean forceReset) throws AOKeyStoreManagerException,
                                                      IOException {
		setKeyStore(init(store, pssCallBack));
	}

}
