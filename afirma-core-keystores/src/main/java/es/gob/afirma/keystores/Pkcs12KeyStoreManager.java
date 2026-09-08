/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

import javax.security.auth.callback.PasswordCallback;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves tipo PKCS#12 / PFX.
 * Contempla la posibilidad de que el almac&eacute;n y las claves tengan distintas contrase&ntilde;as
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class Pkcs12KeyStoreManager extends FileKeyStoreManager {

	private PasswordCallback passwordCallBack = null;

	private String pkcs12Path = null;

	Pkcs12KeyStoreManager() {
		setType(AOKeyStore.PKCS12);
	}

	/** {@inheritDoc} */
	@Override
	public void init(final AOKeyStore type,
                     final InputStream store,
                     final PasswordCallback pssCallBack,
                     final Object[] params,
                     final boolean forceReset) throws AOKeyStoreManagerException,
                                                      IOException {

		this.passwordCallBack = pssCallBack;

		// Si es posible, personalizamos el dialogo de solicitud de contrasena con el nombre del fichero PKCS#12
		if (this.passwordCallBack instanceof UIPasswordCallback) {
			this.pkcs12Path = getKeyStoreFile();
			final String promptText = this.pkcs12Path != null
					? KeyStoreMessages.getString("AOKeyStore.15", new File(this.pkcs12Path).getName()) //$NON-NLS-1$
					: KeyStoreMessages.getString("AOKeyStore.1"); //$NON-NLS-1$
			((UIPasswordCallback) this.passwordCallBack).setPrompt(promptText);
		}

		setKeyStore(init(store, this.passwordCallBack));
	}

	@Override
	public String getReference() {
		return this.getType().name() + ":" + this.pkcs12Path; //$NON-NLS-1$
	}
}
