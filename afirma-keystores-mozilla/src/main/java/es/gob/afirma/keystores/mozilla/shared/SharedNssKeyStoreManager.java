/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla.shared;

import java.io.InputStream;
import java.util.Map;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.mozilla.MozillaUnifiedKeyStoreManager;
import es.gob.afirma.keystores.mozilla.NssKeyStoreManager;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves tipo NSS de sistema (compartido)
 * en el que se tratan de forma unificada los m&oacute;dulos internos y externos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SharedNssKeyStoreManager extends MozillaUnifiedKeyStoreManager {

	/** Crea un <i>AOKeyStoreManager</i> para acceso a almacenes de claves
	 * tipo NSS compartido (de sistema). */
	public SharedNssKeyStoreManager() {
		setKeyStoreType(AOKeyStore.SHARED_NSS);
	}

	@Override
	protected Map<String, String> getExternalStores() {
		return SharedNssUtil.getSharedNssPKCS11Modules(
			!(Boolean.getBoolean(INCLUDE_NATIVE_DNIE_P11) || Boolean.parseBoolean(System.getenv(INCLUDE_NATIVE_DNIE_P11_ENV))), // Excluir modulos nativos DNIe
			true  // Incluir los PKCS#11 que esten instalados en el sistema pero no en Mozilla
		);
	}

	@Override
	protected AOKeyStoreManager getNssKeyStoreManager() {
		return new NssKeyStoreManager(getParentComponent(), true);
	}

	/** Inicializa la clase gestora de almacenes de claves. */
	@Override
	public void init(final AOKeyStore type,
			               final InputStream store,
			               final PasswordCallback pssCallBack,
			               final Object[] params,
			               final boolean forceReset) {
		super.init(AOKeyStore.SHARED_NSS, store, pssCallBack, params, forceReset);
	}
}
