/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla.shared;

import java.util.Map;

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
	protected final Map<String, String> getExternalStores() {
		return SharedNssUtil.getSharedNssPKCS11Modules(
			!(Boolean.getBoolean(INCLUDE_NATIVE_DNIE_P11) || Boolean.parseBoolean(System.getenv(INCLUDE_NATIVE_DNIE_P11))), // Excluir modulos nativos DNIe
			true  // Incluir los PKCS#11 que esten instalados en el sistema pero no en Mozilla
		);
	}

	@Override
	protected AOKeyStoreManager getNssKeyStoreManager() {
		return new NssKeyStoreManager(getParentComponent(), true);
	}

}
