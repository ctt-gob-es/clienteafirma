/* Copyright (C) 2025 [Gobierno de Espana]
 * This file is part of "Autofirma".
 * "Autofirma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.IOException;
import java.io.InputStream;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox accedidos
 *  v&iacute;a NSS en el que se tratan de forma unificada los m&oacute;dulos internos y externos.
 *  Se a&ntilde;de tambi&eacute;n el almac&eacute;n de claves del sistema operativo que se est&eacute; utilizando */
public class MozillaUnifiedWithOSKeyStoreManager extends MozillaUnifiedKeyStoreManager {

	/** Crea un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox. */
	public MozillaUnifiedWithOSKeyStoreManager() {
		setKeyStoreType(AOKeyStore.MOZ_UNI_WITH_OS);
	}

	/** Inicializa la clase gestora de almacenes de claves. */
	@Override
	public void init(final AOKeyStore type,
			               final InputStream store,
			               final PasswordCallback pssCallBack,
			               final Object[] params,
			               final boolean forceReset) {

		LOGGER.info("Inicializamos el almacen de tipo: " + type); //$NON-NLS-1$

		this.passwordCallback = pssCallBack;
		this.configParams = params != null ? params.clone() : null;

		// Vaciamos el listado de almacenes agregados ya que esta llamada puede realizarse como
		// parte de una operacion de refresco del almacen
		removeAll();
		
		super.init(type, store, pssCallBack, params, forceReset);

		Object parentComponent = null;
		if (this.configParams != null && this.configParams.length > 0) {
			parentComponent = this.configParams[0];
		}
		
		//Agregamos el almacen del SO
		AOKeyStoreManager osKeystore;

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			LOGGER.info("Agregamos las entradas del almacen de claves de Windows"); //$NON-NLS-1$
			try {
				osKeystore = AOKeyStoreManagerFactory.getWindowsMyCapiKeyStoreManager(true);
				addKeyStoreManager(osKeystore);
			} catch (final Exception e) {
				LOGGER.severe("No se ha podido agregar el almacen de Windows: " + e //$NON-NLS-1$
				);
			}
		} else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			LOGGER.info("Agregamos las entradas del almacen de claves del llavero de macOS"); //$NON-NLS-1$
			try {
				osKeystore = AOKeyStoreManagerFactory.getMacOSXKeyStoreManager(AOKeyStore.APPLE, null, true,
						parentComponent);
				addKeyStoreManager(osKeystore);
			} catch (final Exception e) {
				LOGGER.severe("No se ha podido agregar el almacen de MacOS: " + e //$NON-NLS-1$
				);
			}
		}

		setKeyStoreType(type);

		this.initialized = true;
	}

	@Override
	public void refresh() throws IOException {
		init(AOKeyStore.MOZ_UNI_WITH_OS, null, this.passwordCallback, this.configParams, true);
	}

}
