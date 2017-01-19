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

import java.io.IOException;
import java.io.InputStream;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;

/** Representa a un <i>AOKeyStoreManager</i> para acceso al almacen de claves de Windows en el que
 * se da prioridad al uso de los certificados del DNIe y CERES desde los almacenes preferentes
 * que desde el CSP/MiniDriver de Windows de estas tarjetas. */
public class CAPIUnifiedKeyStoreManager extends AggregatedKeyStoreManager {

	private PasswordCallback passwordCallback = null;
	private Object[] configParams = null;

	/** Indica si el almacen se cargo previamente. */
	private boolean initialized = false;

	/** Crea un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox. */
	public CAPIUnifiedKeyStoreManager() {
		setKeyStoreType(AOKeyStore.WINDOWS);
	}

	/** Inicializa la clase gestora de almacenes de claves. */
	@Override
	public final void init(final AOKeyStore type,
			               final InputStream store,
			               final PasswordCallback pssCallBack,
			               final Object[] params,
			               final boolean forceReset) {

		this.passwordCallback = pssCallBack;
		this.configParams = params;

		// Vaciamos el listado de almacenes agregados
		removeAll();

		final Object parentComponent = params != null && params.length > 0 ? params[0] : null;

		// Primero anadimos el almacen principal de Windows
		final AOKeyStoreManager capiKsm = new CAPIKeyStoreManager();
		try {
			capiKsm.init(type, store, pssCallBack, params, forceReset);
		}
		catch(final Exception e) {
			LOGGER.severe(
					"No se ha podido cargar el almacen de Windows, se continuara con los almacenes preferentes: " + e //$NON-NLS-1$
					);
		}
		addKeyStoreManager(capiKsm);

		// Intentamos ahora agregar los almacenes externos preferentes ajenos a los
		// dispositivos de seguridad configurados en Firefox haciendo uso del controlador Java
		boolean preferredKsPresent = false;
		if (forceReset || !this.initialized) {
			try {
				preferredKsPresent = KeyStoreUtilities.addPreferredKeyStoreManagers(this, parentComponent);
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Se cancelo el uso del driver Java"); //$NON-NLS-1$
				preferredKsPresent = true;
			}
		}

		// Al comprobar si estaba disponible alguno de los almacenes preferentes (tarjetas)
		// se habra perdido la conexion con cualquier otra tarjeta configurada en el almacen
		// de Windows. Asi que, cuando no se encuentran los preferentes, se reinicia el almacen
		// para recuperar la conexion con cualquier tarjeta conectada y que posiblemente desee usarse
		if (!preferredKsPresent) {
			try {
				capiKsm.refresh();
			} catch (final IOException e) {
				LOGGER.warning("Error al refrescar el almacen de claves de Windows"); //$NON-NLS-1$
			}
		}

		this.initialized = true;
	}

	@Override
	public void refresh() throws IOException {
		init(AOKeyStore.WINDOWS, null, this.passwordCallback, this.configParams, true);
	}
}
