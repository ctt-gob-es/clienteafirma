/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;

/** Representa a un <i>AOKeyStoreManager</i> para acceso al almacen de claves de Windows en el que
 * se da prioridad al uso de los certificados del DNIe y CERES desde los almacenes preferentes
 * que desde el CSP/MiniDriver de Windows de estas tarjetas. */
public class CAPIUnifiedKeyStoreManager extends AggregatedKeyStoreManager {

	private static final String SYSTEM_PROPERTY_USERNAME = "user.name"; //$NON-NLS-1$
	private static final String ENVIRONMENT_PROPERTY_USERPROFILE = "USERPROFILE"; //$NON-NLS-1$
	private static final String TEMPORARY_PROFILE_NAME = "TEMP"; //$NON-NLS-1$

	private PasswordCallback passwordCallback = null;
	private Object[] configParams = null;

	/** Indica si el almacen se cargo previamente. */
	private boolean initialized = false;

	/** Crea un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Windows. */
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
		this.configParams = params != null ? params.clone() : null;

		// Vaciamos el listado de almacenes agregados
		removeAll();

		final Object parentComponent = params != null && params.length > 0 ? params[0] : null;

		// Si NO nos encontramos en un perfil temporal, cargaremos el almacen de Windows y a continuacion
		// los almacenes preferentes. En cambio, si estamos en un almacen temporal y, por tanto no tendremos
		// certificados en el almacen, cargaremos primero los almacenes preferentes y, si no se encuentran,
		// intentaremos cargar el resto de tarjetas inteligentes que soportemos.
		AOKeyStoreManager capiKsm = null;
		final boolean usingTemporaryProfile = isTemporaryProfile();
		if (usingTemporaryProfile) {
			LOGGER.info("Detectado perfil temporal. Se omitira la carga del almacen de Windows y se buscaran tarjetas conocidas insertadas"); //$NON-NLS-1$
		}
		else {
			// Primero anadimos el almacen principal de Windows
			capiKsm = new CAPIKeyStoreManager();
			try {
				capiKsm.init(type, store, pssCallBack, params, forceReset);
			}
			catch(final Exception e) {
				LOGGER.severe(
						"No se ha podido cargar el almacen de Windows, se continuara con los almacenes preferentes: " + e //$NON-NLS-1$
						);
			}
			addKeyStoreManager(capiKsm);
		}

		// Intentamos ahora agregar los almacenes externos preferentes (DNIe/CERES)
		boolean preferredKsPresent = false;
		if (forceReset || !this.initialized) {
			try {
				preferredKsPresent = KeyStoreUtilities.addPreferredKeyStoreManagers(this, parentComponent);
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Se cancelo el uso del driver Java: " + e); //$NON-NLS-1$
				preferredKsPresent = true;
			}
		}

		// Si estamos en un perfil temporal, cargaremos las tarjetas que encontremos a
		// partir de su PKCS#12
		if (!preferredKsPresent && usingTemporaryProfile) {

			final AOKeyStoreManager scKs = new SmartCardUnifiedKeyStoreManager();
			try {
				scKs.init(type, store, pssCallBack, params, forceReset);
			}
			catch(final Exception e) {
				LOGGER.severe(
						"No se ha podido cargar el almacen unificado de tarjetas: " + e //$NON-NLS-1$
						);
			}
			addKeyStoreManager(scKs);
		}

		// Si se cargo el almacen de Windows, al comprobar si estaba disponible alguno de
		// los almacenes preferentes (tarjetas) se habra perdido la conexion con cualquier
		// otra tarjeta configurada en el almacen de Windows. Asi que, cuando no se
		// encuentran los preferentes, se reinicia el almacen para recuperar la conexion
		// con cualquier tarjeta conectada y que posiblemente desee usarse
		if (capiKsm != null && !preferredKsPresent) {
			try {
				capiKsm.refresh();
			} catch (final IOException e) {
				LOGGER.warning("Error al refrescar el almacen de claves de Windows: " + e); //$NON-NLS-1$
			}
		}

		this.initialized = true;
	}

	@Override
	public void refresh() throws IOException {
		init(AOKeyStore.WINDOWS, null, this.passwordCallback, this.configParams, true);
	}

	/**
	 * Comprueba si el perfil de Windows activo se trata de un perfil temporal.<br>
	 * Una forma mas segura de comprobarlo seria insertar un KeyEntry en el almacen
	 * de Windows. Si esta operacion fallase con KeyStoreException indicando que se
	 * trata de un perfil temporal, se trataria de edste tipo de perfil, pero la
	 * operaci&oacute;n seria demasiado problem&aacute;tica (se necesita una clave
	 * a guardar, el almacen podr&iacute;a tener contrase&ntilde;a,...).
	 * @return {@code true} si consideramos que estamos en un perfil temporal,
	 * {@code false} en caso contrario o si no se ha podido comprobar.
	 */
	private static boolean isTemporaryProfile() {

		final String userName = System.getProperty(SYSTEM_PROPERTY_USERNAME);
		final String profileDirName = System.getenv(ENVIRONMENT_PROPERTY_USERPROFILE);
		if (userName == null || profileDirName == null) {
			LOGGER.warning("No se ha podido identificar el nombre de usuario o su directorio de perfil. No se comprobara si se trata de un perfil temporal"); //$NON-NLS-1$
			return false;
		}
		final String profileName = new File(profileDirName).getName();

		return !userName.equals(profileName) &&
				(TEMPORARY_PROFILE_NAME.equals(profileName) ||
						profileName.startsWith(TEMPORARY_PROFILE_NAME + '.'));

	}
}
