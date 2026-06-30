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
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

/** Representa a un <i>AOKeyStoreManager</i> para acceso al almacen de claves de Windows en el que
 * se da prioridad al uso de los certificados del DNIe y CERES desde los almacenes preferentes
 * que desde el CSP/MiniDriver de Windows de estas tarjetas. */
public class CAPIUnifiedKeyStoreManager extends AggregatedKeyStoreManager {

	private static final String SYSTEM_PROPERTY_USERNAME = "user.name"; //$NON-NLS-1$
	private static final String ENVIRONMENT_PROPERTY_USERPROFILE = "USERPROFILE"; //$NON-NLS-1$
	private static final String TEMPORARY_PROFILE_NAME = "TEMP"; //$NON-NLS-1$
	
	/** Nombres del controlador nativo de DNIe en sistemas no-Linux (Windows, OS X, etc.). */
	private static final String[] DNI_P11_FILES = {
		"DNIe_P11_x64.dll", //$NON-NLS-1$
		"DNIe_P11.dll" //$NON-NLS-1$
	};

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
		
		// Comprobamos si existe el PKCS#11 para el DNIe en el sistema y en caso afirmativo,
		// utilizarlo en lugar el de Windows para evitar problemas al realizar firmas consecutivas.
		for (String file : DNI_P11_FILES) {
			final String pkcs11Path = Platform.getSystemLibDir() + "\\" + file; //$NON-NLS-1$
			if (new File(pkcs11Path).exists()) {
				final AOKeyStoreManager tmpKsm = new AOKeyStoreManager();
				try {
					internalInitStore(tmpKsm, "PKCS#11", parentComponent, forceReset, pkcs11Path); //$NON-NLS-1$
				}
				catch (final AOCancelledOperationException ex) {
					LOGGER.warning(
						"Se cancelo el acceso al PKCS#11 del DNIe, se continuara con el siguiente: " + ex //$NON-NLS-1$
					);
					continue;
				}
				catch (final Exception ex) {
					LOGGER.warning(
							"No se ha podido inicializar el PKCS#11: " + ex //$NON-NLS-1$
					);
					continue;
				}
				addKeyStoreManager(tmpKsm);

				LOGGER.info(
					"El almacen externo PKCS#11 ha podido inicializarse correctamente" //$NON-NLS-1$
				);
				break;
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
	
	/** Inicializa un almac&eacute;n externo PKCS#11, mostrando un di&aacute;logo de inserci&oacute;n de PIN al usuario
	 * si es necesario.
	 * @param tmpKsm Gestor del almac&eacute;n.
	 * @param descr Nombre descriptivo del almac&eacute;n.
	 * @param parentComponent Componente padre sobre el que mostrar componentes gr&aacute;ficos.
	 * @param forceReset Indica si se debe forzar al reinicio del almac&eacute;n si ya estaba iniciado.
	 * @param libName Nombre del m&oacute;dulo PKCS#11 del almac&eacute;n.
     * @throws AOKeyStoreManagerException Cuando ocurre cualquier problema durante la inicializaci&oacute;n
     * @throws IOException Si se ha insertado una contrase&ntilde;a incorrecta para la apertura del
     *                     almac&eacute;n de certificados.
     * @throws AOCancelledOperationException Cuando se cancela el di&aacute;logo de inserci&oacute;n de PIN. */
	private static void internalInitStore(final AOKeyStoreManager tmpKsm,
			                              final String descr,
			                              final Object parentComponent,
			                              final boolean forceReset,
			                              final String libName) throws AOKeyStoreManagerException, IOException {
		tmpKsm.init(
			AOKeyStore.PKCS11,
			null,
			new UIPasswordCallback(
				descr,
				parentComponent
			),
			new String[] {
				libName, descr.toString()
			},
			forceReset
		);
	}

}
