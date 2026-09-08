/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.jmulticard.ui.DialogBuilder;

import javax.security.auth.callback.PasswordCallback;
import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Level;

/** Representa a un <i>AOKeyStoreManager</i> para acceso al almacen de claves de Windows en el que
 * se da prioridad al uso de los certificados del DNIe y CERES desde los almacenes preferentes
 * que desde el CSP/MiniDriver de Windows de estas tarjetas. */
public class CAPIUnifiedKeyStoreManager extends AggregatedKeyStoreManager {

	/**
	 * Propiedad del sistema con la que se indica que debe usarse el PKCS#11 del DNIe en lugar del CSP/MiniDriver
	 * de Windows. Esta propiedad no deber&iacute;a usarse cuando se encuentre JMulticard activado, ya que lo
	 * pedir&iacute;a simultaneamente.
	 */
	private static final String SYSTEM_PROPERTY_ENABLED_PKCS11_DNIE = "dnie.pkcs11.enabled"; //$NON-NLS-1$

	private static final String SYSTEM_PROPERTY_USERNAME = "user.name"; //$NON-NLS-1$
	private static final String ENVIRONMENT_PROPERTY_USERPROFILE = "USERPROFILE"; //$NON-NLS-1$
	private static final String TEMPORARY_PROFILE_NAME = "TEMP"; //$NON-NLS-1$

	private PasswordCallback passwordCallback = null;
	private Object[] configParams = null;

	/** Indica si el almacen se carg&oacute; previamente. */
	private boolean initialized = false;

	/** Crea un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Windows. */
	CAPIUnifiedKeyStoreManager() {
		setType(AOKeyStore.WINDOWS_UNI);
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
		// los almacenes preferentes. En cambio, si estamos en un almacen temporal y, por tanto, no tendremos
		// certificados en el almacen, cargaremos primero los almacenes preferentes y, si no se encuentran,
		// intentaremos cargar el resto de tarjetas inteligentes que soportemos.
		AOKeyStoreManager capiKsm = null;
		final boolean usingTemporaryProfile = isTemporaryProfile();
		if (usingTemporaryProfile) {
			LOGGER.info("Detectado perfil temporal. Se omitira la carga del almacen de Windows y se buscaran tarjetas conocidas insertadas"); //$NON-NLS-1$
		}
		else {
			// Primero anadimos el almacen principal de Windows
			try {
				capiKsm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
						AOKeyStore.WINDOWS, null, null, pssCallBack, parentComponent, forceReset);
				addKeyStoreManager(capiKsm);
			}
			catch(final Exception e) {
				LOGGER.severe(
						"No se ha podido cargar el almacen de Windows, se continuara con los almacenes preferentes: " + e //$NON-NLS-1$
						);
			}
		}

		// Intentamos ahora agregar los almacenes externos preferentes (DNIe/CERES)
		boolean preferredKsPresent = false;
		if (forceReset || !this.initialized) {
			try {
				preferredKsPresent = KeyStoreUtilities.addJMulticardKeyStoreManagers(this, parentComponent, forceReset);
				setSmartCardAdded(preferredKsPresent);
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Se cancelo el uso del driver Java: " + e); //$NON-NLS-1$
				preferredKsPresent = true;
			}
		}


		// Si se pide cargar el PKCS#11 del DNIe y no se han cargado tarjetas mediante JMulticard,
		// intentamos cargar el DNIe mediante su PKCS#11
		boolean useDnieWithPkcs11 = Boolean.getBoolean(SYSTEM_PROPERTY_ENABLED_PKCS11_DNIE);
		if (useDnieWithPkcs11 && !preferredKsPresent) {
			try {
				addKeyStoreManager(0, getDNIePKCS11KeyStoreManager(forceReset));
				setSmartCardAdded(true);
			}
			catch (AOCancelledOperationException e) {
				LOGGER.info("El usuario cancelo el dialogo de carga del DNIe con PKCS#11: " + e); //$NON-NLS-1$
			}
			catch (AOKeyStoreManagerException e) {
				LOGGER.log(Level.WARNING, "No se pudo inicializar el DNIe mediante PKCS#11", e); //$NON-NLS-1$
			}
			catch (IOException e) {
				LOGGER.log(Level.WARNING, "No se pudo acceder a los recursos para la carga del DNIe mediante PKCS#11", e); //$NON-NLS-1$
			}
		}

		// Si estamos en un perfil temporal y no se han cargado tarjetas aun, cargaremos las tarjetas que encontremos a
		// partir de su PKCS#11
		if (!isSmartCardAdded() && usingTemporaryProfile) {
			try {
				addKeyStoreManager(AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.KNOWN_SMARTCARDS,
						null, null, pssCallBack, parentComponent, forceReset));
			}
			catch(final Exception e) {
				LOGGER.severe(
						"No se ha podido cargar el almacen unificado de tarjetas: " + e //$NON-NLS-1$
						);
			}
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
		// Reestablecemos el componente padre si se establecio externamente.
		if (getParentComponent() != null) {
			if (this.configParams == null || this.configParams.length == 0) {
				this.configParams = new Object[1];
			}
			this.configParams[0] = getParentComponent();
		}
		init(AOKeyStore.WINDOWS_UNI, null, this.passwordCallback, this.configParams, true);
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

	/**
	 * Obtiene el almacen del DNIe mediante su PKCS#11
	 * @return Almac&eacute;n de DNIe.
	 * @throws AOKeyStoreManagerException si ocurre un error al acceder o validar el keystore alternativo.
	 * @throws IOException si se produce un error de entrada/salida durante la lectura o escritura de datos.
	 */
	public AOKeyStoreManager getDNIePKCS11KeyStoreManager(boolean forceReset)
			throws AOKeyStoreManagerException, IOException {

		Component parent = null;
		if (this.configParams != null && this.configParams.length > 0) {
			parent = this.configParams[0] instanceof Component ? (Component) this.configParams[0] : null;
		}

		final PasswordCallback psc = DialogBuilder.getDefaultDniePasswordCallback(parent);

		final AOKeyStoreManager ksmCapi;
        try {
            ksmCapi = AOKeyStoreManagerFactory.getAOKeyStoreManager(
                    AOKeyStore.PKCS11_DNIE, null, null, psc, parent, forceReset);
        }
        catch (KeystoreAlternativeException e) {
            throw new AOKeyStoreManagerException("No se pudo cargar el DNIe a traves de su PKCS#11",
					e, KeyStoreErrorCode.Internal.LOADING_PKCS11_DNIE_ERROR);
        }

        return ksmCapi;
	}
}
