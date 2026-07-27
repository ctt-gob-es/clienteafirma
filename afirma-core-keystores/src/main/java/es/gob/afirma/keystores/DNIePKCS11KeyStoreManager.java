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

/** Representa a un <i>AOKeyStoreManager</i> para acceso al almacen de claves del DNIe
 *  mediante su PKCS#11 */
public class DNIePKCS11KeyStoreManager extends AggregatedKeyStoreManager {

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
	public DNIePKCS11KeyStoreManager() {
		setKeyStoreType(AOKeyStore.PKCS11);
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

		// Se carga el almacen de DNIe con su PKCS#11
		if (!this.initialized) {
			final AOKeyStoreManager dniePkcs11Ksm = initDniePkcs11(forceReset);
			final boolean dniePkcs11Present = dniePkcs11Ksm != null;

			if (dniePkcs11Present) {
			    addKeyStoreManager(dniePkcs11Ksm);
			}
		}

		// El DNIe con PKCS#11 siempre tendra preferencia al MiniDriver de Windows
		setPreferred(true);

		this.initialized = true;
	}

	@Override
	public void refresh() throws IOException {
		init(AOKeyStore.PKCS11, null, this.passwordCallback, this.configParams, true);
	}

	/**
	 * Inicializa el almac&eacute;n del DNIe mediante su PKCS#11.
	 * @param forceReset Indica si se debe forzar al reinicio del almac&eacute;n si ya estaba iniciado.
	 */
	private AOKeyStoreManager initDniePkcs11(final boolean forceReset) {

		for (final String file : DNI_P11_FILES) {

			final File pkcs11File = new File(Platform.getSystemLibDir(), file);

			if (!pkcs11File.exists()) {
				continue;
			}

			final AOKeyStoreManager tmpKsm = new AOKeyStoreManager();

			try {
				internalInitStore(
						tmpKsm,
						"PKCS#11 DNIe", //$NON-NLS-1$
						forceReset,
						pkcs11File.getAbsolutePath()
				);

				LOGGER.info(
						"El almacen externo PKCS#11 del DNIe ha podido inicializarse correctamente: " + //$NON-NLS-1$
						pkcs11File.getAbsolutePath()
				);

				return tmpKsm;
			}
			catch (final AOCancelledOperationException ex) {
				LOGGER.warning(
						"Se cancelo el acceso al PKCS#11 del DNIe, se continuara con el siguiente: " + ex //$NON-NLS-1$
				);
			}
			catch (final Exception ex) {
				LOGGER.warning(
						"No se ha podido inicializar el PKCS#11 del DNIe desde '" + //$NON-NLS-1$
						pkcs11File.getAbsolutePath() + "': " + ex //$NON-NLS-1$
				);
			}
		}

		return null;
	}

	/** Inicializa un almac&eacute;n externo PKCS#11, mostrando un di&aacute;logo de inserci&oacute;n de PIN al usuario
	 * si es necesario.
	 * @param tmpKsm Gestor del almac&eacute;n.
	 * @param descr Nombre descriptivo del almac&eacute;n.
	 * @param forceReset Indica si se debe forzar al reinicio del almac&eacute;n si ya estaba iniciado.
	 * @param libName Nombre del m&oacute;dulo PKCS#11 del almac&eacute;n.
     * @throws AOKeyStoreManagerException Cuando ocurre cualquier problema durante la inicializaci&oacute;n
     * @throws IOException Si se ha insertado una contrase&ntilde;a incorrecta para la apertura del
     *                     almac&eacute;n de certificados.
     * @throws AOCancelledOperationException Cuando se cancela el di&aacute;logo de inserci&oacute;n de PIN. */
	private void internalInitStore(final AOKeyStoreManager tmpKsm,
			                              final String descr,
			                              final boolean forceReset,
			                              final String libName) throws AOKeyStoreManagerException, IOException {
		tmpKsm.init(
			AOKeyStore.PKCS11,
			null,
			this.passwordCallback != null
				? this.passwordCallback
				: new UIPasswordCallback(descr, null),
			new String[] {
				libName, descr.toString()
			},
			forceReset
		);
	}

}
