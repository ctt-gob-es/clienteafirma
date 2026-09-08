/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */
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
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

import javax.security.auth.callback.PasswordCallback;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Level;

/**
 * Representa a un <i>AOKeyStoreManager</i> para acceso al almacen de claves del DNIe
 *  mediante su PKCS#11 en Windows.
 */
public class DNIePKCS11KeyStoreManager extends AggregatedKeyStoreManager {

	/** Nombres del controlador nativo de DNIe en Windows. */
	private static final String[] DNI_P11_FILES = {
		"DNIe_P11_x64.dll", //$NON-NLS-1$
		"DNIe_P11.dll" //$NON-NLS-1$
	};

	private PasswordCallback passwordCallback = null;

	/** Indica si el almacen se carg&oacute; previamente. */
	private boolean initialized = false;

	/** Crea un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Windows. */
	DNIePKCS11KeyStoreManager() {
		setType(AOKeyStore.PKCS11_DNIE);
	}

	/** Inicializa la clase gestora de almacenes de claves. */
	@Override
	public final void init(final AOKeyStore type,
			               final InputStream store,
			               final PasswordCallback pssCallBack,
			               final Object[] params,
			               final boolean forceReset) throws AOKeyStoreManagerException, IOException {

		this.passwordCallback = pssCallBack;

		// Se carga el almacen de DNIe con su PKCS#11
		if (forceReset || !this.initialized) {

			removeAll();

			Exception exception = null;
			for (final String libName : DNI_P11_FILES) {

				try {
					AOKeyStoreManager ksm = initDniePkcs11(libName, forceReset);
					addKeyStoreManager(ksm);
					LOGGER.info("El almacen externo PKCS#11 del DNIe ha podido inicializarse correctamente: " + libName); //$NON-NLS-1$
					break;
				}
				catch (AOCancelledOperationException e) {
					// El usuario cancelo la operacion
					throw e;
				}
				catch (FileNotFoundException e) {
					LOGGER.fine("No se encuentra la biblioteca PKCS#11 del DNIe: " + libName); //$NON-NLS-1$
				}
				catch (Exception e) {
					LOGGER.log(Level.FINE, "Fallo la carga del PKCS#11 del DNIe: " + libName, e); //$NON-NLS-1$
					exception = e;
				}
			}
			if (exception != null) {
				throw new AOKeyStoreManagerException("No se pudo inicializar ninguno de los PKCS#11 conocidos del DNIe",
						exception, KeyStoreErrorCode.Internal.LOADING_PKCS11_DNIE_ERROR);
			}
		}

		this.initialized = true;
	}

	/**
	 * Inicializa el almac&eacute;n del DNIe mediante su PKCS#11.
	 * @param p11LibName Nombre del fichero de la librer&iacute;a PKCS#11 del DNIe.
	 * @param forceReset Indica si se debe forzar al reinicio del almac&eacute;n si ya estaba iniciado.
	 * @return Gestor de almacenes de claves del PKCS#11 del DNIe.
	 * @throws IOException Cuando ocurre un error de entrada/salida.
	 * @throws KeystoreAlternativeException Cuando ocurre un error al inicializar el almac&eacute;n
	 * @throws FileNotFoundException Cuando no se encuentra el fichero de la librer&iacute;a PKCS#11 del DNIe.
	 */
	private AOKeyStoreManager initDniePkcs11(final String p11LibName, final boolean forceReset)
			throws FileNotFoundException, IOException, KeystoreAlternativeException {

		final File pkcs11File = new File(Platform.getSystemLibDir(), p11LibName);
		if (!pkcs11File.exists()) {
			throw new FileNotFoundException("No se encontro el PKCS#11 del DNIe en la ruta: " + pkcs11File.getAbsolutePath()); //$NON-NLS-1$
		}

		String libName = pkcs11File.getAbsolutePath();
		String descr = "PKCS#11 DNIe"; //$NON-NLS-1$

		PasswordCallback psc = this.passwordCallback != null
				? this.passwordCallback
				: new UIPasswordCallback(descr, getParentComponent());

		return AOKeyStoreManagerFactory.getAOKeyStoreManager(
				AOKeyStore.PKCS11, libName, descr, psc, getParentComponent(), forceReset);
	}

	@Override
	public void refresh() throws IOException {
		if (getParentComponent() != null
				&& this.passwordCallback != null && this.passwordCallback instanceof UIPasswordCallback) {
			((UIPasswordCallback) this.passwordCallback).setParent(getParentComponent());
		}

        try {
            init(AOKeyStore.PKCS11_DNIE, null, this.passwordCallback, null, true);
        } catch (AOKeyStoreManagerException e) {
            throw new IOException("Error al refrescar el almacen del DNIe mediante su PKCS#11", e); //$NON-NLS-1$
        }
    }
}
