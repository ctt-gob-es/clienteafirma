/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.*;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

import javax.security.auth.callback.PasswordCallback;
import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.logging.Level;

/**
 * Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves tipo NSS de sistema (compartido)
 * en el que se tratan de forma unificada los m&oacute;dulos internos y externos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s.
 */
public final class MozillaLikeKeyStoreManager extends AggregatedKeyStoreManager {

	private final File nssKeystoreDir;

	/** Crea un <i>AOKeyStoreManager</i> para acceso a almacenes de claves
	 * tipo NSS compartido (de sistema). */
	public MozillaLikeKeyStoreManager(File nssKeystoreDir) {
		this.nssKeystoreDir = nssKeystoreDir;
	}

	/** Rutas posibles del almac&eacute;n de Chrome en Linux relativas al directorio de usuario. */
	private static final String[] POSSIBLE_CHROME_NSS_PATHS = new String[] {
			".local/share/pki/nssdb", // Ruta clasica de almacen del sistema en Ubuntu //$NON-NLS-1$
			"etc/pki/nssdb" // Ruta clasica de almacen del sistema en Fedora //$NON-NLS-1$
	};

	/** Rutas posibles del almac&eacute;n de Chromium en Linux relativas al directorio de usuario. */
	private static final String[] POSSIBLE_CHROMIUM_NSS_PATHS = new String[] {
			"snap/chromium/current/.local/share/pki/nssdb", // Ruta clasica en Ubuntu //$NON-NLS-1$
            ".var/app/org.chromium.Chromium/data/pki/nssdb" // Ruta clasica en Fedora //$NON-NLS-1$
	};

	/** Rutas posibles del almac&eacute;n de Brave en Linux relativas al directorio de usuario. */
	private static final String[] POSSIBLE_BRAVE_NSS_PATHS = new String[] {
			"snap/brave/current/.local/share/pki/nssdb", // Ruta clasica en Ubuntu //$NON-NLS-1$
			".var/app/com.brave.Browser/data/pki/nssdb" // Ruta clasica en Fedora //$NON-NLS-1$
	};

	/** Crea un <i>AOKeyStoreManager</i> para acceso a almacenes de claves
	 * tipo NSS compartido (de sistema). */
	public MozillaLikeKeyStoreManager(AOKeyStore storeType) {
		File nssDir;
		switch (storeType) {
			case NSS_CHROME:
				nssDir = MozillaKeyStoreUtilitiesUnix.getNssProfileDir(POSSIBLE_CHROME_NSS_PATHS);
				break;
			case NSS_CHROMIUM:
				nssDir = MozillaKeyStoreUtilitiesUnix.getNssProfileDir(POSSIBLE_CHROMIUM_NSS_PATHS);
				break;
			case NSS_BRAVE:
				nssDir = MozillaKeyStoreUtilitiesUnix.getNssProfileDir(POSSIBLE_BRAVE_NSS_PATHS);
				break;
			default:
				throw new IllegalArgumentException("No se puede buscar automaticamente la ruta del almacen  " //$NON-NLS-1$
						+ storeType + ". Utilice el constructor en el que se indica la ruta"); //$NON-NLS-1$
		}
		this.nssKeystoreDir = nssDir;
	}

	/**
	 * Inicializa el almac&eacute;n NSS del directorio indicado en el constructor y trata de inicializar los PKCS#11
	 * asociados. En el momento de inicializar con &eacute;xito un PKCS#11, se deja de intentar inicializar el resto
	 * para evitar interrupciones con el actual.
	 * @param type Tipo de almac&eacute;n de claves interno de NSS.
	 * @param store No se utiliza.
	 * @param pssCallBack Manejador de contrase&ntilde;as de acceso al almac&eacute;n.
	 * @param params Listado de par&aacute;metros, el primero de los cuales puede ser el componente padre a nivel
	 *                  gr&aacute;fico.
	 * @param forceReset Fuerza la reinicializaci&oacute;n del almac&eacute;n.
	 * @throws AOKeyStoreManagerException Cuando ocurre un error en la inicializaci&oacute;n del almac&eacute;n.
	 * @throws IOException Cuando ocurre un error de entrada/salida durante la inicializaci&oacute;n del almac&eacute;n.
	 */
	@Override
	public void init(final AOKeyStore type,
			               final InputStream store,
			               final PasswordCallback pssCallBack,
			               final Object[] params,
			               final boolean forceReset) throws AOKeyStoreManagerException, IOException {

		// Guardamos los datos que parametros que podamos volver a necesitar
		this.setType(type);
		this.setEntryPasswordCallBack(pssCallBack);

		if (params != null && params.length > 0 && params[0] instanceof Component) {
			setParentComponent(params[0]);
		}

		// Inicializamos el almac&eacute;n NSS interno
		GenericNssKeyStoreManager internalNssKsm = new GenericNssKeyStoreManager(type, nssKeystoreDir);
		internalNssKsm.init(type, store, pssCallBack, params, forceReset);

		addKeyStoreManager(internalNssKsm);

		// Obtenemos los PKCS#11 externos y tratamos de inicializarlos, pero solo hasta que uno se inicialice correctamente
		Map<String, String> pkcs11Modules = MozillaKeyStoreUtilities.getExternalPkcs11ModulesFromPKCS11Txt(
				this.nssKeystoreDir, true, false);

		for (Map.Entry<String, String> entry : pkcs11Modules.entrySet()) {

			String name = entry.getKey();
			String libraryPath = entry.getValue();

			final AOKeyStoreManager pkcs11Ksm;
			try {
				pkcs11Ksm = initPkcs11KeyStoreManager(name, libraryPath, forceReset);
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.warning(
						"Se cancelo el acceso al almacen externo  '" + name + "', se continuara con el siguiente: " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
				continue;
			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido inicializar el PKCS#11 '" + name + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}
			addKeyStoreManager(pkcs11Ksm);

			LOGGER.info(
					"El almacen externo '" + name + "' ha podido inicializarse, se anadiran sus entradas y se detiene la carga del resto de almacenes" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
	}

	private AOKeyStoreManager initPkcs11KeyStoreManager(String name, String libraryPath, boolean forceReset)
			throws AOKeyStoreManagerException, IOException {

        try {
            return AOKeyStoreManagerFactory.getAOKeyStoreManager(
                    AOKeyStore.PKCS11, libraryPath, name,
                    new UIPasswordCallback(
                            FirefoxKeyStoreMessages.getString("MozillaUnifiedKeyStoreManager.1") + " " + name, //$NON-NLS-1$ //$NON-NLS-2$
                            getParentComponent()),
					getParentComponent(),
					forceReset
            );
        }
        catch (KeystoreAlternativeException e) {
            throw new AOKeyStoreManagerException("Fallo la carga del DNIe a traves de su PKCS#11", e,
					KeyStoreErrorCode.Internal.LOADING_PKCS11_DNIE_ERROR);
        }
	}

	@Override
	public void refresh() throws IOException {

		// Actualizamos el componente padre de los parametros
		PasswordCallback pc = getEntryPasswordCallBack();
		Object[] params = null;
		if (getParentComponent() != null) {
			params = new Object[] { getParentComponent() };
			if (pc != null && pc instanceof UIPasswordCallback) {
				((UIPasswordCallback) pc).setParent(getParentComponent());
			}
		}

		try {
			init(getType(), null, pc, params, true);
		}
		catch (IOException e) {
			throw e;
		}
		catch (AOKeyStoreManagerException e) {
			LOGGER.log(Level.WARNING,"No se ha podido actualizar el almacen de claves de tipo Mozilla", e); //$NON-NLS-1$
		}
	}
}
