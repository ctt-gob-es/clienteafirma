/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.AggregatedKeyStoreManager;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox accedidos
 *  v&iacute;a NSS en el que se tratan de forma unificada los m&oacute;dulos internos y externos. */
public final class MozillaUnifiedKeyStoreManager extends AggregatedKeyStoreManager {

	/** Inicializa la clase gestora de almacenes de claves.
	 * @throws AOKeyStoreManagerException
	 *         Si no puede inicializarse ning&uacute;n almac&eacute;n de
	 *         claves, ni el NSS interno, ni ning&uacute;n PKCS#11 externo
	 *         definido en SecMod
	 * @throws IOException Cuando hay errores de entrada / salida */
	@Override
	public synchronized void init(final AOKeyStore type,
			                      final InputStream store,
			                      final PasswordCallback pssCallBack,
			                      final Object[] params,
			                      final boolean forceReset) throws AOKeyStoreManagerException,
			                                                       IOException {

		final Object parentComponent = params != null && params.length > 0 ? params[0] : null;

		// Primero anadimos el almacen principal NSS
		final AOKeyStoreManager ksm = new NssKeyStoreManager(parentComponent);
		ksm.init(type, store, pssCallBack, params, forceReset);
		addKeyStoreManager(ksm);

		// Vamos ahora con los almacenes externos, que se limpian antes de usarse quitando DNIe (porque se usa
		// el controlador Java) y anadiendo modulos conocidos si se encuentran en el sistema.
		final Map<String, String> externalStores = MozillaKeyStoreUtilities.getMozillaPKCS11Modules(true, true);

		if (externalStores.size() > 0) {
			final StringBuilder logStr = new StringBuilder("Encontrados los siguientes modulos PKCS#11 externos instalados en Mozilla / Firefox: "); //$NON-NLS-1$
			for (final String key : externalStores.keySet()) {
				logStr.append("'"); //$NON-NLS-1$
				logStr.append(externalStores.get(key));
				logStr.append("' "); //$NON-NLS-1$
			}
			LOGGER.info(logStr.toString());
		}
		else {
			LOGGER.info("No se han encontrado modulos PKCS#11 externos instalados en Firefox"); //$NON-NLS-1$
		}

		for (final String descr : externalStores.keySet()) {
			try {
				final AOKeyStoreManager tmpKsm = new AOKeyStoreManager();
				tmpKsm.init(
					AOKeyStore.PKCS11,
					null,
					new UIPasswordCallback(
						FirefoxKeyStoreMessages.getString("MozillaUnifiedKeyStoreManager.1") + " " + MozillaKeyStoreUtilities.getMozModuleName(descr.toString()), //$NON-NLS-1$ //$NON-NLS-2$
						parentComponent
					),
					new String[] {
						externalStores.get(descr), descr.toString()
					},
					forceReset
				);
				addKeyStoreManager(tmpKsm);
			}
			catch (final AOCancelledOperationException ex) {
				LOGGER.warning("Se cancelo el acceso al almacen externo  '" + descr + "', se continuara con el siguiente: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}
			catch (final Exception ex) {
				LOGGER.severe("No se ha podido inicializar el PKCS#11 '" + descr + "': " + ex); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}

			LOGGER.info("El almacen externo '" + descr + "' ha podido inicializarse, se anadiran sus entradas"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// Anadimos el controlador Java del DNIe **SIEMPRE**
		try {
			final AOKeyStoreManager tmpKsm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
				AOKeyStore.DNIEJAVA,
				null,
				null,
				null,
				parentComponent
			);
			LOGGER.info("El DNIe 100% Java ha podido inicializarse, se anadiran sus entradas"); //$NON-NLS-1$
			addKeyStoreManager(tmpKsm);
		}
		catch (final AOCancelledOperationException ex) {
			LOGGER.warning("Se cancelo el acceso al almacen DNIe 100% Java: " + ex); //$NON-NLS-1$
		}
		catch (final Exception ex) {
			LOGGER.warning("No se ha podido inicializar el controlador DNIe 100% Java: " + ex); //$NON-NLS-1$
		}

		if (lacksKeyStores()) {
			throw new AOKeyStoreManagerException("No se ha podido inicializar ningun almacen, interno o externo, de Firefox"); //$NON-NLS-1$
		}

	}

}
