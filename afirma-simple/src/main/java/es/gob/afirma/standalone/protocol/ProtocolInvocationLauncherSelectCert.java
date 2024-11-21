/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.io.File;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateEncodingException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.keystores.CertificateContext;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.UrlParametersToSelectCert;
import es.gob.afirma.core.prefs.KeyStorePreferencesManager;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.CertificateFilter;
import es.gob.afirma.keystores.filters.CertFilterManager;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleKeyStoreManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.crypto.CypherDataManager;
import es.gob.afirma.standalone.so.macos.MacUtils;

final class ProtocolInvocationLauncherSelectCert {

	private static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ProtocolInvocationLauncherSelectCert() {
		// No instanciable
	}

	/** Procesa una peticion de selecci&oacute;n de certificado en invocaci&oacute;n
	 * por protocolo.
	 * @param options Par&aacute;metros de la operaci&oacute;n.
	 * @param protocolVersion Versi&oacute;n del protocolo de comunicaci&oacute;n.
	 * @param bySocket <code>true</code> para usar comunicaci&oacute;n por <i>socket</i> local,
	 *                 <code>false</code> para usar servidor intermedio.
	 * @return Certificado en base 64 o mensaje de error.
	 * @throws SocketOperationException Si hay errores en la
	 *                                  comunicaci&oacute;n por <i>socket</i> local. */
	static String processSelectCert(final UrlParametersToSelectCert options,
			final int protocolVersion,
			final boolean bySocket) throws SocketOperationException {

		if (options == null) {
			LOGGER.severe("Las opciones de firma son nulas"); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_NULL_URI;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}

        // Comprobamos si soportamos la version del protocolo indicada
		if (!ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED.support(protocolVersion)) {
			LOGGER.severe(String.format(
					"Version de protocolo no soportada (%1s). Version actual: %s2. Hay que actualizar la aplicacion.", //$NON-NLS-1$
					Integer.valueOf(protocolVersion),
					Integer.valueOf(ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED.getVersion())));
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_PROCEDURE;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}

        // Comprobamos si se exige una version minima del Cliente
        if (options.getMinimumClientVersion() != null) {
        	final String minimumRequestedVersion = options.getMinimumClientVersion();
        	final Version requestedVersion = new Version(minimumRequestedVersion);
        	if (requestedVersion.greaterThan(SimpleAfirma.getVersion())) {
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_MINIMUM_VERSION_NON_SATISTIED;
				ProtocolInvocationLauncherErrorManager.showError(errorCode);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
        	}
        }

        final String lastSelectedKeyStore = KeyStorePreferencesManager.getLastSelectedKeystore();
		final boolean useDefaultStore = PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_USE_DEFAULT_STORE_IN_BROWSER_CALLS);

		// Si hay marcado un almacen como el ultimo seleccionado, lo usamos (este es el caso en el que se llaman
		// varias operaciones de firma dentro de la misma invocacion a la aplicacion)
		AOKeyStore aoks = null;
		if (lastSelectedKeyStore != null && !lastSelectedKeyStore.isEmpty()) {
			aoks = SimpleKeyStoreManager.getLastSelectedKeystore();
		}
		// Si no, si el usuario definio un almacen por defecto para usarlo en las llamadas a la aplicacion, lo usamos
		else if (useDefaultStore) {
			final String defaultStore = PreferencesManager.get(PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE);
			if (!PreferencesManager.VALUE_KEYSTORE_DEFAULT.equals(defaultStore)) {
				aoks = SimpleKeyStoreManager.getKeyStore(defaultStore);
			}
		}
		// Si no, si en la llamada se definio el almacen que se debia usar, lo usamos
		else {
			aoks = SimpleKeyStoreManager.getKeyStore(options.getDefaultKeyStore());
		}

		// Si aun no se ha definido el almacen, se usara el por defecto para el sistema operativo
		if (aoks == null) {
			aoks = AOKeyStore.getDefaultKeyStoreTypeByOs(Platform.getOS());
		}

		final String aoksLib;
		if (useDefaultStore && (AOKeyStore.PKCS12.equals(aoks) || AOKeyStore.PKCS11.equals(aoks))) {
			aoksLib = PreferencesManager.get(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH);
		} else {
			aoksLib = options.getDefaultKeyStoreLib();
		}
		final CertFilterManager filterManager = new CertFilterManager(options.getExtraParams());
		final List<CertificateFilter> filters = filterManager.getFilters();
		final boolean mandatoryCertificate = filterManager.isMandatoryCertificate();
		final PrivateKeyEntry pke;

		if (options.getSticky() && !options.getResetSticky() && ProtocolInvocationLauncher.getStickyKeyEntry() != null) {

			LOGGER.info("Se usa Sticky Signature y tenemos valor de clave privada"); //$NON-NLS-1$
			pke = ProtocolInvocationLauncher.getStickyKeyEntry();

		} else {

			final PasswordCallback pwc = aoks.getStorePasswordCallback(null);
			final AOKeyStoreManager ksm;
			try {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
						aoks, // Store
						aoksLib, // Lib
						null, // Description
						pwc, // PasswordCallback
						null // Parent
				);
			} catch (final Exception e) {
				LOGGER.log(Level.SEVERE, "Error obteniendo el AOKeyStoreManager", e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_ACCESS_KEYSTORE;
				ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}

			LOGGER.info("Obtenido gestor de almacenes de claves: " + ksm); //$NON-NLS-1$

			try {
				MacUtils.focusApplication();
				String libName = null;
				if (aoksLib != null) {
					final File file = new File(aoksLib);
					libName = file.getName();
				}
				final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
					ksm,
					null,
					true,
					true, // showExpiredCertificates
					true, // checkValidity
					filters,
					mandatoryCertificate,
					libName
				);
				dialog.allowOpenExternalStores(filterManager.isExternalStoresOpeningAllowed());
				dialog.show();

				// Obtenemos el almacen del certificado seleccionado (que puede no ser el mismo
		    	// que se indico originalmente por haberlo cambiado desde el dialogo de seleccion)
				// y de ahi sacamos la referencia a la clave
				final CertificateContext context = dialog.getSelectedCertificateContext();
		    	final KeyStoreManager currentKsm = context.getKeyStoreManager();
				pke = currentKsm.getKeyEntry(context.getAlias());

				if (options.getSticky()) {
					ProtocolInvocationLauncher.setStickyKeyEntry(pke);
				} else {
					ProtocolInvocationLauncher.setStickyKeyEntry(null);
				}
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.severe("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
				if (!bySocket) {
					throw new SocketOperationException(getResultCancel());
				}
				return getResultCancel();
			}
			catch (final AOCertificatesNotFoundException e) {
				LOGGER.severe("No hay certificados validos en el almacen: " + e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_NO_CERTIFICATES_KEYSTORE;
				ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}
			catch (final Exception e) {
				LOGGER.severe("Error al mostrar el dialogo de seleccion de certificados: " + e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_ACCESS_KEYSTORE;
				ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}

		}

		// Concatenamos el certificado utilizado para firmar y la firma con un
		// separador para que la pagina pueda recuperar ambos
		byte[] certEncoded;
		try {
			certEncoded = pke.getCertificateChain()[0].getEncoded();
		} catch (final CertificateEncodingException e) {
			LOGGER.severe("Error en la decodificacion del certificado de firma: " + e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_DECODING_CERTIFICATE;
			ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}

		String dataToSend;
		if (options.getDesKey() != null) {
			try {
				// El CipherData devuelve los datos directamente en Base64
				dataToSend = CypherDataManager.cipherData(certEncoded, options.getDesKey());
			} catch (final Exception e) {
				LOGGER.severe("Error en el cifrado de los datos a enviar: " + e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_ENCRIPTING_DATA;
				ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}
		} else {
			LOGGER.warning(
					"Se omite el cifrado de los datos resultantes por no haberse proporcionado una clave de cifrado" //$NON-NLS-1$
			);
			dataToSend = Base64.encode(certEncoded, true);
		}

		if (options.getStorageServletUrl() != null) {
			// Detenemos la espera activa
			final Thread waitingThread = ProtocolInvocationLauncher.getActiveWaitingThread();
			if (waitingThread != null) {
				waitingThread.interrupt();
			}
			// Esperamos a que termine cualquier otro envio al servidor para que no se pisen
			synchronized (IntermediateServerUtil.getUniqueSemaphoreInstance()) {
				try {
					LOGGER.info("Enviamos el resultado de la operacion de seleccion de certificado al servidor intermedio"); //$NON-NLS-1$
					IntermediateServerUtil.sendData(dataToSend, options.getStorageServletUrl().toString(), options.getId());
				}
				catch (final Exception e) {
					LOGGER.log(Level.SEVERE, "Error al enviar los datos al servidor", e); //$NON-NLS-1$
					ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.ERROR_SENDING_RESULT, e);
					return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_SENDING_RESULT);
				}
			}
		}
		else {
			LOGGER.info(
				"Se omite el envio por red del resultado por no haberse proporcionado una URL de destino" //$NON-NLS-1$
			);
		}

		return dataToSend;
	}

	public static String getResultCancel() {
		return RESULT_CANCEL;
	}
}
