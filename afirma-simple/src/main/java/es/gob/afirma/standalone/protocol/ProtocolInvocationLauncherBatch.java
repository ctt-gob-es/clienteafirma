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
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateEncodingException;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.ciphers.ServerCipher;
import es.gob.afirma.ciphers.ServerCipherFactory;
import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOControlledException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.core.keystores.CertificateContext;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.keystores.LockedKeyStoreException;
import es.gob.afirma.core.keystores.PinException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.http.ConnectionConfig;
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.protocol.ParameterException;
import es.gob.afirma.core.misc.protocol.ProtocolVersion;
import es.gob.afirma.core.misc.protocol.UrlParametersForBatch;
import es.gob.afirma.core.prefs.KeyStorePreferencesManager;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.CertificateFilter;
import es.gob.afirma.keystores.KeyStoreErrorCode;
import es.gob.afirma.keystores.filters.CertFilterManager;
import es.gob.afirma.keystores.filters.EncodedCertificateFilter;
import es.gob.afirma.signers.batch.client.BatchSigner;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.SimpleErrorCode;
import es.gob.afirma.standalone.SimpleKeyStoreManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.so.macos.MacUtils;
import es.gob.afirma.standalone.ui.ProgressInfoDialogManager;

final class ProtocolInvocationLauncherBatch {

	private static final char RESULT_SEPARATOR = '|';

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ProtocolInvocationLauncherBatch() {
		// No instanciable
	}



	/** Procesa un lote de firma en invocaci&oacute;n por protocolo.
	 * @param options Par&aacute;metros de la operaci&oacute;n.
	 * @param protocolVersion Versi&oacute;n del protocolo de comunicaci&oacute;n.
	 * @return XML de respuesta del procesado.
	 * @throws SocketOperationException Si hay errores en la
	 *                                  comunicaci&oacute;n por <i>socket</i> local. */
	static String processBatch(final UrlParametersForBatch options,
			final ProtocolVersion protocolVersion) throws SocketOperationException {

        // Comprobamos si soportamos la version del protocolo indicada
		if (!ProtocolInvocationLauncher.isCompatibleWith(protocolVersion)) {
			LOGGER.severe(String.format("Version de protocolo no soportada (%1s). Hay que actualizar la aplicacion.", //$NON-NLS-1$
					protocolVersion.toString()));
			throw new SocketOperationException(SimpleErrorCode.Request.UNSUPPORTED_PROTOCOL_VERSION);
		}

		// Comprobamos si se exige una version minima del Cliente
        if (options.getMinimumClientVersion() != null) {
        	final String minimumRequestedVersion = options.getMinimumClientVersion();
        	final Version requestedVersion = new Version(minimumRequestedVersion);
        	if (requestedVersion.greaterThan(SimpleAfirma.getVersion())) {
   				throw new SocketOperationException(SimpleErrorCode.Functional.MINIMUM_VERSION_NON_SATISTIED);
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
				aoks = SimpleKeyStoreManager.getKeyStore(defaultStore, true);
			}
		}
		// Si no, si en la llamada se definio el almacen que se debia usar, lo usamos
		else {
			aoks = SimpleKeyStoreManager.getKeyStore(options.getDefaultKeyStore(), true);
		}

		// Si aun no se ha definido el almacen, se usara el por defecto para el sistema operativo
		if (aoks == null) {
			aoks = AOKeyStore.getDefaultKeyStoreTypeByOs(Platform.getOS());
		}

		final CertFilterManager filterManager = new CertFilterManager(options.getExtraParams());

		SignOperationResult operationResult;
		try {
			operationResult = sign(options, aoks, useDefaultStore, filterManager, protocolVersion);
		}
		catch (final AOCancelledOperationException e) {
			ProgressInfoDialogManager.hideProgressDialog();
			throw e;
		}
		catch (final SocketOperationException e) {
			ProgressInfoDialogManager.hideProgressDialog();
			throw e;
		}

		final StringBuilder result = new StringBuilder();

		// Si se nos ha indicado en la llamadada que devolvamos el certificado de firma, lo adjuntamos al resultado con un separador
		byte[] signingCertEncoded = null;
		if (options.isCertNeeded()) {
			try {
				signingCertEncoded = operationResult.getPke().getCertificate().getEncoded();
			} catch (final CertificateEncodingException e) {
				LOGGER.log(Level.SEVERE, "No se ha podido codificar el certificado de firma para su devolucion", e); //$NON-NLS-1$
				throw new SocketOperationException(e, ErrorCode.Internal.ENCODING_SIGNING_CERTIFICATE);
			}
		}

		String cipheredBatchResult;
		String cipheredSigninCert = null;

		// Si hay configuracion de cifrado y somos compatibles, ciframos el resultado para la subida al servidor intermedio
		ServerCipher cipher = null;
		if (options.getCipherConfig()!= null) {
			try {
				cipher = ServerCipherFactory.newServerCipher(options.getCipherConfig());
			}
			catch (final Exception e) {
				LOGGER.severe("No se soporta la configuracion de cifrado proporcionada. Es posible que deba actualzar la aplicacion: " + e); //$NON-NLS-1$
				throw new SocketOperationException(e, SimpleErrorCode.Internal.ENCRIPTING_BATCH_RESULT);
			}
		}

		if (cipher != null) {
			try {
				cipheredBatchResult = cipher.cipherData(operationResult.getResult());
			}
			catch (final Exception e) {
				LOGGER.severe("Error en el cifrado del resultado del lote: " + e); //$NON-NLS-1$
				throw new SocketOperationException(e, SimpleErrorCode.Internal.ENCRIPTING_BATCH_RESULT);
			}

			if (signingCertEncoded != null) {
				try {
					cipheredSigninCert = cipher.cipherData(signingCertEncoded);
				}
				catch (final Exception e) {
					LOGGER.severe("Error en el cifrado de los datos a enviar: " + e); //$NON-NLS-1$
					throw new SocketOperationException(e, SimpleErrorCode.Internal.ENCRIPTING_BATCH_SIGNING_CERT);
				}
			}

			// El resultado es la concatenacion de ambas cadenas cifradas
			result.append(cipheredBatchResult);
			if (cipheredSigninCert != null) {
				result.append(RESULT_SEPARATOR).append(cipheredSigninCert);
			}

		}
		else {
			LOGGER.warning(
				"Se omite el cifrado de los datos resultantes por no haberse proporcionado una clave de cifrado" //$NON-NLS-1$
			);
			result.append(Base64.encode(operationResult.getResult()));
			if (signingCertEncoded != null) {
				result.append(RESULT_SEPARATOR).append(Base64.encode(signingCertEncoded));
			}
		}

		// Si hay servidor intermedio, se envia
		if (options.getStorageServletUrl() != null) {
			// Detenemos la espera activa
			final Thread waitingThread = ProtocolInvocationLauncher.getActiveWaitingThread();
			if (waitingThread != null) {
				waitingThread.interrupt();
			}
			// Esperamos a que termine cualquier otro envio al servidor para que no se pisen
			synchronized (IntermediateServerUtil.getUniqueSemaphoreInstance()) {
				try {
					LOGGER.info("Enviamos el resultado de la operacion de firma de lote al servidor intermedio"); //$NON-NLS-1$
					IntermediateServerUtil.sendData(result, options.getStorageServletUrl().toString(), options.getId());
				}
				catch (final Exception e) {
					LOGGER.log(Level.SEVERE, "Error al enviar los datos al servidor", e); //$NON-NLS-1$
					throw new SocketOperationException(e, SimpleErrorCode.Communication.SENDING_RESULT_OPERATION);
				}
			}
		}
		else {
			LOGGER.info(
				"Se omite el envio por red de los datos resultantes por no haberse proporcionado una URL de destino" //$NON-NLS-1$
			);
		}

		return result.toString();
	}

	private static SignOperationResult sign(final UrlParametersForBatch options, final AOKeyStore aoks, final boolean useDefaultStore,
			final CertFilterManager filterManager, final ProtocolVersion protocolVersion)
			throws AOCancelledOperationException, SocketOperationException {

		final PrivateKeyEntry pke;
		if (options.getSticky() && !options.getResetSticky() && ProtocolInvocationLauncher.getStickyKeyEntry() != null) {

			LOGGER.info("Se usa Sticky Signature y tenemos valor de clave privada"); //$NON-NLS-1$
			pke = ProtocolInvocationLauncher.getStickyKeyEntry();

		} else {

			final String aoksLib;
			if (useDefaultStore && (AOKeyStore.PKCS12.equals(aoks) || AOKeyStore.PKCS11.equals(aoks))) {
				aoksLib = PreferencesManager.get(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH);
			} else {
				aoksLib = options.getDefaultKeyStoreLib();
			}

			final AOKeyStoreManager ksm;
			try {
				ksm = ProtocolInvocationLauncherUtil.getAOKeyStoreManager(aoks, aoksLib);
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
				throw e;
			}
			catch (final Exception e) {
				LOGGER.log(Level.SEVERE, "Error obteniendo el AOKeyStoreManager", e); //$NON-NLS-1$
    			final ErrorCode errorCode = e instanceof AOControlledException ? ((AOControlledException) e).getErrorCode() : KeyStoreErrorCode.Internal.LOADING_KEYSTORE_INTERNAL_ERROR;
				ProtocolInvocationLauncherErrorManager.showError(protocolVersion, errorCode);
				throw new SocketOperationException(e, errorCode);
			}

			try {
				if (Platform.OS.MACOSX.equals(Platform.getOS())) {
					MacUtils.focusApplication();
				}
				String libName = null;
				if (aoksLib != null) {
					final File file = new File(aoksLib);
					libName = file.getName();
				}
				ProgressInfoDialogManager.hideProgressDialog();
				final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
					ksm,
					null,
					true,
					true, // showExpiredCertificates
					true, // checkValidity
					filterManager.getFilters(),
					filterManager.isMandatoryCertificate(),
					libName,
					true
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
				LOGGER.info("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
				throw e;
			}
			catch(final AOCertificatesNotFoundException e) {
				LOGGER.severe("No hay certificados validos en el almacen: " + e); //$NON-NLS-1$
				final ErrorCode errorCode = SimpleErrorCode.Functional.NO_CERTS_FOUND_SIGNING_BATCH;
				ProtocolInvocationLauncherErrorManager.showError(protocolVersion, errorCode);
				throw new SocketOperationException(errorCode);
			}
			catch (final Exception e) {
				LOGGER.severe("Error al mostrar el dialogo de seleccion de certificados: " + e); //$NON-NLS-1$
    			final ErrorCode errorCode = e instanceof AOControlledException ? ((AOControlledException) e).getErrorCode() : KeyStoreErrorCode.Internal.LOADING_KEYSTORE_INTERNAL_ERROR;
				ProtocolInvocationLauncherErrorManager.showError(protocolVersion, errorCode);
				throw new SocketOperationException(errorCode);
			}
		}

		final byte[] batchResult;
		try {
			// Si debe ser una operacion sin interfaz grafica, omitimos el dialogo de espera de firma
			if (!Boolean.parseBoolean(options.getExtraParams().getProperty(AfirmaExtraParams.HEADLESS))) {
				ProgressInfoDialogManager.showProgressDialog(SimpleAfirmaMessages.getString("ProgressInfoDialog.1")); //$NON-NLS-1$
			}
			batchResult = signBatch(options, pke);
		}
		catch (final PinException e) {
			// Si falla la operacion por culpa del PIN, configuramos el uso del mismo certificado, pero obligamos al
			// almacen a cargarse de nuevo
			List<CertificateFilter> filters;
			try {
				final byte[] certEncoded = pke.getCertificate().getEncoded();
				final CertificateFilter filter = new EncodedCertificateFilter(Base64.encode(certEncoded));
				filters = Collections.singletonList(filter);
			}
			catch (final Exception ex) {
				filters = null;
			}
			final CertFilterManager newFilterManager = new CertFilterManager(filters, filters != null, true);
			ProtocolInvocationLauncher.setStickyKeyEntry(null);
			return sign(options, aoks, useDefaultStore, newFilterManager, protocolVersion);
		}
		catch (final AOCancelledOperationException e) {
			LOGGER.info("Operacion cancelada por el usuario: " + LoggerUtil.getTrimStr(e.toString())); //$NON-NLS-1$
			throw e;
		}
		catch (final IllegalArgumentException e) {
			LOGGER.log(Level.SEVERE, "Alguno de los parametros de firma del lote es invalido o incompatible", e); //$NON-NLS-1$
			final ErrorCode errorCode = SimpleErrorCode.Request.INVALID_FORMAT_SIGN_BATCH_PARAM;
			throw new SocketOperationException(e, errorCode);
		}
		catch (final CertificateEncodingException e) {
			LOGGER.info("Error en la codificacion del certificado: " + LoggerUtil.getTrimStr(e.toString())); //$NON-NLS-1$
			final ErrorCode errorCode = ErrorCode.Internal.ENCODING_SIGNING_CERTIFICATE;
			throw new SocketOperationException(e, errorCode);
		}
		catch (final LockedKeyStoreException e) {
			LOGGER.info("El almacen de claves esta bloqueado: " + LoggerUtil.getTrimStr(e.toString())); //$NON-NLS-1$
			// En este caso no dejamos prefijado el certificado
			ProtocolInvocationLauncher.setStickyKeyEntry(null);
			throw new SocketOperationException(e);
		}
		catch (final AOException e) {
			LOGGER.info("Error durante la firma del lote: " + e); //$NON-NLS-1$
			throw new SocketOperationException(e);
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error en el proceso del lote de firmas", e); //$NON-NLS-1$
			final ErrorCode errorCode = options.isLocalBatchProcess()
					? SimpleErrorCode.Internal.INTERNAL_LOCAL_BATCH_ERROR
					: options.isJsonBatch()
						? SimpleErrorCode.Internal.INTERNAL_JSON_BATCH_ERROR
						: SimpleErrorCode.Internal.INTERNAL_XML_BATCH_ERROR;
			ProtocolInvocationLauncherErrorManager.showError(protocolVersion, errorCode);
			throw new SocketOperationException(e, errorCode);
		} finally {
			ProgressInfoDialogManager.hideProgressDialog();
		}

		return new SignOperationResult(batchResult, pke);
	}

	private static byte[] signBatch(final UrlParametersForBatch options, final PrivateKeyEntry pke)
			throws AOCancelledOperationException, PinException, IllegalArgumentException,
			CertificateEncodingException, AOException, ParameterException, IOException {

		String batchResult;

		ConnectionConfig connectionConfig = null;
		final int serviceTimeout = options.getServiceTimeout();

		if (serviceTimeout >= 0) {
			connectionConfig = new ConnectionConfig();
			connectionConfig.setReadTimeout(serviceTimeout);
		}

		try {
			SimpleAfirma.getSSLContextConfigurationTask().join();
		} catch (final InterruptedException e) {
			LOGGER.log(Level.SEVERE, "No se ha podido configurar el contexto SSL correctamente: ", e); //$NON-NLS-1$
		}

		final UrlHttpManager urlHttpManager = ProtocolInvocationLauncherUtil.getConfiguredHttpConnection(connectionConfig);

		if (options.isJsonBatch()) {
			if(options.isLocalBatchProcess()) {
				final BatchSignOperation batchConfig =
						JSONBatchManager.parseBatchConfig(options.getData());
				batchResult = LocalBatchSigner.signLocalBatch(batchConfig, pke);
			} else {
				batchResult = BatchSigner.signJSON(
						options.getData(),
						options.getBatchPresignerUrl(),
						options.getBatchPostSignerUrl(),
						pke.getCertificateChain(),
						pke.getPrivateKey(),
						options.getExtraParams(),
						urlHttpManager);
			}
		} else {
			batchResult = BatchSigner.signXML(
					options.getData(),
					options.getBatchPresignerUrl(),
					options.getBatchPostSignerUrl(),
					pke.getCertificateChain(),
					pke.getPrivateKey(),
					options.getExtraParams(),
					urlHttpManager);
		}
		return batchResult.getBytes(StandardCharsets.UTF_8);
	}
}
