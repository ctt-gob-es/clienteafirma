/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.nio.charset.StandardCharsets;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateEncodingException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.keystores.CertificateContext;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.http.HttpError;
import es.gob.afirma.core.misc.protocol.UrlParametersForBatch;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.filters.CertFilterManager;
import es.gob.afirma.keystores.filters.CertificateFilter;
import es.gob.afirma.signers.batch.client.BatchSigner;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.crypto.CypherDataManager;
import es.gob.afirma.standalone.so.macos.MacUtils;

final class ProtocolInvocationLauncherBatch {

	private static final char RESULT_SEPARATOR = '|';

	private static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ProtocolInvocationLauncherBatch() {
		// No instanciable
	}

	/** Procesa un lote de firma en invocaci&oacute;n por protocolo.
	 * @param options Par&aacute;metros de la operaci&oacute;n.
	 * @param protocolVersion Versi&oacute;n del protocolo de comunicaci&oacute;n.
	 * @param bySocket <code>true</code> para usar comunicaci&oacute;n por <i>socket</i> local,
	 *                 <code>false</code> para usar servidor intermedio.
	 * @return XML de respuesta del procesado.
	 * @throws SocketOperationException Si hay errores en la
	 *                                  comunicaci&oacute;n por <i>socket</i> local. */
	static String processBatch(final UrlParametersForBatch options,
			final int protocolVersion,
			final boolean bySocket) throws SocketOperationException {

        // Comprobamos si soportamos la version del protocolo indicada
		if (!ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED.support(protocolVersion)) {
			LOGGER.severe(String.format("Version de protocolo no soportada (%1s). Version actual: %s2. Hay que actualizar la aplicacion.", //$NON-NLS-1$
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

		final AOKeyStore aoks = AOKeyStore.getKeyStore(options.getDefaultKeyStore());
		if (aoks == null) {
			LOGGER.severe("No hay un KeyStore asociado al valor: " + options.getDefaultKeyStore()); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_FIND_KEYSTORE;
			ProtocolInvocationLauncherErrorManager.showError(errorCode);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}

		final CertFilterManager filterManager = new CertFilterManager(options.getExtraParams());
		final List<CertificateFilter> filters = filterManager.getFilters();
		final boolean mandatoryCertificate = filterManager.isMandatoryCertificate();
		final PrivateKeyEntry pke;

		if (options.getSticky() && !options.getResetSticky() && ProtocolInvocationLauncher.getStickyKeyEntry() != null) {

			LOGGER.info("Se usa Sticky Signature y tenemos valor de clave privada"); //$NON-NLS-1$
			pke = ProtocolInvocationLauncher.getStickyKeyEntry();

		} else {

			final String aoksLib = options.getDefaultKeyStoreLib();

			final PasswordCallback pwc = aoks.getStorePasswordCallback(null);
			final AOKeyStoreManager ksm;
			try {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
					aoks, // Store
					aoksLib, // Lib
					null, // Description
					pwc,  // PasswordCallback
					null  // Parent
				);
			}
			catch (final Exception e3) {
				LOGGER.severe("Error obteniendo el AOKeyStoreManager: " + e3); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_ACCESS_KEYSTORE;
    			ProtocolInvocationLauncherErrorManager.showError(errorCode, e3);
    			if (!bySocket){
    				throw new SocketOperationException(errorCode);
    			}
    			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}

			try {
				if (Platform.OS.MACOSX.equals(Platform.getOS())) {
					MacUtils.focusApplication();
				}
				final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
					ksm,
					null,
					true,
					true, // showExpiredCertificates
					true, // checkValidity
					filters,
					mandatoryCertificate
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
				if (!bySocket){
					throw new SocketOperationException(RESULT_CANCEL);
				}
				return RESULT_CANCEL;
			}
			catch(final AOCertificatesNotFoundException e) {
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

		String batchResult;
		try {
			if (options.isJsonBatch()) {
				if(options.isLocalBatchProcess()) {
					final BatchSignOperation batchConfig =
							JSONBatchManager.parseBatchConfig(options.getData());
					batchResult = LocalBatchSigner.signLocalBatch(batchConfig, pke);
				} else {
					batchResult = BatchSigner.signJSON(
							Base64.encode(options.getData(), true),
							options.getBatchPresignerUrl(),
							options.getBatchPostSignerUrl(),
							pke.getCertificateChain(),
							pke.getPrivateKey()
							);
				}
			} else {
				batchResult = BatchSigner.signXML(
						Base64.encode(options.getData(), true),
						options.getBatchPresignerUrl(),
						options.getBatchPostSignerUrl(),
						pke.getCertificateChain(),
						pke.getPrivateKey()
						);
			}
		}
		catch (final AOCancelledOperationException e) {
			LOGGER.info("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
			if (!bySocket){
				throw new SocketOperationException(RESULT_CANCEL);
			}
			return RESULT_CANCEL;
		}
		catch (final IllegalArgumentException e) {
			LOGGER.info("Los parametros de la peticion no eran validos: " + e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PARAMS;
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final CertificateEncodingException e) {
			LOGGER.info("Error en la codificacion del certificado: " + LoggerUtil.getTrimStr(e.toString())); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_DECODING_CERTIFICATE;
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final HttpError e) {
			String errorCode;
			if (e.getResponseCode() == 400) {
				errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PARAMS;
				LOGGER.severe("Error en los parametros enviados al servicio: " + e.toString());  //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
			}
			else if (e.getResponseCode() / 100 == 4) {
				errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CONTACT_BATCH_SERVICE;
				LOGGER.severe("Error en la comunicacion con el servicio de firma de lotes: " + e);//$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
			}
			else {
				errorCode = ProtocolInvocationLauncherErrorManager.ERROR_BATCH_SIGNATURE;
				LOGGER.severe("Error en el servicio de firma de lotes: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
			}

			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final AOException e) {
			LOGGER.info("Error durante la firma del lote: " + e); //$NON-NLS-1$
			final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_BATCH_SIGNATURE;
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error en el proceso del lote de firmas", e); //$NON-NLS-1$
			final String errorCode = options.isLocalBatchProcess()
					? ProtocolInvocationLauncherErrorManager.ERROR_LOCAL_BATCH_SIGN
					: ProtocolInvocationLauncherErrorManager.ERROR_BATCH_SIGNATURE;
			ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
			if (!bySocket){
				throw new SocketOperationException(errorCode);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
		}

		final StringBuilder result = new StringBuilder();

		// Si se nos ha indicado en la llamadada que devolvamos el certificado de firma, lo adjuntamos al resultado con un separador
		byte[] signingCertEncoded = null;
		if (options.isCertNeeded()) {
			try {
				signingCertEncoded = pke.getCertificate().getEncoded();
			} catch (final CertificateEncodingException e) {
				LOGGER.log(Level.SEVERE, "No se ha podido codificar el certificado de firma para su devolucion", e); //$NON-NLS-1$
					final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_DECODING_CERTIFICATE;
					ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
					if (!bySocket){
						throw new SocketOperationException(errorCode);
					}
					return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}
		}

		// Si hay clave de cifrado, ciframos
		if (options.getDesKey() != null) {
			try {
				result.append(CypherDataManager.cipherData(batchResult.getBytes(StandardCharsets.UTF_8), options.getDesKey()));
				if (signingCertEncoded != null) {
					result.append(RESULT_SEPARATOR)
						.append(CypherDataManager.cipherData(signingCertEncoded, options.getDesKey()));
				}
			}
			catch (final Exception e) {
				LOGGER.severe("Error en el cifrado de los datos a enviar: " + e); //$NON-NLS-1$
				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_ENCRIPTING_DATA;
				ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
				if (!bySocket){
					throw new SocketOperationException(errorCode);
				}
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
			}
		}
		else {
			LOGGER.warning(
				"Se omite el cifrado de los datos resultantes por no haberse proporcionado una clave de cifrado" //$NON-NLS-1$
			);
			result.append(Base64.encode(batchResult.getBytes(StandardCharsets.UTF_8)));
			if (signingCertEncoded != null) {
				result.append(RESULT_SEPARATOR).append(Base64.encode(signingCertEncoded));
			}
		}

		// Si hay servidor intermedio, se envia
		if (options.getStorageServletUrl() != null) {
			// Enviamos la firma cifrada al servicio remoto de intercambio y detenemos la espera
			// activa si se encontraba vigente
			synchronized (IntermediateServerUtil.getUniqueSemaphoreInstance()) {
				final Thread waitingThread = ProtocolInvocationLauncher.getActiveWaitingThread();
				if (waitingThread != null) {
					waitingThread.interrupt();
				}
				try {
					IntermediateServerUtil.sendData(result.toString(), options.getStorageServletUrl().toString(), options.getId());
				}
				catch (final Exception e) {
					LOGGER.log(Level.SEVERE, "Error al enviar los datos al servidor", e); //$NON-NLS-1$
					final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_SENDING_RESULT;
					ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
					if (!bySocket){
						throw new SocketOperationException(errorCode);
					}
					return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
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

	public static String getResultCancel() {
		return RESULT_CANCEL;
	}
}
