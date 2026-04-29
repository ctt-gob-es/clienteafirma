/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.padestri.client;

import java.io.IOException;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.util.Arrays;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.http.HttpError;
import es.gob.afirma.core.misc.http.SSLErrorProcessor;
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOTriphaseException;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseDataSigner;
import es.gob.afirma.core.signers.TriphaseUtil;
import es.gob.afirma.signers.pades.common.BadPdfPasswordException;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.pades.common.PdfFormModifiedException;
import es.gob.afirma.signers.pades.common.PdfHasUnregisteredSignaturesException;
import es.gob.afirma.signers.pades.common.PdfIsCertifiedException;
import es.gob.afirma.signers.pades.common.PdfIsPasswordProtectedException;
import es.gob.afirma.signers.pades.common.SuspectedPSAException;

/** Utilidades para el firmador PAdES en tres fases.
 * @author Tom&acute;s Garc&iacute;a-Mer&aacute;s */
final class PDFTriPhaseSignerUtil {

	private PDFTriPhaseSignerUtil() {
		// No instanciable
	}

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Identificador de la operacion de prefirma en servidor. */
	private static final String OPERATION_PRESIGN = "pre"; //$NON-NLS-1$

	/** Identificador de la operacion de postfirma en servidor. */
	private static final String OPERATION_POSTSIGN = "post"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro de c&oacute;digo de operaci&oacute;n en la URL de llamada al servidor de firma. */
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de firma. */
	private static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n criptogr&aacute;fica en la URL del servidor de firma. */
	private static final String PARAMETER_NAME_CRYPTO_OPERATION = "cop"; //$NON-NLS-1$

	private static final String HTTP_CGI = "?"; //$NON-NLS-1$
	private static final String HTTP_EQUALS = "="; //$NON-NLS-1$
	private static final String HTTP_AND = "&"; //$NON-NLS-1$

	// Parametros que necesitamos para la URL de las llamadas al servidor de firma
	private static final String PARAMETER_NAME_DOCID = "doc"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_ALGORITHM = "algo"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_FORMAT = "format"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_CERT = "cert"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_EXTRA_PARAM = "params"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_SESSION_DATA = "session"; //$NON-NLS-1$
	private static final String PARAMETER_SERVICE_TIMEOUT = "servicetimeout"; //$NON-NLS-1$

	private static final String PADES_FORMAT = "pades"; //$NON-NLS-1$


	/** Prefijo del mensaje de error del servicio de prefirma. */
	private static final String ERROR_PREFIX = "ERR-"; //$NON-NLS-1$
	/** Prefijo del mensaje de error cuando para completar la operaci&oacute;n se requiere intervenci&oacute;n del usuario. */
	private static final String CONFIG_NEEDED_ERROR_PREFIX = ERROR_PREFIX + "21:"; //$NON-NLS-1$

	/** Indicador de finalizaci&oacute;n correcta de proceso. */
	private static final String SUCCESS = "OK"; //$NON-NLS-1$

	static byte[] doPresign(final UrlHttpManager urlManager,
							final URL signServerUrl,
			                final String algorithm,
			                final Certificate[] certChain,
			                final String documentId,
			                final Properties extraParams) throws AOException {

		byte[] preSignResult;

		// Empezamos la prefirma
		try {
			// Llamamos a una URL pasando como parametros los datos necesarios para
			// configurar la operacion:
			//  - Operacion trifasica (prefirma o postfirma)
			//  - Operacion criptografica (firma, cofirma o contrafirma)
			//  - Formato de firma
			//  - Algoritmo de firma a utilizar
			//  - Certificado de firma
			//  - Parametros extra de configuracion
			//  - Datos o identificador del documento a firmar
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServerUrl).append(HTTP_CGI)
				.append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_PRESIGN).append(HTTP_AND)
				.append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(CRYPTO_OPERATION_SIGN).append(HTTP_AND)
				.append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(PADES_FORMAT).append(HTTP_AND)
				.append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(algorithm).append(HTTP_AND)
				.append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(TriphaseUtil.prepareCertChainParam(certChain, extraParams)).append(HTTP_AND)
				.append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(documentId);

			if (extraParams.size() > 0) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS)
					.append(AOUtil.properties2Base64(extraParams));
			}

			final SSLErrorProcessor errorProcessor = new SSLErrorProcessor(extraParams);
			try {
				preSignResult = urlManager.readUrl(
						urlBuffer.toString(), UrlHttpMethod.POST, errorProcessor);

			} catch (final IOException e) {
				if (errorProcessor.isCancelled()) {
					LOGGER.info(
							"El usuario no permite la importacion del certificado SSL de confianza del servicio de firma trifasica: " //$NON-NLS-1$
							+ LoggerUtil.getTrimStr(signServerUrl.toString()));
				}
				throw e;
			}
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante en la prefirma", e, ErrorCode.Internal.ENCODING_SIGNING_CERTIFICATE); //$NON-NLS-1$
		}
		catch (final HttpError e) {
			LOGGER.log(Level.WARNING, "El servicio de firma devolvio un error durante la prefirma: " + e, e); //$NON-NLS-1$
			final AOException ex;
			if (e.getResponseCode() == 400) {
				ex = new AOException("Error en los parametros enviados al servicio de prefirma", e, ErrorCode.Request.INVALID_PARAMS_TO_PRESIGN);  //$NON-NLS-1$
			}
			else if (e.getResponseCode() / 100 == 4) {
				ex = new AOException("Error en la comunicacion con el servicio de firma de prefirma", e, ErrorCode.Communication.PRESIGN_SERVICE_COMMUNICATION_ERROR);  //$NON-NLS-1$
			}
			else {
				ex = new AOException("El servicio de prefirma devolvio un error", e, ErrorCode.ThirdParty.PRESIGN_HTTP_ERROR);  //$NON-NLS-1$
			}
			throw ex;
		}
		catch (final SocketTimeoutException e) {
			throw new AOException("La prefirma en servidor ha excedido el tiempo de espera", e, ErrorCode.Communication.PRESIGN_SERVICE_TIMEOUT); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("No se ha podido conectar con el servicio de prefirma", e, ErrorCode.Communication.PRESIGN_SERVICE_CONNECTION_ERROR); //$NON-NLS-1$
		}


		// Comprobamos que no se trate de un error
		if (preSignResult.length <= 8) {
			final String msg = new String(preSignResult, StandardCharsets.UTF_8);
			LOGGER.warning("No se han obtenido datos de la prefirma: " + msg); //$NON-NLS-1$
			throw new AOException("No se han obtenido datos de la prefirma", ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE); //$NON-NLS-1$
		}

		final String headMsg = new String(Arrays.copyOf(preSignResult, 8), StandardCharsets.UTF_8);
		if (headMsg.startsWith(ERROR_PREFIX)) {
			final String msg = new String(preSignResult, StandardCharsets.UTF_8);
			LOGGER.warning("Error durante la prefirma: " + msg); //$NON-NLS-1$
			throw buildInternalException(msg, extraParams, true);
		}

		return preSignResult;
	}

	static byte[] doSign(final byte[] preSignResult,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) throws AOException {

		// Convertimos la respuesta del servidor en un objeto TriphaseData
		final TriphaseData triphaseData;
		try {
			triphaseData = TriphaseData.parser(Base64.decode(preSignResult, 0, preSignResult.length, true));
		}
		catch (final Exception e) {
			LOGGER.severe("Error al analizar la prefirma enviada por el servidor: " + e); //$NON-NLS-1$
			throw new AOException("Error al analizar la prefirma enviada por el servidor", e, ErrorCode.ThirdParty.MALFORMED_PRESIGN_RESPONSE); //$NON-NLS-1$
		}

		return TriphaseDataSigner.doSign(
			new AOPkcs1Signer(),
			algorithm,
			key,
			certChain,
			triphaseData,
			extraParams
		).toString().getBytes();
	}

	static byte[] doPostSign(final UrlHttpManager urlManager,
							 final String preResultAsBase64,
			                 final URL signServerUrl,
                             final String algorithm,
                             final Certificate[] certChain,
                             final String documentId,
                             final Properties extraParams) throws AOException {
		byte[] postSignResult;
		try {
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServerUrl).append(HTTP_CGI)
				.append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_POSTSIGN).append(HTTP_AND)
				.append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(CRYPTO_OPERATION_SIGN).append(HTTP_AND)
				.append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(PADES_FORMAT).append(HTTP_AND)
				.append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(algorithm).append(HTTP_AND)
				.append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(TriphaseUtil.prepareCertChainParam(certChain, extraParams))
				.append(HTTP_AND).append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(documentId)
				.append(HTTP_AND).append(PARAMETER_NAME_SESSION_DATA).append(HTTP_EQUALS).append(preResultAsBase64);

			if (extraParams.size() > 0) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS)
					.append(AOUtil.properties2Base64(extraParams));
			}

			final SSLErrorProcessor errorProcessor = new SSLErrorProcessor(extraParams);
			try {
				postSignResult = urlManager.readUrl(
						urlBuffer.toString(), UrlHttpMethod.POST, errorProcessor);
			} catch (final IOException e) {
				if (errorProcessor.isCancelled()) {
					LOGGER.info(
							"El usuario no permite la importacion del certificado SSL de confianza del servicio de firma trifasica: " //$NON-NLS-1$
							+ LoggerUtil.getTrimStr(signServerUrl.toString()));
				}
				throw e;
			}

			urlBuffer.setLength(0);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante en la postfirma", e, ErrorCode.Internal.ENCODING_SIGNING_CERTIFICATE); //$NON-NLS-1$
		}
		catch (final HttpError e) {
			LOGGER.log(Level.WARNING, "El servicio de firma devolvio un error durante la postfirma: " + e, e); //$NON-NLS-1$
			final AOException ex;
			if (e.getResponseCode() == 400) {
				ex = new AOException("Error en los parametros enviados al servicio de postfirma", e, ErrorCode.Request.INVALID_PARAMS_TO_POSTSIGN);  //$NON-NLS-1$
			}
			else if (e.getResponseCode() / 100 == 4) {
				ex = new AOException("Error en la comunicacion con el servicio de firma de postfirma", e, ErrorCode.Communication.POSTSIGN_SERVICE_COMMUNICATION_ERROR);  //$NON-NLS-1$
			}
			else {
				ex = new AOException("El servicio de postfirma devolvio un error", e, ErrorCode.ThirdParty.POSTSIGN_HTTP_ERROR);  //$NON-NLS-1$
			}
			throw ex;
		}
		catch (final SocketTimeoutException e) {
			throw new AOException("La postfirma en servidor ha excedido el tiempo de espera", e, ErrorCode.Communication.POSTSIGN_SERVICE_TIMEOUT); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("No se ha podido conectar con el servicio de postfirma", e, ErrorCode.Communication.POSTSIGN_SERVICE_CONNECTION_ERROR); //$NON-NLS-1$
		}

		// Comprobamos que no se trate de un error
		if (postSignResult.length <= 8) {
			final String msg = new String(postSignResult, StandardCharsets.UTF_8);
			LOGGER.warning("No se han obtenido datos de la postfirma: " + msg); //$NON-NLS-1$
			throw new AOException("No se han obtenido datos de la postfirma", ErrorCode.ThirdParty.MALFORMED_POSTSIGN_RESPONSE); //$NON-NLS-1$
		}
		final String headPostMsg = new String(Arrays.copyOf(postSignResult, 8), StandardCharsets.UTF_8);
		if (headPostMsg.startsWith(ERROR_PREFIX)) {
			final String msg = new String(postSignResult, StandardCharsets.UTF_8);
			LOGGER.warning("Error durante la prostirma: " + msg); //$NON-NLS-1$
			throw buildInternalException(msg, extraParams, false);
		}

		// Analizamos la respuesta del servidor
		final String stringTrimmedResult = new String(postSignResult).trim();
		if (!stringTrimmedResult.startsWith(SUCCESS)) {
			throw new AOException("La firma trifasica no ha finalizado correctamente: " + stringTrimmedResult, ErrorCode.ThirdParty.POSTSIGN_ERROR); //$NON-NLS-1$
		}

		// Los datos no se devuelven, se quedan en el servidor
		try {
			return Base64.decode(stringTrimmedResult.substring((SUCCESS + " NEWID=").length()), true); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.warning("El resultado de NEWID del servidor no estaba en Base64: " + e); //$NON-NLS-1$
			throw new AOException("El resultado devuelto por el servidor no es correcto", e, ErrorCode.ThirdParty.MALFORMED_POSTSIGN_RESPONSE); //$NON-NLS-1$
		}
	}

	/**
	 * Construye una excepci&oacute;n a partir del mensaje interno de error
	 * notificado por el servidor trif&aacute;sico.
	 * @param msg Mensaje de error devuelto por el servidor trif&aacute;sico.
	 * @return Excepci&oacute;n construida.
	 */
	private static AOException buildInternalException(final String msg, final Properties extraParams, final boolean presign) {

		AOException exception = null;
		final int separatorPos = msg.indexOf(":"); //$NON-NLS-1$
		if (msg.startsWith(CONFIG_NEEDED_ERROR_PREFIX)) {
			final int separatorPos2 = msg.indexOf(":", separatorPos + 1); //$NON-NLS-1$
			final String errorCode = msg.substring(separatorPos + 1, separatorPos2);
			final String errorMsg = msg.substring(separatorPos2 + 1);
			if (PdfIsCertifiedException.REQUESTOR_MSG_CODE.equals(errorCode)) {
				exception = new PdfIsCertifiedException(errorMsg);
			}
			else if (PdfHasUnregisteredSignaturesException.REQUESTOR_MSG_CODE.equals(errorCode)) {
				exception =  new PdfHasUnregisteredSignaturesException(errorMsg);
			}
			else if (PdfFormModifiedException.REQUESTOR_MSG_CODE.equals(errorCode)) {
				exception =  new PdfFormModifiedException(errorMsg);
			}
			else if (SuspectedPSAException.REQUESTOR_MSG_CODE.equals(errorCode)) {
				exception =  new SuspectedPSAException(errorMsg);
			}
			else if (PdfIsPasswordProtectedException.REQUESTOR_MSG_CODE.equals(errorCode)
					|| BadPdfPasswordException.REQUESTOR_MSG_CODE.equals(errorCode)) {
				if (extraParams != null && (extraParams.containsKey(PdfExtraParams.OWNER_PASSWORD_STRING)
						|| extraParams.containsKey(PdfExtraParams.USER_PASSWORD_STRING))) {
					exception = new BadPdfPasswordException(errorMsg);
				}
				else {
					exception = new PdfIsPasswordProtectedException(errorMsg);
				}
			}
		}

		if (exception == null) {
			final String triphaseErrorCode = msg.substring(0, separatorPos);
			String exceptionClassName = null;
			String intMessage = null;
			final int internalExceptionPos = msg.indexOf(":", separatorPos + 1); //$NON-NLS-1$
			if (internalExceptionPos > 0) {
				final int internalExceptionLimitPos = msg.indexOf(":", internalExceptionPos + 1); //$NON-NLS-1$
				if (internalExceptionLimitPos > 0) {
					exceptionClassName = msg.substring(internalExceptionPos + 1, internalExceptionLimitPos).trim();
					intMessage = msg.substring(internalExceptionLimitPos + 1).trim();
				}
				else {
					exceptionClassName = msg.substring(internalExceptionPos + 1).trim();
					intMessage = msg.substring(separatorPos + 1, internalExceptionPos).trim();
				}
			}
			else {
				intMessage = msg.substring(separatorPos + 1).trim();
			}
			if (presign) {
				exception = AOTriphaseException.parsePresignException(triphaseErrorCode, intMessage, exceptionClassName);
			}
			else {
				exception = AOTriphaseException.parsePostsignException(triphaseErrorCode, intMessage, exceptionClassName);
			}
		}

		return exception;
	}
}

