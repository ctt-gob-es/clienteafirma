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
import java.net.URL;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.http.SSLErrorProcessor;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseDataSigner;
import es.gob.afirma.core.signers.TriphaseUtil;

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

	private static final String PADES_FORMAT = "pades"; //$NON-NLS-1$

	/** Indicador de finalizaci&oacute;n correcta de proceso. */
	private static final String SUCCESS = "OK"; //$NON-NLS-1$

	static byte[] doPresign(final URL signServerUrl,
			                final String algorithm,
			                final Certificate[] certChain,
			                final String documentId,
			                final Properties extraParams) throws AOException {

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

			byte[] data;
			final SSLErrorProcessor errorProcessor = new SSLErrorProcessor(extraParams);
			try {
				data = UrlHttpManagerFactory.getInstalledManager().readUrl(
						urlBuffer.toString(), UrlHttpMethod.POST, errorProcessor);

			} catch (final IOException e) {
				if (errorProcessor.isCancelled()) {
					LOGGER.info(
							"El usuario no permite la importacion del certificado SSL de confianza del servicio de firma trifasica: " //$NON-NLS-1$
							+ LoggerUtil.getTrimStr(signServerUrl.toString()));
				}
				throw new AOException("Error en la llamada de prefirma al servidor: " + e, e); //$NON-NLS-1$
			}

			return data;
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante: " + e, e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de prefirma al servidor: " + e, e); //$NON-NLS-1$
		}
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
			throw new AOException("Error al analizar la prefirma enviada por el servidor", e); //$NON-NLS-1$
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

	static byte[] doPostSign(final String preResultAsBase64,
			                 final URL signServerUrl,
                             final String algorithm,
                             final Certificate[] certChain,
                             final String documentId,
                             final Properties extraParams) throws AOException {
		byte[] triSignFinalResult;
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
				triSignFinalResult = UrlHttpManagerFactory.getInstalledManager().readUrl(
						urlBuffer.toString(), UrlHttpMethod.POST, errorProcessor);
			} catch (final IOException e) {
				if (errorProcessor.isCancelled()) {
					LOGGER.info(
							"El usuario no permite la importacion del certificado SSL de confianza del servicio de firma trifasica: " //$NON-NLS-1$
							+ LoggerUtil.getTrimStr(signServerUrl.toString()));
				}
				throw new AOException("Error en la llamada de postfirma al servidor: " + e, e); //$NON-NLS-1$
			}

			urlBuffer.setLength(0);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante: " + e, e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de postfirma al servidor: " + e, e); //$NON-NLS-1$
		}

		// Analizamos la respuesta del servidor
		final String stringTrimmedResult = new String(triSignFinalResult).trim();
		if (!stringTrimmedResult.startsWith(SUCCESS)) {
			throw new AOException("La firma trifasica no ha finalizado correctamente: " + new String(triSignFinalResult)); //$NON-NLS-1$
		}

		// Se devuelve la respuesta del servidor sin la cabecera
		try {
			return Base64.decode(stringTrimmedResult.substring((SUCCESS + " NEWID=").length()), true); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.warning("El resultado de NEWID del servidor no estaba en Base64: " + e); //$NON-NLS-1$
			throw new AOException("El resultado devuelto por el servidor no es correcto", e); //$NON-NLS-1$
		}

	}

}
