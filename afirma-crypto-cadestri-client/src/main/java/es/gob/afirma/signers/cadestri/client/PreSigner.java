/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cadestri.client;

import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.HTTP_AND;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.HTTP_CGI;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.HTTP_EQUALS;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.OPERATION_PRESIGN;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_ALGORITHM;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_CERT;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_CRYPTO_OPERATION;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_DOCID;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_EXTRA_PARAM;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_FORMAT;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_OPERATION;

import java.io.IOException;
import java.net.URL;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.http.SSLErrorProcessor;
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.core.signers.TriphaseUtil;

final class PreSigner {

	private PreSigner() {
		// No instanciable
	}

	static byte[] preSign(final String format,
			              final String algorithm,
			              final Certificate[] certChain,
			              final String cryptoOperation,
			              final String documentId,
			              final UrlHttpManager urlManager,
			              final URL signServerUrl,
			              final Properties extraParams) throws CertificateEncodingException,
			                                                   IOException {

		// Llamamos a una URL pasando como parametros los datos necesarios para
		// configurar la operacion:
		//  - Operacion trifasica (prefirma o postfirma)
		//  - Operacion criptografica (firma, cofirma o contrafirma)
		//  - Formato de firma ("CAdES" o "CAdES-ASiC-S" en este caso)
		//  - Algoritmo de firma a utilizar
		//  - Certificado de firma
		//  - Parametros extra de configuracion
		//  - Datos o identificador del documento a firmar
		final StringBuilder urlBuffer = new StringBuilder();
		urlBuffer.append(signServerUrl).append(HTTP_CGI).
		append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_PRESIGN).append(HTTP_AND).
		append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(cryptoOperation).append(HTTP_AND).
		append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(format).append(HTTP_AND).
		append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(algorithm).append(HTTP_AND).
		append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(TriphaseUtil.prepareCertChainParam(certChain, extraParams)).append(HTTP_AND).
		append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(documentId);

		// Estos parametros se pasan por URL, se quitan aqui para evitar un doble proceso
		extraParams.remove("serverUrl"); //$NON-NLS-1$
		extraParams.remove("documentId"); //$NON-NLS-1$

		if (extraParams.size() > 0) {
			urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS).
			append(AOUtil.properties2Base64(extraParams));
		}

		byte[] data;

		final SSLErrorProcessor errorProcessor = new SSLErrorProcessor(extraParams);
		try {
			data = urlManager.readUrl(urlBuffer.toString(), UrlHttpMethod.POST, errorProcessor);
		} catch (final IOException e) {
			if (errorProcessor.isCancelled()) {
				Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
						"El usuario no permite la importacion del certificado SSL de confianza del servicio de firma trifasica: " //$NON-NLS-1$
						+ LoggerUtil.getTrimStr(signServerUrl.toString()));
			}
			throw e;
		}

		return data;
	}
}
