/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch.server;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.cert.X509Certificate;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.json.JSONSignBatch;
import es.gob.afirma.signers.batch.json.JSONSignBatchConcurrent;
import es.gob.afirma.signers.batch.json.JSONSignBatchSerial;
import es.gob.afirma.triphase.server.ConfigManager;

/**
 * Realiza la tercera (y &uacute;ltima) fase de un proceso de firma por lote.
 * Servlet implementation class BatchPostsigner
 */
public final class JSONBatchPostsigner extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String BATCH_JSON_PARAM = "json"; //$NON-NLS-1$
	private static final String BATCH_CRT_PARAM = "certs"; //$NON-NLS-1$
	private static final String BATCH_TRI_PARAM = "tridata"; //$NON-NLS-1$

	private static final Charset DEFAULT_CHARSET = StandardCharsets.UTF_8;

	/**
	 * Realiza la tercera y &uacute;ltima fase de un proceso de firma por lote.
	 * Debe recibir la definici&oacute;n JSON (<a href="../doc-files/batch-scheme.html">descripci&oacute;n
	 * del formato</a>) del lote (exactamente la misma enviada para la primera fase)
	 * en un JSON pero convertido a Base64 (puede ser en formato <i>URL Safe</i>) y la cadena de
	 * certificados del firmante (exactamente la misma que la enviada en la primera fase),
	 * convertidos a Base64 (puede ser <i>URL Safe</i>) y separados por punto y coma (<code>;</code>).<br>
	 * Devuelve un JSON de resumen de resultado (<a href="../doc-files/resultlog-scheme.html">descripci&oacute;n
	 * del formato</a>)
	 * @see HttpServlet#service(HttpServletRequest request, HttpServletResponse response)
	 * */
	@Override
	protected void service(final HttpServletRequest request,
			               final HttpServletResponse response) throws ServletException,
			                                                          IOException {

		final Map<String, String> parameters = RequestParameters.extractParameters(request);

		final String json = parameters.get(BATCH_JSON_PARAM);
		if (json == null) {
			LOGGER.severe("No se ha recibido una definicion de lote en el parametro " + BATCH_JSON_PARAM); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"No se ha recibido una definicion de lote en el parametro " + BATCH_JSON_PARAM //$NON-NLS-1$
			);
			return;
		}

		final JSONSignBatch batch;
		try {
			final byte[] jsonBatch = Base64.decode(json, true);
			batch = ConfigManager.isConcurrentModeEnable() ?
					new JSONSignBatchConcurrent(jsonBatch) :
						new JSONSignBatchSerial(jsonBatch);
		}
		catch(final Exception e) {
			LOGGER.severe("La definicion de lote es invalida: " + e); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"La definicion de lote es invalida: " + e //$NON-NLS-1$
			);
			return;
		}

		final String certListUrlSafeBase64 = parameters.get(BATCH_CRT_PARAM);
		if (certListUrlSafeBase64 == null) {
			LOGGER.severe("No se ha recibido la cadena de certificados del firmante en el parametro " + BATCH_CRT_PARAM); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"No se ha recibido la cadena de certificados del firmante en el parametro " + BATCH_CRT_PARAM //$NON-NLS-1$
			);
			return;
		}

		final X509Certificate[] certs;
		try {
			certs = BatchServerUtil.getCertificates(certListUrlSafeBase64);
		}
		catch (final Exception e) {
			LOGGER.severe("La cadena de certificados del firmante es invalida: " + e); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"La cadena de certificados del firmante es invalida: " + e //$NON-NLS-1$
			);
			return;
		}

		final String triphaseDataAsUrlSafeBase64 = parameters.get(BATCH_TRI_PARAM);
		if (triphaseDataAsUrlSafeBase64 == null) {
			LOGGER.severe("No se ha recibido el resultado de las firmas cliente en el parametro " + BATCH_TRI_PARAM); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"No se ha recibido el resultado de las firmas cliente en el parametro " + BATCH_TRI_PARAM //$NON-NLS-1$
			);
			return;
		}

		final TriphaseData td;
		try {
			td = BatchServerUtil.getTriphaseDataFromJSON(triphaseDataAsUrlSafeBase64.getBytes(DEFAULT_CHARSET));
		}
		catch(final Exception e) {
			LOGGER.severe("El JSON de firmas cliente es invalido: " + e); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"El JSON de firmas cliente es invalido: " + e //$NON-NLS-1$
			);
			return;
		}

		final String ret;
		try {
			ret = batch.doPostBatch(certs, td);
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error en el postproceso del lote", e); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
				"Error en el postproceso del lote: " + e //$NON-NLS-1$
			);
			return;
		}

		final String allowOrigin = ConfigManager.getAccessControlAllowOrigin();
		response.setHeader("Access-Control-Allow-Origin", allowOrigin); //$NON-NLS-1$
		response.setContentType("application/json;charset=UTF-8"); //$NON-NLS-1$
		try (
			final PrintWriter writer = response.getWriter();
		) {
			writer.write(ret);
			writer.flush();
		}
	}

}
