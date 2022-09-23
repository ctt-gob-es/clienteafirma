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
import java.security.cert.X509Certificate;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.batch.json.JSONSignBatch;
import es.gob.afirma.signers.batch.json.JSONSignBatchConcurrent;
import es.gob.afirma.signers.batch.json.JSONSignBatchSerial;
import es.gob.afirma.signers.xml.XmlDSigProviderHelper;
import es.gob.afirma.triphase.server.ConfigManager;

/** Realiza la primera fase de un proceso de firma por lote. */
public final class JSONBatchPresigner extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String BATCH_JSON_PARAM = "json"; //$NON-NLS-1$
	private static final String BATCH_CRT_PARAM = "certs"; //$NON-NLS-1$

	static {
		// Indicamos si se debe instalar el proveedor de firma XML de Apache
		XmlDSigProviderHelper.configureXmlDSigProvider();
	}

	/**
	 * Realiza la primera fase de un proceso de firma por lote.
	 * Debe recibir la definici&oacute;n del lote en un JSON (<a href="../doc-files/batch-scheme.html">descripci&oacute;n
	 * del formato</a>) convertido completamente
	 * en Base64 y la cadena de certificados del firmante, convertidos a Base64 (puede ser
	 * <i>URL Safe</i>) y separados por punto y coma (<code>;</code>).
	 * Devuelve un JSON de sesi&oacute;n trif&aacute;sica.
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
		catch(final SecurityException e) {
			LOGGER.severe("Se sobrepaso alguno de los limites de seguridad establecidos en servidor para los lotes: " + e); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"El lote de firma no cumple con los requisitos establecidos en servidor" //$NON-NLS-1$
			);
			return;
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

		final JSONObject jsonPreBatch;
		try {
			jsonPreBatch = batch.doPreBatch(certs);
		}
		catch(final Exception e) {
			LOGGER.log(Level.SEVERE, "Error en el preproceso del lote", e); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
				"Error en el preproceso del lote: " + e //$NON-NLS-1$
			);
			return;
		}

		final String allowOrigin = ConfigManager.getAccessControlAllowOrigin();
		response.setHeader("Access-Control-Allow-Origin", allowOrigin); //$NON-NLS-1$
		response.setContentType("application/json;charset=UTF-8"); //$NON-NLS-1$
		try (PrintWriter writer = response.getWriter()) {
			writer.write(jsonPreBatch.toString());
			writer.flush();
		}
	}
}
