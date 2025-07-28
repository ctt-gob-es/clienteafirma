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

import es.gob.afirma.signers.batch.BatchConfigManager;
import es.gob.afirma.signers.batch.xml.SignBatch;
import es.gob.afirma.signers.batch.xml.SignBatchConcurrent;
import es.gob.afirma.signers.batch.xml.SignBatchSerial;
import es.gob.afirma.signers.xml.XmlDSigProviderHelper;
import es.gob.afirma.triphase.server.ConfigManager;

/** Realiza la primera fase de un proceso de firma por lote.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class BatchPresigner extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String BATCH_XML_PARAM = "xml"; //$NON-NLS-1$
	private static final String BATCH_CRT_PARAM = "certs"; //$NON-NLS-1$

	private static final Charset DEFAULT_CHARSET = StandardCharsets.UTF_8;

	static {
		// Indicamos si se debe instalar el proveedor de firma XML de Apache
		XmlDSigProviderHelper.configureXmlDSigProvider();
	}

	/** Realiza la primera fase de un proceso de firma por lote.
	 * Debe recibir la definici&oacute;n del lote en un XML (<a href="../doc-files/batch-scheme.html">descripci&oacute;n
	 * del formato</a>) convertido completamente
	 * en Base64 y la cadena de certificados del firmante, convertidos a Base64 (puede ser
	 * <i>URL Safe</i>) y separados por punto y coma (<code>;</code>).
	 * Devuelve un XML de sesi&oacute;n trif&aacute;sica.
	 * @see HttpServlet#service(HttpServletRequest request, HttpServletResponse response) */
	@Override
	protected void service(final HttpServletRequest request,
			               final HttpServletResponse response) throws ServletException,
			                                                          IOException {

		// Si no se ha podido cargar el fichero de configuracion de la firma de lotes XML, se considera que
		// el servicio no esta inicializado
		if (!BatchConfigManager.isInitialized()) {
			LOGGER.severe("No se ha inicializado el servicio"); //$NON-NLS-1$
			response.sendError(HttpServletResponse.SC_SERVICE_UNAVAILABLE, "No se ha inicializado el servicio"); //$NON-NLS-1$
			return;
		}

		final Map<String, String> parametes = RequestParameters.extractParameters(request);

		final String xml = parametes.get(BATCH_XML_PARAM);
		if (xml == null) {
			LOGGER.severe("No se ha recibido una definicion de lote en el parametro " + BATCH_XML_PARAM); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"No se ha recibido una definicion de lote en el parametro " + BATCH_XML_PARAM //$NON-NLS-1$
			);
			return;
		}

		final SignBatch batch;
		try {
			final byte[] batchConfig = BatchServerUtil.getSignBatchConfig(xml.getBytes(DEFAULT_CHARSET));
			batch = BatchConfigManager.isConcurrentMode() ?
					new SignBatchConcurrent(batchConfig) :
						new SignBatchSerial(batchConfig);
		}
		catch(final Exception e) {
			LOGGER.severe("La definicion de lote es invalida: " + e); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"La definicion de lote es invalida: " + e //$NON-NLS-1$
			);
			return;
		}

		final String certListUrlSafeBase64 = parametes.get(BATCH_CRT_PARAM);
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

		final String pre;
		try {
			pre = batch.doPreBatch(certs);
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
		response.setContentType("text/xml;charset=UTF-8"); //$NON-NLS-1$
		try (final PrintWriter writer = response.getWriter()) {
			writer.write(pre);
			writer.flush();
		}
	}
}
