/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server.javax;

import java.io.IOException;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import es.gob.afirma.triphase.server.BadRequestException;
import es.gob.afirma.triphase.server.ConfigManager;
import es.gob.afirma.triphase.server.JSONBatchPresignerHandler;
import es.gob.afirma.triphase.server.ServletResponseException;

/** Realiza la primera fase de un proceso de firma por lote. */
public final class JSONBatchPresigner extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");
	
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

		// Extraemos los parametros de la peticion
		Map<String, String> parameters;
		try {
			parameters = RequestParameters.extractParameters(request);
		}
		catch (Exception e) {
			LOGGER.log(Level.WARNING, "No se puedieron extraer los parametros de la peticion", e);
			response.sendError(HttpServletResponse.SC_BAD_REQUEST, "No se pudieron extraer los parametros de la peticion");
			return;
		}

		// Procesamos la peticion
		byte[] result;
		try {
			result = JSONBatchPresignerHandler.processRequest(parameters);
		}
		catch (BadRequestException e) {
			LOGGER.log(Level.WARNING, "Los datos de la peticion no son validos", e);
			response.sendError(HttpServletResponse.SC_BAD_REQUEST, e.getMessage());
			return;
		}
		catch (ServletResponseException e) {
			LOGGER.log(Level.SEVERE, "Ocurrio un error al procesar la peticion", e);
			response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
			return;
		}
		
		// Configuramos el formato de la respuesta
		final String allowOrigin = ConfigManager.getAccessControlAllowOrigin();
		response.setHeader("Access-Control-Allow-Origin", allowOrigin); //$NON-NLS-1$
		response.setContentType("application/json;charset=UTF-8"); //$NON-NLS-1$
		
		// Enviamos la respuesta
		try (ServletOutputStream out = response.getOutputStream()) {
			out.write(result);
			out.flush();
		}
	}
}
