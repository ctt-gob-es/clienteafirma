/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server.jakarta;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.triphase.server.ConfigManager;
import es.gob.afirma.triphase.server.SignatureServiceHandler;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/** Servicio de firma electr&oacute;nica en 3 fases. */
public final class SignatureService extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger LOGGER = Logger.getLogger(ConfigManager.LOGGER_NAME);

	private static final String URL_DEFAULT_CHARSET = StandardCharsets.UTF_8.displayName();

	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) {

		LOGGER.info("== INICIO FIRMA TRIFASICA =="); //$NON-NLS-1$


		// Extraemos los parametros de la peticion
		Map<String, String> parameters;
		try (InputStream is = request.getInputStream()) {
			parameters = extractParameters(is);
			
		}
		catch (final Exception | Error e) {
			LOGGER.severe("No se pudieron leer los parametros de la peticion: " + e); //$NON-NLS-1$
			try {
				response.sendError(HttpServletResponse.SC_BAD_REQUEST);
			} catch (final IOException e1) {
				LOGGER.log(Level.SEVERE, "No se pudo enviar un error al cliente", e); //$NON-NLS-1$
			}
			return;
		}

		
		// Comfiguramos el formato de la respuesta
		final String allowOrigin = ConfigManager.getAccessControlAllowOrigin();
		response.setHeader("Access-Control-Allow-Origin", allowOrigin); //$NON-NLS-1$
		response.setContentType("text/plain"); //$NON-NLS-1$
		response.setCharacterEncoding("utf-8"); //$NON-NLS-1$
		
		// Abrimos el escritor para la respuesta y procesamos la peticion
		try (
			final PrintWriter out = response.getWriter();
		) {
			SignatureServiceHandler.processRequest(parameters, out);
		}
        catch (final Exception e) {
        	LOGGER.log(Level.SEVERE, "No se pudo contestar a la peticion", e); //$NON-NLS-1$
        	try {
				response.sendError(HttpURLConnection.HTTP_INTERNAL_ERROR, "No se pude contestar a la peticion: " + e); //$NON-NLS-1$
			}
        	catch (final IOException e1) {
        		LOGGER.severe("No se pudo enviar un error HTTP 500: " + e1); //$NON-NLS-1$
			}
        	return;
        }
	}

	/**
	 * Extrae los parametros del payload de la aplicaci&oacute;n.
	 * @param is Entrada con el payload de la petici&oacute;n.
	 * @return Mapa con el nombre y valor de los par&aacute;metros extra&iacute;dos.
	 * @throws UnsupportedEncodingException
	 * @throws IOException
	 */
	private Map<String, String> extractParameters(InputStream is) throws UnsupportedEncodingException, IOException {

		Map<String, String> parameters = new HashMap<>();
		
		final String[] params = new String(AOUtil.getDataFromInputStream(is), URL_DEFAULT_CHARSET).split("&"); //$NON-NLS-1$
		for (final String param : params) {
			if (param.indexOf('=') != -1) {
				try {
					final String key = param.substring(0, param.indexOf('='));
					final String value = URLDecoder.decode(param.substring(param.indexOf('=') + 1), URL_DEFAULT_CHARSET);
					parameters.put(key, value);
				}
				catch (final Exception e) {
					LOGGER.warning("Error al decodificar un parametro de la peticion: " + e); //$NON-NLS-1$
				}
			}
		}
		
		return parameters;
	}
	
	
}
