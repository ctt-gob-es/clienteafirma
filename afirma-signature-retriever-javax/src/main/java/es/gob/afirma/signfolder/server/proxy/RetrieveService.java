/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signfolder.server.proxy;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servicio de almacenamiento temporal de firmas.
 * &Uacute;til para servir de intermediario en comunicaci&oacute;n entre JavaScript y aplicaciones nativas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s.
 */
public final class RetrieveService extends HttpServlet {

	private static final long serialVersionUID = -3272368448371213403L;

	/** Log para registrar las acciones del servicio. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	private static final String URL_DEFAULT_CHARSET = "utf-8";  //$NON-NLS-1$
	
	private static final int BUFFER_SIZE = 2048; 
	
	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) throws ServletException, IOException {

		LOGGER.info(" == INICIO RECUPERACION"); //$NON-NLS-1$
		
		// Extraemos los parametros de la peticion
		Map<String, String> parameters;
		try {
			parameters = extractParameters(request);
		}
		catch (final Exception e) {
			LOGGER.severe("No se pudieron leer los parametros de la peticion: " + e); //$NON-NLS-1$
			response.sendError(HttpServletResponse.SC_BAD_REQUEST);
			return;
		}
		
		
		byte[] result = RetrieveServiceHandler.process(parameters);
		
		response.setHeader("Access-Control-Allow-Origin", "*"); //$NON-NLS-1$ //$NON-NLS-2$
		response.setContentType("text/plain"); //$NON-NLS-1$
		response.setCharacterEncoding("utf-8"); //$NON-NLS-1$

		final PrintWriter out = response.getWriter();
		out.println(result);
		out.flush();
				
		LOGGER.info(" == FIN RECUPERACION"); //$NON-NLS-1$
	}
	
	/**
	 * Extrae los parametros del payload de la aplicaci&oacute;n.
	 * @param request Petici&oacute;n de entrada..
	 * @return Mapa con el nombre y valor de los par&aacute;metros extra&iacute;dos.
	 * @throws IOException Cuando no se puedan leer los par&aacute;metros de la petici&oacute;n.
	 */
	private static Map<String, String> extractParameters(final HttpServletRequest request) throws IOException {

		Map<String, String> parameters = new HashMap<>();
		
		try (InputStream is = request.getInputStream()) {
			final String[] params = new String(getDataFromInputStream(is), URL_DEFAULT_CHARSET).split("&"); //$NON-NLS-1$
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
		}
		catch (UnsupportedOperationException e) {
			LOGGER.log(Level.SEVERE, "La codificacion por defecto no es compatible con la JRE: " + URL_DEFAULT_CHARSET, e);
		}
		
		return parameters;
	}
	
    /**
     * Lee un flujo de datos de entrada y los recupera en forma de array de
     * bytes. Este m&eacute;todo consume, pero no cierra el flujo de datos de
     * entrada.
     * @param input Flujo de donde se toman los datos.
     * @return Los datos obtenidos del flujo.
     * @throws IOException Cuando ocurre un problema durante la lectura.
     */
    public static byte[] getDataFromInputStream(final InputStream input) throws IOException {
        if (input == null) {
            return new byte[0];
        }
        int nBytes;
        final byte[] buffer = new byte[BUFFER_SIZE];
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        while ((nBytes = input.read(buffer)) != -1) {
            baos.write(buffer, 0, nBytes);
        }
        return baos.toByteArray();
    }
}