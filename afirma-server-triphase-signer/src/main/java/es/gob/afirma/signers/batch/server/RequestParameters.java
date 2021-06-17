/* Copyright (C) 2020 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */
package es.gob.afirma.signers.batch.server;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;

import javax.servlet.http.HttpServletRequest;

/**
 * Clase para la lectura y guardado de los par&aacute;metros de una petici&oacute;n.
 */
public class RequestParameters extends HashMap<String, String> {

	/** Serial Id. */
	private static final long serialVersionUID = 6942017463741129394L;

	private RequestParameters() {
		super();
	}

	/**
	 * Recupera un par&aacute;metro de la petici&oacute;n.
	 * @param name Nombre del par&aacute;metro.
	 * @return Valor del par&aacute;metro o {@code null} si no existe.
	 */
	public String getParameter(final String name) {
		return get(name);
	}

	/**
	 * Indica si se especific&oacute; un par&aacute;metro en la petici&oacute;n.
	 * @param name Nombre del par&aacute;metro.
	 * @return {@code true} si se indic&oacute; el par&aacute;metro,
	 * {@code false} en caso contrario.
	 */
	public boolean containsParameter(final String name) {
		return containsKey(name);
	}

	/**
	 * Parsea una petici&oacute;n al servicio.
	 * @param request Petici&oacute;n al servicio.
	 * @return Objetos extra&iacute;dos de la petici&oacute;n.
	 * @throws IOException Cuando ocurre un error en la lectura de la petici&oacute;n
	 * o uno de sus par&aacute;metros.
	 * @throws IllegalArgumentException Si la peticion no esta bien formada.
	 */
	public static RequestParameters extractParameters(final HttpServletRequest request) throws IOException {

		final RequestParameters params = new RequestParameters();

		if ("GET".equals(request.getMethod())) { //$NON-NLS-1$
			extractParametersFromUrl(request, params);
		}
		else {
			extractParametersFromBody(request, params);
		}

		return params;
	}


	private static void extractParametersFromUrl(final HttpServletRequest request, final RequestParameters params) {

		final Enumeration<?> names = request.getParameterNames();
		while (names.hasMoreElements()) {
			final String name = (String) names.nextElement();
			params.put(name, request.getParameter(name));
		}
	}

	private static void extractParametersFromBody(final HttpServletRequest request, final RequestParameters params) throws IOException {
		final char[] block = new char[1048576];
		final StringBuilder buffer = new StringBuilder(1048576);

		int n = 0;
		request.setCharacterEncoding("utf-8"); //$NON-NLS-1$
		try (final BufferedReader reader = request.getReader(); ) {
			while ((n = reader.read(block, 0, block.length)) > 0) {
				int startParams = 0;
				for (int i = 0; i < n; i++) {
					if (block[i] == '&') {
						if (i > 0) {
							buffer.append(Arrays.copyOfRange(block, startParams, i));
						}
						saveParam(params, buffer);
						startParams = i + 1;
					}
				}
				buffer.append(Arrays.copyOfRange(block, startParams, n));
			}
			saveParam(params, buffer);
		}
	}

	private static void saveParam(final HashMap<String, String> params, final StringBuilder param) {

		if (param.length() == 0) {
			return;
		}

		final int sep = param.indexOf("="); //$NON-NLS-1$
		if (sep == -1) {
			throw new IllegalArgumentException("La peticion no esta bien formada"); //$NON-NLS-1$
		}
		params.put(param.substring(0, sep), param.substring(sep + 1));
		param.setLength(0);
	}

}
