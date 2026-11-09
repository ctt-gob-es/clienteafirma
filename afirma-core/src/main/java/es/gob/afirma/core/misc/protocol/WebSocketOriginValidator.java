/* Copyright (C) 2026 [Soluciones Alexendros]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 */

package es.gob.afirma.core.misc.protocol;

import java.util.Locale;

/**
 * Validacion del encabezado <code>Origin</code> del canal local. No hay comodin por
 * defecto: la lista permitida se configura y, en modo estricto, una lista vacia rechaza
 * todos los origenes.
 */
public final class WebSocketOriginValidator {

	private WebSocketOriginValidator() {
		// No instanciable
	}

	/**
	 * Indica si un origen esta configurado expresamente en la lista.
	 * @param origin Origen recibido, tal cual llega en el encabezado.
	 * @param allowedOrigins Lista separada por comas; admite entradas <code>*.dominio</code>.
	 * @return {@code true} si el origen figura en la lista.
	 */
	public static boolean isConfigured(final String origin, final String allowedOrigins) {
		if (origin == null || origin.trim().isEmpty() || allowedOrigins == null) {
			return false;
		}
		final String value = normalize(origin);
		for (final String candidate : allowedOrigins.split(",")) { //$NON-NLS-1$
			final String entry = normalize(candidate);
			if (entry.isEmpty()) {
				continue;
			}
			if (entry.equals(value)) {
				return true;
			}
			if (entry.startsWith("*.") && value.endsWith(entry.substring(1)) //$NON-NLS-1$
					&& value.length() > entry.length()) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Decide si se acepta el origen.
	 * En modo estricto solo se aceptan los origenes configurados; sin lista, ninguno.
	 * En modo permisivo se acepta siempre (el llamador debe avisar si no esta configurado).
	 * @param origin Origen recibido.
	 * @param allowedOrigins Lista permitida.
	 * @param strict Modo estricto.
	 * @return {@code true} si se acepta el origen.
	 */
	public static boolean isAllowed(final String origin, final String allowedOrigins, final boolean strict) {
		if (!strict) {
			return true;
		}
		return isConfigured(origin, allowedOrigins);
	}

	/**
	 * Compone el valor del encabezado <code>Access-Control-Allow-Origin</code>.
	 * @param origin Origen recibido.
	 * @param allowedOrigins Lista permitida.
	 * @param strict Modo estricto.
	 * @return Valor del encabezado o {@code null} si no debe enviarse.
	 */
	public static String allowOriginValue(final String origin, final String allowedOrigins, final boolean strict) {
		if (!strict) {
			return "*"; //$NON-NLS-1$
		}
		return isConfigured(origin, allowedOrigins) ? normalize(origin) : null;
	}

	/**
	 * Compone los encabezados CORS completos, con <code>Vary: Origin</code> cuando la
	 * respuesta depende del origen.
	 * @param origin Origen recibido.
	 * @param allowedOrigins Lista permitida.
	 * @param strict Modo estricto.
	 * @return Encabezados CORS listos para insertar en la respuesta HTTP.
	 */
	public static String corsHeaders(final String origin, final String allowedOrigins, final boolean strict) {
		final String value = allowOriginValue(origin, allowedOrigins, strict);
		if (value == null) {
			return ""; //$NON-NLS-1$
		}
		final StringBuilder sb = new StringBuilder();
		sb.append("Access-Control-Allow-Origin: ").append(value).append('\n'); //$NON-NLS-1$
		if (strict) {
			sb.append("Vary: Origin\n"); //$NON-NLS-1$
		}
		return sb.toString();
	}

	private static String normalize(final String value) {
		return value == null ? "" : value.trim().toLowerCase(Locale.ROOT); //$NON-NLS-1$
	}
}
