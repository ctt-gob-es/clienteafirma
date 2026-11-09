/* Copyright (C) 2026 [Soluciones Alexendros]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 */

package es.gob.afirma.core.misc.protocol;

import java.time.Clock;
import java.util.logging.Logger;

/**
 * Configuracion y registro compartido del canal local (idsession y <code>Origin</code>).
 * <p>Propiedades del sistema:</p>
 * <ul>
 *   <li><code>strictWebSocketSession</code>: activa la exigencia de idsession y la
 *       validacion estricta del <code>Origin</code>. Por defecto, <code>false</code>.</li>
 *   <li><code>websocket.allowedOrigins</code>: lista separada por comas de origenes
 *       permitidos. Por defecto, vacia (ninguno).</li>
 *   <li><code>websocket.sessionTtlMillis</code>: tiempo de vida del idsession. Por
 *       defecto, {@link WebSocketSessionValidator#DEFAULT_TTL_MILLIS}.</li>
 * </ul>
 */
public final class LocalChannelSessions {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Propiedad que activa el modo estricto del canal local. */
	public static final String STRICT_PROPERTY = "strictWebSocketSession"; //$NON-NLS-1$

	/** Propiedad con la lista de origenes permitidos. */
	public static final String ALLOWED_ORIGINS_PROPERTY = "websocket.allowedOrigins"; //$NON-NLS-1$

	/** Propiedad con el tiempo de vida del idsession. */
	public static final String SESSION_TTL_PROPERTY = "websocket.sessionTtlMillis"; //$NON-NLS-1$

	private static final WebSocketSessionValidator SHARED = create();

	private LocalChannelSessions() {
		// No instanciable
	}

	private static WebSocketSessionValidator create() {
		return new WebSocketSessionValidator(Clock.systemUTC(), configuredTtlMillis());
	}

	/** Indica si el canal local funciona en modo estricto. */
	public static boolean isStrict() {
		return Boolean.getBoolean(STRICT_PROPERTY);
	}

	/** Lista de origenes permitidos configurada (vacia si no hay ninguna). */
	public static String getAllowedOrigins() {
		final String value = System.getProperty(ALLOWED_ORIGINS_PROPERTY);
		return value == null ? "" : value.trim(); //$NON-NLS-1$
	}

	/** Tiempo de vida configurado para el idsession. */
	public static long configuredTtlMillis() {
		final String value = System.getProperty(SESSION_TTL_PROPERTY);
		if (value == null || value.trim().isEmpty()) {
			return WebSocketSessionValidator.DEFAULT_TTL_MILLIS;
		}
		try {
			return WebSocketSessionValidator.clampTtl(Long.parseLong(value.trim()));
		}
		catch (final NumberFormatException e) {
			LOGGER.warning("Valor no numerico en " + SESSION_TTL_PROPERTY + "; se usa el valor por defecto"); //$NON-NLS-1$
			return WebSocketSessionValidator.DEFAULT_TTL_MILLIS;
		}
	}

	/** Registro compartido de sesiones del canal local. */
	public static WebSocketSessionValidator validator() {
		return SHARED;
	}

	/**
	 * Cierra el canal y consume su identificador de sesion.
	 * @param idSession Identificador del canal.
	 */
	public static void closeChannel(final String idSession) {
		SHARED.consume(idSession);
	}

	/**
	 * Encabezados CORS que corresponden al origen recibido.
	 * @param origin Origen de la peticion.
	 * @return Encabezados CORS; cadena vacia si no debe enviarse ninguno.
	 */
	public static String corsHeaders(final String origin) {
		final boolean strict = isStrict();
		final String allowed = getAllowedOrigins();
		if (!strict && !WebSocketOriginValidator.isConfigured(origin, allowed)) {
			LOGGER.warning("Origen no configurado en modo permisivo; se acepta y se responde con CORS abierto"); //$NON-NLS-1$
		}
		return WebSocketOriginValidator.corsHeaders(origin, allowed, strict);
	}

	/** Vacia el registro compartido. Solo para pruebas. */
	public static void clearForTests() {
		SHARED.clear();
	}
}
