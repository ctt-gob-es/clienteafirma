/* Copyright (C) 2026 [Soluciones Alexendros]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 */

package es.gob.afirma.core.misc.protocol;

import java.security.SecureRandom;
import java.time.Clock;
import java.util.Base64;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * Registro de sesiones del canal local (idsession). La caducidad se aplica aqui, con un
 * reloj inyectable, y no en el codigo que llama. La memoria esta acotada: las sesiones
 * caducadas se purgan en cada operacion y nunca se superan {@link #MAX_TRACKED_SESSIONS}
 * entradas activas.
 */
public final class WebSocketSessionValidator {

	/** Estado de una sesion recibida. */
	public enum Status {
		/** Sesion valida y vigente. */
		OK,
		/** No se recibio identificador de sesion. */
		MISSING,
		/** El identificador no tiene un formato admitido. */
		MALFORMED,
		/** El identificador no se registro en este canal. */
		UNKNOWN,
		/** El identificador ya caduco. */
		EXPIRED,
		/** El identificador ya se uso o esta en uso por otro canal. */
		REUSED
	}

	/** Tiempo de vida por defecto de un identificador de sesion (2 minutos). */
	public static final long DEFAULT_TTL_MILLIS = 120000L;

	/** Tiempo de vida minimo admitido. */
	public static final long MIN_TTL_MILLIS = 1000L;

	/** Tiempo de vida maximo admitido. */
	public static final long MAX_TTL_MILLIS = 3600000L;

	/** Maximo de sesiones activas registradas a la vez. */
	public static final int MAX_TRACKED_SESSIONS = 128;

	/** Maximo de identificadores consumidos que se recuerdan. */
	public static final int MAX_CONSUMED_SESSIONS = 512;

	private static final int MIN_ID_LENGTH = 8;
	private static final int MAX_ID_LENGTH = 64;
	private static final int RANDOM_ID_BYTES = 16;

	private final Clock clock;
	private final long ttlMillis;
	private final Map<String, Long> expiresAt = new LinkedHashMap<>();
	private final Set<String> inUse = new LinkedHashSet<>();
	private final Set<String> consumed = new LinkedHashSet<>();

	/**
	 * Crea el validador.
	 * @param clock Reloj a usar para la caducidad.
	 * @param ttlMillis Tiempo de vida de cada sesion en milisegundos.
	 */
	public WebSocketSessionValidator(final Clock clock, final long ttlMillis) {
		if (clock == null) {
			throw new IllegalArgumentException("El reloj no puede ser nulo"); //$NON-NLS-1$
		}
		this.clock = clock;
		this.ttlMillis = clampTtl(ttlMillis);
	}

	/** Ajusta el tiempo de vida al rango admitido. */
	public static long clampTtl(final long ttlMillis) {
		if (ttlMillis < MIN_TTL_MILLIS) {
			return MIN_TTL_MILLIS;
		}
		if (ttlMillis > MAX_TTL_MILLIS) {
			return MAX_TTL_MILLIS;
		}
		return ttlMillis;
	}

	/** Tiempo de vida configurado, en milisegundos. */
	public long getTtlMillis() {
		return this.ttlMillis;
	}

	/**
	 * Genera un identificador de sesion aleatorio con {@link SecureRandom}.
	 * @return Identificador de sesion aleatorio.
	 */
	public static String generateSessionId() {
		final byte[] bytes = new byte[RANDOM_ID_BYTES];
		new SecureRandom().nextBytes(bytes);
		return Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
	}

	/**
	 * Comprueba que un identificador tenga un formato admitido.
	 * @param id Identificador a comprobar.
	 * @return {@code true} si el formato es valido.
	 */
	public static boolean isValidSessionId(final String id) {
		if (id == null || id.isEmpty() || id.length() < MIN_ID_LENGTH || id.length() > MAX_ID_LENGTH) {
			return false;
		}
		for (int i = 0; i < id.length(); i++) {
			final char c = id.charAt(i);
			if (!(Character.isLetterOrDigit(c) || c == '-' || c == '_')) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Registra una sesion con su caducidad.
	 * @param id Identificador de sesion.
	 * @return {@code true} si se registro; {@code false} si el formato no es valido o el
	 * identificador ya se consumio.
	 */
	public synchronized boolean register(final String id) {
		if (!isValidSessionId(id)) {
			return false;
		}
		purge();
		if (this.consumed.contains(id)) {
			return false;
		}
		this.expiresAt.put(id, Long.valueOf(this.clock.millis() + this.ttlMillis));
		while (this.expiresAt.size() > MAX_TRACKED_SESSIONS) {
			final Iterator<String> it = this.expiresAt.keySet().iterator();
			it.next();
			it.remove();
		}
		return true;
	}

	/**
	 * Genera y registra una sesion aleatoria.
	 * @return Identificador generado y registrado.
	 */
	public String registerNew() {
		String id;
		do {
			id = generateSessionId();
		} while (!register(id));
		return id;
	}

	/**
	 * Valida un identificador sin consumirlo. Aplica la caducidad.
	 * @param id Identificador recibido.
	 * @return Estado de la sesion.
	 */
	public synchronized Status validate(final String id) {
		if (id == null || id.isEmpty()) {
			purge();
			return Status.MISSING;
		}
		if (!isValidSessionId(id)) {
			purge();
			return Status.MALFORMED;
		}
		if (this.consumed.contains(id) || this.inUse.contains(id)) {
			purge();
			return Status.REUSED;
		}
		final Long expiry = this.expiresAt.get(id);
		if (expiry == null) {
			purge();
			return Status.UNKNOWN;
		}
		if (this.clock.millis() > expiry.longValue()) {
			// Se retira del registro y se informa de la caducidad de forma explicita
			this.expiresAt.remove(id);
			this.inUse.remove(id);
			return Status.EXPIRED;
		}
		purge();
		return Status.OK;
	}

	/**
	 * Indica que el identificador esta siendo usado por un canal. Una segunda conexion
	 * concurrente con el mismo identificador se rechaza.
	 * @param id Identificador de sesion.
	 */
	public synchronized void markInUse(final String id) {
		if (id != null) {
			this.inUse.add(id);
		}
	}

	/**
	 * Libera el identificador y lo consume: no se puede volver a usar.
	 * @param id Identificador de sesion.
	 */
	public synchronized void consume(final String id) {
		if (id == null) {
			return;
		}
		this.inUse.remove(id);
		this.expiresAt.remove(id);
		this.consumed.add(id);
		while (this.consumed.size() > MAX_CONSUMED_SESSIONS) {
			final Iterator<String> it = this.consumed.iterator();
			it.next();
			it.remove();
		}
	}

	/**
	 * Indica si el identificador ya se consumio.
	 * @param id Identificador de sesion.
	 * @return {@code true} si ya se uso.
	 */
	public synchronized boolean isConsumed(final String id) {
		return id != null && this.consumed.contains(id);
	}

	/** Numero de sesiones activas registradas (las caducadas no cuentan). */
	public synchronized int activeSessions() {
		purge();
		return this.expiresAt.size();
	}

	/** Elimina las sesiones caducadas del registro. */
	public synchronized void purge() {
		final long now = this.clock.millis();
		final Iterator<Map.Entry<String, Long>> it = this.expiresAt.entrySet().iterator();
		while (it.hasNext()) {
			final Map.Entry<String, Long> entry = it.next();
			if (now > entry.getValue().longValue()) {
				this.inUse.remove(entry.getKey());
				it.remove();
			}
		}
	}

	/** Borra todo el estado del registro. Solo para pruebas. */
	public synchronized void clear() {
		this.expiresAt.clear();
		this.inUse.clear();
		this.consumed.clear();
	}
}
