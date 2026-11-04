/* Copyright (C) 2026 [Soluciones Alexendros]
 * This file is part of "Cliente @Firma".
 */
package es.gob.afirma.standalone.protocol;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import org.java_websocket.WebSocket;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.protocol.LocalChannelSessions;
import es.gob.afirma.core.misc.protocol.ProtocolVersion;
import es.gob.afirma.core.misc.protocol.WebSocketSessionValidator;

/** Utilidades comunes de las pruebas de integracion del canal local. */
final class LocalChannelTestSupport {

	private LocalChannelTestSupport() {
		// No instanciable
	}

	/** Devuelve un puerto libre de la interfaz local. */
	static int freePort() throws IOException {
		// nosemgrep: java.lang.security.audit.crypto.unencrypted-socket.unencrypted-socket -- canal local loopback bajo prueba, test
		try (ServerSocket socket = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"))) { //$NON-NLS-1$
			return socket.getLocalPort();
		}
	}

	/** Compone una peticion HTTP del protocolo local tal y como la envia la web.
	 * @param origin Origen declarado (puede ser {@code null}).
	 * @param idSession Identificador de sesion (puede ser {@code null}).
	 * @return Peticion en crudo. */
	static String rawRequest(final String origin, final String idSession) {
		final StringBuilder sb = new StringBuilder();
		sb.append("GET / HTTP/1.1\r\n"); //$NON-NLS-1$
		sb.append("Host: 127.0.0.1\r\n"); //$NON-NLS-1$
		sb.append("Content-Type: text/plain\r\n"); //$NON-NLS-1$
		if (origin != null) {
			sb.append("Origin: ").append(origin).append("\r\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		sb.append("\r\n"); //$NON-NLS-1$
		// Relleno para que la etiqueta de fin y el idsession queden fuera del rango de
		// seguridad del lector, igual que en una peticion real.
		sb.append("echo="); //$NON-NLS-1$
		for (int i = 0; i < 60; i++) {
			sb.append('A');
		}
		if (idSession != null) {
			sb.append("idsession=").append(idSession); //$NON-NLS-1$
		}
		sb.append("@EOF"); //$NON-NLS-1$
		return sb.toString();
	}

	/** Envia una peticion en crudo al puerto indicado y devuelve la respuesta completa. */
	static String sendRaw(final int port, final String request) throws IOException {
		// nosemgrep: java.lang.security.audit.crypto.unencrypted-socket.unencrypted-socket -- canal local loopback bajo prueba, test
		try (Socket socket = new Socket(InetAddress.getByName("127.0.0.1"), port)) { //$NON-NLS-1$
			socket.setSoTimeout(15000);
			final OutputStream out = socket.getOutputStream();
			out.write(request.getBytes(StandardCharsets.UTF_8));
			out.flush();
			final InputStream in = socket.getInputStream();
			final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
			final byte[] chunk = new byte[4096];
			int read;
			while ((read = in.read(chunk)) != -1) {
				buffer.write(chunk, 0, read);
			}
			return new String(buffer.toByteArray(), StandardCharsets.UTF_8);
		}
	}

	/** Extrae el cuerpo (Base64) de una respuesta HTTP del canal local. */
	static String responseBody(final String response) {
		final int idx = response.indexOf("\n\n"); //$NON-NLS-1$
		final String body = idx >= 0 ? response.substring(idx + 2) : ""; //$NON-NLS-1$
		try {
			return new String(Base64.decode(body.trim(), true), StandardCharsets.UTF_8);
		}
		catch (final IOException e) {
			return ""; //$NON-NLS-1$
		}
	}

	/** Indica si la respuesta del canal local es un OK. */
	static boolean isOk(final String response) {
		final String body = responseBody(response);
		return "OK".equals(body) || "SAVE_OK".equals(body); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Devuelve el valor de un encabezado de la respuesta HTTP. */
	static String header(final String response, final String name) {
		for (final String line : response.split("\r?\n")) { //$NON-NLS-1$
			final int sep = line.indexOf(':');
			if (sep > 0 && line.substring(0, sep).trim().equalsIgnoreCase(name)) {
				return line.substring(sep + 1).trim();
			}
		}
		return null;
	}

	/** Activa o desactiva el modo estricto y la lista de origenes durante una prueba. */
	static void configure(final boolean strict, final String allowedOrigins) {
		System.setProperty(LocalChannelSessions.STRICT_PROPERTY, Boolean.toString(strict));
		System.setProperty(LocalChannelSessions.ALLOWED_ORIGINS_PROPERTY, allowedOrigins == null ? "" : allowedOrigins); //$NON-NLS-1$
	}

	/** Restaura la configuracion por defecto (permisiva y sin origenes declarados). */
	static void restoreConfiguration() {
		System.clearProperty(LocalChannelSessions.STRICT_PROPERTY);
		System.clearProperty(LocalChannelSessions.ALLOWED_ORIGINS_PROPERTY);
		System.clearProperty(LocalChannelSessions.SESSION_TTL_PROPERTY);
	}

	/** Captura los mensajes de log del cliente para comprobar que no hay fugas. */
	static final class LogCapture extends Handler {

		private final List<String> messages = new ArrayList<>();

		LogCapture(final Logger logger) {
			logger.addHandler(this);
			setLevel(Level.ALL);
		}

		@Override
		public synchronized void publish(final LogRecord record) {
			if (record != null && record.getMessage() != null) {
				this.messages.add(record.getMessage());
			}
		}

		@Override
		public void flush() {
			// Sin buffer
		}

		@Override
		public void close() {
			// Sin recursos
		}

		synchronized List<String> getMessages() {
			return new ArrayList<>(this.messages);
		}

		boolean contains(final String text) {
			for (final String message : getMessages()) {
				if (message.contains(text)) {
					return true;
				}
			}
			return false;
		}
	}

	/** Servidor websocket real que no cierra la maquina virtual al cerrarse el cliente. */
	static final class NonHaltingServer extends AfirmaWebSocketServerV4Sup {

		NonHaltingServer(final int port, final String sessionId, final WebSocketSessionValidator validator) {
			super(port, sessionId, ProtocolVersion.getInstance(ProtocolVersion.VERSION_4), validator);
		}

		@Override
		public void onClose(final WebSocket ws, final int code, final String reason, final boolean remote) {
			// En produccion este cierre termina la aplicacion; aqui solo se libera la sesion.
			releaseSession(ws);
		}
	}
}
