/* Copyright (C) 2019 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.security.GeneralSecurityException;
import java.util.Collections;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.net.ssl.SSLContext;

import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.java_websocket.server.DefaultSSLWebSocketServerFactory;
import org.java_websocket.server.WebSocketServer;

/** Gestor de la invocaci&oacute;n por <i>WebSocket</i>. */
public final class AfirmaWebSocketServer extends WebSocketServer {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la versi&oacute;n del protocolo que se va a utilizar. */
	private static final String PROTOCOL_VERSION_PARAM = "v"; //$NON-NLS-1$

	/** Versi&oacute;n de protocolo m&aacute;s avanzada soportada. */
	private static final int CURRENT_PROTOCOL_VERSION = 1;

	/** Listado de versiones de protocolo soportadas. */
	private static final int[] SUPPORTED_PROTOCOL_VERSIONS = new int[] { CURRENT_PROTOCOL_VERSION };

	/** Prefijo de las peticiones de eco. */
	private static final String ECHO_REQUEST_PREFIX = "echo="; //$NON-NLS-1$

	/** Respuesta que se debe enviar ante las peticiones de echo. */
	private static final String ECHO_RESPONSE = "OK"; //$NON-NLS-1$


	private static final int PORT = 13117;

	static AfirmaWebSocketServer instance = null;

	public static void startService(final String url) throws UnsupportedProtocolException, GeneralSecurityException, IOException {

		checkSupportProtocol(getVersion(url));

		instance = new AfirmaWebSocketServer(PORT);

		final SSLContext sc = SecureSocketUtils.getSecureSSLContext();
		instance.setWebSocketFactory(new DefaultSSLWebSocketServerFactory(sc));
		instance.start();
	}


	/**
	 * Genera un servidor websocket que atiende las peticiones del Cliente @firma.
	 * @param port Puerto a trav&eacute;s del que realizar la comunicaci&oacute;n.
	 */
	public AfirmaWebSocketServer(final int port) {
		super(new InetSocketAddress(port));
		setReuseAddr(true);

		// Nos aseguramos de que al cierre de la aplicacion, se dejara de escuchar en el puerto
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				if (AfirmaWebSocketServer.instance != null) {
					try {
						AfirmaWebSocketServer.instance.stop();
					} catch (final Exception e) {
						LOGGER.log(Level.SEVERE, "No se pudo detener la escucha en el puerto", e); //$NON-NLS-1$
					}
				}
            }
		});
	}

	@Override
	public void onOpen(final WebSocket ws, final ClientHandshake handshake) {
		LOGGER.info("Apertura del socket"); //$NON-NLS-1$
	}

	@Override
	public void onClose(final WebSocket ws, final int code, final String reason, final boolean remote) {
		// Si se cierra el socket es que se ha terminado de operar con la aplicacion y puede cerrarse o,
		// incluso, que hubo un problema al abrirlo, con lo cual puede que ya hubiese otra instancia de
		// la aplicacion escuchando en el
		LOGGER.info("Se ha cerrado la comunicacion con el socket. Cerramos la aplicacion"); //$NON-NLS-1$
		Runtime.getRuntime().halt(0);
	}

	@Override
	public void onStart() {
		LOGGER.fine("Inicio del socket"); //$NON-NLS-1$
	}

	@Override
	public void onMessage(final WebSocket ws, final String message) {
		LOGGER.info("Recibimos una peticion en el socket"); //$NON-NLS-1$
		// Si recibimos en el socket un eco, lo respondemos con un OK
		if (message.startsWith(ECHO_REQUEST_PREFIX)) {
			broadcast(ECHO_RESPONSE, Collections.singletonList(ws));
		}
		// Si recibimos cualquier cosa distinta de un eco, consideraremos que es una peticion de operacion y
		// la procesaremos como tal
		else {
			broadcast(ProtocolInvocationLauncher.launch(message, true), Collections.singletonList(ws));
		}
	}

	@Override
	public void onError(final WebSocket ws, final Exception ex) {
		LOGGER.log(Level.SEVERE, "Error en el socket", ex); //$NON-NLS-1$
	}


	/** Obtiene el par&aacute;metro de versi&oacute;n declarado en la URL.
	 * @param url URL de la que extraer la versi&oacute;n.
	 * @return Valor del par&aacute;metro de versi&oacute;n ('v') o {@code null} si no est&aacute; definido. */
	private static String getVersion(final String url) {

		final URI u;
		try {
			u = new URI(url);
		}
		catch (final Exception e) {
			throw new IllegalArgumentException("La URI " + url + "de invocacion no es valida: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		final String query = u.getQuery();
		checkNullParameter(query, "La URI de invocacion no contiene parametros: " + url); //$NON-NLS-1$
		final Properties p = new Properties();
		try {
			p.load(new ByteArrayInputStream(query.replace("&", "\n").getBytes())); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final IOException e) {
			throw new IllegalArgumentException(
				"Los parametros de la URI de invocacion no estan el el formato correcto: " + url //$NON-NLS-1$
			, e);
		}
		return p.getProperty(PROTOCOL_VERSION_PARAM);
	}

	/** Comprueba si una versi&oacute;n de protocolo est&aacute; soportado por la implementaci&oacute;n actual.
	 * @param protocolId Identificador de la versi&oacute;n del protocolo.
	 * @throws UnsupportedProtocolException Cuando la versi&oacute;n de protocolo utilizada no se encuentra
	 *                                      entre las soportadas. */
	private static void checkSupportProtocol(final String protocolId) throws UnsupportedProtocolException {
		int protocolVersion = 1;
		if (protocolId != null) {
			try {
				protocolVersion = Integer.parseInt(protocolId.trim());
			}
			catch (final Exception e) {
				LOGGER.info(
					"El ID de protocolo indicado no es un numero entero (" + protocolId + "): " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
				protocolVersion = -1;
			}
		}

		for (final int version : SUPPORTED_PROTOCOL_VERSIONS) {
			if (version == protocolVersion) {
				return;
			}
		}

		throw new UnsupportedProtocolException(protocolVersion, protocolVersion > CURRENT_PROTOCOL_VERSION);
	}

	/** Comprueba que un par&aacute;metro no sea nulo.
	 * @param parameter Par&aacute;metro que se debe comprobar que no sea nulo.
	 * @param excepcionText Texto que se debe lanzar con la excepci&oacute;n. */
	private static void checkNullParameter (final Object parameter, final String excepcionText){
		if (parameter == null) {
			throw new IllegalArgumentException(excepcionText);
		}
	}
}
