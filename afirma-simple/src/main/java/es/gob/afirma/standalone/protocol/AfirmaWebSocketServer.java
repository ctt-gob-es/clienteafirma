/* Copyright (C) 2019 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.security.GeneralSecurityException;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.net.ssl.SSLContext;
import javax.swing.Timer;

import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.java_websocket.server.DefaultSSLWebSocketServerFactory;
import org.java_websocket.server.WebSocketServer;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.so.macos.MacUtils;

/** Gestor de la invocaci&oacute;n por <i>WebSocket</i>. */
public final class AfirmaWebSocketServer extends WebSocketServer {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Prefijo de la URL de invocaci&oacute;n. */
	private static final String URL_PREFIX = "afirma://"; //$NON-NLS-1$

	/** Versi&oacute;n de protocolo m&aacute;s avanzada soportada. */
	private static final int CURRENT_PROTOCOL_VERSION = 3;

	/** Listado de versiones de protocolo soportadas. */
	private static final int[] SUPPORTED_PROTOCOL_VERSIONS = new int[] { CURRENT_PROTOCOL_VERSION };

	/** Prefijo de las peticiones de eco. */
	private static final String ECHO_REQUEST_PREFIX = "echo="; //$NON-NLS-1$

	/** Respuesta que se debe enviar ante las peticiones de echo. */
	private static final String ECHO_RESPONSE = "OK"; //$NON-NLS-1$

	/** Puerto a trav&eacute;s del que se realiza la comunicaci&oacute;n. */
	private static final int PORT = 63117;

	private static final int SOCKET_TIMEOUT = 60000;

	private static Timer inactivityTimer;

	private static int protocolVersion = -1;

	static AfirmaWebSocketServer instance = null;


	public static void startService(final int requestedProtocolVersion) throws UnsupportedProtocolException, GeneralSecurityException, IOException {

		checkSupportProtocol(requestedProtocolVersion);

		protocolVersion = requestedProtocolVersion;

		instance = new AfirmaWebSocketServer(PORT);

		final SSLContext sc = SecureSocketUtils.getSecureSSLContext();
		instance.setWebSocketFactory(new DefaultSSLWebSocketServerFactory(sc));
		instance.start();

		// Temporizador para cerrar la aplicaci&oacute;n cuando pase un determinado tiempo
		// sin haber recibido peticiones por el socket. Cuando se recibe la primera peticion,
		// se desactiva.
		inactivityTimer = new Timer(SOCKET_TIMEOUT, evt -> {
			LOGGER.warning("Se ha caducado la conexion. Se deja de escuchar en el puerto..."); //$NON-NLS-1$
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				MacUtils.closeMacService(URL_PREFIX);
			}
			Runtime.getRuntime().halt(0);
		});
		inactivityTimer.start();
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

		// Si aun corre el temporizador de inactividad, lo detenemos para que no
		// cierre la aplicacion
		if (inactivityTimer != null) {
			inactivityTimer.stop();
			inactivityTimer = null;
		}

		// Si recibimos en el socket un eco, lo respondemos con un OK
		if (message.startsWith(ECHO_REQUEST_PREFIX)) {
			broadcast(ECHO_RESPONSE, Collections.singletonList(ws));
		}
		// Si recibimos cualquier cosa distinta de un eco, consideraremos que es una peticion de operacion y
		// la procesaremos como tal
		else {
			broadcast(ProtocolInvocationLauncher.launch(message, protocolVersion, true), Collections.singletonList(ws));
		}
	}

	@Override
	public void onError(final WebSocket ws, final Exception ex) {
		LOGGER.log(Level.SEVERE, "Error en el socket", ex); //$NON-NLS-1$
	}

	/** Comprueba si una versi&oacute;n de protocolo est&aacute; soportado por la implementaci&oacute;n actual.
	 * @param version Identificador de la versi&oacute;n del protocolo.
	 * @throws UnsupportedProtocolException Cuando la versi&oacute;n de protocolo utilizada no se encuentra
	 *                                      entre las soportadas. */
	private static void checkSupportProtocol(final int version) throws UnsupportedProtocolException {
		for (final int supportedVersion : SUPPORTED_PROTOCOL_VERSIONS) {
			if (supportedVersion == version) {
				return;
			}
		}
		throw new UnsupportedProtocolException(version, version > CURRENT_PROTOCOL_VERSION);
	}
}
