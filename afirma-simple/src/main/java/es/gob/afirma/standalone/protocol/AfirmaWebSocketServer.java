/* Copyright (C) 2022 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.java_websocket.server.WebSocketServer;

/**
 * Servidor para la comunicaci&oacute;n por <i>WebSocket</i> acorde a la versi&oacute;n
 * inicial del protocolo.
 */
public class AfirmaWebSocketServer extends WebSocketServer {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Prefijo de las peticiones de eco. */
	private static final String ECHO_REQUEST_PREFIX = "echo="; //$NON-NLS-1$

	/** Respuesta que se debe enviar ante las peticiones de echo correctas. */
	private static final String ECHO_OK_RESPONSE = "OK"; //$NON-NLS-1$

	/** Uno de los prefijos que puede presentar el mensaje de invocaci&oacute;n de una firma de lote. Versi&oacute;n 1. */
	private static final String HEADER_BATCH_1 = "afirma://batch?"; //$NON-NLS-1$

	/** Uno de los prefijos que puede presentar el mensaje de invocaci&oacute;n de una firma de lote. Versi&oacute;n 1. */
	private static final String HEADER_BATCH_2 = "afirma://batch/?"; //$NON-NLS-1$

	private static int protocolVersion = -1;

	protected String sessionId;

	/**
	 * Genera un servidor websocket que atiende las peticiones de Autofirma.
	 * @param port Puerto a trav&eacute;s del que realizar la comunicaci&oacute;n.
	 * @param sessionId Identificador con el que se deber&aacute; autenticar la web
	 * para usar el servicio.
	 */
	public AfirmaWebSocketServer(final int port, final String sessionId) {
		super(new InetSocketAddress(port));
		setReuseAddr(true);

		this.sessionId = sessionId;

		// Nos aseguramos de que al cierre de la aplicacion, se dejara de escuchar en el puerto
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				try {
					AfirmaWebSocketServer.this.stop();
				} catch (final Exception e) {
					LOGGER.log(Level.SEVERE, "No se pudo detener la escucha en el puerto", e); //$NON-NLS-1$
				}
			}
		});
	}

	private WebSocket wsClient = null;

	@Override
	public void onOpen(final WebSocket ws, final ClientHandshake handshake) {
		LOGGER.info("Apertura del socket del puerto " + getAddress().getPort()); //$NON-NLS-1$

		if (this.wsClient == null) {
			this.wsClient = ws;
		}
	}

	@Override
	public void onClose(final WebSocket ws, final int code, final String reason, final boolean remote) {
		// Si se cierra el socket es que se ha terminado de operar con la aplicacion y puede cerrarse.
		// Sin embargo, comprobamos que sea este el primer cliente que lo abrio y no otra instancia
		// de la aplicacion que luego ha intentado acceder a el
		LOGGER.info("Se ha cerrado la comunicacion con el socket del puerto " + getAddress().getPort() + " . Codigo: " + code + ": " + reason); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		if (this.wsClient == null || this.wsClient.equals(ws)) {
			LOGGER.info("Cerramos la aplicacion"); //$NON-NLS-1$
			Runtime.getRuntime().halt(0);
		}
	}

	@Override
	public void onStart() {
		LOGGER.fine("Inicio del socket"); //$NON-NLS-1$
	}

	@Override
	public void onMessage(final WebSocket ws, final String message) {
		LOGGER.info("Recibimos una peticion en el socket del puerto: " + getAddress().getPort()); //$NON-NLS-1$

		// Si recibimos en el socket un eco, lo respondemos con un OK
		if (message.startsWith(ECHO_REQUEST_PREFIX)) {
			broadcast(ECHO_OK_RESPONSE, Collections.singletonList(ws));
		}
		// Si recibimos cualquier cosa distinta de un eco, consideraremos que es una peticion de
		// operacion y la procesaremos como tal
		else {
			// Si se trata de una operacion de firma de lote, incrementamos el tiempo de timeout
			final boolean batchOperation = message.startsWith(HEADER_BATCH_1) || message.startsWith(HEADER_BATCH_2);
			setConnectionLostTimeout(batchOperation ? 240 : 60);
			// Ejecutamos la peticion y devolvemos el resultado
			broadcast(ProtocolInvocationLauncher.launch(message, protocolVersion, true), Collections.singletonList(ws));
		}
	}

	@Override
	public void onError(final WebSocket ws, final Exception ex) {
		LOGGER.log(Level.SEVERE, "Error en el socket del puerto " + getAddress().getPort(), ex); //$NON-NLS-1$
	}
}
