/* Copyright (C) 2019 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.net.ssl.SSLContext;

import org.java_websocket.server.DefaultSSLWebSocketServerFactory;

import es.gob.afirma.standalone.configurator.common.PreferencesManager;

/** Gestor de la invocaci&oacute;n por <i>WebSocket</i>. */
public class AfirmaWebSocketServerManager {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Versi&oacute;n de protocolo en la que el puerto de conexi&oacute;n era fijo. */
	private static final int PROTOCOL_VERSION_3 = 3;

	/** Versi&oacute;n de protocolo con varios puertos y comprobaci&oacute;n de ID de sesi&oacute;n. */
	private static final int PROTOCOL_VERSION_4 = 4;

	/** Versi&oacute;n de protocolo actual. */
	private static final int CURRENT_PROTOCOL_VERSION = PROTOCOL_VERSION_4;

	/** Listado de versiones de protocolo soportadas. */
	private static final int[] SUPPORTED_PROTOCOL_VERSIONS = new int[] { PROTOCOL_VERSION_3, PROTOCOL_VERSION_4 };

    /** Propiedad del sistema para configurar la optimizacion de WebSockets para VDI. */
	private static final String SYSTEM_PROPERTY_OPTIMIZED_FOR_VDI = "websockets.optimizedForVdi"; //$NON-NLS-1$

	private static int protocolVersion = -1;

	static AfirmaWebSocketServer instance = null;

	/**
	 * Inicia un WebSocket para la comunicaci&oacute;n con el navegador.
	 * @param channelInfo Informaci&oacute;n para la construcci&oacute;n de la comunicaci&oacute;n.
	 * @param requestedProtocolVersion Versi&oacute;n del protocolo de comunicaci&oacute;n.
	 * @throws UnsupportedProtocolException Cuando se ha solicitado el uso de una versi&oacute;n de protocolo no soportada.
	 * @throws SocketOperationException Cuando
	 */
	public static void startService(final ChannelInfo channelInfo, final int requestedProtocolVersion) throws UnsupportedProtocolException, SocketOperationException {

		checkSupportProtocol(requestedProtocolVersion);

		protocolVersion = requestedProtocolVersion;

 		// Configuramos la optimizacion para VDI segun lo establecido en el dialogo de preferencias
 		final boolean optimizedForVdi = PreferencesManager
 				.getBoolean(PreferencesManager.PREFERENCE_GENERAL_VDI_OPTIMIZATION);
         System.setProperty(SYSTEM_PROPERTY_OPTIMIZED_FOR_VDI, Boolean.toString(optimizedForVdi));

		int i = 0;
		final int[] ports = channelInfo.getPorts();
		do {
			LOGGER.info("Tratamos de abrir el socket en el puerto: " + ports[i]); //$NON-NLS-1$

			try {
				switch (protocolVersion) {
				case PROTOCOL_VERSION_4:
					instance = new AfirmaWebSocketServerV4(ports[i], channelInfo.getIdSession());
					break;

				default:
					instance = new AfirmaWebSocketServer(ports[i], channelInfo.getIdSession());
					break;
				}

				final SSLContext sc = SecureSocketUtils.getSecureSSLContext();
				instance.setWebSocketFactory(new DefaultSSLWebSocketServerFactory(sc));
				instance.start();
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se ha podido abrir un socket en el puerto: " + ports[i], e); //$NON-NLS-1$
				instance = null;
			}
			i++;
		}
		while (instance == null && i < ports.length);

		if (instance == null) {
			throw new SocketOperationException("No se ha podido abrir ningun socket. Se aborta la comunicacion."); //$NON-NLS-1$
		}
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
