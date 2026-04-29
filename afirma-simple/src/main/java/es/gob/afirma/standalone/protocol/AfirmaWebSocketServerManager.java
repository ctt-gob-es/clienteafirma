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

import es.gob.afirma.core.misc.protocol.ProtocolVersion;
import es.gob.afirma.standalone.SimpleErrorCode;
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
	private static final int[] SUPPORTED_PROTOCOL_VERSIONS = { PROTOCOL_VERSION_3, PROTOCOL_VERSION_4 };

    /** Propiedad del sistema para configurar la optimizacion de WebSockets para VDI. */
	private static final String SYSTEM_PROPERTY_OPTIMIZED_FOR_VDI = "websockets.optimizedForVdi"; //$NON-NLS-1$

	static AfirmaWebSocketServer instance = null;

	/**
	 * Inicia un WebSocket para la comunicaci&oacute;n con el navegador.
	 * @param channelInfo Informaci&oacute;n para la construcci&oacute;n de la comunicaci&oacute;n.
	 * @param requestedProtocolVersion Versi&oacute;n del protocolo de comunicaci&oacute;n.
	 * @param asynchronous Si viene con valor <code>true</code> tratar&aacute; las operaciones de forma as&iacute;ncrona si viene a false las tratara como s&iacute;ncronas<code>false</code>.
	 * @throws UnsupportedProtocolException Cuando se ha solicitado el uso de una versi&oacute;n de protocolo no soportada.
	 * @throws SocketOperationException Cuando no se pueda abrir el websocket.
	 * @throws SllKeyStoreException Cuando no se encuentre o no se pueda cargar el almac&eacute;n SSL para la securizaci&oacute;n del websocket.
	 */
	public static void startService(final ChannelInfo channelInfo, final ProtocolVersion requestedProtocolVersion, final boolean asynchronous) throws UnsupportedProtocolException, SocketOperationException, SllKeyStoreException {

		checkSupportProtocol(requestedProtocolVersion);

    	// Si al intentar obtener el contexto SSL, se recibe alguna excepcion, se mostrar el error
		 SSLContext sc;
    	try {
    		sc = SecureSocketUtils.getSecureSSLContext();
    	} catch (final Exception e) {
			throw new SllKeyStoreException("No se ha podido cargar el certificado SSL para la securizacion del WebSocket", e); //$NON-NLS-1$
    	}

 		// Configuramos la optimizacion para VDI segun lo establecido en el dialogo de preferencias
 		final boolean optimizedForVdi = PreferencesManager
 				.getBoolean(PreferencesManager.PREFERENCE_GENERAL_VDI_OPTIMIZATION);
         System.setProperty(SYSTEM_PROPERTY_OPTIMIZED_FOR_VDI, Boolean.toString(optimizedForVdi));

		int port = channelInfo.nextPortAvailable();
		while (instance == null && port != -1) {
			LOGGER.info("Tratamos de abrir el socket en el puerto: " + port); //$NON-NLS-1$

			try {
				switch (requestedProtocolVersion.getMajorVersion()) {
				case PROTOCOL_VERSION_4:
					LOGGER.info("Se procesada la peticion de forma " + (asynchronous ? "asincrona" : "sincrona")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					instance = new AfirmaWebSocketServerV4Sup(port, channelInfo.getIdSession(), requestedProtocolVersion);
					((AfirmaWebSocketServerV4Sup) instance).setAsyncOperation(asynchronous);
					break;

				default:
					instance = new AfirmaWebSocketServer(port, channelInfo.getIdSession());
					break;
				}

				sc = SecureSocketUtils.getSecureSSLContext();
				instance.setWebSocketFactory(new DefaultSSLWebSocketServerFactory(sc));
				instance.setBindingErrorListener(new BindingErrorListener(channelInfo, requestedProtocolVersion, asynchronous));
				instance.start();

				// Iniciamos un temporizador para cerrar el socket si no se recibe una
				// primera peticion dentro de un tiempo determinado
				instance.initInactivityCountdown();
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se ha podido abrir un socket en el puerto: " + port, e); //$NON-NLS-1$
				instance = null;
				port = channelInfo.nextPortAvailable();
			}
		}

		if (instance == null) {
			throw new SocketOperationException("No se ha podido abrir el Websocket en ninguno de los puertos indicados. Se aborta la comunicacion.", SimpleErrorCode.Internal.SOCKET_INITIALIZING_ERROR); //$NON-NLS-1$
		}
	}

	/**
	 * Clase para procesar los errores de conexi&oacute;n con el WebSocket.
	 */
	public static class BindingErrorListener implements AfirmaWebSocketBindingErrorListener {

		private final ChannelInfo channelInfo;
		private final ProtocolVersion protocolVersion;
		private final boolean asynchronous;

		public BindingErrorListener(final ChannelInfo channelInfo, final ProtocolVersion protocolVersion, final boolean asynchronous) {
			this.channelInfo = channelInfo;
			this.protocolVersion = protocolVersion;
			this.asynchronous = asynchronous;
		}

		@Override
		public void onErrorBinding() {
			try {
				instance = null;
				AfirmaWebSocketServerManager.startService(this.channelInfo, this.protocolVersion, this.asynchronous);
			} catch (final Exception e) {
				LOGGER.log(Level.SEVERE, "El reintento de inicio del websockets ha fallado. Cerramos la aplicacion", e); //$NON-NLS-1$
				Runtime.getRuntime().halt(-1);
			}
		}
	}

	/**
	 * Comprueba si una versi&oacute;n de protocolo est&aacute; soportado por la implementaci&oacute;n actual.
	 * @param version Identificador de la versi&oacute;n del protocolo.
	 * @throws UnsupportedProtocolException Cuando la versi&oacute;n de protocolo utilizada no se encuentra
	 *                                      entre las soportadas.
	 */
	private static void checkSupportProtocol(final ProtocolVersion requestversion) throws UnsupportedProtocolException {
		for (final int supportedVersion : SUPPORTED_PROTOCOL_VERSIONS) {
			if (supportedVersion == requestversion.getMajorVersion()) {
				return;
			}
		}
		throw new UnsupportedProtocolException(requestversion, requestversion.getMajorVersion() > CURRENT_PROTOCOL_VERSION);
	}

}
