/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.net.BindException;
import java.net.SocketTimeoutException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLServerSocketFactory;
import javax.swing.Timer;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.ProtocolVersion;
import es.gob.afirma.standalone.so.macos.MacUtils;

/** Gestor de la invocaci&oacute;n por <i>socket</i>. */
public final class ServiceInvocationManager {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$


	/** Tiempo de espera de cada <i>socket</i> en milisegundos. */
	private static int SOCKET_TIMEOUT = 90000;

	/** Versi&oacute;n de protocolo m&aacute;s avanzada soportada. */
	private static final int CURRENT_PROTOCOL_VERSION = 3;

	/** Listado de versiones de protocolo soportadas. */
	private static final int[] SUPPORTED_PROTOCOL_VERSIONS = new int[] { 1, 2, CURRENT_PROTOCOL_VERSION };

	/** Suites TLS 1.2 admitidas en el canal local. Solo suites con intercambio de claves
	 * efimero (ECDHE/DHE) y sin MAC SHA-1: se descartan las suites de RSA estatico
	 * (TLS_RSA_WITH_*), las de ECDH estatico (TLS_ECDH_*) y las CBC con SHA-1
	 * (ASVS 4.0.3 V9.1.3 y V6.2). */
	private static final String[] ENABLED_CIPHER_SUITES = new String[] {
			"TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384", //$NON-NLS-1$
			"TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256", //$NON-NLS-1$
			"TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384", //$NON-NLS-1$
			"TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256", //$NON-NLS-1$
			"TLS_DHE_RSA_WITH_AES_256_GCM_SHA384", //$NON-NLS-1$
			"TLS_DHE_RSA_WITH_AES_128_GCM_SHA256", //$NON-NLS-1$
			"TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384", //$NON-NLS-1$
			"TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384", //$NON-NLS-1$
			"TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256", //$NON-NLS-1$
			"TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256" //$NON-NLS-1$
	};

	/** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
	private ServiceInvocationManager(){
		// No instanciable
	}

	/**
	 * Inicia el servicio. Se intenta establecer un <code>socket</code> que escuche en el puerto pasado por la URL.
	 * @param channelInfo Informaci&oacute;n para el establecimiento del canal Par&aacute;metros de la URL de llamada (debe indicarse el puerto).
	 * @param protocolVersion Versi&oacute;n declarada del protocolo.
	 * @throws UnsupportedProtocolException Si no se soporta el protocolo o la versi&oacute;n de este.
	 * @throws SllKeyStoreException Cuando no se encuentre o no se pueda cargar el almac&eacute;n SSL para la securizaci&oacute;n del socket.
	 * @throws IOException Cuando no se puede abrir el socket o se cierra durante la operaci&oacute;n.
	 */
	static void startService(final ChannelInfo channelInfo, final ProtocolVersion protocolVersion) throws UnsupportedProtocolException,
		SllKeyStoreException, IOException {

		checkSupportProtocol(protocolVersion);

		SSLContext sc = null;

    	// Si al intentar obtener el contexto SSL, se recibe alguna excepcion, se mostrar el error
    	try {
    		sc = SecureSocketUtils.getSecureSSLContext();
    	} catch (final Exception e) {
			throw new SllKeyStoreException("No se ha podido cargar el certificado SSL para la securizacion del WebSocket", e); //$NON-NLS-1$
    	}

		final SSLServerSocketFactory ssocketFactory = sc.getServerSocketFactory();

		final SSLServerSocket ssocket = tryPorts(channelInfo.getPorts(), ssocketFactory);
		ssocket.setReuseAddress(true);

		// Restringimos el protocolo y las suites del canal local. Java no responde a los
		// mecanismos tradicionales para desactivar un protocolo (como la propiedad
		// "jdk.tls.disabledAlgorithms"), asi que se fijan de forma explicita en el socket.
		// TLS 1.3 no se habilita: el objetivo de compilacion del producto es Java 8, que no
		// lo soporta, y sus suites forman un conjunto distinto del de TLS 1.2.
		ssocket.setEnabledProtocols(SecureSocketUtils.getEnabledProtocols());
		ssocket.setEnabledCipherSuites(
			filterSupportedCipherSuites(ENABLED_CIPHER_SUITES, ssocket.getSupportedCipherSuites())
		);

		// Empieza la cuenta atras del temporizador.

		/** Temporizador para cerrar la aplicaci&oacute;n cuando pase un tiempo de inactividad. */
		final Timer timer = new Timer(SOCKET_TIMEOUT, evt -> {
			LOGGER.warning("Se ha caducado la conexion. Se deja de escuchar en el puerto..."); //$NON-NLS-1$
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				MacUtils.closeMacService(channelInfo.getIdSession());
			}
			Runtime.getRuntime().halt(0);
		});
		timer.start();

		while (true){
			try {
				new CommandProcessorThread(ssocket.accept(), timer, channelInfo.getIdSession(), protocolVersion).start();
			}
			catch (final SocketTimeoutException e) {
				LOGGER.severe("Tiempo de espera del socket terminado: " + e); //$NON-NLS-1$
			}
		}
	}

	/** Filtra las suites solicitadas dejando solo las soportadas por la maquina virtual.
	 * @param requested Suites que se quieren habilitar.
	 * @param supported Suites soportadas por el socket.
	 * @return Suites que se deben habilitar.
	 * @throws IOException Si ninguna de las suites solicitadas esta soportada. */
	private static String[] filterSupportedCipherSuites(final String[] requested, final String[] supported) throws IOException {
		final Set<String> supportedSet = new HashSet<>(Arrays.asList(supported));
		final List<String> enabled = new ArrayList<>();
		for (final String suite : requested) {
			if (supportedSet.contains(suite)) {
				enabled.add(suite);
			}
		}
		if (enabled.isEmpty()) {
			throw new IOException("Ninguna de las suites TLS configuradas para el canal local esta soportada por la maquina virtual"); //$NON-NLS-1$
		}
		return enabled.toArray(new String[enabled.size()]);
	}

	/** Intenta realizar una conexi&oacute; por <i>socket</i> en los puertos que se pasan por par&aacute;metro.
	 * @param ports Puertos a probar.
	 * @param socket <i>Socket</i> que se intenta conectar.
	 * @return El <code>SSLServerSocket</code> ya creado en el primer puerto encontrado disponible.
	 * @throws IOException Si ocurren errores durante el intento. */
	private static SSLServerSocket tryPorts(final int[] ports, final SSLServerSocketFactory socket) throws IOException {
		checkNullParameter(ports, "La lista de puertos no puede ser nula"); //$NON-NLS-1$
		checkNullParameter(socket, "El socket servidor no puede ser nulo"); //$NON-NLS-1$
		for (final int port : ports) {
			try {
				final SSLServerSocket ssocket = (SSLServerSocket) socket.createServerSocket(port);
				LOGGER.info("Establecido el puerto " + port + " para el servicio Autofirma"); //$NON-NLS-1$ //$NON-NLS-2$
				return ssocket;
			}
			catch (final BindException e) {
				LOGGER.warning(
					"El puerto " + port + " parece estar en uso, se continua con el siguiente: " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
			catch(final Exception e) {
				LOGGER.warning(
					"No se ha podido conectar al puerto " + port + ", se intentara con el siguiente: " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}
		throw new IOException("No se ha podido ligar el socket servidor a ningun puerto"); //$NON-NLS-1$
	}

	/** Comprueba que un par&aacute;metro no sea nulo.
	 * @param parameter Par&aacute;metro que se debe comprobar que no sea nulo.
	 * @param excepcionText Texto que se debe lanzar con la excepci&oacute;n. */
	private static void checkNullParameter (final Object parameter, final String excepcionText){
		if (parameter == null) {
			throw new IllegalArgumentException(excepcionText);
		}
	}

	/** Comprueba si una versi&oacute;n de protocolo est&aacute; soportado por la implementaci&oacute;n actual.
	 * @param protocolVersion Identificador de la versi&oacute;n del protocolo.
	 * @throws UnsupportedProtocolException Cuando la versi&oacute;n de protocolo utilizada no se encuentra
	 *                                      entre las soportadas. */
	private static void checkSupportProtocol(final ProtocolVersion protocolVersion) throws UnsupportedProtocolException {
		for (final int version : SUPPORTED_PROTOCOL_VERSIONS) {
			if (version == protocolVersion.getMajorVersion()) {
				return;
			}
		}

		throw new UnsupportedProtocolException(protocolVersion, protocolVersion.getMajorVersion() > CURRENT_PROTOCOL_VERSION);
	}
}
