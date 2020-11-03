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
import java.security.GeneralSecurityException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLServerSocketFactory;
import javax.swing.Timer;

import es.gob.afirma.core.misc.Platform;
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

	private static final String IDSESSION = "idsession"; //$NON-NLS-1$

	private static final String[] ENABLED_CIPHER_SUITES = new String[] {
			"TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384", //$NON-NLS-1$
			"TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256", //$NON-NLS-1$
			"TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384", //$NON-NLS-1$
			"TLS_RSA_WITH_AES_256_GCM_SHA384", //$NON-NLS-1$
			"TLS_ECDH_ECDSA_WITH_AES_256_GCM_SHA384", //$NON-NLS-1$
			"TLS_ECDH_RSA_WITH_AES_256_GCM_SHA384", //$NON-NLS-1$
			"TLS_DHE_RSA_WITH_AES_256_GCM_SHA384", //$NON-NLS-1$
			"TLS_DHE_DSS_WITH_AES_256_GCM_SHA384", //$NON-NLS-1$
			"TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256", //$NON-NLS-1$
			"TLS_RSA_WITH_AES_128_GCM_SHA256", //$NON-NLS-1$
			"TLS_ECDH_ECDSA_WITH_AES_128_GCM_SHA256", //$NON-NLS-1$
			"TLS_ECDH_RSA_WITH_AES_128_GCM_SHA256", //$NON-NLS-1$
			"TLS_DHE_RSA_WITH_AES_128_GCM_SHA256", //$NON-NLS-1$
			"TLS_DHE_DSS_WITH_AES_128_GCM_SHA256", //$NON-NLS-1$


			"TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384", //$NON-NLS-1$
			"TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384", //$NON-NLS-1$
			"TLS_RSA_WITH_AES_256_CBC_SHA256", //$NON-NLS-1$
			"TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA384", //$NON-NLS-1$
			"TLS_ECDH_RSA_WITH_AES_256_CBC_SHA384", //$NON-NLS-1$
			"TLS_DHE_RSA_WITH_AES_256_CBC_SHA256", //$NON-NLS-1$
			"TLS_DHE_DSS_WITH_AES_256_CBC_SHA256", //$NON-NLS-1$
			"TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA", //$NON-NLS-1$
			"TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA", //$NON-NLS-1$
			"TLS_RSA_WITH_AES_256_CBC_SHA", //$NON-NLS-1$
			"TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA", //$NON-NLS-1$
			"TLS_ECDH_RSA_WITH_AES_256_CBC_SHA", //$NON-NLS-1$
			"TLS_DHE_RSA_WITH_AES_256_CBC_SHA", //$NON-NLS-1$
			"TLS_DHE_DSS_WITH_AES_256_CBC_SHA", //$NON-NLS-1$
			"TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256", //$NON-NLS-1$
			"TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256", //$NON-NLS-1$
			"TLS_RSA_WITH_AES_128_CBC_SHA256", //$NON-NLS-1$
			"TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA256", //$NON-NLS-1$
			"TLS_ECDH_RSA_WITH_AES_128_CBC_SHA256", //$NON-NLS-1$
			"TLS_DHE_RSA_WITH_AES_128_CBC_SHA256", //$NON-NLS-1$
			"TLS_DHE_DSS_WITH_AES_128_CBC_SHA256", //$NON-NLS-1$
			"TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA", //$NON-NLS-1$
			"TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA", //$NON-NLS-1$
			"TLS_RSA_WITH_AES_128_CBC_SHA", //$NON-NLS-1$
			"TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA", //$NON-NLS-1$
			"TLS_ECDH_RSA_WITH_AES_128_CBC_SHA", //$NON-NLS-1$
			"TLS_DHE_RSA_WITH_AES_128_CBC_SHA", //$NON-NLS-1$
			"TLS_DHE_DSS_WITH_AES_128_CBC_SHA" //$NON-NLS-1$
	};

	/** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
	private ServiceInvocationManager(){
		// No instanciable
	}

	/** Inicia el servicio. Se intenta establecer un <code>socket</code> que escuche en el puerto pasado por la URL.
	 * @param urlParams Par&aacute;metros de la URL de llamada (debe indicarse el puerto).
	 * @param protocolVersion Versi&oacute;n declarada del protocolo.
	 * @throws UnsupportedProtocolException Si no se soporta el protocolo o la versi&oacute;n de este. */
	static void startService(final Map<String, String> urlParams, final int protocolVersion) throws UnsupportedProtocolException {

		checkSupportProtocol(protocolVersion);

		try {
			final SSLContext sc = SecureSocketUtils.getSecureSSLContext();

			final SSLServerSocketFactory ssocketFactory = sc.getServerSocketFactory();

			final ChannelInfo channelInfo = getChannelInfo(urlParams);
			final SSLServerSocket ssocket = tryPorts(channelInfo.getPorts(), ssocketFactory);
			ssocket.setReuseAddress(true);

			// TODO: Restringimos las suites a utilizar a las por defecto de Java 11 (compatible con Java 8 y posteriores)
			// omitiendo las suites de TLSv1.3 ya que la implementacion de estas no es compatible con la implementacion
			// de Chrome v74 y anteriores (probablemente porque la version implementada en Java se basa en uno de los
			// ultimos bocetos del estandar y no en la version final).
			// Se hace de esta manera porque Java no responde a los mecanismos tradicionales para desactivar el protocolo
			// (como la propiedad "jdk.tls.disabledAlgorithms").
			// Una vez se implemente una version compatible del estandar se deberia eliminar esta limitacion.
			ssocket.setEnabledCipherSuites(ENABLED_CIPHER_SUITES);

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

		// Con las excepciones no hacemos nada ya que no tenemos forma de transmitir el
		// error de vuelta y no debemos mostrar dialogos graficos
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Error en la comunicacion a traves del socket", e); //$NON-NLS-1$
		}
		catch(final KeyStoreException e){
            LOGGER.severe("Error con el keyStore: " + e); //$NON-NLS-1$
		}
        catch(final NoSuchAlgorithmException e){
            LOGGER.severe("Error con el algoritmo del  certificado: " + e); //$NON-NLS-1$
        }
        catch(final CertificateException e){
            LOGGER.severe("Error con el certificado: " + e); //$NON-NLS-1$
        }
        catch(final UnrecoverableKeyException e){
            LOGGER.severe("Error al recuperar la key: " + e); //$NON-NLS-1$
        }
        catch(final KeyManagementException e){
            LOGGER.severe("Error con el KeyManager: " + e); //$NON-NLS-1$
        }
		catch (final GeneralSecurityException e) {
			LOGGER.severe("Error durante la carga del almacen de claves SSL: " + e); //$NON-NLS-1$
		}
	}

	/** Obtiene los puertos que se deben probar para la conexi&oacute;n externa.
	 * Asigna cual es la clave.
	 * @param urlParams Par&aacute;metros de la URL de entre los que obtener los puertos.
	 * @return Listados de puertos. */
	private static ChannelInfo getChannelInfo(final Map<String, String> urlParams) {

		final String ps = urlParams.get("ports"); //$NON-NLS-1$
		checkNullParameter(ps, "La URI de invocacion no contiene el parametro 'ports'"); //$NON-NLS-1$

		final String[] portsText = ps.split(","); //$NON-NLS-1$
		final int[] ports = new int[portsText.length];
		for (int i=0; i<portsText.length; i++) {
			try {
				ports[i] = Integer.parseInt(portsText[i]);
			}
			catch(final Exception e) {
				throw new IllegalArgumentException(
					"El parametro 'ports' de la URI de invocacion contiene valores no numericos: " + e //$NON-NLS-1$
				, e);
			}
		}
		final String idSession = urlParams.get(IDSESSION);
		if(idSession != null ){
		    LOGGER.info("Se ha recibido un idSesion para la transaccion: " + idSession); //$NON-NLS-1$
		}
		else {
            LOGGER.info("No se utilizara idSesion durante la transaccion"); //$NON-NLS-1$
        }

		return new ChannelInfo(idSession, ports);
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
				LOGGER.info("Establecido el puerto " + port + " para el servicio Cliente @firma"); //$NON-NLS-1$ //$NON-NLS-2$
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
	private static void checkSupportProtocol(final int protocolVersion) throws UnsupportedProtocolException {
		for (final int version : SUPPORTED_PROTOCOL_VERSIONS) {
			if (version == protocolVersion) {
				return;
			}
		}

		throw new UnsupportedProtocolException(protocolVersion, protocolVersion > CURRENT_PROTOCOL_VERSION);
	}

	private static class ChannelInfo {

		private final String idSession;
		private final int[] ports;

		public ChannelInfo(final String idSession, final int[] ports) {
			this.idSession = idSession;
			this.ports = ports;
		}

		public String getIdSession() {
			return this.idSession;
		}

		public int[] getPorts() {
			return this.ports;
		}
	}
}
