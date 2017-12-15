/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.BindException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLServerSocketFactory;
import javax.net.ssl.SSLSocket;
import javax.swing.Timer;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.mozilla.apple.AppleScript;
import es.gob.afirma.standalone.AutoFirmaUtil;

/** Gestor de la invocaci&oacute;n por <i>socket</i>. */
public final class ServiceInvocationManager {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int READ_BUFFER_SIZE = 2048;

	/** Tama&ntilde;o del segmento de cada fragmento de datos que se lea del <code>socket</code> que se mantendr&aacute;
	 * almacenado por si el segmento de fin de entrada queda dividido entre dos fragmentos. El valor
	 * permite que quepa al completo en un subfragmento la etiqueta <i>EOF</i> y la etiqueta <i>idSession</i> con
	 * su valor. */
	private static final int BUFFERED_SECURITY_RANGE = 36;

	/** N&uacte;mero m&aacute;ximo de intentos de lectura consecutivos en el buffer sin que se encuentren datos. */
	private static final int MAX_READING_BUFFER_TRIES = 10;

	/** Tiempo de espera de cada socket en milisegundos. */
	private static int SOCKET_TIMEOUT = 90000;

	/** M&aacute;ximo numero de caracteres que podemos enviar en una respuesta. */
	private static final int RESPONSE_MAX_SIZE = 1000000;
	//  peticiones que podemos recibir
	private static final String CMD = "cmd="; //$NON-NLS-1$
	private static final String ECHO = "echo="; //$NON-NLS-1$
	private static final String FRAGMENT = "fragment="; //$NON-NLS-1$
	private static final String SEND = "send="; //$NON-NLS-1$
	private static final String SIGN = "firm="; //$NON-NLS-1$
	private static final String AFIRMA = "afirma://" ; //$NON-NLS-1$
	private static final String AFIRMA2 = "afirma://service?" ; //$NON-NLS-1$
	private static final String AFIRMA3 = "afirma://service/?" ; //$NON-NLS-1$
	private static final String SAVE = "afirma://save?"; //$NON-NLS-1$
	private static final String SAVE2 = "afirma://save/?"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la versi&oacute;n del protocolo que se va a utilizar. */
	private static final String PROTOCOL_VERSION_PARAM = "v"; //$NON-NLS-1$

	/** Versi&oacute;n de protocolo m&aacute;s avanzada soportada. */
	private static final int CURRENT_PROTOCOL_VERSION = 1;

	/** Listado de versiones de protocolo soportadas. */
	private static final int[] SUPPORTED_PROTOCOL_VERSIONS = new int[] { CURRENT_PROTOCOL_VERSION };

	// cadenas usadas dentro de las peticiones
	private static final String RESET = "-" ; //$NON-NLS-1$
	private static final String SEPARADOR = "@" ;   //$NON-NLS-1$
	private static final String EOF = SEPARADOR+"EOF" ; //$NON-NLS-1$
	private static final String IDSESSION = "idsession"; //$NON-NLS-1$
	// respuesta que podemos mandar.
	private static final String MORE_DATA_NEED = "MORE_DATA_NEED"; //$NON-NLS-1$
	private static final String OK = "OK"; //$NON-NLS-1$
	private static final String SAVE_OK = "SAVE_OK"; //$NON-NLS-1$
	private static final String CANCEL = "CANCEL"; //$NON-NLS-1$
	private static final String MEMORY_ERROR = "MEMORY_ERROR";//$NON-NLS-1$
	// ip locales para que no nos vengan peticiones externas
	private static final String LOCALHOST = "localhost"; //$NON-NLS-1$
	private static final String LOOP_DIR = "127.0.0.1";  //$NON-NLS-1$
	private static final String LOOP_DIR_2 = "0:0:0:0:0:0:0:1";  //$NON-NLS-1$
	// parametros para carga del certificado SSL
	private static final String KSPASS = "654321"; //$NON-NLS-1$
	private static final String CTPASS = "654321"; //$NON-NLS-1$
	private static final String KEYSTORE_NAME = "autofirma.pfx"; //$NON-NLS-1$
	private static final String PKCS12 = "PKCS12"; //$NON-NLS-1$
	private static final String KEY_MANAGER_TYPE = "SunX509"; //$NON-NLS-1$
	private static final String SSLCONTEXT = "TLSv1"; //$NON-NLS-1$

	/** Temporizador para cerrar la aplicaci&oacute;n cuando pase un tiempo de inactividad. */
	private final static Timer timer = new Timer(SOCKET_TIMEOUT, evt -> {
		LOGGER.warning("Se ha caducado la conexion. Se deja de escuchar en el puerto..."); //$NON-NLS-1$
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			closeMacService();
		}
		System.exit(-4);
	});

	/** Mata el proceso de AutoFirma cuando estamos en macOS. En el resto de sistemas
	 * no hace nada. */
	public static void closeMacService() {
		LOGGER.warning("Ejecuto kill"); //$NON-NLS-1$
		final AppleScript script = new AppleScript(
				"kill -9 $(ps -ef | grep " + idSession + " | awk '{print $2}')"  //$NON-NLS-1$ //$NON-NLS-2$
				);
		try {
			script.run();
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido cerrar la aplicacion: " + e); //$NON-NLS-1$
		}
	}

	/** Coge el foco del sistema en macOS. En el resto de sistemas no hace nada. */
	public static void focusApplication() {
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			final String scriptCode = "tell me to activate"; //$NON-NLS-1$
			final AppleScript script = new AppleScript(scriptCode);
			try {
				script.run();
			}
			catch (final Exception e) {
				LOGGER.warning("Fallo cogiendo el foco en macOS: " + e); //$NON-NLS-1$
			}
		}
	}

	private final static List<String> request = new ArrayList<>();
	private final static List<String> toSend = new ArrayList<>();
	private static int parts = 0;
	private static String idSession ;

	/** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
	private ServiceInvocationManager(){
		// No instanciable
	}

	/** Inicia el servicio. Se intenta establecer un <code>socket</code> que escuche en el puerto pasado por la URL.
	 * @param url URL (debe indicarse el puerto).
	 * @throws UnsupportedProtocolException Si no se soporta el protocolo o la versi&oacute;n de este. */
	static void startService(final String url) throws UnsupportedProtocolException {

		checkSupportProtocol(getVersion(url));

		try {
			// ruta de la que debe buscar el fichero
			final File sslKeyStoreFile = getKeyStoreFile();
			if (sslKeyStoreFile == null) {
				throw new KeyStoreException(
					"No se encuentra el almacen para el cifrado de la comunicacion SSL" //$NON-NLS-1$
				);
			}

			LOGGER.info("Se utilizara el siguiente almacen para establecer el socket SSL: " + sslKeyStoreFile.getAbsolutePath()); //$NON-NLS-1$

			// pass del fichero
			final char ksPass[] = KSPASS.toCharArray();
			final char ctPass[] = CTPASS.toCharArray();
			// generamos el key store desde el fichero del certificado, de tipo PKCS12
			final KeyStore ks = KeyStore.getInstance(PKCS12);
			ks.load(new FileInputStream(sslKeyStoreFile), ksPass);
			// key manager factory de tipo SunX509
			final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KEY_MANAGER_TYPE);
			kmf.init(ks, ctPass);

			final SSLContext sc = SSLContext.getInstance(SSLCONTEXT);
			sc.init(kmf.getKeyManagers(), null, null);
			LOGGER.info("Iniciando servicio local de firma: " + url); //$NON-NLS-1$
			final SSLServerSocketFactory ssocketFactory = sc.getServerSocketFactory();
			final SSLServerSocket ssocket = tryPorts(getPorts(url), ssocketFactory);
			ssocket.setReuseAddress(true);

			// empieza la cuenta atras del temporizador.
			timer.start();

			while (true){
				try {

					final SSLSocket sslSocket = (SSLSocket) ssocket.accept();

					LOGGER.info("Detectada conexion entrante"); //$NON-NLS-1$

					// comprobamos que la direccion es local. Si no es local se descarta la peticion
					if (!isLocalAddress((InetSocketAddress) sslSocket.getRemoteSocketAddress())) {
						sslSocket.close();
						ssocket.close();
						LOGGER.severe(
							"Se ha detectado un acceso no autorizado desde " + //$NON-NLS-1$
								((InetSocketAddress) sslSocket.getRemoteSocketAddress()).getHostString()
						);
						continue;
					}
					new Thread(() ->  {
						try {
							final String httpRequest = read(sslSocket.getInputStream());

							// comprobamos que la peticion no es vacia
							if (httpRequest == null) {
								LOGGER.warning("Se ha recibido una peticion vacia"); //$NON-NLS-1$
							}
							else {
								LOGGER.fine("Peticion HTTP recibida:\n" + httpRequest); //$NON-NLS-1$
								getCommandUri(httpRequest, sslSocket);
							}
						}
						catch (final IllegalArgumentException e) {
							LOGGER.severe(
								"Los parametros recibidos a traves del socket no son validos, se ignorara la peticion: " + e //$NON-NLS-1$
							);
							return;
						}
						catch(final IOException e) {
							LOGGER.severe(
								"Error en el envio a traves del socket: " + e //$NON-NLS-1$
							);
							return;
						}
					}).start();

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
            LOGGER.severe("Error con el algorimto del  certificado: " + e); //$NON-NLS-1$
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

	}

	/** Obtiene el fichero del almac&eacute;n con la clave SSL de alguno de los directorios
	 * del sistema en los que puede estar.
	 * @return Almac&eacute;n de claves o {@code null} si no se encontr&oacute;. */
	private static File getKeyStoreFile() {

		File appDir = AutoFirmaUtil.getApplicationDirectory();

		if (appDir != null && new File(appDir, KEYSTORE_NAME).exists()) {
			return new File(appDir, KEYSTORE_NAME);
		}

		if (Platform.getOS() == Platform.OS.WINDOWS) {
			appDir = AutoFirmaUtil.getWindowsAlternativeAppDir();
			if (appDir != null && new File(appDir, KEYSTORE_NAME).exists()) {
				return new File(appDir, KEYSTORE_NAME);
			}
		}
		else if (Platform.getOS() == Platform.OS.LINUX) {
			appDir = AutoFirmaUtil.getLinuxAlternativeAppDir();
			if (appDir != null && new File(appDir, KEYSTORE_NAME).exists()) {
				return new File(appDir, KEYSTORE_NAME);
			}
		}
		else if (Platform.getOS() == Platform.OS.MACOSX) {
			appDir = AutoFirmaUtil.getMacOsXAlternativeAppDir();
			if (new File(appDir, KEYSTORE_NAME).exists()) {
				return new File(appDir, KEYSTORE_NAME);
			}
		}

		return null;
	}

	/** Manda los datos de respuesta a la aplicaci&oacute;n.
	 * @param response Respuesta al env&iacute;o.
	 * @param socket <code>Socket</code> a donde mandar la respuesta.
	 * @param petition Petici&oacute;n que se manda, para registrarla en el <i>log</i>.
	 * @throws IOException Si hay errores en el env&iacute;o. */
	private static void sendData(final byte[] response, final Socket socket, final String petition) throws IOException {
		socket.getOutputStream().write(response);
		socket.getOutputStream().flush();
		LOGGER.info("Mandando respuesta a la peticion: " + petition);  //$NON-NLS-1$
		// volvemos a activar el timer
		timer.restart();
	}

	/** Crea una respuesta HTTP para enviar a traves del <i>socket</i>.
	 * @param ok Indica si la operacion finaliz&oacute; bien o mal.
	 * @param response La respuesta que se mandar&aacute; en el HTTP.
	 * @return Devuelve el byte array con la respuesta en formato HTTP. */
	private static byte[] createHttpResponse(final boolean ok, final String response) {
		final StringBuilder sb = new StringBuilder();
		if (ok) {
			sb.append("HTTP/1.1 200 OK\n"); //$NON-NLS-1$
		}
		else  {
			sb.append("HTTP/1.1 500 Internal Server Error"); //$NON-NLS-1$
		}
		sb.append("Connection: close\n"); //$NON-NLS-1$
		sb.append("Pragma: no-cache\n"); //$NON-NLS-1$
		sb.append("Server: Cliente @firma\n"); //$NON-NLS-1$
		sb.append("Content-Type: text/html; charset=utf-8\n"); //$NON-NLS-1$
		sb.append("Access-Control-Allow-Origin: *\n"); //$NON-NLS-1$
		sb.append('\n');
		if (response != null) {
			sb.append(Base64.encode(response.getBytes(), true));
		}
		return sb.toString().getBytes();
	}

	/** Obtiene los puertos que se deben probar para la conexi&oacute;n externa.
	 * Asigna cual es la clave.
	 * @param url URL de la que extraer los puertos.
	 * @return Listados de puertos. */
	private static int[] getPorts(final String url) {
		final URI u;
		try {
			u = new URI(url);
		}
		catch (final Exception e) {
			throw new IllegalArgumentException("La URI (" + url + ") de invocacion no es valida: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		final String query = u.getQuery();
		checkNullParameter(query, "La URI de invocacion no contiene parametros: " + url); //$NON-NLS-1$
		final Properties p = new Properties();
		try {
			p.load(new ByteArrayInputStream(query.replace("&", "\n").getBytes())); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final IOException e) {
			throw new IllegalArgumentException(
				"Los parametros de la URI de invocacion no estan el el formato correcto: " + url, //$NON-NLS-1$
				e);
		}
		final String ps = p.getProperty("ports"); //$NON-NLS-1$
		checkNullParameter(ps, "La URI de invocacion no contiene el parametro 'ports': " + url); //$NON-NLS-1$
		final String[] ports = ps.split(","); //$NON-NLS-1$
		final int[] ret = new int[ports.length];
		for (int i=0; i<ports.length; i++) {
			try {
				ret[i] = Integer.parseInt(ports[i]);
			}
			catch(final Exception e) {
				throw new IllegalArgumentException(
					"El parametro 'ports' de la URI de invocacion contiene valores no numericos: " + e //$NON-NLS-1$
				, e);
			}
		}
		idSession = p.getProperty(IDSESSION);
		if(idSession != null ){
		    LOGGER.info("Se ha recibido un idSesion para la transaccion: " + idSession); //$NON-NLS-1$
		}
		else {
            LOGGER.info("No se utilizara idSesion durante la transaccion"); //$NON-NLS-1$
        }
		return ret;
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

	/** Lee los datos recibidos en el <i>socket</i>. Si se recibe una petici&oacute;n sin datos o compuesto de
	 * caracteres blancos (como ocurre cuando el certificado SSL no es correcto) se devuelve nulo.
	 * @param socketIs Flujo de entrada de datos del <i>socket</i>.
	 * @return Los datos recibidos o {@code null} si se recibe una petici&oacute;n vac&iacute;a.
	 * @throws IOException Si ocurren errores durante la lectura. */
	private static String read(final InputStream socketIs) throws IOException {
		// Buffer en el que se ira almacenando toda la entrada
		final StringBuilder data = new StringBuilder();
		// Cadena que se ira actualizando para contener siempre la union de los
		// 2 ultimos fragmentos con un tamano suficiente para contener el
		// fragmento de fin y el identificador de sesion
		String subFragment = ""; //$NON-NLS-1$
		final byte[] reqBuffer = new byte[READ_BUFFER_SIZE];
		boolean readed = true;
		String insert;
		// Limite alcanzado de lecturas vacias del socket
		int readingTries;
		while (readed) {

			// Leemos del socket hasta obtener algo mas que caracteres vacios
			readingTries = 0;
			do {
				readingTries++;
				socketIs.read(reqBuffer);
				insert = new String(reqBuffer, StandardCharsets.UTF_8);
			} while (insert.trim().isEmpty() && readingTries <= MAX_READING_BUFFER_TRIES);

			// Para evitar un error de memoria, si llegamos al maximo numero de intentos en el
			// que solo hemos leido caracteres vacios, entendemos que era una peticion invalida
			// y develvemos nulo para que se ignore
			if (readingTries > MAX_READING_BUFFER_TRIES) {
				return null;
			}

			subFragment += insert.substring(0, Math.min(BUFFERED_SECURITY_RANGE, insert.length()));

			// Comprobamos si la etiqueta de fin se encuentra en el nuevo fragmento o si esta a medias
			// entre este y el anterior. En base a eso, tendremos que agregar o quitar datos del buffer
			if (subFragment.indexOf(EOF) != -1 || insert.indexOf(EOF) != -1) {

				readed = false;

				final boolean findOnSubFragment;
				int eofPos;
				int idSessionPos;
				String requestSessionId = null;

				// Si se detecta el fragmento de fin en la union de ambos fragmentos,
				// comprobamos si parte del contenido del buffer sobra o si hay algo que agregar
				if (subFragment.indexOf(EOF) != -1) {
					findOnSubFragment = true;
					eofPos = subFragment.indexOf(EOF);
					idSessionPos = subFragment.indexOf(IDSESSION);
					if (idSessionPos != -1) {
						requestSessionId = subFragment.substring(
								idSessionPos + IDSESSION.length() + 1,
								eofPos);
					}
				}
				else {
					findOnSubFragment = false;
					eofPos = insert.indexOf(EOF);
					idSessionPos = insert.indexOf(IDSESSION);
					if (idSessionPos != -1) {
						requestSessionId = insert.substring(
								idSessionPos + IDSESSION.length() + 1,
								eofPos);
					}
				}

				// Comprobamos que el id de sesion transmitido es correcto
				checkIdSession(requestSessionId);

				if (findOnSubFragment) {
					// Si el id de session es anterior al rango de seguridad, en el buffer de
					// la peticion se habra introducido parte de este identificador y hay que
					// borrarlos.
					if (idSessionPos != -1 && idSessionPos < BUFFERED_SECURITY_RANGE) {
						data.replace(data.length() - (BUFFERED_SECURITY_RANGE - idSessionPos), data.length(), ""); //$NON-NLS-1$
					}
					// Si la posicion del EOF es anterior al rango de seguridad, entonces
					// en el buffer de la peticion se ha introducido parte de esta etiqueta
					// y hay que borrarla.
					else if (eofPos < BUFFERED_SECURITY_RANGE) {
						data.replace(data.length() - (BUFFERED_SECURITY_RANGE - eofPos), data.length(), ""); //$NON-NLS-1$
					}
					// Si tanto el EOF como el id de sesion (en caso de haber) son posteriores
					// al rango de seguridad, solo habra que incorporar los datos encontrados
					// antes de estas posiciones (siempre y cuando no esten al principio, en
					// cuyo caso, no hay que agregar nada)
					else if (idSessionPos > 0 || eofPos > 0) {
						data.append(subFragment.substring(0, idSessionPos > -1 ? idSessionPos : eofPos));
					}
				}
				else {
					data.append(insert.substring(0, idSessionPos > -1 ? idSessionPos : eofPos));
				}
			}
			else {
				// Agregamos los datos al total, ya que no se ha encontrado la cadena de fin de lectura
				data.append(insert);
				subFragment = insert.substring(Math.max(0, insert.length() - BUFFERED_SECURITY_RANGE));
			}
		}
		return data.toString();
	}

	/** Comprueba que la direcci&oacute;n que intenta conectarse es local.
	 * @param a Direcci&oacute;n a probar.
	 * @return <code>true</code> si es direcci&oacute;n local, <code>false</code> en caso contrario. */
	private static boolean isLocalAddress(final InetSocketAddress a) {
		final String hostString = a.getHostString();
		if (LOOP_DIR_2.equals(hostString) ||
			LOOP_DIR.equals(hostString) ||
			LOCALHOST.equals(hostString)) {
			return true;
		}
		return false;
	}

	/** Analiza cual el <i>ComandUri</i> de la petici&oacute;n recibida y realiza sus operaciones pertinentes.
	 * @param httpRequest Petici&oacute;n http recibida de la que hay que extraer el <i>command uri</i>.<br>
	 * Las peticiones permitidas son:
	 * <ul>
	 * 		<li>cmd= Iniciar una operaci&oacute;n que viene sin fragmentar.</li>
	 * 		<li>echo= Petici&oacute;n echo para comprobar que la aplicaci&oacute;n esta lista</li>
	 * 		<li>fragment= Inicia el env&iacute;o de los datos fragment&aacute;ndolos en varias peticiones.</li>
	 * 		<li>firm= Inicia una operaci&oacute;n juntando los datos fragmentados de las peticiones anteriores.</li>
	 * 		<li>send= Env&iacute;a la respuesta de la una operaci&oacute;n realizada. Si es demasiado grande se fragmenta en varios env&iacute;os.</li>
	 * </ul>
	 * @param socket El <code>Socket</code> en el que se escucha la petici&oacute;n.
	 * @throws IOException Si hay error en el tratamiento de datos. */
	private static void getCommandUri(final String httpRequest, final Socket socket) throws IOException {
		checkNullParameter(httpRequest, "Los datos recibidos por HTTP son nulos"); //$NON-NLS-1$
		final String uriType = getUriTypeFromRequest(httpRequest);
		LOGGER.info("Recibido comando de tipo: " + uriType); //$NON-NLS-1$

		try {
			switch (uriType) {

				case ECHO:
					doEchoPetition(httpRequest.substring(httpRequest.indexOf(ECHO) + ECHO.length()), socket);
					break;

				case CMD:
					doCmdPetition(httpRequest.substring(httpRequest.indexOf(CMD) + CMD.length()), socket);
					break;

				case FRAGMENT:
					doFragmentPetition(httpRequest.substring(httpRequest.indexOf(FRAGMENT) + FRAGMENT.length()), socket);
					break;

				case SIGN:
					doFragmentedProcess(socket);
					break;

				case SEND:
					doSendPetition(httpRequest.substring(httpRequest.indexOf(SEND) + SEND.length()), socket);
					break;

				default: // Nunca deberia entrar aqui
					throw new IllegalStateException("Comando no permitido: " + uriType); //$NON-NLS-1$
			}
		}
        catch (final OutOfMemoryError e){
            LOGGER.severe("Se ha producido un error por falta memoria de la maquina virtual: " + e); //$NON-NLS-1$
            sendData(createHttpResponse(true, MEMORY_ERROR), socket, "Error de memoria"); //$NON-NLS-1$
        }
		catch(final Exception e) {
			throw new IllegalArgumentException(
				"Error al procesar el comando de tipo '" + uriType + "': " + e, e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		socket.close();
	}

	/** Devuelve el <code>uriType</code> de la petici&oacute;n recibida. Lanza una excepci&oacute;n en caso
	 * de que la petici&oacute;n no sea petici&oacute;n.
	 * @param httpRequest La petici&oacute;n a tratar.
	 * @return String <i>uriType</i> de la petici&oacute;n. */
	private static String getUriTypeFromRequest(final String httpRequest) {
		final String[] supportedUriTypes = new String[] {CMD, ECHO, FRAGMENT, SIGN, SEND};

		int i = 0;
		String uriType = null;
		while (uriType == null && i < supportedUriTypes.length) {
			if (httpRequest.indexOf(supportedUriTypes[i]) != -1) {
				uriType = supportedUriTypes[i];
			}
			i++;
		}
		if (uriType == null) {
			throw new IllegalArgumentException(
				"Los datos recibidos por HTTP no contienen comando reconocido: " + httpRequest //$NON-NLS-1$
			);
		}
		return uriType;
	}

	/** Realiza las acciones pertinentes en caso de que la petici&oacute;n contenga una petici&oacute;n <i>echo</i>.
	 * Se reinician las variables de control de la aplicaci&oacute;n para desechar cualquier petici&oacute;n anterior.
	 * @param cmd Comando URI.
	 * @param socket <i>Socket</i> donde se recibe la petici&oacute;n.
	 * @throws IOException Si hay error en el env&iacute;o de datos. */
	private static void doEchoPetition(final String cmd, final Socket socket) throws IOException {
		// paramos el timer mientras la aplicacion realiza operaciones
		timer.stop();
		// solo reseteamos las variables de control si la peticion es echo=- en caso de que sea un echo= simplemente respondemos
		if (cmd.contains(RESET)){
			reset();
		}

		LOGGER.info("Comando URI recibido por HTTP: " + ECHO); //$NON-NLS-1$
		sendData(createHttpResponse(true, OK), socket, ECHO);
	}

	/** Realiza la operaci&oacute;n que corresponda cuando ya se han recibido todos los
	 * fragmentos de la petici&oacute;n (<code>firm=</code>).
	 * @param socket <code>Socket</code> donde se recibe la petici&oacute;n.
	 * @throws IOException Si hay error en la lectura o env&iacute;o de datos. */
	private static void doFragmentedProcess(final Socket socket) throws IOException {
		// paramos el timer mientras la aplicacion realiza operaciones
		boolean isSave = false;
	    timer.stop();
		LOGGER.info("Comando URI recibido por HTTP: " + SIGN); //$NON-NLS-1$
		// en caso de que sea la primera vez que lo ejecutamos, realizamos la operacion.
		// si la respuesta no llega al JS y vuelve a realizar la misma peticion, ya tenemos
		// formada la respuesta y solo hay que devolver el numero de peticiones.
		if (toSend.isEmpty()) {
			final StringBuilder totalhttpRequest = new StringBuilder();
			for(final String object: request){
				totalhttpRequest.append(object);
				LOGGER.info("PETICION PROCESADA: " + totalhttpRequest); //$NON-NLS-1$
			}

			// Si la operacion es de guardado (Respuesta fija)
			if (totalhttpRequest.toString().startsWith("afirma://save?") || totalhttpRequest.toString().startsWith("afirma://save/?")){ //$NON-NLS-1$  //$NON-NLS-2$
			    isSave = true;
				final String operationResult = ProtocolInvocationLauncher.launch(totalhttpRequest.toString(), true);
				if (operationResult.equals(OK)){
					sendData(createHttpResponse(true, SAVE_OK), socket, "Operacion save realizada con exito"); //$NON-NLS-1$
				}
				else if (operationResult.equals(CANCEL)){
				    sendData(createHttpResponse(true, CANCEL), socket, "Cancelado por el usuario"); //$NON-NLS-1$
				}
				else {
					throw new IllegalArgumentException(
						"Error al realizar la operacion save" //$NON-NLS-1$
					);
				}
			}
			// Si hay que devolver el valor obtenido
			else {
				final String operationResult = ProtocolInvocationLauncher.launch(totalhttpRequest.toString(), true);
				calculateNumberPartsResponse(operationResult);
			}
		}
		else {
			LOGGER.info("Se habia calculado el numero de partes anteriormente"); //$NON-NLS-1$
		}
		// si no es una operacion save y nos vuelven a pedir una parte.
		if (!isSave){
		    sendData(
	    		createHttpResponse(true, Integer.toString(parts)), socket, "Se mandaran " + parts + " partes"  //$NON-NLS-1$//$NON-NLS-2$
    		);
		}
	}

	/** Realiza las acciones pertinentes en caso de que la petici&oacute;n contenga una petici&oacute;n <code>cmd=</code>.
	 * @param cmd Valor del par&aacute;metro <code>cmd</code>.
	 * @param socket <i>Socket</i> donde se recibe la petici&oacute;n.
	 * @throws IOException Error en la lectura o en el env&iacute;o de datos. */
	private static void doCmdPetition (final String cmd, final Socket socket) throws IOException{
		final String cmdUri = new String(Base64.decode(cmd.trim(), true));
		if (cmdUri.startsWith(AFIRMA) && !(cmdUri.startsWith(AFIRMA2) || cmdUri.startsWith(AFIRMA3))) {
			// paramos el timer mientras la aplicacion realiza operaciones
			timer.stop();
			LOGGER.info("Comando URI recibido por HTTP: " + cmdUri); //$NON-NLS-1$
			if (cmdUri.startsWith(SAVE) || cmdUri.startsWith(SAVE2)) {
				final String operationResult = ProtocolInvocationLauncher.launch(cmdUri.toString(), true);
				if (operationResult.equals(OK)) {
					sendData(createHttpResponse(true, SAVE_OK), socket, "save"); //$NON-NLS-1$
				}
				else if (operationResult.equals(CANCEL)) {
                    sendData(createHttpResponse(true, CANCEL), socket, "Cancelado por el usuario"); //$NON-NLS-1$
                }
				else {
					throw new IllegalArgumentException(
						"Error al realizar la operacion 'save'" //$NON-NLS-1$
					);
				}
			}
			else {
				if (toSend.isEmpty()){
					// Usamos la url que acabamos de recibir sin fragmentar
					final String operationResult = ProtocolInvocationLauncher.launch(cmdUri.toString(), true);
					calculateNumberPartsResponse(operationResult);
				}
				sendData(createHttpResponse(true, Integer.toString(parts)), socket, "Se mandaran " + parts + " partes"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		else {
			throw new IllegalArgumentException(
				"Los datos recibidos en el parametro 'cmd' por HTTP no son una URI del tipo 'afirma://': " + cmdUri //$NON-NLS-1$
			);
		}
	}

	/** Realiza las acciones pertinentes en caso de que la petici&oacute;n contenta una petici&oacute;n <code>fragment=</code>.
	 * @param fragment httpRequest URL de la que hay que substraer el commandUri.
	 * @param socket <code>Socket</code> donde se recibe la petici&oacute;n.
	 * @throws IOException Si hay error en el tratamiento de datos. */
	private static void doFragmentPetition (final String fragment, final Socket socket) throws IOException{

		// paramos el timer mientras la aplicacion realiza operaciones
		timer.stop();
		LOGGER.info("Comando URI recibido por HTTP: " + fragment); //$NON-NLS-1$
		final String[] petition = fragment.split(SEPARADOR);
		final int part = Integer.parseInt(petition [1]);
		final int partTotal = Integer.parseInt(petition [2]);
		final String save = new String (Base64.decode(petition[3].trim(), true));

		if (request.size() == part ){
			LOGGER.info("sustituimos la parte " + part); //$NON-NLS-1$
			request.set(part-1, save);
		}
		else {
			LOGGER.info("insertamos la parte " + part); //$NON-NLS-1$
			request.add(part-1, save);
		}
		if (part == partTotal){
			sendData(createHttpResponse(true, OK), socket, "Mandada la ultima parte " + part +"de " + partTotal); //$NON-NLS-1$ //$NON-NLS-2$
		}
		else {
			sendData(createHttpResponse(true, MORE_DATA_NEED), socket, "Mandar resto de datos de la firma, parte " + part + "de " + partTotal);  //$NON-NLS-1$//$NON-NLS-2$
		}
	}

	/** Realiza el env&iacute;o de datos.
	 * @param send Configuraci&oacute;n para el env&iacute;o de datos.
	 * @param socket <i>Socket</i> donde se recibe la petici&oacute;n.
	 * @throws IOException Si hay error en el tratamiento de datos. */
	private static void doSendPetition (final String send, final Socket socket) throws IOException {

		// paramos el timer mientras la aplicacion realiza operaciones
		timer.stop();
		LOGGER.info("Comando URI recibido por HTTP: " + send); //$NON-NLS-1$
		final String[] petition = send.split(SEPARADOR);
		final int part = Integer.parseInt(petition [1]);
		final int partTotal = Integer.parseInt(petition [2]);
		if (part < 1 || part > partTotal) {
			throw new IllegalArgumentException(
				"Se ha solicitado enviar un fragmento invalido: " + part + "de " + partTotal //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		sendData(createHttpResponse(true, toSend.get(part-1)), socket, "Mandada la parte " + part + " de " + partTotal); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Calcula en cuantas partes hay que realizar el env&iacute;o de la operaci&oacute;n y divide la respuesta en dichas partes.
	 * @param operationResult La operaci&oacute;n resultante que hay que dividir. */
	private static void calculateNumberPartsResponse(final String operationResult) {

		parts = (int) Math.ceil((double)operationResult.length() / (double) RESPONSE_MAX_SIZE);
		LOGGER.info("Se mandaran " + parts + "partes");  //$NON-NLS-1$//$NON-NLS-2$
		LOGGER.info("tam total=" + operationResult.length()); //$NON-NLS-1$
		// si recibimos la misma peticion otra vez ya tenemos los datos preparados, solo devolvemos las peticiones
		int offset;
		for (int i = 0; i < parts; i++){
			offset = RESPONSE_MAX_SIZE * i;
			toSend.add(operationResult.substring(offset, Math.min(offset + RESPONSE_MAX_SIZE, operationResult.length())));
			LOGGER.info("Tamano de la parte " + (i+1) + " =" + toSend.get(i).length()); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/** Comprueba que un par&aacute;metro no sea nulo.
	 * @param parameter Par&aacute;metro que se debe comprobar que no sea nulo.
	 * @param excepcionText Texto que se debe lanzar con la excepci&oacute;n. */
	private static void checkNullParameter (final Object parameter, final String excepcionText){
		if (parameter == null) {
			throw new IllegalArgumentException(excepcionText);
		}
	}

	/** Reinicia las variables de control si se realiza una nueva llamada y hay que descartar
	 * todas las las operaciones pendientes. */
	private static void reset(){
		request.clear();
		toSend.clear();
		parts = 0;
	}

	/** Comprueba que el <code>idSession</code> de la petici&oacute;n recibida coincida con el
	 * <code>idSession</code> generado al abrir la aplicaci&oacute;n por <i>socket</i>.
	 * @param requestSessionId Identificador de sesi&oacute;n enviado en la petici&oacute;n. */
	private static void checkIdSession (final String requestSessionId) {
		// se esperaba un idSession y no se ha recibido
		if (idSession != null && !idSession.equals(requestSessionId)) {
			throw new IllegalArgumentException("No se ha recibido el idSession esperado."); //$NON-NLS-1$
		}
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
}
