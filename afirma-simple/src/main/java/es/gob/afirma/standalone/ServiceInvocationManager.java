package es.gob.afirma.standalone;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.BindException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.SocketTimeoutException;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Base64;

final class ServiceInvocationManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int READ_BUFFER_SIZE = 2048;

	/** M&aacute;ximo tiempo de espera entre operaciones. */
	private static int MAX_WAITING_TIME = 60000;

	/** Tiempo de espera de cada socket. */
	private static int SOCKET_TIMEOUT = 15000;

	/** Momento en el que se realiz&oacute; la operaci&oacute;n anterior. */
	private static long lastOperationTime = 0;

	static void startService(final String url) {

		LOGGER.info("Iniciando servicio local de firma...: " + url); //$NON-NLS-1$


		try {
			try ( final ServerSocketChannel serverSocketChannel = ServerSocketChannel.open(); ) {

				tryPorts(getPorts(url), serverSocketChannel.socket());
				serverSocketChannel.configureBlocking(true);

				while(!isExpiratedExecution()){

					LOGGER.info("Quedamos a la espera de una llamada por socket"); //$NON-NLS-1$

					try ( final SocketChannel socketChannel = serverSocketChannel.accept(); ) {

						LOGGER.info("Detectada conexion entrante"); //$NON-NLS-1$

						if (!isLocalAddress((InetSocketAddress) socketChannel.getRemoteAddress())) {
							socketChannel.close();
							serverSocketChannel.close();
							LOGGER.severe("Se ha detectado un acceso no autorizado desde " + //$NON-NLS-1$
								 ((InetSocketAddress) socketChannel.getRemoteAddress()).getHostString());
							continue;
						}

						LOGGER.info("Aceptada conexion desde: " + socketChannel); //$NON-NLS-1$
						socketChannel.configureBlocking(false);

						final String commandUri;
						try {

							final String httpRequest = read(socketChannel);
							if (httpRequest.trim().length() == 0) {
								LOGGER.warning("Se recibe una peticion HTTP vacia"); //$NON-NLS-1$
								continue;
							}
							LOGGER.info("Peticion HTTP recibida:\n" + httpRequest); //$NON-NLS-1$

							commandUri = getCommandUri(httpRequest);
						}
						catch (final IllegalArgumentException e) {
							LOGGER.warning("Los parametros recibidos a traves del socket no son validos, se ignorara la peticion: " + e); //$NON-NLS-1$
							continue;
						}
						LOGGER.info("Comando URI recibido por HTTP: " + commandUri); //$NON-NLS-1$
						if (commandUri.startsWith("afirma://service?") || commandUri.startsWith("afirma://service/?")) { //$NON-NLS-1$ //$NON-NLS-2$
							LOGGER.warning("Invocacion recursiva para la apertura del servicio, se ignorara:\n" + commandUri); //$NON-NLS-1$
							continue;
						}


						// TODO: Hay que incorporar el soporte de distintos tipos de llamadas.
						// Primeramente, una llamada de tipo echo

						final String operationResult = ProtocolInvocationLauncher.launch(commandUri);

						// Gestion de la respuesta
						final byte[] response = createHttpResponse(
								operationResult != null && !operationResult.startsWith("SAF_"), //$NON-NLS-1$
								operationResult != null ? operationResult : "NULL" //$NON-NLS-1$
								);

						LOGGER.info("Resultado: " + operationResult); //$NON-NLS-1$

						updateLastAccess();

						final ByteBuffer bb = ByteBuffer.allocate(response.length);
						bb.clear();
						bb.put(response);
						bb.flip();
						socketChannel.write(bb);

					}
					catch (final SocketTimeoutException e) {
						LOGGER.info("Tiempo de espera del socket terminado"); //$NON-NLS-1$
					}
				}
				LOGGER.warning("Se ha caducado la conexion. Se deja de escuchar en el puerto..."); //$NON-NLS-1$
			}

		}

		catch (final IOException e) {
			// No hacemos nada ya que no tenemos forma de transmitir el error de vuelta y no
			// debemos mostrar dialogos graficos
			LOGGER.severe("Ocurrio un error en la comunicacion a traves del socket: " + e); //$NON-NLS-1$
			e.printStackTrace();
		}

	}

	/**
	 * Comprueba si el servicio ya ha sobrepasado el tiempo m&aacute;ximo de espera.
	 * @return {@code true} si se ha sobrepasado
	 */
	private static boolean isExpiratedExecution() {
		if (lastOperationTime == 0) {
			updateLastAccess();
		}
		return lastOperationTime + MAX_WAITING_TIME < new Date().getTime();
	}

	/**
	 * Actualiza la hora del &uacute;ltimo acceso al servicio para que el tiempo de
	 * expiraci&oacute;n l&iacute;mite se calcule a partir de esta.
	 */
	private static void updateLastAccess() {
		lastOperationTime = new Date().getTime();
	}

	/**
	 * Crea una respuesta HTTP para enviar a traves del socket.
	 * @param ok Indica si la operacion finaliz&oacute; bien o mal.
	 * @param response
	 * @return
	 */
	private static byte[] createHttpResponse(final boolean ok, final String response) {
		final StringBuilder sb = new StringBuilder();
		if (ok) {
			sb.append("HTTP/1.1 200 OK\n"); //$NON-NLS-1$
		}
		else  {
			sb.append("HTTP/1.1 500 Internal Server Error"); //$NON-NLS-1$
		}
		sb.append("Connection: close\n"); //$NON-NLS-1$
		sb.append("Server: Cliente @firma\n"); //$NON-NLS-1$
		sb.append("Access-Control-Allow-Origin: *\n"); //$NON-NLS-1$
		sb.append('\n');
		if (response != null) {
			sb.append(Base64.encode(response.getBytes(), true));
		}
		return sb.toString().getBytes();
	}

	/** Obtiene los puertos que se deben probar para la conexi&oacute;n externa.
	 * @param url URL de la que extraer los puertos.
	 * @return Listados de puertos. */
	private static int[] getPorts(final String url) {
		if (url == null) {
			throw new IllegalArgumentException("La URI de invocacion no puede ser nula"); //$NON-NLS-1$
		}
		final URI u;
		try {
			u = new URI(url);
		}
		catch (final Exception e) {
			throw new IllegalArgumentException("La URI de invocacion no es valida: " + url); //$NON-NLS-1$
		}
		final String query = u.getQuery();
		if (query == null) {
			throw new IllegalArgumentException("La URI de invocacion no contiene parametros: " + url); //$NON-NLS-1$
		}
		final Properties p = new Properties();
		try {
			p.load(new ByteArrayInputStream(query.replace("&", "\n").getBytes())); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final IOException e) {
			throw new IllegalArgumentException(
				"Los parametros de la URI de invocacion no estan el el formato correcto: " + url //$NON-NLS-1$
			);
		}
		final String ps = p.getProperty("ports"); //$NON-NLS-1$
		if (ps == null) {
			throw new IllegalArgumentException("La URI de invocacion no contiene el parametro 'ports': " + url); //$NON-NLS-1$
		}
		final String[] ports = ps.split(","); //$NON-NLS-1$
		final int[] ret = new int[ports.length];
		for (int i=0; i<ports.length; i++) {
			try {
				ret[i] = Integer.parseInt(ports[i]);
			}
			catch(final Exception e) {
				throw new IllegalArgumentException(
					"El parametro 'ports' de la URI de invocacion contiene valores no numericos: " + e //$NON-NLS-1$
				);
			}
		}
		return ret;
	}

	private static void tryPorts(final int[] ports, final ServerSocket socket) throws IOException {
		if (ports == null) {
			throw new IllegalArgumentException("La lista de puertos no puede ser nula"); //$NON-NLS-1$
		}
		if (socket == null) {
			throw new IllegalArgumentException("El socket servidor no puede ser nulo"); //$NON-NLS-1$
		}
		for (final int port : ports) {
			try {
				socket.setSoTimeout(SOCKET_TIMEOUT);
				socket.bind(new InetSocketAddress(port));
				LOGGER.info("Establecido el puerto " + port + " para el servicio Cliente @firma"); //$NON-NLS-1$ //$NON-NLS-2$
				return;
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

	private static String read(final SocketChannel channel) throws IOException {

		final StringBuilder response = new StringBuilder();

		final ByteBuffer buffer = ByteBuffer.allocate(READ_BUFFER_SIZE);
		buffer.clear();
		final Charset charset = Charset.forName("UTF-8"); //$NON-NLS-1$

		int readed = 1;
		while (readed > 0) {
		 readed = channel.read(buffer);
		 buffer.flip();
		 response.append(charset.decode(buffer));
		 buffer.clear();
		}

		LOGGER.info("Datos recibidos en el socket servidor:\n" + response ); //$NON-NLS-1$

		return response.toString();
	}

	private static boolean isLocalAddress(final InetSocketAddress a) {
		final String hostString = a.getHostString();
		if ("0:0:0:0:0:0:0:1".equals(hostString) || //$NON-NLS-1$
			"127.0.0.1".equals(hostString) || //$NON-NLS-1$
			"localhost".equals(hostString)) { //$NON-NLS-1$
			return true;
		}
		return false;
	}

	private static String getCommandUri(final String httpRequest) {
		if (httpRequest == null) {
			throw new IllegalArgumentException(
				"Los datos recibidos por HTTP son nulos" //$NON-NLS-1$
			);
		}
		final int pos = httpRequest.indexOf("cmd="); //$NON-NLS-1$
		if (pos == -1) {
			throw new IllegalArgumentException(
				"Los datos recibidos por HTTP no contienen un parametro 'cmd': " + httpRequest //$NON-NLS-1$
			);
		}

		final String cmdUri;
		try {
			cmdUri = new String(Base64.decode(httpRequest.substring(pos + "cmd=".length()).trim(), true)); //$NON-NLS-1$
		}
		catch(final Exception e) {
			throw new IllegalArgumentException(
				"Los datos recibidos en el parametro 'cmd' por HTTP no estan en Base64: " + e //$NON-NLS-1$
			);
		}
		if (!cmdUri.startsWith("afirma://")) { //$NON-NLS-1$
			throw new IllegalArgumentException(
				"Los datos recibidos en el parametro 'cmd' por HTTP no son una URI del tipo 'afirma://': " + cmdUri //$NON-NLS-1$
			);
		}
		return cmdUri;
	}

	public static void main(final String[] args) {

		final String url = "afirma://service/?ports=59188,58339,64805&amp;idsession=001599791810";

		startService(url);
	}
}
