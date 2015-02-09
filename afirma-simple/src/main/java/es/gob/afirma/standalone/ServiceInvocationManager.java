package es.gob.afirma.standalone;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.BindException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.charset.Charset;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Base64;

final class ServiceInvocationManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int READ_BUFFER_SIZE = 2048;

	static void startService(final String url) {

		LOGGER.info("Iniciando servicio local de firma..."); //$NON-NLS-1$

		try {
			final ServerSocketChannel serverSocketChannel = ServerSocketChannel.open();
			tryPorts(getPorts(url), serverSocketChannel.socket());
			serverSocketChannel.configureBlocking(true);

			final SocketChannel socketChannel = serverSocketChannel.accept();

			if (!isLocalAddress((InetSocketAddress) socketChannel.getRemoteAddress())) {
				socketChannel.close();
				serverSocketChannel.close();
				throw new SecurityException(
					"Se ha detectado un acceso no autorizado desde " + ((InetSocketAddress) socketChannel.getRemoteAddress()).getHostString() //$NON-NLS-1$
				);
			}

			LOGGER.info("Aceptada conexion desde: " + socketChannel); //$NON-NLS-1$
			socketChannel.configureBlocking(false);

			final String commandUri = getCommandUri(read(socketChannel));
			LOGGER.info("Comando URI recibido por HTTP: " + commandUri); //$NON-NLS-1$
			if (commandUri.startsWith("afirma://service?") || commandUri.startsWith("afirma://service/?")) { //$NON-NLS-1$ //$NON-NLS-2$
				//TODO: No permitir acceso recursivo
			}

			final String operationResult = ProtocolInvocationLauncher.launch(commandUri);

			// Gestion de la respuesta
			final byte[] response = createHttpResponse(
				operationResult != null && !operationResult.startsWith("SAF_"), //$NON-NLS-1$
				operationResult != null ? operationResult : "NULL" //$NON-NLS-1$
			);

			final ByteBuffer bb = ByteBuffer.allocate(response.length);
			bb.clear();
			bb.put(response);
			bb.flip();
			socketChannel.write(bb);

			socketChannel.close();
			serverSocketChannel.close();

		}
		catch (final IOException e) {
			// No hacemos nada ya que no tenemos forma de transmitir el error de vuelta y no
			// debemos mostrar dialogos graficos
			LOGGER.severe("Ocurrio un error en la comunicacion a traves del socket: " + e); //$NON-NLS-1$
			e.printStackTrace();
		}

	}

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

	/**
	 * Obtiene los puertos que se deben probar para la conexi&oacute;n externa.
	 * @param url URL de la que extraer los puertos.
	 * @return Listados de puertos.
	 */
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

	private static String getCommandUri(final String httpResponse) {
		if (httpResponse == null) {
			throw new IllegalArgumentException(
				"Los datos recibidos por HTTP son nulos" //$NON-NLS-1$
			);
		}
		if (!httpResponse.contains("cmd=")) { //$NON-NLS-1$
			throw new IllegalArgumentException(
				"Los datos recibidos por HTTP no contienen un parametro 'cmd'" //$NON-NLS-1$
			);
		}

		final String cmdUri;
		try {
			cmdUri = new String(Base64.decode(httpResponse.split("cmd=")[1].trim(), true)); //$NON-NLS-1$
		}
		catch(final Exception e) {
			throw new IllegalArgumentException(
				"Los datos recibidos en el parametro 'cmd' por HTTP no estan en Base64: " + e //$NON-NLS-1$
			);
		}
		if (!cmdUri.startsWith("afirma://")) { //$NON-NLS-1$
			throw new IllegalArgumentException(
				"Los datos recibidos en el parametro 'cmd' por HTTP no son una URI del tipo 'afirma://'" //$NON-NLS-1$
			);
		}
		return cmdUri;
	}
}
