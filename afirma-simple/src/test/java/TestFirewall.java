import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.net.ServerSocketFactory;
import javax.net.SocketFactory;


/** Prueba simple de conexi&oacute;n a <i>socket</i> local.
 * Para comprobar si el cortafuegos bloquea o no la conexi&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s.*/
public final class TestFirewall {

	private static final int PORT = 6629;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Main para pruebas.
	 * @param args No se usa.
	 * @throws Exception En cualquier error no controlado. */
	public static void main(final String[] args) throws Exception {
		new Thread(
			() -> {
				try {
					createSocket(PORT);
				}
				catch (final IOException e) {
					LOGGER.log(Level.SEVERE, "Error creando el servicio servidor en el puerto " + PORT, e); //$NON-NLS-1$
				}

			}
		).start();
		Thread.sleep(3000);
		final SocketFactory sf = SocketFactory.getDefault();
		try ( final Socket s = sf.createSocket(); ) {
			try {
				s.connect(
					new InetSocketAddress("127.0.0.1", PORT), //$NON-NLS-1$
					6000
				);
			}
			catch(final SocketTimeoutException | java.net.ConnectException ste) {
				LOGGER.log(Level.SEVERE, "No se ha podido conectar", ste); //$NON-NLS-1$
				System.exit(-2);
			}
			try ( final InputStream is = s.getInputStream(); ) {
				final byte[] d = getDataFromInputStream(is);
				LOGGER.log(Level.INFO, new String(d));
			}
		}

	}

	static void createSocket(final int port) throws IOException {
		final ServerSocketFactory ssf = ServerSocketFactory.getDefault();
		try (
			final ServerSocket ss = ssf.createServerSocket(port);
			final Socket s = ss.accept();
			final OutputStream os = s.getOutputStream();
		) {
			os.write("Conexion establecida".getBytes()); //$NON-NLS-1$
			os.flush();
		}
	}

    /** Lee un flujo de datos de entrada y los recupera en forma de array de
     * bytes. Este m&eacute;todo consume pero no cierra el flujo de datos de
     * entrada.
     * @param input Flujo de donde se toman los datos.
     * @return Los datos obtenidos del flujo.
     * @throws IOException Cuando ocurre un problema durante la lectura. */
    public static byte[] getDataFromInputStream(final InputStream input) throws IOException {
        if (input == null) {
            return new byte[0];
        }
        int nBytes = 0;
        final byte[] buffer = new byte[32];
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        while ((nBytes = input.read(buffer)) != -1) {
            baos.write(buffer, 0, nBytes);
        }
        return baos.toByteArray();
    }

}
