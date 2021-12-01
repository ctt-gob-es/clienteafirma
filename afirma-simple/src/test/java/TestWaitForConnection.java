import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.net.ServerSocketFactory;


/** Prueba simple de conexi&oacute;n a <i>socket</i> local.
 * Para comprobar si el cortafiegos bloquea o no la conexi&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s.*/
public final class TestWaitForConnection {

	private static final int PORT = 6629;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Main para pruebas.
	 * @param args No se usa. */
	public static void main(final String[] args) {
		new Thread(
			() -> {
				try {
					createSocket(PORT);
				}
				catch (final IOException e) {
					LOGGER.log(Level.SEVERE,
							"Error creando el servicio servidor en el puerto " + PORT, //$NON-NLS-1$
							e);
				}

			}
		).start();
//		Thread.sleep(3000);
//		final SocketFactory sf = SocketFactory.getDefault();
//		try ( final Socket s = sf.createSocket(); ) {
//			try {
//				s.connect(
//					new InetSocketAddress("127.0.0.1", PORT), //$NON-NLS-1$
//					6000
//				);
//			}
//			catch(final SocketTimeoutException | java.net.ConnectException ste) {
//				System.out.println("No se ha podido conectar"); //$NON-NLS-1$
//				System.exit(-2);
//			}
//			try ( final InputStream is = s.getInputStream(); ) {
//				final byte[] d = getDataFromInputStream(is);
//				System.out.println(new String(d));
//			}
//		}

	}

	static void createSocket(final int port) throws IOException {
		final ServerSocketFactory ssf = ServerSocketFactory.getDefault();
		System.out.println("Antes del accept"); //$NON-NLS-1$
		try (
			final ServerSocket ss = ssf.createServerSocket(port);
			final Socket s = ss.accept();
			final OutputStream os = s.getOutputStream();
		) {
			System.out.println("Despues del accept"); //$NON-NLS-1$
			os.write("Conexion establecida".getBytes()); //$NON-NLS-1$
			os.flush();
		}
	}

    /** Lee un flujo de datos de entrada y los recupera en forma de array de
     * bytes. Este m&eacute;todo consume pero no cierra el flujo de datos de
     * entrada.
     * @param input
     *        Flujo de donde se toman los datos.
     * @return Los datos obtenidos del flujo.
     * @throws IOException
     *         Cuando ocurre un problema durante la lectura */
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
