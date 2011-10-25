package es.gob.afirma.test.pades;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Logger;

/** Servicio HTTP simple para simplificar las pruebas desde dispositivos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SimpleDebugService {
    
    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final static int port = 80;

    /** Punto de entrada.
     * @param args No se aceptan argumentos
     * @throws Exception */
    public static void main(String args[]) throws Exception {
        final ServerSocket serversocket = new ServerSocket(port);
        while (true) {
            final Socket connectionsocket = serversocket.accept();
            LOGGER.info(connectionsocket.getInetAddress().getHostName() + " contectado"); //$NON-NLS-1$
            final BufferedReader input = new BufferedReader(new InputStreamReader(connectionsocket.getInputStream()));
            final DataOutputStream output = new DataOutputStream(connectionsocket.getOutputStream());
            new Thread(new Runnable() {
                public void run() {
                    try {
                        final String request = new String(input.readLine());
                        LOGGER.info(request);
                        output.close();
                    }
                    catch(final Exception e) {
                        LOGGER.severe("Error en la obtencion de datos: " + e); //$NON-NLS-1$
                    }
                }
            }).start();
        }
    }

}
