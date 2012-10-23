package es.gob.afirma.signers.padestri.client;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import es.gob.afirma.signers.padestri.client.UrlHttpManager;

/**
 * Implementacion de ua clase para la lectura del contenido de una URL.
 * @author Carlos Gamuci
 */
public class UrlHttpManagerImpl implements UrlHttpManager {

	private static final int BUFFER_SIZE = 1024;

	@Override
	public byte[] readUrl(final String url) throws IOException {
		final URL uri = new URL(url);
		final InputStream is = uri.openStream();
		final byte[] data = getDataFromInputStream(is);
		try { is.close(); } catch (final Exception e) { /* Ignoramos el error */ }
		return data;
	}

    /** Lee un flujo de datos de entrada y los recupera en forma de array de
     * bytes. Este m&eacute;todo consume pero no cierra el flujo de datos de
     * entrada.
     * @param input
     *        Flujo de donde se toman los datos.
     * @return Los datos obtenidos del flujo.
     * @throws IOException
     *         Cuando ocurre un problema durante la lectura */
    private static byte[] getDataFromInputStream(final InputStream input) throws IOException {
        if (input == null) {
            return new byte[0];
        }
        int nBytes = 0;
        final byte[] buffer = new byte[BUFFER_SIZE];
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        while ((nBytes = input.read(buffer)) != -1) {
            baos.write(buffer, 0, nBytes);
        }
        return baos.toByteArray();
    }
}
