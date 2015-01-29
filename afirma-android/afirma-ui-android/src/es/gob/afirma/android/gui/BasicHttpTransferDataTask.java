package es.gob.afirma.android.gui;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.net.Proxy;
import java.net.URL;
import java.net.URLConnection;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import android.os.AsyncTask;
import es.gob.afirma.android.network.AndroidUrlHttpManager;
import es.gob.afirma.core.AOCancelledOperationException;

/** Tarea de transferencia en segundo plano de datos por HTTP. */
public abstract class BasicHttpTransferDataTask extends AsyncTask<Void, Void, byte[]> {

	/** Lee una URL HTTP o HTTPS por POST si se indican par&aacute;metros en la URL y por GET en caso contrario.
	 * En HTTPS no se hacen comprobaciones del certificado servidor.
	 * @param url URL a leer
	 * @return Contenido de la URL
	 * @throws IOException Si no se puede leer la URL */
	protected byte[] readUrlByPost(final String url) throws IOException {
		return readUrlByPost(url, AndroidUrlHttpManager.DEFAULT_TIMEOUT);
	}

	/** Lee una URL HTTP o HTTPS por POST si se indican par&aacute;metros en la URL y por GET en caso contrario.
	 * En HTTPS no se hacen comprobaciones del certificado servidor.
	 * @param url URL a leer
	 * @param timeout Tiempo m&aacute;ximo en milisegundos que se debe esperar por la respuesta. Un timeout de 0
	 * se interpreta como un timeout infinito. Si se indica -1, se usar&aacute; el por defecto de Java.
	 * @return Contenido de la URL
	 * @throws IOException Si no se puede leer la URL */
	protected byte[] readUrlByPost(final String url, final int timeout) throws IOException {

		if (url == null) {
			throw new IllegalArgumentException("La URL a leer no puede ser nula"); //$NON-NLS-1$
		}

		// Si la URL no tiene parametros la leemos por GET
		if (!url.contains("?")) { //$NON-NLS-1$
			return readUrlByGet(url);
		}

		final StringTokenizer st = new StringTokenizer(url, "?"); //$NON-NLS-1$
		final String request = st.nextToken();
		final String urlParameters = st.nextToken();

		final URL uri = new URL(request);

		if (uri.getProtocol().equals(AndroidUrlHttpManager.HTTPS)) {
			try {
				AndroidUrlHttpManager.disableSslChecks();
			}
			catch(final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"No se ha podido ajustar la confianza SSL, es posible que no se pueda completar la conexion: " + e //$NON-NLS-1$
				);
			}
		}

		final URLConnection conn = uri.openConnection(Proxy.NO_PROXY);

		//conn.setRequestMethod("POST"); //$NON-NLS-1$
		conn.setDoOutput(true);
		if (timeout != AndroidUrlHttpManager.DEFAULT_TIMEOUT) {
			conn.setConnectTimeout(timeout);
			conn.setReadTimeout(timeout);
		}

		final OutputStreamWriter writer = new OutputStreamWriter(conn.getOutputStream());

		writer.write(urlParameters);
		writer.flush();
		writer.close();

		final InputStream is = conn.getInputStream();

		final byte[] data = readDataFromInputStream(is);

		is.close();

		if (uri.getProtocol().equals(AndroidUrlHttpManager.HTTPS)) {
			AndroidUrlHttpManager.enableSslChecks();
		}

		return data;
	}

	/** Lee una URL HTTP o HTTPS por GET. En HTTPS no se hacen comprobaciones del certificado servidor.
	 * @param url URL a leer
	 * @return Contenido de la URL
	 * @throws IOException Si no se puede leer la URL */
	protected byte[] readUrlByGet(final String url) throws IOException {
		final URL uri = new URL(url);
		if (uri.getProtocol().equals(AndroidUrlHttpManager.HTTPS)) {
			try {
				AndroidUrlHttpManager.disableSslChecks();
			}
			catch(final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"No se ha podido ajustar la confianza SSL, es posible que no se pueda completar la conexion: " + e //$NON-NLS-1$
				);
			}
		}
		final InputStream is = uri.openStream();
		final byte[] data = readDataFromInputStream(is);
		is.close();
		if (uri.getProtocol().equals("https")) { //$NON-NLS-1$
			AndroidUrlHttpManager.enableSslChecks();
		}
		return data;
	}

	/** Lee el contenido de un InputStream, abortando la operaci&oacute;n si en alg&uacute;n momento se
	 * detiene la tarea.
	 * @param is Flujo de datos de entrada.
	 * @return Datos le&iacute;dos del flujo.
	 * @throws IOException Cuando ocurre un error durante la lectura.
	 * @throws AOCancelledOperationException Cuando se cancela la tarea. */
	public byte[] readDataFromInputStream(final InputStream is) throws IOException {

		int n = 0;
		final byte[] buffer = new byte[1024];
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		while ((n = is.read(buffer)) > 0) {
			baos.write(buffer, 0, n);
			if (isCancelled()) {
				throw new AOCancelledOperationException("Se ha cancelado el acceso a informacion remota"); //$NON-NLS-1$
			}
		}

		return baos.toByteArray();
	}

	@Override
	protected void onCancelled(final byte[] result) {
		super.onCancelled(result);
	}
}
