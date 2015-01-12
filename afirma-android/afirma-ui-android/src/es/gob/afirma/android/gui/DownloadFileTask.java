package es.gob.afirma.android.gui;

import java.io.IOException;
import java.net.URL;

import android.util.Log;
import es.gob.afirma.core.AOCancelledOperationException;

/** Tarea para la descarga de un fichero del servidor intermedio. */
public final class DownloadFileTask extends BasicHttpTransferDataTask {

	private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

	/** Juego de carateres UTF-8. */
	private static final String DEFAULT_URL_ENCODING = "UTF-8"; //$NON-NLS-1$

	private static final String METHOD_OP_GET = "get"; //$NON-NLS-1$

	private static final String SYNTAX_VERSION = "1_0"; //$NON-NLS-1$

	private static final String ERROR_PREFIX = "ERR-"; //$NON-NLS-1$

	private final String fileId;
	private final URL retrieveServletUrl;
	private final DownloadDataListener ddListener;

	private String errorMessage = null;
	private Throwable errorThowable = null;

	/** Crea una tarea para la descarga de un fichero del servidor intermedio.
	 * @param fileId Identificadod del fichero en el servidor intermedio
	 * @param retrieveServletUrl URL del servidor intermedio
	 * @param ddListener Clase a la que hay que notificar el resultado de la descraga */
	public DownloadFileTask(final String fileId, final URL retrieveServletUrl, final DownloadDataListener ddListener) {
		this.fileId = fileId;
		this.retrieveServletUrl = retrieveServletUrl;
		this.ddListener = ddListener;
	}

	@Override
	protected byte[] doInBackground(final Void... arg0) {

		Log.i(ES_GOB_AFIRMA, "Invocando retrieveData"); //$NON-NLS-1$

		byte[] data;
		try {
			final StringBuffer url = new StringBuffer(this.retrieveServletUrl.toExternalForm());
			url.append("?op=").append(METHOD_OP_GET); //$NON-NLS-1$
			url.append("&v=").append(SYNTAX_VERSION); //$NON-NLS-1$
			url.append("&id=").append(this.fileId); //$NON-NLS-1$

			Log.i(ES_GOB_AFIRMA, "URL: " + url); //$NON-NLS-1$

			// Llamamos al servicio para guardar los datos
			data = this.readUrlByPost(url.toString());

			Log.i(ES_GOB_AFIRMA, "Descarga de datos finalizada"); //$NON-NLS-1$

			if (ERROR_PREFIX.equalsIgnoreCase(new String(data, 0, 4, DEFAULT_URL_ENCODING))) {
				this.errorMessage = "El servidor devolvio el siguiente error al descargar los datos: " + new String(data, DEFAULT_URL_ENCODING); //$NON-NLS-1$
				this.errorThowable = new IOException(this.errorMessage);
				return null;
			}
		}
		catch (final IOException e) {
			this.errorMessage = "No se pudo conectar con el servidor intermedio"; //$NON-NLS-1$
			this.errorThowable = e;
			return null;
		}
		catch (final AOCancelledOperationException e) {
			this.errorMessage = "Se cancelo la descarga de los datos"; //$NON-NLS-1$
			this.errorThowable = e;
			return null;
		}
		catch (final Throwable e) {
			this.errorMessage = "Error desconocido durante la descarga de datos: " + e; //$NON-NLS-1$
			this.errorThowable = e;
			return null;
		}

		// Comprobamos que la tarea no se haya cancelado
		if (isCancelled()) {
			this.errorMessage = "Se ha cancelado la tarea de descarga"; //$NON-NLS-1$
			this.errorThowable = new AOCancelledOperationException(this.errorMessage);
			return null;
		}

		return data;
	}

	@Override
	protected void onPostExecute(final byte[] result) {

		if (result != null) {
			this.ddListener.processData(result);
		}
		else if (this.errorMessage != null) {
			this.ddListener.onErrorDownloadingData(this.errorMessage, this.errorThowable);
		}
		else {
			Log.e(ES_GOB_AFIRMA, "La actividad de descarga ha finalizado sin obtener resultados"); //$NON-NLS-1$
		}
	}

	/** Interfaz para la notificaci&oacute;n de finalizaci&oacute;n de la
	 * tarea de descarga del fichero. */
	public interface DownloadDataListener {

		/** Procesa los datos obtenidos de la descarga del fichero.
		 * @param data Datos obtenidos de la descarga del fichero */
		public void processData(byte[] data);

		/** Se ejecuta al producirse un error durante la descarga de datos.
		 * @param msg Mensaje del error
		 * @param t Error producido */
		public void onErrorDownloadingData(String msg, Throwable t);
	}
}
