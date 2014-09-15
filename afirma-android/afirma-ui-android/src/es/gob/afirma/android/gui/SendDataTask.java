package es.gob.afirma.android.gui;

import java.io.IOException;

import android.util.Log;
import es.gob.afirma.core.AOCancelledOperationException;

/** Tarea para el env&iacute;o de datos al servidor de intercambio. Si la entrega de estos datos es
 * cr&iacute;tica para la correcta ejecuc&iacute;n del procedimiento, la tarea tratar&aacute; de
 * finalizar la actividad.
 * @author Carlos Gamuci */
public final class SendDataTask extends BasicHttpTransferDataTask {

	private static final String METHOD_OP_PUT = "put"; //$NON-NLS-1$

	private static final String SYNTAX_VERSION = "1_0"; //$NON-NLS-1$

	private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

	final String id;
	final String servletUrl;
	final String dataB64;
	final SendDataListener listener;
	final boolean critical;
	final boolean needCloseApp;
	Throwable error = null;

	/** Crea la tarea con los datos necesarios para el intercambio.
	 * @param id Identificador del intercambio.
	 * @param servletUrl URL del servlet para la subida de datos.
	 * @param dataB64 Datos en base 64 que se desean enviar.
	 * @param listener Clase a la que se notifica el resultado del env&iacute;o de datos */
	public SendDataTask(final String id, final String servletUrl, final String dataB64, final SendDataListener listener) {
		this.id = id;
		this.servletUrl = servletUrl;
		this.dataB64 = dataB64;
		this.listener = listener;
		this.critical = false;
		this.needCloseApp = true;
	}

	/** Crea la tarea con los datos necesarios para el intercambio, permitiendo que se indique si la
	 * entrega de estos datos es un proceso cr&iacute;tico para la ejecuci&oacute;n del procedimiento.
	 * @param id Identificador del intercambio.
	 * @param servletUrl URL del servlet para la subida de datos.
	 * @param dataB64 Datos en base 64 que se desean enviar.
	 * @param listener Clase a la que se notifica el resultado del env&iacute;o de datos
	 * @param critical {@code true} si el procedimiento es cr&iacute;tico, {@code false} en caso contrario.
	 * @param needCloseApp {@code true} si debe cerrarse la aplicaci&oacute;n tras el env&iacute;o. */
	public SendDataTask(final String id, final String servletUrl, final String dataB64, final SendDataListener listener, final boolean critical, final boolean needCloseApp) {
		this.id = id;
		this.servletUrl = servletUrl;
		this.dataB64 = dataB64;
		this.listener = listener;
		this.critical = critical;
		this.needCloseApp = needCloseApp;
	}

	@Override
	protected byte[] doInBackground(final Void... arg0) {

		final byte[] result;
		try {
			final StringBuffer url = new StringBuffer(this.servletUrl);
			url.append("?op=").append(METHOD_OP_PUT); //$NON-NLS-1$
			url.append("&v=").append(SYNTAX_VERSION); //$NON-NLS-1$
			url.append("&id=").append(this.id); //$NON-NLS-1$
			url.append("&dat=").append(this.dataB64); //$NON-NLS-1$

			// Llamamos al servicio para guardar los datos
			result = readUrlByPost(url.toString());
		}
		catch (final IOException e) {
			Log.e(ES_GOB_AFIRMA, "No se pudo conectar con el servidor intermedio para el envio de datos: " + e); //$NON-NLS-1$
			this.error = e;
			return null;
		}
		catch (final AOCancelledOperationException e) {
			Log.e(ES_GOB_AFIRMA, "Se cancelo el envio de datos: " + e); //$NON-NLS-1$
			this.error = e;
			return null;
		}

		return result;
	}

	@Override
	protected void onPostExecute(final byte[] result) {
		super.onPostExecute(result);

		if (result != null) {
			this.listener.onSuccessSendingData(result, this.critical, this.needCloseApp);
		} else {
			this.listener.onErrorSendingData(this.error, this.critical, this.needCloseApp);
		}

	}

	/** Listener para el manejo del resultado devuelto por la tarea de envio de datos al servidor. */
	public interface SendDataListener {

		/** Llamada cuando el env&iacute;o termino satisfactoriamente.
		 * @param result Resultado del servidor
		 * @param critical <code>true</code> si es un resultado cr&iacute;tico, <code>false</code>
		 *                 en caso contrario
		 * @param needCloseApp Si es necesario cerrar la aplicaci&oacute;n tras esta llamada */
		void onSuccessSendingData(byte[] result, boolean critical, boolean needCloseApp);

		/** Llamada cuando el env&iacute;o termin&oacute; en error
		 * @param error Error que se ha producido
		 * @param critical <code>true</code> si es un error cr&iacute;tico, <code>false</code>
		 *                 en caso contrario
		 * @param needCloseApp Si es necesario cerrar la aplicaci&oacute;n tras esta llamada */
		void onErrorSendingData(Throwable error, boolean critical, boolean needCloseApp);

	}

}
