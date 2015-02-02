package es.gob.afirma.android.signfolder;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import android.app.Activity;
import android.content.Context;
import android.os.AsyncTask;
import android.os.Environment;
import android.util.Log;

/** Tarea para descarga de fichero en segundo plano. */
public class SaveFileTask extends AsyncTask<Void, Void, File> {

	private final InputStream dataIs;
	private final String filename;
	private final boolean extDir;
	private final SaveFileListener listener;
	private final Activity activity;

	/** Crea una tarea para descarga de fichero en segundo plano.
	 * @param dataIs Flujo de lectura de los datos del fichero.
	 * @param filename Nombre del fichero a guardar.
	 * @param extDir Si se establece a <code>true</code> se guarda en un directorio externo,
	 *               si se establece a <code>false</code> de guardar&aacute; en un directorio interno.
	 * @param listener Clase a la que notificar el sesultado de la tarea.
	 * @param activity Actividad que invoca a la tarea.
	 */
	public SaveFileTask(final InputStream dataIs, final String filename,
			final boolean extDir, final SaveFileListener listener, final Activity activity) {
		this.dataIs = dataIs;
		this.filename = filename;
		this.extDir = extDir;
		this.listener = listener;
		this.activity = activity;
	}

	@Override
	protected File doInBackground(final Void... arg0) {

		File outFile;
		if (this.extDir) {
			int i = 0;
			do {
				outFile = new File(
					Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS),
					generateFileName(this.filename, i++)
				);
			} while (outFile.exists());

			Log.i(SFConstants.LOG_TAG, "Se intenta guardar en el directorio externo el fichero: " + outFile.getAbsolutePath()); //$NON-NLS-1$
			try {
				final FileOutputStream fos = new FileOutputStream(outFile);
				writeData(this.dataIs, fos);
				fos.close();
				this.dataIs.close();
			}
			catch (final Exception e) {
				Log.e(SFConstants.LOG_TAG, "Error al guardar el fichero en un directorio externo: " + e); //$NON-NLS-1$
				return null;
			}
		}
		else {
			try {

				Log.i(SFConstants.LOG_TAG, "Se intenta guardar en el directorio interno el fichero: " + new File(this.activity.getFilesDir(), this.filename).getAbsolutePath()); //$NON-NLS-1$
				final FileOutputStream fos = this.activity.openFileOutput(this.filename, Context.MODE_WORLD_READABLE);
				writeData(this.dataIs, fos);
				fos.close();
				this.dataIs.close();
				outFile = new File(this.activity.getFilesDir(), this.filename);
			}
			catch (final Exception e) {
				Log.e(SFConstants.LOG_TAG, "Error al guardar el fichero en un directorio interno: " + e); //$NON-NLS-1$
				return null;
			}
		}

		return outFile;
	}

	/**
	 * Escribe los datos de un flujo de entrada en uno de salida.
	 * @param is Flujo de entrada.
	 * @param os Flujo de salida.
	 * @throws IOException Cuando ocurre un error.
	 */
	private static void writeData(final InputStream is, final OutputStream os) throws IOException {
		int n = -1;
		final byte[] buffer = new byte[1024];
		while ((n = is.read(buffer)) > 0) {
			os.write(buffer, 0, n);
		}
	}

	@Override
	protected void onPostExecute(final File result) {

		if (result == null) {
			this.listener.saveFileError(this.filename);
		}
		else {
			this.listener.saveFileSuccess(result);
		}
	}

	/**
	 * Genera un nombre de fichero agregando un indice al final del nombre propuesto. Si el
	 * &iacute;ndice es menor o igual a 0, se devuelve el nombre indicado.
	 * @param docName Nombre inicial del fichero.
	 * @param index &Iacute;ndice que agregar.
	 * @return Nombre generado.
	 */
	private static String generateFileName(final String docName, final int index) {
		if (index <= 0) {
			return docName;
		}

		final int lastDocPos = docName.lastIndexOf('.');
		if (lastDocPos == -1) {
			return docName + '(' + index + ')';
		}

		return docName.substring(0, lastDocPos) + '(' + index + ')' + docName.substring(lastDocPos);
	}

	interface SaveFileListener {

		void saveFileSuccess(File outputFile);

		void saveFileError(String filename);
	}
}