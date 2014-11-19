package es.gob.afirma.android;

import java.io.FileInputStream;

import android.os.AsyncTask;
import android.util.Log;
import es.gob.afirma.core.misc.AOUtil;

final class ReadLocalFileTask extends AsyncTask<String, Void, Object> {

	private final ReadLocalFileListener listener;

	interface ReadLocalFileListener {
		void setData(final Object data);
	}

	ReadLocalFileTask(final ReadLocalFileListener list) {
		if (list == null) {
			throw new IllegalArgumentException("Es obligatorio indicar a quien trasladar los datos leidos"); //$NON-NLS-1$
		}
		this.listener = list;
	}

	@Override
	protected Object doInBackground(final String... files) {
		final byte[] dataToSign;
		try {
			final FileInputStream fis = new FileInputStream(files[0]);
			dataToSign = AOUtil.getDataFromInputStream(fis);
			fis.close();
		}
		catch(final OutOfMemoryError e) {
			Log.e("es.gob.afirma", "El fichero a firmar es demasiado grande: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return e;
		}
		catch (final Exception e) {
			Log.e("es.gob.afirma", "Error leyendo el fichero a firmar: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return e;
		}
		return dataToSign;
	}

	@Override
	protected void onPostExecute(final Object data) {
		super.onPostExecute(data);
		this.listener.setData(data);
	}

}
