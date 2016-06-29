package es.gob.afirma.android;


import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import android.app.ListActivity;
import android.app.ProgressDialog;
import android.media.MediaScannerConnection;
import android.os.AsyncTask.Status;
import android.os.Bundle;
import android.os.Environment;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;
import es.gob.afirma.R;
import es.gob.afirma.android.crypto.CipherDataManager;
import es.gob.afirma.android.gui.DownloadFileTask;
import es.gob.afirma.android.gui.DownloadFileTask.DownloadDataListener;
import es.gob.afirma.android.gui.FileArrayAdapter;
import es.gob.afirma.android.gui.FileOption;
import es.gob.afirma.android.network.AndroidUrlHttpManager;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.protocol.ParameterException;
import es.gob.afirma.core.misc.protocol.ProtocolInvocationUriParser;
import es.gob.afirma.core.misc.protocol.UrlParametersToSave;

/** Actividad Android para la elecci&oacute;n de un fichero en el almacenamiento del dispositivo. */
public final class SaveDataActivity extends ListActivity implements DownloadDataListener {

	private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

	private static final String SAVE_INSTANCE_KEY_CURRENT_DIR = "currentDir"; //$NON-NLS-1$
	private static final String SAVE_INSTANCE_KEY_INITIAL_DIR = "initialDir"; //$NON-NLS-1$
	private static final String SAVE_INSTANCE_KEY_SELECTED_DIR = "selectedDir"; //$NON-NLS-1$

	private static final String DEFAULT_FILENAME = "firma"; //$NON-NLS-1$

	private FileArrayAdapter adapter;
	private File currentDir;
	File getCurrentDir() {
		return this.currentDir;
	}
	private File selectedDir;
	void setSelectedDir(final File sd) {
		this.selectedDir = sd;
	}
	File getSelectedDir() {
		return this.selectedDir;
	}

	private String initialDirectoryName = null;

	private UrlParametersToSave parameters = null;
	UrlParametersToSave getParameters() {
		return this.parameters;
	}

	private DownloadFileTask downloadFileTask = null;
	DownloadFileTask getDownloadFileTask() {
		return this.downloadFileTask;
	}

	/** Di&aacute;logo de espera durante la carga de los datos. */
	ProgressDialog progressDialog = null;

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		if (getIntent() == null || getIntent().getData() == null) {
			Log.w(ES_GOB_AFIRMA, "No se han indicado parametros de entrada para la actividad"); //$NON-NLS-1$
			closeActivity();
			return;
		}

		// Si ya estaban configurado el estadp de la activity, lo recargamos
		if (savedInstanceState != null && savedInstanceState.containsKey(SAVE_INSTANCE_KEY_INITIAL_DIR)) {
			this.initialDirectoryName = savedInstanceState.getString(SAVE_INSTANCE_KEY_INITIAL_DIR);
			if (savedInstanceState.containsKey(SAVE_INSTANCE_KEY_CURRENT_DIR)) {
				this.currentDir = new File(savedInstanceState.getString(SAVE_INSTANCE_KEY_CURRENT_DIR));
			}
			this.selectedDir = new File(savedInstanceState.getString(SAVE_INSTANCE_KEY_SELECTED_DIR));
		}

		if (this.currentDir == null) {
			if (Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState())) {
				this.currentDir = Environment.getExternalStorageDirectory();
			}
			else {
				this.currentDir = Environment.getDownloadCacheDirectory();
			}
			this.initialDirectoryName = this.currentDir.getName();
			this.selectedDir = null;
		}

		Log.d(ES_GOB_AFIRMA, "Se abre el directorio: " + this.currentDir.getAbsolutePath());  //$NON-NLS-1$

		// Establecemos el layout con la interfaz
		setContentView(R.layout.activity_save_data);

		fill(this.currentDir);

		// Recogemos los datos de la URI
		if (this.parameters == null) {
			try {
				this.parameters = ProtocolInvocationUriParser.getParametersToSave(getIntent().getDataString());
			}
			catch (final ParameterException e) {
				showMessage(getString(R.string.error_bad_params));
				Log.e(ES_GOB_AFIRMA, "Error en los parametros de entrada: " + e.toString()); //$NON-NLS-1$
				closeActivity();
				return;
			}
		}

		UrlHttpManagerFactory.install(new AndroidUrlHttpManager());
	}

	private void fill(final File f) {

		((TextView) findViewById(R.id.current_directory)).setText(getString(R.string.file_chooser_directorio_actual) + " " + f.getName());  //$NON-NLS-1$

		final List<FileOption> dir = new ArrayList<FileOption>();
		for (final File ff : f.listFiles()) {
			// No mostramos ficheros ni directorios ocultos
			if (ff.getName().startsWith(".")) { //$NON-NLS-1$
				continue;
			}
			if (ff.isDirectory()) {
				dir.add(new FileOption(ff));
			}
		}

		Collections.sort(dir);
		if (!f.getName().equalsIgnoreCase(this.initialDirectoryName)) {
			dir.add(0, new FileOption(f, true));
		}

		this.adapter = new FileArrayAdapter(SaveDataActivity.this, R.layout.array_adapter_file_chooser, dir);
		setListAdapter(this.adapter);
	}

	/**
	 * Muestra un mensaje al usuario.
	 * @param message Mensaje que se desea mostrar.
	 */
	private void showMessage(final String message) {

		//TODO: Comprobar que se muestran los mensajes de error antes de cerrar la aplicacion

		runOnUiThread(
			new Runnable() {
				@Override
				public void run() {
					Toast.makeText(SaveDataActivity.this, message, Toast.LENGTH_LONG).show();
				}
			}
		);
	}

	@Override
	protected void onListItemClick(final ListView l, final View v, final int position, final long id) {
		super.onListItemClick(l, v, position, id);

		final FileOption item = (FileOption) l.getItemAtPosition(position);
		if (item.isDirectory()) {
			this.currentDir = new File(item.getPath());
			fill(this.currentDir);
		}
	}

	@Override
	protected void onStart() {
		super.onStart();

		//EasyTracker.getInstance().activityStart(this);

		((Button) findViewById(R.id.saveButton)).setOnClickListener(new SaveDataAction(this));

		((Button) findViewById(R.id.cancelButton)).setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(final View v) {
				Log.i(ES_GOB_AFIRMA, "Se cancela el guardado de los datos");  //$NON-NLS-1$
				closeActivity();
			}
		});

		if (this.parameters != null) {
			// Si no tenemos datos, los podamos descargar y no hemos empezado todavia la descarga, empezamos a descargarlos
			if (this.parameters.getData() == null && this.parameters.getFileId() != null && this.downloadFileTask == null) {
				this.downloadFileTask = new DownloadFileTask(
						this.parameters.getFileId(),
						this.parameters.getRetrieveServletUrl(),
						this);
				this.downloadFileTask.execute();
			}

			// Establecemos el titulo de la ventana si se definio
			if (this.parameters.getTitle() != null) {
				setTitle(this.parameters.getTitle());
			}
		}
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();

		if (this.downloadFileTask != null) {
			this.downloadFileTask.cancel(true);
		}
	}

	/**
	 * Construye un nombre apropiado para un fichero de firma en base a un nombre base
	 * y un &iacute;ndice.
	 * @param originalName Nombre base del fichero.
	 * @param index &Iacute;ndice.
	 * @return Nombre apropiado para el fichero de firma.
	 */
	private static String buildName(final String originalName, final int index) {

		String indexSuffix = ""; //$NON-NLS-1$
		if (index > 0) {
			indexSuffix = "(" + index + ")"; //$NON-NLS-1$ //$NON-NLS-2$
		}

		final int dotPos = originalName.lastIndexOf('.');
		if (dotPos == -1) {
			return originalName + indexSuffix;
		}
		return originalName.substring(0, dotPos) + indexSuffix + originalName.substring(dotPos);
	}

	/**
	 * Recupera la extensi&oacute;n preferente para un fichero con los datos indicados.
	 * @param data Datos para los que se busca una extensi&oacute;n de fichero.
	 * @return Extensi&oacute;n (por ejemplo, "jpg") o {@code null} si no se identific&oacute; una.
	 */
	private static String getExtension(final byte[] data) {
		return new MimeHelper(data).getExtension();
	}

	void saveData(final byte[] data, final File dir, final String configuredFilename) {
		String filename = configuredFilename;
		if (filename == null) {
			final String ext = getExtension(data);
			if (ext != null) {
				filename = DEFAULT_FILENAME + "." + ext; //$NON-NLS-1$
			}
			else {
				filename = DEFAULT_FILENAME;
			}
		}
		int i = 0;
		File outFile = new File(dir, filename);
		while (outFile.exists()) {
			outFile = new File(dir, buildName(filename, ++i));
		}

		try {
			final FileOutputStream fos = new FileOutputStream(outFile);
			fos.write(data);
			fos.close();
		}
		catch (final IOException e) {
			Log.e(ES_GOB_AFIRMA, "No se han podido guardar los datos: " + e); //$NON-NLS-1$
			showMessage(getString(R.string.error_saving_data));
			return;
		}
		// Mostramos el mensaje de confirmacion del guardado
		showMessage(getString(R.string.data_saved, outFile.getName()));

		Log.d(ES_GOB_AFIRMA, "Los datos se han guardado correctamente"); //$NON-NLS-1$

		// Refrescamos el directorio para permitir acceder al fichero
		try {
			MediaScannerConnection.scanFile(
				this,
				new String[] { outFile.toString(),
					outFile.getParentFile().toString()
				},
				null,
				null
			);
		}
		catch(final Exception e) {
			Log.w(ES_GOB_AFIRMA, "Error refrescando el MediaScanner: " + e); //$NON-NLS-1$
		}

		closeActivity();
	}

	@Override
	public void processData(final byte[] data) {

		Log.i(ES_GOB_AFIRMA, "Datos descargados correctamente"); //$NON-NLS-1$

		// Si hemos tenido que descargar los datos desde el servidor, los desciframos y llamamos
		// al dialogo de seleccion de certificados para la firma
		byte[] decipheredData;
		try {
			decipheredData = CipherDataManager.decipherData(data, this.parameters.getDesKey());
		}
		catch (final IOException e) {
			Log.e(ES_GOB_AFIRMA, "Los datos proporcionados no estan correctamente codificados en Base64: " + e); //$NON-NLS-1$
			showMessage(getString(R.string.error_bad_params));
			closeActivity();
			return;
		}
		catch (final GeneralSecurityException e) {
			Log.e(ES_GOB_AFIRMA, "Error al descifrar los datos recuperados del servidor para la firma: " + e); //$NON-NLS-1$
			showMessage(getString(R.string.error_bad_params));
			closeActivity();
			return;
		}
		catch (final IllegalArgumentException e) {
			Log.e(ES_GOB_AFIRMA, "Los datos recuperados no son un base64 valido: " + e.toString()); //$NON-NLS-1$
			showMessage(getString(R.string.error_bad_params));
			closeActivity();
			return;
		}

		try {
			this.parameters = ProtocolInvocationUriParser.getParametersToSave(decipheredData);
		}
		catch (final ParameterException e) {
			showMessage(getString(R.string.error_bad_params));
			Log.e(ES_GOB_AFIRMA, "Error en los parametros XML de configuracion de guardado: " + e); //$NON-NLS-1$
			closeActivity();
			return;
		}

		// Establecemos el titulo de la ventana si se definio
		if (this.parameters.getTitle() != null) {
			setTitle(this.parameters.getTitle());
		}

		// Si ya se selecciono el directorio en el que guardar los datos, lo hacemos directamente
		if (this.selectedDir != null) {
			saveData(this.parameters.getData(), this.selectedDir, this.parameters.getFileName());
		}
	}

	@Override
	public void onErrorDownloadingData(final String msg, final Throwable t) {
		Log.e(ES_GOB_AFIRMA, "Ocurrio un error descargando los datos del servidor intermedio: " + msg); //$NON-NLS-1$
		if (t != null) {
			t.printStackTrace();
		}
		showMessage(getString(R.string.error_saving_data));
		closeActivity();
	}

	@Override
	protected void onSaveInstanceState(final Bundle outState) {
		super.onSaveInstanceState(outState);

		outState.putString(SAVE_INSTANCE_KEY_CURRENT_DIR, this.currentDir.getAbsolutePath());
		outState.putString(SAVE_INSTANCE_KEY_INITIAL_DIR, this.initialDirectoryName);
		if (this.selectedDir != null) {
			outState.putString(SAVE_INSTANCE_KEY_SELECTED_DIR, this.selectedDir.getAbsolutePath());
		}
	}

	private class SaveDataAction implements OnClickListener {

		final SaveDataActivity saveDataActivity;

		SaveDataAction(final SaveDataActivity saveDataActivity) {
			this.saveDataActivity = saveDataActivity;
		}

		@Override
		public void onClick(final View v) {

			this.saveDataActivity.setSelectedDir(this.saveDataActivity.getCurrentDir());

			// Si todavia estamos descargando los datos, mostramos un dialogo de espera.
			// Sera el propio proceso de descarga el que guarde los datos cuando detecte
			// un directorio seleccionado.
			if (SaveDataActivity.this.getDownloadFileTask() != null && SaveDataActivity.this.getDownloadFileTask().getStatus() == Status.RUNNING) {
				SaveDataActivity.this.progressDialog = ProgressDialog.show(SaveDataActivity.this, "", //$NON-NLS-1$
						getString(R.string.dialog_msg_loading_data), true);
				return;
			}


			final UrlParametersToSave params = this.saveDataActivity.getParameters();
			saveData(params.getData(), this.saveDataActivity.getSelectedDir(), params.getFileName());
		}

	}

	void closeActivity() {

		if (this.progressDialog != null && this.progressDialog.isShowing()) {
			runOnUiThread(new Runnable() {
				@Override
				public void run() {
					SaveDataActivity.this.progressDialog.dismiss();
				}
			});
		}

		finish();
	}

//	@Override
//	protected void onStop() {
//		super.onStop();
//		EasyTracker.getInstance().activityStop(this);
//	}
}