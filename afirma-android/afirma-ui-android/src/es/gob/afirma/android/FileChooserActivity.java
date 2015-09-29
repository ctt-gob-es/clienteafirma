package es.gob.afirma.android;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import android.app.Activity;
import android.app.ListActivity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Environment;
import android.util.Log;
import android.view.View;
import android.widget.ListView;
import android.widget.TextView;
import es.gob.afirma.R;
import es.gob.afirma.android.gui.FileArrayAdapter;
import es.gob.afirma.android.gui.FileOption;

/** Actividad Android para la elecci&oacute;n de un fichero en el almacenamiento del dispositivo. */
public final class FileChooserActivity extends ListActivity {

//	@Override
//	  public void onStart() {
//	    super.onStart();
//	    EasyTracker.getInstance().activityStart(this); // Add this method.
//	}
//
//	@Override
//	  public void onStop() {
//	    super.onStop();
//	    EasyTracker.getInstance().activityStop(this); // Add this method.
//	}

	private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

	private static final String SAVE_INSTANCE_KEY_CURRENT_DIR = "currentDir"; //$NON-NLS-1$
	private static final String SAVE_INSTANCE_KEY_INITIAL_DIR = "initialDir"; //$NON-NLS-1$

	static final String RESULT_DATA_STRING_FILENAME = "filename"; //$NON-NLS-1$

	private String[] excludedDirs = new String[0];

	private FileArrayAdapter adapter;
	private File currentDir;

	private String initialDirectoryName = null;

	private String extFilters = null;

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setContentView(R.layout.activity_file_chooser);

		// Establecemos los filtros por extension de fichero
		this.extFilters = getIntent().getExtras().getString("es.gob.afirma.android.exts"); //$NON-NLS-1$
		this.excludedDirs = getIntent().getExtras().getStringArray("es.gob.afirma.android.excludedDirs"); //$NON-NLS-1$

		// Establecemos el titulo de la ventana
		final String title = getIntent().getExtras().getString("es.gob.afirma.android.title"); //$NON-NLS-1$
		if (title != null) {
			setTitle(title);
		}

		if (savedInstanceState != null && savedInstanceState.containsKey(SAVE_INSTANCE_KEY_INITIAL_DIR)) {
			this.initialDirectoryName = savedInstanceState.getString(SAVE_INSTANCE_KEY_INITIAL_DIR);
			if (savedInstanceState.containsKey(SAVE_INSTANCE_KEY_CURRENT_DIR)) {
				this.currentDir = new File(savedInstanceState.getString(SAVE_INSTANCE_KEY_CURRENT_DIR));
			}
		}

		if (this.currentDir == null) {
			if (Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState())) {
				this.currentDir = Environment.getExternalStorageDirectory();
			}
			else {
				this.currentDir = Environment.getDownloadCacheDirectory();
			}
			this.initialDirectoryName = this.currentDir.getName();
		}

		Log.d(ES_GOB_AFIRMA, "Se abre el directorio: " + this.currentDir.getAbsolutePath());  //$NON-NLS-1$

		fill(this.currentDir);
	}

	private void fill(final File f) {

		//Descomentar
		((TextView) findViewById(R.id.current_directory)).setText(getString(R.string.file_chooser_directorio_actual) + " " + f.getName());  //$NON-NLS-1$

		final List<FileOption> dir = new ArrayList<FileOption>();
		final List<FileOption> fls = new ArrayList<FileOption>();

		for (final File ff : f.listFiles()) {
			// No mostramos ficheros ni directorios ocultos
			if (ff.getName().startsWith(".")) { //$NON-NLS-1$
				continue;
			}
			// Si es un directorio y no esta en el listado de excluidos...
			if (ff.isDirectory() && !arrayContains(ff.getName(), this.excludedDirs)) {
				dir.add(new FileOption(ff));
			}
			else {
				if (this.extFilters == null) {
					fls.add(new FileOption(ff));
				} else {
					for (final String extFilter : this.extFilters.split(",")) { //$NON-NLS-1$
						if (ff.getName().toLowerCase(Locale.ENGLISH).endsWith(extFilter)) {
							fls.add(new FileOption(ff));
							break;
						}
					}
				}
			}
		}

		Collections.sort(dir);
		Collections.sort(fls);
		dir.addAll(fls);
		if (!f.getName().equalsIgnoreCase(this.initialDirectoryName)) {
			dir.add(0, new FileOption(f, true));
		}

		this.adapter = new FileArrayAdapter(FileChooserActivity.this, R.layout.array_adapter_file_chooser, dir);
		setListAdapter(this.adapter);
	}

	private static boolean arrayContains(final String key, final String[] array) {
		if (array == null) {
			return false;
		}
		for (final String s : array) {
			if (s.equals(key)) {
				return true;
			}
		}
		return false;
	}

	@Override
	protected void onListItemClick(final ListView l, final View v, final int position, final long id) {
		super.onListItemClick(l, v, position, id);

		final FileOption item = (FileOption) l.getItemAtPosition(position);
		if (item.isDirectory()) {
			this.currentDir = new File(item.getPath());
			fill(this.currentDir);
		}
		else {
			onFileClick(item);
		}
	}

	private void onFileClick(final FileOption o) {
		final Intent dataIntent = new Intent();
		dataIntent.putExtra(RESULT_DATA_STRING_FILENAME, o.getPath());
		setResult(Activity.RESULT_OK, dataIntent);
		finish();
	}

	@Override
	protected void onSaveInstanceState(final Bundle outState) {
		super.onSaveInstanceState(outState);

		outState.putString(SAVE_INSTANCE_KEY_CURRENT_DIR, this.currentDir.getAbsolutePath());
		outState.putString(SAVE_INSTANCE_KEY_INITIAL_DIR, this.initialDirectoryName);
	}

	@Override
	public void onBackPressed() {
		if (this.initialDirectoryName.equals(this.currentDir.getName())) {
			this.setResult(RESULT_CANCELED);
			finish();
		}
		else {
			this.currentDir = this.currentDir.getParentFile();
			fill(this.currentDir);
		}
	}
}