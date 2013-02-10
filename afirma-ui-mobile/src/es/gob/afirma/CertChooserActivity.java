package es.gob.afirma;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import android.app.ListActivity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Environment;
import android.view.View;
import android.widget.ListView;
import android.widget.Toast;
import es.gob.afirma.android.crypto.Android4KeyStoreManager;
import es.gob.afirma.android.gui.FileArrayAdapter;
import es.gob.afirma.android.gui.Option;

/** Actividad Android para la elecci&oacue;n de un fichero PFX/P12 en el almacenamiento del dispositivo.
 * @author Alberto Mart&iacute;nez */
public final class CertChooserActivity extends ListActivity {

    private static final String P12 = ".p12"; //$NON-NLS-1$
    private static final String PFX = ".pfx"; //$NON-NLS-1$

    private FileArrayAdapter adapter;
    private File currentDir;

    @Override
    public void onBackPressed() {
        final Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setClass(this, MainActivity.class);
        startActivity(intent);
        finish();
    }

    @Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        this.currentDir = Environment.getExternalStorageDirectory();
        fill(this.currentDir);
    }

    private void fill(final File f) {
        this.setTitle(getString(R.string.cert_chooser_directorio_actual) + " " + f.getName()); //$NON-NLS-1$
        final List<Option> dir = new ArrayList<Option>();
        final List<Option> fls = new ArrayList<Option>();

        for (final File ff : f.listFiles()) {
        	// No mostramos ficheros ni directorios ocultos
        	if (ff.getName().startsWith(".")) { //$NON-NLS-1$
        		continue;
        	}
            if (ff.isDirectory()) {
                dir.add(new Option(ff.getName(), getString(R.string.cert_chooser_directorio), ff.getAbsolutePath()));
            }
            else {
            	// Solo mostramos P12 o PFX
            	if (ff.getName().toLowerCase().endsWith(PFX) || ff.getName().toLowerCase().endsWith(P12)) {
            		fls.add(new Option(ff.getName(), getString(R.string.cert_chooser_tamano_del_fichero) + " " + ff.length(), ff.getAbsolutePath())); //$NON-NLS-1$
            	}
            }
        }

        Collections.sort(dir);
        Collections.sort(fls);
        dir.addAll(fls);
        if (!f.getName().equalsIgnoreCase("sdcard")) { //$NON-NLS-1$
            dir.add(0, new Option("..", getString(R.string.cert_chooser_directorio_padre), f.getParent())); //$NON-NLS-1$
        }

        this.adapter = new FileArrayAdapter(CertChooserActivity.this, R.layout.activity_cert_chooser, dir);
        this.setListAdapter(this.adapter);
    }

    @Override
    protected void onListItemClick(final ListView l, final View v, final int position, final long id) {
        super.onListItemClick(l, v, position, id);
        final Option o = this.adapter.getItem(position);
        if (o.getData().equalsIgnoreCase(getString(R.string.cert_chooser_directorio_padre)) || o.getData().equalsIgnoreCase(getString(R.string.cert_chooser_directorio))) {
            this.currentDir = new File(o.getPath());
            fill(this.currentDir);
        }
        else {
            onFileClick(o);
        }
    }

    private void onFileClick(final Option o) {
    	final byte[] data;
    	try {
            final FileInputStream fis = new FileInputStream(o.getPath());
            data = new byte[(int) fis.getChannel().size()];
            fis.read(data);
            fis.close();
    	}
    	catch(final IOException e) {
    		Toast.makeText(this, getString(R.string.cert_chooser_read_error), Toast.LENGTH_LONG).show();
    		return;
    	}
        new Android4KeyStoreManager(this).importCertificateFromPkcs12(data, null);
        finish();
    }
}