package es.gob.afirma;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import android.app.ListActivity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Environment;
import android.util.Log;
import android.view.View;
import android.widget.ListView;
import android.widget.Toast;
import es.gob.afirma.android.crypto.Android4KeyStoreManager;
import es.gob.afirma.android.gui.FileArrayAdapter;
import es.gob.afirma.android.gui.Option;

/** @author Alberto Mart&iacute;nez */
public class CertChooserActivity extends ListActivity {

    private static final String P12 = ".p12"; //$NON-NLS-1$
    private static final String PFX = ".pfx"; //$NON-NLS-1$
    private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

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
        final File[] dirs = f.listFiles();
        this.setTitle(getString(R.string.cert_chooser_directorio_actual) + " " + f.getName()); //$NON-NLS-1$
        final List<Option> dir = new ArrayList<Option>();
        final List<Option> fls = new ArrayList<Option>();

        try {
            for (final File ff : dirs) {
            	// No mostramos ficheros ni directorios ocultos
            	if (ff.getName().startsWith(".")) { //$NON-NLS-1$
            		continue;
            	}
                if (ff.isDirectory()) {
                    dir.add(new Option(ff.getName(), getString(R.string.cert_chooser_directorio), ff.getAbsolutePath()));
                }
                else {
                	if (ff.getName().toLowerCase().endsWith(PFX) || ff.getName().toLowerCase().endsWith(P12)) {
                		fls.add(new Option(ff.getName(), getString(R.string.cert_chooser_tamano_del_fichero) + " " + ff.length(), ff.getAbsolutePath())); //$NON-NLS-1$
                	}
                }
            }
        }
        catch (final Exception e) {
            Log.e(ES_GOB_AFIRMA, e.getMessage() != null ? e.getMessage() : e.getClass().toString());
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
        try {
            if (o.getPath().endsWith(CertChooserActivity.PFX) || o.getPath().endsWith(CertChooserActivity.P12)) {
                final FileInputStream fis = new FileInputStream(o.getPath());
                final byte[] data = new byte[(int) fis.getChannel().size()];
                fis.read(data);
                fis.close();
                new Android4KeyStoreManager(this).importCertificateFromPkcs12(data, null);
                finish();
            }
            else {
                Toast.makeText(this, getString(R.string.cert_chooser_seleccionar_p12_pxf), Toast.LENGTH_LONG).show();
            }
        }
        catch (final Exception e) {
            Log.e(CertChooserActivity.ES_GOB_AFIRMA, e.getMessage());
        }
    }
}