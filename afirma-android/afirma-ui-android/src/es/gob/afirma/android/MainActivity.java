package es.gob.afirma.android;


import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import android.app.AlertDialog;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.security.KeyChain;
import android.support.v4.app.FragmentActivity;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;

import com.google.analytics.tracking.android.EasyTracker;

import es.gob.afirma.R;
import es.gob.afirma.android.gui.AppProperties;
import es.gob.afirma.android.gui.VerifyCaAppsTask;
import es.gob.afirma.android.gui.VerifyCaAppsTask.CaAppsVerifiedListener;

/** Actividad que se muestra cuando se arranca la aplicaci&oaute;n pulsando su icono.
 * @author Alberto Mart&iacute;nez */
public final class MainActivity extends FragmentActivity implements CaAppsVerifiedListener {

	@Override
	  public void onStart() {
	    super.onStart();
	    EasyTracker.getInstance().activityStart(this); // Add this method.
	}

	@Override
	  public void onStop() {
	    super.onStop();
	    EasyTracker.getInstance().activityStop(this); // Add this method.
	}

	private final static String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

	private final static String EXTRA_RESOURCE_TITLE = "es.gob.afirma.android.title"; //$NON-NLS-1$
	private final static String EXTRA_RESOURCE_EXT = "es.gob.afirma.android.exts"; //$NON-NLS-1$

	private final static String CERTIFICATE_EXTS = ".p12,.pfx"; //$NON-NLS-1$

	private final static int SELECT_CERT_REQUEST_CODE = 1;

	static final String[] COMMON_EXCLUDED_DIRS = new String[] {
		"cache", //$NON-NLS-1$
		"config", //$NON-NLS-1$
		"d", //$NON-NLS-1$
		"dev", //$NON-NLS-1$
		"etc", //$NON-NLS-1$
		"lib", //$NON-NLS-1$
		"proc", //$NON-NLS-1$
		"sbin", //$NON-NLS-1$
		"sys", //$NON-NLS-1$
		"system", //$NON-NLS-1$
		"LOST.DIR", //$NON-NLS-1$
		"log", //$NON-NLS-1$
		"Android" //$NON-NLS-1$
	};

	private List<AppProperties> apps;

	@Override
	public void onCreate(final Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);

		// Por defecto no se muestra el boton de solicitud de certificados
		this.apps = null;

		// Comprobamos si esta instalada la aplicacion de algun proveedor de servicios de certificacion
		// para mostrar el boton de peticion de certificados en caso afirmativo
		verifyCaApps();
	}

	/** @param v Vista sobre la que se hace clic. */
	public void onClick(final View v) {

		// Dialogo de solicitud de certificados
		if (v.getId() == R.id.requestCertButton) {

			final AlertDialog ad = new AlertDialog.Builder(MainActivity.this).create();
			ad.setTitle(getString(R.string.appsDialogTitle));
			ad.setMessage(getString(R.string.appsDialogMessage));

			final LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			final View view = inflater.inflate(R.layout.dialog_list, null);
			final ListView listView = (ListView) view.findViewById(R.id.listViewListadoApp);

			final AppAdapter listaAppAdapter = new AppAdapter(MainActivity.this, ad, R.layout.array_adapter_apps, this.apps);
			listView.setAdapter(listaAppAdapter);
			ad.setView(view);
			ad.show();
		}
		//Boton firmar fichero local
		else if(v.getId() == R.id.buttonSign){
			final Intent intent = new Intent(getApplicationContext(), LocalSignResultActivity.class);
			startActivity(intent);
		}
		// Instalacion de certificados
		else {
			final Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
			intent.setClass(this, FileChooserActivity.class);
			intent.putExtra(EXTRA_RESOURCE_TITLE, getString(R.string.title_activity_cert_chooser));
			intent.putExtra(EXTRA_RESOURCE_EXT, CERTIFICATE_EXTS);
			startActivityForResult(intent, SELECT_CERT_REQUEST_CODE);
		}
	}

	@Override
	protected void onActivityResult(final int requestCode, final int resultCode, final Intent data) {
		super.onActivityResult(requestCode, resultCode, data);

		if (requestCode == SELECT_CERT_REQUEST_CODE && resultCode == RESULT_OK) {

			final String filename = data.getStringExtra(FileChooserActivity.RESULT_DATA_STRING_FILENAME);

			int n = 0;
			final byte[] buffer = new byte[1024];
			final ByteArrayOutputStream baos;
			try {
				baos = new ByteArrayOutputStream();
				final InputStream is = new FileInputStream(filename);
				while ((n = is.read(buffer)) > 0) {
					baos.write(buffer, 0, n);
				}
				is.close();
			} catch (final IOException e) {
				showErrorMessage(getString(R.string.error_loading_selected_file, filename));
				Log.e(ES_GOB_AFIRMA, "Error al cargar el fichero: " + e.toString()); //$NON-NLS-1$
				e.printStackTrace();
				return;
			}
/* COMENTADO PARA EVITAR UTILIZAR LA CLASE PFX DE BOUNCyCASTLE
			// Comprobamos que es un PKCS#12 valido porque de no serlo la importacion en Android
			// bloquea la aplicacion
			try {
				org.bouncycastle.asn1.pkcs.Pfx.getInstance(baos.toByteArray());
			} catch (final Exception e) {
				Log.e(ES_GOB_AFIRMA, "El fichero seleccionado no era un PKCS#12 valido: " + e.toString()); //$NON-NLS-1$
				showErrorMessage(getString(R.string.error_no_valid_p12));
				e.printStackTrace();
				return;
			}
*/
			final Intent intent = KeyChain.createInstallIntent();
			intent.putExtra(KeyChain.EXTRA_PKCS12, baos.toByteArray());
			startActivity(intent);
		}
	}

	/**
	 * Muestra un mensaje de advertencia al usuario.
	 * @param message Mensaje que se desea mostrar.
	 */
	private void showErrorMessage(final String message) {
		new MessageDialog(message, null, this).show(getSupportFragmentManager(), "ErrorDialog"); //$NON-NLS-1$;
	}

	private void verifyCaApps() {
		new VerifyCaAppsTask(this, this).execute();
	}

	@Override
	public void caAppsVerified(final List<AppProperties> ap) {
		this.apps = ap;
	}

	/**Clase para crear cada fila de la tabla.
	 * Cada fila consta de: logo, nombre de la aplicaci&oacute;n, nombre del paquete
	 * @author Astrid Idoate Gil*/
	final class AppAdapter extends ArrayAdapter<AppProperties> {

		private final List<AppProperties> items;
		private final AlertDialog ad;
		private TextView mTextViewAppName;
		private TextView mTextViewAppDescription;
		private ImageView mImageViewIconApp;

		AppAdapter(final Context context, final AlertDialog ad, final int textViewResourceId,  final List<AppProperties> items) {
			super(context, textViewResourceId, items != null ? items : new ArrayList<AppProperties>());
			this.ad = ad;
			this.items = items == null ? new ArrayList<AppProperties>(0) : items;
		}

		@Override
		public View getView(final int position, final View convertView, final ViewGroup parent) {
			View v;
			final LayoutInflater vi = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			v = vi.inflate(R.layout.array_adapter_apps, null);

			final AppProperties appProperties = this.items.get(position);

			this.mTextViewAppName = (TextView) v.findViewById(R.id.tvName);
			this.mTextViewAppDescription = (TextView) v.findViewById(R.id.tvDescription);
			this.mImageViewIconApp = (ImageView) v.findViewById(R.id.icon);

			this.mTextViewAppName.setText(appProperties.getNameApp());
			this.mTextViewAppDescription.setText(appProperties.getDescription());
			this.mImageViewIconApp.setImageDrawable(appProperties.getIcon());

			final LinearLayout linearLayoutList  = (LinearLayout) v.findViewById(R.id.listaItem);
			linearLayoutList.setOnClickListener(new DialogItemClientListener(this.ad, appProperties));
			return v;
		}

		private class DialogItemClientListener implements OnClickListener {

			private final AlertDialog alertDialog;
			private final AppProperties appProperties;

			DialogItemClientListener(final AlertDialog ad, final AppProperties appProperties) {
				this.alertDialog = ad;
				this.appProperties = appProperties;
			}

			@Override
			public void onClick(final View view) {
				if (this.appProperties.isInstalled()) {
					final ComponentName cn = new ComponentName(this.appProperties.getPackageName(), this.appProperties.getMainActivity());
					final Intent intent = new Intent();
					intent.setComponent(cn);
					startActivity(intent);
				}
				else {
					final Intent intent = new Intent();
					intent.setData(Uri.parse(this.appProperties.getMarketUrl()));
					startActivity(intent);
				}
				this.alertDialog.dismiss();
			}
		}
	}
}