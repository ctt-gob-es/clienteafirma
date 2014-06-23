package es.gob.afirma.android.gui;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import android.content.ContextWrapper;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.os.AsyncTask;
import android.util.Log;
import es.gob.afirma.R;

/** Tarea para la gesti&oacute;n de aplicaciones de solicitud de certificados. */
public final class VerifyCaAppsTask extends AsyncTask<Void, Void, List<AppProperties>> {

	private static final String CAS_DATA_LIST = "cas.properties";  //$NON-NLS-1$

	final ContextWrapper context;
	final CaAppsVerifiedListener listener;

	/** Construye una tarea para la gesti&oacute;n de aplicaciones de solicitud de certificados.
	 * @param context Contexto Android
	 * @param verifiedListener Clase a la que se notifica la lista de aplicaciones de solicitud
	 *                         de certificados que hay en el sistema */
	public VerifyCaAppsTask(final ContextWrapper context, final CaAppsVerifiedListener verifiedListener) {
		this.context = context;
		this.listener = verifiedListener;
	}

	@Override
	protected List<AppProperties> doInBackground(final Void... arg0) {

		// Cargamos de un properties el listado de aplicaciones soportadas
		final Properties caProperties = new Properties();
		try {
			
			final InputStream is = this.context.getAssets().open(CAS_DATA_LIST);
			caProperties.load(is);
			is.close();
		}
		catch (final IOException e) {
			Log.w("es.gob.afirma", "No se han podido comprobar las aplicaciones de CA instaladas: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
		catch (final NullPointerException e) {
			Log.w("es.gob.afirma", "No se ha encontrado el fichero de apps de CA compatibles: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			Log.w("es.gob.afirma", "No se han podido comprobar las aplicaciones de CA instaladas: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}

		final PackageManager pm = this.context.getPackageManager();
		final List<ApplicationInfo> packages = pm.getInstalledApplications(PackageManager.GET_META_DATA);

		final List<AppProperties> apps = new ArrayList<AppProperties>();

		for (final String prefix : caProperties.getProperty("apps.list").split(",")) { //$NON-NLS-1$ //$NON-NLS-2$
			final String caPackage = caProperties.getProperty(prefix + ".package"); //$NON-NLS-1$
			if (caPackage != null) {
				int iconId;
				try {
					final Field iconIdField = R.drawable.class.getField(caProperties.getProperty(prefix + ".logo")); //$NON-NLS-1$
					iconId = iconIdField.getInt(null);
				} catch (final Exception e) {
					Log.w("es.gob.afirma", "No se ha podido cargar el icono de la autoridad " + prefix);  //$NON-NLS-1$//$NON-NLS-2$
					iconId = -1;
				}
				boolean installed = false;
				for (final ApplicationInfo installedApp : packages) {
					if (caPackage.equalsIgnoreCase(installedApp.packageName)) {
						apps.add(
								new AppProperties(installedApp.loadLabel(pm),
										installedApp.packageName,
										caProperties.getProperty(prefix + ".mainActivity"), //$NON-NLS-1$
										caProperties.getProperty(prefix + ".description"), //$NON-NLS-1$
										iconId != -1 ? this.context.getResources().getDrawable(iconId) : null,
												caProperties.getProperty(prefix + ".marketUrl"), //$NON-NLS-1$
												true));
						installed = true;
						break;
					}
				}
				if (!installed) {
					apps.add(
							new AppProperties(caProperties.getProperty(prefix + ".nameApp"), //$NON-NLS-1$
									caPackage,
									caProperties.getProperty(prefix + ".mainActivity"), //$NON-NLS-1$
									caProperties.getProperty(prefix + ".description"), //$NON-NLS-1$
									iconId != -1 ? this.context.getResources().getDrawable(iconId) : null,
											caProperties.getProperty(prefix + ".marketUrl"), //$NON-NLS-1$
											false));
				}
			}
		}

		return apps;
	}

	@Override
	protected void onPostExecute(final List<AppProperties> apps) {
		this.listener.caAppsVerified(apps);
	}


	/** Clase a la que se notifica la lista de aplicaciones de solicitud de certificados que hay en el sistema. */
	public interface CaAppsVerifiedListener {

		/** Se ejecuta al finalizar la comprobaci&oacute;n de cuales de las aplicaciones de CAs
		 * soportadas est&aacute;n instaladas en el dispositivos.
		 * @param apps Listado de aplicaciones instaladas. */
		public void caAppsVerified(List<AppProperties> apps);
	}
}
