package es.gob.afirma.android.signfolder;

import android.content.Context;
import android.os.AsyncTask;
import android.util.Log;
import es.gob.afirma.android.signfolder.proxy.CommManager;
import es.gob.afirma.android.signfolder.proxy.RequestAppConfiguration;

/**
 * Carga los datos remotos necesarios para la configuraci&oacute;n de la aplicaci&oacute;n
 */
public class LoadConfigurationDataTask extends AsyncTask<Void, Void, RequestAppConfiguration> {

	private final String certB64;
	private final String certAlias;
	private final CommManager commManager;
	private final Context context;
	private final LoadConfigurationListener listener;
	private Throwable t = null;

	/**
	 * Crea la tarea para la carga de la configuraci&oacute;n de la aplicaci&oacute;n
	 * necesaria para su correcto funcionamiento.
	 * @param cert Certificado para la autenticaci&oacute;n de la petici&oacute;n.
	 * @param commManager Manejador de los servicios de comunicaci&oacute;n con el portafirmas.
	 * @param context Contexto de la aplicaci&oacute;n.
	 * @param listener Manejador del resultado de la operaci&oacute;n.
	 */
	public LoadConfigurationDataTask(final String certB64, final String certAlias,
			final CommManager commManager, final Context context, final LoadConfigurationListener listener) {
		this.certB64 = certB64;
		this.certAlias = certAlias;
		this.commManager = commManager;
		this.context = context;
		this.listener = listener;
	}

	@Override
	protected RequestAppConfiguration doInBackground(final Void... args) {

		RequestAppConfiguration config;
    	try {
    		config = this.commManager.getApplicationList(this.certB64);
    	} catch (final Exception e) {
    		Log.w(SFConstants.LOG_TAG, "No se pudo obtener la lista de aplicaciones: " + e); //$NON-NLS-1$
    		config = null;
    		this.t = e;
    	}

    	// Agregamos la configuracion necesaria
		// Como primer elemento aparecera el elemento que desactiva el filtro 
    	if (config != null) {
    		config.getAppIdsList().add(0, ""); //$NON-NLS-1$
    		config.getAppNamesList().add(0, this.context.getString(R.string.filter_app_all_request));
    	}

		return config;
	}

	@Override
	protected void onPostExecute(final RequestAppConfiguration appConfig) {
		if (appConfig != null) {
			this.listener.configurationLoadSuccess(appConfig, this.certB64, this.certAlias);
		}
		else {
			this.listener.configurationLoadError(this.t);
		}
	}
	
	/**
	 * Interfaz con los metodos para gestionar los resultados de la carga de la
	 * configuraci&oacute;n de la aplicaci&oacute;n.
	 */
	interface LoadConfigurationListener {
		
		public void configurationLoadSuccess(RequestAppConfiguration appConfig, String certB64, String certAlias);
		
		public void configurationLoadError(Throwable t);
	}
}
