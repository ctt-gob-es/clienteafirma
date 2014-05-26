package es.gob.afirma.android.signfolder;

import android.os.AsyncTask;
import android.util.Log;
import es.gob.afirma.android.signfolder.proxy.CommManager;
import es.gob.afirma.android.signfolder.proxy.RequestDetail;

/**
 * Tarea para la carga de los detalles de una petici&oacute;n en una pantalla para la
 * visualizaci&oacute;n de la descripci&oacute;n de peticiones.
 */
public class LoadPetitionDetailsTask extends AsyncTask<Void, Void, RequestDetail> {

	private final String petitionId;
	private final String certB64;
	private final CommManager commManager;
	private final LoadSignRequestDetailsListener listener;

	/**
	 * Crea la tarea para la carga de los detalles de una petici&oacute;n en una pantalla para la
	 * visualizaci&oacute;n de la descripci&oacute;n de peticiones.
	 * @param petitionId Identificados de la petici&oacute;n de la que se quiere el detalle.
	 * @param cert Certificado para la autenticaci&oacute;n de la petici&oacute;n.
	 * @param commManager Manejador de los servicios de comunicaci&oacute;n con el portafirmas.
	 * @param petitionDetailsActivity Actividad en la que es posible mostrar los datos.
	 */
	public LoadPetitionDetailsTask(final String petitionId, final String certB64,
			final CommManager commManager, final LoadSignRequestDetailsListener listener) {
		this.petitionId = petitionId;
		this.certB64 = certB64;
		this.commManager = commManager;
		this.listener = listener;
	}

	@Override
	protected RequestDetail doInBackground(final Void... args) {

    	RequestDetail requestDetail;
    	try {
    		requestDetail = this.commManager.getRequestDetail(this.certB64, this.petitionId);
    	} catch (final Throwable e) {
    		Log.w(SFConstants.LOG_TAG, "No se pudo obtener el detalle de la solicitud: " + e);
    		requestDetail = null;
    	}

		return requestDetail;
	}

	@Override
	protected void onPostExecute(final RequestDetail result) {
		if (result != null) {
			this.listener.loadedSignRequestDetails(result);
		}
		else {
			this.listener.errorLoadingSignRequestDetails();
		}
	}
	
	/**
	 * Interfaz con los metodos para gestionar los resultados de la peticion del detalle
	 * de una solicitud de firma.
	 */
	interface LoadSignRequestDetailsListener {
		
		public void loadedSignRequestDetails(RequestDetail details);
		
		public void errorLoadingSignRequestDetails();
	}
}
