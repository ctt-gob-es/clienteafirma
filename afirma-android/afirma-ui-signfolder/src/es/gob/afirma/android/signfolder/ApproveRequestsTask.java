package es.gob.afirma.android.signfolder;

import android.os.AsyncTask;
import android.util.Log;
import es.gob.afirma.android.signfolder.proxy.CommManager;
import es.gob.afirma.android.signfolder.proxy.RequestResult;
import es.gob.afirma.android.signfolder.proxy.SignRequest;

/**
 * Tarea as&iacute;ncrona para la aprobaci&oacute;n (visto bueno) de peticiones de firma.
 * Despu&eacute;s de la aprobaci&oacute;n se actualiza la lista con las peticiones pendientes.
 * @author Carlos Gamuci
 */
public class ApproveRequestsTask extends AsyncTask<Void, Void, RequestResult[]> {

	private final String[] requestIds;
	private final String certB64;
	private final CommManager commManager;
	private final OperationRequestListener listener;
	private Throwable t;

	/**
	 * Crea una tarea as&iacute;ncrona para la aprobaci&oacute;n de peticiones.
	 * @param requests Listado de peticiones que se desean aprobar.
	 * @param certB64 Certificado codificado en base64 para autenticar la operaci&oacute;n.
	 * @param commManager Manejador de las comunicaciones.
	 * @param listView Lista que debe actualizar tras la operaci&oacute;n.
	 */
	public ApproveRequestsTask(final SignRequest[] requests, final String certB64, final CommManager commManager, final OperationRequestListener listener) {
		this.requestIds = new String[requests.length];
		this.certB64 = certB64;
		this.commManager = commManager;
		this.listener = listener;
		this.t = null;
		
		for (int i = 0; i < requests.length; i++) {
			this.requestIds[i] = requests[i].getId();
		}
	}
	
	/**
	 * Crea una tarea as&iacute;ncrona para la aprobaci&oacute;n de peticiones.
	 * @param requestId Identificador de la petici&oacute;n a aprobar.
	 * @param certB64 Certificado codificado en base64 para autenticar la operaci&oacute;n.
	 * @param commManager Manejador de las comunicaciones.
	 * @param listView Lista que debe actualizar tras la operaci&oacute;n.
	 * @param activity Actividad sobre la que se ejecuta la operaci&oacute;n.
	 */
	public ApproveRequestsTask(final String requestId, final String certB64, final CommManager commManager, final OperationRequestListener listener) {
		this.requestIds = new String[] { requestId };
		this.certB64 = certB64;
		this.commManager = commManager;
		this.listener = listener;
		this.t = null;
	}
	
    @Override
	protected RequestResult[] doInBackground(final Void... arg) {

        // Enviamos la peticion de rechazo
    	RequestResult[] results;
        try {
			results = this.commManager.approveRequests(this.requestIds, this.certB64);
		} catch (final Exception e) {
			Log.w(SFConstants.LOG_TAG, "Ocurrio un error en la aprobacion de las solicitudes de firma: " + e); //$NON-NLS-1$
			results = new RequestResult[this.requestIds.length];
			for (int i = 0; i < results.length; i++) {
				results[i] = new RequestResult(this.requestIds[i], false);
			}
			this.t = e;
		}

        return results;
    }

    @Override
	protected void onPostExecute(final RequestResult[] approvedRequests) {
    	
    	for (final RequestResult rResult : approvedRequests) {
    		if (rResult.isStatusOk()) {
    			this.listener.requestOperationFinished(OperationRequestListener.APPROVE_OPERATION, rResult);
    		}
    		else {
    			this.listener.requestOperationFailed(OperationRequestListener.APPROVE_OPERATION, rResult, this.t);
    		}
    	}
    }
}
