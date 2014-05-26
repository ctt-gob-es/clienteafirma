package es.gob.afirma.android.signfolder;

import android.os.AsyncTask;
import android.util.Log;
import es.gob.afirma.android.signfolder.proxy.CommManager;
import es.gob.afirma.android.signfolder.proxy.RequestDetail;
import es.gob.afirma.android.signfolder.proxy.RequestResult;
import es.gob.afirma.android.signfolder.proxy.SignRequest;

/**
 * Tarea as&iacute;ncrona para el rechazo de peticiones de firma. Despu&eacute;s
 * del rechazo actualiza la lista con las peticiones pendientes.
 * @author Carlos Gamuci
 */
public class RejectRequestsTask extends AsyncTask<Void, Void, RequestResult[]> {

	private final String[] requestIds;
	private final String certB64;
	private final CommManager commManager;
	private final OperationRequestListener listener;
	private Throwable t;

	/**
	 * Crea una tarea as&iacute;ncrona para el rechazo de peticiones.
	 * @param requests Listado de peticiones que se desean rechazar.
	 * @param certB64 Certificado codificado en base64 para autenticar la operaci&oacute;n.
	 * @param commManager Manejador de las comunicaciones para el rechazo de las peticiones.
	 * @param listView Lista que debe actualizar tras el rechazo.
	 */
	public RejectRequestsTask(final SignRequest[] requests, final String certB64, final CommManager commManager, final OperationRequestListener listener) {
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
	 * Crea una tarea as&iacute;ncrona para el rechazo de peticiones.
	 * @param requests Listado de peticiones que se desean rechazar.
	 * @param certB64 Certificado codificado en base64 para autenticar la operaci&oacute;n.
	 * @param commManager Manejador de las comunicaciones para el rechazo de las peticiones.
	 * @param listView Lista que debe actualizar tras el rechazo.
	 * @param activity Actividad sobre la que se ejecuta la operaci&oacute;n.
	 */
	public RejectRequestsTask(final RequestDetail[] requests, final String certB64, final CommManager commManager, final OperationRequestListener listener) {
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
	 * Crea una tarea as&iacute;ncrona para el rechazo de una petici&oacute;n.
	 * @param requestId Identificador de la petici&oacute;n a rechazar.
	 * @param certB64 Certificado codificado en base64 para autenticar la operaci&oacute;n.
	 * @param commManager Manejador de las comunicaciones para el rechazo de las peticiones.
	 * @param listView Lista que debe actualizar tras el rechazo.
	 * @param activity Actividad sobre la que se ejecuta la operaci&oacute;n.
	 */
	public RejectRequestsTask(final String requestId, final String certB64, final CommManager commManager, final OperationRequestListener listener) {
		this.requestIds = new String[] { requestId };
		this.certB64 = certB64;
		this.commManager = commManager;
		this.listener = listener;
		this.t = null;
	}
	
    @Override
	protected RequestResult[] doInBackground(final Void... arg) {

        	// Enviamos la peticion de rechazo
    	RequestResult[] result = null;
        try {
			result = this.commManager.rejectRequests(this.requestIds, this.certB64);
		} catch (final Exception e) {
			Log.w(SFConstants.LOG_TAG, "Ocurrio un error en el rechazo de las solicitudes de firma: " + e); //$NON-NLS-1$
			this.t = e;
		}

        return result;
    }

    @Override
	protected void onPostExecute(final RequestResult[] rejectedRequests) {

    	if (rejectedRequests != null) {
    		for (final RequestResult rResult : rejectedRequests) {
    			if (rResult.isStatusOk()) {
    				this.listener.requestOperationFinished(OperationRequestListener.REJECT_OPERATION, rResult);
    			}
    			else {
    				this.listener.requestOperationFailed(OperationRequestListener.REJECT_OPERATION, rResult, this.t);
    			}
    		}
    	}
    	else {
    		this.listener.requestOperationFailed(OperationRequestListener.REJECT_OPERATION, null, this.t);
    	}
    }
}
