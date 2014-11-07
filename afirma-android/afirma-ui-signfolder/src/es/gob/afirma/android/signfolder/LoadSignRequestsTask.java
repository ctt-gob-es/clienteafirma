package es.gob.afirma.android.signfolder;

import java.util.List;

import android.os.AsyncTask;
import android.util.Log;
import es.gob.afirma.android.signfolder.proxy.CommManager;
import es.gob.afirma.android.signfolder.proxy.PartialSignRequestsList;
import es.gob.afirma.android.signfolder.proxy.SignRequest;

/** Tarea as&iacute;ncrona para la carga de peticiones de firma en una lista de peticiones.
 * @author Carlos Gamuci. */
final class LoadSignRequestsTask extends AsyncTask<Void, Void, PartialSignRequestsList> {

	private final String certEncodedB64;
	private final String state;
	private final String[] filters;
	private final CommManager commManager;
	private final LoadSignRequestListener listener;
	private final int numPage;
	private final int pageSize;

	/**
	 * Crea la tarea asincrona para la carga de peticiones de firma.
	 * @param commManager Manejador de las comunicaciones para el rechazo de las peticiones.
	 * @param state Estado de las peticiones que se solicitan (pendiente, rechazadas o firmadas).
	 * @param filters Filtros que han de cumplir las peticiones.
	 * @param listener Manejador para el postproceso de las peticiones de firma cargadas..
	 * @param activity Actividad sobre la que se ejecuta la operaci&oacute;n.
	 */
	LoadSignRequestsTask(final String certEncodedB64, final String state, final int numPage, final int pageSize, final List<String> filters, final CommManager commManager, final LoadSignRequestListener listener) {
		this.certEncodedB64 = certEncodedB64;
		this.state = state;
		this.filters = filters != null ? filters.toArray(new String[filters.size()]) : null;
		this.commManager = commManager;
		this.listener = listener;
		this.numPage = numPage;
		this.pageSize = pageSize;
	}

    @Override
    protected PartialSignRequestsList doInBackground(final Void... arg) {

    	// Aqui se carga la lista de peticiones de documentos
    	PartialSignRequestsList signRequests;
    	try {
    		signRequests = this.commManager.getSignRequests(
    				this.certEncodedB64,
    				this.state,
    				this.filters,
    				this.numPage,
    				this.pageSize);
    	}
    	catch (final Exception e) {
    		e.printStackTrace();
    		signRequests = null;
    		Log.e(SFConstants.LOG_TAG, "Ocurrio un error al recuperar las peticiones de firma: " + e); //$NON-NLS-1$
    	}
    	catch (final Throwable t) {
    		t.printStackTrace();
    		signRequests = null;
    		Log.e(SFConstants.LOG_TAG, "Problema grave al listar las peticiones: " + t); //$NON-NLS-1$
    	}

    	return signRequests;
    }

    @Override
	protected void onPostExecute(final PartialSignRequestsList partialSignRequests) {

    	// Si se cancela la operacion, no se actualiza el listado
    	if (isCancelled()) {
    		return;
    	}

		if (partialSignRequests == null) {
			this.listener.errorLoadingSignRequest();
			return;
		}

		final int numPages = partialSignRequests.getTotalSignRequests() / this.pageSize +
				(partialSignRequests.getTotalSignRequests() % this.pageSize == 0 ? 0 : 1);
		this.listener.loadedSignRequest(partialSignRequests.getCurrentSignRequests(), this.numPage, numPages);
    }

    /** Interfaz que gestiona la respuesta a las solicitudes de carga de peticiones de firma. */
    interface LoadSignRequestListener {

    	/** Se ejecuta cuando las peticiones de firma se han cargado correctamente.
    	 * @param signRequests Peticiones de firma cargadas.
    	 * @param pageNumber N&uacute;mero de p&aacute;gina.
    	 * @param numPages N&uacute;mero total de p&aacute;ginas. */
    	void loadedSignRequest(List<SignRequest> signRequests, int pageNumber, int numPages);

    	/** Se ejecuta cuando ocurre un error durante la carga de las peticiones de firma. */
    	void errorLoadingSignRequest();
    }
}
