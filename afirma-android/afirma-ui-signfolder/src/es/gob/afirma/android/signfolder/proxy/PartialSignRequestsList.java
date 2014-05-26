package es.gob.afirma.android.signfolder.proxy;

import java.util.List;

/**
 * Listado parcial del peticiones de firma.
 * @author Carlos Gamuci
 */
public class PartialSignRequestsList {

	private final List<SignRequest> currentSignRequests;
	
	private final int totalSignRequests;
	
	/**
	 * Construye el listado parcial.
	 * @param currentSignRequests
	 * @param totalSignRequests
	 */
	public PartialSignRequestsList(final List<SignRequest> currentSignRequests, final int totalSignRequests) {
		this.currentSignRequests = currentSignRequests;
		this.totalSignRequests = totalSignRequests;
	}

	/**
	 * Recupera el listado de peticiones de firma cargadas.
	 * @return Listado de peticiones.
	 */
	public List<SignRequest> getCurrentSignRequests() {
		return this.currentSignRequests;
	}

	/**
	 * Recupera el n&uacute;mero total de peticiones
	 * @return N&uacute;mero de peticiones.
	 */
	public int getTotalSignRequests() {
		return this.totalSignRequests;
	}
}
