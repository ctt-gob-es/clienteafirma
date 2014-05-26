package es.gob.afirma.signfolder.server.proxy;

/**
 * Listado parcial del peticiones de firma.
 * @author Carlos Gamuci
 */
public class PartialSignRequestsList {

	private final SignRequest[] currentSignRequests;
	
	private final int totalSignRequests;
	
	/**
	 * Construye el listado parcial.
	 * @param currentSignRequests
	 * @param totalSignRequests
	 */
	public PartialSignRequestsList(final SignRequest[] currentSignRequests, final int totalSignRequests) {
		this.currentSignRequests = currentSignRequests;
		this.totalSignRequests = totalSignRequests;
	}

	/**
	 * Recupera el listado de peticiones de firma cargadas.
	 * @return Listado de peticiones.
	 */
	public SignRequest[] getCurrentSignRequests() {
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
