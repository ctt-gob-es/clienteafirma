package es.gob.afirma.signfolder.proxy;

/** Respuesta a la petici&oacute;n de solicitudes de firma.
 * @author Carlos Gamuci */
public class RequestListResponseBean {

	/** Listado de peticiones de firma. */
	private final SignRequest[] signRequests;

	/** Construye un objeto a partir de un listado de las solicitudes de firma.
	 * @param signRequests Solicitudes de firma. */
	public RequestListResponseBean(final SignRequest[] signRequests) {
		this.signRequests = signRequests; //TODO: No se puede clonar por JME, hay que copiar con System.ArrayCopy
	}

	/** Recupera el listado de solicitudes de firma.
	 * @return Solicitudes de firma. */
	public SignRequest[] getSignRequests() {
		return this.signRequests; //TODO: No se puede clonar por JME, hay que copiar con System.ArrayCopy
	}

}
