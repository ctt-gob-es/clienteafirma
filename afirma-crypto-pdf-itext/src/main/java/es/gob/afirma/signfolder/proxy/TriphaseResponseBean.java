package es.gob.afirma.signfolder.proxy;

/**
 * Respuesta a una petici&oacute;n de firma trif&aacute;sica de peticiones de documentos.
 * @author Carlos Gamuci
 */
public class TriphaseResponseBean {

	/** Peticiones de firma trif&aacute;sica de documentos respondidas. */
	private final TriphaseRequest[] triphaseRequests;

	/**
	 * Crea un objeto con la respuesta a las peticiones de firma trif&aacute;sica de documentos.
	 * @param triphaseRequests Peticiones con sus respuestas.
	 */
	public TriphaseResponseBean(final TriphaseRequest[] triphaseRequests) {
		this.triphaseRequests = triphaseRequests;
	}

	/**
	 * Recupera el listado de las respuestas a las peticiones firma de documentos.
	 * @return Peticiones de firma de documentos respondidas.
	 */
	public TriphaseRequest[] getTriphaseRequests() {
		return this.triphaseRequests;
	}
}
