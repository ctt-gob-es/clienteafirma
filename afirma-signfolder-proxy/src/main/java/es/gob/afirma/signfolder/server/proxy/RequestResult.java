package es.gob.afirma.signfolder.server.proxy;

/**
 * Resultado de una operaci&oacute;n particular sobre una petici&oacute;n.
 */
public class RequestResult {

	private final String id;

	private final boolean statusOk;

	/**
	 * Resultado de una petici&oacute;n particular.
	 * @param id Identificador de la petici&oacute;n.
	 * @param ok Resultado de la petici&oacute;n.
	 */
	public RequestResult(final String id, final boolean ok) {
		this.id = id;
		this.statusOk = ok;
	}

	/**
	 * Recupera el identificador de la petici&oacute;n.
	 * @return Identificador.
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * Recupera el resultado de la operaci&oacute;n sobre la petici&oacute;n.
	 * @return {@code true} si la operaci&oacute;n finaliz&oacute; correctamente, {@code false}
	 * en caso contrario.
	 */
	public boolean isStatusOk() {
		return this.statusOk;
	}
}
