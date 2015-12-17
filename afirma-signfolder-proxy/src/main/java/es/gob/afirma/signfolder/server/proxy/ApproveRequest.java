package es.gob.afirma.signfolder.server.proxy;

/**
 * Solicitud de visto bueno.
 */
public class ApproveRequest {

	private final String requestId;

	private boolean ok;

	/**
	 * Construye la solicitud de visto bueno para una petici&oacute;n.
	 * @param requestId Identificador de la petici&oacute;n.
	 */
	public ApproveRequest(final String requestId) {
		this.requestId = requestId;
		this.ok = true;
	}

	/**
	 * Construye una respuesta a la solicitud de visto bueno para una petici&oacute;n con su resultado.
	 * @param requestId Identificador de la petici&oacute;n.
	 * @param ok Resultado de la operaci&oacute;n.
	 */
	public ApproveRequest(final String requestId, final boolean ok) {
		this.requestId = requestId;
		this.ok = ok;
	}

	/**
	 * Indica si la operaci&oacute; es correcta.
	 * @return {@code true} si la operaci&oacute; se ha procesado hasta el momento
	 * correctamente, {@code false} en caso contrario.
	 */
	public boolean isOk() {
		return this.ok;
	}

	/**
	 * Establece el estado de la operaci&oacute;n.
	 * @param ok Estado.
	 */
	public void setOk(final boolean ok) {
		this.ok = ok;
	}

	/**
	 * Recupera el identificador de la operaci&oacute;n.
	 * @return Identificador.
	 */
	public String getRequestId() {
		return this.requestId;
	}


}
