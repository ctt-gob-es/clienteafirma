package es.gob.afirma.signfolder.server.proxy;

/**
 * Solicitud de visto bueno.
 */
public class ApproveRequest {

	private final String requestTagId;

	private boolean ok;

	/**
	 * Construye la solicitud de visto bueno para una petici&oacute;n.
	 * @param requestTagId Identificador de la petici&oacute;n.
	 */
	public ApproveRequest(final String requestTagId) {
		this.requestTagId = requestTagId;
		this.ok = true;
	}

	/**
	 * Construye una respuesta a la solicitud de visto bueno para una petici&oacute;n con su resultado.
	 * @param requestTagId Identificador de la petici&oacute;n.
	 * @param ok Resultado de la operaci&oacute;n.
	 */
	public ApproveRequest(final String requestTagId, final boolean ok) {
		this.requestTagId = requestTagId;
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
	 * Recupera el tag identificador de la operaci&oacute;n.
	 * @return Identificador.
	 */
	public String getRequestTagId() {
		return this.requestTagId;
	}


}
