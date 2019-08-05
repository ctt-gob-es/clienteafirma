package es.gob.afirma.standalone.ui;

/**
 * Excepci&oacute;n que indica un error al realizar una firma, dentro de un proceso de varias firmas.
 */
public class SingleSignatureException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = 5934265863796025388L;

	/**
	 * Crea la excepci&oacute;n.
	 */
	public SingleSignatureException() {
		super();
	}

	/**
	 * Crea la excepci&oacute;n con un mensaje descriptivo del problema.
	 * @param msg Mensaje de error.
	 */
	public SingleSignatureException(final String msg) {
		super(msg);
	}

	/**
	 * Crea la excepci&oacute;n con un mensaje descriptivo del problema y la causa que la origin&oacute;.
	 * @param cause Causa que origin&oacute; la excepci&oacute;n.
	 */
	public SingleSignatureException(final Throwable cause) {
		super(cause);
	}

	/**
	 * Crea la excepci&oacute;n con un mensaje descriptivo del problema y la causa que la origin&oacute;.
	 * @param msg Mensaje de error.
	 * @param cause Causa que origin&oacute; la excepci&oacute;n.
	 */
	public SingleSignatureException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
