package es.gob.afirma.keystores;

/**
 * Excepci&oacute;n que denota un error derivado del uso de una tarjeta inteligente.
 */
public class SmartCardException extends RuntimeException {

	/** Serial Id. */
	private static final long serialVersionUID = -7200145202118291387L;

	/**
	 * Se contruye la excepci&oacute;n con un mensaje descriptivo.
	 * @param msg Mensaje que describe el error que origin&oacute; la excepci&oacurte;n.
	 */
	public SmartCardException(final String msg) {
		super(msg);
	}

	/**
	 * Se contruye la excepci&oacute;n con un mensaje descriptivo y el error que la
	 * origin&oacute;.
	 * @param msg Mensaje que describe el error que origin&oacute; la excepci&oacurte;n.
	 * @param t Error que origin&oacute; la excepci&oacute;n.
	 */
	public SmartCardException(final String msg, final Throwable t) {
		super(msg, t);
	}
}
