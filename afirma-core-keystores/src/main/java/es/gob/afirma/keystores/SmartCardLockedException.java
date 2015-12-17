package es.gob.afirma.keystores;

/**
 * Excepci&oacute;n para se&mntilde;alar un error debido a que se encontr&oacute; una
 * tarjeta inteligente bloqueada.
 */
public class SmartCardLockedException extends SmartCardException {

	/** Serial Id. */
	private static final long serialVersionUID = 482605403233218951L;

	/**
	 * Se contruye la excepci&oacute;n con un mensaje descriptivo.
	 * @param msg Mensaje que describe el error que origin&oacute; la excepci&oacurte;n.
	 */
	public SmartCardLockedException(final String msg) {
		super(msg);
	}

	/**
	 * Se contruye la excepci&oacute;n con un mensaje descriptivo y el error que la
	 * origin&oacute;.
	 * @param msg Mensaje que describe el error que origin&oacute; la excepci&oacurte;n.
	 * @param t Error que origin&oacute; la excepci&oacute;n.
	 */
	public SmartCardLockedException(final String msg, final Throwable t) {
		super(msg, t);
	}
}
