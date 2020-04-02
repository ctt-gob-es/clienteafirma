package es.gob.afirma.signvalidation;

import es.gob.afirma.core.AOException;

/**
 * Excepci&oacute;n para indicar que una firma no es v&aacute;lida.
 */
public class InvalidSignatureException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = -6420658430794700293L;

	/**
	 * Construye la excepci&oacute;n con una descripci&oacute;n.
	 * @param msg Descripci&oacute;n del error.
	 */
	public InvalidSignatureException(final String msg) {
		super(msg);
	}

	/**
	 * Construye la excepci&oacute;n con el motivo que lo origin&oacute;.
	 * @param cause Motivo del error.
	 */
	public InvalidSignatureException(final Throwable cause) {
		super(cause);
	}

	/**
	 * Construye la excepci&oacute;n con una descripci&oacute;n.
	 * @param msg Descripci&oacute;n del error.
	 * @param cause Motivo del error.
	 */
	public InvalidSignatureException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
