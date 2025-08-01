package es.gob.afirma.core.keystores;

import es.gob.afirma.core.ErrorCode;

/**
 * Excepci&oacute;n que refleja un error en el acceso a un almac&oacute;n de claves debido a que el
 * PIN usado es incorrecto.
 */
public class PinException extends AuthenticationException {

	/** Serial Id. */
	private static final long serialVersionUID = 2287275718431510008L;

	/**
	 * Crea la excepci&oacute;n con la descripci&oacute;n del error.
	 * @param message Descripci&oacute;n del error.
	 * @param errorCode C&oacute;digo identificativo del error.
	 */
	public PinException(final String message, final ErrorCode errorCode) {
		super(message, errorCode);
	}

	/**
	 * Crea la excepci&oacute;n con la causa del error.
	 * @param cause Causa del error.
	 * @param errorCode C&oacute;digo identificativo del error.
	 */
	public PinException(final Throwable cause, final ErrorCode errorCode) {
		super(cause, errorCode);
	}

	/**
	 * Crea la excepci&oacute;n con la descripci&oacute;n y causa del error.
	 * @param message Descripci&oacute;n del error.
	 * @param cause Causa del error.
	 * @param errorCode C&oacute;digo identificativo del error.
	 */
	public PinException(final String message, final Throwable cause, final ErrorCode errorCode) {
		super(message, cause, errorCode);
	}
}
