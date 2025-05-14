package es.gob.afirma.core.keystores;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;

/**
 * Excepci&oacute;n que refleja un error al autenticarse en un almac&oacute;n de claves.
 */
public class AuthenticationException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = 7055233482951963862L;

	/**
	 * Crea la excepci&oacute;n con la descripci&oacute;n del error.
	 * @param message Descripci&oacute;n del error.
	 */
	public AuthenticationException(final String message) {
		super(message, ErrorCode.Internal.SIGNINIG_KEY_AUTHENTICATION_ERROR);
	}

	/**
	 * Crea la excepci&oacute;n con la causa del error.
	 * @param cause Causa del error.
	 */
	public AuthenticationException(final Throwable cause) {
		super(cause, ErrorCode.Internal.SIGNINIG_KEY_AUTHENTICATION_ERROR);
	}

	/**
	 * Crea la excepci&oacute;n con la descripci&oacute;n y causa del error.
	 * @param message Descripci&oacute;n del error.
	 * @param cause Causa del error.
	 */
	public AuthenticationException(final String message, final Throwable cause) {
		super(message, cause, ErrorCode.Internal.SIGNINIG_KEY_AUTHENTICATION_ERROR);
	}

	/**
	 * Crea la excepci&oacute;n con la descripci&oacute;n del error.
	 * @param message Descripci&oacute;n del error.
	 */
	protected AuthenticationException(final String message, final ErrorCode errorCode) {
		super(message, errorCode);
	}

	/**
	 * Crea la excepci&oacute;n con la causa del error.
	 * @param cause Causa del error.
	 */
	protected AuthenticationException(final Throwable cause, final ErrorCode errorCode) {
		super(cause, errorCode);
	}

	/**
	 * Crea la excepci&oacute;n con la descripci&oacute;n y causa del error.
	 * @param message Descripci&oacute;n del error.
	 * @param cause Causa del error.
	 */
	protected AuthenticationException(final String message, final Throwable cause, final ErrorCode errorCode) {
		super(message, cause, errorCode);
	}
}
