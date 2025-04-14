package es.gob.afirma.core.keystores;

import es.gob.afirma.core.AOException;

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
		super(message);
	}

	/**
	 * Crea la excepci&oacute;n con la causa del error.
	 * @param cause Causa del error.
	 */
	public AuthenticationException(final Throwable cause) {
		super(cause);
	}

	/**
	 * Crea la excepci&oacute;n con la descripci&oacute;n y causa del error.
	 * @param message Descripci&oacute;n del error.
	 * @param cause Causa del error.
	 */
	public AuthenticationException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
