package es.gob.afirma.core.keystores;

import es.gob.afirma.core.AOException;

/**
 * Excepci&oacute;n que refleja que el usuario cancel&oacute; una operaci&oacute;n relacionada con un
 * certificado en tarjeta.
 */
public class AOCancelledSMOperationException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = -1662659418498317574L;

	/**
	 * Crea la excepci&oacute;n con la descripci&oacute;n del error.
	 * @param message Descripci&oacute;n del error.
	 */
	public AOCancelledSMOperationException(final String message) {
		super(message);
	}
}
