package es.gob.afirma.core.keystores;

/**
 * Excepci&oacute;n que refleja un error en el acceso a un almac&oacute;n de claves debido a que se
 * encuentra bloqueado.
 */
public class LockedKeyStoreException extends AuthenticationException {

	/** Serial Id. */
	private static final long serialVersionUID = -1662659418498317574L;

	/**
	 * Crea la excepci&oacute;n con la descripci&oacute;n del error.
	 * @param message Descripci&oacute;n del error.
	 */
	public LockedKeyStoreException(final String message) {
		super(message);
	}

	/**
	 * Crea la excepci&oacute;n con la causa del error.
	 * @param cause Causa del error.
	 */
	public LockedKeyStoreException(final Throwable cause) {
		super(cause);
	}

	/**
	 * Crea la excepci&oacute;n con la descripci&oacute;n y causa del error.
	 * @param message Descripci&oacute;n del error.
	 * @param cause Causa del error.
	 */
	public LockedKeyStoreException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
