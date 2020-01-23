package es.gob.afirma.standalone.ui.preferences;

/**
 * Excepci&oacute;n con la que se notifica un error al guardar o cargar configuraci&oacute;n.
 * En su mensaje s&oacute;lo se debe guardar el mensaje del usuario.
 */
public class ConfigurationException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = 2228227768602252202L;

	/**
	 * Construye la excepci&oacute;n.
	 */
	public ConfigurationException() {
		super();
	}

	/**
	 * Construye la excepci&oacute;n.
	 * @param msg Mensaje legible con el motivo del error.
	 */
	public ConfigurationException(final String msg) {
		super(msg);
	}

	/**
	 * Construye la excepci&oacute;n.
	 * @param cause Origen del error.
	 */
	public ConfigurationException(final Throwable cause) {
		super(cause);
	}

	/**
	 * Construye la excepci&oacute;n.
	 * @param msg Mensaje legible con el motivo del error.
	 * @param cause Origen del error.
	 */
	public ConfigurationException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
