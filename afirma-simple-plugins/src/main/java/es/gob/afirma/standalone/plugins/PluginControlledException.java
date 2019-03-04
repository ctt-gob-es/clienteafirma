package es.gob.afirma.standalone.plugins;

/**
 *
 */
public class PluginControlledException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = 8060568079042460019L;

	/**
	 * @param message
	 */
	public PluginControlledException(final String message) {
		super(message);
	}

	/**
	 * @param message
	 * @param cause
	 */
	public PluginControlledException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
