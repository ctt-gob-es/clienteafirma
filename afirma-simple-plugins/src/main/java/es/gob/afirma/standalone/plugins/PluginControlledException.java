package es.gob.afirma.standalone.plugins;

/**
 *
 */
public class PluginControlledException extends Exception {

	/**
	 * @param message
	 */
	public PluginControlledException(String message) {
		super(message);
	}

	/**
	 * @param message
	 * @param cause
	 */
	public PluginControlledException(String message, Throwable cause) {
		super(message, cause);
	}
}
