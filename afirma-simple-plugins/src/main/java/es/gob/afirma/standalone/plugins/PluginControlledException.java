package es.gob.afirma.standalone.plugins;

/**
 * Excepci&oacute;n utilizada por los plugins cuando ocurre un error en alguna de sus operaciones. Este tipo
 * de excepci&oacute;n siempre ser&aacute;a controlada por Autofirma.
 */
public class PluginControlledException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = 8060568079042460019L;

	/**
	 * Construye la excepci&oacute;n con un mensaje describiendo el error.
	 * @param message Mensaje que describe el error.
	 */
	public PluginControlledException(final String message) {
		super(message);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje describiendo el error y la causa del mismo.
	 * @param message Mensaje que describe el error.
	 * @param cause Motivi por el cual se lanza la excepci&oacute;n.
	 */
	public PluginControlledException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
