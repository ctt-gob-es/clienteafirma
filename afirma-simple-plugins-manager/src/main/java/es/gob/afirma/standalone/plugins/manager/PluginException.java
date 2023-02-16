package es.gob.afirma.standalone.plugins.manager;

/**
 * Excepci&oacute;n para indicar cualquier problema relativo a la importaci&oacute;n,
 * manejo o borrado de un plugin.
 */
public class PluginException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = 2365171309887841063L;

	/**
	 * Construye la excepci&oacute;n.
	 */
	public PluginException() {
		super();
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje.
	 * @param msg Mensaje descriptivo del error.
	 */
	public PluginException(String msg) {
		super(msg);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param msg Mensaje descriptivo del error.
	 * @param cause Causa que origin&oacute; el error.
	 */
	public PluginException(String msg, Throwable cause) {
		super(msg, cause);
	}
}
