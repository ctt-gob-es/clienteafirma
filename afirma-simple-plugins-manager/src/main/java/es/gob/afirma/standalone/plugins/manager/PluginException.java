package es.gob.afirma.standalone.plugins.manager;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;

/**
 * Excepci&oacute;n para indicar cualquier problema relativo a la importaci&oacute;n,
 * manejo o borrado de un plugin.
 */
public class PluginException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = 2365171309887841063L;

	/**
	 * Construye la excepci&oacute;n.
	 */
	public PluginException() {
		super(PluginManagerError.Internal.PLUGIN_ERROR);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje.
	 * @param msg Mensaje descriptivo del error.
	 */
	public PluginException(final String msg) {
		super(msg, PluginManagerError.Internal.PLUGIN_ERROR);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param msg Mensaje descriptivo del error.
	 * @param cause Causa que origin&oacute; el error.
	 */
	public PluginException(final String msg, final Throwable cause) {
		super(msg, cause, PluginManagerError.Internal.PLUGIN_ERROR);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param errorCode Codigo del error
	 */
	public PluginException(final ErrorCode errorCode) {
		super(errorCode);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param msg Mensaje descriptivo del error.
	 * @param errorCode Codigo del error
	 */
	public PluginException(final String msg, final ErrorCode errorCode) {
		super(msg, errorCode);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param msg Mensaje descriptivo del error.
	 * @param cause Causa que origin&oacute; el error.
	 * @param errorCode Codigo del error
	 */
	public PluginException(final String msg, final Throwable cause, final ErrorCode errorCode) {
		super(msg, cause, errorCode);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param cause Causa que origin&oacute; el error.
	 * @param errorCode Codigo del error
	 */
	public PluginException(final Throwable cause, final ErrorCode errorCode) {
		super(cause, errorCode);
	}
}
