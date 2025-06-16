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
		super(ErrorCode.Internal.PLUGIN_ERROR);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje.
	 * @param msg Mensaje descriptivo del error.
	 */
	public PluginException(String msg) {
		super(msg, ErrorCode.Internal.PLUGIN_ERROR);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param msg Mensaje descriptivo del error.
	 * @param cause Causa que origin&oacute; el error.
	 */
	public PluginException(String msg, Throwable cause) {
		super(msg, cause, ErrorCode.Internal.PLUGIN_ERROR);
	}
	
	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param errorCode Codigo del error
	 */
	public PluginException(ErrorCode errorCode) {
		super(errorCode);
	}
	
	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param msg Mensaje descriptivo del error.
	 * @param errorCode Codigo del error
	 */
	public PluginException(String msg, ErrorCode errorCode) {
		super(msg, errorCode);
	}
	
	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param msg Mensaje descriptivo del error.
	 * @param cause Causa que origin&oacute; el error.
	 * @param errorCode Codigo del error
	 */
	public PluginException(String msg, Throwable cause, ErrorCode errorCode) {
		super(msg, cause, errorCode);
	}
	
	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param cause Causa que origin&oacute; el error.
	 * @param errorCode Codigo del error
	 */
	public PluginException(Throwable cause, ErrorCode errorCode) {
		super(cause, errorCode);
	}
}
