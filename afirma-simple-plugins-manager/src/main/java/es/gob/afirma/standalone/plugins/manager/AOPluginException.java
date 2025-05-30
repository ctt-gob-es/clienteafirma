package es.gob.afirma.standalone.plugins.manager;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;

/**
 * Excepci&oacute;n para indicar cualquier problema relativo a la importaci&oacute;n,
 * manejo o borrado de un plugin.
 */
public class AOPluginException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = 2365171309887841063L;

	/**
	 * Construye la excepci&oacute;n.
	 * @param ec C&oacute;digo de error.
	 */
	public AOPluginException(final ErrorCode ec) {
		super(ec);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje.
	 * @param msg Mensaje descriptivo del error.
	 * @param ec C&oacute;digo de error.
	 */
	public AOPluginException(final String msg, final ErrorCode ec) {
		super(msg, ec);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param msg Mensaje descriptivo del error.
	 * @param cause Causa que origin&oacute; el error.
	 * @param ec C&oacute;digo de error.
	 */
	public AOPluginException(final String msg, final Throwable cause, final ErrorCode ec) {
		super(msg, cause, ec);
	}
}
