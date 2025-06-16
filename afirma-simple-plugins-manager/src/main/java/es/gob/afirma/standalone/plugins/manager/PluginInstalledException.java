package es.gob.afirma.standalone.plugins.manager;

import es.gob.afirma.core.ErrorCode;

/**
 * Excepci&oacute;n para indicar que un plugin ya se encontraba instalado.
 */
public class PluginInstalledException extends PluginException {

	/** Serial Id. */
	private static final long serialVersionUID = 176646986045260013L;

	/**
	 * Construye la excepci&oacute;n.
	 */
	public PluginInstalledException() {
		super();
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje.
	 * @param msg Mensaje descriptivo del error.
	 */
	public PluginInstalledException(String msg) {
		super(msg);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param msg Mensaje descriptivo del error.
	 * @param cause Causa que origin&oacute; el error.
	 */
	public PluginInstalledException(String msg, Throwable cause) {
		super(msg, cause);
	}
	
	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param msg Mensaje descriptivo del error.
	 * @param errorCode Codigo del error
	 */
	public PluginInstalledException(String msg, ErrorCode errorCode) {
		super(msg, errorCode);
	}
	
	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param msg Mensaje descriptivo del error.
	 * @param cause Causa que origin&oacute; el error.
	 * @param errorCode Codigo del error
	 */
	public PluginInstalledException(String msg, Throwable cause, ErrorCode errorCode) {
		super(msg, cause, errorCode);
	}
}
