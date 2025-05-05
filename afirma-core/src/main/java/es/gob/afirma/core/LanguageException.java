package es.gob.afirma.core;

/**
 * Excepci&oacute;n para indicar algun error en un idioma.
 */
public class LanguageException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = 176646986045260013L;

	/**
	 * Construye la excepci&oacute;n.
	 */
	public LanguageException() {
		super();
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje.
	 * @param msg Mensaje descriptivo del error.
	 */
	public LanguageException(final String msg) {
		super(msg);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje y la causa del error.
	 * @param msg Mensaje descriptivo del error.
	 * @param cause Causa que origin&oacute; el error.
	 */
	public LanguageException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
