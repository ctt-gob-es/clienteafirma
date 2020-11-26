package es.gob.afirma.plugin.hash;

/**
 * Excepci&acute;n que denota un error en la generaci&oacute;n del documento
 * con los hashes de un directorio o la carga de un documento de este tipo.
 */
public class DocumentException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = 318461177231931408L;

	/**
	 * Construye la excepci&oacute;n.
	 * @param msg Mensaje descritivo del error.
	 */
	public DocumentException(final String msg) {
		super(msg);
	}

	/**
	 * Construye la excepci&oacute;n.
	 * @param msg Mensaje descritivo del error.
	 * @param cause Motivo del error.
	 */
	public DocumentException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
