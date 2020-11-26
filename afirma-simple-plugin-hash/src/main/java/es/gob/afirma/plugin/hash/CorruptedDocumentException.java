package es.gob.afirma.plugin.hash;

/**
 * Excepci&oacute;n que identifica cuando se ha detectado que un documento de huellas
 * digitales ha sido modificado o ha quedado corrupto.
 */
public class CorruptedDocumentException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = -2577002611261215291L;

	/**
	 * Constructor por defecto.
	 */
	public CorruptedDocumentException() {
		super();
	}

	/**
	 * Construye la excepci&oacute;n junto con una descripci&oacute;n del
	 * error.
	 * @param desc Descripci&oacute;n del error.
	 */
	public CorruptedDocumentException(final String desc) {
		super(desc);
	}
}
