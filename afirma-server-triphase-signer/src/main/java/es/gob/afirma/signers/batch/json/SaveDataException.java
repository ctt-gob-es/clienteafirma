package es.gob.afirma.signers.batch.json;

/**
 * Se&ntilde;ala un error en el guardado de datos.
 */
public class SaveDataException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = -6398721912218205056L;

	/**
	 * Error en el guardado de datos.
	 * @param msg Mensaje descriptivo del error.
	 * @param cause Motivo del error.
	 */
	public SaveDataException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
