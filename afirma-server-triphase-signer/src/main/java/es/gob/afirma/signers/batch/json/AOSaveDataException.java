package es.gob.afirma.signers.batch.json;

import es.gob.afirma.core.AOException;

/**
 * Se&ntilde;ala un error en el guardado de datos.
 */
public class AOSaveDataException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = -6398721912218205056L;

	/**
	 * Error en el guardado de datos.
	 * @param msg Mensaje descriptivo del error.
	 * @param cause Motivo del error.
	 */
	public AOSaveDataException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
