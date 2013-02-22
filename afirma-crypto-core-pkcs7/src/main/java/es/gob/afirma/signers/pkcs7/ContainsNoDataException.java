package es.gob.afirma.signers.pkcs7;

import es.gob.afirma.core.AOInvalidFormatException;

/**
 * Excepci&oacute;n para indicar que no se puede realizar una multifirma porque la firma original
 * no contiene los datos necesarios.
 * @author Carlos Gamuci Mill&aacute;n
 */
public class ContainsNoDataException extends AOInvalidFormatException {

	/** Serial version id */
	private static final long serialVersionUID = -114813694395801268L;

	/**
	 * Crea la excepci&oacute;n asociando un mensaje de error.
	 * @param message Mensaje asociado al error.
	 */
	public ContainsNoDataException(final String message) {
		super(message);
	}

	/**
	 * Crea la excepci&oacute;n asociando un mensaje de error y la causa del problema.
	 * @param message Mensaje asociado al error.
	 * @param e Excepcion previa que caus&oacute; el problema.
	 */
	public ContainsNoDataException(final String message, final Exception e) {
		super(message, e);
	}
}
