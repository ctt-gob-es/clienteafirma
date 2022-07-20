package es.gob.afirma.core;

/**
 * Excepci&oacute;n que denota que no se puede cofirmar o contrafirmar
 * una firma porque alguna firma previa contiene informaci&oacute;n de
 * archivo que quedar&iacute;a invalidada.
 */
public class SigningLTSException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = 995443738935981665L;

	/**
	 * Crear la excepci&oacute;n motivada por lo indicado en el mensaje.
	 * @param message Mensaje descriptivo del problema.
	 */
	public SigningLTSException(final String message) {
		super(message);
	}
}
