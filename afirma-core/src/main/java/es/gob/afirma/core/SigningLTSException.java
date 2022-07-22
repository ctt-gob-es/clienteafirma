package es.gob.afirma.core;

/**
 * Excepci&oacute;n que denota que no se puede cofirmar o contrafirmar
 * una firma porque alguna firma previa contiene informaci&oacute;n de
 * archivo que quedar&iacute;a invalidada.
 */
public class SigningLTSException extends RuntimeConfigNeededException {

	/** Serial Id. */
	private static final long serialVersionUID = 995443738935981665L;

	private static final String REQUESTOR_MSG_CODE = "signingLts"; //$NON-NLS-1$

	private static final String EXTRA_PARAM_NEEDED = "forceSignLTSignature"; //$NON-NLS-1$

	/**
	 * Crear la excepci&oacute;n motivada por lo indicado en el mensaje.
	 * @param message Mensaje descriptivo del problema.
	 */
	public SigningLTSException(final String message) {
		super(message, RuntimeConfigNeededException.RequestType.CONFIRM, REQUESTOR_MSG_CODE, EXTRA_PARAM_NEEDED);
	}
}
