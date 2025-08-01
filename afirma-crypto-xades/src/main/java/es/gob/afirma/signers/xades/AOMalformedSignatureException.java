package es.gob.afirma.signers.xades;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;

/**
 * Excepci&oacute;n que indica cuando se encuentra una firma mal formada.
 */
public class AOMalformedSignatureException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = -2816278896509814440L;

	/**
	 * Construye la excepi&oacute;n con la descripci&oacute;n del problema.
	 * @param msg Texto descriptivo del problema.
	 */
	public AOMalformedSignatureException(final String msg) {
		super(msg, ErrorCode.Functional.SIGNING_MALFORMED_SIGNATURE);
	}

	/**
	 * Construye la excepi&oacute;n con la descripci&oacute;n del problema
	 * y su causa.
	 * @param msg Texto descriptivo del problema.
	 * @param cause Causa del problema.
	 */
	public AOMalformedSignatureException(final String msg, final Throwable cause) {
		super(msg, cause, ErrorCode.Functional.SIGNING_MALFORMED_SIGNATURE);
	}
}
