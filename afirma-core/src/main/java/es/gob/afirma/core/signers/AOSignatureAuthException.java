package es.gob.afirma.core.signers;

import es.gob.afirma.core.AOException;

/**
 * Excepci&oacute;n para se&ntilde;alar un error en la autenticaci&oacute;n para el uso de la clave de firma.
 */
public class AOSignatureAuthException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = 3125380192199703407L;

	/**
	 * Crea la excepci&oacute;n con un mensaje asociado.
	 * @param msg Descripci&oacute;n del error.
	 */
	public AOSignatureAuthException(final String msg) {
		super(msg);
	}

	/**
	 * Crea la excepci&oacute;n con la causa de la misma.
	 * @param cause Excepci&oacute;n o error que origin&oacute; el problema.
	 */
	public AOSignatureAuthException(final Throwable cause) {
		super(cause);
	}

	/**
	 * Crea la excepci&oacute;n con un mensaje asociado y la causa que la origin&oacute;.
	 * @param msg Descripci&oacute;n del error.
	 * @param cause Excepci&oacute;n o error que origin&oacute; el problema.
	 */
	public AOSignatureAuthException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
