package es.gob.afirma.core.signers;

import es.gob.afirma.core.AOException;

/**
 * Excepci&oacute;n que se&ntilde;ala un error durante un proceso de firma trif&aacute;fica.
 */
public class AOTriphaseException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = -6210469965589338895L;

	private String serverExceptionClassname = null;

	/**
	 * Crea la excepci&oacute;n con el mensaje asociado.
	 * @param msg Mensaje con el motivo del error.
	 */
	public AOTriphaseException(final String msg) {
		super(msg);
	}

	/**
	 * Crea la excepci&oacute;n con el error que original la excepci&oacute;n.
	 * @param cause Error que origina la excepci&oacute;n.
	 */
	public AOTriphaseException(final Throwable cause) {
		super(cause);
	}

	/**
	 * Crea la excepci&oacute;n con el mensaje y el error que original la
	 * excepci&oacute;n.
	 * @param msg Mensaje con el motivo del error.
	 * @param cause Error que origina la excepci&oacute;n.
	 */
	public AOTriphaseException(final String msg, final Throwable cause) {
		super(msg, cause);
	}

	public void setServerExceptionClassname(final String serverExceptionClassname) {
		this.serverExceptionClassname = serverExceptionClassname;
	}

	public String getServerExceptionClassname() {
		return this.serverExceptionClassname;
	}

	public static AOTriphaseException parseException(final String msg) {
		AOTriphaseException e;
		final int sepPos = msg.indexOf(":"); //$NON-NLS-1$
		if (sepPos > 0) {
			e = new AOTriphaseException(msg.substring(sepPos + 1).trim());
			e.setServerExceptionClassname(msg.substring(0, sepPos).trim());
		}
		else {
			e = new AOTriphaseException(msg.trim());
		}
		return e;
	}
}
