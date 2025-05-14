package es.gob.afirma.core;

/**
 * Excepci&oacute;n gen&eacute;rica que aquellas excepciones que pueden ser lanzadas en tiempo de ejecuci&oacute;n sin generar error.
 */
public class AORuntimeException extends RuntimeException {

	/** Serial Id. */
	private static final long serialVersionUID = 2606268571651129316L;

	private ErrorCode errorCode = null;

	/**
	 * Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
	 * @param msg Descripci&oacute;n del error.
	 */
	public AORuntimeException(final ErrorCode code) {
		super(code != null ? code.getDescription() : null);
		this.errorCode = code;
	}

	/**
	 * Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
	 * @param msg Descripci&oacute;n del error.
	 */
	public AORuntimeException(final String msg, final ErrorCode code) {
		super(msg);
		this.errorCode = code;
	}

	/**
	 * Contruye una excepci&oacute;n gen&eacute;rica y define su causa.
	 * @param cause Causa del error.
	 */
	public AORuntimeException(final Throwable cause, final ErrorCode code) {
		super(code != null ? code.getDescription() : null, cause);
		this.errorCode = code;
	}

	/**
	 * Contruye una excepci&oacute;n gen&eacute;rica con mensaje y define su causa.
	 * @param msg Descripci&oacute;n del error.
	 * @param cause Causa del error.
	 */
	public AORuntimeException(final String msg, final Throwable cause, final ErrorCode code) {
		super(msg, cause);
		this.errorCode = code;
	}

	/**
	 * Recupera el c&oacute;digo de error asociado a la excepci&oacute;n,
	 * @return C&oacute;digo de error o {@code null} si no se defini&oacute;n.
	 */
	public ErrorCode getErrorCode() {
		return this.errorCode;
	}

	@Override
	public String toString() {
		if (this.errorCode != null) {
			return this.errorCode.toString();
		}
		return super.toString();
	}
}
