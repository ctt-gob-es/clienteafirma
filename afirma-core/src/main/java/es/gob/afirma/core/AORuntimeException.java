package es.gob.afirma.core;

/**
 * Excepci&oacute;n gen&eacute;rica que aquellas excepciones que pueden ser lanzadas en tiempo de ejecuci&oacute;n sin generar error.
 */
public class AORuntimeException extends RuntimeException implements AOControlledException {

	/** Serial Id. */
	private static final long serialVersionUID = 2606268571651129316L;

	private ErrorCode errorCode = null;

	/**
	 * Contruye una excepci&oacute;n gen&eacute;rica con el c&oacute;digo de error.
	 * @param code C&oacute;dico que identifica al error.
	 */
	public AORuntimeException(final ErrorCode code) {
		super(code != null ? code.getDescription() : null);
		this.errorCode = code;
	}

	/**
	 * Contruye una excepci&oacute;n gen&eacute;rica con mensaje y c&oacute;digo de error.
	 * @param msg Descripci&oacute;n del error.
	 * @param code C&oacute;dico que identifica al error.
	 */
	public AORuntimeException(final String msg, final ErrorCode code) {
		super(msg);
		this.errorCode = code;
	}

	/**
	 * Contruye una excepci&oacute;n gen&eacute;rica y define su causa.
	 * @param cause Causa del error.
	 * @param code C&oacute;dico que identifica al error.
	 */
	public AORuntimeException(final Throwable cause, final ErrorCode code) {
		super(code != null ? code.getDescription() : null, cause);
		this.errorCode = code;
	}

	/**
	 * Contruye una excepci&oacute;n gen&eacute;rica con mensaje y define su causa.
	 * @param msg Descripci&oacute;n del error.
	 * @param cause Causa del error.
	 * @param code C&oacute;dico que identifica al error.
	 */
	public AORuntimeException(final String msg, final Throwable cause, final ErrorCode code) {
		super(msg, cause);
		this.errorCode = code;
	}

	@Override
	public ErrorCode getErrorCode() {
		return this.errorCode;
	}

	@Override
	public String toString() {
		String appendix = null;
		if (this.errorCode != null) {
			appendix = " (" + this.errorCode + ")"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		return appendix != null ? super.toString() + appendix : super.toString();
	}
}
