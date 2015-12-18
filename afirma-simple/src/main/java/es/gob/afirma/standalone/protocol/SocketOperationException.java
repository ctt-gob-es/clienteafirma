package es.gob.afirma.standalone.protocol;

/** Error usado para indicar que es necesaria la comunicaci&oacute;n por
 * socket para realizar una operaci&oacute;n. */
class SocketOperationException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = -1031351741046263364L;

	private String errorCode ;

	SocketOperationException(final String code) {
		setErrorCode(code);
	}

	public String getErrorCode() {
		return this.errorCode;
	}

	public void setErrorCode(final String errorCode) {
		this.errorCode = errorCode;
	}


}