package es.gob.afirma.triphase.server;

public class BadRequestException extends ServletResponseException {
	
	/** Serial Id. */
	private static final long serialVersionUID = -5675971275489024227L;

	public BadRequestException(final String message) {
		super(message);
	}
	
	public BadRequestException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
