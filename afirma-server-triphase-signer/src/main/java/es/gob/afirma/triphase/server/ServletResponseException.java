package es.gob.afirma.triphase.server;

public class ServletResponseException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = 5005319987537946042L;	
	
	public ServletResponseException(final String message) {
		super(message);
	}
	
	public ServletResponseException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
