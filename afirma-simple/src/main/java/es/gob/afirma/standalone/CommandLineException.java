package es.gob.afirma.standalone;

/** Excepcion que encapsula cualquier excepcion que se lance durante la operativa del SimpleAfirma en modo consola.
 * @author Carlos Gamuci */
public final class CommandLineException extends Exception {

	private static final long serialVersionUID = 3756189019018723691L;

	CommandLineException(final String message) {
		super(message);
	}

	CommandLineException(final Throwable cause) {
		super(cause);
	}

	CommandLineException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
