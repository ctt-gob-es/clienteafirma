package es.gob.afirma.standalone.configurator;

/**
 * Excepci&oacute;n para indicar que se ha insertado una contrase&ntilde;a
 * incorrecta.
 */
public class InvalidPasswordException extends SecurityException {
    /** Serial Id. */
    private static final long serialVersionUID = -9058805499745499488L;

	public InvalidPasswordException(final String message) {
		super(message);
	}
}
