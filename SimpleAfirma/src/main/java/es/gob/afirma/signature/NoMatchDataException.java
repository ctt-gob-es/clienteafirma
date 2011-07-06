package es.gob.afirma.signature;

public class NoMatchDataException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = 1L;

    public NoMatchDataException() {
        super();
    }

    public NoMatchDataException(final String message) {
        super(message);
    }
}
