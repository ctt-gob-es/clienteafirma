package es.gob.afirma.signature;

/**
 * Indica cuando los datos contenidos en una firma no coincide con los datos firmados.
 * @author Carlos Gamuci
 */
public class NoMatchDataException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = 1L;

    /**
     * Indica que los datos contenidos en la firma no coinciden con los datos firmados.
     */
    public NoMatchDataException() {
        super();
    }

    /**
     * Indica que los datos contenidos en la firma no coinciden con los datos firmados.
     * @param message Mensaje que detalle el error.
     */
    public NoMatchDataException(final String message) {
        super(message);
    }
}
