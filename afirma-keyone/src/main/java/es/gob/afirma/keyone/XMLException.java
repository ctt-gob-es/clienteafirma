package es.gob.afirma.keyone;

public class XMLException extends Exception {

	private static final long serialVersionUID = -7418222120511264939L;

	/** Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
     * @param msg Mensaje de la excepci&oacute;n */
    public XMLException(final String msg) {
        super(msg);
    }

    /** Contruye una excepci&oacute;n gen&eacute;rica con mensaje y define su causa.
     * @param msg Descripci&oacute;n del error.
     * @param cause Causa del error. */
    public XMLException(final String msg, final Throwable cause) {
        super(msg, cause);
    }

    /** Contruye una excepci&oacute;n gen&eacute;rica definiendo su causa.
     * @param cause Causa del error. */
    public XMLException(final Throwable cause) {
        super(cause);
    }
}