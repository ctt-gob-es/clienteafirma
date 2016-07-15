package es.gob.afirma.keyone;

public class PdfException extends Exception {

	private static final long serialVersionUID = -8289354095197221368L;

	/** Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
     * @param msg Mensaje de la excepci&oacute;n */
    public PdfException(final String msg) {
        super(msg);
    }

    /** Contruye una excepci&oacute;n gen&eacute;rica con mensaje y define su causa.
     * @param msg Descripci&oacute;n del error.
     * @param cause Causa del error. */
    public PdfException(final String msg, final Throwable cause) {
        super(msg, cause);
    }

    /** Contruye una excepci&oacute;n gen&eacute;rica definiendo su causa.
     * @param cause Causa del error. */
    public PdfException(final Throwable cause) {
        super(cause);
    }
}