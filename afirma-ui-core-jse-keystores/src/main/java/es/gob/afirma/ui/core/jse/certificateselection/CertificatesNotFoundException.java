package es.gob.afirma.ui.core.jse.certificateselection;

public class CertificatesNotFoundException extends Exception {

    /** Crea la excepci&oacute;n con el mensaje indicado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n. */
    public CertificatesNotFoundException(final String msg) {
        super(msg);
    }

    /** Crea la excepci&oacute;n con el mensaje indicado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n.
     * @param cause
     * 		  Motivo que origin&oacute; el error. */
    public CertificatesNotFoundException(final String msg, final Throwable cause) {
        super(msg, cause);
    }
}
