package es.atosorigin;

import java.util.Date;

/** Excepcion lanzada cuando se detecta que un certificado ha sido revocado.
 * @author Carlos Gamuci */
public class AOCertificateRevokedException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = -4433892673404934489L;

    /** Fecha de revocaci&oacute;n. */
    private Date revocationDate = null;

    /** Motivo de la revocaci&oacute;n. */
    private String revocationReason = null;

    /** Crea una excepcion para la notificaci&oacute;n de que se ha detectado que
     * el certificado que se valid&oacute; estaba revocado.
     * @param message Mensaje de error.
     * @param cause Excepci&oacute;n que causo el error. */
    public AOCertificateRevokedException(final String message, final Throwable cause) {
        super(message, cause);
    }

    public Date getRevocationDate() {
        return revocationDate;
    }

    public void setRevocationDate(Date revocationDate) {
        this.revocationDate = revocationDate;
    }

    public String getRevocationReason() {
        return revocationReason;
    }

    public void setRevocationReason(String revocationReason) {
        this.revocationReason = revocationReason;
    }
}
