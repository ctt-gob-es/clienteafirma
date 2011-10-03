/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.util;

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

    /** Obtiene la fecha de revocaci&ocaite;n del certificado.
     * @return Fecha de revocaci&ocaite;n del certificado */
    public Date getRevocationDate() {
        return this.revocationDate;
    }

    /** Establece la fecha de revocaci&ocaite;n del certificado.
     * @param revocationDate Fecha de revocaci&ocaite;n del certificado */
    public void setRevocationDate(final Date revocationDate) {
        this.revocationDate = revocationDate;
    }

    /** Obtiene el motivo de la revocaci&ocaite;n del certificado.
     * @return Motivo de la revocaci&ocaite;n del certificado */
    public String getRevocationReason() {
        return this.revocationReason;
    }

    /** Establece el motivo de la revocaci&ocaite;n del certificado.
     * @param revocationReason Motivo de la revocaci&ocaite;n del certificado */
    public void setRevocationReason(final String revocationReason) {
        this.revocationReason = revocationReason;
    }
}
