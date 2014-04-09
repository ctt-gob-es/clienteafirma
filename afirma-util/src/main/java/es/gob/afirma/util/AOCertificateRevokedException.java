/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.util;

import java.util.Date;

/** Excepci&oacute;n lanzada cuando se detecta que un certificado ha sido revocado.
 * @author Carlos Gamuci */
public class AOCertificateRevokedException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = -4433892673404934489L;

    /** Fecha de revocaci&oacute;n. */
    private Date revocationDate = null;

    /** Motivo de la revocaci&oacute;n. */
    private String revocationReason = null;

    /** Crea una excepci&oacute;n para la notificaci&oacute;n de que se ha detectado que
     * el certificado que se valid&oacute; estaba revocado.
     * @param message Mensaje de error.
     * @param cause Excepci&oacute;n que causo el error. */
    public AOCertificateRevokedException(final String message, final Throwable cause) {
        super(message, cause);
    }

    /** Obtiene la fecha de revocaci&oacute;n del certificado.
     * @return Fecha de revocaci&oacute;n del certificado */
    public Date getRevocationDate() {
        return this.revocationDate;
    }

    /** Establece la fecha de revocaci&oacute;n del certificado.
     * @param revocationDate Fecha de revocaci&oacute;n del certificado */
    public void setRevocationDate(final Date revocationDate) {
        this.revocationDate = revocationDate;
    }

    /** Obtiene el motivo de la revocaci&oacute;n del certificado.
     * @return Motivo de la revocaci&oacute;n del certificado */
    public String getRevocationReason() {
        return this.revocationReason;
    }

    /** Establece el motivo de la revocaci&oacute;n del certificado.
     * @param revocationReason Motivo de la revocaci&oacute;n del certificado */
    public void setRevocationReason(final String revocationReason) {
        this.revocationReason = revocationReason;
    }
}
