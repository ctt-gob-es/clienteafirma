/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */


package es.gob.afirma.applet;

import es.gob.afirma.core.AOException;

/** Excepci&oacute;n utilizada para indicar que se ha sobrepasado el
 * n&uacute;mero m&aacute;ximo de intentos de acceso a un recurso protegido. */
final class AOMaxAttemptsExceededException extends AOException {

    private static final long serialVersionUID = -1340950945486548407L;

    /** Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n. */
    AOMaxAttemptsExceededException(final String msg) {
        super(msg);
    }

    /** Crea la excepci&oacute;n con un mensaje determinado y una excepci&oacute;n origen.
     * @param msg Mensaje descriptivo de la excepci&oacute;n.
     * @param t Excepci&oacute;n origen */
    AOMaxAttemptsExceededException(final String msg, final Throwable t) {
        super(msg, t);
    }
}
