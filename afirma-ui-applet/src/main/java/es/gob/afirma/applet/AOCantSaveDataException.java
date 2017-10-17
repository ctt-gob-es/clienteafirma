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

/** Excepci&oacute;n para notificar que no se han podido guardar datos. */
final class AOCantSaveDataException extends AOException {

    private static final long serialVersionUID = -7566417065515232066L;

    /** Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n. */
    AOCantSaveDataException(final String msg) {
        super(msg);
    }

}
