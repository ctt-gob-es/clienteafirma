/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xml.style;

/** No se puede dereferenciar la hoja de estilo. */
public final class CannotDereferenceException extends StyleException {

    private static final long serialVersionUID = 5883820163272098664L;

    /** Construye una excepci&oacute;n que indica la imposibilidad de
     * dereferenciar una hoja de estilo.
     * @param s
     *        Mesaje de excepci&oacute;n */
    CannotDereferenceException(final String s) {
        super(s);
    }

    /** Construye una excepci&oacute;n que indica la imposibilidad de
     * dereferenciar una hoja de estilo.
     * @param s
     *        Mesaje de excepci&oacute;n
     * @param e
     *        Excepci&oacute;n anterior en la cadena */
    CannotDereferenceException(final String s, final Exception e) {
        super(s, e);
    }
}
