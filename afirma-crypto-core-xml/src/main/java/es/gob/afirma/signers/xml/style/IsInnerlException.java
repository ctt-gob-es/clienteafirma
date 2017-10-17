/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xml.style;

/** Hoja de estilo local (rutal local no dereferenciable) a un XML */
public final class IsInnerlException extends StyleException {

    private static final long serialVersionUID = -8769490831203570286L;

    /** Construye la excepci&oacute;n que indica que una referencia apunta al interior del mismo XML.
     * @param e Excepci&oacute;n anterior en la cadena */
    public IsInnerlException(final Throwable e) {
        super(e);
    }

}
