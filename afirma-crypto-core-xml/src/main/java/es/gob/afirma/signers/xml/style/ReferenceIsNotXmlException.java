/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xml.style;

/** La referencia de hoja de estilo apunta a un no-XML. */
public final class ReferenceIsNotXmlException extends StyleException {

    private static final long serialVersionUID = 8076672806350530425L;

    ReferenceIsNotXmlException(final Throwable e) {
        super(e);
    }
}
