/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.ooxml;

/** Relaci&oacute;n XML seg&uacute;n la normativa OOXML. */
final class Relationship {

    private String id = null;
    private String type = null;
    private String target = null;

    /** Construye un objeto de relaci&oacute;n OOXML.
     * @param id
     *        Identificador de la relaci&oacute;n
     * @param type
     *        Typo de la relaci&oacute;n
     * @param target
     *        Destino de la relaci&oacute;n (objeto relacionado) */
    Relationship(final String id, final String type, final String target) {
        this.id = id;
        this.type = type;
        this.target = target;
    }

    /** Obtiene el identificador de la relaci&oacute;n.
     * @return Identificador de la relaci&oacute;n */
    String getId() {
        return this.id;
    }

    /** Obtiene el tipo de la relaci&oacute;n.
     * @return Tipo de la relaci&oacute;n */
    String getType() {
        return this.type;
    }

    /** Obtiene el destino de la relaci&oacute;n (el objeto relacionado)
     * @return Destino de la relaci&oacute;n (objeto relacionado) */
    String getTarget() {
        return this.target;
    }

    @Override
    public String toString() {
        return "<Relationship Id=\"" + this.id + "\" Type=\"" + this.type + "\" Target=\"" + this.target + "\"/>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    }
}
