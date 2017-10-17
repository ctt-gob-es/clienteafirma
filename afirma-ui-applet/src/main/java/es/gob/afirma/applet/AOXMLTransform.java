/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */


package es.gob.afirma.applet;

/** Transformada XML que se aplicar&aacute; a las firmas que las soporten. */
final class AOXMLTransform {

    /** Tipo de transformaci&oacute;n. */
    private String type = null;

    /** Subtipo de la transformaci&oacute;n. */
    private String subtype = null;

    /** Cuerpo de la transformaci&oacute;n. */
    private String body = null;

    /** Crea una transformaci&oacute;n XML.
     * @param type
     *        Tipo de transformaci&oacute;n.
     * @param subtype
     *        Subtipo de la transformaci&oacute;n.
     * @param body
     *        Cuerpo de la transformaci&oacute;n. */
    AOXMLTransform(final String type, final String subtype, final String body) {
        if (type == null) {
            throw new IllegalArgumentException("El tipo de una transformacion XML no puede ser nulo"); //$NON-NLS-1$
        }
        this.type = type;
        this.subtype = subtype;
        this.body = body;
    }

    /** Recupera el tipo de la transformaci&oacute;n.
     * @return Tipo de transformaci&oacute;n. */
    String getType() {
        return this.type;
    }

    /** Recupera el subtipo de la transformaci&oacute;n. Este elemento puede ser
     * nulo.
     * @return Subtipo de la transformaci&oacute;n. */
    String getSubtype() {
        return this.subtype;
    }

    /** Recupera el cuerpo de la transformaci&oacute;n.
     * @return Cuerpo de la transformaci&oacute;n. */
    String getBody() {
        return this.body;
    }
}
