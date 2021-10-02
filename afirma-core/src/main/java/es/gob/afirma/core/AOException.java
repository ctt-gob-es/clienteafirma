/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core;

/** Excepci&oacute;n gen&eacute;rica.
 * @version 1.0 */
public class AOException extends Exception {

    private static final long serialVersionUID = -662191654860389176L;

    /** Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
     * @param msg Mensaje de la excepci&oacute;n */
    public AOException(final String msg) {
        super(msg);
    }

    /** Contruye una excepci&oacute;n gen&eacute;rica con mensaje y define su causa.
     * @param msg Descripci&oacute;n del error.
     * @param cause Causa del error. */
    public AOException(final String msg, final Throwable cause) {
        super(msg, cause);
    }

    /** Contruye una excepci&oacute;n gen&eacute;rica definiendo su causa.
     * @param cause Causa del error. */
    public AOException(final Throwable cause) {
        super(cause);
    }
}