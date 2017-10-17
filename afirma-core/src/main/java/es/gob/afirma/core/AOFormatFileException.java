/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core;

/** Excepci&oacute;n para notificar que se ha proporcionado un fichero o dato con
 * un formato no v&aacute;lido para la acci&oacute;n en curso. */
public class AOFormatFileException extends AOException {

    private static final long serialVersionUID = 6785819338728771962L;

    /** Crea una excepci&oacute;n relacionada con un formato de fichero con un mensaje determinado.
     * @param msg Mensaje descriptivo de la excepci&oacute;n. */
    public AOFormatFileException(final String msg) {
        super(msg);
    }

    /** Crea una excepci&oacute;n relacionada con un formato de fichero con un mensaje determinado
     * y preservando la pila de exceptiones.
     * @param msg Mensaje descriptivo de la excepci&oacute;n.
     * @param e Excepci&oacute;n que a su vez origin&oacute; esta
     */
    public AOFormatFileException(final String msg, final Throwable e) {
        super(msg, e);
    }

}
