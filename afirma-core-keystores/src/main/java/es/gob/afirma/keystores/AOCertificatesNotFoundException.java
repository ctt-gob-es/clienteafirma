/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import es.gob.afirma.core.AOException;

/** Excepci&oacute;n para notificar que no se han encontrado certificados de
 * usuario en un almac&eacute;n de certificados. */
public final class AOCertificatesNotFoundException extends AOException {

    private static final long serialVersionUID = -6996346324337434742L;

    /** Crea la excepci&oacute;n con el mensaje por defecto. */
    public AOCertificatesNotFoundException() {
        super("No hay certificados en el almacen"); //$NON-NLS-1$
    }

    /** Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n. */
    public AOCertificatesNotFoundException(final String msg) {
        super(msg);
    }

    /** Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n.
     * @param cause
     * 		  Motivo que origin&oacute; el error. */
    public AOCertificatesNotFoundException(final String msg, final Throwable cause) {
        super(msg, cause);
    }
}
