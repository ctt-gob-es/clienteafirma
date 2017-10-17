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

/** Excepci&oacute;n para notificar un error en la inicializacion o en el acceso
 * a un almac&eacute;n de certificados. */
public final class AOKeyStoreManagerException extends AOException {

    private static final long serialVersionUID = 2896862509190263027L;

    /** Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg Mensaje descriptivo de la excepci&oacute;n. */
    public AOKeyStoreManagerException(final String msg) {
        super(msg);
    }

    /** Crea la excepci&oacute;n con un mensaje determinado y una excepci&oacute;n de origen.
     * @param msg Mensaje descriptivo de la excepci&oacute;n.
     * @param e Excepci&oacute;n que ha causado el lanzamiento de esta. */
    public AOKeyStoreManagerException(final String msg, final Exception e) {
        super(msg, e);
    }

    /** Crea la excepci&oacute;n con una excepci&oacute;n de origen.
     * @param e Excepci&oacute;n que ha causado el lanzamiento de esta. */
    public AOKeyStoreManagerException(final Exception e) {
    	super(e);
    }

}
