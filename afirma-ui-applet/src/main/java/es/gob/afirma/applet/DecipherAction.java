/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.io.IOException;
import java.security.KeyException;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.AOException;

/** Acci&oacute;n privilegiada para el descifrado de datos. */
final class DecipherAction implements PrivilegedExceptionAction<Void> {

    /** Manejador de cifrado. */
    private final CipherManager cipherManager;

    /** Datos que se desean descifrar. */
    private final byte[] data;

    /** Construye la operaci&oacute;n de descifrado de datos. Si se indican
     * datos, se descifraran estos; si no se indican se tomar&aacute;n los
     * configurados en el manejador de cifrado.
     * @param cipherManager
     *        Manejador de cifrado de datos.
     * @param data
     *        Datos que se desean descifrar, {@code null} si se desean tomar
     *        los del manejador. */
    DecipherAction(final CipherManager cipherManager, final byte[] data) {

        if (cipherManager == null) {
            throw new IllegalArgumentException("El CipherManager no puede ser nulo"); //$NON-NLS-1$
        }

        this.cipherManager = cipherManager;
        this.data = data == null ? null : data.clone();
    }

    /** {@inheritDoc} */
    @Override
	public Void run() throws IOException, KeyException, AOException {
        if (this.data == null) {
            this.cipherManager.decipherData();
        }
        else {
            this.cipherManager.decipherData(this.data);
        }
        return null;
    }
}
