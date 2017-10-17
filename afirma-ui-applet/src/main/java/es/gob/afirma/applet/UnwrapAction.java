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
import java.security.PrivilegedExceptionAction;
import java.security.cert.CertificateEncodingException;

import es.gob.afirma.core.AOException;

/** Acci&oacute;n privilegiada para el desensobrado de datos. La ejecuci&oacute;n
 * de la acci&oacute;n devuelve {@code true} o {@code false} y el resultado
 * almacenado es un array de bytes. */
final class UnwrapAction implements PrivilegedExceptionAction<byte[]> {

    /** Manejador de ensobrado. */
    private final EnveloperManager enveloperManager;

    /** Envoltorio que se desea desensobrar. */
    private final byte[] envelop;

    /** Construye la operaci&oacute;n de desensobrado de datos. Si se indica un
     * sobre, se ensobrara este; si no se indica se tomar&aacute; el configurado
     * en el manejador de ensobrado.
     * @param enveloperManager
     *        Manejador de ensobrado de datos.
     * @param envelop
     *        Sobre que se desea desensobrar, {@code null} si se desean
     *        tomar los del manejador. */
    UnwrapAction(final EnveloperManager enveloperManager, final byte[] envelop) {

        if (enveloperManager == null) {
            throw new IllegalArgumentException("El EnveloperManager no puede ser nulo"); //$NON-NLS-1$
        }

        this.enveloperManager = enveloperManager;
        this.envelop = envelop != null ? envelop.clone() : null;
    }

    /** {@inheritDoc} */
    @Override
	public byte[] run() throws IOException, AOException, CertificateEncodingException {
        this.enveloperManager.unwrap(this.envelop);
        return this.enveloperManager.getContentData();
    }

}
