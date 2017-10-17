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

/** Acci&oacute;n privilegiada para agregar un nuevo remitentes a un sobre
 * electr&oacute;nico. La ejecuci&oacute;n de la acci&oacute;n devuelve {@code true} o {@code false} y el resultado almacenado es un array de bytes. */
final class CoEnvelopAction implements PrivilegedExceptionAction<byte[]> {

    /** Manejador de ensobrado. */
    private final EnveloperManager enveloperManager;

    /** Envoltorio que se desea desensobrar. */
    private final byte[] envelop;

    /** Envoltorio de m&uacute;ltiples remitentes.
     * @param enveloperManager
     *        Gestor de envoltorios
     * @param envelop
     *        Envoltorio */
    CoEnvelopAction(final EnveloperManager enveloperManager, final byte[] envelop) {
        if (enveloperManager == null) {
            throw new IllegalArgumentException("El EnverloperManager no puede ser nulo"); //$NON-NLS-1$
        }

        this.enveloperManager = enveloperManager;
        this.envelop = envelop.clone();
    }

    /** {@inheritDoc}
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura de los datos.
     * @throws CertificateEncodingException Si hay alg&uacute;n certificado inv&aacute;lido */
    @Override
	public byte[] run() throws AOException, IOException, CertificateEncodingException {
        this.enveloperManager.coEnvelop(this.envelop);
        return this.enveloperManager.getEnvelopedData();
    }

}
