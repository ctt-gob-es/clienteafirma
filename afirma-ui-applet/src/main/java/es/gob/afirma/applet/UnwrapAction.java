/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.io.IOException;
import java.security.PrivilegedExceptionAction;
import java.security.cert.CertificateEncodingException;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.envelopers.cms.AOInvalidRecipientException;

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
    public byte[] run() throws IOException, AOException, CertificateEncodingException {

        try {
            this.enveloperManager.unwrap(this.envelop);
        }
        catch (final AOCancelledOperationException e) {
            throw e;
        }
        catch (final AOInvalidRecipientException e) {
            throw e;
        }
        catch (final AOInvalidFormatException e) {
            throw new AOInvalidFormatException("No se ha proporcionado un envoltorio soportado", e); //$NON-NLS-1$
        }
        catch (final IllegalArgumentException e) {
            throw new IllegalArgumentException("Modo de clave no soportado", e); //$NON-NLS-1$
        }
        catch (final IOException e) {
            throw new IOException("El envoltorio esta corrupto o no ha podido leerse"); //$NON-NLS-1$
        }
        catch (final AOException e) {
            throw e;
        }
        catch (final CertificateEncodingException e) {
            throw new CertificateEncodingException("El certificado del destinatario no es valido", e); //$NON-NLS-1$
        }
       return this.enveloperManager.getContentData();
    }

}
