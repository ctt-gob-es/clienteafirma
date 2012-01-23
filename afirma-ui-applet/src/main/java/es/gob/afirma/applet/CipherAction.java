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
import java.security.KeyException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;

/** Acci&oacute;n privilegiada para el cifrado de datos. */
public final class CipherAction implements PrivilegedExceptionAction<Void> {

    /** Manejador de cifrado. */
    private final CipherManager cipherManager;

    /** Datos que se desean cifrar. */
    private final byte[] data;

    /** Construye la operaci&oacute;n de cifrado de datos. Si se indican datos,
     * se cifraran estos; si no se indican se tomar&aacute;n los configurados en
     * el manejador de cifrado.
     * @param cipherManager
     *        Manejador de cifrado de datos.
     * @param data
     *        Datos que se desean cifrar, {@code null} si se desean tomar
     *        los del manejador. */
    public CipherAction(final CipherManager cipherManager, final byte[] data) {
        if (cipherManager == null) {
            throw new IllegalArgumentException("El CipherManager no puede ser nulo"); //$NON-NLS-1$
        }
        this.cipherManager = cipherManager;
        this.data = data != null ? data.clone() : null;
    }

    /** {@inheritDoc} */
    public Void run() throws NoSuchAlgorithmException, KeyException, IOException, AOException {

        try {
            if (this.data == null) {
                this.cipherManager.cipherData();
            }
            else {
                this.cipherManager.cipherData(this.data);
            }
        }
        catch (final AOCancelledOperationException e) {
            throw e;
        }
        catch (final IllegalArgumentException e) {
            throw new IllegalArgumentException("Modo de clave no soportado", e); //$NON-NLS-1$
        }
        catch (final NoSuchAlgorithmException e) {
        	throw new NoSuchAlgorithmException("Algoritmo de cifrado no soportado", e); //$NON-NLS-1$
        }
        catch (final KeyException e) {
            throw new KeyException("Clave de cifrado no valida", e); //$NON-NLS-1$
        }
        catch (final IOException e) {
            throw new IOException("No se han podido leer los datos a cifrar"); //$NON-NLS-1$
        }
        catch (final AOException e) {
           throw e;
        }

        return null;
    }
}
