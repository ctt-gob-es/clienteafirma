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
import java.security.cert.CertificateEncodingException;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;

/** Acci&oacute;n privilegiada para el ensobrado de datos. La ejecuci&oacute;n de
 * la acci&oacute;n devuelve {@code true} o {@code false} y el resultado
 * almacenado es un array de bytes. */
final class WrapAction implements PrivilegedExceptionAction<byte[]> {

    /** Manejador de ensobrado. */
    private final EnveloperManager enveloperManager;

    /** Datos que se desean ensobrar. */
    private final byte[] data;

    /** Construye la operaci&oacute;n de ensobrado de datos. Si se indican datos,
     * se ensobraran estos; si no se indican se tomar&aacute;n los configurados
     * en el manejador de ensobrado.
     * @param enveloperManager
     *        Manejador de ensobrado de datos.
     * @param data
     *        Datos que se desean ensobrar, {@code null} si se desean tomar
     *        los del manejador. */
    WrapAction(final EnveloperManager enveloperManager, final byte[] data) {

        if (enveloperManager == null) {
            throw new IllegalArgumentException("El EnveloperManager no puede ser nulo"); //$NON-NLS-1$
        }

        this.enveloperManager = enveloperManager;
        this.data = data != null ? data.clone() : null;
    }

    /** {@inheritDoc} */
    public byte[] run() throws NoSuchAlgorithmException,
                               IOException,
                               AOException,
                               CertificateEncodingException,
                               KeyException {
    	try {
    		if (this.enveloperManager.getCmsContentType().equals(AOSignConstants.CMS_CONTENTTYPE_ENCRYPTEDDATA)) {
    			if (this.data == null) {
    				this.enveloperManager.encrypt();
    			}
    			else {
    				this.enveloperManager.encrypt(this.data);
    			}
    		}
    		else {
    			if (this.data == null) {
    				this.enveloperManager.envelop();
    			}
    			else {
    				this.enveloperManager.envelop(this.data);
    			}
    		}
    		return this.enveloperManager.getEnvelopedData();
    	}
    	catch (final AOCancelledOperationException e) {
    		throw e;
    	}
    	catch (final IllegalArgumentException e) {
    		throw new IllegalArgumentException("Modo de clave no soportado", e); //$NON-NLS-1$
    	}
    	catch (final NullPointerException e) {
    		throw new IllegalArgumentException("No se ha indicado el tipo de envoltorio o los destinatarios del mismo", e); //$NON-NLS-1$
    	}
    	catch (final NoSuchAlgorithmException e) {
    		throw new NoSuchAlgorithmException("Algoritmo de ensobrado no soportado", e); //$NON-NLS-1$
    	}
    	catch (final IOException e) {
    		throw new IOException("No se han podido leer los datos a ensobrar"); //$NON-NLS-1$
    	}
    	catch (final CertificateEncodingException e) {
    		throw new CertificateEncodingException("El certificado del remitente no es valido", e); //$NON-NLS-1$
    	}
    	catch (final KeyException e) {
    		throw new KeyException("La clave de envoltura generada no es valida", e); //$NON-NLS-1$
    	}
    }

}
