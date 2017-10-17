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
import java.security.InvalidAlgorithmParameterException;
import java.security.KeyException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivilegedExceptionAction;
import java.security.SignatureException;
import java.security.cert.CertificateEncodingException;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;

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

    /** {@inheritDoc}
     * @throws SignatureException Si hay problemas con la firma PKCS#1.
     * @throws BadPaddingException Si hay problemas con los rellenos criptogr&aacute;ficos.
     * @throws IllegalBlockSizeException Si hay tama&ntilde;os de bloque no v&aacute;lidos.
     * @throws InvalidAlgorithmParameterException Si hay problemas de configuraci&oacute;n de los algoritmos.
     * @throws NoSuchPaddingException Si hay problemas con los rellenos criptogr&aacute;ficos. */
    @Override
	public byte[] run() throws NoSuchAlgorithmException,
                               IOException,
                               AOException,
                               CertificateEncodingException,
                               KeyException, NoSuchPaddingException, InvalidAlgorithmParameterException, IllegalBlockSizeException, BadPaddingException, SignatureException {
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

}
