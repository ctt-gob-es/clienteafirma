/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.miniapplet;

import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivilegedExceptionAction;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSigner;

/**
 * Acci&oacute;n privilegiada de firma electr&oacute;nica.
 * @author Carlos Gamuci Mill&aacute;n.
 */
final class SignAction implements PrivilegedExceptionAction<byte[]> {

	private final AOSigner signer;
	private final byte[] data;
	private final String algorithm;
	private final PrivateKeyEntry keyEntry;
	private final Properties extraParams;

	/**
	 * Crea la acci&oacute;n para la firma de datos.
	 * @param signer Manejador de firma.
	 * @param data Datos que se desean firmar.
	 * @param algorithm Algoritmo de firma.
	 * @param keyEntry Clave privada de firma.
	 * @param extraParams Datos adicionales para la configuraci&oacute;n de la firma.
	 */
	SignAction(final AOSigner signer,
	                  final byte[] data,
	                  final String algorithm,
	                  final PrivateKeyEntry keyEntry,
	                  final Properties extraParams) {
		this.signer = signer;
		this.data = data != null ? data.clone() : null;
		this.algorithm = algorithm;
		this.keyEntry = keyEntry;
		this.extraParams = extraParams;
	}

	/** {@inheritDoc} */
	@Override
	public byte[] run() throws AOException, IOException {
		return this.signer.sign(
			this.data,
			this.algorithm,
			this.keyEntry.getPrivateKey(),
			this.keyEntry.getCertificateChain(),
			this.extraParams
		);
	}

}
