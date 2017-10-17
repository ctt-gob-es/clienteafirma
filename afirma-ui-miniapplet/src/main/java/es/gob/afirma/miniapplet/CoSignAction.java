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
 * Acci&oacute;n privilegiada para realizar una cofirma electr&oacute;nica.
 * @author Carlos Gamuci Mill&aacute;n.
 */
final class CoSignAction implements PrivilegedExceptionAction<byte[]> {

	private final AOSigner signer;
	private final byte[] sign;
	private final byte[] data;
	private final String algorithm;
	private final PrivateKeyEntry keyEntry;
	private final Properties extraParams;

	/**
	 * Crea la acci&oacute;n para cofirmar de una firma electr&oacute;nica.
	 * @param signer Manejador de firma.
	 * @param sign Firma que se desea cofirmar.
	 * @param data Datos que se firmaron originalmente.
	 * @param algorithm Algoritmo de firma.
	 * @param keyEntry Clave privada de firma.
	 * @param extraParams Datos adicionales para la configuraci&oacute;n de la contrafirma.
	 */
	CoSignAction(final AOSigner signer,
	                    final byte[] sign,
	                    final byte[] data,
	                    final String algorithm,
	                    final PrivateKeyEntry keyEntry,
	                    final Properties extraParams) {
		this.signer = signer;
		this.sign = sign != null ? sign.clone() : null;
		this.data = data != null ? data.clone() : null;
		this.algorithm = algorithm;
		this.keyEntry = keyEntry;
		this.extraParams = extraParams;
	}

	/** {@inheritDoc}
	 * @throws IOException Cuando se produce un error durante la lectura de los datos. */
	@Override
	public byte[] run() throws AOException, IOException {
		if (this.data == null) {
			return this.signer.cosign(
				this.sign,
				this.algorithm,
				this.keyEntry.getPrivateKey(),
				this.keyEntry.getCertificateChain(),
				this.extraParams
			);
		}
		return this.signer.cosign(
			this.data,
			this.sign,
			this.algorithm,
			this.keyEntry.getPrivateKey(),
			this.keyEntry.getCertificateChain(),
			this.extraParams
		);
	}
}
