/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.multi.cades.asic;

import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Map;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOCoSigner;
import es.gob.afirma.core.signers.asic.ASiCUtil;
import es.gob.afirma.signers.multi.cades.AOCAdESCoSigner;

/** Operaciones de cofirma CAdES ASiC-S.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AOCAdESASiCSCoSigner implements AOCoSigner {

	/** {@inheritDoc} */
	@Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) throws AOException, IOException {
		return cosign(sign, algorithm, key, certChain, extraParams);
	}

	/** {@inheritDoc} */
	@Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) throws AOException,
			                                                  IOException {
		// Extraemos firma y datos del ASiC
		byte[] packagedSign;
		try {
			packagedSign = ASiCUtil.getASiCSBinarySignature(sign);
		} catch (final IOException e) {
			throw new AOException("No se encontro la firma CAdES dentro del contenedor ASiC", e); //$NON-NLS-1$
		}

		// Utilizarmos solo los primeros datos que encontremos
		String signedDataName;
		byte[] packagedData;
		try {
			final Map<String, byte[]> signedData = ASiCUtil.getASiCSData(sign);
			signedDataName = signedData.keySet().iterator().next();
			packagedData = signedData.get(signedDataName);
		} catch (final IOException e) {
			throw new AOException("No se encontraron los datos firmados dentro del contenedor CAdES-ASiC", e); //$NON-NLS-1$
		}

		// Creamos la cofirma
		final byte[] newSign = new AOCAdESCoSigner().cosign(packagedData, packagedSign, algorithm, key, certChain, extraParams);

		// Devolvemos un nuevo ASiC
		return ASiCUtil.createSContainer(
			newSign,
			packagedData,
			signedDataName,
			ASiCUtil.ENTRY_NAME_BINARY_SIGNATURE
		);
	}

}
