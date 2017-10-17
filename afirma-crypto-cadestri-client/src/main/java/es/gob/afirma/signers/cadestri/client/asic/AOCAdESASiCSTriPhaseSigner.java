/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cadestri.client.asic;

import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cadestri.client.AOCAdESTriPhaseSigner;

/** Firmador CAdES-ASiC-S en tres fases.
 * @author Tom&acute;s Garc&iacute;a-Mer&aacute;s */
public final class AOCAdESASiCSTriPhaseSigner extends AOCAdESTriPhaseSigner {

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de firma. */
	private static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$

	@Override
	public byte[] sign(final byte[] data,
			           final String algorithm,
			           final PrivateKey key,
			           final Certificate[] certChain,
			           final Properties extraParams) throws AOException {
		return triPhaseOperation(
			AOSignConstants.SIGN_FORMAT_CADES_ASIC_S,
			CRYPTO_OPERATION_SIGN,
			data,
			algorithm,
			key,
			certChain,
			extraParams
		);
	}

	@Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) {
		throw new UnsupportedOperationException(
			"No se soporta cofirma trifasica CAdES-ASiC-S" //$NON-NLS-1$
		);
	}

	@Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) {
		throw new UnsupportedOperationException(
			"No se soporta cofirma trifasica CAdES-ASiC-S" //$NON-NLS-1$
		);
	}

	@Override
	public byte[] countersign(final byte[] sign,
			final String algorithm,
			final CounterSignTarget targetType,
			final Object[] targets,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties extraParams) {
		throw new UnsupportedOperationException(
			"No se soporta contrafirma trifasica CAdES-ASiC-S" //$NON-NLS-1$
		);
	}

	@Override
	public String getSignedName(final String originalName, final String inText) {
		return originalName + (inText != null ? inText : "") + ".asics"; //$NON-NLS-1$ //$NON-NLS-2$
	}
}