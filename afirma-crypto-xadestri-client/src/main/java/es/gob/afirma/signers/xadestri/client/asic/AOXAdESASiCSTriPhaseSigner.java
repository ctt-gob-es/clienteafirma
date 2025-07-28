/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xadestri.client.asic;

import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.asic.ASiCUtil;
import es.gob.afirma.signers.xadestri.client.AOXAdESTriPhaseSigner;

/** Manejador de firma XAdES-ASiC-S trif&aacute;sicas.
 * Mediante este manejador un usuario puede firmar un documento remoto
 * indicando el identificador del documento. Este manejador requiere de un servicio remoto que genere la estructura
 * de firma en servidor. La operaci&oacute;n criptogr&aacute;fica de firma se realiza en el PC o dispositivo del usuario,
 * por lo que la clave privada de su certificado nunca sale de este.<br>
 * El resultado de las operaciones criptogr&aacute;ficas no es el resultado generado sino el identificador con el que
 * el resultado se ha guardado en el servidor remoto (gestor documental, sistema de ficheros,...).
 * La l&oacute;gica de resoluci&oacute;n del identificador de entrada, recuperaci&oacute;n de los datos y el guardado
 * del resultado recae el un manejador conectado al servicio de firma.
 * Como alternativa, a indicar los datos mediante un identificador, un usuario puede introducir directamente los datos
 * (prevaleciendo estos sobre el identificador) de tal forma que estos viajan en cada una de las operaciones con el
 * servidor. El resultado ser&aacute; an&aacute;logo al anterior, recuperandose &uacute;nicamente el identificador
 * remoto asignado al resultado. */
public final class AOXAdESASiCSTriPhaseSigner extends AOXAdESTriPhaseSigner {

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de firma. */
	private static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$
	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de cofirma. */
	private static final String CRYPTO_OPERATION_COSIGN = "cosign"; //$NON-NLS-1$

	@Override
	public byte[] sign(final byte[] data,
			           final String algorithm,
			           final PrivateKey key,
			           final Certificate[] certChain,
			           final Properties xParams) throws AOException {
		return triPhaseOperation(
			AOSignConstants.SIGN_FORMAT_XADES_ASIC_S,
			CRYPTO_OPERATION_SIGN,
			data,
			algorithm,
			key,
			certChain,
			xParams
		);
	}

	@Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties xParams) throws AOException {
		return triPhaseOperation(
			AOSignConstants.SIGN_FORMAT_XADES_ASIC_S,
			CRYPTO_OPERATION_COSIGN,
			sign,
			algorithm,
			key,
			certChain,
			xParams
		);
	}

	@Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties xParams) throws AOException {
		return cosign(null, sign, algorithm, key, certChain, xParams);
	}

	@Override
	public byte[] countersign(final byte[] sign,
			                  final String algorithm,
			                  final CounterSignTarget targetType,
			                  final Object[] targets,
			                  final PrivateKey key,
			                  final Certificate[] certChain,
			                  final Properties xParams) {

		throw new UnsupportedOperationException("No se soportan contrafirmas trifasicas XAdES-ASiC-S"); //$NON-NLS-1$
	}

	/** {@inheritDoc} */
	@Override
	public boolean isSign(final byte[] asic) {
		final byte[] sign;
		try {
			sign = ASiCUtil.getASiCSXMLSignature(asic);
		}
		catch(final Exception e) {
			LOGGER.info("La firma proporcionada no es XAdES ASiC-S: " + e); //$NON-NLS-1$
			return false;
		}
		try {
			return super.isSign(sign);
		}
		catch (final Exception e) {
			LOGGER.warning("Error extrayendo la firma del contenedor ASiC: " + e); //$NON-NLS-1$
			return false;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getSignedName(final String originalName, final String inText) {
		return originalName + (inText != null ? inText : "") + ".asics"; //$NON-NLS-1$ //$NON-NLS-2$
	}

}
