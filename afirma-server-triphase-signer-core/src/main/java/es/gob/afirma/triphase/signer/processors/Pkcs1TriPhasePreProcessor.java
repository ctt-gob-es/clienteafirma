/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.signer.processors;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;

/** Procesador de firmas trif&aacute;sicas PKCS#1.
 * @author Tom&aacute;s Garc&iacute;a Mer&aacute;s. */
public final class Pkcs1TriPhasePreProcessor implements TriPhasePreProcessor {

	/** Prefijo para cada prefirma. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Etiqueta de firma PKCS#1 en el XML de sesi&oacute;n trif&aacute;sica. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Manejador de log. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	@Override
	public TriphaseData preProcessPreSign(final byte[] data,
			                              final String algorithm,
			                              final X509Certificate[] cert,
			                              final Properties params,
			                              final boolean checkSignatures) throws IOException,
	                                                                      AOException {
		LOGGER.info("Prefirma PKCS#1 - Firma - INICIO"); //$NON-NLS-1$

		if (data == null || data.length < 1) {
			throw new IllegalArgumentException("Los datos no pueden ser nulos"); //$NON-NLS-1$
		}

		// Generamos el mensaje para la configuracion de la operacion
		final TriphaseData triphaseData = new TriphaseData();

		final Map<String, String> signConfig = new ConcurrentHashMap<>();
		signConfig.put(PROPERTY_NAME_PRESIGN, Base64.encode(data));

		triphaseData.addSignOperation(
			new TriSign(
				signConfig,
				TriPhaseUtil.getSignatureId(params)
			)
		);

		LOGGER.info("Prefirma PKCS#1 - Firma - FIN"); //$NON-NLS-1$

		return triphaseData;
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			                         final String algorithm,
			                         final X509Certificate[] cert,
			                         final Properties extraParams,
			                         final byte[] session) throws NoSuchAlgorithmException,
			                                                                AOException,
			                                                                IOException {
		if (session == null) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos"); //$NON-NLS-1$
		}

		return preProcessPostSign(data, algorithm, cert, extraParams, TriphaseData.parser(session));
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			                         final String algorithm,
			                         final X509Certificate[] cert,
			                         final Properties params,
			                         final TriphaseData triphaseData) throws NoSuchAlgorithmException,
			                                                                 IOException,
			                                                                 AOException {
		LOGGER.info("Postfirma PKCS#1 - Firma - INICIO"); //$NON-NLS-1$

		if (triphaseData == null) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos"); //$NON-NLS-1$
		}

		// Cargamos la configuracion de la operacion
		if (triphaseData.getSignsCount() < 1) {
			LOGGER.severe("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
			throw new AOException("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
		}

		final TriSign config = triphaseData.getSign(0);

		final byte[] signature = Base64.decode(config.getProperty(PROPERTY_NAME_PKCS1_SIGN));

		LOGGER.info("Postfirma PKCS#1 - Firma - FIN"); //$NON-NLS-1$

		return signature;
	}

	@Override
	public TriphaseData preProcessPreCoSign(final byte[] sign,
			final String algorithm,
			final X509Certificate[] cert,
			final Properties params,
            final boolean checkSignatures) throws IOException, AOException {

		throw new UnsupportedOperationException("No se soporta la multifirma de firmas NONE"); //$NON-NLS-1$
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] sign,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final byte[] session) throws NoSuchAlgorithmException, AOException, IOException {

		throw new UnsupportedOperationException("No se soporta la multifirma de firmas NONE"); //$NON-NLS-1$
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] sign,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final TriphaseData triphaseData) throws NoSuchAlgorithmException, AOException, IOException {

		throw new UnsupportedOperationException("No se soporta la multifirma de firmas NONE"); //$NON-NLS-1$
	}

	@Override
	public TriphaseData preProcessPreCounterSign(final byte[] sign,
			                               final String algorithm,
			                               final X509Certificate[] cert,
			                               final Properties extraParams,
			                               final CounterSignTarget targetType,
				                           final boolean checkSignatures) throws IOException,
			                                                                          AOException {
		throw new UnsupportedOperationException("No se soporta la multifirma de firmas NONE"); //$NON-NLS-1$
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			                                final String algorithm,
			                                final X509Certificate[] cert,
			                                final Properties extraParams,
			                                final byte[] session,
			                                final CounterSignTarget targetType) throws NoSuchAlgorithmException,
			                                                                           AOException,
			                                                                           IOException {
		throw new UnsupportedOperationException("No se soporta la multifirma de firmas NONE"); //$NON-NLS-1$
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			                                final String algorithm,
			                                final X509Certificate[] cert,
			                                final Properties extraParams,
			                                final TriphaseData triphaseData,
			                                final CounterSignTarget targetType) throws NoSuchAlgorithmException,
			                                                                           AOException,
			                                                                           IOException {
		throw new UnsupportedOperationException("No se soporta la multifirma de firmas NONE"); //$NON-NLS-1$
	}
}
