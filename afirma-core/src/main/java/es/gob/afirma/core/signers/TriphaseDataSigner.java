/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;

/** Utilidad para la firma en una operaci&oacute;n trif&aacute;sica.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TriphaseDataSigner {

	/** Nombre de la propiedad para almac&eacute;n de prefirmas. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Nombre de la propiedad para almac&eacute;n de firmas PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Nombre de la propiedad  que indica si la postfirma requiere la prefirma. */
	private static final String PROPERTY_NAME_NEED_PRE = "NEED_PRE"; //$NON-NLS-1$

	private TriphaseDataSigner() {
		// No instanciable
	}

	/** Realiza la operaci&oacute;n de Firma (PKCS#1) sobre una sesi&oacute;n de firma
	 * trif&aacute;sica.
	 * @param signer Firmador PKCS#1.
	 * @param algorithm Algoritmo de firma.
	 * @param key Clave privada para la firma.
	 * @param certChain Cadena de certificados del firmante.
	 * @param triphaseData Datos de sesi&oacute;n trif&aacute;sica.
	 * @param extraParams Par&aacute;metros adicionales (aqu&iacute; solo aplican a la
	 *                    firma PKCS#1.
	 * @return Sesi&oacute;n trif&aacute;sica con las firmas PKCS#1 incluidas.
	 * @throws AOException Si ocurre cualquier error durante la firma. */
	public static TriphaseData doSign(final AOPkcs1Signer signer,
			                          final String algorithm,
			                          final PrivateKey key,
			                          final Certificate[] certChain,
			                          final TriphaseData triphaseData,
			                          final Properties extraParams) throws AOException {

		if (triphaseData.getSignsCount() < 1) {
			throw new AOException("No se han recibido prefirmas que firmar");  //$NON-NLS-1$
		}

		for (int i = 0; i < triphaseData.getSignsCount(); i++) {
			final TriphaseData.TriSign signConfig = triphaseData.getSign(i);
			final String base64PreSign = signConfig.getProperty(PROPERTY_NAME_PRESIGN);
			if (base64PreSign == null) {
				throw new AOException(
					"El servidor no ha devuelto la prefirma numero " + i //$NON-NLS-1$
				);
			}

			final byte[] preSign;
			try {
				preSign = Base64.decode(base64PreSign);
			}
			catch (final IOException e) {
				throw new AOException("Error decodificando la prefirma: " + e, e); //$NON-NLS-1$
			}

			final String signatureAlgorithm = AOSignConstants.composeSignatureAlgorithmName(algorithm, key.getAlgorithm());

			final byte[] pkcs1sign = signer.sign(
				preSign,
				signatureAlgorithm,
				key,
				certChain,
				extraParams // Parametros para PKCS#1
			);

			// Configuramos la peticion de postfirma indicando las firmas PKCS#1 generadas
			signConfig.addProperty(PROPERTY_NAME_PKCS1_SIGN, Base64.encode(pkcs1sign));

			// Si no es necesaria la prefirma para completar la postfirma, la eliminamos.
			// Cuando no se establece la propiedad "NEED_PRE" se supone que no es necesaria.
			if (signConfig.getProperty(PROPERTY_NAME_NEED_PRE) == null ||
					!Boolean.parseBoolean(signConfig.getProperty(PROPERTY_NAME_NEED_PRE))) {
				signConfig.deleteProperty(PROPERTY_NAME_PRESIGN);
			}

		}

		return triphaseData;
	}

}
