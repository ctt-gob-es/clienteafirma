/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.triphase.server.cades;

import java.io.IOException;
import java.util.Date;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.multi.cades.AOCAdESCounterSigner;

/** Contrafirmador CAdES trif&aacute;sico.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class AOCAdESTriPhaseCounterSigner {

	private static final String PARAM_DATE = "DATE"; //$NON-NLS-1$

	private AOCAdESTriPhaseCounterSigner() {
		// No permitimos instanciar
	}

	/** Realiza la pre-contrafirma trif&aacute;sica.
	 * B&aacute;sicamente, se realiza una contrafirma completa usando un firmador PKCS#1 <i>falso</i>,
	 * que almacena los octetos a firmar e introduce en vez de las firmas,
     * @param sign Flujo de lectura de los datos a firmar.
     * @param algorithm Algoritmo a usar para la firma (SHA1withRSA, SHA512withRSA, etc.).
     * @param targetType Tipo de objetivo de la contrafirma.
     * @param targets Informaci&oacute;n complementaria seg&uacute;n el tipo de objetivo de la contrafirma.
     * @param cChain Cadena de certificados del firmante.
     * @param xParams Par&aacute;metros adicionales para la contrafirma.
	 * @param date Fecha de la contrafirma.
     * @return Prefirma en formato XML.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma. */
	public static byte[] preCountersign(final byte[] sign,
                                        final String algorithm,
                                        final CounterSignTarget targetType,
                                        final Object[] targets,
                                        final java.security.cert.Certificate[] cChain,
                                        final Properties xParams,
                                        final Date date) throws AOException,
                                                                IOException {
		final TriphaseData triphaseData = new TriphaseData();

		final AOCAdESCounterSigner countersigner = new AOCAdESCounterSigner(
			new CAdESFakePkcs1Signer(triphaseData, null, true),
			date
		);

		// No queremos la contrafirma sino los PKCS#1 generados y almacenados internamente en
		// la siguiente operacion
		countersigner.countersign(
			sign,
			algorithm,
			targetType,
			targets,
			null,
			cChain,
			xParams
		);

		for (int i = 0; i < triphaseData.getSignsCount(); i++) {
			triphaseData.getSign(i).addProperty(PARAM_DATE, Long.toString(date.getTime()));
		}

		return triphaseData.toString().getBytes();
    }
}
