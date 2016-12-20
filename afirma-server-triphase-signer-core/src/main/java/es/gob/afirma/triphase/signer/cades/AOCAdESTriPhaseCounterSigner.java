/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.triphase.signer.cades;

import java.io.IOException;
import java.util.Arrays;
import java.util.Date;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;
import es.gob.afirma.signers.multi.cades.AOCAdESCounterSigner;
import es.gob.afirma.triphase.signer.processors.TriPhaseUtil;

/** Contrafirmador CAdES trif&aacute;sico.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class AOCAdESTriPhaseCounterSigner {

	private static final String PARAM_DATE = "DATE"; //$NON-NLS-1$

	/** Etiqueta de firma PKCS#1 en el XML de sesi&oacute;n trif&aacute;sica. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Etiqueta de firma PKCS#1 temporal en el XML de sesi&oacute;n trif&aacute;sica. */
	private static final String PROPERTY_NAME_DUMMY_PK1 = "DPK1"; //$NON-NLS-1$

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
	public static TriphaseData preCountersign(final byte[] sign,
                                        final String algorithm,
                                        final CounterSignTarget targetType,
                                        final Object[] targets,
                                        final java.security.cert.Certificate[] cChain,
                                        final Properties xParams,
                                        final Date date) throws AOException,
                                                                IOException {
		final TriphaseData triphaseData = new TriphaseData();

		// Recuperamos el identificador asociado a la firma u obtenemos uno si no lo tenia
		final String signatureId = TriPhaseUtil.getSignatureId(xParams);

		final AOCAdESCounterSigner countersigner = new AOCAdESCounterSigner(
			new CAdESFakePkcs1Signer(triphaseData, signatureId, true),
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

		return triphaseData;
    }

	/** Realiza la post-contrafirma trif&aacute;sica.
	 * @param sign Flujo de lectura de los datos a firmar.
     * @param algorithm Algoritmo a usar para la firma (SHA1withRSA, SHA512withRSA, etc.).
     * @param targetType Tipo de objetivo de la contrafirma.
     * @param targets Informaci&oacute;n complementaria seg&uacute;n el tipo de objetivo de la contrafirma.
     * @param cChain Cadena de certificados del firmante.
     * @param xParams Par&aacute;metros adicionales para la contrafirma.
	 * @param triphaseData Estado intermedio de las firmas.
     * @return Prefirma en formato XML.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma. */
	public static byte[] postCountersign(final byte[] sign,
                                        final String algorithm,
                                        final CounterSignTarget targetType,
                                        final Object[] targets,
                                        final java.security.cert.Certificate[] cChain,
                                        final Properties xParams,
                                        final TriphaseData triphaseData) throws AOException,
                                                                IOException {

		if (triphaseData == null || triphaseData.getSignsCount() == 0) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos y deben contener firmas"); //$NON-NLS-1$
		}

		final Date date = new Date(Long.parseLong(triphaseData.getSign(0).getProperty(PARAM_DATE)));

		byte[] newSign = new AOCAdESCounterSigner(
			new CAdESFakePkcs1Signer(
				triphaseData,
				null,
				false
			),
			date
		).countersign(
			sign,
			algorithm,
			targetType,
			targets,
			null,
			cChain,
			xParams
		);

		// En esta contrafirma las firmas PKCS#1 son falsas, asi que vamos buscando los
		// valores reales para insertarlo, teniendo en cuenta que en esta contrafirma y
		// en la sesion se comparte el valor de los datos que se firman con PKCS#1
		// Los datos dummy son iguales en la prefirma y la postfirma porque se generan
		// en base al hash de la prefirma firmada.
		for (int i = 0; i < triphaseData.getSignsCount(); i++) {

			final TriSign signConfig = triphaseData.getSign(i);

			// Los datos que hay que sustituir en la firma recien creada
			final byte[] dataToReplace = Base64.decode(signConfig.getProperty(PROPERTY_NAME_DUMMY_PK1));

			// La firma real PKCS#1
			final byte[] pkcs1Sign = Base64.decode(signConfig.getProperty(PROPERTY_NAME_PKCS1_SIGN));

			// Reemplazamos
			newSign = searchAndReplace(newSign, dataToReplace, pkcs1Sign);
		}

		return newSign;
    }

	/** Reemplaza un subarray por otro del mismo tama&ntilde;o dentro un un array contenedor.
	 * @param source Array contenedor en el que se realiza la b&uacute;squeda.
	 * @param search SubArray que hay que sustituir.
	 * @param replace SubArray por el que se sustituye.
	 * @return Array contenedor con el reemplazo hecho.	 */
	private static byte[] searchAndReplace(final byte[] source, final byte[] search, final byte[] replace) {
		if (search.length != replace.length) {
			return source;
		}
		int p = searchFor(source, search);
		if (p == -1) {
			throw new IllegalArgumentException("No se ha encontrado la cadena a sustituir"); //$NON-NLS-1$
		}
		final byte[] result = Arrays.copyOf(source, source.length);
		for (final byte element : replace) {
			result[p] = element;
			p++;
		}
		return result;
	}

	/** Busca un subarray dentro de otro array.
	 * @param array Array sobre el que se realiza la b&uacute;squeda.
	 * @param subArray SubArray que buscamos.
	 * @return Posici&oacute;n en la que se encuentra por primera vez o -1 si no se encuentra. */
	private static int searchFor(final byte[] array, final byte[] subArray) {
		if (subArray.length > array.length) {
			return -1;
		}
		for (int i = 0; i <= array.length - subArray.length; i++) {
			if (array[i] == subArray[0]) {
				int j;
				for (j = 1; j < subArray.length; j++) {
					if (array[i + j] != subArray[j]) {
						break;
					}
				}
				if (j == subArray.length) {
					return i;
				}
			}
		}
		return -1;
	}
}
