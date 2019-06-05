/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.signer.cades;

import java.security.MessageDigest;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.security.interfaces.RSAPublicKey;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSimpleSigner;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;

/** Sustituto del firmador PKCS#1 para firmas trif&aacute;sicas.
 * No firma realmente, sino que devuelve unos datos aleatorios del tama&ntilde;o adecuado y
 * guarda estos m&aacute;s los datos que deben ser firmados para que en el cliente pueda
 * realizarse la firma. Si se construye un objeto con los datos generados en cliente,
 * se dispondr&aacute; de los PKCS1 para sustituir en la verdadera firma. Los datos dummy
 * son siempre los mismos para los mismos datos, ya que se compone a base de concatenar el
 * hash de esos datos. Por eso, puede directamente sustituirse en la postfirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CAdESFakePkcs1Signer implements AOSimpleSigner {

	private static final String MD_ALGORITHM = "SHA-512";   //$NON-NLS-1$

	/** Tama&ntilde;o de una firma PKCS#1 con clave RSA de 1024 bits. */
	private static final Integer PKCS1_DEFAULT_SIZE_1024 = Integer.valueOf(128);

	/** Tama&ntilde;o de una firma PKCS#1 con clave RSA de 2048 bits. */
	private static final Integer PKCS1_DEFAULT_SIZE_2048 = Integer.valueOf(256);

	/** Tama&ntilde;o de una firma PKCS#1 con clave RSA de 4096 bits. */
	private static final Integer PKCS1_DEFAULT_SIZE_4096 = Integer.valueOf(512);

	private static final Integer KEY_SIZE_1024 = Integer.valueOf(1024);
	private static final Integer KEY_SIZE_2048 = Integer.valueOf(2048);
	private static final Integer KEY_SIZE_4096 = Integer.valueOf(4096);

	private static final Map<Integer, Integer> P1_SIZES = new HashMap<>(3);
	static {
		P1_SIZES.put(KEY_SIZE_1024, PKCS1_DEFAULT_SIZE_1024);
		P1_SIZES.put(KEY_SIZE_2048, PKCS1_DEFAULT_SIZE_2048);
		P1_SIZES.put(KEY_SIZE_4096, PKCS1_DEFAULT_SIZE_4096);
	}

	private static final String PARAM_PRE = "PRE";   //$NON-NLS-1$

	private static final String PARAM_DUMMY_PK1 = "DPK1";   //$NON-NLS-1$

	private final TriphaseData triphaseData;
	private final String id;
	private final boolean registry;

	/** Construye el sustituto del firmador PKCS#1 para firmas trif&aacute;sicas.
	 * @param triphaseData Resultado donde ir almacenando los pares de datos a firmar
	 *                     y datos aleatorios a sustituir.
	 * @param signId Identificador de la firma a realizar
	 * @param registry Indica si las firmas realizadas deben quedar registrada internamente.
	 *                 Esto es de utilidad en la prefirma, no en la postfirma.*/
	public CAdESFakePkcs1Signer(final TriphaseData triphaseData,
			                    final String signId,
			                    final boolean registry) {
		if (triphaseData == null) {
			throw new IllegalArgumentException(
				"Es necesario un resultado de PreContrafirma para ir almacenando las firmas" //$NON-NLS-1$
			);
		}
		this.triphaseData = triphaseData;
		this.registry = registry;
		this.id = signId;
	}

	@Override
	public byte[] sign(final byte[] data,
			           final String algorithm,
			           final PrivateKey key,
			           final Certificate[] certChain,
			           final Properties extraParams) throws AOException {

		// La clave debe ser nula porque este proceso se realiza en servidor, pero el tamano de la clave
		// puede salir del Certificado

		// Obtenemos el tamano de clave y de PKCS#1
		final Integer p1Size;
		if (certChain[0].getPublicKey() instanceof RSAPublicKey) {
			final int keySize = ((RSAPublicKey)((X509Certificate)certChain[0]).getPublicKey()).getModulus().bitLength();
			p1Size = P1_SIZES.get(Integer.valueOf(keySize));
		}
		else {
			throw new AOException(
				"Tipo de clave no soportada: " + certChain[0].getPublicKey().getAlgorithm() //$NON-NLS-1$
			);
		}
		if (p1Size == null) {
			throw new AOException("Tamano de clave no soportado: " + p1Size); //$NON-NLS-1$
		}

		// Calculamos un valor que sera siempre el mismo para los mismos datos y de las dimensiones que
		// corresponden a un PKCS#1 del tamano de clave del certificado utilizado
		final byte[] sha512;
		try {
			sha512 = MessageDigest.getInstance(MD_ALGORITHM).digest(data);
		}
		catch (final Exception e) {
			throw new AOException(
				"Ocurrio un error al generar el PKCS#1 temporal de los datos: " + e, e //$NON-NLS-1$
			);
		}

		// Metemos una huella SHA512 que se repite hasta completar el hueco
		final byte[] dummyData = new byte[p1Size.intValue()];
		for (int i = 0; i < dummyData.length; i += sha512.length) {
			System.arraycopy(sha512, 0, dummyData, i, sha512.length);
		}

		// Si no existe ya, guardamos el par de PKCS#1 falso y datos a firmar
		if (this.registry) {
			final Map<String, String> signConfig = new HashMap<>();
			signConfig.put(PARAM_PRE, Base64.encode(data));
			signConfig.put(PARAM_DUMMY_PK1, Base64.encode(dummyData));
			this.triphaseData.addSignOperation(
				new TriSign(
					signConfig,
					this.id
				)
			);
		}

		// Devolvemos el dato unico que luego sera reemplazado por la firma adecuada
		return dummyData;
	}
}
