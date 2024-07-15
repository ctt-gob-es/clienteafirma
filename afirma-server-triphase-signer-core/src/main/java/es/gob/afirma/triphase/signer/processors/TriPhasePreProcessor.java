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
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;

/** Operaciones gen&eacute;ricas de firma trif&aacute;sica.
 * @author Tom&aacute;s Garc&iacute;a Mer&aacute;s. */
public interface TriPhasePreProcessor {

	/**
	 * Prefirma.
	 * @param data Datos a firmar.
	 * @param signAlgorithm Algoritmo de firma.
	 * @param cert Cadena de certificados del firmante.
	 * @param extraParams Par&aacute;meros adicionales de la firma.
	 * @param checkSignatures Comprobar si la entrada es una firma v&aacute;lida firma antes de agregarle la nueva firma.
	 * @return Resultado de la prefirma.
	 * @throws IOException Si hay errores en el tratamiento de los datos.
	 * @throws AOException En cualquier otro tipo de error.
	 */
	TriphaseData preProcessPreSign(byte[] data, String signAlgorithm, X509Certificate[] cert, Properties extraParams, boolean checkSignatures)
			throws IOException, AOException;

	/**
	 * Postfirma.
	 * @param data Datos a firmar.
	 * @param signAlgorithm Algoritmo de firma.
	 * @param cert Cadena de certificados del firmante.
	 * @param extraParams Par&aacute;meros adicionales de la firma.
	 * @param session Datos de la sesi&oacute;n (PK1, prefirma,...).
	 * @return Firma completa.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario.
	 * @throws IOException Si hay errores en el tratamiento de los datos.
	 * @throws AOException En cualquier otro tipo de error.
	 */
	byte[] preProcessPostSign(byte[] data, String signAlgorithm, X509Certificate[] cert, Properties extraParams, byte[] session)
			throws NoSuchAlgorithmException, IOException, AOException;

	/**
	 * Postfirma.
	 * @param data Datos a firmar.
	 * @param signAlgorithm Algoritmo de firma.
	 * @param cert Cadena de certificados del firmante.
	 * @param extraParams Par&aacute;meros adicionales de la firma.
	 * @param sessionData Datos de la sesi&oacute;n (PK1, prefirma,...).
	 * @return Firma completa.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario.
	 * @throws IOException Si hay errores en el tratamiento de los datos.
	 * @throws AOException En cualquier otro tipo de error.
	 */
	byte[] preProcessPostSign(byte[] data, String signAlgorithm, X509Certificate[] cert, Properties extraParams, TriphaseData sessionData) throws NoSuchAlgorithmException, IOException, AOException;


	/**
	 * Precofirma.
	 * @param data Datos a cofirmar.
	 * @param signAlgorithm Algoritmo de firma.
	 * @param cert Cadena de certificados del firmante.
	 * @param extraParams Par&aacute;meros adicionales de la firma.
	 * @param checkSignatures Comprobar si la entrada es una firma v&aacute;lida firma antes de agregarle la nueva firma.
	 * @return Resultado de la precofirma.
	 * @throws IOException Si hay errores en el tratamiento de los datos.
	 * @throws AOException En cualquier otro tipo de error.
	 */
	TriphaseData preProcessPreCoSign(byte[] data, String signAlgorithm, X509Certificate[] cert, Properties extraParams, boolean checkSignatures)
			throws IOException, AOException;

	/**
	 * Postcofirma.
	 * @param data Datos a cofirmar.
	 * @param signAlgorithm Algoritmo de firma.
	 * @param cert Cadena de certificados del firmante.
	 * @param extraParams Par&aacute;meros adicionales de la firma.
	 * @param session Datos de la sesi&oacute;n (PK1, prefirma,...).
	 * @return Cofirma completa.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario.
	 * @throws IOException Si hay errores en el tratamiento de los datos.
	 * @throws AOException En cualquier otro tipo de error.
	 */
	byte[] preProcessPostCoSign(byte[] data, String signAlgorithm, X509Certificate[] cert, Properties extraParams, byte[] session) throws NoSuchAlgorithmException, AOException, IOException;

	/**
	 * Postcofirma.
	 * @param data Datos a cofirmar.
	 * @param signAlgorithm Algoritmo de firma.
	 * @param cert Cadena de certificados del firmante.
	 * @param extraParams Par&aacute;meros adicionales de la firma.
	 * @param sessionData Datos de la sesi&oacute;n (PK1, prefirma,...).
	 * @return Cofirma completa.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario.
	 * @throws IOException Si hay errores en el tratamiento de los datos.
	 * @throws AOException En cualquier otro tipo de error.
	 */
	byte[] preProcessPostCoSign(byte[] data, String signAlgorithm, X509Certificate[] cert, Properties extraParams, TriphaseData sessionData) throws NoSuchAlgorithmException, AOException, IOException;


	/**
	 * Precontrafirma.
	 * @param sign Firma a contrafirmar.
	 * @param signAlgorithm Algoritmo de firma.
	 * @param cert Cadena de certificados del firmante.
	 * @param extraParams Par&aacute;meros adicionales de la firma.
	 * @param targets Objetivo de la contrafirma.
	 * @param checkSignatures Comprobar si la entrada es una firma v&aacute;lida firma antes de agregarle la nueva firma.
	 * @return Resultado de la precontrafirma.
	 * @throws IOException Si hay errores en el tratamiento de los datos.
	 * @throws AOException En cualquier otro tipo de error.
	 */
	TriphaseData preProcessPreCounterSign(byte[] sign, String signAlgorithm, X509Certificate[] cert, Properties extraParams, CounterSignTarget targets, boolean checkSignatures)
			throws IOException, AOException;

	/**
	 * Postcontrafirma.
	 * @param sign Firma a contrafirmar.
	 * @param signAlgorithm Algoritmo de firma.
	 * @param cert Cadena de certificados del firmante.
	 * @param extraParams Par&aacute;meros adicionales de la firma.
	 * @param session Datos de la sesi&oacute;n (PK1, prefirma,...).
	 * @param targets Objetivo de la contrafirma.
	 * @return Contrafirma completa.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario.
	 * @throws IOException Si hay errores en el tratamiento de los datos.
	 * @throws AOException En cualquier otro tipo de error.
	 */
	byte[] preProcessPostCounterSign(byte[] sign, String signAlgorithm, X509Certificate[] cert, Properties extraParams, byte[] session, CounterSignTarget targets) throws NoSuchAlgorithmException, AOException, IOException;

	/**
	 * Postcontrafirma.
	 * @param sign Firma a contrafirmar.
	 * @param signAlgorithm Algoritmo de firma.
	 * @param cert Cadena de certificados del firmante.
	 * @param extraParams Par&aacute;meros adicionales de la firma.
	 * @param sessionData Datos de la sesi&oacute;n (PK1, prefirma,...).
	 * @param targets Objetivo de la contrafirma.
	 * @return Contrafirma completa.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario.
	 * @throws IOException Si hay errores en el tratamiento de los datos.
	 * @throws AOException En cualquier otro tipo de error.
	 */
	byte[] preProcessPostCounterSign(byte[] sign, String signAlgorithm, X509Certificate[] cert, Properties extraParams, TriphaseData sessionData, CounterSignTarget targets) throws NoSuchAlgorithmException, AOException, IOException;
}
