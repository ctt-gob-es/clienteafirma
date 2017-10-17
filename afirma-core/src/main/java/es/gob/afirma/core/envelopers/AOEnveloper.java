/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.envelopers;

import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.security.spec.InvalidKeySpecException;
import java.util.Properties;
import java.util.zip.DataFormatException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;

/** Funcionalidades de sobres digitales. */
public interface AOEnveloper {

    /** Contruye distintas estructuras PKCS#7.
     * @param data
     *        Datos que se van a envolver.
     * @param digestAlgorithm
     *        Algoritmo a usar para la firma y huella digital (SHA1withRSA, MD5withRSA,...)
     * @param type
     *        Tipo de estructura que se quiere construir
     * @param keyEntry
     *        Clave privada a usar para firmar.
     * @param certDest
     *        Certificados de los usuarios a los que va destinado el sobre
     *        digital.
     * @param cipherAlgorithm
     *        Algoritmo a usar para el cifrado
     * @param dataType
     *        OID del tipo de datos a encriptar
     * @param extraParams
     *        Par&aacute;metros adicionales
     * @return Estructura PKCS#7/CMS/CAdES/Etc.
     * @throws AOException
     *         Cuando ocurre cualquier problema en el proceso. */
    byte[] envelop(byte[] data, String digestAlgorithm, String type, PrivateKeyEntry keyEntry, X509Certificate[] certDest, AOCipherAlgorithm cipherAlgorithm, String dataType, Properties extraParams) throws AOException;

    /** Cifra un contenido.
     * @param data
     *        Datos que se van a encriptar.
     * @param digestAlgorithm
     *        Algoritmo a usar para la firma y la huella digital (SHA1withRSA, MD5withRSA,...)
     * @param key
     *        Clave codificada o contrase&ntilde;a usada
     *        para cifrar el contenido.
     * @param cipherAlgorithm
     *        Algoritmo a usar para el cifrado
     * @param dataType
     *        OID del tipo de datos a encriptar
     * @return Contenido encriptado
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    byte[] encrypt(byte[] data, String digestAlgorithm, String key, AOCipherAlgorithm cipherAlgorithm, String dataType) throws AOException;


    /** Recupera los datos contenidos en un envoltorio.
     * @param envelop Envoltorio de datos.
     * @param addresseePke Clave privada del receptor del envoltorio (si es necesario)
     * @return Datos contenidos en el envoltorio.
     * @throws InvalidKeyException Si la clave indicada no es v&aacute;lida o no pertenece a un destinatario del envoltorio.
     * @throws AOException Cuando ocurre algun problema en la apetura del envoltorio.
     * @throws IOException Si hay problema de lectura / escritura de datos.
     * @throws InvalidKeySpecException Cuando la clave es inv&aacute;lida.
     * @throws DataFormatException Si hay errores en el formato de datos esperados. */
    byte[] recoverData(byte[] envelop, PrivateKeyEntry addresseePke) throws InvalidKeyException, AOException, IOException, InvalidKeySpecException, DataFormatException;
}
