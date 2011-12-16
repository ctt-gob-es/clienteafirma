/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.envelopers;

import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;

/** Funcionalidades de sobres digitales. */
public interface AOEnveloper {
    
    /** Contruye distintas estructuras PKCS#7.
     * @param file
     *        Flujo de lectura de los datos a tratar.
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
    byte[] envelop(InputStream file, String digestAlgorithm, String type, PrivateKeyEntry keyEntry, X509Certificate[] certDest, AOCipherAlgorithm cipherAlgorithm, String dataType, Properties extraParams) throws AOException;

    /** Cifra un contenido.
     * @param file
     *        Flujo de lectura de los datos a firmar
     * @param digestAlgorithm
     *        Algoritmo a usar para la firma y la huella digital (SHA1withRSA, MD5withRSA,...)
     * @param key
     *        Puede ser una clave codificada o una contrase&ntilde;a usada
     *        para cifrar el contenido.
     * @param cipherAlgorithm
     *        Algoritmo a usar para el cifrado
     * @param dataType
     *        OID del tipo de datos a encriptar
     * @return Contenido encriptado
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    byte[] encrypt(final InputStream file, final String digestAlgorithm, final String key, AOCipherAlgorithm cipherAlgorithm, String dataType) throws AOException;
}
