/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Autofirma".
 * "Autofirma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
  * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.ciphers;

import java.security.Key;
import java.security.KeyException;
import java.security.NoSuchAlgorithmException;

import es.gob.afirma.core.AOException;


/** Define los requerimientos de las clases capaces de efectuar cifrado de datos.
 * Una clase de cifrado puede efectuar cifrados y descifrado de datos en
 * m&aacute;s de un formato.
 * @version 1.0 */
public interface AOCipher {

    /** Recupera las distintas configuraciones de algoritmos de cifrado
     * (algoritmo-modo_de_bloque-padding) que soporta el proveedor.
     * @return Configuraciones de cifrado. */
    AOCipherConfig[] getSupportedConfigs();

    /** Cifra un mensaje. El algoritmo que deseamos utilizar para el descifrado
     * puede ir acompa&ntilde;ado de una configuraci&oacute;n, seg&uacute;n lo
     * requiera el proveedor de cifrado. Por ejemplo, podr&iacute;a utilizarse
     * una de las siguientes cadenas:<br>
     * <ul>
     * <li><i>Algoritmo</i></li>
     * <li><i>Algoritmo</i>/<i>Modo</i>/<i>Padding</i></li>
     * </ul>
     * @param data
     *        Datos a cifrar.
     * @param algorithmConfig
     *        Configuraci&oacute;n del algoritmo de cifrado.
     * @param cipherKey
     *        Clave con la que se desea cifrar.
     * @return Datos cifrados.
     * @throws AOException
     *         Cuando ocurre un error durante la operaci&oacute;n
     * @throws KeyException
     *         Cuando la clave de cifrado introducida no es compatible con
     *         este algoritmo. */
    byte[] cipher(byte[] data, AOCipherConfig algorithmConfig, Key cipherKey) throws AOException, KeyException;

    /** Descifra un mensaje. El algoritmo que deseamos utilizar para el
     * descifrado puede ir acompa&ntilde;ado de una configuraci&oacute;n,
     * seg&uacute;n lo requiera el proveedor de cifrado. Por ejemplo,
     * podr&iacute;a utilizarse una de las siguientes cadenas:<br>
     * <ul>
     * <li><i>Algoritmo</i></li>
     * <li><i>Algoritmo</i>/<i>Modo</i>/<i>Padding</i></li>
     * </ul>
     * @param data
     *        Datos a descifrar.
     * @param algorithmConfig
     *        Configuraci&oacute;n del algoritmo de cifrado.
     * @param decipherKey
     *        Clave para el descifrado de los datos.
     * @return Datos descifrados.
     * @throws AOException
     *         Cuando ocurre un error durante la operaci&oacute;n
     * @throws KeyException
     *         Cuando la clave de cifrado introducida no es compatible con
     *         este algoritmo. */
    byte[] decipher(byte[] data, AOCipherConfig algorithmConfig, Key decipherKey) throws AOException, KeyException;

    /** Obtiene una clave para el algoritmo seleccionado a partir de su
     * codificaci&oacute;n. Seg&uacute;n el algoritmo puede ser necesario o no
     * el uso de par&aacute;metros adicionales.
     * @param key
     *        Clave codificada.
     * @param algorithmConfig
     *        Algoritmo de cifrado.
     * @param params
     *        Par&aacute;metros adicionales.
     * @return Clave.
     * @throws KeyException
     *         Cuando se produce un error al generar la clave. */
    Key decodeKey(byte[] key, AOCipherConfig algorithmConfig, Object[] params) throws KeyException;

    /** Obtiene una clave para el algoritmo seleccionado a partir de la
     * contrase&ntilde;a. Seg&uacute;n el algoritmo puede ser necesario o no el
     * uso de par&aacute;metros adicionales.
     * @param passphrase
     *        Contrase&ntilde;a para la generaci&oacute;n de la clave.
     * @param algorithmConfig
     *        Algoritmo de cifrado.
     * @param params
     *        Par&aacute;metros adicionales.
     * @return Clave.
     * @throws AOException
     *         Cuando se produce un error al generar la clave. */
    Key decodePassphrase(char[] passphrase, AOCipherConfig algorithmConfig, Object[] params) throws AOException;

    /** Genera una nueva clave para el algoritmo seleccionado.
     * @param algorithmConfig
     *        Algoritmo de cifrado.
     * @return Clave.
     * @throws NoSuchAlgorithmException
     *         Cuando el algoritmo de cifrado no est&aacute; soportado.
     * @throws AOException
     *         Cuando se produce un error al generar la clave. */
    Key generateKey(AOCipherConfig algorithmConfig) throws NoSuchAlgorithmException, AOException;

}
