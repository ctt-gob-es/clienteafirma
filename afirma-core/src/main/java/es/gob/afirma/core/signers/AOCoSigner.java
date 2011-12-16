/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.signers;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import es.gob.afirma.core.AOException;

/** Define los requerimientos de las clases capaces de efectuar cofirmas digitales.
 * @version 1.0 */
public interface AOCoSigner {
    
    /** Cofirma un contenido (t&iacute;picamente un fichero). Para realizar la
     * cofirma se necesitan los datos originales (que este m&eacute;todo
     * firmar&aacute; normalmente) y la firma sobre la que se realiza la cofirma
     * (a los que se agregar&aacute; el resultado de la nueva firma).<br/>
     * @param data
     *        Datos que deseamos a cofirmar.
     * @param sign
     *        Flujo de lectura de la firma de los datos que se quiere
     *        cofirmar.
     * @param algorithm
     *        Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @param extraParams
     *        Par&aacute;metros adicionales para la cofirma
     * @return Contenido firmado
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    byte[] cosign(byte[] data, byte[] sign, String algorithm, PrivateKeyEntry keyEntry, Properties extraParams) throws AOException;

    
    /** Cofirma un contenido (t&iacute;picamente un fichero). Para realizar la
     * cofirma se necesita el documento en el que se encuentra la firma sobre la
     * que se realiza la cofirma (a los que se agregar&aacute; el resultado de
     * la nueva firma).<br/>
     * @param sign
     *        Firma de los datos que se quiere cofirmar.
     * @param algorithm
     *        Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @param extraParams
     *        Par&aacute;metros adicionales para la cofirma
     * @return Contenido firmado
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    byte[] cosign(byte[] sign, String algorithm, PrivateKeyEntry keyEntry, Properties extraParams) throws AOException;


}
