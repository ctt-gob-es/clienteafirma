/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.signers;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import es.gob.afirma.core.AOException;

/** Define los requerimientos de las clases capaces de efectuar firmas digitales simples (un &uacute;nico firmante).
 * @version 1.0
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface AOSimpleSigner {

    /** Firma electr&oacute;nicamente unos datos (t&iacute;picamente el contenido de un fichero).
     * @param data Datos que deseamos firmar.
     * @param algorithm Algoritmo a usar para la firma (cada implementaci&oacute;n puede aceptar unos valores diferentes)
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param extraParams Par&aacute;metros adicionales para la firma (dependientes de cada implementaci&oacute;n)
     * @return Contenido firmado
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    byte[] sign(byte[] data, String algorithm, PrivateKeyEntry keyEntry, Properties extraParams) throws AOException;

}
