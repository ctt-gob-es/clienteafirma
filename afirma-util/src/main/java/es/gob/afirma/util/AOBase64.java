/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.util;

import java.util.logging.Logger;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.binary.StringUtils;

/**
 * Permite la transformar de cadenas de bytes a Base64 y viceversa. 
 */
public final class AOBase64 {
    
    private AOBase64() {
        // No permitimos la instancacion
    }

    /** Codifica unos datos a base 64. Si ocurre cualquier error durante la
     * lectura de los datos, se devolver&aacute; {@code null}.
     * @param data
     *        Datos que deseamos transformar.
     * @param chunked
     *        Indica si debe insertarse un salto de l&iacute;nea cada 76
     *        caracteres.
     * @return Cadena en base 64. */
    public static String encode(final byte[] data, final boolean chunked) {
        try {
            return StringUtils.newStringUtf8(Base64.encodeBase64(data, chunked));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("No se pudo convertir un binario a base 64, se devolvera null: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
        }
    }
    
    /** Descodifica una cadena en base 64. Si se le proporciona un {@code null},
     * devuelve {@code null}.
     * @param b64Data
     *        Cadena de texto en base 64.
     * @return Datos descodificados. */
    public static byte[] decode(final String b64Data) {
        return (b64Data == null ? null : Base64.decodeBase64(b64Data));
    }

    /** Descodifica un array en base 64. Si se le proporciona un {@code null},
     * devuelve {@code null}.
     * @param b64Data
     *        Array con los contenidos en base 64.
     * @return Datos descodificados. */
    public static byte[] decode(final byte[] b64Data) {
        return (b64Data == null ? null : Base64.decodeBase64(b64Data));
    }
}
