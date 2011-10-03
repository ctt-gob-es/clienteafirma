/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.util;

import java.util.logging.Logger;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.binary.StringUtils;

/**
 * Permite la transformar de cadenas de bytes a Base64 y viceversa. 
 */
public class AOBase64 {

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
