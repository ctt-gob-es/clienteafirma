/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.io.IOException;
import java.security.PrivateKey;
import java.util.Properties;

import es.gob.afirma.core.AOException;

/** Define los requerimientos de las clases capaces de efectuar contrafirmas digitales.
 * @version 1.0 */
public interface AOCounterSigner {

    /** Contrafirma nodos de firma concretos de una firma electr&oacute;nica.<br>
     * Los nodos que se deben firmar se indican en <code>targetType</code> y
     * pueden ser:
     * <ul>
     * <li>Todos los nodos del &aacute;rbol de firma</li>
     * <li>Los nodos hoja del &aacute;rbol de firma</li>
     * <li>Los nodos de firma cuyas posiciones se especifican en <code>target</code></li>
     * <li>Los nodos de firma realizados por los firmantes cuyo <i>Common Name</i> se indica en <code>target</code></li>
     * </ul>
     * Los algoritmos disponibles para la contrafirma se
     * declaran en {@link es.gob.afirma.core.signers.AOSignConstants}, mientras que los tipos de objetivo
     * se declaran en {@link es.gob.afirma.core.signers.CounterSignTarget}.
     * @param sign Flujo de lectura de los datos a firmar.
     * @param algorithm Algoritmo a usar para la firma (SHA1withRSA, SHA512withRSA, etc.).
     * @param targetType Tipo de objetivo de la contrafirma.
     * @param targets Informaci&oacute;n complementaria seg&uacute;n el tipo de objetivo de la contrafirma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param extraParams Par&aacute;metros adicionales para la contrafirma.
     * @return Contenido firmado
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma. */
    byte[] countersign(byte[] sign,
                              String algorithm,
                              CounterSignTarget targetType,
                              Object[] targets,
                              PrivateKey key,
                              final java.security.cert.Certificate[] certChain,
                              Properties extraParams) throws AOException, IOException;

}
