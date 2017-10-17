/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.massive;

/** Tipo de firma masiva. Declara los tipos de firma masiva que existen:
 * <ul>
 * <li>SIGN: Firma.</li>
 * <li>COSIGN: Cofirma.</li>
 * <li>COUNTERSIGN_ALL: Contrafirma de todos los nodos de firma.</li>
 * <li>COUNTERSIGN_LEAFS: Contrafirma de los nodos hoja de firma.</li>
 * </ul> */
public enum MassiveType {
    /** Firma convencional. */
    SIGN,
    /** Cofirma. */
    COSIGN,
    /** Contrafirma de todo el &aacute;rbol de firmantes. */
    COUNTERSIGN_ALL,
    /** Contrafirma de solo las hojas del &aacute;rbol de firmantes. */
    COUNTERSIGN_LEAFS
}