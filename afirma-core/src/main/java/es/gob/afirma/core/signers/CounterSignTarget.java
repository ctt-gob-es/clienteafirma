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

/** Permite definir los objetivos para la contrafirma:
 * <ul>
 * <li>SIGNERS: Contrafirma de firmantes concretos.</li>
 * <li>NODES: Contrafirma de nodos de firma concretos.</li>
 * <li>TREE: Contrafirma de todo el &aacute;rbol de firma.</li>
 * <li>LEAFS: Contrafirma de todos los nodos de firma.</li>
 * </ul> */
public enum CounterSignTarget {
    /** Contrafirma de firmantes concretos. */
    SIGNERS,
    /** Contrafirma de nodos de firma concretos. */
    NODES,
    /** Contrafirma de todo el &aacute;rbol de firma. */
    TREE,
    /** Contrafirma de todas las hojas del &aacute;rbol de firma. */
    LEAFS
}