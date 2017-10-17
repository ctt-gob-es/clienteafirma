/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
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
    SIGNERS("signers"), //$NON-NLS-1$
    /** Contrafirma de nodos de firma concretos. */
    NODES("nodes"), //$NON-NLS-1$
    /** Contrafirma de todo el &aacute;rbol de firma. */
    TREE("tree"), //$NON-NLS-1$
    /** Contrafirma de todas las hojas del &aacute;rbol de firma. */
    LEAFS("leafs"); //$NON-NLS-1$

    private final String name;

    private CounterSignTarget(final String n) {
    	this.name = n;
    }

    @Override
	public String toString() {
    	return this.name;
    }

    /** Obtiene el objetivo de la contrafirma a partir de su nombre.
     * @param name Nombre del objetivo de la contrafirma
     * @return Objetivo de la contrafirma. */
    public static CounterSignTarget getTarget(final String name) {
    	if (name == null) {
        	throw new IllegalArgumentException(
    			"El objetivo de la contrafirma no puede ser nulo" //$NON-NLS-1$
    		);
    	}
    	final String n = name.trim();
    	if ("signers".equalsIgnoreCase(n)) { //$NON-NLS-1$
    		return SIGNERS;
    	}
    	if ("nodes".equalsIgnoreCase(n)) { //$NON-NLS-1$
    		return NODES;
    	}
    	if ("tree".equalsIgnoreCase(n)) { //$NON-NLS-1$
    		return TREE;
    	}
    	if ("leafs".equalsIgnoreCase(n)) { //$NON-NLS-1$
    		return LEAFS;
    	}
    	throw new IllegalArgumentException(
			"Objetivo no soportado: " + n //$NON-NLS-1$
		);
    }
}