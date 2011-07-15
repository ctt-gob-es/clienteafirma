/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.misc;

/** Constantes relativas a las firmas digitales. */
public final class AOSignConstants {

    // ************************************************************
    // ************* OPCIONES DE MULTIFIRMA ***********************
    // ************************************************************

    /** Permite definir los objetivos para la contrafirma:
     * <ul>
     * <li>Signers: Contrafirma de firmantes concretos.</li>
     * <li>Nodes: Contrafirma de nodos de firma concretos.</li>
     * <li>Tree: Contrafirma de todo el &aacute;rbol de firma.</li>
     * <li>Leafs: Contrafirma de todos los nodos de firma.</li>
     * </ul> */
    public static enum CounterSignTarget {
        /** Contrafirma de firmantes concretos. */
        Signers,
        /** Contrafirma de nodos de firma concretos. */
        Nodes,
        /** Contrafirma de todo el &aacute;rbol de firma. */
        Tree,
        /** Contrafirma de todas las hojas del &aacute;rbol de firma. */
        Leafs
    }

    private AOSignConstants() {}
}
