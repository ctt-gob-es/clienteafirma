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