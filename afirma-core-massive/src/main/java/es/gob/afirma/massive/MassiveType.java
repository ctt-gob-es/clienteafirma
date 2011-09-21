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