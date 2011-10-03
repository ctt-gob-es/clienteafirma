/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

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