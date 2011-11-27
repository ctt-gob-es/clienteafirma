/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.core.signers;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import es.gob.afirma.core.AOException;

/** Define los requerimientos de las clases capaces de efectuar contrafirmas digitales.
 * @version 1.0 */
public interface AOCounterSigner {

    /** Contrafirma nodos de firma concretos de una firma electr&oacute;nica.<br/>
     * Los nodos que se deben firmar se indican en <code>targetType</code> y
     * pueden ser:
     * <ul>
     * <li>Todos los nodos del &aacute;rbol de firma</li>
     * <li>Los nodos hoja del &aacute;rbol de firma</li>
     * <li>Los nodos de firma cuyas posiciones se especifican en <code>target</code></li>
     * <li>Los nodos de firma realizados por los firmantes cuyo <i>Common Name</i> se indica en <code>target</code></li>
     * </ul>
     * Los algoritmos y tipos de objetivo de la contrafirma disponibles se
     * declaran en {@link es.gob.afirma.misc.AOConstants}.
     * @param sign
     *        Flujo de lectura de los datos a firmar
     * @param algorithm
     *        Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
     * @param targetType
     *        Tipo de objetivo de la contrafirma
     * @param targets
     *        Informaci&oacute;n complementario seg&uacute;n el tipo de
     *        objetivo de la contrafirma
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @param extraParams
     *        Par&aacute;metros adicionales para la contrafirma
     * @return Contenido firmado
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    byte[] countersign(byte[] sign,
                              String algorithm,
                              CounterSignTarget targetType,
                              Object[] targets,
                              PrivateKeyEntry keyEntry,
                              Properties extraParams) throws AOException;
    
}
