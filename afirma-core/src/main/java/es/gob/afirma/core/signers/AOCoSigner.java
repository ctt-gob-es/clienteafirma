/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.core.signers;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import es.gob.afirma.core.AOException;

/** Define los requerimientos de las clases capaces de efectuar cofirmas digitales.
 * @version 1.0 */
public interface AOCoSigner {
    
    /** Cofirma un contenido (t&iacute;picamente un fichero). Para realizar la
     * cofirma se necesitan los datos originales (que este m&eacute;todo
     * firmar&aacute; normalmente) y la firma sobre la que se realiza la cofirma
     * (a los que se agregar&aacute; el resultado de la nueva firma).<br/>
     * Los algoritmos y modos de firma disponibles se declaran en {@link es.gob.afirma.misc.AOConstants}.
     * @param data
     *        Datos que deseamos a cofirmar.
     * @param sign
     *        Flujo de lectura de la firma de los datos que se quiere
     *        cofirmar.
     * @param algorithm
     *        Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @param extraParams
     *        Par&aacute;metros adicionales para la cofirma
     * @return Contenido firmado
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    byte[] cosign(byte[] data, byte[] sign, String algorithm, PrivateKeyEntry keyEntry, Properties extraParams) throws AOException;

    
    /** Cofirma un contenido (t&iacute;picamente un fichero). Para realizar la
     * cofirma se necesita el documento en el que se encuentra la firma sobre la
     * que se realiza la cofirma (a los que se agregar&aacute; el resultado de
     * la nueva firma).<br/>
     * Los algoritmos y modos de firma disponibles se declaran en {@link AOConstants}.
     * @param sign
     *        Firma de los datos que se quiere cofirmar.
     * @param algorithm
     *        Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @param extraParams
     *        Par&aacute;metros adicionales para la cofirma
     * @return Contenido firmado
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    byte[] cosign(byte[] sign, String algorithm, PrivateKeyEntry keyEntry, Properties extraParams) throws AOException;


}
