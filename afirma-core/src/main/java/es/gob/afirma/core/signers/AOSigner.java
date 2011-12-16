/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.signers;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.util.tree.AOTreeModel;

/** Define los requerimientos de las clases capaces de efectuar firmas digitales.
 * @version 1.0 */
public interface AOSigner extends AOCoSigner, AOCounterSigner {

    /** Firma electr&oacute;nicamente unos datos (t&iacute;picamente el contenido de un fichero).
     * @param data Datos que deseamos firmar.
     * @param algorithm Algoritmo a usar para la firma (cada implementaci&oacute;n puede aceptar unos valores diferentes)
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param extraParams Par&aacute;metros adicionales para la firma (dependientes de cada implementaci&oacute;n)
     * @return Contenido firmado
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    byte[] sign(byte[] data, String algorithm, PrivateKeyEntry keyEntry, Properties extraParams) throws AOException;

    /** Recupera el &aacute;rbol de nodos de firma de una firma electr&oacute;nica.
     * Los nodos del &aacute;rbol ser&aacute;n cadena de texto con el CommonName (CN X.500)
     * del titular del certificado u objetos de tipo AOSimpleSignInfo con la
     * informaci&oacute;n b&aacute;sica de las firmas individuales, seg&uacute;n
     * el valor del par&aacute;metro <code>asSimpleSignInfo</code>. Los nodos se
     * mostrar&aacute;n en el mismo orden y con la misma estructura con el que
     * aparecen en la firma electr&oacute;nica.<br>
     * La propia estructura de firma se considera el nodo ra&iacute;z, la firma y cofirmas
     * pender&aacute;n directamentede de este.
     * @param sign Firma electr&oacute;nica de la que se desea obtener la estructura.
     * @param asSimpleSignInfo
     *        Si es <code>true</code> se devuelve un &aacute;rbol con la
     *        informaci&oacute;n b&aacute;sica de cada firma individual
     *        mediante objetos <code>AOSimpleSignInfo</code>, si es <code>false</code> un &aacute;rbol con los nombres (CN X.500) de los
     *        titulares de los certificados.
     * @return &Aacute;rbol de nodos de firma o <code>null</code> en caso de error. */
    AOTreeModel getSignersStructure(byte[] sign, boolean asSimpleSignInfo);

    /** Indica si un dato es una firma compatible con la implementaci&oacute;n concreta.
     * @param is Dato que deseamos comprobar.
     * @return <code>true</code> si el dato es una firma reconocida por
     *         esta clase, <code>false</code> en caso contrario. */
    boolean isSign(byte[] is);

    /** Comprueba si el dato introducido es v&aacute;lido para ser firmado por
     * este manejador de firma.<br/>
     * @param is Dato que deseamos comprobar.
     * @return Devuelve <code>true</code> si el dato es susceptible de ser firmado por la implementaci&oacute;n concreta, 
     *         <code>false</code> en caso contrario. */
    boolean isValidDataFile(byte[] is);

    /** Devuelve el nombre de fichero de firma recomendado para el resultado de firmar un fichero 
     * con el nombre proporcionado. Si se indica una part&iacute;cula intermedia, se a&ntilde;ade esta al
     * nombre resultante seg&uacute;n el criterio de la implementaci&oacute;n concreta.
     * @param originalName Nombre del fichero original que se firma
     * @param inText Particula intermedia que agregar al nombre del fichero de
     *        firma.
     * @return Nombre apropiado para el fichero de firma o fichero firmado. */
    String getSignedName(String originalName, String inText);

    /** Recupera los datos originalmente firmados de una firma. 
     * En el caso de que la firma no contenga los datos firmados, se
     * devuelve <code>null</code>.
     * @param signData Datos de firma o fichero firmado.
     * @return Datos originalmente firmados.
     * @throws AOInvalidFormatException
     *         Si no se ha introducido un fichero de firma v&aacute;lido o no
     *         ha podido leerse la firma.
     * @throws AOException
     *         En caso de cualquier error durante la recuperaci&oacute;n de los
     *         datos.
     * @throws NullPointerException
     *         Si la firma introducida es nula. */
    byte[] getData(byte[] signData) throws AOException;

    /** Obtiene la informaci&oacute;n general de un objeto de firma. Ya que un objeto de
     * firma puede contener muchas firmas, se considera informaci&oacute;n
     * general la com&uacute;n que aplique a todo el objeto.
     * @param signData
     *        Firma que se desea analizar.
     * @return Informaci&oacute;n sobre la firma electr&oacute;nica
     * @throws AOInvalidFormatException
     *         Cuando la firma introducida no es un objeto de firma
     *         reconocido por este manejador.
     * @throws AOException
     *         Ocurri&oacute; un error durante la recuperaci&oacute;n de los
     *         datos.
     * @throws NullPointerException
     *         La firma introducida es nula. */
    AOSignInfo getSignInfo(byte[] signData) throws AOException;

}
