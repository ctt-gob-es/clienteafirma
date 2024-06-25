/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.io.IOException;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.util.tree.AOTreeModel;

/** Define los requerimientos de las clases capaces de efectuar firmas digitales.
 * @version 1.0 */
public interface AOSigner extends AOCoSigner, AOCounterSigner, AOSimpleSigner {

    /** Recupera el &aacute;rbol de nodos de firma de una firma electr&oacute;nica.
     * Los nodos del &aacute;rbol ser&aacute;n cadena de texto con el <i>CommonName</i> (CN X.500)
     * del titular del certificado u objetos de tipo <code>AOSimpleSignInfo</code> con la
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
     * @return &Aacute;rbol de nodos de firma o <code>null</code> en caso de error.
     * @throws AOInvalidFormatException
     *         Si no se ha introducido un fichero de firma v&aacute;lido del formato correspondiente.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma */
    AOTreeModel getSignersStructure(byte[] sign, boolean asSimpleSignInfo) throws AOInvalidFormatException, IOException;

	/** Recupera el &aacute;rbol de nodos de firma de una firma electr&oacute;nica.
	 * Los nodos del &aacute;rbol ser&aacute;n textos con el <i>CommonName</i> (CN X.500)
	 * del titular del certificado u objetos de tipo AOSimpleSignInfo con la
	 * informaci&oacute;n b&aacute;sica de las firmas individuales, seg&uacute;n
	 * el valor del par&aacute;metro <code>asSimpleSignInfo</code>. Los nodos se
	 * mostrar&aacute;n en el mismo orden y con la misma estructura con el que
	 * aparecen en la firma electr&oacute;nica.<br>
	 * La propia estructura de firma se considera el nodo ra&iacute;z, la firma y cofirmas
	 * pender&aacute;n directamentede de este.
	 * @param sign Firma electr&oacute;nica de la que se desea obtener la estructura.
	 * @param params Par&aacute;metros necesarios para comprobar si una firma es compatible.
	 * @param asSimpleSignInfo Si es <code>true</code> se devuelve un &aacute;rbol con la
	 *                         informaci&oacute;n b&aacute;sica de cada firma individual
	 *                         mediante objetos <code>AOSimpleSignInfo</code>, si es <code>false</code>
	 *                         un &aacute;rbol con los nombres (CN X.500) de los titulares certificados.
	 * @return &Aacute;rbol de nodos de firma o <code>null</code> en caso de error.
	 * @throws AOInvalidFormatException
     *         Si no se ha introducido un fichero de firma v&aacute;lido del formato correspondiente.
	 * @throws IOException Si ocurren problemas relacionados con la lectura de la firma */
	AOTreeModel getSignersStructure(byte[] sign, Properties params, boolean asSimpleSignInfo) throws AOInvalidFormatException, IOException;

    /** Indica si un dato es una firma compatible con la implementaci&oacute;n concreta.
     * @param is Dato que deseamos comprobar.
     * @return <code>true</code> si el dato es una firma reconocida por
     *         esta clase, <code>false</code> en caso contrario.
     * @throws IOException Si ocurren problemas relacionados con la lectura de los datos */
    boolean isSign(byte[] is) throws IOException;

    /** Indica si un dato es una firma compatible con la implementaci&oacute;n concreta.
     * @param signData Dato que deseamos comprobar.
     * @param params Par&aacute;metros necesarios para comprobar si una firma es compatible.
     * @return <code>true</code> si el dato es una firma reconocida por
     *         esta clase, <code>false</code> en caso contrario.
     * @throws IOException Si ocurren problemas relacionados con la lectura de los datos */
    boolean isSign(byte[] signData, Properties params) throws IOException;

    /** Comprueba si el dato introducido es v&aacute;lido para ser firmado por
     * este manejador de firma.<br>
     * @param is Dato que deseamos comprobar.
     * @return Devuelve <code>true</code> si el dato es susceptible de ser firmado por la implementaci&oacute;n concreta,
     *         <code>false</code> en caso contrario.
     * @throws IOException Cuando ocurre alg&uacute;n error durante la lectura de los datos */
    boolean isValidDataFile(byte[] is) throws IOException;

    /** Devuelve el nombre de fichero de firma recomendado para el resultado de firmar un fichero
     * con el nombre proporcionado. Si se indica una part&iacute;cula intermedia, se a&ntilde;ade esta al
     * nombre resultante seg&uacute;n el criterio de la implementaci&oacute;n concreta.
     * @param originalName Nombre del fichero original que se firma
     * @param inText Part&iacute;cula intermedia que agregar al nombre del fichero de
     *        firma.
     * @return Nombre apropiado para el fichero de firma o fichero firmado. */
    String getSignedName(String originalName, String inText);

    /** Recupera los datos originalmente firmados de una firma.
     * En el caso de que la firma no contenga los datos firmados, se
     * devuelve <code>null</code>.
     * @param signData Datos de firma o fichero firmado.
     * @return Datos originalmente firmados.
     * @throws es.gob.afirma.core.AOInvalidFormatException
     *         Si no se ha introducido un fichero de firma v&aacute;lido o no
     *         ha podido leerse la firma.
     * @throws AOException
     *         En caso de cualquier error durante la recuperaci&oacute;n de los
     *         datos.
     * @throws IOException Si no se puede leer la firma. */
    byte[] getData(byte[] signData) throws AOException, IOException;

	/** Si la entrada es un documento PDF, devuelve el mismo documento PDF.
	 * @param sign Documento PDF
	 * @param params Par&aacute;metros necesarios para comprobar si una firma es compatible.
	 * @return Mismo documento PDF de entrada, sin modificar en ning&uacute; aspecto.
	 * @throws AOInvalidFormatException Si los datos de entrada no son un documento PDF.
	 * @throws IOException si no se puede leer la firma
	 * @throws AOException En caso de cualquier error durante la recuperaci&oacute;n de los datos*/
	byte[] getData(byte[] sign, Properties params) throws AOInvalidFormatException, IOException, AOException;

    /** Obtiene la informaci&oacute;n general de un objeto de firma. Ya que un objeto de
     * firma puede contener muchas firmas, se considera informaci&oacute;n
     * general la com&uacute;n que aplique a todo el objeto.
     * @param signData
     *        Firma que se desea analizar.
     * @return Informaci&oacute;n sobre la firma electr&oacute;nica
     * @throws es.gob.afirma.core.AOInvalidFormatException
     *         Cuando la firma introducida no es un objeto de firma
     *         reconocido por este manejador.
     * @throws AOException
     *         Ocurri&oacute; un error durante la recuperaci&oacute;n de los
     *         datos.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma. */
    AOSignInfo getSignInfo(byte[] signData) throws AOException, IOException;

	/** Si la entrada es un documento PDF, devuelve un objeto <code>AOSignInfo</code>
	 * con el formato establecido a <code>AOSignConstants.SIGN_FORMAT_PDF</code>.
	 * @param data Documento PDF.
	 * @param params Par&aacute;metros necesarios para comprobar si una firma es compatible.
	 * @return Objeto <code>AOSignInfo</code> con el formato establecido a <code>AOSignConstants.SIGN_FORMAT_PDF</code>.
	 * @throws AOException Si los datos de entrada no son un documento PDF.
	 * @throws IOException si ocurren problemas relacionados con la lectura de la firma.*/
	AOSignInfo getSignInfo(byte[] data, Properties params) throws AOException, IOException;

}
