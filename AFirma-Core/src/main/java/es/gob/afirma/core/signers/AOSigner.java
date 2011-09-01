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
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.AOUnsupportedSignFormatException;
import es.gob.afirma.core.signers.beans.AOSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;

/** Define los requerimientos de las clases capaces de efectuar firmas digitales.
 * @version 1.0 */
public interface AOSigner extends AOCoSigner, AOCounterSigner {

    /** Firma un contenido (t&iacute;picamente un fichero).<br/>
     * Los algoritmos y modos de firma disponibles se declaran en {@link es.gob.afirma.misc.AOConstants}.
     * @param data
     *        Datos que deseamos a firmar.
     * @param algorithm
     *        Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @param extraParams
     *        Par&aacute;metros adicionales para la firma
     * @return Contenido firmado
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    byte[] sign(byte[] data, String algorithm, PrivateKeyEntry keyEntry, Properties extraParams) throws AOException;

    /** Recupera el &aacute;rbol de nodos de firma de una firma
     * electr&oacute;nica. Cada uno de los nodos de firma est&aacute;
     * representado por el alias del certificado con el que se realizo firma.<br/>
     * Los nodos del &aacute;rbol ser&aacute;n cadena de texto con el CommonName
     * del certificado u objetos de tipo {@link es.gob.afirma.signers.beans.AOSimpleSignInfo} con la
     * informaci&oacute;n b&aacute;sica de las firmas individuales, seg&uacute;n
     * el valor del par&aacute;metro <code>asSimpleSignInfo</code>. Los nodos se
     * mostrar&aacute;n en el mismo orden y con la misma estructura con el que
     * aparecen en la firma electr&oacute;nica.<br/>
     * El fichero se considera el nodo ra&iacute;z, la firma y cofirmas
     * pender&aacute;n directamentede de &eacute;ste.
     * @param sign
     *        Firma electr&oacute;nica de la que se desea obtener la
     *        estructura.
     * @param asSimpleSignInfo
     *        Si es <code>true</code> se devuelve un &aacute;rbol con la
     *        informaci&oacute;n b&aacute;sica de cada firma individual
     *        mediante objetos {@link es.gob.afirma.signers.beans.AOSimpleSignInfo}, si es <code>false</code> un &aacute;rbol con los nombres de los
     *        certificados.
     * @return &Aacute;rbol de nodos de firma o <code>null</code> en caso de
     *         error. */
    AOTreeModel getSignersStructure(byte[] sign, boolean asSimpleSignInfo);

    /** Indica si un dato es una firma compatible con el signer concreto.
     * @param is
     *        Dato que deseamos comprobar.
     * @return Devuelve <code>true</code> si el dato es una firma reconocida por
     *         este signer, <code>false</code> en caso contrario. */
    boolean isSign(byte[] is);

    /** Comprueba que el dato introducido sea v&aacute;lido para ser firmado por
     * este manejador de firma.<br/>
     * Por ejemplo, un signer CMS siempre devolver&iacute;a <code>true</code> ,
     * ya que puede firmar cualquier fichero binario, en cambio, un signer de
     * PDF s&oacute;lo podr&iacute;a firmar PDF. Si un signer puede firmar unos
     * datos segun la configuraci&oacute;n establecida para la firma, se
     * aceptar&aacute;n todos los formatos permitidos para alguna de las
     * configuraciones. Por ejemplo, un signer XAdES aceptar datos XML o
     * binarios para la firma Detached pero s&oacute;lo XML para la Enveloped.
     * Si este signer recibe unos datos binarios, devolver&aacute; <code>true</code>.
     * @param is
     *        Dato que deseamos comprobar.
     * @return Devuelve <code>true</code> si el dato es V6aacute;lido para
     *         firmar, <code>false</code> en caso contrario. */
    boolean isValidDataFile(byte[] is);

    /** Devuelve el nombre de fichero de firma predeterminado que
     * asignar&iacute;a este signer a un fichero con el nombre asignado. Si se
     * indica una part&iacute;cula intermedia, se devolver&aacute; el nombre con
     * esta particula asignada siempre y cuando esto tenga sentido.
     * @param originalName
     *        Nombre del fichero original que se firma
     * @param inText
     *        Particula intermedia que agregar al nombre del fichero de
     *        firma.
     * @return Nombre apropiado para el fichero de firma. */
    String getSignedName(String originalName, String inText);

    /** Establece el formato que tienen los datos que se firman.
     * @param description
     *        Descripci&oacute;n opcional del elemento DataObjectFormat
     * @param objectIdentifier
     *        Instancia de <code>ObjectIdentifier</code>
     * @param mimeType
     *        MimeType de los datos. Por defecto <code>application/octet-stream</code>
     * @param encoding
     *        Codificaci&oacute;n de los datos. Por defecto <code>base64</code> */
    void setDataObjectFormat(String description, String objectIdentifier, String mimeType, String encoding);

    /** Recupera los datos originalmente firmados de la firma pasada por
     * par&aacute;metro. En caso de no contener la firma los datos firmados, se
     * devuelve <code>null</code>.
     * @param signData
     *        Datos de firma.
     * @return Datos originalmente firmados.
     * @throws AOInvalidFormatException
     *         No se ha introducido un fichero de firma v&aacute;lido o no
     *         ha podido leerse la firma.
     * @throws AOException
     *         Ocurri&oacute; un error durante la recuperaci&oacute;n de los
     *         datos.
     * @throws NullPointerException
     *         La firma introducida es nula. */
    byte[] getData(byte[] signData) throws AOInvalidFormatException, AOException;

    /** Obtiene la informacion general de un objeto de firma. Ya que un objeto de
     * firma puede contener muchas firmas, se considera informaci&oacute;n
     * general la que aplica a todo el objeto. Esto es:
     * <ul>
     * <li>Formato de firma: Formato general de la firma (p.e. CAdES, XAdES,...)</li>
     * <li>Variante: Variante del formato de firma (p.e. Enveloped, Detached,...)</li>
     * <li>URL de firma: URL desde donde descargar el fichero de firma.  Esta
     * informaci&oacute;n puede haberse insertado en alg&uacute;n campo no estandarizado.</li>
     * <li>URL de datos: URL desde donde descargar el fichero de datos. Esta
     * informaci&oacute;n puede haberse insertado en alg&uacute;n campo no estandarizado.</li>
     * <li>C&oacute;digo de verificaci&oacute;n: C&oacute;digo en base64 para la
     * verificaci&oacute;n de la firma.</li>
     * </ul>
     * Todos los campos, salvo el "Formato de firma" son opcionales.<br/>
     * <br/>
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
    AOSignInfo getSignInfo(byte[] signData) throws AOInvalidFormatException, AOException;

    /** Obtiene el tipo de datos declarado en una firma mediante su Mime Type. Si
     * no se conoce el tipo de dato se devolver&aacute; <code>null</code>.
     * Seg&uacute;n el formato de firma puede haber un tipo de datos por
     * defecto: application/octect-stream, data...
     * @param signData
     *        Firma electr&oacute;nica.
     * @return Mime Type de los datos contenidos en la firma.
     * @throws AOUnsupportedSignFormatException
     *         Cuando la firma no est&eacute; soportada por el manejador
     *         proporcionado. */
    String getDataMimeType(byte[] signData) throws AOUnsupportedSignFormatException;
}
