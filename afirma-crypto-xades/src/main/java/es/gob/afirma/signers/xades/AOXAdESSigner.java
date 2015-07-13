/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.xades;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Transform;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import net.java.xades.util.XMLUtils;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;

/** Manejador de firmas XML XAdES
 * <p>Soporta XAdES-BES y XAdES-EPES.</p>
 * <p>
 *  Debido a errores en algunas versiones del entorno de ejecuci&oacute;n de Java, esta clase puede generar ocasionalmente
 *  mensajes en consola del tipo: <code>[Fatal Error] :1:1: Content is not allowed in prolog.</code>. Estos
 *  deben ignorarse, ya que no indican ninguna condici&oacute;n de error ni malfuncionamiento.
 * </p>
 * <p>
 *  Los atributos espec&iacute;ficos XAdES implementados por esta clase (adem&aacute;s de los
 *  relativos a las politicas de firma) son:
 * </p>
 * <ul>
 *  <li><i>SigningTime</i></li>
 *  <li><i>SigningCerticate</i></li>
 *  <li><i>IssuerSerial</i></li>
 *  <li><i>SignedDataObjectProperties</i></li>
 * </ul>
 * <p><b>Distintos formatos de firmas XML</b></p>
 * <dl>
 *  <dt><i>Detached</i></dt>
 *  <dd>
 *   <p>
 *    La firma XML en modo <i>Detached</i> permite tener una firma de forma separada e independiente del
 *    contenido firmado, pudiendo relacionar firma con contenido firmado mediante una referencia de tipo
 *    URI. Este tipo de firmas es &uacute;til cuando no se puede modificar el contenido original pero se
 *    desea constatar su autenticidad, procedencia, etc.<br>
 *   </p>
 *   <p>
 *    Un uso com&uacute;n de este formato es en la descarga de ficheros, pudiendo poner a disposici&oacute;n
 *    del internauta, junto al contenido a descargar, un peque&ntilde;o fichero de firma para verificar la
 *    integridad del primero.
 *   </p>
 *   <p>
 *    Un ejemplo de este tipo de firmas ser&iacute;a la siguiente estructura (resumida) XML:
 *   </p>
 *   <pre>
 *   &lt;?xml version="1.0" encoding="UTF-8"?&gt;
 *    &lt;ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig#"&gt;
 *     &lt;ds:SignedInfo&gt;
 *      &lt;ds:CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/&gt;
 *      &lt;ds:SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/&gt;
 *      &lt;ds:Reference URI="http://www.mpt.es/contenido"&gt;
 *       &lt;ds:DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/&gt;
 *       &lt;ds:DigestValue/&gt;
 *      &lt;/ds:Reference&gt;
 *     &lt;/ds:SignedInfo&gt;
 *     &lt;ds:SignatureValue/&gt;
 *    &lt;/ds:Signature&gt;
 *   </pre>
 *   <p>
 *    En este ejemplo, los datos firmados se encuentran en un servidor Web accesible p&uacute;blicamente:
 *    <cite>http://www.mpt.es/contenido</cite>, y se referencia como tal, conformando lo que se denomina
 *    <i>Externally Detached</i> o "Detached Externa".
 *   </p>
 *   <p>
 *    Cuando se desea firmar un contenido con un formato <i>Detached</i>, pero se quiere eliminar la
 *    dependencia de la disponibilidad externa del contenido firmado, es posible crear una estructura XML
 *    que contenga los propios contenidos y la firma, pero cada uno en una subestructura independiente,
 *    manteniendo asi el concepto de <i>Detached</i> (firma y contenido firmado no se interrelacionan
 *    directamente).  Para adecuarse al est&aacute;ndar los nodos de firma y contenido debe encontrarse en el
 *    mismo nivel dentro del XML.
 *   </p>
 *   <p>
 *    Un ejemplo de esta estructura XML ser&iacute;a:
 *   </p>
 *   <pre>
 *    &lt;?xml version="1.0" encoding="UTF-8"?&gt;
 *    &lt;internally-detached&gt;
 *     &lt;ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig#"&gt;
 *      &lt;ds:SignedInfo&gt;
 *       &lt;ds:CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/&gt;
 *       &lt;ds:SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/&gt;
 *       &lt;ds:Reference URI="#data"&gt;
 *         &lt;ds:DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/&gt;
 *         &lt;ds:DigestValue/&gt;
 *       &lt;/ds:Reference&gt;
 *      &lt;/ds:SignedInfo&gt;
 *      &lt;ds:SignatureValue/&gt;
 *     &lt;/ds:Signature&gt;
 *     &lt;document Id="data"&gt;
 *      &lt;title&gt;title&lt;/title&gt;
 *      &lt;author&gt;writer&lt;/author&gt;
 *      &lt;date&gt;today&lt;/date&gt;
 *      &lt;content&gt;
 *       &lt;para&gt;First paragraph&lt;/para&gt;
 *       &lt;para&gt;Second paragraph&lt;/para&gt;
 *      &lt;/content&gt;
 *     &lt;/document&gt;
 *    &lt;/internally-detached&gt;
 *   </pre>
 *   <p>
 *    En este caso, la estructura <i>internally-detached</i> contiene dos subestructuras, la firma (<i>Signature</i>) y el propio
 *    contenido firmado (<i>document</i>). La forma de relacionar ambos es, como ocurr&iacute;a en el primer ejemplo, con una URI,
 *    solo que en este caso es interna al documento XML, referenciando el identificador de la subestructura del contenido firmado
 *    (<i>data</i>).
 *   </p>
 *   <p>
 *    A esta variante de firma <i>Detached</i> se la conoce como <i>Internally Detached</i>, o "Detached Interna".
 *   </p>
 *   <p>
 *    Para unificar las superestructuras creadas dentro de un formato "Detached Interno", el Cliente @firma
 *    construye siempre el siguiente esqueleto XML:
 *   </p>
 *   <pre>
 *    &lt;CONTENT Id="id" Encoding="codificacion" MimeType="MimeType" Algorithm=""&gt;
 *     &lt;!  CONTENIDO FIRMADO --&gt;
 *    &lt;/CONTENT&gt;
 *   </pre>
 *   <p>
 *    Es decir, el contenido a firmar, ya sea XML o no-XML, se encapsula dentro de una etiqueta XML llamada
 *    CONTENT, en la que se indica la codificaci&oacute;n del contenido (UTF-8, Base64, etc.), el tipo de
 *    contenido (imagen JPEG, texto, XML, etc.) y el algoritmo utilizado para calcular la huella digital de
 *    este (por ejemplo, SHA-1).
 *   </p>
 *   <p>
 *    Como la superestructura es XML, si el contenido tambi&eacute;n es XML la inserci&oacute;n es directa
 *    (como en el primer ejemplo de "Detached Interna", pero si no es XML se codifica en Base64 antes de
 *    insertarse, resultando una estructura con una forma similar a la siguiente:
 *   </p>
 *   <pre>
 *    &lt;CONTENT Id="id" Encoding="Base64" MimeType="application/octect-stream" Algorithm=""&gt;
 *     SFGJKASGFJKASEGUYFGEYGEYRGADFJKASGDFSUYFGAUYEGWEYJGDFYKGYKGWJKEGYFWYJ=
 *    &lt;/CONTENT&gt;
 *   </pre>
 *   <p>
 *    La larga cadena de caracteres ser&iacute;a una codificaci&oacute;n Base64 del original interpretado en su
 *    forma binaria pura.
 *   </p>
 *  </dd>
 *  <dt><i>Enveloping</i></dt>
 *  <dd>
 *   <p>
 *    Otra variante de firma es la <i>Enveloping</i>, en la que la estructura XML de firma es la &uacute;nica
 *    en el documento de firma, y esta contiene internamente el contenido firmado (en un nodo propio).
 *   </p>
 *   <pre>
 *    &lt;?xml version="1.0" encoding="UTF-8"?&gt;
 *    &lt;ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig#"&gt;
 *     &lt;ds:SignedInfo&gt;
 *      &lt;ds:CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/&gt;
 *      &lt;ds:SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/&gt;
 *      &lt;ds:Reference URI="#obj"&gt;
 *       &lt;ds:DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/&gt;
 *       &lt;ds:DigestValue/&gt;
 *      &lt;/ds:Reference&gt;
 *     &lt;/ds:SignedInfo&gt;
 *     &lt;ds:SignatureValue/&gt;
 *     &lt;ds:Object Id="obj"&gt;SFGJKASGFJKASEGUYFGEYGEYRGADFJKASGDFSUYFG=&lt;/ds:Object&gt;
 *    &lt;/ds:Signature&gt;
 *   </pre>
 *   <p>
 *    En este caso, los datos firmados se encuentran en el nodo <i>Object</i>, referenciados
 *    internamente al XML mediante el identificador <i>obj</i>.
 *   </p>
 *   <p>
 *    Al igual que ocurr&iacute;a con el formato <i>Detached</i>, si los datos no son XML, no
 *    es posible insertarlos directamente dentro de una estructura XML, por lo que se codifican
 *    previamente en Base64.
 *   </p>
 *  </dd>
 *  <dt><i>Enveloped</i></dt>
 *  <dd>
 *   <p>
 *    Este formato de firma XMLDSig est&aacute; pensado para que un contenido XML pueda auto-contener
 *    su propia firma digital, insert&aacute;ndola en un nodo propio interno, por lo que, al contrario
 *    que en los formatos anteriores, no es posible firmar contenido que no sea XML.
 *   </p>
 *   <p>
 *    Un ejemplo simple del resultado de una firma <i>Enveloped</i> podr&iacute;a ser el siguiente:
 *   </p>
 *   <pre>
 *    &lt;!DOCTYPE Enveloped [
 *     &lt;!ENTITY ds "http://www.w3.org/2000/09/xmldsig#"&gt;
 *     &lt;!ENTITY c14n "http://www.w3.org/TR/2001/REC-xml-c14n-20010315"&gt;
 *     &lt;!ENTITY enveloped "http://www.w3.org/2000/09/xmldsig#enveloped-signature"&gt;
 *     &lt;!ENTITY xslt "http://www.w3.org/TR/1999/REC-xslt-19991116"&gt;
 *     &lt;!ENTITY digest "http://www.w3.org/2000/09/xmldsig#sha1"&gt;
 *    ]&gt;
 *    &lt;Letter&gt;
 *     &lt;Return-address&gt;address&lt;/Return-address&gt;
 *     &lt;To&gt;You&lt;/To&gt;
 *     &lt;Message&gt;msg body&lt;/Message&gt;
 *     &lt;From&gt;
 *      &lt;ds:Signature xmlns:ds="ds"&gt;
 *       &lt;ds:SignedInfo&gt;
 *        &lt;ds:CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/&gt;
 *        &lt;ds:SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/&gt;
 *        &lt;ds:Reference URI=""&gt;
 *         &lt;ds:Transforms&gt;
 *          &lt;ds:Transform Algorithm="enveloped"&gt;&lt;/ds:Transform&gt;
 *         &lt;/ds:Transforms&gt;
 *         &lt;ds:DigestMethod Algorithm="digest"/&gt;
 *         &lt;ds:DigestValue&gt;&lt;/ds:DigestValue&gt;
 *        &lt;/ds:Reference&gt;
 *       &lt;/ds:SignedInfo&gt;
 *       &lt;ds:SignatureValue/&gt;
 *      &lt;/ds:Signature&gt;
 *     &lt;/From&gt;
 *     &lt;Attach&gt;attachement&lt;/Attach&gt;
 *    &lt;/Letter&gt;
 *   </pre>
 *   <p>
 *    En este caso, el documento original (<i>Letter</i>), contiene internamente la estructura de firma digital
 *    (<i>Signature</i>).
 *   </p>
 *   <p>
 *    Una peculiaridad de la estructura generada es que esta referenciada mediante una URI vac&iacute;a
 *    (<code>URI=""</code>), lo cual indica que la firma aplica a la totalidad del documento original.
 *   </p>
 *  </dd>
 * </dl>
 * @version 0.3.1 */
public final class AOXAdESSigner implements AOSigner {

    static final Logger LOGGER = Logger.getLogger("es.agob.afirma"); //$NON-NLS-1$

    private static final String ID_IDENTIFIER = "Id"; //$NON-NLS-1$

    /** Etiqueta de los nodos firma de los XML firmados. */
    public static final String SIGNATURE_TAG = "Signature"; //$NON-NLS-1$

    /** URI que define la versi&oacute;n por defecto de XAdES. */
    static final String XADESNS = "http://uri.etsi.org/01903/v1.3.2#"; //$NON-NLS-1$

    /** URI que define el tipo de propiedades firmadas de XAdES (1.4.x). */
    static final String XADES_SIGNED_PROPERTIES_TYPE = "http://uri.etsi.org/01903#SignedProperties"; //$NON-NLS-1$

    /** URI que define una referencia de tipo MANIFEST. */
    static final String MANIFESTURI = "http://www.w3.org/2000/09/xmldsig#Manifest"; //$NON-NLS-1$

    static final String AFIRMA = "AFIRMA"; //$NON-NLS-1$
    static final String XML_SIGNATURE_PREFIX = "ds"; //$NON-NLS-1$
    static final String XADES_SIGNATURE_PREFIX = "xades"; //$NON-NLS-1$
    static final String SIGNATURE_NODE_NAME = XML_SIGNATURE_PREFIX + ":Signature"; //$NON-NLS-1$
    static final String DETACHED_CONTENT_ELEMENT_NAME = "CONTENT"; //$NON-NLS-1$
    static final String DETACHED_STYLE_ELEMENT_NAME = "STYLE"; //$NON-NLS-1$

    /** Algoritmo de huella digital por defecto para las referencias XML. */
    static final String DIGEST_METHOD = DigestMethod.SHA512;

    static final String STYLE_REFERENCE_PREFIX = "StyleReference-"; //$NON-NLS-1$

    static final String MIMETYPE_STR = "MimeType"; //$NON-NLS-1$
    static final String ENCODING_STR = "Encoding"; //$NON-NLS-1$

    static {
        Utils.installXmlDSigProvider();
    }

    /** Firma datos en formato XAdES.
     * <p>
     * Este m&eacute;todo, al firmar un XML, firmas tambi&eacute;n sus hojas de estilo XSL asociadas, siguiendo el siguiente criterio:
     * <ul>
     *  <li>Firmas XML <i>Enveloped</i>
     *   <ul>
     *    <li>
     *     Hoja de estilo con ruta relativa
     *     <ul>
     *      <li>No se firma.</li>
     *     </ul>
     *    </li>
     *    <li>
     *     Hola de estilo remota con ruta absoluta
     *     <ul>
     *      <li>Se restaura la declaraci&oacute;n de hoja de estilo tal y como estaba en el XML original</li>
     *      <li>Se firma una referencia (<i>canonicalizada</i>) a esta hoja remota</li>
     *     </ul>
     *    </li>
     *    <li>
     *     Hoja de estilo empotrada
     *     <ul>
     *      <li>Se restaura la declaraci&oacute;n de hoja de estilo tal y como estaba en el XML original</li>
     *     </ul>
     *    </li>
     *   </ul>
     *  </li>
     *  <li>
     *   Firmas XML <i>Externally Detached</i>
     *   <ul>
     *    <li>
     *     Hoja de estilo con ruta relativa
     *     <ul>
     *      <li>No se firma.</li>
     *     </ul>
     *    </li>
     *    <li>
     *     Hola de estilo remota con ruta absoluta
     *     <ul>
     *      <li>Se firma una referencia (<i>canonicalizada</i>) a esta hoja remota</li>
     *     </ul>
     *    </li>
     *    <li>
     *     Hoja de estilo empotrada
     *     <ul>
     *      <li>No es necesaria ninguna acci&oacute;n</li>
     *     </ul>
     *    </li>
     *   </ul>
     *  </li>
     *  <li>
     *   Firmas XML <i>Enveloping</i>
     *   <ul>
     *    <li>
     *     Hoja de estilo con ruta relativa
     *     <ul>
     *      <li>No se firma.</li>
     *     </ul>
     *    </li>
     *    <li>
     *     Hola de estilo remota con ruta absoluta
     *     <ul>
     *      <li>Se firma una referencia (<i>canonicalizada</i>) a esta hoja remota</li>
     *     </ul>
     *    </li>
     *    <li>
     *     Hoja de estilo empotrada
     *     <ul>
     *      <li>No es necesaria ninguna acci&oacute;n</li>
     *     </ul>
     *    </li>
     *   </ul>
     *  </li>
     *  <li>
     *   Firmas XML <i>Internally Detached</i>
     *   <ul>
     *    <li>
     *     Hoja de estilo con ruta relativa
     *     <ul>
     *      <li>No se firma.</li>
     *     </ul>
     *    </li>
     *    <li>
     *     Hola de estilo remota con ruta absoluta
     *     <ul>
     *      <li>Se firma una referencia (<i>canonicalizada</i>) a esta hoja remota</li>
     *     </ul>
     *    </li>
     *    <li>
     *     Hoja de estilo empotrada
     *     <ul>
     *      <li>No es necesaria ninguna acci&oacute;n</li>
     *     </ul>
     *    </li>
     *   </ul>
     *  </li>
     * </ul>
     * @param data Datos que deseamos firmar.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del cliente
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Firma en formato XAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] sign(final byte[] data,
                       final String algorithm,
                       final PrivateKey key,
                       final Certificate[] certChain,
                       final Properties xParams) throws AOException {
    	return XAdESSigner.sign(data, algorithm, key, certChain, xParams);
    }

    /** Comprueba si la firma es detached. Previamente debe haberse comprobado que el XML se
     * corresponde con una firma XAdES.
     * @param element
     *        Elemento que contiene el nodo ra&iacute;z del documento que se
     *        quiere comprobar
     * @return Valor booleano, siendo verdadero cuando la firma es detached */
    public static boolean isDetached(final Element element) {
        if (element == null) {
            return false;
        }

        try {
        	String dataNodeId = null;
        	final NodeList mainChildNodes = element.getChildNodes();
        	for (int i = 0; i < mainChildNodes.getLength(); i++) {
        		if (!mainChildNodes.item(i).getNodeName().equals(SIGNATURE_TAG)) {
        			dataNodeId = ((Element) mainChildNodes.item(i)).getAttribute(ID_IDENTIFIER);
        			break;
        		}
        	}
        	if (dataNodeId == null || dataNodeId.length() == 0) {
        		return false;
        	}

        	final NodeList transformList = element.getElementsByTagNameNS(XMLConstants.DSIGNNS, "Reference"); //$NON-NLS-1$
        	for (int i = 0; i < transformList.getLength(); i++) {
        		if (((Element) transformList.item(i)).getAttribute("URI").equals('#' + dataNodeId)) { //$NON-NLS-1$
        			return true;
        		}
        	}
        } catch (final Exception e) {
        	return false;
        }

        return false;
    }

    /** Comprueba si la firma es enveloped. Previamente debe haberse comprabado que el XML se
     * corresponde con una firma XAdES.
     * @param element
     *        Elemento que contiene el nodo ra&iacute;z del documento que se
     *        quiere comprobar
     * @return Valor booleano, siendo verdadero cuando la firma es enveloped */
    public static boolean isEnveloped(final Element element) {
        final NodeList transformList = element.getElementsByTagNameNS(XMLConstants.DSIGNNS, "Transform"); //$NON-NLS-1$
        for (int i = 0; i < transformList.getLength(); i++) {
            if (((Element) transformList.item(i)).getAttribute("Algorithm").equals(Transform.ENVELOPED)) { //$NON-NLS-1$
                return true;
            }
        }
        return false;
    }

    /** Comprueba si la firma es enveloping. Previamente debe haberse comprabado que el XML se
     * corresponde con una firma XAdES.
     * @param element
     *        Elemento que contiene el nodo ra&iacute;z del documento que se
     *        quiere comprobar
     * @return Valor booleano, siendo verdadero cuando la firma es enveloping */
    public static boolean isEnveloping(final Element element) {
        if (element.getLocalName().equals(SIGNATURE_TAG) ||
           element.getLocalName().equals(AFIRMA) && element.getFirstChild().getLocalName().equals(SIGNATURE_TAG)) {
            return true;
        }
        return false;
    }

    /** {@inheritDoc} */
    @Override
	public byte[] getData(final byte[] sign) throws AOInvalidFormatException {
        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        final Element rootSig;
        Element elementRes = null;
        try {

            // comprueba que sea una documento de firma valido
            if (!isSign(sign)) {
                throw new AOInvalidFormatException("El documento no es un documento de firmas valido."); //$NON-NLS-1$
            }

            // obtiene la raiz del documento de firmas
            rootSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign)).getDocumentElement();

            // si es detached
            if (AOXAdESSigner.isDetached(rootSig)) {

                final Element firstChild = (Element) rootSig.getFirstChild();
                // si el documento es un xml se extrae como tal
                if (firstChild.getAttribute(MIMETYPE_STR).equals("text/xml")) { //$NON-NLS-1$
                    elementRes = (Element) firstChild.getFirstChild();
                }
                // si el documento es binario se deshace la codificacion en
                // Base64 si y solo si esta declarada esta transformacion
                else {
                	//TODO: Deshacer solo el Base64 si existe la transformacion Base64 (COMPROBAR)
                	return isBase64TransformationDeclared(rootSig, firstChild.getAttribute(ID_IDENTIFIER)) ?
                				Base64.decode(firstChild.getTextContent()) :
                					firstChild.getTextContent().getBytes();
                }
            }

            // Si es enveloped
            else if (AOXAdESSigner.isEnveloped(rootSig)) {

            	removeEnvelopedSignatures(rootSig);

                elementRes = rootSig;
            }

            // Si es enveloping
            else if (AOXAdESSigner.isEnveloping(rootSig)) {

                // Obtiene el nodo Object de la primera firma
                final Element object = (Element) rootSig.getElementsByTagNameNS(XMLConstants.DSIGNNS, "Object").item(0); //$NON-NLS-1$
                // Si el documento es un xml se extrae como tal
                if (object.getAttribute(MIMETYPE_STR).equals("text/xml")) { //$NON-NLS-1$
                    elementRes = (Element) object.getFirstChild();
                }
                // Si el documento es binario se deshace la codificacion en
                // Base64 si y solo si esta declarada esta transformacion
                else {
                	//TODO: Deshacer solo el Base64 si existe la transformacion Base64 (COMPROBAR)
                	return isBase64TransformationDeclared(rootSig, object.getAttribute(ID_IDENTIFIER)) ?
                				Base64.decode(object.getTextContent()) :
                					object.getTextContent().getBytes();
                }
            }
        }
        catch (final Exception ex) {
            throw new AOInvalidFormatException("Error al leer el fichero de firmas: " + ex, ex); //$NON-NLS-1$
        }

        // si no se ha recuperado ningun dato se devuelve null
        if (elementRes == null) {
            return null;
        }

        // convierte el documento obtenido en un array de bytes
        final ByteArrayOutputStream baosSig = new ByteArrayOutputStream();
        XMLUtils.writeXML(baosSig, elementRes, false);

        return baosSig.toByteArray();
    }

    private void removeEnvelopedSignatures(final Element rootSig) {
        // obtiene las firmas y las elimina
    	final NodeList mainChildNodes = rootSig.getChildNodes();
    	for (int i = 0; i < mainChildNodes.getLength(); i++) {
    		if (mainChildNodes.item(i).getNodeType() == Node.ELEMENT_NODE &&
    				mainChildNodes.item(i).getNodeName().endsWith(":" + SIGNATURE_TAG)) { //$NON-NLS-1$
    			rootSig.removeChild(mainChildNodes.item(i));
    			removeEnvelopedSignatures(rootSig);
    			return;
    		}
        }
	}

	/** Comprueba si unos datos firmados tienen declarados una transformaci&oacute;n de tipo Base64.
     * @param rootSig Nodo raiz de la firma.
     * @param objectId Identificador de los datos.
     * @return {@code true} si la transformaci&oacute;n est&aacute; definida, {@code false}
     *         en caso contrario. */
    private static boolean isBase64TransformationDeclared(final Element rootSig, final String objectId) {
    	if (objectId == null || objectId.trim().equals("")) { //$NON-NLS-1$
    		return false;
    	}

    	Element reference = null;
		final NodeList references = rootSig.getElementsByTagNameNS(XMLConstants.DSIGNNS, "Reference"); //$NON-NLS-1$
		for (int i = 0; i < references.getLength(); i++) {
			reference = (Element) references.item(i);
			if (reference.hasAttribute("URI") && ("#" + objectId).equals(reference.getAttribute("URI"))) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				break;
			}
			reference = null;
		}
		if (reference != null) {
			final NodeList transforms = reference.getElementsByTagNameNS(XMLConstants.DSIGNNS, "Transform"); //$NON-NLS-1$
			for (int i = 0; i < transforms.getLength(); i++) {
				if (((Element) transforms.item(i)).hasAttribute("Algorithm") && //$NON-NLS-1$
						XMLConstants.BASE64_ENCODING.equals(((Element) transforms.item(i)).getAttribute("Algorithm"))) { //$NON-NLS-1$
					return true;
				}
			}
		}
		return false;
    }

    /** Cofirma datos en formato XAdES.
     * <p>
     *  Este m&eacute;todo firma todas las referencias a datos declaradas en la firma original,
     *  ya apunten estas a datos, hojas de estilo o cualquier otro elemento. En cada referencia
     *  firmada se introduciran las mismas transformaciones que existiesen en la firma original.
     * </p>
     * <p>
     *  A nivel de formato interno, cuando cofirmamos un documento ya firmado previamente, esta
     *  firma previa no se modifica. Si tenemos en cuenta que XAdES es en realidad un subconjunto
     *  de XMLDSig, el resultado de una cofirma XAdES sobre un documento firmado previamente con
     *  XMLDSig (o viceversa), son dos firmas independientes, una en XAdES y otra en XMLDSig.<br>
     *  Dado que todas las firmas XAdES son XMLDSig pero no todas las firmas XMLDSig son XAdES,
     *  el resultado global de la firma se adec&uacute;a al estandar mas amplio, XMLDSig en este caso.
     * </p>
     * @param data No se utiliza.
     * @param sign Documento con las firmas iniciales.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del cliente.
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Cofirma en formato XAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] cosign(final byte[] data,
                         final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final Certificate[] certChain,
                         final Properties xParams) throws AOException {
    	return XAdESCoSigner.cosign(sign, algorithm, key, certChain, xParams);
    }

    /** Cofirma datos en formato XAdES.
     * <p>
     *  Este m&eacute;todo firma todas las referencias a datos declaradas en la firma original,
     *  ya apunten estas a datos, hojas de estilo o cualquier otro elemento. En cada referencia
     *  firmada se introduciran las mismas transformaciones que existiesen en la firma original.
     * </p>
     * <p>
     *  A nivel de formato interno, cuando cofirmamos un documento ya firmado previamente, esta
     *  firma previa no se modifica. Si tenemos en cuenta que XAdES es en realidad un subconjunto
     *  de XMLDSig, el resultado de una cofirma XAdES sobre un documento firmado previamente con
     *  XMLDSig (o viceversa), son dos firmas independientes, una en XAdES y otra en XMLDSig.<br>
     *  Dado que todas las firmas XAdES son XMLDSig pero no todas las firmas XMLDSig son XAdES,
     *  el resultado global de la firma se adec&uacute;a al estandar mas amplio, XMLDSig en este caso.
     * </p>
     * @param sign Documento con las firmas iniciales.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Cofirma en formato XAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] cosign(final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final Certificate[] certChain,
                         final Properties extraParams) throws AOException {

    	if (!isSign(sign)) {
    		throw new AOInvalidFormatException("No se ha indicado una firma XAdES para cofirmar"); //$NON-NLS-1$
    	}

    	return XAdESCoSigner.cosign(sign, algorithm, key, certChain, extraParams);
    }

    /** Contrafirma firmas en formato XAdES.
     * <p>
     * Este m&eacute;todo contrafirma los nodos de firma indicados de un documento de firma.
     * </p>
     * @param sign Documento con las firmas iniciales.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param targetType Mecanismo de selecci&oacute;n de los nodos de firma que se deben
     * contrafirmar.
     * <p>Las distintas opciones son:</p>
     * <ul>
     * <li>Todos los nodos del &aacute;rbol de firma</li>
     * <li>Los nodos hoja del &aacute;rbol de firma</li>
     * <li>Los nodos de firma cuyas posiciones se especifican en <code>target</code></li>
     * <li>Los nodos de firma realizados por los firmantes cuyo <i>Common Name</i> se indica en <code>target</code></li>
     * </ul>
     * <p>Cada uno de estos tipos se define en {@link es.gob.afirma.core.signers.CounterSignTarget}.
     * @param targets Listado de nodos o firmantes que se deben contrafirmar seg&uacute;n el
     * {@code targetType} seleccionado.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Contrafirma en formato XAdES.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKey key,
                              final Certificate[] certChain,
                              final Properties xParams) throws AOException {
    	if (!isSign(sign)) {
    		throw new AOInvalidFormatException("No se ha indicado una firma XAdES para contrafirmar"); //$NON-NLS-1$
    	}

    	return XAdESCounterSigner.countersign(
			sign,
			algorithm,
			targetType,
			targets,
			key,
			certChain,
			xParams
		);
    }

    /** {@inheritDoc} */
    @Override
	public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) throws AOInvalidFormatException {

    	if (!isSign(sign)) {
    		throw new AOInvalidFormatException("Los datos indicados no son una firma XAdES compatible"); //$NON-NLS-1$
    	}

        // Obtenemos el arbol del documento
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        final Document signDoc;
        try {
            signDoc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));
        }
        catch (final Exception e) {
            LOGGER.warning("Se ha producido un error al obtener la estructura de firmas: " + e); //$NON-NLS-1$
            return null;
        }

        // Obtenemos todas las firmas del documento y el SignatureValue de cada
        // una de ellas
        final NodeList signatures = signDoc.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_TAG);

        // Mantendremos 3 listas: la de identificadores de firma, la de
        // identificadores a las
        // que referencia cada firma (cadena vacia salvo para las contrafirmas)
        // y los objetos
        // con los que representaremos cada uno de los nodos de firma.
        final List<String> arrayIds = new ArrayList<String>();
        final List<String> arrayRef = new ArrayList<String>();
        final List<AOTreeNode> arrayNodes = new ArrayList<AOTreeNode>();

        // Rellenamos cada las listas con los datos de las firmas del documento
        for (int i = 0; i < signatures.getLength(); i++) {
            final Element signature = (Element) signatures.item(i);

            // Recogemos el identificador del nodo de firma
            arrayIds.add(signature.getAttribute(ID_IDENTIFIER));

            // Recogemos los objetos que identificaran a los nodos de firma
            arrayNodes.add(new AOTreeNode(asSimpleSignInfo ? Utils.getSimpleSignInfoNode(Utils.guessXAdESNamespaceURL(signDoc.getDocumentElement()),
                                                                                       signature) : Utils.getStringInfoNode(signature)));

            // Recogemos el identificador de la firma a la que se referencia (si
            // no es contrafirma sera cadena vacia)
            if (signature.getParentNode().getNodeName().equals(XADES_SIGNATURE_PREFIX + ":CounterSignature")) { //$NON-NLS-1$
                arrayRef.add(Utils.getCounterSignerReferenceId(signature, signDoc.getElementsByTagNameNS(XMLConstants.DSIGNNS, "SignatureValue"))); //$NON-NLS-1$
            }
            else {
                arrayRef.add(""); //$NON-NLS-1$
            }
        }

        // Se crea el que sera el nodo raiz del arbol
        final AOTreeNode treeRoot = new AOTreeNode("Datos"); //$NON-NLS-1$

        // Se crea el arbol componiendo las subrama de cada firma directa de los
        // datos
        for (int i = 0; i < arrayRef.size(); i++) {
            if (arrayRef.get(i).equals("")) { //$NON-NLS-1$
                treeRoot.add(generateSignsTree(i, signatures.getLength() - 1, arrayNodes, arrayIds, arrayRef)[i]);
            }
        }
        return new AOTreeModel(treeRoot);
    }

    /** M&eacute;todo recursivo para la obtenci&oacute;n de la estructura de
     * &aacute;rbol
     * @param i Inicio de lectura del array de identificadores
     * @param j Inicio de lectura inversa del array de referencias
     * @param arrayNodes Array de objetos <code>TreeNode</code>
     * @param arrayIds Array de identificadores
     * @param arrayRef Array de referencias
     * @return Array de objetos <code>TreeNode</code> */
    private AOTreeNode[] generateSignsTree(final int i,
                                         final int j,
                                         final List<AOTreeNode> arrayNodes,
                                         final List<String> arrayIds,
                                         final List<String> arrayRef) {

        final int max = arrayIds.size();

        if (i < max && j > 0) {
            if (arrayIds.get(i).equals(arrayRef.get(j))) {
                generateSignsTree(i + 1, j - 1, arrayNodes, arrayIds, arrayRef);
            }

            if (i < j) {
                generateSignsTree(i, j - 1, arrayNodes, arrayIds, arrayRef);
            }

            if (!arrayIds.get(i).equals(arrayRef.get(j))) {
                return arrayNodes.toArray(new AOTreeNode[0]);
            }

            generateSignsTree(j, max - 1, arrayNodes, arrayIds, arrayRef);

            arrayNodes.get(i).add(arrayNodes.get(j));
        }

        return arrayNodes.toArray(new AOTreeNode[0]);
    }

    /** {@inheritDoc} */
    @Override
	public boolean isSign(final byte[] sign) {

        if (sign == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }

        try {
            // Carga el documento a validar
            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);

            // JXades no captura un nodo de firma si se pasa este como raiz del
            // arbol de firmas, asi
            // que nos vemos obligados a crear un nodo padre, del que colgara
            // todo el arbol de firmas,
            // para que lo detecte correctamente
            final Element rootNode = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign)).getDocumentElement();

            final List<Node> signNodes = new ArrayList<Node>();
            if (rootNode.getNodeName().equals(SIGNATURE_NODE_NAME)) {
                signNodes.add(rootNode);
            }

            final NodeList signatures = rootNode.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_TAG);
            for (int i = 0; i < signatures.getLength(); i++) {
                signNodes.add(signatures.item(i));
            }

            // Si no se encuentran firmas, no es un documento de firma
            if (signNodes.size() == 0 || !checkSignNodes(signNodes)) {
                return false;
            }
        }
        catch (final Exception e) {
            return false;
        }
        return true;
    }

    /** Comprueba que los nodos de firma proporcionados sean firmas en formato XAdES.
     * @param signNodes
     *        Listado de nodos de firma.
     * @return Devuelve {@code true} cuando todos los nodos sean firmas en este
     *         formato. */
    private static boolean checkSignNodes(final List<Node> signNodes) {
        String xadesNamespace;
        for (final Node signNode : signNodes) {
            xadesNamespace = Utils.guessXAdESNamespaceURL(signNode);
            if (((Element) signNode).getElementsByTagNameNS(xadesNamespace, "QualifyingProperties").getLength() == 0) { //$NON-NLS-1$
                return false;
            }
        }
        return true;
    }

    /** {@inheritDoc} */
    @Override
	public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return true;
    }

    /** {@inheritDoc} */
    @Override
	public String getSignedName(final String originalName, final String inText) {
        return originalName + (inText != null ? inText : "") + ".xsig"; //$NON-NLS-1$ //$NON-NLS-2$
    }

    /** Devuelve un nuevo documento con ra&iacute;z "AFIRMA" del que cuelga el
     * documento especificado.
     * @param docu Documento que estar&aacute; contenido en el nuevo documento.
     * @return Documento con ra&iacute;z "AFIRMA".
     * @throws ParserConfigurationException Cuando se produce un error al analizar el XML. */
    static Document insertarNodoAfirma(final Document docu) throws ParserConfigurationException {

        // Nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // Crea un nuevo documento con la raiz "AFIRMA"
        final Document docAfirma = dbf.newDocumentBuilder().newDocument();
        final Element rootAfirma = docAfirma.createElement(AFIRMA);
        rootAfirma.setAttributeNS(null, ID_IDENTIFIER, "AfirmaRoot-" + UUID.randomUUID().toString());  //$NON-NLS-1$

        // Inserta el documento pasado por parametro en el nuevo documento
        rootAfirma.appendChild(docAfirma.adoptNode(docu.getDocumentElement()));
        docAfirma.appendChild(rootAfirma);

        return docAfirma;
    }

    /** {@inheritDoc} */
    @Override
	public AOSignInfo getSignInfo(final byte[] sign) throws AOException {
        if (sign == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(sign)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }

        final AOSignInfo signInfo = new AOSignInfo(AOSignConstants.SIGN_FORMAT_XADES);

        // Analizamos mas en profundidad la firma para obtener el resto de datos

        // Tomamos la raiz del documento
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        Element rootSig = null;
        try {
            rootSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign)).getDocumentElement();
        }
        catch (final Exception e) {
            LOGGER.warning("Error al analizar la firma: " + e); //$NON-NLS-1$
            rootSig = null;
        }

        // Establecemos la variante de firma
        if (rootSig != null) {
            if (isDetached(rootSig)) {
                signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
            }
            else if (isEnveloped(rootSig)) {
                signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED);
            }
            else if (isEnveloping(rootSig)) {
                signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING);
            }
        }

        // Aqui vendria el analisis de la firma buscando alguno de los otros
        // datos de relevancia
        // que se almacenan en el objeto AOSignInfo

        return signInfo;
    }

}
