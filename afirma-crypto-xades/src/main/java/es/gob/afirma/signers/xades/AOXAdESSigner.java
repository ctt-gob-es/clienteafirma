/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades;

import java.io.ByteArrayInputStream;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.crypto.dsig.Transform;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.OptionalDataInterface;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;
import es.gob.afirma.signers.xml.XmlDSigProviderHelper;

/** Manejador de firmas XML XAdES
 * <p>Soporta XAdES-BES, XAdES-EPES y Baseline B-LEVEL.</p>
 * <p>
 *  Debido a errores en algunas versiones del entorno de ejecuci&oacute;n de Java, esta clase puede generar ocasionalmente
 *  mensajes en consola del tipo: <code>[Fatal Error] :1:1: Content is not allowed in prolog.</code>. Estos
 *  deben ignorarse, ya que no indican ninguna condici&oacute;n de error ni malfuncionamiento.
 * </p>
 * <p>
 *  Los atributos espec&iacute;ficos XAdES implementados por esta clase (adem&aacute;s de los
 *  relativos a las pol&iacute;ticas de firma) son:
 * </p>
 * <ul>
 *  <li><i>SigningTime</i></li>
 *  <li><i>SigningCerticate</i></li>
 *  <li><i>SigningCerticateV2</i></li>
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
 * </dl> */
public final class AOXAdESSigner implements AOSigner, OptionalDataInterface {

    static final Logger LOGGER = Logger.getLogger("es.agob.afirma"); //$NON-NLS-1$

    private static final String ID_IDENTIFIER = "Id"; //$NON-NLS-1$

    /** Etiqueta de los nodos de sello de tiempo. */
	private static final String TIMESTAMP_TAG = "Timestamp"; //$NON-NLS-1$

    /** Transformaci&oacute;n XPATH para eliminar todas las firmas de un XML. */
    private static final String XPATH_ENVELOPED_EQ = "not(ancestor-or-self::%1$s:Signature)"; //$NON-NLS-1$

    /** Transformaci&oacute;n XPATH equivalente a la transformaci&oacute;n enveloped. Elimina la firma actual. */
    private static final String XPATH_ENVELOPED_EQ2 = "count(ancestor-or-self::%1$s:Signature|here()/ancestor::%1$s:Signature[1])>count(ancestor-or-self::%1$s:Signature)"; //$NON-NLS-1$

    static final String DETACHED_CONTENT_ELEMENT_NAME = "CONTENT"; //$NON-NLS-1$
    static final String DETACHED_STYLE_ELEMENT_NAME = "STYLE"; //$NON-NLS-1$

    static final String XMLDSIG_ATTR_MIMETYPE_STR = "MimeType"; //$NON-NLS-1$
    static final String XMLDSIG_ATTR_ENCODING_STR = "Encoding"; //$NON-NLS-1$

    // Instalamos el proveedor de Apache. Esto es necesario para evitar problemas con los saltos de linea
    // de los Base 64
    static {
    	XmlDSigProviderHelper.configureXmlDSigProvider();
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

    	final Properties extraParams = getExtraParams(xParams);

		// La firma generada con el AOSigner se le pasa al sellador de tiempo,
		// pero este solo estampara un sello si asi se le ha indicado en los
		// parametros adicionales, no haciendo nada en caso contrario
    	return XAdESTspUtil.timestampXAdES(
    			XAdESSigner.sign(
    					data,
    					algorithm,
    					key,
    					certChain,
    					extraParams
    					),
    			extraParams
    			);
    }

    /** Comprueba si la firma es <i>internally detached</i>. Seg&uacute;n la definici&oacute;n del est&aacute;ndar:<br>
     * <p><b><i>Signature, Detached</i></b></p>
     * <p><i>The signature is over content external to the Signature element,
     * and can be identified via a URI or transform. Consequently, the signature is "detached" from
     * the content it signs. This definition typically applies to separate data objects, but it also
     * includes the instance where the Signature and data object reside within the same XML document
     * but are sibling elements.</i></p>
     * <p>En el caso que nos ocupa, la referencia sera a un nodo hermano a la propia firma. Es decir, un
     * nodo referenciado mediante un fragmento o transformaci&oacute;n pero que no se encuentre dentro
     * de la propia firma.</p>
     * <p>Este m&eacute;todo s&oacute;lo comprueba que sea <i>internally detached</i> la primera
     * firma encontrada en el XML.</p>
     * @param element Elemento que contiene el nodo ra&iacute;z del documento que se
     *                quiere comprobar
     * @return <code>true</code> si la firma es <i>detached</i>, <code>false</code> en caso contrario. */
    public static boolean isDetached(final Element element) {

    	// Obtenemos la primera firma encontrada en el elemento XML
    	final Element signatureElement = XAdESUtil.getFirstSignatureElement(element);

    	// Si no se encuentran firmas, no es una firma
    	if (signatureElement == null) {
    		return false;
    	}

    	// Obtenemos el listado de referencias a datos de la firma
    	final List<Element> dataReferenceList = XAdESUtil.getSignatureDataReferenceList(signatureElement);

    	return isSignatureElementInternallyDetached(element, dataReferenceList);
    }

    /**
     * Indica si la firma a la que pertenecen las referencias usadas es internally detached.
     * @param docElement Elemento ra&iacute;z del XML.
     * @param references Referencias a datos encontrados en la firma.
     * @return {@code true} si la firma es internally detached, {@code false} en caso contrario.
     */
    private static boolean isSignatureElementInternallyDetached(final Element docElement, final List<Element> references) {

    	if (docElement == null || references == null) {
    		return false;
    	}

    	// La consideraremos internally detached si alguno de los datos referenciados esta contenido dentro
    	// del XML padre, sin entrar en ninguna firma.
    	// NOTA: Una cofirma de una firma enveloping podria considerarse
    	// internally detached, ya que tendria una referencia a los datos contenidos en la otra firma, que es
    	// externa a ella misma. Se omite la busqueda en todas las firmas para omitir, ademas de otros posibles,
    	// este caso de uso
    	for (int i = 0; i < references.size(); i++) {
    		final String uri = references.get(i).getAttribute("URI"); //$NON-NLS-1$
    		if (uri != null && uri.startsWith("#")) { //$NON-NLS-1$
				final Node referencedNode = XAdESUtil.findElementById(uri.substring(1), docElement, true);
				// Si los datos referenciados estan contenidos dentro del XML padre y fuera de las firmas,
				// se trata de una firma internally detached
				if (referencedNode != null) {
					return true;
				}
    		}
    	}
    	// Se supone que los datos estarian dentro de alguna de las firmas o fuera del XML
    	return false;
	}

	/** Comprueba si la firma es <i>externally detached</i>. Seg&uacute;n la definici&oacute;n del est&aacute;ndar:<br>
     * <p><b><i>Signature, Detached</i></b></p>
     * <p><i>The signature is over content external to the Signature element,
     * and can be identified via a URI or transform. Consequently, the signature is "detached" from
     * the content it signs. This definition typically applies to separate data objects, but it also
     * includes the instance where the Signature and data object reside within the same XML document
     * but are sibling elements.</i></p>
     * <p>En el caso que nos ocupa, la referencia sera una referencia externa al XML. Es decir, un
     * nodo referenciado mediante una URI externa, por lo que se comprobar&aacute; si la URI utiliza
     * el esquema http o https. En caso afirmativo, se considerara una firma externally detached.</p>
     * <p>Este m&eacute;todo s&oacute;lo comprueba que sea <i>externally detached</i> la primera
     * firma encontrada en el XML.</p>
     * @param element Elemento que contiene el nodo ra&iacute;z del documento que se
     *                quiere comprobar
     * @return {@code true} si la firma es <i>externally detached</i>, {@code false}
     * en caso contrario. */
    public static boolean isExternallyDetached(final Element element) {
    	// Obtenemos la primera firma encontrada en el elemento XML
    	final Element signatureElement = XAdESUtil.getFirstSignatureElement(element);

    	// Si no se encuentran firmas, no es una firma
    	if (signatureElement == null) {
    		return false;
    	}

    	// Obtenemos el listado de referencias a datos de la firma
    	final List<Element> dataReferenceList = XAdESUtil.getSignatureDataReferenceList(signatureElement);

    	return isSignatureElementExternallyDetached(dataReferenceList);
    }

    /**
     * Indica si la firma a la que pertenecen las referencias usadas es externally detached.
     * @param references Referencias a datos encontrados en la firma.
     * @return {@code true} si la firma es externally detached, {@code false} en caso contrario.
     */
    private static boolean isSignatureElementExternallyDetached(final List<Element> references) {

    	if (references == null) {
    		return false;
    	}

    	// La consideraremos externally detached si se encuentra una referencia que utilice el
    	// esquema http o https
    	for (int i = 0; i < references.size(); i++) {
    		final String uri = references.get(i).getAttribute("URI"); //$NON-NLS-1$
    		if (uri != null && (uri.toLowerCase(Locale.US).startsWith("http://") || //$NON-NLS-1$
    				uri.toLowerCase(Locale.US).startsWith("https://"))) { //$NON-NLS-1$
    			return true;
    		}
    	}
    	return false;
	}

    /** Comprueba si una firma de manifest. Seg&uacute;n la definici&oacute;n del est&aacute;ndar:<br>
     * <p><b><i>Signature, Detached</i></b></p>
     * <p><i>The signature is over content external to the Signature element,
     * and can be identified via a URI or transform. Consequently, the signature is "detached" from
     * the content it signs. This definition typically applies to separate data objects, but it also
     * includes the instance where the Signature and data object reside within the same XML document
     * but are sibling elements.</i></p>
     * <p>Este m&eacute;todo s&oacute;lo comprueba que sea una firma <i>manifest</i> la primera
     * firma encontrada en el XML.</p>
     * @param element Elemento que contiene el nodo ra&iacute;z del documento que se
     *                quiere comprobar
     * @return {@code true} si es una firma con <i>manifest</i>, {@code false} en caso contrario. */
    public static boolean isManifestSignature(final Element element) {
    	// Obtenemos la primera firma encontrada en el elemento XML
    	final Element signatureElement = XAdESUtil.getFirstSignatureElement(element);

    	// Si no se encuentran firmas, no es una firma
    	if (signatureElement == null) {
    		return false;
    	}

    	// Obtenemos el listado de referencias a datos de la firma
    	final List<Element> dataReferenceList = XAdESUtil.getSignatureDataReferenceList(signatureElement);

    	return isSignatureWithManifest(dataReferenceList);
    }

    /**
     * Indica si la firma utiliza un manifest para referenciar datos.
     * @param references Referencias a datos encontrados en la firma.
     * @return {@code true} si es una firma con manifest, {@code false} en caso contrario.
     */
    static boolean isSignatureWithManifest(final List<Element> references) {

    	if (references == null) {
    		return false;
    	}

    	// La consideraremos firma con manifest si alguna de las referencias que firma esta
    	// declarada como de tipo manifest
    	for (int i = 0; i < references.size(); i++) {
    		final String type = references.get(i).getAttribute("Type"); //$NON-NLS-1$
    		if (type != null && type.equals(XAdESConstants.REFERENCE_TYPE_MANIFEST)) {
    			return true;
    		}
    	}
    	return false;
	}

	/** Comprueba si la firma es <i>enveloped</i>. Seg&uacute;n la definici&oacute;n del est&aacute;ndar:<br>
     * <p><b><i>Signature, Enveloped</i></b></p>
     * <p><i>The signature is over the XML content that contains the signature as an element. The
     * content provides the root XMLdocument element. Obviously, enveloped signatures must take
     * care not to include their own value in the calculation of the SignatureValue.</i></p>
     * <p>En la pr&aacute;ctica, se evaluara que la URI a los datos este vac&iacute;a (lo que,
     * seg&uacute;n el est&aacute;ndar "<i>Identifies the node-set (minus any comment nodes)
     * of theXML resource containing the signature</i>") y que se declare la transformaci&oacute;n
     * enveloped o una de las transformacines XPATH sugeridas en el est&aacute;ndar:</p>
     * <p>{@code not(ancestor-or-self::dsig:Signature)}</p>
     * <p>o</p>
     * <p>{@code count(ancestor-or-self::dsig:Signature |
     * here()/ancestor::dsig:Signature[1]) >
     * count(ancestor-or-self::dsig:Signature) }</p>
     * <p>Este m&eacute;todo s&oacute;lo comprueba que sea <i>enveloped</i> la primera
     * firma encontrada en el XML.</p>
     * @param element Elemento que contiene el nodo ra&iacute;z del documento que se
     *        	      quiere comprobar
     * @return <code>true</code> cuando la firma es <i>enveloped</i>, <code>false</code> en caso
     *         contrario. */
    public static boolean isEnveloped(final Element element) {
    	// Obtenemos la primera firma encontrada en el elemento XML
    	final Element signatureElement = XAdESUtil.getFirstSignatureElement(element);

    	// Si no se encuentran firmas, no es una firma
    	if (signatureElement == null) {
    		return false;
    	}

    	// Obtenemos el listado de referencias a datos de la firma
    	final List<Element> dataReferenceList = XAdESUtil.getSignatureDataReferenceList(signatureElement);

    	return isSignatureElementEnveloped(signatureElement, dataReferenceList);
    }

    /**
     * Comprueba a trav&eacute;s de las referencias a datos declaradas en una firma si se
     * trata de una firma enveloped.
     * @param signatureElement Elemento con la firma a comprobar.
     * @param references Referencias a datos declaradas en la firma.
     * @return {@code true} si es una firma enveloped, {@code false} en caso contrario.
     */
    static boolean isSignatureElementEnveloped(final Element signatureElement, final List<Element> references) {

    	if (references == null) {
    		return false;
    	}

    	// La consideraremos enveloping si alguno de los datos referenciados esta contenido dentro de
    	// la propia firma.
    	for (int i = 0; i < references.size(); i++) {
    		final NodeList transformList = references.get(i).getElementsByTagNameNS(XMLConstants.DSIGNNS, "Transform"); //$NON-NLS-1$
    		for (int j = 0; j < transformList.getLength(); j++) {
    			final String algorithm = ((Element) transformList.item(j)).getAttribute("Algorithm"); //$NON-NLS-1$
    			if (Transform.ENVELOPED.equals(algorithm)) {
    				return true;
    			}
    			else if (Transform.XPATH.equals(algorithm)) {
    				final String signaturePrefix = signatureElement.getPrefix();
    				final String xPath = transformList.item(j).getTextContent().replaceAll("\\s+", ""); //$NON-NLS-1$ //$NON-NLS-2$
    				if (String.format(XPATH_ENVELOPED_EQ, signaturePrefix).equals(xPath) ||
    						String.format(XPATH_ENVELOPED_EQ2, signaturePrefix).equals(xPath)) {
    					return true;
    				}
    			}
    		}
    	}
    	return false;
    }

    /** Comprueba si la firma es <i>enveloping</i>. Seg&uacute;n la definici&oacute;n del est&aacute;ndar:<br>
     * <p><b><i>Signature, Enveloping</i></b></p>
     * <p><i>The signature is over content found within an Object element of the signature itself. The
     * Object (or its content) is identified via a Reference (via a URI fragment identifier or transform).</i>
     * <p>En la pr&aacute;ctica, comprobaremos que la firma se encuentra en un nodo "object" contenido en
     * la firma.</p>
     * <p>Este m&eacute;todo s&oacute;lo comprueba que sea <i>enveloping</i> la primera
     * firma encontrada en el XML. Tengase en cuenta, que una firma Manifest ser&aacute; considerada
     * enveloping cuando el manifest se encuentre dentro de la propia firma.</p>
     * @param element Elemento que contiene el nodo ra&iacute;z del documento que se
     *                quiere comprobar.
     * @return <code>true</code> cuando la firma es <i>enveloping</i>, <code>false</code> en caso
     *         contrario. */
    public static boolean isEnveloping(final Element element) {

    	// Obtenemos la primera firma encontrada en el elemento XML
    	final Element signatureElement = XAdESUtil.getFirstSignatureElement(element);

    	// Si no se encuentran firmas, no es una firma
    	if (signatureElement == null) {
    		return false;
    	}

    	// Obtenemos el listado de referencias a datos de la firma
    	final List<Element> dataReferenceList = XAdESUtil.getSignatureDataReferenceList(signatureElement);

    	return isSignatureElementEnveloping(signatureElement, dataReferenceList);
    }

    /**
     * Comprueba a trav&eacute;s de las referencias a datos declaradas en una firma si se
     * trata de una firma enveloping.
     * @param signatureElement Elemento con la firma a comprobar.
     * @param references Referencias a datos declaradas en la firma.
     * @return {@code true} si es una firma enveloping, {@code false} en caso contrario.
     */
    static boolean isSignatureElementEnveloping(final Element signatureElement, final List<Element> references) {

    	if (signatureElement == null || references == null) {
    		return false;
    	}

    	// La consideraremos enveloping si alguno de los datos referenciados esta contenido dentro de
    	// la propia firma.
    	for (int i = 0; i < references.size(); i++) {
    		final String uri = references.get(i).getAttribute("URI"); //$NON-NLS-1$
    		if (uri != null && uri.startsWith("#")) { //$NON-NLS-1$
				final Node referencedNode = XAdESUtil.findElementById(uri.substring(1), signatureElement, false);
				// Si los datos referenciados estan contenidos en la firma, se trata de una firma enveloping
				if (referencedNode != null) {
					return true;
				}
    		}
    	}
    	return false;
    }

    /** {@inheritDoc} */
    @Override
	public byte[] getData(final byte[] sign) throws AOInvalidFormatException {

    	// Construimos el arbol DOM
        Document doc;
        try {
            doc = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign));
        }
        catch (final Exception ex) {
            throw new AOInvalidFormatException("Error al leer el fichero de firmas: " + ex, ex); //$NON-NLS-1$
        }

        return getData(doc);
    }

    //TODO: Mejorar el proceso de extraccion de datos para que se extraigan a partir de las referencias
    // y no dando por hecho la distribucion de la firma

    /** Recupera los datos originalmente firmados de una firma.
     * En el caso de que la firma no contenga los datos firmados, se
     * devuelve <code>null</code>.
     * @param signDocument Documento XML de firma.
     * @return Datos originalmente firmados.
     * @throws es.gob.afirma.core.AOInvalidFormatException
     *         Si no se ha introducido un fichero de firma v&aacute;lido o no
     *         ha podido leerse la firma. */
    public static byte[] getData(final Document signDocument) throws AOInvalidFormatException {

        // Comprueba que sea una documento de firma valido
        if (!isSign(signDocument)) {
            throw new AOInvalidFormatException("El documento no es un documento de firmas valido."); //$NON-NLS-1$
        }

        // Obtenemos la primera firma encontrada en el elemento XML
    	final Element signatureElement = XAdESUtil.getFirstSignatureElement(signDocument.getDocumentElement());

    	// Si no se encuentran firmas, no se recuperan los datos
    	if (signatureElement == null) {
    		return null;
    	}

    	// Obtenemos el listado de referencias a datos de la firma
    	final List<Element> dataReferenceList = XAdESUtil.getSignatureDataReferenceList(signatureElement);

        Element elementRes;
        try {
            // Obtiene la raiz del documento de firmas
            final Element docElement = signDocument.getDocumentElement();

            // Si la firma es externally detached o de tipo Manifest, consideramos que los datos
            // son externos y no se devolveran. Esto se hace asi por seguridad, incluso si se
            // pudiese acceder a los datos  traves de URLs externas
            if (isSignatureElementExternallyDetached(dataReferenceList) ||
            		isSignatureWithManifest(dataReferenceList)) {
            	elementRes = null;
            }

            // Si es enveloped
            else if (isSignatureElementEnveloped(signatureElement, dataReferenceList)) {

            	removeEnvelopedSignatures(docElement);

                elementRes = docElement;
            }

            // Si es internally detached
            else if (isSignatureElementInternallyDetached(docElement, dataReferenceList)) {

                final Element firstChild = (Element) docElement.getFirstChild();

                // Si el contenido firmado es un nodo de texto, lo extramos como tal
                if (firstChild.getFirstChild().getNodeType() == Node.TEXT_NODE) {
                	// Si existe una transformacion Base64, la deshacemos
                	return isBase64TransformationDeclared(docElement, firstChild.getAttribute(ID_IDENTIFIER)) ?
        				Base64.decode(firstChild.getTextContent()) :
        					firstChild.getTextContent().getBytes();
                }
                // Si no era un nodo de texto, se considera que es XML
                elementRes = (Element) firstChild.getFirstChild();
            }

            // Si es enveloping y no es manifest (porque de estas ultimas no podemos extraer los datos)
            else if (isSignatureElementEnveloping(signatureElement, dataReferenceList)) {

                // Obtiene el nodo Object de la primera firma
                final Element object = (Element) docElement.getElementsByTagNameNS(XMLConstants.DSIGNNS, "Object").item(0); //$NON-NLS-1$
                // Si el documento es un xml se extrae como tal
                if (object.getAttribute(XMLDSIG_ATTR_MIMETYPE_STR).equals("text/xml")) { //$NON-NLS-1$
                    elementRes = (Element) object.getFirstChild();
                }
                // Si el documento es binario se deshace la codificacion en
                // Base64 si y solo si esta declarada esta transformacion
                else {
                	// Se deshace el Base64 si existe la transformacion Base64
                	return isBase64TransformationDeclared(docElement, object.getAttribute(ID_IDENTIFIER)) ?
        				Base64.decode(object.getTextContent()) :
        					object.getTextContent().getBytes();
                }
            }
            // No se ha podido identificar el tipo de firma
            else {
            	elementRes = null;
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
        return Utils.writeXML(elementRes, null, null, null);
    }

    private static void removeEnvelopedSignatures(final Element rootSig) {
        // obtiene las firmas y las elimina
    	final NodeList mainChildNodes = rootSig.getChildNodes();
    	for (int i = 0; i < mainChildNodes.getLength(); i++) {
    		if (mainChildNodes.item(i).getNodeType() == Node.ELEMENT_NODE &&
    				mainChildNodes.item(i).getLocalName().equals(XMLConstants.TAG_SIGNATURE)) {
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

    		return cosign(sign, algorithm, key, certChain, xParams);
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

    	Document signDocument;
    	try {
    		signDocument = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign));
    	}
    	catch (final Exception e) {
    		throw new AOInvalidFormatException("No se ha podido cargar el documento XML de firmas", e); //$NON-NLS-1$
    	}

    	final Properties newExtraParams = getExtraParams(extraParams);

    	return cosign(signDocument, algorithm, key, certChain, newExtraParams);
    }

    /** Cofirma datos en formato XAdES.
     * @param signDocument Documento XML con las firmas iniciales.
     * @param algorithm Algoritmo a usar para la firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Cofirma en formato XAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	private static byte[] cosign(final Document signDocument,
                         final String algorithm,
                         final PrivateKey key,
                         final Certificate[] certChain,
                         final Properties extraParams) throws AOException {

		if (!isSign(signDocument)) {
			throw new AOInvalidFormatException("No se ha indicado una firma XAdES para cofirmar"); //$NON-NLS-1$
		}

		//Se comprueba si las firmas que ya contiene el documento tienen una version valida
		if (!checkSignVersion(signDocument)) {
			throw new AOFormatFileException("Error al analizar si el XML era una firma XAdES"); //$NON-NLS-1$
		}

		return XAdESCoSigner.cosign(signDocument, algorithm, key, certChain, extraParams);
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
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Contrafirma en formato XAdES.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKey key,
                              final Certificate[] certChain,
                              final Properties extraParams) throws AOException {

    	Document signDocument;
    	try {
    		signDocument = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign));
    	}
    	catch (final Exception e) {
    		throw new AOInvalidFormatException("No se ha podido cargar el documento XML de firmas", e); //$NON-NLS-1$
    	}

    	final Properties newExtraParams = getExtraParams(extraParams);

    	return countersign(signDocument, algorithm, targetType, targets, key, certChain, newExtraParams);
    }

    /** Contrafirma firmas en formato XAdES.
     * @param signDocument Documento XML con las firmas iniciales.
     * @param algorithm Algoritmo a usar para la firma.
     * @param targetType Mecanismo de selecci&oacute;n de los nodos de firma que se deben
     * contrafirmar.
     * @param targets Listado de nodos o firmantes que se deben contrafirmar seg&uacute;n el
     * {@code targetType} seleccionado.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Contrafirma en formato XAdES.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	private static byte[] countersign(final Document signDocument,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKey key,
                              final Certificate[] certChain,
                              final Properties extraParams) throws AOException {
	    	if (!isSign(signDocument)) {
	    		throw new AOInvalidFormatException("No se ha indicado una firma XAdES para contrafirmar"); //$NON-NLS-1$
	    	}

	    	return XAdESCounterSigner.countersign(
	    			signDocument,
	    			algorithm,
	    			targetType,
	    			targets,
	    			key,
	    			certChain,
	    			extraParams
	    			);
    }

    /** {@inheritDoc} */
    @Override
	public AOTreeModel getSignersStructure(final byte[] sign,
			                               final boolean asSimpleSignInfo) throws AOInvalidFormatException {

        // Obtenemos el arbol del documento
        final Document signDoc;
        try {
            signDoc = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign));
        }
        catch (final Exception e) {
            LOGGER.warning("Se ha producido un error al obtener la estructura de firmas: " + e); //$NON-NLS-1$
            return null;
        }

        return getSignersStructure(signDoc, asSimpleSignInfo);
    }

    /** Recupera el &aacute;rbol de nodos de firma de una firma electr&oacute;nica.
     * Los nodos del &aacute;rbol ser&aacute;n cadena de texto con el <i>CommonName</i> (CN X.500)
     * del titular del certificado u objetos de tipo <code>AOSimpleSignInfo</code> con la
     * informaci&oacute;n b&aacute;sica de las firmas individuales, seg&uacute;n
     * el valor del par&aacute;metro <code>asSimpleSignInfo</code>. Los nodos se
     * mostrar&aacute;n en el mismo orden y con la misma estructura con el que
     * aparecen en la firma electr&oacute;nica.<br>
     * La propia estructura de firma se considera el nodo ra&iacute;z, la firma y cofirmas
     * pender&aacute;n directamentede de este.
     * @param signDocument Documento XML de la firma de la que se desea obtener la estructura.
     * @param asSimpleSignInfo
     *        Si es <code>true</code> se devuelve un &aacute;rbol con la
     *        informaci&oacute;n b&aacute;sica de cada firma individual
     *        mediante objetos <code>AOSimpleSignInfo</code>, si es <code>false</code> un &aacute;rbol con los nombres (CN X.500) de los
     *        titulares de los certificados.
     * @return &Aacute;rbol de nodos de firma o <code>null</code> en caso de error.
     * @throws AOInvalidFormatException
     *         Si no se ha introducido un fichero de firma v&aacute;lido del formato correspondiente. */
    public static AOTreeModel getSignersStructure(final Document signDocument, final boolean asSimpleSignInfo)
    		throws AOInvalidFormatException {

    	if (!isSign(signDocument)) {
    		throw new AOInvalidFormatException(
    				"Los datos indicados no son una firma XAdES compatible" //$NON-NLS-1$
    				);
    	}

    	// Obtenemos todas las firmas del documento y el SignatureValue de cada
    	// una de ellas
    	final NodeList signatures = signDocument.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);

    	// Mantendremos 3 listas: la de identificadores de firma, la de identificadores a las
    	// que referencia cada firma (cadena vacia salvo para las contrafirmas) y los objetos
    	// con los que representaremos cada uno de los nodos de firma.
    	final List<String> arrayIds = new ArrayList<>();
    	final List<String> arrayRef = new ArrayList<>();
    	final List<AOTreeNode> arrayNodes = new ArrayList<>();

    	// Rellenamos cada las listas con los datos de las firmas del documento
    	for (int i = 0; i < signatures.getLength(); i++) {
    		final Element signature = (Element) signatures.item(i);

    		// Recogemos el identificador del nodo de firma
    		arrayIds.add(signature.getAttribute(ID_IDENTIFIER));

    		// Recogemos los objetos que identificaran a los nodos de firma
    		if (asSimpleSignInfo) {
    			String xadesNamespace = null;
    			final Element signatureElement = XAdESUtil.getFirstSignatureElement(signDocument.getDocumentElement());
    			final Element signedProperties = XAdESUtil.getSignedPropertiesElement(signatureElement);
    			if (signedProperties != null) {
    				xadesNamespace = signedProperties.getNamespaceURI();
    			}
    			arrayNodes.add(new AOTreeNode(Utils.getSimpleSignInfoNode(xadesNamespace, signature)));
    		}
    		else {
    			arrayNodes.add(new AOTreeNode(Utils.getStringInfoNode(signature)));
    		}

    		// Recogemos el identificador de la firma a la que se referencia (si
    		// no es contrafirma sera cadena vacia)
    		if (signature.getParentNode() != null && "CounterSignature".equals(signature.getParentNode().getLocalName())) { //$NON-NLS-1$
    			arrayRef.add(Utils.getCounterSignerReferenceId(signature, signDocument.getElementsByTagNameNS(XMLConstants.DSIGNNS, "SignatureValue"))); //$NON-NLS-1$
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
    private static AOTreeNode[] generateSignsTree(final int i,
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

        Document signDocument;
        try {
        	final DocumentBuilder docBuilder = Utils.getNewDocumentBuilder();
            signDocument = docBuilder.parse(new ByteArrayInputStream(sign));
        }
        catch (final Exception e) {
        	LOGGER.log(Level.INFO, "El documento no es un XML"); //$NON-NLS-1$
            return false;
        }
        return isSign(signDocument);
    }

    public static boolean isSign(final Document signDocument) {

        if (signDocument == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }

        try {
            final Element rootNode = signDocument.getDocumentElement();
            final List<Node> signNodes = new ArrayList<>();

            // Comprobamos si el nodo raiz es una firma
            if (rootNode.getLocalName().equals(XMLConstants.TAG_SIGNATURE)) {
                signNodes.add(rootNode);
            }

            // Identificamos las firmas internas del XML
            final NodeList signatures = rootNode.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);
            for (int i = 0; i < signatures.getLength(); i++) {
            	// Omitimos las firmas extraidas de sellos de tiempo
            	final Node parentNode = signatures.item(i).getParentNode();
            	if (parentNode == null || !TIMESTAMP_TAG.equals(parentNode.getLocalName())) {
            		signNodes.add(signatures.item(i));
            	}
            }

            // Si no se encuentran firmas o si estas no son firmas XAdES, entonces este no sera
            // un documento de firma XAdES
            if (signNodes.size() == 0 || !XAdESUtil.checkSignNodes(signNodes)) {
                return false;
            }
        }
        catch (final Exception e) {
        	LOGGER.log(Level.WARNING, "Error al analizar si el XML era una firma XAdES", e); //$NON-NLS-1$
            return false;
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

        // Crea un nuevo documento con la raiz "AFIRMA"
        final Document docAfirma = Utils.getNewDocumentBuilder().newDocument();
        final Element rootAfirma = docAfirma.createElement(XAdESConstants.TAG_PARENT_NODE);
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

        // Cargamos el arbol DOM del documento
        Document signDocument;
        try {
        	signDocument = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign));
        }
        catch (final Exception e) {
            LOGGER.warning("Error al analizar la firma: " + e); //$NON-NLS-1$
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un documento XML", e); //$NON-NLS-1$
        }

        return getSignInfo(signDocument);
    }

    /** Obtiene la informaci&oacute;n general de un objeto de firma. Ya que un objeto de
     * firma puede contener muchas firmas, se considera informaci&oacute;n
     * general la com&uacute;n que aplique a todo el objeto.
     * @param signDocument
     *        Firma XAdES que se desea analizar.
     * @return Informaci&oacute;n sobre la firma electr&oacute;nica.
     * @throws AOException
     *         Ocurri&oacute; un error durante la recuperaci&oacute;n de los
     *         datos. */
    public static AOSignInfo getSignInfo(final Document signDocument) throws AOException {
        if (signDocument == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(signDocument)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }

        // Tomamos la raiz del documento
        final Element rootSig = signDocument.getDocumentElement();

        // Componemos el objeto con la informacion de firma
        final AOSignInfo signInfo = new AOSignInfo(AOSignConstants.SIGN_FORMAT_XADES);

        // Identificamos el tipo de la firma por medio de las referencias de la primera de ellas
    	final Element signatureElement = XAdESUtil.getFirstSignatureElement(signDocument.getDocumentElement());

    	// Obtenemos el listado de referencias a datos de la firma
    	final List<Element> dataReferenceList = XAdESUtil.getSignatureDataReferenceList(signatureElement);

        // Establecemos la variante de firma
    	if (isSignatureElementEnveloped(signatureElement, dataReferenceList)) {
        	signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED);
        }
        else if (isSignatureElementExternallyDetached(dataReferenceList)) {
        	signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED);
        }
        else if (isSignatureElementInternallyDetached(rootSig, dataReferenceList)) {
        	signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
        }
        else if (isSignatureElementEnveloping(signatureElement, dataReferenceList)) {
        	signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING);
        }

        // Aqui vendria el analisis de la firma buscando alguno de los otros
        // datos de relevancia
        // que se almacenan en el objeto AOSignInfo

        return signInfo;
    }

    @Override
    public boolean needData(final Properties config) {

    	// Sera obligatorio que se indiquen los datos de entrada siempre que el formato no sea
    	// Externally Detached y no se trate de una firma manifest
    	return !AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED.equals(config.getProperty(XAdESExtraParams.FORMAT)) &&
    			!Boolean.parseBoolean(config.getProperty(XAdESExtraParams.USE_MANIFEST));
    }

    private static Properties getExtraParams(final Properties extraParams) {
    	final Properties newExtraParams = extraParams != null ?
    			(Properties) extraParams.clone() : new Properties();

    	// Eliminamos configuraciones que no deseemos que se utilicen desde el exterior
    	newExtraParams.remove(XAdESExtraParams.INTERNAL_VALIDATE_PKCS1);

    	return newExtraParams;
    }

    /**
     * Comprueba si el documento a cofirmar, contiene firmas de una versi&oacute;n distinta
     * a la 1.3.2.
     * @param signDocument Documento que contiene las firmas y datos.
     * @return Devuelve true en caso de que la versi&oacute;n sea correcta o false en caso
     * de que se encuentre alguna firma con una versi&oacute;n no soportada.
     * @throws AOFormatFileException
     */
    public static boolean checkSignVersion(final Document signDocument) throws AOFormatFileException {

        try {
            final Element rootNode = signDocument.getDocumentElement();
            final List<Node> signNodes = new ArrayList<>();

            // Comprobamos si el nodo raiz es una firma
            if (rootNode.getLocalName().equals(XMLConstants.TAG_SIGNATURE)) {
                signNodes.add(rootNode);
            }

            // Identificamos las firmas internas del XML
            final NodeList signatures = rootNode.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);
            for (int i = 0; i < signatures.getLength(); i++) {
            	// Omitimos las firmas extraidas de sellos de tiempo
            	final Node parentNode = signatures.item(i).getParentNode();
            	if (parentNode == null || !TIMESTAMP_TAG.equals(parentNode.getLocalName())) {
            		signNodes.add(signatures.item(i));
            	}
            }

            XAdESUtil.checkSignVersion(signNodes);
        }
        catch (final AOFormatFileException aoffe) {
        	throw aoffe;
        }
        catch (final Exception e) {
        	LOGGER.log(Level.WARNING, "Error al analizar si el XML era una firma XAdES", e); //$NON-NLS-1$
            return false;
        }
        return true;
    }
}
