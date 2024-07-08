/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation,
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xmldsig;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URI;
import java.security.InvalidAlgorithmParameterException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.crypto.dsig.spec.XPathFilterParameterSpec;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.SAXException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xml.InvalidXMLException;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;
import es.gob.afirma.signers.xml.XmlDSigProviderHelper;
import es.gob.afirma.signers.xml.style.CannotDereferenceException;
import es.gob.afirma.signers.xml.style.IsInnerlException;
import es.gob.afirma.signers.xml.style.ReferenceIsNotXmlException;
import es.gob.afirma.signers.xml.style.XmlStyle;
import es.uji.crypto.xades.jxades.util.DOMOutputImpl;

/** Manejador de firmas XML en formato XMLDSig.
 * @version 0.2 */
public final class AOXMLDSigSigner implements AOSigner {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** URI que define la versi&oacute;n por defecto de XAdES. */
    private static final String XADESNS = "http://uri.etsi.org/01903#"; //$NON-NLS-1$

    private static final String MIMETYPE_STR = "MimeType"; //$NON-NLS-1$
    private static final String ENCODING_STR = "Encoding"; //$NON-NLS-1$
    private static final String REFERENCE_STR = "Reference"; //$NON-NLS-1$

    private static final String ID_IDENTIFIER = "Id"; //$NON-NLS-1$

    private static final String HTTP_PROTOCOL_PREFIX = "http://"; //$NON-NLS-1$
    private static final String HTTPS_PROTOCOL_PREFIX = "https://"; //$NON-NLS-1$

    private static final String STYLE_REFERENCE_PREFIX = "StyleReference-"; //$NON-NLS-1$

    private static final String CSURI = "http://uri.etsi.org/01903#CountersignedSignature"; //$NON-NLS-1$
    private static final String AFIRMA = "AFIRMA"; //$NON-NLS-1$
    private static final String XML_SIGNATURE_PREFIX = "ds"; //$NON-NLS-1$

    private static final String DETACHED_CONTENT_ELEMENT_NAME = "CONTENT"; //$NON-NLS-1$
    private static final String DETACHED_STYLE_ELEMENT_NAME = "STYLE"; //$NON-NLS-1$

    /** Algoritmo de huella digital por defecto para las referencias XML. */
    private static final String DIGEST_METHOD = DigestMethod.SHA1;

    private static final String SIGNATURE_VALUE = "SignatureValue"; //$NON-NLS-1$

    private static final String URI_STR = "URI"; //$NON-NLS-1$

    private String algo;
    private Document doc;

    // Instalamos el proveedor de Apache. Esto es necesario para evitar problemas con los saltos de linea
    // de los Base 64
    static {
    	XmlDSigProviderHelper.configureXmlDSigProvider();
    }

    /** Firma datos en formato XMLDSig 1.0 (XML Digital Signature).
     * <p>
     *  En el caso de que se firma un fichero con formato XML que contenga hojas de estilo
     *  XSL, y siempre que no se haya establecido el par&aacute;metro <i>ignoreStyleSheets</i> a
     *  <i>true</i>, se sigue la siguiente convenci&oacute;n para la firma es estas:
     * </p>
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
     * @param key Clave privada a usar para firmar
     * @param certChain Cadena de certificados del firmante
     * @param xParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>uri</i></b></dt>
     *   <dd>URI en la que se encuentra el documento, necesario en el caso de modo expl&iacute;cito y formato detached</dd>
     *  <dt><b><i>mode</i></b></dt>
     *   <dd>
     *    Modo de firma a usar. Se admiten los siguientes valores:
     *    <ul>
     *     <li>
     *      &nbsp;&nbsp;&nbsp;<i>explicit</i><br>(<code>AOSignConstants.SIGN_MODE_EXPLICIT</code>)<br>
     *      <b>
     *       <br>Importante: Las firmas XMLDSig expl&iacute;citas no se adec&uacute;an a ninguna normativa,
     *       y pueden ser rechazadas por sistemas de validaci&oacute;n de firmas.
     *      </b>
     *     </li>
     *     <li>&nbsp;&nbsp;&nbsp;<i>implicit</i><br>(<code>AOSignConstants.SIGN_MODE_IMPLICIT</code>)</li>
     *    </ul>
     *   </dd>
     *  <dt><b><i>xmlSignaturePrefix</i></b></dt>
     *   <dd>
     *    Prefijo de espacio de nombres XML para los nodos de firma. Si no se especifica este par&aacute;metro
     *    se usa el valor por defecto (<i>ds</i>).
     *   </dd>
     *  <dt><b><i>format</i></b></dt>
     *   <dd>
     *    Formato en que se realizar&aacute; la firma. Se admiten los siguientes valores:
     *    <ul>
     *     <li>&nbsp;&nbsp;&nbsp;<i>XMLDSig Detached</i><br>(<code>AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED</code>)</li>
     *     <li>&nbsp;&nbsp;&nbsp;<i>XMLDSig Externally Detached</i><br>(<code>AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED</code>)</li>
     *     <li>&nbsp;&nbsp;&nbsp;<i>XMLDSig Enveloped</i><br>(<code>AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED</code>)</li>
     *     <li>&nbsp;&nbsp;&nbsp;<i>XMLDSig Enveloping</i><br>(<code>AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING</code>)</li>
     *    </ul>
     *   </dd>
     *  <dt><b><i>precalculatedHashAlgorithm</i></b></dt>
     *   <dd>Algoritmo de huella digital cuando esta se proporciona precalculada</dd>
     *  <dt><b><i>xmlTransforms</i></b></dt>
     *   <dd>N&uacute;mero de transformaciones a aplicar al XML antes de firmarlo</dd>
     *  <dt><b><i>xmlTransform</i>n<i>Type</i></b></dt>
     *   <dd>Tipo de la transformaci&oacute;n <i>n</i> (debe ser la URL del algoritmo segun define W3C)</dd>
     *  <dt><b><i>xmlTransform</i>n<i>Subtype</i></b></dt>
     *   <dd>Subtipo de la transformaci&oacute;n <i>n</i> (por ejemplo, "intersect", "subtract" o "union" para XPATH2)</dd>
     *  <dt><b><i>xmlTransform</i>n<i>Body</i></b></dt>
     *   <dd>Cuerpo de la transformaci&oacute;n <i>n</i></dd>
     *  <dt><b><i>referencesDigestMethod</i></b></dt>
     *   <dd>Algoritmo de huella digital a usar en las referencias XML</dd>
     *  <dt><b><i>canonicalizationAlgorithm</i></b></dt>
     *   <dd>Algoritmo de canonicalizaci&oacute;n<i>n</i></dd>
     *  <dt><b><i>ignoreStyleSheets</i></b></dt>
     *   <dd>Ignora las hojas de estilo externas de los XML (no las firma) si se establece a <code>true</code>, si se establece a <code>false</code> s&iacute; las firma</dd>
     *  <dt><b><i>mimeType</i></b></dt>
     *   <dd>MIME-Type de los datos a firmar</dd>
     *  <dt><b><i>encoding</i></b></dt>
     *   <dd>Codificaci&oacute;n de los datos a firmar</dd>
     *  <dt><b><i>avoidBase64Transforms</i></b></dt>
     *   <dd>
     *    No declara transformaciones Base64 incluso si son necesarias si se establece a <code>true</code>, si se establece a <code>false</code>
     *    act&uacute;a normalmente (s&iacute; las declara)
     *   </dd>
     *  <dt><b><i>headless</i></b></dt>
     *   <dd>
     *    Evita cualquier interacci&oacute;n con el usuraio si se establece a <code>true</code>, si se establece a <code>false</code> act&uacute;a
     *    normalmente (puede mostrar di&aacute;logos, por ejemplo, para la dereferenciaci&oacute;n de hojas de estilo enlazadas con rutas relativas).
     *    &Uacute;til para los procesos desatendidos y por lotes
     *   </dd>
     *  <dt><b><i>includeOnlySignningCertificate</i></b></dt>
	 *   <dd>Indica, mediante un {@code true} o {@code false}, que debe
	 *   incluirse en la firma &uacute;nicamente el certificado utilizado
	 *   para firmar y no su cadena de certificaci&oacute;n completa.
	 *   Por defecto, se incluir&aacute; toda la cadena de certificaci&oacute;n.
	 *   </dd>
     * </dl>
     * @return Firma en formato XMLDSig 1.0
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] sign(final byte[] data,
                       final String algorithm,
                       final PrivateKey key,
                       final Certificate[] certChain,
                       final Properties xParams) throws AOException {

        final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
        if (algoUri == null) {
            throw new UnsupportedOperationException(
        		"La URI de definicion del algoritmo de firma no puede ser nula" //$NON-NLS-1$
    		);
        }

        final Properties extraParams = xParams != null ? xParams : new Properties();

        final String format = extraParams.getProperty(
        		AOXMLDSigExtraParams.FORMAT, AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING);
        final String mode = extraParams.getProperty(
        		AOXMLDSigExtraParams.MODE, AOSignConstants.SIGN_MODE_IMPLICIT);
        final String digestMethodAlgorithm = extraParams.getProperty(
        		AOXMLDSigExtraParams.REFERENCES_DIGEST_METHOD, DIGEST_METHOD);
        final String canonicalizationAlgorithm = extraParams.getProperty(
        		AOXMLDSigExtraParams.CANONICALIZATION_ALGORITHM, CanonicalizationMethod.INCLUSIVE);
		final boolean ignoreStyleSheets = Boolean.parseBoolean(extraParams.getProperty(
		        AOXMLDSigExtraParams.IGNORE_STYLE_SHEETS, Boolean.FALSE.toString()));
		final boolean avoidBase64Transforms = Boolean.parseBoolean(extraParams.getProperty(
		        AOXMLDSigExtraParams.AVOID_BASE64_TRANSFORMS, Boolean.FALSE.toString()));
		final boolean headless = Boolean.parseBoolean(extraParams.getProperty(
		        AOXMLDSigExtraParams.HEADLESS, Boolean.TRUE.toString()));
		final boolean avoidXpathExtraTransformsOnEnveloped = Boolean.parseBoolean(extraParams.getProperty(
		        AOXMLDSigExtraParams.AVOID_XPATH_EXTRA_TRANSFORMS_ON_ENVELOPED, Boolean.FALSE.toString()));
        final String xmlSignaturePrefix = extraParams.getProperty(
        		AOXMLDSigExtraParams.XML_SIGNATURE_PREFIX, XML_SIGNATURE_PREFIX);

        String mimeType = extraParams.getProperty(
        		AOXMLDSigExtraParams.MIME_TYPE);
        String encoding = extraParams.getProperty(
        		AOXMLDSigExtraParams.ENCODING);
        if ("base64".equalsIgnoreCase(encoding)) { //$NON-NLS-1$
            encoding = XMLConstants.BASE64_ENCODING;
        }

        URI uri = null;
        try {
            uri = AOUtil.createURI(extraParams.getProperty(AOXMLDSigExtraParams.URI));
        }
        catch (final Exception e) {
            // Se ignora, puede estar ausente
        }

        final String precalculatedHashAlgorithm = extraParams.getProperty(AOXMLDSigExtraParams.PRECALCULATED_HASH_ALGORITHM);

        Utils.checkIllegalParams(format, mode, false, uri, precalculatedHashAlgorithm, false);

        // Un externally detached con URL permite los datos nulos o vacios
        if ((data == null || data.length == 0) && !(format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED) && uri != null)) {
            throw new AOException("No se han podido leer los datos a firmar"); //$NON-NLS-1$
        }

        // Propiedades del documento XML original
        final Map<String, String> originalXMLProperties = new Hashtable<>();

        // Elemento de datos
        Element dataElement;

        final String contentId = DETACHED_CONTENT_ELEMENT_NAME + "-" + UUID.randomUUID().toString() + "-" + DETACHED_CONTENT_ELEMENT_NAME; //$NON-NLS-1$ //$NON-NLS-2$
        final String styleId = DETACHED_STYLE_ELEMENT_NAME + "-" + UUID.randomUUID().toString() + "-" + DETACHED_STYLE_ELEMENT_NAME; //$NON-NLS-1$ //$NON-NLS-2$
        boolean isBase64 = false;
        boolean wasEncodedToBase64 = false;

		// Elemento de estilo
		XmlStyle xmlStyle = new XmlStyle();

        if (mode.equals(AOSignConstants.SIGN_MODE_IMPLICIT)) {
            try {
                // Obtenemos el objeto XML y su codificacion
                final Document docum = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(data));

                // Obtenemos la hoja de estilo del XML
                if (!ignoreStyleSheets) {
                	try {
                		xmlStyle = new XmlStyle(data, headless);
                	}
					catch (final IsInnerlException ex) {
						LOGGER.info(
							"La hoja de estilo esta referenciada internamente, por lo que no se necesita dereferenciar: " + ex//$NON-NLS-1$
						);
					}
					catch (final ReferenceIsNotXmlException ex) {
						LOGGER.warning(
							"La hoja de estilo referenciada no es XML o no se ha dereferenciado apropiadamente: " + ex //$NON-NLS-1$
						);
					}
					catch (final CannotDereferenceException ex) {
						LOGGER.warning(
							"La hoja de estilo no ha podido dereferenciar, probablemente sea un enlace relativo local: " + ex //$NON-NLS-1$
						);
					}
					catch (final Exception ex) {
						LOGGER.severe(
							"Error intentando dereferenciar la hoja de estilo: " + ex //$NON-NLS-1$
						);
					}
                }

                // Si no hay asignado un MimeType o es el por defecto
                // establecemos el de XML
                if (mimeType == null || XMLConstants.DEFAULT_MIMETYPE.equals(mimeType)) {
                    mimeType = "text/xml"; //$NON-NLS-1$
                }

                if (encoding == null) {
                    encoding = docum.getXmlEncoding();
                }

                // Ademas del encoding, sacamos otros datos del doc XML original

                // Hacemos la comprobacion del base64 por si se establecido
                // desde fuera
                if (encoding != null && !XMLConstants.BASE64_ENCODING.equals(encoding)) {
                    originalXMLProperties.put(OutputKeys.ENCODING, encoding);
                }
                String tmpXmlProp = docum.getXmlVersion();
                if (tmpXmlProp != null) {
                    originalXMLProperties.put(OutputKeys.VERSION, tmpXmlProp);
                }
                final DocumentType dt = docum.getDoctype();
                if (dt != null) {
                    tmpXmlProp = dt.getSystemId();
                    if (tmpXmlProp != null) {
                        originalXMLProperties.put(OutputKeys.DOCTYPE_SYSTEM, tmpXmlProp);
                    }
                }

                if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED)) {
                    dataElement = docum.createElement(DETACHED_CONTENT_ELEMENT_NAME);
                    dataElement.setAttributeNS(null, ID_IDENTIFIER, contentId);
                    dataElement.setAttributeNS(null, MIMETYPE_STR, mimeType);
                    dataElement.setAttributeNS(null, ENCODING_STR, encoding);
                    dataElement.appendChild(docum.getDocumentElement());

                    // Tambien el estilo
                    if (xmlStyle.getStyleElement() != null) {
                        try {
                            final Element tmpStyleElement = docum.createElement(DETACHED_STYLE_ELEMENT_NAME);
                            tmpStyleElement.setAttributeNS(null, ID_IDENTIFIER, styleId);
                            if (xmlStyle.getStyleType() != null) {
                                tmpStyleElement.setAttributeNS(null, MIMETYPE_STR, xmlStyle.getStyleType());
                            }
                            tmpStyleElement.setAttributeNS(null, ENCODING_STR, xmlStyle.getStyleEncoding());
                            tmpStyleElement.appendChild(docum.adoptNode(xmlStyle.getStyleElement().cloneNode(true)));
                            xmlStyle.setStyleElement(tmpStyleElement);
                        }
                        catch (final Exception e) {
                            LOGGER.warning(
                        		"No ha sido posible crear el elemento DOM para incluir la hoja de estilo del XML como Internally Detached: " + e //$NON-NLS-1$
                    		);
                            xmlStyle.setStyleElement(null);
                        }
                    }
                }
                else {
                    dataElement = docum.getDocumentElement();
                }

            }
            // captura de error en caso de no ser un documento xml
            catch (final Exception e) {
                if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) {
                    throw new InvalidXMLException(e);
                }
                // para los formatos de firma internally detached y enveloping
                // se trata de convertir el documento a base64
                try {
                    LOGGER.info("El documento no es un XML valido. Se convertira a Base64: " + e); //$NON-NLS-1$

                    // crea un nuevo nodo xml para contener los datos en base 64
                    final Document docFile = Utils.getNewDocumentBuilder().newDocument();
                    dataElement = docFile.createElement(DETACHED_CONTENT_ELEMENT_NAME);
                    uri = null;
                    encoding = XMLConstants.BASE64_ENCODING;
                    if (mimeType == null) {
                        mimeType = XMLConstants.DEFAULT_MIMETYPE;
                    }

                    dataElement.setAttributeNS(null, ID_IDENTIFIER, contentId);

                    // Si es base 64, lo firmamos indicando como contenido el
                    // dato pero, ya que puede
                    // poseer un formato particular o caracteres valido pero
                    // extranos para el XML,
                    // realizamos una decodificacion y recodificacion para asi
                    // homogenizar el formato.
                    if (Base64.isBase64(data) && (XMLConstants.BASE64_ENCODING.equals(encoding) || (encoding != null ? encoding : "").toLowerCase().equals("base64"))) { //$NON-NLS-1$ //$NON-NLS-2$
                        LOGGER.info("El documento se ha indicado como Base64, se insertara como tal en el XML"); //$NON-NLS-1$

                        // Adicionalmente, si es un base 64 intentamos obtener
                        // el tipo del contenido
                        // decodificado para asi reestablecer el MimeType.
                        final byte[] decodedData = Base64.decode(data, 0, data.length, false);
                        final MimeHelper mimeTypeHelper = new MimeHelper(decodedData);
                        final String tempMimeType = mimeTypeHelper.getMimeType();
                        mimeType = tempMimeType != null ? tempMimeType : XMLConstants.DEFAULT_MIMETYPE;
                        dataElement.setAttributeNS(null, MIMETYPE_STR, mimeType);
                        dataElement.setTextContent(new String(data));
                    }
                    else {
                        if (XMLConstants.BASE64_ENCODING.equals(encoding)) {
                            LOGGER.info("El documento se ha indicado como Base64, pero no es un Base64 valido. Se convertira a Base64 antes de insertarlo en el XML y se declarara la transformacion"); //$NON-NLS-1$
                        }
                        else {
                            LOGGER.info("El documento se considera binario, se convertira a Base64 antes de insertarlo en el XML y se declarara la transformacion"); //$NON-NLS-1$
                        }

                        // Identificamos el MimeType
                        if (XMLConstants.DEFAULT_MIMETYPE.equals(mimeType)) {
                        	final MimeHelper mimeTypeHelper = new MimeHelper(data);
                            final String tempMimeType = mimeTypeHelper.getMimeType();
                            mimeType = tempMimeType != null ? tempMimeType : XMLConstants.DEFAULT_MIMETYPE;
                        }
                        dataElement.setAttributeNS(null, MIMETYPE_STR, mimeType);
                        dataElement.setTextContent(Base64.encode(data));
                        wasEncodedToBase64 = true;
                    }
                    isBase64 = true;
                    encoding = XMLConstants.BASE64_ENCODING;
                    dataElement.setAttributeNS(null, ENCODING_STR, encoding);
                }
                catch (final Exception ex) {
                    throw new AOException("Error al convertir los datos a base64", ex); //$NON-NLS-1$
                }
            }
        }

        // Firma Explicita
        else {
            // ESTE BLOQUE CONTIENE EL PROCESO A SEGUIR EN EL MODO EXPLICITO,
            // ESTO ES, NO FIRMAMOS LOS DATOS SINO SU HASH
            byte[] digestValue = null;
            // Si la URI no es nula recogemos los datos de fuera
            if (uri != null) {
                byte[] tmpData = null;
                try {
                    tmpData = AOUtil.getDataFromInputStream(AOUtil.loadFile(uri));
                }
                catch (final Exception e) {
                    throw new AOException("No se han podido obtener los datos de la URI externa", e); //$NON-NLS-1$
                }
                // Vemos si hemos obtenido bien los datos de la URI
                if (tmpData != null && tmpData.length > 0) {
                    try {
                        digestValue = MessageDigest.getInstance("SHA1").digest(tmpData); //$NON-NLS-1$
                    }
                    catch (final Exception e) {
                        throw new AOException("No se ha podido obtener el SHA1 de los datos de la URI externa", e); //$NON-NLS-1$
                    }
                }
            }
            // Si no tenemos URI y se nos inserto directamente el hash de los
            // datos
            else if (precalculatedHashAlgorithm != null) {
                digestValue = data;
            }
            // Si solo tenemos los datos
            else {
                try {
                    digestValue = MessageDigest.getInstance("SHA1").digest(data); //$NON-NLS-1$
                }
                catch (final Exception e) {
                    throw new AOException("No se ha podido obtener el SHA1 de los datos proporcionados: " + e, e); //$NON-NLS-1$
                }
            }

            if (digestValue == null || digestValue.length < 1) {
                throw new AOException("Error al obtener la huella SHA1 de los datos"); //$NON-NLS-1$
            }

            final Document docFile;
            try {
                docFile = Utils.getNewDocumentBuilder().newDocument();
            }
            catch (final Exception e) {
                throw new AOException("No se ha podido crear el documento XML contenedor: " + e, e); //$NON-NLS-1$
            }
            dataElement = docFile.createElement(DETACHED_CONTENT_ELEMENT_NAME);

            encoding = XMLConstants.BASE64_ENCODING;
            // En el caso de la firma explicita, se firma el Hash de los datos
            // en lugar de los propios datos.
            // En este caso, los indicaremos a traves del MimeType en donde
            // establecemos un tipo especial
            // que designa al hash. Independientemente del algoritmo de firma
            // utilizado, el Hash de las firmas
            // explicitas de datos siempre sera SHA1, salvo que el hash se haya
            // establecido desde fuera.
            if (precalculatedHashAlgorithm != null) {
                mimeType = "hash/" + precalculatedHashAlgorithm.toLowerCase(); //$NON-NLS-1$
            }
            else {
                mimeType = "hash/sha1"; //$NON-NLS-1$
            }

            dataElement.setAttributeNS(null, ID_IDENTIFIER, contentId);
            dataElement.setAttributeNS(null, MIMETYPE_STR, mimeType);
            dataElement.setAttributeNS(null, ENCODING_STR, encoding);

            dataElement.setTextContent(Base64.encode(digestValue));
            isBase64 = true;

            // FIN BLOQUE EXPLICITO
        }

        // ***************************************************
        // ***************************************************

        final String tmpUri = "#" + contentId; //$NON-NLS-1$
        final String tmpStyleUri = "#" + styleId; //$NON-NLS-1$

        // Crea el nuevo documento de firma
        Document docSignature = null;
        try {
            docSignature = Utils.getNewDocumentBuilder().newDocument();
            if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) {
                docSignature.appendChild(docSignature.adoptNode(dataElement));
            }
            else {
                docSignature.appendChild(docSignature.createElement(AFIRMA));
            }
        }
        catch (final Exception e) {
            throw new AOException("Error al crear la firma en formato " + format + ", modo " + mode, e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        final List<Reference> referenceList = new ArrayList<>();
        final XMLSignatureFactory fac = Utils.getDOMFactory();
        final DigestMethod digestMethod;
        try {
            digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        final String referenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$
        final String referenceStyleId = STYLE_REFERENCE_PREFIX + UUID.randomUUID().toString();

        final List<Transform> transformList = new ArrayList<>();

        // Primero anadimos las transformaciones a medida
        Utils.addCustomTransforms(transformList, extraParams, xmlSignaturePrefix);

        final Transform canonicalizationTransform;
        if ("none".equalsIgnoreCase(canonicalizationAlgorithm)) { //$NON-NLS-1$
        	canonicalizationTransform = null;
        }
        else {
        	try {
				canonicalizationTransform = fac.newTransform(canonicalizationAlgorithm, (TransformParameterSpec) null);
			}
        	catch (final Exception e) {
				throw new AOException("No se ha podido crear la transformacion de canonicalizacion para el algoritmo '" + canonicalizationAlgorithm + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
        }

        // Solo canonicalizo si es XML y no me han declarado el algoritmo como "none"
        if (!isBase64) {
        	if (canonicalizationTransform != null) {
	            try {
	                // Transformada para la canonicalizacion inclusiva
	                transformList.add(canonicalizationTransform);
	            }
	            catch (final Exception e) {
	                LOGGER
	                      .severe("No se puede encontrar el algoritmo de canonicalizacion, la referencia no se canonicalizara: " + e); //$NON-NLS-1$
	            }
        	}
        }
        // Si no era XML y tuve que convertir a Base64 yo mismo declaro la
        // transformacion
        else if (wasEncodedToBase64 && !avoidBase64Transforms) {
            try {
                transformList.add(fac.newTransform(Transform.BASE64, (TransformParameterSpec) null));
            }
            catch (final Exception e) {
                LOGGER.severe("No se puede encontrar el algoritmo transformacion Base64, esta no se declarara: " + e); //$NON-NLS-1$
            }
        }

        // crea una referencia al documento insertado en un nodo Object para la
        // firma enveloping y a el estilo
        XMLObject envelopingObject = null;
        XMLObject envelopingStyleObject = null;

        if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING)) {
            try {
                // crea el nuevo elemento Object que contiene el documento a
                // firmar
                final List<XMLStructure> structures = new ArrayList<>(1);

                // Si los datos se han convertido a base64, bien por ser
                // binarios o explicitos
                if (isBase64) {
                    structures.add(new DOMStructure(dataElement.getFirstChild()));
                }
                else {
                    structures.add(new DOMStructure(dataElement));
                }

                final String objectId = "Object-" + UUID.randomUUID().toString(); //$NON-NLS-1$
                envelopingObject = fac.newXMLObject(structures, objectId, mimeType, encoding);

                // crea la referencia al nuevo elemento Object
                referenceList.add(
            		fac.newReference(
        				"#" + objectId, //$NON-NLS-1$
        				digestMethod,
        				transformList,
        				XMLConstants.OBJURI,
        				referenceId
    				)
				);

                // Vamos con la hoja de estilo
                if (xmlStyle.getStyleElement() != null) {
                    final String objectStyleId = "StyleObject-" + UUID.randomUUID().toString(); //$NON-NLS-1$
                    envelopingStyleObject = fac.newXMLObject(
                		Collections.singletonList(new DOMStructure(xmlStyle.getStyleElement())),
                		objectStyleId,
                		xmlStyle.getStyleType(),
                		xmlStyle.getStyleEncoding()
            		);
                    referenceList.add(
                		fac.newReference(
            				"#" + objectStyleId, //$NON-NLS-1$
                            digestMethod,
                            canonicalizationTransform != null ?
                        		Collections.singletonList(canonicalizationTransform) :
                        			null,
            				XMLConstants.OBJURI,
                            referenceStyleId
                        )
                    );

                }
            }
            catch (final Exception e) {
                throw new AOException("Error al generar la firma en formato enveloping", e); //$NON-NLS-1$
            }

            // Hojas de estilo para enveloping en Externally Detached. Comprobamos si la referencia al estilo es externa
            if (xmlStyle.getStyleHref() != null &&
            	xmlStyle.getStyleElement() == null &&
            		(xmlStyle.getStyleHref().startsWith(HTTP_PROTOCOL_PREFIX) ||
            		 xmlStyle.getStyleHref().startsWith(HTTPS_PROTOCOL_PREFIX))) {
                try {
                    referenceList.add(
                		fac.newReference(
            				xmlStyle.getStyleHref(),
                            digestMethod,
                            canonicalizationTransform != null ?
                            		Collections.singletonList(canonicalizationTransform) :
                            			null,
                            XMLConstants.OBJURI,
                            referenceStyleId
                        )
                    );
                }
                catch (final Exception e) {
                    LOGGER.severe(
                		"No ha sido posible anadir la referencia a la hoja de estilo del XML en la firma Externally Detached, esta no se firmara: " + e //$NON-NLS-1$
            		);
                }
            }

        }

        // crea una referencia al documento mediante la URI hacia el
        // identificador del nodo CONTENT
        else if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED)) {
            try {
                if (dataElement != null) {
                    // inserta en el nuevo documento de firma el documento a
                    // firmar
                    docSignature.getDocumentElement().appendChild(docSignature.adoptNode(dataElement));
                    // crea la referencia a los datos firmados que se encontraran en el mismo documento
                    referenceList.add(
                		fac.newReference(
            				tmpUri,
            				digestMethod,
            				transformList,
            				XMLConstants.OBJURI,
            				referenceId
        				)
    				);
                }
                if (xmlStyle.getStyleElement() != null) {
                    // inserta en el nuevo documento de firma la hoja de estilo
                    docSignature.getDocumentElement().appendChild(docSignature.adoptNode(xmlStyle.getStyleElement()));
                    // crea la referencia a los datos firmados que se encontraran en el mismo documento
                    referenceList.add(
                		fac.newReference(
            				tmpStyleUri,
                            digestMethod,
                            canonicalizationTransform != null ?
                        		Collections.singletonList(canonicalizationTransform) :
                        			null,
                            XMLConstants.OBJURI,
                            referenceStyleId
                        )
                    );
                }

            }
            catch (final Exception e) {
                throw new AOException("Error al generar la firma en formato detached implicito", e); //$NON-NLS-1$
            }

            // Hojas de estilo remotas para detached. Comprobamos si la referencia al estilo es externa
            if (xmlStyle.getStyleHref() != null && xmlStyle.getStyleElement() == null &&
            		(xmlStyle.getStyleHref().startsWith(HTTP_PROTOCOL_PREFIX) || xmlStyle.getStyleHref().startsWith(HTTPS_PROTOCOL_PREFIX))) {
                try {
                    referenceList.add(
                		fac.newReference(
            				xmlStyle.getStyleHref(),
                            digestMethod,
                            Collections.singletonList(fac.newTransform(canonicalizationAlgorithm, (TransformParameterSpec) null)),
                            XMLConstants.OBJURI,
                            referenceStyleId
                        )
                    );
                }
                catch (final Exception e) {
                    LOGGER.severe(
                		"No ha sido posible anadir la referencia a la hoja de estilo del XML en la firma Detached Implicita, esta no se firmara: " + e //$NON-NLS-1$
                    );
                }
            }

        }

        // Crea una referencia al documento mediante la URI externa si la
        // tenemos o usando un Message Digest
        // precalculado si no tenemos otro remedio
        else if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED)) {
            Reference ref = null;
            // No tenemos uri, suponemos que los datos son el message digest
            if (precalculatedHashAlgorithm != null && (uri == null || uri.getScheme().equals("") || uri.getScheme().equals("file"))) { //$NON-NLS-1$ //$NON-NLS-2$
                DigestMethod dm = null;
                try {
                    // Convertimos el algoritmo del Message Digest externo a la
                    // nomenclatura XML
                    if (AOSignConstants.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA1")) { //$NON-NLS-1$
                        dm = fac.newDigestMethod(DigestMethod.SHA1, null);
                    }
                    else if (AOSignConstants.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA-256")) { //$NON-NLS-1$
                        dm = fac.newDigestMethod(DigestMethod.SHA256, null);
                    }
                    else if (AOSignConstants.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("SHA-512")) { //$NON-NLS-1$
                        dm = fac.newDigestMethod(DigestMethod.SHA512, null);
                    }
                    else if (AOSignConstants.getDigestAlgorithmName(precalculatedHashAlgorithm).equalsIgnoreCase("RIPEMD160")) { //$NON-NLS-1$
                        dm = fac.newDigestMethod(DigestMethod.RIPEMD160, null);
                    }
                }
                catch (final Exception e) {
                    throw new AOException("No se ha podido crear el metodo de huella digital para la referencia Externally Detached", e); //$NON-NLS-1$
                }
                if (dm == null) {
                    throw new AOException("Metodo de Message Digest para la referencia Externally Detached no soportado: " + precalculatedHashAlgorithm); //$NON-NLS-1$
                }
                ref = fac.newReference(
            		"", //$NON-NLS-1$
            		dm,
            		null,
            		XMLConstants.OBJURI, // Es un nodo a firmar
            		referenceId,
            		data
        		);
            } else // Si es una referencia de tipo file:// obtenemos el fichero y
			// creamos una referencia solo con
			// el message digest
			if (uri != null && uri.getScheme().equals("file")) { //$NON-NLS-1$
			    try {
			        ref = fac.newReference(
			    		"", //$NON-NLS-1$
			            digestMethod,
			            null,
			            XMLConstants.OBJURI,
			            referenceId,
			            MessageDigest.getInstance(
			        		AOSignConstants.getDigestAlgorithmName(digestMethodAlgorithm)
			    		).digest(AOUtil.getDataFromInputStream(AOUtil.loadFile(uri)))
					);
			    }
			    catch (final Exception e) {
			        throw new AOException("No se ha podido crear la referencia XML a partir de la URI local (" + uri.toASCIIString() + ")", e); //$NON-NLS-1$ //$NON-NLS-2$
			    }
			}
			// Si es una referencia distinta de file:// suponemos que es
			// dereferenciable de forma universal
			// por lo que dejamos que Java lo haga todo
			else if (uri != null) {
			    try {
			        ref = fac.newReference(uri.toASCIIString(), digestMethod);
			    }
			    catch (final Exception e) {
			        throw new AOException(
			    		"No se ha podido crear la referencia Externally Detached, probablemente por no obtenerse el metodo de digest", //$NON-NLS-1$
			            e
			        );
			    }
			}
            if (ref == null) {
                throw new AOException("Error al generar la firma Externally Detached, no se ha podido crear la referencia externa"); //$NON-NLS-1$
            }
            referenceList.add(ref);

            // Hojas de estilo remotas en Externally Detached
            if (xmlStyle.getStyleHref() != null && xmlStyle.getStyleElement() == null) {
                // Comprobamos que la URL es valida
                if (xmlStyle.getStyleHref().startsWith(HTTP_PROTOCOL_PREFIX) || xmlStyle.getStyleHref().startsWith(HTTPS_PROTOCOL_PREFIX)) {
                    try {
                        referenceList.add(
                    		fac.newReference(
                				xmlStyle.getStyleHref(),
                                digestMethod,
                                canonicalizationTransform != null ?
                            		Collections.singletonList(canonicalizationTransform) :
                            			null,
                                XMLConstants.OBJURI,
                                referenceStyleId
                            )
                        );
                    }
                    catch (final Exception e) {
                        LOGGER.severe(
                    		"No ha sido posible anadir la referencia a la hoja de estilo remota del XML en la firma Externally Detached, esta no se firmara: " + e //$NON-NLS-1$
                        );
                    }
                }
                else {
                    LOGGER.warning("Se necesita una referencia externa HTTP o HTTPS a la hoja de estilo para referenciarla en firmas XML Externally Detached"); //$NON-NLS-1$
                }
            }

        }

        // crea una referencia indicando que se trata de una firma enveloped
        else if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) {
            try {

                // Transformacion enveloped
                // La enveloped siempre la primera, para que no se quede sin
                // nodos Signature por haber
                // ejecutado antes otra transformacion
                transformList.add(fac.newTransform(Transform.ENVELOPED, (TransformParameterSpec) null));

                if (!avoidXpathExtraTransformsOnEnveloped) {
	                // Transformacion XPATH para eliminar el resto de firmas del documento
	                transformList.add(
	                  fac.newTransform(
	                    Transform.XPATH,
	                    new XPathFilterParameterSpec("not(ancestor-or-self::" + xmlSignaturePrefix + ":Signature)", //$NON-NLS-1$ //$NON-NLS-2$
	                    Collections.singletonMap(xmlSignaturePrefix, XMLSignature.XMLNS))
	                  )
	                );
                }

                // crea la referencia
                referenceList.add(
            		fac.newReference(
        				"", //$NON-NLS-1$
        				digestMethod,
        				transformList,
        				XMLConstants.OBJURI, // Aunque sea Enveloped, es un nodo a firmar
        				referenceId)
    				);
            }
            catch (final Exception e) {
                throw new AOException("Error al generar la firma en formato enveloped", e); //$NON-NLS-1$
            }

            // Hojas de estilo remotas para enveloped. Comprobamos si la referencia al estilo es externa
            if (xmlStyle.getStyleHref() != null && xmlStyle.getStyleElement() == null &&
            		(xmlStyle.getStyleHref().startsWith(HTTP_PROTOCOL_PREFIX) || xmlStyle.getStyleHref().startsWith(HTTPS_PROTOCOL_PREFIX))) {
                try {
                    referenceList.add(
                		fac.newReference(
            				xmlStyle.getStyleHref(),
            				digestMethod,
            				canonicalizationTransform != null ?
                        		Collections.singletonList(canonicalizationTransform) :
                        			null,
            				XMLConstants.OBJURI,
            				referenceStyleId
        				)
            		);
                }
                catch (final Exception e) {
                    LOGGER.severe(
                		"No ha sido posible anadir la referencia a la hoja de estilo remota del XML en la firma Enveloped, esta no se firmara: " + e //$NON-NLS-1$
            		);
                }
            }

        }

        // definicion de identificadores
        final String id = UUID.randomUUID().toString();
        final String keyInfoId = "KeyInfo-" + id; //$NON-NLS-1$

        try {

            // se anade una referencia a KeyInfo
            referenceList.add(fac.newReference("#" + keyInfoId, digestMethod, transformList, null, null)); //$NON-NLS-1$

            // KeyInfo
            final KeyInfoFactory kif = fac.getKeyInfoFactory();
            final List<XMLStructure> content = new ArrayList<>();
            final X509Certificate cert = (X509Certificate) certChain[0];
            content.add(kif.newKeyValue(cert.getPublicKey()));

            // Si se nos ha pedido expresamente que no insertemos la cadena de certificacion,
            // insertamos unicamente el certificado firmante. Tambien lo haremos cuando al
            // recuperar la cadena nos devuelva null
            Certificate[] certs = null;
            final boolean onlySignningCert = Boolean.parseBoolean(
            		extraParams.getProperty(
            		        AOXMLDSigExtraParams.INCLUDE_ONLY_SIGNNING_CERTIFICATE, Boolean.FALSE.toString()));
			if (!onlySignningCert) {
				certs = certChain;
			}
            if (certs == null) {
                certs = new Certificate[] {
                    cert
                };
            }
            content.add(kif.newX509Data(Arrays.asList(certs)));

            // Object
            final List<XMLObject> objectList = new ArrayList<>();

            // en el caso de formato enveloping se inserta el elemento Object
            // con el documento a firmar
            if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING) && envelopingObject != null) {
                objectList.add(envelopingObject);
                if (envelopingStyleObject != null) {
                    objectList.add(envelopingStyleObject);
                }
            }

            // Si es enveloped hay que anadir la hoja de estilo dentro de la
            // firma y referenciarla
            if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED) && xmlStyle.getStyleElement() != null) {
                objectList.add(
            		fac.newXMLObject(
        				Collections.singletonList(new DOMStructure(xmlStyle.getStyleElement())),
        				styleId,
        				xmlStyle.getStyleType(),
        				xmlStyle.getStyleEncoding()
    				)
				);
                try {
                    referenceList.add(
                		fac.newReference(
            				tmpStyleUri,
                            digestMethod,
                            canonicalizationTransform != null ?
                        		Collections.singletonList(canonicalizationTransform) :
                        			null,
                            XMLConstants.OBJURI,
                            referenceStyleId
                        )
                    );
                }
                catch (final Exception e) {
                    LOGGER
                          .severe("No se ha podido anadir una referencia a la hoja de estilo, esta se incluira dentro de la firma, pero no estara firmada: " + e); //$NON-NLS-1$
                }
            }

            // genera la firma
            final XMLSignature signature = fac.newXMLSignature(
            		fac.newSignedInfo(
            				fac.newCanonicalizationMethod(canonicalizationTransform != null ? canonicalizationAlgorithm : CanonicalizationMethod.INCLUSIVE, (C14NMethodParameterSpec) null),
            				fac.newSignatureMethod(algoUri, null),
            				XmlDSigUtil.cleanReferencesList(referenceList)),
            		kif.newKeyInfo(content, keyInfoId),
            		objectList,
            		"Signature-" + id, //$NON-NLS-1$
            		"SignatureValue-" + id //$NON-NLS-1$
            		);

            final DOMSignContext signContext = new DOMSignContext(
        		key, docSignature.getDocumentElement()
    		);
            signContext.putNamespacePrefix(XMLConstants.DSIGNNS, xmlSignaturePrefix);

            try {
            	// Instalamos un dereferenciador nuevo que solo actua cuando falla el por defecto
            	signContext.setURIDereferencer(
        			new CustomUriDereferencer(CustomUriDereferencer.getDefaultDereferencer())
    			);
            }
            catch (final Exception e) {
            	LOGGER.warning("No se ha podido instalar un dereferenciador a medida, es posible que fallen las firmas de nodos concretos: " + e); //$NON-NLS-1$
            }

            signature.sign(signContext);
        }
        catch (final NoSuchAlgorithmException e) {
            throw new UnsupportedOperationException(
        		"Hay al menos un algoritmo no soportado: " + e, e //$NON-NLS-1$
    		);
        }
        catch (final Exception e) {
            throw new AOException("Error al generar la firma XMLdSig: " + e, e); //$NON-NLS-1$
        }

        final String signatureNodeName = (xmlSignaturePrefix == null || xmlSignaturePrefix.isEmpty() ? "" : xmlSignaturePrefix + ":") + XMLConstants.TAG_SIGNATURE; //$NON-NLS-1$ //$NON-NLS-2$
        // Si se esta realizando una firma enveloping simple no tiene sentido el
        // nodo raiz,
        // asi que sacamos el nodo de firma a un documento aparte
        if (format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING)) {
            try {
                if (docSignature.getElementsByTagName(signatureNodeName).getLength() == 1) {
                    final Document newdoc = Utils.getNewDocumentBuilder().newDocument();
                    newdoc.appendChild(newdoc.adoptNode(docSignature.getElementsByTagName(signatureNodeName).item(0)));
                    docSignature = newdoc;
                }
            }
            catch (final Exception e) {
                LOGGER.info("No se ha eliminado el nodo padre '<AFIRMA>': " + e); //$NON-NLS-1$
            }
        }

        // Si no es enveloped quito los valores para que no se inserte la
        // cabecera de hoja de estilo
        return Utils.writeXML(
    		docSignature.getDocumentElement(),
    		originalXMLProperties,
    		format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED) ?
				xmlStyle.getStyleHref() :
					null,
			format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED) ?
				xmlStyle.getStyleType() :
					null
		);
    }

    /** Comprueba si la firma es detached.
     * @param element
     *        Elemento que contiene el nodo ra&iacute;z del documento que se
     *        quiere comprobar
     * @return Valor booleano, siendo verdadero cuando la firma es detached */
    private static boolean isDetached(final Element element) {
        if (element == null) {
            return false;
        }
        if (element.getFirstChild().getLocalName() != null && element.getFirstChild().getLocalName().equals(DETACHED_CONTENT_ELEMENT_NAME)) {
            return true;
        }
        return false;
    }

    /** Comprueba si la firma es <i>enveloped</i>.
     * @param element Elemento que contiene el nodo ra&iacute;z del documento que se
     *                quiere comprobar.
     * @return Valor booleano, siendo verdadero cuando la firma es <i>enveloped</i>. */
    private static boolean isEnveloped(final Element element) {
        final NodeList transformList = element.getElementsByTagNameNS(XMLConstants.DSIGNNS, "Transform"); //$NON-NLS-1$
        for (int i = 0; i < transformList.getLength(); i++) {
            if (((Element) transformList.item(i)).getAttribute("Algorithm").equals(Transform.ENVELOPED)){ //$NON-NLS-1$
                return true;
            }
        }
        return false;
    }

    /** Comprueba si la firma es <i>enveloping</i>.
     * @param element Elemento que contiene el nodo ra&iacute;z del documento que se quiere comprobar.
     * @return Valor booleano, siendo verdadero cuando la firma es <i>enveloping</i>. */
    private static boolean isEnveloping(final Element element) {
        if (element == null) {
            return false;
        }
        return XMLConstants.TAG_SIGNATURE.equals(element.getLocalName()) ||
        		AFIRMA.equals(element.getNodeName()) && XMLConstants.TAG_SIGNATURE.equals(element.getFirstChild().getLocalName());
    }

    /** {@inheritDoc} */
	@Override
	public byte[] getData(final byte[] sign) throws AOInvalidFormatException {
		return getData(sign, null);
	}

    /** {@inheritDoc} */
    @Override
	public byte[] getData(final byte[] sign, final Properties params) throws AOInvalidFormatException {
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
            rootSig = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign)).getDocumentElement();

            // si es detached
            if (AOXMLDSigSigner.isDetached(rootSig)) {
                final Element firstChild = (Element) rootSig.getFirstChild();
                // si el documento es un xml se extrae como tal
                if (firstChild.getAttribute(MIMETYPE_STR).equals("text/xml")) { //$NON-NLS-1$
                    elementRes = (Element) firstChild.getFirstChild();
                }
                // si el documento es binario se deshace la codificacion en Base64
                else {
                    return Base64.decode(firstChild.getTextContent());
                }
            }

            // si es enveloped
            else if (AOXMLDSigSigner.isEnveloped(rootSig)) {
                // obtiene las firmas y las elimina
                final NodeList signatures = rootSig.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);
                final int numSignatures = signatures.getLength();
                for (int i = 0; i < numSignatures; i++) {
                	// Comprobamos que no sean nulas, ya que las contrafirmas
                	// han podido eliminarse al eliminar alguna otra firma
                	if (signatures.item(i) != null) {
                		rootSig.removeChild(signatures.item(i));
                	}
                }
                elementRes = rootSig;
            }

            // si es enveloping
            else if (AOXMLDSigSigner.isEnveloping(rootSig)) {
                // obtiene el nodo Object de la primera firma
                final Element object = (Element) rootSig.getElementsByTagNameNS(XMLConstants.DSIGNNS, "Object").item(0); //$NON-NLS-1$
                // si el documento es un xml se extrae como tal
                if (object.getAttribute(MIMETYPE_STR).equals("text/xml")) { //$NON-NLS-1$
                    elementRes = (Element) object.getFirstChild();
                }
                else {
                    return Base64.decode(object.getTextContent());
                }
            }
        }
        catch (final Exception ex) {
            throw new AOInvalidFormatException("Error al leer el fichero de firmas", ex); //$NON-NLS-1$
        }

        // si no se ha recuperado ningun dato se devuelve null
        if (elementRes == null) {
            return null;
        }

        // convierte el documento obtenido en un array de bytes
        final ByteArrayOutputStream baosSig = new ByteArrayOutputStream();
        writeXML(new BufferedWriter(new OutputStreamWriter(baosSig)), elementRes);
        return baosSig.toByteArray();
    }

    /** Cofirma una firma en formato XMLdSig.
     * <p>
     *  Este m&eacute;todo firma todas las referencias a datos declaradas en la firma original,
     *  ya apunten estas a datos, hojas de estilo o cualquier otro elemento. En cada referencia
     *  firmada se introduciran las mismas transformaciones que existiesen en la firma original.
     * </p>
     * <p>
     *  A nivel de formato interno, cuando cofirmamos un documento ya firmado previamente, esta
     *  firma previa no se modifica. Si tenemos en cuenta que XAdES es en realidad un subconjunto
     *  de XMLDSig, el resultado de una cofirma XMLdSig sobre un documento firmado previamente con
     *  XAdES (o viceversa), son dos firmas independientes, una en XAdES y otra en XMLDSig.<br>
     *  Dado que todas las firmas XAdES son XMLDSig pero no todas las firmas XMLDSig son XAdES,
     *  el resultado global de la firma se adec&uacute;a al estandar mas amplio, XMLDSig en este caso.
     * </p>
     * @param data No se utiliza.
     * @param sign Firma que se desea cofirmar.
     * @param algorithm Algoritmo a usar para la firma.
     * @param key Clave privada a usar para firmar
     * @param certChain Cadena de certificados del firmante
     * @param xParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>xmlSignaturePrefix</i></b></dt>
     *   <dd>
     *    Prefijo de espacio de nombres XML para los nodos de firma. Si no se especifica este par&aacute;metro
     *    se usa el valor por defecto (<i>ds</i>).
     *   </dd>
     *  <dt><b><i>referencesDigestMethod</i></b></dt>
     *   <dd>Algoritmo de huella digital a usar en las referencias XML</dd>
     *  <dt><b><i>canonicalizationAlgorithm</i></b></dt>
     *   <dd>Algoritmo de canonicalizaci&oacute;n<i>n</i></dd>
     *  <dt><b><i>includeOnlySignningCertificate</i></b></dt>
	 *   <dd>Indica, mediante un {@code true} o {@code false}, que debe
	 *   incluirse en la firma &uacute;nicamente el certificado utilizado
	 *   para firmar y no su cadena de certificaci&oacute;n completa.
	 *   Por defecto, se incluir&aacute; toda la cadena de certificaci&oacute;n.
	 *   </dd>
     * </dl>
     * @return Firma en formato XMLDSig 1.0
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] cosign(final byte[] data,
                         final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final Certificate[] certChain,
                         final Properties xParams) throws AOException {

        final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
        if (algoUri == null) {
            throw new UnsupportedOperationException(
        		"La URI de definicion del algoritmo de firma no puede ser nula" //$NON-NLS-1$
    		);
        }

        final Properties extraParams = xParams != null ? xParams : new Properties();

        final String digestMethodAlgorithm = extraParams.getProperty(AOXMLDSigExtraParams.REFERENCES_DIGEST_METHOD, DIGEST_METHOD);
        final String canonicalizationAlgorithm = extraParams.getProperty(AOXMLDSigExtraParams.CANONICALIZATION_ALGORITHM, CanonicalizationMethod.INCLUSIVE);
        final String xmlSignaturePrefix = extraParams.getProperty(AOXMLDSigExtraParams.XML_SIGNATURE_PREFIX, XML_SIGNATURE_PREFIX);

        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // Propiedades del documento XML original
        final Map<String, String> originalXMLProperties = new Hashtable<>();

        // carga el documento XML de firmas y su raiz
        Document docSig;
        Element rootSig;
        try {
            docSig = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign));
            rootSig = docSig.getDocumentElement();

            // si el documento contiene una firma simple se inserta como raiz el
            // nodo AFIRMA
            if (XMLConstants.TAG_SIGNATURE.equals(rootSig.getLocalName()) && XMLConstants.DSIGNNS.equals(rootSig.getNamespaceURI())) {
                docSig = insertarNodoAfirma(docSig);
                rootSig = docSig.getDocumentElement();
            }
        }
        catch (final ParserConfigurationException pcex) {
            throw new AOException("Error en el amalizador XML: " + pcex, pcex); //$NON-NLS-1$
        }
        catch (final SAXException saxex) {
            throw new AOException("Formato de documento de firmas (XML firmado de entrada) incorrecto: " + saxex, saxex); //$NON-NLS-1$
        }
        catch (final IOException ioex) {
            throw new AOException("Error al leer el documento de firmas", ioex); //$NON-NLS-1$
        }
        catch (final IllegalArgumentException iaex) {
            throw new AOException("Parametro de entrada incorrecto", iaex); //$NON-NLS-1$
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido leer el documento XML de firmas", e); //$NON-NLS-1$
        }

        final List<Reference> referenceList = new ArrayList<>();
        final XMLSignatureFactory fac = Utils.getDOMFactory();
        final DigestMethod digestMethod;
        try {
            digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // Localizamos la primera firma (primer nodo XMLConstants.TAG_SIGNATURE) en profundidad
        // en el arbol de firma.
        // Se considera que todos los objetos XMLConstants.TAG_SIGNATURE del documento firman
        // (referencian) los mismos
        // objetos, por lo que podemos extraerlos de cualquiera de las firmas
        // actuales.
        // Buscamos dentro de ese Signature todas las referencias que apunten a
        // datos para firmarlas
        final ArrayList<String> referencesIds = new ArrayList<>();
        Node currentReference;
        final NodeList nl = ((Element) docSig.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE).item(0)).getElementsByTagNameNS(XMLConstants.DSIGNNS, REFERENCE_STR);

        // Se considera que la primera referencia de la firma son los datos que
        // debemos firmar, ademas
        // de varias referencias especiales
        XMLObject envelopingObject = null;
        for (int i = 0; i < nl.getLength(); i++) {
            currentReference = nl.item(i);

            // Firmamos la primera referencia (que seran los datos firmados) y
            // las hojas de estilo que
            // tenga asignadas. Las hojas de estilo tendran un identificador que
            // comience por STYLE_REFERENCE_PREFIX.
            // TODO: Identificar las hojas de estilo de un modo generico.
            final NamedNodeMap currentNodeAttributes = currentReference.getAttributes();
            if (i == 0 || currentNodeAttributes.getNamedItem(ID_IDENTIFIER) != null &&
            		currentNodeAttributes.getNamedItem(ID_IDENTIFIER).getNodeValue().startsWith(STYLE_REFERENCE_PREFIX)) {

                // Buscamos las transformaciones declaradas en la Referencia,
                // para anadirlas
                // tambien en la nueva
                List<Transform> currentTransformList;
                try {
                    currentTransformList = Utils.getObjectReferenceTransforms(currentReference, xmlSignaturePrefix);
                }
                catch (final NoSuchAlgorithmException e) {
                    Logger.getLogger("Se ha declarado una transformacion personalizada de un tipo no soportado: " + e); //$NON-NLS-1$
                    throw new AOException("Se ha declarado una transformacion personalizada de un tipo no soportado", e); //$NON-NLS-1$
                }
                catch (final InvalidAlgorithmParameterException e) {
                    Logger.getLogger("Se han especificado parametros erroneos para una transformacion personalizada: " + e); //$NON-NLS-1$
                    throw new AOException("Se han especificado parametros erroneos para una transformacion personalizada", e); //$NON-NLS-1$
                }

                // Creamos un identificador de referencia para el objeto a
                // firmar y la almacenamos
                // para mantener un listado con todas. En el caso de las hojas
                // de estilo lo creamos con un
                // identificador descriptivo
                String referenceId = null;
                if (currentNodeAttributes.getNamedItem(ID_IDENTIFIER) != null &&
                		currentNodeAttributes.getNamedItem(ID_IDENTIFIER).getNodeValue().startsWith(STYLE_REFERENCE_PREFIX)) {
                    referenceId = STYLE_REFERENCE_PREFIX + UUID.randomUUID().toString();
                }
                else {
                    referenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$
                }
                referencesIds.add(referenceId);

                // Creamos la propia referencia con las transformaciones de la original
                // TODO: Copiar el nodo para enveloping
                final String referenceUri = ((Element) currentReference).getAttribute(URI_STR);
                if (isEnveloping(rootSig) && referenceUri != null) {

                	final Node dataNode = searchDataElement(referenceUri, rootSig);
                	if (dataNode == null) {
                		LOGGER.severe("No se ha identificado el nodo de datos a firmar"); //$NON-NLS-1$
                		throw new AOException("No se ha identificado el nodo de datos a firmar"); //$NON-NLS-1$
                	}

                	// crea el nuevo elemento Object que con el documento afirmar
					final List<XMLStructure> structures = new ArrayList<>(1);
					structures.add(new DOMStructure(dataNode.getFirstChild().cloneNode(true)));

					final String mimeType = ((Element) dataNode).getAttribute(MIMETYPE_STR);
					final String encoding = ((Element) dataNode).getAttribute(ENCODING_STR);

					final String newObjectId = "Object-" + UUID.randomUUID().toString(); //$NON-NLS-1$
					envelopingObject = fac.newXMLObject(structures, newObjectId, mimeType, encoding);

					// Agregamos la referencia al nuevo objeto de datos
					referenceList.add(
						fac.newReference(
							"#" + newObjectId, //$NON-NLS-1$
							digestMethod,
							currentTransformList,
							XMLConstants.OBJURI,
							referenceId
						)
					);
                }
                else {
                	referenceList.add(
            			fac.newReference(
                			((Element) currentReference).getAttribute(URI_STR),
                			digestMethod,
                			currentTransformList, // Lista de transformaciones
                			XMLConstants.OBJURI,
                			referenceId
            			)
        			);
                }
            }
        }

        // definicion de identificadores
        final String id = UUID.randomUUID().toString();
        final String signatureId = "Signature-" + id; //$NON-NLS-1$
        final String signatureValueId = "SignatureValue-" + id; //$NON-NLS-1$
        final String keyInfoId = "KeyInfo-" + id; //$NON-NLS-1$

        try {

            // CanonicalizationMethod
            final CanonicalizationMethod cm = fac.newCanonicalizationMethod(canonicalizationAlgorithm, (C14NMethodParameterSpec) null);

            // se anade una referencia a KeyInfo
            final List<Transform> transformList = new ArrayList<>();
            final Transform trCanonicalization = fac.newTransform(canonicalizationAlgorithm, (TransformParameterSpec) null);
            transformList.add(trCanonicalization);
            referenceList.add(fac.newReference("#" + keyInfoId, digestMethod, transformList, null, null)); //$NON-NLS-1$

            // SignatureMethod
            final SignatureMethod sm = fac.newSignatureMethod(algoUri, null);

            // KeyInfo
            final KeyInfoFactory kif = fac.getKeyInfoFactory();
            final X509Certificate cert = (X509Certificate) certChain[0];

            final List<XMLStructure> content = new ArrayList<>();
            content.add(kif.newKeyValue(cert.getPublicKey()));

            // Si se nos ha pedido expresamente que no insertemos la cadena de certificacion,
            // insertamos unicamente el certificado firmante. Tambien lo haremos cuando al
            // recuperar la cadena nos devuelva null
            Certificate[] certs = null;
            final boolean onlySignningCert = Boolean.parseBoolean(
        		extraParams.getProperty(
        		        AOXMLDSigExtraParams.INCLUDE_ONLY_SIGNNING_CERTIFICATE,
    				Boolean.FALSE.toString()
				)
    		);
			if (!onlySignningCert) {
				certs = certChain;
			}
            if (certs == null) {
                certs = new Certificate[] {
                    cert
                };
            }
            content.add(kif.newX509Data(Arrays.asList(certs)));

            final DOMSignContext signContext = new DOMSignContext(key, rootSig);
            signContext.putNamespacePrefix(XMLConstants.DSIGNNS, xmlSignaturePrefix);
            try {
            	// Instalamos un dereferenciador nuevo que solo actua cuando falla el por defecto
            	signContext.setURIDereferencer(
        			new CustomUriDereferencer(CustomUriDereferencer.getDefaultDereferencer())
    			);
            }
            catch (final Exception e) {
            	LOGGER.warning("No se ha podido instalar un dereferenciador a medida, es posible que fallen las firmas de nodos concretos: " + e); //$NON-NLS-1$
            }

            // en el caso de formato enveloping se inserta el elemento Object
            // con el documento a firmar
            final List<XMLObject> objectList = new ArrayList<>();
            if (isEnveloping(rootSig) && envelopingObject != null) {
                objectList.add(envelopingObject);
            }

            fac.newXMLSignature(
        		fac.newSignedInfo(cm, sm, referenceList), // SignedInfo
                kif.newKeyInfo(content, keyInfoId), // KeyInfo
                objectList,
                signatureId,
                signatureValueId
            ).sign(signContext);

        }
        catch (final NoSuchAlgorithmException e) {
            throw new UnsupportedOperationException(
        		"Hay al menos un algoritmo no soportado: " + e, e //$NON-NLS-1$
    		);
        }
        catch (final Exception e) {
            throw new AOException("Error al generar la cofirma XMLdSig: " + e, e); //$NON-NLS-1$
        }

        return Utils.writeXML(rootSig, originalXMLProperties, null, null);
    }

    /** Cofirma una firma en formato XMLdSig.
     * <p>
     *  Este m&eacute;todo firma todas las referencias a datos declaradas en la firma original,
     *  ya apunten estas a datos, hojas de estilo o cualquier otro elemento. En cada referencia
     *  firmada se introduciran las mismas transformaciones que existiesen en la firma original.
     * </p>
     * <p>
     *  A nivel de formato interno, cuando cofirmamos un documento ya firmado previamente, esta
     *  firma previa no se modifica. Si tenemos en cuenta que XAdES es en realidad un subconjunto
     *  de XMLDSig, el resultado de una cofirma XMLdSig sobre un documento firmado previamente con
     *  XAdES (o viceversa), son dos firmas independientes, una en XAdES y otra en XMLDSig.<br>
     *  Dado que todas las firmas XAdES son XMLDSig pero no todas las firmas XMLDSig son XAdES,
     *  el resultado global de la firma se adec&uacute;a al estandar mas amplio, XMLDSig en este caso.
     * </p>
     * @param sign Firma que se desea cofirmar.
     * @param algorithm Algoritmo a usar para la firma.
     * @param key Clave privada a usar para firmar
     * @param certChain Cadena de certificados del firmante
     * @param xParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>xmlSignaturePrefix</i></b></dt>
     *   <dd>
     *    Prefijo de espacio de nombres XML para los nodos de firma. Si no se especifica este par&aacute;metro
     *    se usa el valor por defecto (<i>ds</i>).
     *   </dd>
     *  <dt><b><i>referencesDigestMethod</i></b></dt>
     *   <dd>Algoritmo de huella digital a usar en las referencias XML</dd>
     *  <dt><b><i>canonicalizationAlgorithm</i></b></dt>
     *   <dd>Algoritmo de canonicalizaci&oacute;n<i>n</i></dd>
     *  <dt><b><i>includeOnlySignningCertificate</i></b></dt>
	 *   <dd>Indica, mediante un {@code true} o {@code false}, que debe
	 *   incluirse en la firma &uacute;nicamente el certificado utilizado
	 *   para firmar y no su cadena de certificaci&oacute;n completa.
	 *   Por defecto, se incluir&aacute; toda la cadena de certificaci&oacute;n.
	 *   </dd>
     * </dl>
     * @return Firma en formato XMLDSig 1.0
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties xParams) throws AOException {

        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // carga la raiz del documento XML de firmas
        // y crea un nuevo documento que contendra solo los datos sin firmar
        Element rootSig;
        Element rootData;
        try {
            rootSig = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign)).getDocumentElement();

            final Document docData = Utils.getNewDocumentBuilder().newDocument();
            rootData = (Element) docData.adoptNode(rootSig.cloneNode(true));

            // Obtiene las firmas y las elimina. Para evitar eliminar firmas de
            // las que cuelgan otras
            // y despues intentar eliminar estas, las buscamos y eliminamos de
            // una en una
            NodeList signatures = rootData.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);
            while (signatures.getLength() > 0) {
                rootData.removeChild(signatures.item(0));
                signatures = rootData.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);
            }

            docData.appendChild(rootData);
        }
        catch (final ParserConfigurationException pcex) {
            throw new AOException("Error en el analizador XML: " + pcex, pcex); //$NON-NLS-1$
        }
        catch (final SAXException saxex) {
            throw new AOException("Formato de documento de firmas (XML firmado de entrada) incorrecto: " + saxex, saxex); //$NON-NLS-1$
        }
        catch (final IOException ioex) {
            throw new AOException("Error al leer el documento de firmas: " + ioex, ioex); //$NON-NLS-1$
        }
        catch (final IllegalArgumentException iaex) {
            throw new AOException("Parametro de entrada incorrecto: " + iaex, iaex); //$NON-NLS-1$
        }

        // convierte el documento de firmas en un InputStream
        final ByteArrayOutputStream baosSig = new ByteArrayOutputStream();
        writeXML(new BufferedWriter(new OutputStreamWriter(baosSig)), rootSig);

        // convierte el documento a firmar en un InputStream
        final ByteArrayOutputStream baosData = new ByteArrayOutputStream();
        writeXML(new BufferedWriter(new OutputStreamWriter(baosData)), rootData);

        return cosign(baosData.toByteArray(), baosSig.toByteArray(), algorithm, key, certChain, xParams);
    }

    /** Contrafirma firmas en formato XMLdSig.
     * <p>
     * Este m&eacute;todo contrafirma los nodos de firma indicados de un documento de firma.
     * </p>
     * @param sign Documento con las firmas iniciales.
     * @param algorithm Algoritmo a usar para la firma.
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
     * @param xParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>encoding</i></b></dt>
     *   <dd>Fuerza la codificaci&oacute;n del XML de salida (utf-8, iso-8859-1,...)</dd>
     *  <dt><b><i>xmlSignaturePrefix</i></b></dt>
     *   <dd>
     *    Prefijo de espacio de nombres XML para los nodos de firma. Si no se especifica este par&aacute;metro
     *    se usa el valor por defecto (<i>ds</i>).
     *   </dd>
     *  <dt><b><i>referencesDigestMethod</i></b></dt>
     *   <dd>Algoritmo de huella digital a usar en las referencias XML</dd>
     *  <dt><b><i>canonicalizationAlgorithm</i></b></dt>
     *   <dd>Algoritmo de canonicalizaci&oacute;n<i>n</i></dd>
     *  <dt><b><i>includeOnlySignningCertificate</i></b></dt>
	 *   <dd>Indica, mediante un {@code true} o {@code false}, que debe
	 *   incluirse en la firma &uacute;nicamente el certificado utilizado
	 *   para firmar y no su cadena de certificaci&oacute;n completa.
	 *   Por defecto, se incluir&aacute; toda la cadena de certificaci&oacute;n.</dd>
     * </dl>
     * @return Contrafirma en formato XMLdSig.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKey key,
                              final Certificate[] certChain,
                              final Properties xParams) throws AOException {

        final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
        if (algoUri == null) {
            throw new UnsupportedOperationException(
        		"La URI de definicion del algoritmo de firma no puede ser nula" //$NON-NLS-1$
    		);
        }

        final Properties extraParams = xParams != null ? xParams : new Properties();

        final String digestMethodAlgorithm = extraParams.getProperty(AOXMLDSigExtraParams.REFERENCES_DIGEST_METHOD, DIGEST_METHOD);
        final String canonicalizationAlgorithm = extraParams.getProperty(AOXMLDSigExtraParams.CANONICALIZATION_ALGORITHM, CanonicalizationMethod.INCLUSIVE);
        String encoding = extraParams.getProperty(AOXMLDSigExtraParams.ENCODING);
        if ("base64".equalsIgnoreCase(encoding)) { //$NON-NLS-1$
            encoding = XMLConstants.BASE64_ENCODING;
        }
        final String xmlSignaturePrefix = extraParams.getProperty(AOXMLDSigExtraParams.XML_SIGNATURE_PREFIX, XML_SIGNATURE_PREFIX);
        final boolean onlySignningCert = Boolean.parseBoolean(
        		extraParams.getProperty(AOXMLDSigExtraParams.INCLUDE_ONLY_SIGNNING_CERTIFICATE, Boolean.FALSE.toString()));

        this.algo = algorithm;

        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // se carga el documento XML y su raiz
        final Map<String, String> originalXMLProperties = new Hashtable<>();
        Element root;
        try {
            this.doc = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign));

            // Tomamos la configuracion del XML que contrafirmamos
            if (encoding == null) {
                encoding = this.doc.getXmlEncoding();
            }

            // Ademas del encoding, sacamos otros datos del doc XML original
            // Hacemos la comprobacion del base64 por si se establecido desde
            // fuera
            if (encoding != null && !XMLConstants.BASE64_ENCODING.equalsIgnoreCase(encoding)) {
                originalXMLProperties.put(OutputKeys.ENCODING, encoding);
            }
            String tmpXmlProp = this.doc.getXmlVersion();
            if (tmpXmlProp != null) {
                originalXMLProperties.put(OutputKeys.VERSION, tmpXmlProp);
            }
            final DocumentType dt = this.doc.getDoctype();
            if (dt != null) {
                tmpXmlProp = dt.getSystemId();
                if (tmpXmlProp != null) {
                    originalXMLProperties.put(OutputKeys.DOCTYPE_SYSTEM, tmpXmlProp);
                }
            }

            root = this.doc.getDocumentElement();

            // si el nodo raiz del documento es una firma simple, se inserta como raiz el
            // nodo AFIRMA

            if (XMLConstants.TAG_SIGNATURE.equals(root.getLocalName()) && XMLConstants.DSIGNNS.equals(root.getNamespaceURI())) {
                this.doc = insertarNodoAfirma(this.doc);
                root = this.doc.getDocumentElement();
            }

            if (targetType == CounterSignTarget.TREE) {
                countersignTree(root, key, certChain, onlySignningCert, digestMethodAlgorithm, canonicalizationAlgorithm, xmlSignaturePrefix);
            }
            else if (targetType == CounterSignTarget.LEAFS) {
                countersignLeafs(root, key, certChain, onlySignningCert, digestMethodAlgorithm, canonicalizationAlgorithm, xmlSignaturePrefix);
            }
            else if (targetType == CounterSignTarget.NODES) {
                countersignNodes(root, targets, key, certChain, onlySignningCert, digestMethodAlgorithm, canonicalizationAlgorithm, xmlSignaturePrefix);
            }
            else if (targetType == CounterSignTarget.SIGNERS) {
                countersignSigners(root, targets, key, certChain, onlySignningCert, digestMethodAlgorithm, canonicalizationAlgorithm, xmlSignaturePrefix);
            }

        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma: " + e, e); //$NON-NLS-1$
        }

        // convierte el xml resultante para devolverlo como byte[]
        return Utils.writeXML(this.doc.getDocumentElement(), originalXMLProperties, null, null);
    }

    /** Realiza la contrafirma de todos los nodos del &aacute;rbol.
     * @param root Elemento ra&iacute;z del documento xml que contiene las firmas.
     * @param key Clave para la firma.
     * @param certChain Cadena de certificados del firmante.
     * @param onlySignningCert Si se establece a <code>true</code> incluye solo el certificado del firmante,
     *                         si se establece a <code>false</code> incluye la cadena completa.
     * @param refsDigestMethod Algoritmo a usar en la huella digital de las referencias internas.
     * @param canonicalizationAlgorithm Algoritmo de <i>canonicalizaci&oacute;n</i> a usar.
     * @param xmlSignaturePrefix Prefijo del espacio de nombres de XMLDSig.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    private void countersignTree(final Element root,
                                 final PrivateKey key,
                                 final Certificate[] certChain,
                                 final boolean onlySignningCert,
                                 final String refsDigestMethod,
                                 final String canonicalizationAlgorithm,
                                 final String xmlSignaturePrefix) throws AOException {

        // obtiene todas las firmas
        final NodeList signatures = root.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);

        final Element[] nodes = new Element[signatures.getLength()];
        for (int i = 0; i < signatures.getLength(); i++) {
            nodes[i] = (Element) signatures.item(i);
        }

        // y crea sus contrafirmas
        for (final Element node : nodes) {
	        try {
	                cs(node, key, certChain, onlySignningCert, refsDigestMethod, canonicalizationAlgorithm, xmlSignaturePrefix);
	        }
	        catch (final Exception e) {
	            throw new AOException("No se ha podido realizar la contrafirma del nodo '" + (node != null ? node.getNodeName() : "nulo") + "': " + e, e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	        }
        }
    }

    /** Realiza la contrafirma de todos los nodos hoja del &aacute;rbol.
     * @param root Elemento ra&iacute;z del documento xml que contiene las firmas
     * @param key Clave para la firma.
     * @param certChain Cadena de certificados del firmante.
     * @param onlySignningCert Si se establece a <code>true</code> incluye solo el certificado del firmante,
     *                         si se establece a <code>false</code> incluye la cadena completa.
     * @param refsDigestMethod Algoritmo a usar en la huella digital de las referencias internas.
     * @param canonicalizationAlgorithm Algoritmo de <i>canonicalizaci&oacute;n</i> a usar.
     * @param xmlSignaturePrefix Prefijo del espacio de nombres de XMLDSig.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    private void countersignLeafs(final Element root,
                                  final PrivateKey key,
                                  final Certificate[] certChain,
                                  final boolean onlySignningCert,
                                  final String refsDigestMethod,
                                  final String canonicalizationAlgorithm,
                                  final String xmlSignaturePrefix) throws AOException {

        // obtiene todas las firmas y las referencias
        final NodeList signatures = root.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);
        final NodeList references = root.getElementsByTagNameNS(XMLConstants.DSIGNNS, REFERENCE_STR);

        // obtenemos el ID de los SignatureValue de cada una de las firmas recuperadas
        final String signatureValueID[] = new String[signatures.getLength()];
        for (int i = 0; i < signatures.getLength(); i++) {
        	signatureValueID[i] = ((Element) signatures.item(i)).
        		getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_VALUE).item(0).
        		getAttributes().getNamedItem(ID_IDENTIFIER).getNodeValue();
        }

        // hojas seran aquellas firmas cuyo SignatureValue no haya sido referenciado
        try {
            for (int i = 0; i < signatureValueID.length; i++) {
                final String refURI = "#" + signatureValueID[i]; //$NON-NLS-1$

                boolean isLeaf = true;
                for (int j = 0; j < references.getLength(); j++) {
                    if (((Element) references.item(j)).getAttribute(URI_STR).equals(refURI)) {
                        isLeaf = false;
                    }
                }

                // y crea sus contrafirmas
                if (isLeaf) {
                    cs(
                		(Element) signatures.item(i),
                		key,
                		certChain,
                		onlySignningCert,
                		refsDigestMethod,
                		canonicalizationAlgorithm,
                		xmlSignaturePrefix
            		);
                }
            }
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma: " + e, e); //$NON-NLS-1$
        }
    }

    /** Realiza la contrafirma de los nodos indicados en el par&aacute;metro
     * targets.
     * @param root Elemento raiz del documento xml que contiene las firmas.
     * @param key Clave para la firma.
     * @param certChain Cadena de certificados del firmante.
     * @param onlySignningCert Si se establece a <code>true</code> incluye solo el certificado del firmante,
     *                         si se establece a <code>false</code> incluye la cadena completa.
     * @param refsDigestMethod Algoritmo a usar en la huella digital de las referencias internas.
     * @param canonicalizationAlgorithm Algoritmo de <i>canonicalizaci&oacute;n</i> a usar.
     * @param xmlSignaturePrefix Prefijo del espacio de nombres de XMLDSig.
     * @param tgts Array con las posiciones de los nodos a contrafirmar.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    private void countersignNodes(final Element root,
    		final Object[] tgts,
    		final PrivateKey key,
    		final Certificate[] certChain,
    		final boolean onlySignningCert,
            final String refsDigestMethod,
    		final String canonicalizationAlgorithm,
    		final String xmlSignaturePrefix) throws AOException {

    	if (tgts == null) {
    		throw new IllegalArgumentException("La lista de nodos a contrafirmar no puede ser nula"); //$NON-NLS-1$
    	}

        // obtiene todas las firmas y las referencias
        final NodeList signatures = root.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);

        // obtenemos el ID de los SignatureValue de cada una de las firmas recuperadas
        final String signatureValuesID[] = new String[signatures.getLength()];
        for (int i = 0; i < signatures.getLength(); i++) {
        	signatureValuesID[i] = ((Element) signatures.item(i)).
        		getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_VALUE).item(0).
        		getAttributes().getNamedItem(ID_IDENTIFIER).getNodeValue();
        }

        // ordenamos los nodos de firma en preorden segun el arbol de firmas
        final List<Element> sortedSignatures = new ArrayList<>(signatures.getLength());
        for (int i = 0; i < signatures.getLength(); i++) {

        	boolean isMainSignature = true;
        	final NodeList references = ((Element) signatures.item(i)).
        		getElementsByTagNameNS(XMLConstants.DSIGNNS, REFERENCE_STR);
        	for (int j = 0; j < references.getLength(); j++) {
        		if (AOXMLDSigSigner.CSURI.equals(((Element)references.item(j)).getAttribute("Type"))) { //$NON-NLS-1$
        			isMainSignature = false;
        			break;
        		}
        	}

        	if (isMainSignature) {
        		sortedSignatures.add((Element) signatures.item(i));
        		addSubNodes(signatureValuesID[i], signatures, signatureValuesID, sortedSignatures);
        	}
        }

        // Recorremos todos los nodos comprobando si su posicion esta entre las seleccionadas
        // como nodo objetivo y, dado el caso, se contrafirma
        final List<Object> targetsList = Arrays.asList(tgts);
    	for (int i = 0; i < sortedSignatures.size(); i++) {
    		if (targetsList.contains(Integer.valueOf(i))) {
    			cs(sortedSignatures.get(i), key, certChain, onlySignningCert, refsDigestMethod, canonicalizationAlgorithm, xmlSignaturePrefix);
    		}
    	}
    }

    /**
     * Funci&oacute;n que busca los nodos de firma que referencian a uno indicado
     * y, recursivamente, a cada uno de estos. Seg&uacute;n los encuentra los inserta en un
     * listado de nodos.
     * @param signatureValueID Identificador del valor de firma para el que se deben buscar las
     * firmas que lo referencian.
     * @param signatures Listado de firmas.
     * @param signatureValuesID Listado de indetificadores de valor de firma ordenados
     * seg&uacute;n el listado de firmas.
     * @param sortedSignatures Listado donde se monta la sucesi&oacute;n ordenada de firmas.
     */
    private void addSubNodes(final String signatureValueID, final NodeList signatures,
			final String[] signatureValuesID, final List<Element> sortedSignatures) {

    	for (int i = 0; i < signatures.getLength(); i++) {
    		final NodeList references = ((Element) signatures.item(i)).getElementsByTagNameNS(
    				XMLConstants.DSIGNNS, REFERENCE_STR);

    		for (int j = 0; j < references.getLength(); j++) {
    			if (("#" + signatureValueID).equals(((Element)references.item(j)).getAttribute(URI_STR))) { //$NON-NLS-1$
    				// Si una firma contiene una referencia a si misma,
    				// rompemos el ciclo para evitar un bucle infinito
    				if (signatureValuesID[i].equals(signatureValueID)) {
    					break;
    				}
    				sortedSignatures.add((Element) signatures.item(i));
    				addSubNodes(signatureValuesID[i], signatures, signatureValuesID, sortedSignatures);
    				break;
    			}
    		}
    	}
	}

	/** Realiza la contrafirma de los firmantes indicados en el par&aacute;metro
     * targets.
     * @param root Elemento ra&iacute;z del documento xml que contiene las firmas.
     * @param targets Array con el nombre de los firmantes de los nodos a contrafirmar.
     * @param key Clave para la firma.
     * @param certChain Cadena de certificados del firmante.
     * @param onlySignningCert Si se establece a <code>true</code> incluye solo el certificado del firmante,
     *                         si se establece a <code>false</code> incluye la cadena completa.
     * @param refsDigestMethod Algoritmo a usar en la huella digital de las referencias internas.
     * @param canonicalizationAlgorithm Algoritmo de <i>canonicalizaci&oacute;n</i> a usar.
     * @param xmlSignaturePrefix Prefijo del espacio de nombres de XMLDSig.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    private void countersignSigners(final Element root,
                                    final Object[] targets,
                                    final PrivateKey key,
                                    final Certificate[] certChain,
                                    final boolean onlySignningCert,
                                    final String refsDigestMethod,
                                    final String canonicalizationAlgorithm,
                                    final String xmlSignaturePrefix) throws AOException {

        // obtiene todas las firmas
        final NodeList signatures = root.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);
        final int numSignatures = signatures.getLength();

        final List<Object> signers = Arrays.asList(targets);
        final List<Element> nodes = new ArrayList<>();

        // obtiene los nodos de los firmantes indicados en targets
        for (int i = 0; i < numSignatures; i++) {
            final Element node = (Element) signatures.item(i);
            if (signers.contains(AOUtil.getCN(Utils.getCertificate(node.getElementsByTagNameNS(XMLConstants.DSIGNNS, "X509Certificate").item(0))))) { //$NON-NLS-1$
                nodes.add(node);
            }
        }

        // y crea sus contrafirmas
        final Iterator<Element> i = nodes.iterator();
        while (i.hasNext()) {
            cs(i.next(), key, certChain, onlySignningCert, refsDigestMethod, canonicalizationAlgorithm, xmlSignaturePrefix);
        }
    }

    /** Realiza la contrafirma de la firma pasada por par&aacute;metro.
     * @param signature Elemento con el nodo de la firma a contrafirmar
     * @param key Clave privada de firma.
     * @param certChain Cadena de certificados del firmante.
     * @param onlySignningCert Indica si debe incluirse solo el certificado de firma o toda la cadena
     * @param refsDigestMethod Algoritmo de huella digital
     * @param canonicalizationAlgorithm Algoritmo de canonicalizaci&oacute;n
     * @param xmlSignaturePrefix Prefijo del namespace de firma
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    private void cs(final Element signature,
                    final PrivateKey key,
                    final Certificate[] certChain,
                    final boolean onlySignningCert,
                    final String refsDigestMethod,
                    final String canonicalizationAlgorithm,
                    final String xmlSignaturePrefix) throws AOException {

        // obtiene el nodo SignatureValue
        final Element signatureValue = (Element) signature.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_VALUE).item(0);

        // crea la referencia a la firma que se contrafirma
        final List<Reference> referenceList = new ArrayList<>();
        final XMLSignatureFactory fac = Utils.getDOMFactory();
        final DigestMethod digestMethod;
        try {
            digestMethod = fac.newDigestMethod(refsDigestMethod, null);
        }
        catch (final Exception e) {
            throw new AOException(
        		"No se ha podido obtener un generador de huellas digitales para el algoritmo '" + refsDigestMethod + "': " + e, e //$NON-NLS-1$ //$NON-NLS-2$
    		);
        }
        final String referenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$

        try {
            // Transformada para la canonicalizacion inclusiva con comentarios
            final List<Transform> transformList = new ArrayList<>();
            transformList.add(fac.newTransform(canonicalizationAlgorithm, (TransformParameterSpec) null));
            referenceList.add(
        		fac.newReference(
            		"#" + signatureValue.getAttribute(ID_IDENTIFIER), //$NON-NLS-1$
            		digestMethod,
            		transformList,
            		CSURI,
            		referenceId
        		)
    		);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido anadir la transformacion de canonizacion en la contrafirma: " + e, e); //$NON-NLS-1$
        }

        // definicion de identificadores
        final String id = UUID.randomUUID().toString();
        final String signatureId = "Signature-" + id; //$NON-NLS-1$
        final String signatureValueId = "SignatureValue-" + id; //$NON-NLS-1$
        final String keyInfoId = "KeyInfo-" + id; //$NON-NLS-1$

        try {

            // se anade una referencia a KeyInfo
            referenceList.add(fac.newReference("#" + keyInfoId, digestMethod)); //$NON-NLS-1$

            // KeyInfo
            final KeyInfoFactory kif = fac.getKeyInfoFactory();
            final X509Certificate cert = (X509Certificate) certChain[0];

            final List<XMLStructure> content = new ArrayList<>();
            content.add(kif.newKeyValue(cert.getPublicKey()));

            // Si se nos ha pedido expresamente que no insertemos la cadena de certificacion,
            // insertamos unicamente el certificado firmante. Tambien lo haremos cuando al
            // recuperar la cadena nos devuelva null
            Certificate[] certs = null;
			if (!onlySignningCert) {
				certs = certChain;
			}
            if (certs == null) {
                certs = new Certificate[] {
                    cert
                };
            }
            content.add(kif.newX509Data(Arrays.asList(certs)));

            final XMLSignature sign = fac.newXMLSignature(
        		fac.newSignedInfo(
    				fac.newCanonicalizationMethod(canonicalizationAlgorithm, (C14NMethodParameterSpec) null),
    				fac.newSignatureMethod(XMLConstants.SIGN_ALGOS_URI.get(this.algo), null),
    				referenceList
				),
        		kif.newKeyInfo(content, keyInfoId),
        		null,
        		signatureId,
        		signatureValueId
    		);

            final DOMSignContext signContext = new DOMSignContext(
        		key,
        		signature.getOwnerDocument().getDocumentElement()
    		);

            signContext.putNamespacePrefix(XMLConstants.DSIGNNS, xmlSignaturePrefix);

            try {
            	// Instalamos un dereferenciador nuevo que solo actua cuando falla el por defecto
            	signContext.setURIDereferencer(
        			new CustomUriDereferencer(CustomUriDereferencer.getDefaultDereferencer())
    			);
            }
            catch (final Exception e) {
            	LOGGER.warning("No se ha podido instalar un dereferenciador a medida, es posible que fallen las firmas de nodos concretos: " + e); //$NON-NLS-1$
            }

            sign.sign(signContext);
        }
        catch (final NoSuchAlgorithmException e) {
            throw new UnsupportedOperationException(
        		"Hay al menos un algoritmo no soportado: " + e, e //$NON-NLS-1$
        	);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido realizar la contrafirma: " + e, e); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
	@Override
	public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {
		return getSignersStructure(sign, null, asSimpleSignInfo);
	}

    /** {@inheritDoc} */
    @Override
	public AOTreeModel getSignersStructure(final byte[] sign, final Properties params, final boolean asSimpleSignInfo) {

        // recupera la raiz del documento de firmas
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        Element root;
        final String completePrefix;
        try {
            this.doc = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign));
            root = this.doc.getDocumentElement();

            // Identificamos el prefijo que se utiliza en los nodos de firma
            final String xmlDSigNSPrefix = XmlDSigUtil.guessXmlDSigNamespacePrefix(root);
            completePrefix = "".equals(xmlDSigNSPrefix) ? "" : xmlDSigNSPrefix + ":"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

            // Si el documento tiene como nodo raiz el nodo de firma, se agrega
            // un nodo raiz previo para que la lectura de las firmas del
            // documento
            // se haga correctamente
            if (root.getNodeName().equals(completePrefix + XMLConstants.TAG_SIGNATURE)) {
                this.doc = insertarNodoAfirma(this.doc);
                root = this.doc.getDocumentElement();
            }
        }
        catch (final Exception e) {
            LOGGER.warning("Se ha producido un error al obtener la estructura de firmas: " + e); //$NON-NLS-1$
            return null;
        }

        final AOTreeNode tree = new AOTreeNode("Datos"); //$NON-NLS-1$

        // Obtenemos todas las firmas y los signature value
        final NodeList signatures = root.getElementsByTagName(completePrefix + XMLConstants.TAG_SIGNATURE);
        final NodeList signatureValues = root.getElementsByTagName(completePrefix + SIGNATURE_VALUE);

        final int numSignatures = signatures.getLength();
        final String[] arrayIds = new String[numSignatures];
        final String[] arrayRef = new String[numSignatures];
        final AOTreeNode[] arrayNodes = new AOTreeNode[numSignatures];

        for (int i = 0; i < numSignatures; i++) {

            final Element signature = (Element) signatures.item(i);

            arrayIds[i] = signature.getAttribute(ID_IDENTIFIER);

            arrayNodes[i] = new AOTreeNode(asSimpleSignInfo ? Utils.getSimpleSignInfoNode(XADESNS, signature) : Utils.getStringInfoNode(signature));

            // Recogemos el identificador de la firma a la que se referencia (si
            // no es contrafirma sera cadena vacia)
            final String typeReference = ((Element) signature.getElementsByTagNameNS(XMLConstants.DSIGNNS, REFERENCE_STR).item(0)).getAttribute("Type"); //$NON-NLS-1$
            if (typeReference.equals(CSURI)) {
                arrayRef[i] = Utils.getCounterSignerReferenceId(signature, signatureValues);
            }
            else {
                arrayRef[i] = ""; //$NON-NLS-1$
            }
        }

        // Se buscan las contrafirmas de cada firma o cofirma
        for (int i = 0; i < numSignatures; i++) {
            if ("".equals(arrayRef[i])) { //$NON-NLS-1$
                tree.add(generaArbol(i, numSignatures - 1, arrayNodes, arrayIds, arrayRef)[i]);
            }
        }

        return new AOTreeModel(tree);
    }

    /** M&eacute;todo recursivo para la obtenci&oacute;n de la estructura de
     * &aacute;rbol.
     * @param i Inicio de lectura del array de identificadores
     * @param j Inicio de lectura inversa del array de referencias
     * @param arrayNodes Array de objetos AOTreeNode
     * @param arrayIds Array de identificadores
     * @param arrayRef Array de referencias
     * @return Array de objetos AOTreeNode */
    private AOTreeNode[] generaArbol(final int i, final int j, final AOTreeNode arrayNodes[], final String arrayIds[], final String arrayRef[]) {
        final int max = arrayIds.length;
        if (i < max && j > 0) {
            if (arrayIds[i].equals(arrayRef[j])) {
                generaArbol(i + 1, j - 1, arrayNodes, arrayIds, arrayRef);
            }
            if (i < j) {
                generaArbol(i, j - 1, arrayNodes, arrayIds, arrayRef);
            }
            if (!arrayIds[i].equals(arrayRef[j])) {
                return arrayNodes;
            }
            generaArbol(j, max - 1, arrayNodes, arrayIds, arrayRef);
            arrayNodes[i].add(arrayNodes[j]);
        }
        return arrayNodes;
    }

    /** {@inheritDoc} */
	@Override
	public boolean isSign(final byte[] sign){
		return isSign(sign, null);
	}

    /** {@inheritDoc} */
    @Override
	public boolean isSign(final byte[] sign, final Properties params) {

        if (sign == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }

        try {
            // Carga el documento a validar
            final Document signDoc = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(sign));
            final Element rootNode = signDoc.getDocumentElement();

            final ArrayList<Node> signNodes = new ArrayList<>();
            if (XMLConstants.TAG_SIGNATURE.equals(rootNode.getLocalName())
            		&& XMLConstants.DSIGNNS.equals(rootNode.getNamespaceURI())) {
                signNodes.add(rootNode);
            }

            final NodeList signatures = rootNode.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);
            for (int i = 0; i < signatures.getLength(); i++) {
                signNodes.add(signatures.item(i));
            }

            // Si no se encuentran firmas, no es un documento de firma
            if (signNodes.size() == 0) {
                return false;
            }

        }
        catch (final Exception e) {
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

    /** Devuelve un nuevo documento con ra&iacute;z "AFIRMA" y conteniendo al
     * documento pasado por par&aacute;metro.
     * @param docu Documento que estar&aacute; contenido en el nuevo documento
     * @return Documento con ra&iacute;z "AFIRMA"
     * @throws ParserConfigurationException Si hay problemas con el analizador XML por defecto. */
    private static Document insertarNodoAfirma(final Document docu) throws ParserConfigurationException {

        // Nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // Crea un nuevo documento con la raiz "AFIRMA"
        final Document docAfirma = Utils.getNewDocumentBuilder().newDocument();
        final Element rootAfirma = docAfirma.createElement(AFIRMA);

        // Inserta el documento pasado por parametro en el nuevo documento
        rootAfirma.appendChild(docAfirma.adoptNode(docu.getDocumentElement()));
        docAfirma.appendChild(rootAfirma);

        return docAfirma;
    }

	@Override
	public AOSignInfo getSignInfo(final byte[] data) throws AOException {
		return getSignInfo(data, null);
	}

    /** {@inheritDoc} */
    @Override
	public AOSignInfo getSignInfo(final byte[] data, final Properties params) throws AOException {
        if (data == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(data)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }

        final AOSignInfo signInfo = new AOSignInfo(AOSignConstants.SIGN_FORMAT_XMLDSIG);

        // Analizamos mas en profundidad la firma para obtener el resto de datos

        // Tomamos la raiz del documento
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        Element rootSig = null;
        try {
            rootSig = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(data)).getDocumentElement();
        }
        catch (final Exception e) {
            LOGGER.warning("Error al analizar la firma: " + e); //$NON-NLS-1$
            rootSig = null;
        }

        // Establecemos la variante de firma
        if (rootSig != null) {
            if (isDetached(rootSig)) {
                signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED);
            }
            else if (isEnveloped(rootSig)) {
                signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED);
            }
            else if (isEnveloping(rootSig)) {
                signInfo.setVariant(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING);
            }
        }

        // Aqui vendria el analisis de la firma buscando alguno de los otros
        // datos de relevancia
        // que se almacenan en el objeto AOSignInfo

        return signInfo;
    }

    /** Escribe el documento especificado al documento dado. La codificaci&oacute;n por
     * defecto es UTF-8.
     * @param writer Clase para la escritura.
     * @param node Nodo ra&iacute;z del XML. */
    private static void writeXML(final Writer writer, final Node node) {
        final Document document = node.getOwnerDocument();
        final DOMImplementationLS domImplLS = (DOMImplementationLS) document.getImplementation();
        final LSSerializer serializer = domImplLS.createLSSerializer();
        serializer.getDomConfig().setParameter("namespaces", Boolean.FALSE); //$NON-NLS-1$
        final DOMOutputImpl output = new DOMOutputImpl();
        output.setCharacterStream(writer);
        serializer.write(node, output);
    }

    /**
     * Busca en un elemento XML el nodo de datos con un ID concreto.
     * @param dataElementIdReference Referencia al elemento que se busca.
     * @param rootElement Elemento XML a partir del cual buscar.
     * @return Elemento XML con los datos o {@code null} si no se encuentra.
     */
    private static Element searchDataElement(final String dataElementIdReference, final Element rootElement) {

    	final String dataElementId = dataElementIdReference.substring(dataElementIdReference.startsWith("#") ? 1 : 0); //$NON-NLS-1$
		Element dataObjectElement = null;

		// Comprobamos si el nodo raiz o sus hijos inmediatos son el nodo de datos
		Node nodeAttributeId = rootElement.getAttributes() != null ? rootElement.getAttributes().getNamedItem(ID_IDENTIFIER) : null;
		if (nodeAttributeId != null && dataElementId.equals(nodeAttributeId.getNodeValue())) {
			dataObjectElement = rootElement;
		}
		else {
			// Recorremos los hijos al reves para acceder antes a los datos y las firmas
			final NodeList rootChildNodes = rootElement.getChildNodes();
			for (int j = rootChildNodes.getLength() - 1; j >= 0; j--) {

				nodeAttributeId = rootChildNodes.item(j).getAttributes() != null ? rootChildNodes.item(j).getAttributes().getNamedItem(ID_IDENTIFIER) : null;
				if (nodeAttributeId != null && dataElementId.equals(nodeAttributeId.getNodeValue())) {
					dataObjectElement = (Element) rootChildNodes.item(j);
					break;
				}

				// Si es un nodo de firma tambien miramos en sus nodos hijos
				if (XMLConstants.TAG_SIGNATURE.equals(rootChildNodes.item(j).getLocalName())) {
					final NodeList subChildsNodes = rootChildNodes.item(j).getChildNodes();
					for (int k = subChildsNodes.getLength() - 1; k >= 0; k--) {
						nodeAttributeId = subChildsNodes.item(k).getAttributes() != null ? subChildsNodes.item(k).getAttributes().getNamedItem(ID_IDENTIFIER) : null;
						if (nodeAttributeId != null && dataElementId.equals(nodeAttributeId.getNodeValue())) {
							dataObjectElement = (Element) subChildsNodes.item(k);
							break;
						}
					}
					if (dataObjectElement != null) {
						break;
					}
				}
			}
		}
    	return dataObjectElement;
    }

}
