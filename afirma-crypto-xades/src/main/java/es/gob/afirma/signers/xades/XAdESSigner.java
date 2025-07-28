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
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Hashtable;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.crypto.URIDereferencer;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.crypto.dsig.spec.XPathFilterParameterSpec;
import javax.xml.parsers.DocumentBuilder;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.misc.http.SSLErrorProcessor;
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.xml.InvalidXMLException;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;
import es.gob.afirma.signers.xml.dereference.CustomUriDereferencer;
import es.gob.afirma.signers.xml.style.CannotDereferenceException;
import es.gob.afirma.signers.xml.style.IsInnerlException;
import es.gob.afirma.signers.xml.style.ReferenceIsNotXmlException;
import es.gob.afirma.signers.xml.style.XmlStyle;
import es.uji.crypto.xades.jxades.security.xml.XAdES.CommitmentTypeIndication;
import es.uji.crypto.xades.jxades.security.xml.XAdES.DataObjectFormat;
import es.uji.crypto.xades.jxades.security.xml.XAdES.DataObjectFormatImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.ObjectIdentifier;
import es.uji.crypto.xades.jxades.security.xml.XAdES.ObjectIdentifierImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdESBase;

/**
 * Firmador simple XAdES.
 * <p>
 * Esta clase, al firmar un XML, firmas tambi&eacute;n sus hojas de
 * estilo XSL asociadas, siguiendo el siguiente criterio:
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
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class XAdESSigner {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

	private static final String FILE_PROTOCOL_PREFIX = "file://"; //$NON-NLS-1$
    private static final String HTTP_PROTOCOL_PREFIX = "http://"; //$NON-NLS-1$
    private static final String HTTPS_PROTOCOL_PREFIX = "https://"; //$NON-NLS-1$

	private XAdESSigner() {
		// No permitimos la instanciacion
	}

	/**
     * Firma datos en formato XAdES.
     * @param data Datos que deseamos firmar.
     * @param signAlgorithm
     *            Algoritmo a usar para la firma.
     * @param certChain Cadena de certificados del firmante.
     * @param pk Clave privada del firmante.
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>).
     * @return Firma en formato XAdES.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     */
	public static byte[] sign(final byte[] data,
			                  final String signAlgorithm,
			                  final PrivateKey pk,
			                  final Certificate[] certChain,
			                  final Properties xParams) throws AOException {
		return sign(data, signAlgorithm, pk, certChain, xParams, null);
	}

    /**
     * Firma datos en formato XAdES.
     * @param data Datos que deseamos firmar.
     * @param signAlgorithm
     *            Algoritmo a usar para la firma.
     * @param certChain Cadena de certificados del firmante.
     * @param pk Clave privada del firmante.
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>).
     * @param uriDereferencer Derreferenciador a medida.
     * @return Firma en formato XAdES.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     */
	public static byte[] sign(final byte[] data,
			                  final String signAlgorithm,
			                  final PrivateKey pk,
			                  final Certificate[] certChain,
			                  final Properties xParams,
			                  final URIDereferencer uriDereferencer) throws AOException {

		final String algorithm = signAlgorithm != null ? signAlgorithm : AOSignConstants.DEFAULT_SIGN_ALGO;
		final Properties extraParams = xParams != null ? xParams : new Properties();

		// Comprobamos que no se hayan configurado opciones incompatibles y, en caso afirmativo,
		// omitimos las que correspondan
		checkParams(algorithm, extraParams);

		final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new IllegalArgumentException(
				"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		// ***********************************************************************************************
		// ********** LECTURA PARAMETROS ADICIONALES *****************************************************


		final boolean avoidXpathExtraTransformsOnEnveloped = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.AVOID_XPATH_EXTRA_TRANSFORMS_ON_ENVELOPED, Boolean.FALSE.toString()));

		final boolean onlySignningCert = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.INCLUDE_ONLY_SIGNNING_CERTIFICATE, Boolean.FALSE.toString()));

		final boolean useManifest = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.USE_MANIFEST, Boolean.FALSE.toString()));

		final String envelopedNodeXPath = extraParams.getProperty(
		        XAdESExtraParams.INSERT_ENVELOPED_SIGNATURE_ON_NODE_BY_XPATH);

		String nodeToSign = extraParams.getProperty(
		        XAdESExtraParams.NODE_TOSIGN);

		final boolean avoidEnveloped = nodeToSign == null ?
			false :
				Boolean.parseBoolean(extraParams.getProperty(
					XAdESExtraParams.AVOID_ENVELOPED_TRANSFORM_WHEN_SIGNING_NODE, Boolean.FALSE.toString()));

		String format = extraParams.getProperty(
		        XAdESExtraParams.FORMAT, AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING);

		final String digestMethodAlgorithm = extraParams.getProperty(
		        XAdESExtraParams.REFERENCES_DIGEST_METHOD, XAdESConstants.DEFAULT_DIGEST_METHOD);

		// Algoritmo de huella usado en las referencias externas (manifest)
		final String externalReferencesHashAlgorithm = extraParams.getProperty(
		        XAdESExtraParams.PRECALCULATED_HASH_ALGORITHM, digestMethodAlgorithm);

		String canonicalizationAlgorithm = extraParams.getProperty(
		        XAdESExtraParams.CANONICALIZATION_ALGORITHM, CanonicalizationMethod.INCLUSIVE);
		if ("none".equalsIgnoreCase(canonicalizationAlgorithm)) { //$NON-NLS-1$
			canonicalizationAlgorithm = null;
		}

		final String xadesNamespace = extraParams.getProperty(
		        XAdESExtraParams.XADES_NAMESPACE, XAdESConstants.DEFAULT_NAMESPACE_XADES);

		final String signedPropertiesTypeUrl = extraParams.getProperty(
		        XAdESExtraParams.SIGNED_PROPERTIES_TYPE_URL, XAdESConstants.REFERENCE_TYPE_SIGNED_PROPERTIES);

		final boolean ignoreStyleSheets = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.IGNORE_STYLE_SHEETS, Boolean.FALSE.toString()));

		final boolean avoidBase64Transforms = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.AVOID_BASE64_TRANSFORMS, Boolean.FALSE.toString()));

		final boolean headless = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.HEADLESS, Boolean.TRUE.toString()));

		final boolean addKeyInfoKeyValue = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.ADD_KEY_INFO_KEY_VALUE, Boolean.TRUE.toString()));

		final boolean addKeyInfoKeyName = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.ADD_KEY_INFO_KEY_NAME, Boolean.FALSE.toString()));

		final boolean addKeyInfoX509IssuerSerial = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.ADD_KEY_INFO_X509_ISSUER_SERIAL, Boolean.FALSE.toString()));

		final boolean facturaeSign = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.FACTURAE_SIGN, Boolean.FALSE.toString()));

		final String outputXmlEncoding = extraParams.getProperty(
		        XAdESExtraParams.OUTPUT_XML_ENCODING);

		String mimeType = extraParams.getProperty(
		        XAdESExtraParams.CONTENT_MIME_TYPE);

		final String oid = extraParams.getProperty(
				XAdESExtraParams.CONTENT_TYPE_OID);

		String encoding = extraParams.getProperty(
		        XAdESExtraParams.CONTENT_ENCODING);

		// Dejamos que indiquen "base64" en vez de la URI, hacemos el cambio manualmente
		if ("base64".equalsIgnoreCase(encoding)) { //$NON-NLS-1$
			encoding = XMLConstants.BASE64_ENCODING;
		}

		// Comprobamos que sea una URI
		if (encoding != null && !encoding.isEmpty()) {
			try {
				new URI(encoding);
			}
			catch(final Exception e) {
				throw new AOException(
					"La codificacion indicada en 'encoding' debe ser una URI: " + e, e //$NON-NLS-1$
				);
			}
		}

		// Comprobamos si se ha indicado validar el PKCS#1 generado (por defecto, si)
		final boolean validatePkcs1 = Boolean.parseBoolean(extraParams.getProperty(
				XAdESExtraParams.INTERNAL_VALIDATE_PKCS1, Boolean.TRUE.toString()));

		// Perfil de firma XAdES que se desea aplicar
		final String profile = extraParams.getProperty(
		        XAdESExtraParams.PROFILE, AOSignConstants.DEFAULT_SIGN_PROFILE);

		// El KeyInfo se firmara salvo que la firma sea Baseline o que se indique
        // expresamente que no se haga
        final boolean keepKeyInfoUnsigned =
                AOSignConstants.SIGN_PROFILE_BASELINE.equalsIgnoreCase(profile)
                || Boolean.parseBoolean(extraParams.getProperty(
                        XAdESExtraParams.KEEP_KEYINFO_UNSIGNED, Boolean.FALSE.toString()));

		// ********** FIN LECTURA PARAMETROS ADICIONALES *************************************************
		// ***********************************************************************************************

		// Las firmas manifest siempre se realizaran sobre firmas Externally Detached
		if (useManifest) {
			format = AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED;
		}

		URI uri = null;
		try {
			uri = extraParams.containsKey(XAdESExtraParams.URI) ?
				AOUtil.createURI(extraParams.getProperty(XAdESExtraParams.URI)) :
					null;
		}
		catch (final Exception e) {
			LOGGER.warning("Se ha pasado una URI invalida como referencia a los datos a firmar: " + e); //$NON-NLS-1$
		}

		Utils.checkIllegalParams(
			format,
			extraParams.getProperty(XAdESExtraParams.MODE),
			useManifest,
			uri,
			externalReferencesHashAlgorithm,
			true
		);

		// Un externally detached con URL permite los datos nulos o vacios
		if ((data == null || data.length == 0)
				&& !AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED.equals(format)) {
			throw new AOException("No se han podido leer los datos a firmar"); //$NON-NLS-1$
		}

		// Propiedades del documento XML original
		Map<String, String> originalXMLProperties = new Hashtable<>();

		// Factoria XML
		DocumentBuilder docBuilder;
		try {
			docBuilder = Utils.getNewDocumentBuilder();
		}
		catch (final Exception e) {
			throw new AOException("No se han podido componer la factoria para la construccion de la firma", e); //$NON-NLS-1$
		}

		// Elemento de datos
		Element dataElement;

		// Documento final de firma
		Document docSignature = null;

		final String contentId = AOXAdESSigner.DETACHED_CONTENT_ELEMENT_NAME + "-" + generateUUID(); //$NON-NLS-1$
		final String styleId = AOXAdESSigner.DETACHED_STYLE_ELEMENT_NAME + "-" + generateUUID(); //$NON-NLS-1$
		boolean isBase64 = false;
		boolean wasEncodedToBase64 = false;
		boolean avoidDetachedContentInclusion = false;

		// Elemento de estilo
		XmlStyle xmlStyle = new XmlStyle();

		// Nodo donde insertar la firma
		Element signatureInsertionNode = null;

		// Intentamos carga el documento como XML
		final Document docum = loadDataAsXml(docBuilder, data);

		// Si los datos son XML
		if (docum != null) {

			// ************************************************
			// **** Obtencion de la hoja de estilo del XML ****
			// ************************************************
			if (!ignoreStyleSheets) {
				try {
					xmlStyle = new XmlStyle(data, headless, false);
				}
				catch (final IsInnerlException ex) {
					LOGGER.info(
						"La hoja de estilo esta referenciada internamente, por lo que no se necesita dereferenciar: " + ex //$NON-NLS-1$
					);
				}
				catch (final ReferenceIsNotXmlException ex) {
					LOGGER.warning(
						"La hoja de estilo referenciada no es XML o no se ha dereferenciado apropiadamente: " + ex //$NON-NLS-1$
					);
				}
				catch (final CannotDereferenceException ex) {
					LOGGER.warning(
						"La hoja de estilo no ha podido dereferenciar, probablemente sea un enlace relativo local: " + ex//$NON-NLS-1$
					);
				}
				catch (final Exception ex) {
					LOGGER.severe(
						"Error intentando dereferenciar la hoja de estilo: " + ex //$NON-NLS-1$
					);
				}
			}
			// ************************************************
			// ** Fin obtencion de la hoja de estilo del XML **
			// ************************************************

			// Si no hay asignado un MimeType o es el por defecto
			// establecemos el de XML
			if (mimeType == null || XMLConstants.DEFAULT_MIMETYPE.equals(mimeType)) {
				mimeType = "text/xml"; //$NON-NLS-1$
			}

			// Obtenemos las propiedades del documento original
			originalXMLProperties = XAdESUtil.getOriginalXMLProperties(docum, outputXmlEncoding);

			// Creamos un elemento raiz nuevo unicamente si es necesario
			if (AOSignConstants.SIGN_FORMAT_XADES_DETACHED.equals(format)) {
				// Si se esta firmando un nodo con un ID concreto, ese nodo es ya el elemento de datos,
				// por lo que no es necesario crear un elemento nuevo, a menos que sea justo el raiz,
				// en cuyo caso no podemos usarlo directamente por la imposibilidad de poner la firma
				// como su hermano, tal y como obliga la normativa
				if (nodeToSign != null) {
						dataElement = CustomUriDereferencer.getElementById(docum, nodeToSign);
						if (!dataElement.isSameNode(docum.getDocumentElement())) {

							// El documento de firma es este mismo
							docSignature = docum;

							// No debemos anadir el contenido, ya que vamos a firmar sobre el mismo,
							// por lo que ya esta en el XML
							avoidDetachedContentInclusion = true;

							// El punto de insercion de la firma debe ser el padre, para que nodo firmado
							// y firma queden como hermanos
							signatureInsertionNode = (Element) dataElement.getParentNode();
						}
				}
				// Si no, creamos un nuevo nodo con identificador para insertar el contenido
				else {
					dataElement = docum.createElement(AOXAdESSigner.DETACHED_CONTENT_ELEMENT_NAME);
					dataElement.setAttributeNS(null, XAdESConstants.ID_IDENTIFIER, contentId);
					dataElement.setAttributeNS(null, AOXAdESSigner.XMLDSIG_ATTR_MIMETYPE_STR, mimeType);
					if (encoding != null && !"".equals(encoding)) { //$NON-NLS-1$
						dataElement.setAttributeNS(null, AOXAdESSigner.XMLDSIG_ATTR_ENCODING_STR, encoding);
					}
					dataElement.appendChild(docum.getDocumentElement());
				}

				// Tambien el estilo
				if (xmlStyle.getStyleElement() != null) {
					try {
						final Element tmpStyleElement = docum.createElement(AOXAdESSigner.DETACHED_STYLE_ELEMENT_NAME);
						tmpStyleElement.setAttributeNS(null, XAdESConstants.ID_IDENTIFIER, styleId);
						if (xmlStyle.getStyleType() != null) {
							tmpStyleElement.setAttributeNS(null, AOXAdESSigner.XMLDSIG_ATTR_MIMETYPE_STR, xmlStyle.getStyleType());
						}
						tmpStyleElement.setAttributeNS(null, AOXAdESSigner.XMLDSIG_ATTR_ENCODING_STR, xmlStyle.getStyleEncoding());
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
			// En cualquier otro caso los datos a firmar son o el XML inicial o el
			// nodo especifico indicado
			else {
				dataElement = docum.getDocumentElement();
			}

		}

		// Los datos no son XML o son datos externos
		else {

			// Error cuando los datos no son XML y se pide firma enveloped o si se pide firmar
			// un nodo XML especifico
			if (AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(format) || nodeToSign != null) {
				throw new InvalidXMLException("Las firmas XAdES Enveloped solo pueden realizarse sobre datos XML"); //$NON-NLS-1$
			}

			// Para los formatos de firma internally detached y enveloping se convierte el documento a base64
			if (AOSignConstants.SIGN_FORMAT_XADES_DETACHED.equals(format) || AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING.equals(format)) {

				LOGGER.fine("El documento se convertira a Base64"); //$NON-NLS-1$

				try {
					// Crea un nuevo nodo XML para contener los datos en base 64
					final Document docFile = docBuilder.newDocument();
					dataElement = docFile.createElement(AOXAdESSigner.DETACHED_CONTENT_ELEMENT_NAME);

					dataElement.setAttributeNS(null, XAdESConstants.ID_IDENTIFIER, contentId);


					// Si es Base64, lo firmamos indicando como contenido el dato pero, ya que puede
					// poseer un formato particular o caracteres valido pero extranos para el XML,
					// realizamos una decodificacion y recodificacion para asi homogenizar el formato.
					if (data != null && Base64.isBase64(data) && XMLConstants.BASE64_ENCODING.equals(encoding)) {

						LOGGER.info(
							"El documento se ha indicado como Base64, se insertara como tal en el XML" //$NON-NLS-1$
						);

						// Adicionalmente, si es un base 64 intentamos obtener el tipo del contenido
						// decodificado para asi reestablecer el MimeType.
						final byte[] decodedData = Base64.decode(data, 0, data.length, false);
						if (mimeType == null) {
							final MimeHelper mimeTypeHelper = new MimeHelper(decodedData);
							final String tempMimeType = mimeTypeHelper.getMimeType();
							if (tempMimeType != null) {
								mimeType = tempMimeType;
							}
						}

						dataElement.setAttributeNS(null, AOXAdESSigner.XMLDSIG_ATTR_MIMETYPE_STR,
								mimeType != null ? mimeType : XMLConstants.DEFAULT_MIMETYPE);

						dataElement.setTextContent(Base64.encode(decodedData));
					}
					else {
						if (XMLConstants.BASE64_ENCODING.equals(encoding)) {
							LOGGER.warning(
								"El documento se ha indicado como Base64, pero no es un Base64 valido. Se convertira a Base64 antes de insertarlo en el XML y se declarara la transformacion" //$NON-NLS-1$
							);
						}
						else {
							LOGGER.fine(
								"El documento se considera binario, se convertira a Base64 antes de insertarlo en el XML y se declarara la transformacion" //$NON-NLS-1$
							);
						}

						if (mimeType == null) {
							final MimeHelper mimeTypeHelper = new MimeHelper(data);
							final String tempMimeType = mimeTypeHelper.getMimeType();
							if (tempMimeType != null) {
								mimeType = tempMimeType;
							}
						}

						dataElement.setAttributeNS(null, AOXAdESSigner.XMLDSIG_ATTR_MIMETYPE_STR,
								mimeType != null ? mimeType : XMLConstants.DEFAULT_MIMETYPE);

						dataElement.setTextContent(Base64.encode(data));
						wasEncodedToBase64 = true;
					}
					isBase64 = true;
					encoding = XMLConstants.BASE64_ENCODING;
					dataElement.setAttributeNS(null, AOXAdESSigner.XMLDSIG_ATTR_ENCODING_STR, encoding);
				}
				catch (final Exception ex) {
					throw new AOException("Error al convertir los datos a Base64", ex); //$NON-NLS-1$
				}
			}

			else {
				// En el resto de formatos (Externally Detached) no hay nodo de datos
				dataElement = null;
			}
		}

		// **********************************************************
		// ********* Fin contenido no XML ***************************
		// **********************************************************

		// ***************************************************
		// ***************************************************

		// La URI de contenido a firmar puede ser el nodo especifico si asi se indico o el
		// nodo de contenido completo
		final String tmpUri = "#" + (nodeToSign != null ? nodeToSign : contentId); //$NON-NLS-1$
		final String tmpStyleUri = "#" + styleId; //$NON-NLS-1$

		// Crea el nuevo documento de firma si no lo tenemos ya
		if (docSignature == null) {
			try {
				docSignature = docBuilder.newDocument();
				if (AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(format)) {
					// En este caso, dataElement es siempre el XML original a firmar
					// (o un nodo de este si asi se especifico)
					docSignature.appendChild(docSignature.adoptNode(dataElement));
				}
				else {
					final Element afirmaRoot = XAdESUtil.getRootElement(docSignature, extraParams);
					docSignature.appendChild(afirmaRoot);
				}
			}
			catch (final Exception e) {
				throw new AOException(
					"Error al crear la firma en formato " + format + ": " + e, e //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}

		final List<Reference> referenceList = new ArrayList<>();
		final XMLSignatureFactory fac = Utils.getDOMFactory();

		final DigestMethod digestMethod;
		try {
			digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
		}
		catch (final Exception e) {
			throw new AOException(
				"No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm + "'", e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		final String referenceId = "Reference-" + generateUUID(); //$NON-NLS-1$
		final String referenceStyleId = "StyleReference-" + generateUUID(); //$NON-NLS-1$

		final List<Transform> transformList = new ArrayList<>();

		// Primero anadimos las transformaciones a medida
		Utils.addCustomTransforms(transformList, extraParams, XAdESConstants.DEFAULT_XML_SIGNATURE_PREFIX);

		final Transform canonicalizationTransform;
		if (canonicalizationAlgorithm != null) {
			try {
				canonicalizationTransform = fac.newTransform(
					canonicalizationAlgorithm,
					(TransformParameterSpec) null
				);
			}
			catch (final Exception e1) {
				throw new AOException(
					"No se ha podido crear el canonizador para el algoritmo indicado (" + canonicalizationAlgorithm + "): " + e1, e1 //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}
		else {
			canonicalizationTransform = null;
		}

		// Si es Base 64 porque tuvimos que convertirlo y no se ha pedido evitar la transformacion,
		// la declaramos
		if (isBase64 && wasEncodedToBase64 && !avoidBase64Transforms) {
			try {
				transformList.add(
					fac.newTransform(
						Transform.BASE64,
						(TransformParameterSpec) null
					)
				);
			}
			catch (final Exception e) {
				throw new AOException(
					"No se puede encontrar el algoritmo transformacion Base64: " + e, e //$NON-NLS-1$
				);
			}
		}

		// Creamos un listado de datos asociadas a las referencias firmadas
		// Esto ahora solo se usa para las firmas manifest
		List<ExternalReferenceData> refDataList = null;

		// crea una referencia al documento insertado en un nodo Object para la
		// firma enveloping y a el estilo
		XMLObject envelopingObject = null;
		XMLObject envelopingStyleObject = null;

		if (AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING.equals(format)) {

			// Si los datos son XML, y tenemos configurada la canicalizacion de los datos,
			// agregamos la transformacion
			if (!isBase64 && canonicalizationTransform != null) {
				transformList.add(canonicalizationTransform);
			}

			try {
				// crea el nuevo elemento Object que contiene el documento a firmar
				final List<XMLStructure> structures = new ArrayList<>(1);

				// Comprobacion de costesia
				if (dataElement == null) {
					throw new IllegalStateException("El elemento (nodo) de datos no puede ser nulo en este punto"); //$NON-NLS-1$
				}

				// Si los datos se han convertido a base64, bien por ser binarios o explicitos
				structures.add(
					new DOMStructure(
						isBase64 ? dataElement.getFirstChild() : dataElement
					)
				);

				final String objectId = "Object-" + generateUUID(); //$NON-NLS-1$
				envelopingObject = fac.newXMLObject(
						structures,
						objectId,
						mimeType != null ? mimeType : XMLConstants.DEFAULT_MIMETYPE,
						encoding);

				// Crea la referencia al nuevo elemento Object o al nodo especifico a firmar
				// si asi se hubiese indicado. El tipo de la referencia sera Object, si no
				// se proporciono un nodo a firmar o nulo si se proporciono.

				referenceList.add(
					fac.newReference(
						"#" + (nodeToSign != null ? nodeToSign : objectId), //$NON-NLS-1$
						digestMethod,
						transformList,
						nodeToSign != null ? null : XMLConstants.OBJURI,
						referenceId
					)
				);

				// ******************************************************************
				// ************** Hojas de estilo ***********************************
				// ******************************************************************
				if (xmlStyle.getStyleElement() != null) {
					final String objectStyleId = "StyleObject-" + generateUUID(); //$NON-NLS-1$
					envelopingStyleObject = fac.newXMLObject(
						Collections.singletonList(
							new DOMStructure(xmlStyle.getStyleElement())
						),
						objectStyleId,
						xmlStyle.getStyleType(),
						xmlStyle.getStyleEncoding()
					);
					referenceList.add(
						fac.newReference(
							"#" + objectStyleId, //$NON-NLS-1$
							digestMethod,
							Collections.singletonList(canonicalizationTransform),
							XMLConstants.OBJURI,
							referenceStyleId
						)
					);

				}
				// ******************************************************************
				// ************** Fin hojas de estilo *******************************
				// ******************************************************************
			}
			catch (final Exception e) {
				throw new AOException(
					"Error al generar la firma en formato enveloping", e //$NON-NLS-1$
				);
			}

			// ******************************************************************
			// *********** Hojas de estilo externas para enveloping *************
			// ******************************************************************
			if (xmlStyle.getStyleHref() != null
					&&  xmlStyle.getStyleElement() == null
					&& (xmlStyle.getStyleHref().startsWith(HTTP_PROTOCOL_PREFIX) ||
						xmlStyle.getStyleHref().startsWith(HTTPS_PROTOCOL_PREFIX))) {
				// Comprobamos Si la referencia al estilo es externa
				try {
					referenceList.add(
						fac.newReference(
							xmlStyle.getStyleHref(),
							digestMethod,
							canonicalizationTransform != null ?
								Collections.singletonList(canonicalizationTransform) :
									new ArrayList<Transform>(0),
							null,
							referenceStyleId
						)
					);
				}
				catch (final Exception e) {
					LOGGER.severe(
						"No ha sido posible anadir la referencia a la hoja de estilo externa del XML para Enveloping, esta no se firmara: " + e //$NON-NLS-1$
					);
				}
			}
			// ******************************************************************
			// ********** Fin hojas de estilo externas para enveloping **********
			// ******************************************************************

		}

		// crea una referencia al documento mediante la URI hacia el
		// identificador del nodo CONTENT o el de datos si ya tenia Id
		else if (AOSignConstants.SIGN_FORMAT_XADES_DETACHED.equals(format)) {

			// Si los datos son XML, y tenemos configurada la canicalizacion de los datos,
			// agregamos la transformacion
			if (!isBase64 && canonicalizationTransform != null) {
				transformList.add(canonicalizationTransform);
			}

			try {
				if (dataElement != null) {
					// Inserta en el nuevo documento de firma el documento a firmar
					// si es nccesario
					if (!avoidDetachedContentInclusion) {
						docSignature.getDocumentElement().appendChild(
							docSignature.adoptNode(dataElement)
						);
					}
					// Crea la referencia a los datos firmados que se encontraran en el mismo
					// documento
					referenceList.add(
						fac.newReference(
							tmpUri, // Puede ser el nodo CONTENT o un nodo concreto dentro del mismo
							digestMethod,
							transformList,
							null, // El tipo referenciado no es Object ni ningun otro
							referenceId
						)
					);
				}

				// ******************************************************************
				// ************** Hojas de estilo ***********************************
				// ******************************************************************
				if (xmlStyle.getStyleElement() != null) {
					// Inserta en el nuevo documento de firma la hoja de estilo
					docSignature.getDocumentElement().appendChild(
						docSignature.adoptNode(xmlStyle.getStyleElement())
					);
					// Crea la referencia a los datos firmados que se encontraran en el mismo documento
					referenceList.add(
						fac.newReference(
							tmpStyleUri,
							digestMethod,
							Collections.singletonList(canonicalizationTransform),
							null, // El nodo de datos es de tipo STYLE
							referenceStyleId
						)
					);
				}
				// ******************************************************************
				// ************** Fin hojas de estilo *******************************
				// ******************************************************************
			}
			catch (final Exception e) {
				throw new AOException(
					"Error al generar la firma en formato detached: " + e, e //$NON-NLS-1$
				);
			}

			// ******************************************************************
			// ************* Hojas de estilo remotas para Detached **************
			// ******************************************************************
			if (xmlStyle.getStyleHref() != null
					&&  xmlStyle.getStyleElement() == null
					&& (xmlStyle.getStyleHref().startsWith(HTTP_PROTOCOL_PREFIX) ||
						xmlStyle.getStyleHref().startsWith(HTTPS_PROTOCOL_PREFIX))) {
				// Comprobamos si la referencia al estilo es externa
				try {
					referenceList.add(
						fac.newReference(
							xmlStyle.getStyleHref(),
							digestMethod,
							canonicalizationTransform != null ?
								Collections.singletonList(canonicalizationTransform) :
									new ArrayList<Transform>(0),
							null,	// Nodo de tipo STYLE
							referenceStyleId
						)
					);
				}
				catch (final Exception e) {
					LOGGER.severe(
						"No ha sido posible anadir la referencia a la hoja remota de estilo del XML para firma Detached, esta no se firmara: " + e //$NON-NLS-1$
					);
				}
			}
			// ******************************************************************
			// *********** Fin hojas de estilo remotas para Detached ************
			// ******************************************************************

		}

		// Firma con referencias externas a los datos. Distinguimos dos casos de uso: referencias
		// directas a los datos y referencias a traves de un manifest. Las referencias se crean a
		// partir de URIs externas y, si se usa manifest o los datos no son derreferenciables, los
		// Message Digest precalculados
		else if (AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED.equals(format)) {

			// Los datos proporcionados son el message digest
			final String digestAlgorithm = externalReferencesHashAlgorithm != null ?
					externalReferencesHashAlgorithm :
						digestMethodAlgorithm;

			final DigestMethod dm;
			try {
				dm = fac.newDigestMethod(
						XAdESUtil.getDigestMethodByCommonName(digestAlgorithm),
						null
						);
			}
			catch (final Exception e) {
				throw new AOException(
						"No se ha podido crear el metodo de huella digital para la referencia a datos externos: " + e, e //$NON-NLS-1$
						);
			}

			//  ********** FIRMA MANIFEST ***********
			if (useManifest) {

				// Cargamos la informacion proporcionada sobre las referencias externas
				final boolean dataIsMessageDigest = externalReferencesHashAlgorithm != null;
				if (!dataIsMessageDigest) {
					LOGGER.warning("No se ha proporcionado la huella asociada a la referencia del parametro '" + XAdESExtraParams.URI  //$NON-NLS-1$
						+ "'. Compruebe proporcionar la huella como datos y el parametro '"  //$NON-NLS-1$
							+ XAdESExtraParams.PRECALCULATED_HASH_ALGORITHM + "' con algoritmo de hash"); //$NON-NLS-1$
					throw new AOException("No se ha proporcionado la huella asociada a la referencia"); //$NON-NLS-1$
				}

				try {
					refDataList = loadManifestReferencesData(
						data,
						referenceId,
						extraParams
					);
				}
				catch (final NoSuchAlgorithmException e) {
					throw new AOException(
						"No se ha podido calcular la huella de los datos proporcionados", //$NON-NLS-1$
						e
					);
				}
				catch (final Exception e) {
					LOGGER.severe("No se han indicado los parametros necesarios ('uri1', 'uri2'... y 'md1', 'md2'...) para construir las referencias a los datos"); //$NON-NLS-1$
					throw new AOException(
						"No se han indicado los parametros necesarios para construir las referencias a los datos", //$NON-NLS-1$
						e
					);
				}


				// Creamos todas las referencias
				for (final ExternalReferenceData refData : refDataList) {
					try {
						referenceList.add(fac.newReference(
							refData.getUri(),
							dm,
							null,
							null, // A las referencias externas no se les pone tipo
							refData.getId(),
							refData.getMessageDigest())
						);
					}
					catch (final Exception e) {
						throw new AOException(
							"Error al generar la firma Manifest al no poder crear una de las referencias externas", //$NON-NLS-1$
							e
						);
					}
				}
			}
			//  ********** FIRMA DE REFERENCIA EXTERNA ***********
			else {

				final String uriString = extraParams.getProperty(XAdESExtraParams.URI);
				final boolean dataIsMessageDigest = externalReferencesHashAlgorithm != null;

				// Cargamos la informacion de la referencia externa
				ExternalReferenceData refData;
				try {
					refData = loadExternalReferenceData(uriString, data, dataIsMessageDigest, digestAlgorithm);
				} catch (final NoSuchAlgorithmException e) {
					throw new AOException(
							"Error al cargar la referencia externa de la firma Externally Detached", //$NON-NLS-1$
							e
							);
				}

				// Si se tiene la huella de la referencia externa, se crea directamente la referencia
				Reference ref;
				if (refData.getMessageDigest() != null) {
					try {
						ref = fac.newReference(
								refData.getUri(),
								dm,
								null,
								null, // A las referencias externas no se les pone tipo
								referenceId,
								refData.getMessageDigest());
					}
					catch (final Exception e) {
						throw new AOException(
								"Error crear la referencia externa de la firma Externally Detached", //$NON-NLS-1$
								e
								);
					}
				}
				// Si no se tiene la huella, habra que crear la referencia a partir de la URI
				else {
					ref = createExternalReferenceFromUri(refData.getUri(), fac, digestMethod, referenceId, extraParams);
				}
				referenceList.add(ref);
			}
		}

		// Crea una referencia indicando que se trata de una firma enveloped
		else if (AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(format)) {
			try {
				if (!avoidEnveloped) {
					// Transformacion enveloped.
					// La enveloped siempre la primera, para que no se quede sin
					// nodos Signature por haber ejecutado antes otra transformacion
					transformList.add(
						fac.newTransform(
							Transform.ENVELOPED,
							(TransformParameterSpec) null
						)
					);
				}

				// Canonicalizamos el XML salvo que sea una factura o que se haya establecido que
				// no se canonicalice
				if (!facturaeSign && canonicalizationTransform != null) {
					transformList.add(canonicalizationTransform);
				}

				// Establecemos que es lo que se firma
				// 1.- Si se especifico un nodo, se firma ese nodo
				// 2.- Si el raiz tiene Id, se firma ese Id
				// 3.- Se firma todo el XML con ""
				if (nodeToSign == null) {
					// Tiene la raiz un Id?
					final String ident = docSignature.getDocumentElement().getAttribute(XAdESConstants.ID_IDENTIFIER);
					if (ident != null && !ident.isEmpty()) {
						nodeToSign = ident;
					}
				}

				// Salvo que sea una factura electronica o que se indique lo contrario
				// se agrega una transformacion XPATH para eliminar el resto de
				// firmas del documento en las firmas Enveloped
				if (!facturaeSign && !avoidXpathExtraTransformsOnEnveloped) {
					transformList.add(
						fac.newTransform(
							Transform.XPATH,
							new XPathFilterParameterSpec(
								"not(ancestor-or-self::" + XAdESConstants.DEFAULT_XML_SIGNATURE_PREFIX + ":Signature)", //$NON-NLS-1$ //$NON-NLS-2$
								Collections.singletonMap(
										XAdESConstants.DEFAULT_XML_SIGNATURE_PREFIX,
									XMLSignature.XMLNS
								)
							)
						)
					);
				}

				// Crea la referencia
				referenceList.add(
					fac.newReference(
						nodeToSign != null ? "#" + nodeToSign : "", //$NON-NLS-1$ //$NON-NLS-2$
						digestMethod,
						transformList,
						null,	// La referencia enveloped no tiene tipo
						referenceId
					)
				);
			}
			catch (final Exception e) {
				throw new AOException(
					"Error al generar la firma en formato enveloped: " + e, e //$NON-NLS-1$
				);
			}

			// *******************************************************
			// ******** Hojas de estilo remotas en Enveloped *********
			// *******************************************************
			if (xmlStyle.getStyleHref() != null
					&&  xmlStyle.getStyleElement() == null
					&& (xmlStyle.getStyleHref().startsWith(HTTP_PROTOCOL_PREFIX)
					||  xmlStyle.getStyleHref().startsWith(HTTPS_PROTOCOL_PREFIX))) {
				// Comprobamos si la referencia al estilo es externa
				try {
					referenceList.add(
						fac.newReference(
							xmlStyle.getStyleHref(),
							digestMethod,
							canonicalizationTransform != null ?
								Collections.singletonList(canonicalizationTransform) :
									new ArrayList<Transform>(0),
							null, // Las referencias externas no tienen tipo
							referenceStyleId
						)
					);
				}
				catch (final Exception e) {
					LOGGER.severe(
						"No ha sido posible anadir la referencia a la hoja de estilo remota del XML para firma Enveloped, esta no se firmara: " + e //$NON-NLS-1$
					);
				}
			}
			// *******************************************************
			// ****** Fin hojas de estilo remotas en Enveloped *******
			// *******************************************************
		}

		// Nodo donde insertar la firma
		if (AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(format)) {
			if (envelopedNodeXPath != null) {
				signatureInsertionNode = XAdESUtil.getFirstElementFromXPath(envelopedNodeXPath, docSignature.getDocumentElement());
			}
			else if (nodeToSign != null) {
				signatureInsertionNode = CustomUriDereferencer.getElementById(docSignature, nodeToSign);
			}
		}

		// Instancia XAdES
		final XAdESBase xades = XAdESUtil.newInstance(
			profile,                           	  			// Perfil XAdES
			xadesNamespace,                       			// XAdES NameSpace
			XAdESConstants.DEFAULT_XADES_SIGNATURE_PREFIX,	// XAdES Prefix
			XAdESConstants.DEFAULT_XML_SIGNATURE_PREFIX,	// XMLDSig Prefix
			digestMethodAlgorithm,                			// DigestMethod
			docSignature,                         			// Document
			signatureInsertionNode != null ?      			// Nodo donde se inserta la firma (como hijo), si no se indica se usa la raiz
				signatureInsertionNode:
					docSignature.getDocumentElement(),
			(X509Certificate) certChain[0]		  			// Certificado de firma
		);

		// Metadatos de firma
		XAdESCommonMetadataUtil.addCommonMetadata(xades, extraParams);

		// DataObjectFormats
		// Si se han proporcionado un listado de referencias con sus datos asociados, se construyen los
		// objetos en base a ellos

		final List<DataObjectFormat> objectFormats = new ArrayList<>();
		if (refDataList != null) {
			for (final ExternalReferenceData refData : refDataList) {
				objectFormats.add(
					createDataObjectFormat(
						refData.getId(),
						refData.getMimeType(),
						refData.getOid(),
						refData.getEncoding()
					)
				);
			}

		}
		else {
			objectFormats.add(createDataObjectFormat(referenceId, mimeType, oid, encoding));
		}


		xades.setDataObjectFormats(objectFormats);

		// CommitmentTypeIndications:
		//  - http://www.w3.org/TR/XAdES/#Syntax_for_XAdES_The_CommitmentTypeIndication_element
		//  - http://uri.etsi.org/01903/v1.2.2/ts_101903v010202p.pdf
		final List<CommitmentTypeIndication> ctis = XAdESUtil.parseCommitmentTypeIndications(extraParams, referenceId);
		if (ctis != null && ctis.size() > 0) {
			xades.setCommitmentTypeIndications(ctis);
		}

		final AOXMLAdvancedSignature xmlSignature = XAdESUtil.getXmlAdvancedSignature(
			xades,
			signedPropertiesTypeUrl,
			digestMethodAlgorithm,
			canonicalizationAlgorithm != null ? canonicalizationAlgorithm : CanonicalizationMethod.INCLUSIVE,
			uriDereferencer
		);

		// en el caso de formato enveloping se inserta el elemento Object con el
		// documento a firmar
		if (AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING.equals(format)) {
			xmlSignature.addXMLObject(envelopingObject);
			if (envelopingStyleObject != null) {
				xmlSignature.addXMLObject(envelopingStyleObject);
			}
		}

		// *******************************************************
		// *********** Hojas de estilo en Enveloped **************
		// *******************************************************
		if (AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(format) && xmlStyle.getStyleElement() != null) {

			// Si es enveloped hay que anadir la hoja de estilo dentro de la firma y
			// referenciarla

			xmlSignature.addStyleSheetEnvelopingOntoSignature(
				xmlStyle,
				styleId
			);

			try {
				referenceList.add(
					fac.newReference(
						tmpStyleUri,
						digestMethod,
						canonicalizationTransform != null ?
							Collections.singletonList(canonicalizationTransform) :
								new ArrayList<Transform>(0),
						XMLConstants.OBJURI, // Es un nodo Object que se firma
						referenceStyleId
					)
				);
			}
			catch (final Exception e) {
				LOGGER.severe(
					"No se ha podido anadir una referencia a la hoja de estilo, esta se incluira dentro de la firma, pero no estara firmada: " + e //$NON-NLS-1$
				);
			}
		}
		// *******************************************************
		// ********* Fin hojas de estilo en Enveloped ************
		// *******************************************************

		// *************************************************************************************
		// ********************************* GESTION MANIFEST **********************************

		if (useManifest) {
			XAdESUtil.createManifest(
				referenceList,
				fac,
				xmlSignature,
				digestMethod,
				canonicalizationTransform,
				referenceId
			);
		}

		// ********************************* FIN GESTION MANIFEST ******************************
		// *************************************************************************************

		// Genera la firma
		try {

			xmlSignature.sign(
				onlySignningCert ?
					Arrays.asList(certChain[0]) :
						Arrays.asList(certChain),
				pk,
				algoUri,
				referenceList,
				"Signature-" + generateUUID(), //$NON-NLS-1$
				addKeyInfoKeyValue,
				addKeyInfoKeyName,
				addKeyInfoX509IssuerSerial,
				keepKeyInfoUnsigned,
				validatePkcs1
			);

		}
		catch (final NoSuchAlgorithmException e) {
			throw new IllegalArgumentException(
				"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "':" + e, e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		catch (final AOException e) {
			throw e;
		}
		catch (final Exception e) {
			throw new AOException("Error al generar la firma XAdES: " + e, e); //$NON-NLS-1$
		}

		// Si se esta realizando una firma enveloping simple no tiene sentido el nodo raiz,
		// asi que sacamos el nodo de firma a un documento aparte
		if (AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING.equals(format)) {
			try {
				if (docSignature.getElementsByTagNameNS(XMLConstants.DSIGNNS,
						XMLConstants.TAG_SIGNATURE).getLength() == 1) {
					final Document newdoc = docBuilder.newDocument();
					newdoc.appendChild(
						newdoc.adoptNode(
							docSignature.getElementsByTagNameNS(
								XMLConstants.DSIGNNS,
								XMLConstants.TAG_SIGNATURE
							).item(0)
						)
					);
					docSignature = newdoc;
				}
			}
			catch (final Exception e) {
				LOGGER.info(
					"No se ha eliminado el nodo padre '<AFIRMA>': " + e //$NON-NLS-1$
				);
			}
		}

		// Si no es enveloped quito los valores del estilo para que no se inserte la
		// cabecera de hoja de estilo
		return Utils.writeXML(
			docSignature.getDocumentElement(),
			originalXMLProperties,
			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(format) ?
				xmlStyle.getStyleHref() :
					null,
			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(format) ?
				xmlStyle.getStyleType() :
					null
		);

	}

	/**
	 * Comprueba que no existan incompatibilidades entre los par&aacute;metros proporcionados
	 * y elimina aquellos que se vayan a ignorar. Tambi&eacute;n muestra advertencias sobre
	 * opciones de configuraci&oacute;n no recomendadas.
	 * @param algorithm Algoritmo de firma.
	 * @param extraParams Par&aacute;metros de configuraci&oacute;n.
	 * @throws IllegalArgumentException Cuando se proporciona una configuraci&oacute;n de firma
	 *                                  no v&aacute;lida e incorregible.
	 */
	private static void checkParams(final String algorithm, final Properties extraParams) {

    	if (algorithm.toUpperCase(Locale.US).startsWith("MD")) { //$NON-NLS-1$
    		throw new IllegalArgumentException("XAdES no permite huellas digitales MD2 o MD5 (Decision 130/2011 CE)"); //$NON-NLS-1$
    	}

		// Comprobacion del perfil de firma y el algoritmo de firma seleccionado
		final String profile = extraParams.getProperty(XAdESExtraParams.PROFILE);
		if (AOSignConstants.SIGN_PROFILE_BASELINE.equalsIgnoreCase(profile)) {
			if (AOSignConstants.isSHA1SignatureAlgorithm(algorithm)) {
				LOGGER.warning("El algoritmo '" + algorithm + "' no esta recomendado para su uso en las firmas baseline"); //$NON-NLS-1$ //$NON-NLS-2$
			}

			final String digestMethodAlgorithm = extraParams.getProperty(
			        XAdESExtraParams.REFERENCES_DIGEST_METHOD);
			if (XMLConstants.URL_SHA1.equals(digestMethodAlgorithm)) {
				LOGGER.warning("El algoritmo SHA1 no esta recomendado para generar referencias en las firmas baseline"); //$NON-NLS-1$
			}
		}

		// Si se pide realizar una firma baseline junto con un espacio de nombres
		// que no lo soporta, se ignorara el espacio de nombres y la URL del tipo
		// de signedProperties que se hubiese indicado
		if (AOSignConstants.SIGN_PROFILE_BASELINE.equalsIgnoreCase(profile) &&
				extraParams.containsKey(XAdESExtraParams.XADES_NAMESPACE) &&
				!XAdESUtil.isBaselineCompatible(extraParams.getProperty(XAdESExtraParams.XADES_NAMESPACE))) {
			LOGGER.warning("Se ha indicado realizar una firma baseline con un espacio de nombres que no lo soporta. " //$NON-NLS-1$
					+ "Se ignorara el espacio de nombres indicado"); //$NON-NLS-1$
			extraParams.remove(XAdESExtraParams.XADES_NAMESPACE);
			extraParams.remove(XAdESExtraParams.SIGNED_PROPERTIES_TYPE_URL);
		}
	}

	/**
	 * Crea un elemento DataObjectFormat para una referencia.
	 * @param referenceId Identificador de la referencia a la que corresponde.
	 * @param mimeType MimeType de los datos.
	 * @param oid Identificador del tipo de objecto.
	 * @param encoding Codificaci&oacute;n de los datos.
	 * @return Elemento con el formato de los datos.
	 */
	private static DataObjectFormat createDataObjectFormat(final String referenceId, final String mimeType, final String oid, final String encoding) {

		// Si no se ha proporcionado un OID, se intenta calcular a partir del MimeType
		String dataOid = oid;
		if (dataOid == null && mimeType != null) {
			try {
				dataOid = MimeHelper.transformMimeTypeToOid(mimeType);
			}
        	catch (final Exception e) {
				LOGGER.warning("Error en la obtencion del OID del tipo de datos a partir del MimeType: " + e); //$NON-NLS-1$
			}
			// Si no se reconoce el MimeType se habra establecido el por defecto. Evitamos este comportamiento
			if (!MimeHelper.DEFAULT_MIMETYPE.equals(mimeType) && MimeHelper.DEFAULT_CONTENT_OID_DATA.equals(dataOid)) {
				dataOid = null;
			}
		}

		// Componemos el objeto ObjectIdentifier
		ObjectIdentifier objectIdentifier = null;
		if (dataOid != null) {
			if (!dataOid.startsWith("urn:oid:")) { //$NON-NLS-1$
				dataOid = "urn:oid:" + dataOid; //$NON-NLS-1$
			}
			objectIdentifier = new ObjectIdentifierImpl(
					"OIDAsURN", //$NON-NLS-1$
					dataOid,
					null,
					new ArrayList<String>(0));
		}

		return new DataObjectFormatImpl(
			null,
			objectIdentifier,
			mimeType != null ? mimeType : XMLConstants.DEFAULT_MIMETYPE,
			encoding,
			"#" + referenceId //$NON-NLS-1$
		);
	}

	/**
	 * Intenta cargar el documento como un XML.
	 * @param docBuilder Procesador de XML.
	 * @param data Datos a cargar.
	 * @return Documento XML o {@code null} si no es tal.
	 */
	private static Document loadDataAsXml(final DocumentBuilder docBuilder, final byte[] data) {
		Document doc;
		if (data != null && AOFileUtils.isXML(data)) {
			try {
				doc = docBuilder.parse(
					new ByteArrayInputStream(data)
				);
			}
			catch (final Exception e) {
				LOGGER.fine("Los datos proporcionados no son XML: " + e); //$NON-NLS-1$
				doc = null;
			}
		}
		else {
			LOGGER.info("Se firman datos externos"); //$NON-NLS-1$
			doc = null;
		}
		return doc;
	}

	/**
	 * Se carga la informacion de una referencia compuesta de una URI y una huella
	 * digital que puede haberse proporcionado, no haberse proporcionado pero si los
	 * datos a partir de los cuales calcularla o no poderse calcular sin acceder a la URI.
	 * @param uri URI a la que se hace referencia.
	 * @param data La huella de los datos, los propios datos a firmar o {@code null} si
	 * hay que calcular la huella a partir de la referencia.
	 * @param isMd Indica si el dato anterior es la huella o los datos.
	 * @param mdAlgorithm Algoritmo de huella que se va a utilizar para las referencias a los datos.
	 * @return Referencia externa con la URI y la huella si se ha podido calcular sin utilizar la URI.
	 * @throws NoSuchAlgorithmException Cuando el algoritmo de huella indicado no es v&aacute;lido
	 * e intenta usarse para calcular la huella de los datos.
	 */
	private static ExternalReferenceData loadExternalReferenceData(
			final String uri,
			final byte[] data,
			final boolean isMd,
			final String mdAlgorithm) throws NoSuchAlgorithmException {

		// Si se proporcionan los datos, estos pueden ser la huella digital a utilizar o
		// los propios datos. Si no es la huella, se calcula la huella a partir de los datos.
		ExternalReferenceData ref;
		if (data != null) {
			if (isMd) {
				ref = new ExternalReferenceData(uri, data);
			}
			else {
				ref = new ExternalReferenceData(
					uri,
					MessageDigest.getInstance(mdAlgorithm).digest(data)
				);
			}
		}
		// Si no se tiene ni la huella ni los datos, no tenemos huella
		else {
			ref = new ExternalReferenceData(uri, null);
		}

		return ref;
	}

	/**
	 * Carga el listado de referencias a los datos que se van a firmar. De cada referencia
	 * se intenta cargar la huella digital asociada, calcul&aacute;ndola a partir de los datos
	 * proporcionados si es posible. Durante este proceso no se accede a URI externas para
	 * calcular las huellas. Eso s&oacute;lo se har&aacute;a cuando no se use manifest.
	 * @param digest La huella de los datos referenciados o {@code null} desde los extraParams
	 *               se pueden obtener m&uacute;ltiples referncias con la
	 * @param referenceId Indica si el dato anterior es la huella o los datos.
	 * @param extraParams Configuraci&oacute;n de firma.
	 * @return Listado de referencias externas.
	 * @throws AOException Cuando se encuentra un defecto en los par&aacute;metros proporcionados.
	 * @throws NoSuchAlgorithmException Cuando el algoritmo de huella indicado no es v&aacute;lido
	 *                                  e intenta usarse para calcular la huella de los datos.
	 */
	private static List<ExternalReferenceData> loadManifestReferencesData(
			final byte[] digest,
			final String referenceId,
			final Properties extraParams) throws NoSuchAlgorithmException, AOException {

		final List<ExternalReferenceData> refsList = new ArrayList<>();

		// Si se proporciona el parametro URI entenderemos que es una
		// operacion retrocompatible en la que se proporcionaban los datos
		// o su huella a trav&eacute;s del par&aacute;metro {@code data}.
		if (extraParams.containsKey(XAdESExtraParams.URI)) {
			if (digest == null) {
				throw new AOException(
					"No se ha proporcionado en el parametro de datos la huella correspondiente a los datos referenciados por la URI" //$NON-NLS-1$
				);
			}
			final String uri = extraParams.getProperty(XAdESExtraParams.URI);
			final String mimeType = extraParams.getProperty(XAdESExtraParams.CONTENT_MIME_TYPE);
			final String oid = extraParams.getProperty(XAdESExtraParams.CONTENT_TYPE_OID);
			final String encoding = extraParams.getProperty(XAdESExtraParams.CONTENT_ENCODING);

			final ExternalReferenceData refData = new ExternalReferenceData(uri, digest);
			refData.setId(referenceId);
			refData.setMimeType(mimeType);
			refData.setOid(oid);
			refData.setEncoding(encoding);
			refsList.add(refData);
		}
		else {
			int i = 1;
			while (extraParams.containsKey(XAdESExtraParams.URI_PREFIX + i)) {
				if (!extraParams.containsKey(XAdESExtraParams.MD_PREFIX + i)) {
					throw new AOException(
						String.format("No se ha indicado la huella de la referencia %d del manifest", Integer.valueOf(i)) //$NON-NLS-1$
					);
				}
				final byte[] md;
				try {
					md = Base64.decode(extraParams.getProperty(XAdESExtraParams.MD_PREFIX + i));
				}
				catch (final Exception e) {
					LOGGER.log(Level.WARNING, "Se ha indicado un base 64 no valido como huella de la referencia " + i, e); //$NON-NLS-1$
					throw new AOException(
						String.format("Se ha indicado un base 64 no valido como huella de la referencia %d del manifest", Integer.valueOf(i)), //$NON-NLS-1$
						e
					);
				}
				final ExternalReferenceData refData = new ExternalReferenceData(
					extraParams.getProperty(XAdESExtraParams.URI_PREFIX + i),
					md
				);

				refData.setId("Reference-" + generateUUID()); //$NON-NLS-1$
				refData.setMimeType(extraParams.getProperty(XAdESExtraParams.CONTENT_MIME_TYPE_PREFIX + i));
				refData.setOid(extraParams.getProperty(XAdESExtraParams.CONTENT_TYPE_OID_PREFIX + i));
				refData.setEncoding(extraParams.getProperty(XAdESExtraParams.CONTENT_ENCODING_PREFIX + i));

				refsList.add(refData);
				i++;
			}

			if (refsList.size() == 0) {
				throw new AOException("No se han proporcionado las referencias y huellas de los datos a firmar"); //$NON-NLS-1$
			}
		}

		return refsList;
	}

	/**
	 * Crea una referencia a datos externos.
	 * @param uri URI a los datos externos.
	 * @param fac Factor&iacute;a de firmas XML.
	 * @param digestMethod Identificador del algoritmo de huella digital.
	 * @param referenceId Identificador que se asignara a la nueva referencia.
	 * @param extraParams Configuraci&oacute;n de la operaci&oacute;n de firma.
	 * @return Referencia a datos.
	 * @throws AOException Cuando falla la creaci&oacute;n de la referencia.
	 */
	private static Reference createExternalReferenceFromUri(
			final String uri,
			final XMLSignatureFactory fac,
			final DigestMethod digestMethod,
			final String referenceId,
			final Properties extraParams) throws AOException {

		final Reference ref;

		// Si es una referencia de tipo file:// obtenemos el fichero y
		// creamos una referencia solo con el message digest
		if (FILE_PROTOCOL_PREFIX.equalsIgnoreCase(uri.substring(0, FILE_PROTOCOL_PREFIX.length()))) {
			try (
				final InputStream lfis = AOUtil.loadFile(AOUtil.createURI(uri))
			) {
				ref = fac.newReference(
					uri,
					digestMethod,
					null,
					null, // Las referencias externas no tienen tipo
					referenceId,
					MessageDigest.getInstance(
						AOSignConstants.getDigestAlgorithmName(
							digestMethod.getAlgorithm()
						)
					).digest(AOUtil.getDataFromInputStream(lfis))
				);
			}
			catch (final Exception e) {
				throw new AOException(
					"No se ha podido crear la referencia XML a partir de la URI local (" + uri + "): " + e, e //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}

		// Dereferenciamos las URL de tipo HTTP/HTTPS
		else if (HTTP_PROTOCOL_PREFIX.equalsIgnoreCase(uri.substring(0, HTTP_PROTOCOL_PREFIX.length())) ||
				HTTPS_PROTOCOL_PREFIX.equalsIgnoreCase(uri.substring(0, HTTPS_PROTOCOL_PREFIX.length()))) {
			try {

				final UrlHttpManager httpManager = UrlHttpManagerFactory.getInstalledManager();

				byte[] data;
				final SSLErrorProcessor errorProcessor = new SSLErrorProcessor(extraParams);
				try {
					data = httpManager.readUrl(uri, UrlHttpMethod.GET, errorProcessor);
				} catch (final IOException e) {
					if (errorProcessor.isCancelled()) {
						LOGGER.info(
								"El usuario no permite la importacion del certificado SSL de confianza de un recurso externo en: " //$NON-NLS-1$
								+ LoggerUtil.getTrimStr(uri));
					}
					throw new AOException("Error en la recuperacion de un recurso externo: " + e, e); //$NON-NLS-1$
				}

				final byte[] md = MessageDigest.getInstance(AOSignConstants.getDigestAlgorithmName(digestMethod.getAlgorithm()))
						.digest(data);

				ref = fac.newReference(
					uri,
					digestMethod,
					null,
					null, // Las referencias externas no tienen tipo
					referenceId,
					md
				);
			}
			catch (final Exception e) {
				throw new AOException(
					"No se ha podido crear la referencia XML a partir de la URI local (" + uri + "): " + e, e //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}

		// Si es una referencia distinta de file:// suponemos que es dereferenciable de forma universal
		// por lo que dejamos que Java lo haga todo
		else {
			try {
				ref = fac.newReference(uri, digestMethod, null, null, referenceId);
			}
			catch (final Exception e) {
				throw new AOException(
					"No se ha podido crear la referencia Externally Detached, probablemente por no obtenerse el metodo de digest: " + e, e //$NON-NLS-1$
				);
			}
		}
		return ref;
	}

	/**
	 * Genera un identificador aleatorio.
	 * @return Identificador aleatorio.
	 */
	private static String generateUUID() {
		return UUID.randomUUID().toString();
	}
}
