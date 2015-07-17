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
import java.util.LinkedList;
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
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.crypto.dsig.spec.XPathFilterParameterSpec;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;

import net.java.xades.security.xml.XAdES.CommitmentTypeIndication;
import net.java.xades.security.xml.XAdES.DataObjectFormat;
import net.java.xades.security.xml.XAdES.DataObjectFormatImpl;
import net.java.xades.security.xml.XAdES.ObjectIdentifierImpl;
import net.java.xades.security.xml.XAdES.XAdES;
import net.java.xades.security.xml.XAdES.XAdES_EPES;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.xml.CustomUriDereferencer;
import es.gob.afirma.signers.xml.InvalidXMLException;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;
import es.gob.afirma.signers.xml.style.CannotDereferenceException;
import es.gob.afirma.signers.xml.style.IsInnerlException;
import es.gob.afirma.signers.xml.style.ReferenceIsNotXmlException;
import es.gob.afirma.signers.xml.style.XmlStyle;

/** Firmador simple XAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class XAdESSigner {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

    private static final String HTTP_PROTOCOL_PREFIX = "http://"; //$NON-NLS-1$
    private static final String HTTPS_PROTOCOL_PREFIX = "https://"; //$NON-NLS-1$

    /** Identificador de identificadores en los nodos XML. */
    static final String ID_IDENTIFIER = "Id"; //$NON-NLS-1$

    private static final String EXTRAPARAM_URI ="uri"; //$NON-NLS-1$

	/** Firma datos en formato XAdES.
	 * <p>
	 * Este m&eacute;todo, al firmar un XML, firmas tambi&eacute;n sus hojas de
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
	 * @param data Datos que deseamos firmar.
	 * @param algorithm
	 *            Algoritmo a usar para la firma.
	 *            <p>
	 *              Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:
	 *            </p>
	 *            <ul>
	 *              <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
	 *              <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
	 *              <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
	 *              <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
	 *            </ul>
	 * @param certChain Cadena de certificados del firmante
	 * @param pk Clave privada del firmante
	 * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
	 * @return Firma en formato XAdES
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	public static byte[] sign(final byte[] data,
			                  final String algorithm,
			                  final PrivateKey pk,
			                  final Certificate[] certChain,
			                  final Properties xParams) throws AOException {

		final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new UnsupportedOperationException(
				"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		// ***********************************************************************************************
		// ********** LECTURA PARAMETROS ADICIONALES *****************************************************

		final Properties extraParams = xParams != null ? xParams : new Properties();

		final boolean avoidXpathExtraTransformsOnEnveloped = Boolean.parseBoolean(extraParams.getProperty(
				"avoidXpathExtraTransformsOnEnveloped", Boolean.FALSE.toString())); //$NON-NLS-1$

		final boolean onlySignningCert = Boolean.parseBoolean(extraParams.getProperty(
				"includeOnlySignningCertificate", Boolean.FALSE.toString())); //$NON-NLS-1$

		final boolean useManifest = Boolean.parseBoolean(extraParams.getProperty(
				"useManifest", Boolean.FALSE.toString())); //$NON-NLS-1$

		final String envelopedNodeXPath = extraParams.getProperty(
				"insertEnvelopedSignatureOnNodeByXPath"); //$NON-NLS-1$

		String nodeToSign = extraParams.getProperty(
				"nodeToSign"); //$NON-NLS-1$

		final String format = extraParams.getProperty(
				"format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$

		final String digestMethodAlgorithm = extraParams.getProperty(
				"referencesDigestMethod", AOXAdESSigner.DIGEST_METHOD); //$NON-NLS-1$

		final String canonicalizationAlgorithm = extraParams.getProperty(
				"canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE); //$NON-NLS-1$

		final String xadesNamespace = extraParams.getProperty(
				"xadesNamespace", AOXAdESSigner.XADESNS); //$NON-NLS-1$

		final String signedPropertiesTypeUrl = extraParams.getProperty(
				"signedPropertiesTypeUrl", AOXAdESSigner.XADES_SIGNED_PROPERTIES_TYPE); //$NON-NLS-1$

		final boolean ignoreStyleSheets = Boolean.parseBoolean(extraParams.getProperty(
				"ignoreStyleSheets", Boolean.FALSE.toString())); //$NON-NLS-1$

		final boolean avoidBase64Transforms = Boolean.parseBoolean(extraParams.getProperty(
				"avoidBase64Transforms", Boolean.FALSE.toString())); //$NON-NLS-1$

		final boolean headless = Boolean.parseBoolean(extraParams.getProperty(
				"headless", Boolean.TRUE.toString())); //$NON-NLS-1$

		final boolean addKeyInfoKeyValue = Boolean.parseBoolean(extraParams.getProperty(
				"addKeyInfoKeyValue", Boolean.TRUE.toString())); //$NON-NLS-1$

		final boolean addKeyInfoKeyName = Boolean.parseBoolean(extraParams.getProperty(
				"addKeyInfoKeyName", Boolean.FALSE.toString())); //$NON-NLS-1$

		final String precalculatedHashAlgorithm = extraParams.getProperty(
				"precalculatedHashAlgorithm"); //$NON-NLS-1$

		final boolean facturaeSign = Boolean.parseBoolean(extraParams.getProperty(
				"facturaeSign", Boolean.FALSE.toString())); //$NON-NLS-1$

		String mimeType = extraParams.getProperty(
				"mimeType", XMLConstants.DEFAULT_MIMETYPE); //$NON-NLS-1$

		String encoding = extraParams.getProperty(
				"encoding"); //$NON-NLS-1$

		if ("base64".equalsIgnoreCase(encoding)) { //$NON-NLS-1$
			encoding = XMLConstants.BASE64_ENCODING;
		}

		// ********** FIN LECTURA PARAMETROS ADICIONALES *************************************************
		// ***********************************************************************************************

		URI uri = null;
		try {
			uri = AOUtil.createURI(extraParams.getProperty(EXTRAPARAM_URI)) != null ?
					AOUtil.createURI(extraParams.getProperty(EXTRAPARAM_URI)) :
						null;
		}
		catch (final Exception e) {
			LOGGER.warning("Se ha pasado una URI invalida como referencia a los datos a firmar: " + e); //$NON-NLS-1$
		}

		Utils.checkIllegalParams(format, AOSignConstants.SIGN_MODE_IMPLICIT, uri, precalculatedHashAlgorithm, true);

		// Un externally detached con URL permite los datos nulos o vacios
		if ((data == null || data.length == 0)
				&& !(format.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED) && uri != null)) {
			throw new AOException("No se han podido leer los datos a firmar"); //$NON-NLS-1$
		}

		// Propiedades del documento XML original
		final Map<String, String> originalXMLProperties = new Hashtable<String, String>();

		// Factoria XML
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		// Elemento de datos
		Element dataElement;

		// Documento final de firma
		Document docSignature = null;

		final String contentId = AOXAdESSigner.DETACHED_CONTENT_ELEMENT_NAME + "-" + UUID.randomUUID().toString(); //$NON-NLS-1$
		final String styleId = AOXAdESSigner.DETACHED_STYLE_ELEMENT_NAME + "-" + UUID.randomUUID().toString(); //$NON-NLS-1$
		boolean isBase64 = false;
		boolean wasEncodedToBase64 = false;
		boolean avoidDetachedContentInclusion = false;

		// Elemento de estilo
		XmlStyle xmlStyle = new XmlStyle();

		// Nodo donde insertar la firma
		Element signatureInsertionNode = null;

		try {

			// Obtenemos el objeto XML
			final Document docum = dbf.newDocumentBuilder().parse(
				new ByteArrayInputStream(data)
			);

			// ************************************************
			// **** Obtencion de la hoja de estilo del XML ****
			// ************************************************
			if (!ignoreStyleSheets) {
				try {
					xmlStyle = new XmlStyle(data, headless);
				}
				catch (final IsInnerlException ex) {
					LOGGER.info(
						"La hoja de estilo esta referenciada internamente, por lo que no se necesita dereferenciar" //$NON-NLS-1$
					);
				}
				catch (final ReferenceIsNotXmlException ex) {
					LOGGER.warning(
						"La hoja de estilo referenciada no es XML o no se ha dereferenciado apropiadamente" //$NON-NLS-1$
					);
				}
				catch (final CannotDereferenceException ex) {
					LOGGER.warning(
						"La hoja de estilo no ha podido dereferenciar, probablemente sea un enlace relativo local" //$NON-NLS-1$
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

			// Obtenemos el encoding del documento original
			if (encoding == null) {
				encoding = docum.getXmlEncoding();
			}

			// Hacemos la comprobacion del Base64 por si se establecido desde fuera
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

			// Creamos un elemento raiz nuevo unicamente si es necesario
			if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_DETACHED)) {
				// Si se esta firmando un nodo con un ID concreto, ese nodo es ya el elemento de datos,
				// por lo que no es necesario crear un elemento nuevo, a menos que sea justo el raiz,
				// en cuyo caso no podemos usarlo directamente por la imposibilidad de poner la firma
				// como su hermano, tal y como obliga la normativa
				if (nodeToSign != null) {
						dataElement = CustomUriDereferencer.getElementById(docum, nodeToSign);
						if (!CustomUriDereferencer.getElementById(docum, nodeToSign).isSameNode(docum.getDocumentElement())) {

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
					dataElement.setAttributeNS(null, ID_IDENTIFIER, contentId);
					dataElement.setAttributeNS(null, AOXAdESSigner.MIMETYPE_STR, mimeType);
					if (encoding != null && !"".equals(encoding)) { //$NON-NLS-1$
						dataElement.setAttributeNS(null, AOXAdESSigner.ENCODING_STR, encoding);
					}
					dataElement.appendChild(docum.getDocumentElement());
				}

				// Tambien el estilo
				if (xmlStyle.getStyleElement() != null) {
					try {
						final Element tmpStyleElement = docum.createElement(AOXAdESSigner.DETACHED_STYLE_ELEMENT_NAME);
						tmpStyleElement.setAttributeNS(null, ID_IDENTIFIER, styleId);
						if (xmlStyle.getStyleType() != null) {
							tmpStyleElement.setAttributeNS(null, AOXAdESSigner.MIMETYPE_STR, xmlStyle.getStyleType());
						}
						tmpStyleElement.setAttributeNS(null, AOXAdESSigner.ENCODING_STR, xmlStyle.getStyleEncoding());
						tmpStyleElement.appendChild(docum.adoptNode(xmlStyle.getStyleElement().cloneNode(true)));
						xmlStyle.setStyleElement(tmpStyleElement);
					}
					catch (final Exception e) {
						XAdESSigner.LOGGER.warning(
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
		// Captura de error en caso de no ser un documento XML
		// **********************************************************
		// ********* Contenido no XML *******************************
		// **********************************************************
		catch (final Exception e) {

			// Error cuando los datos no son XML y se pide firma enveloped o si se pide firmar
			// un nodo XML especifico
			if (AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(format) || nodeToSign != null) {
				throw new InvalidXMLException(e);
			}

			// Para los formatos de firma internally detached y enveloping se trata de convertir el documento a base64
			if (AOSignConstants.SIGN_FORMAT_XADES_DETACHED.equals(format) || AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING.equals(format)) {

				XAdESSigner.LOGGER.info(
					"El documento no es un XML valido. Se convertira a Base64: " + e //$NON-NLS-1$
				);

				try {
					// Crea un nuevo nodo XML para contener los datos en base 64
					final Document docFile = dbf.newDocumentBuilder().newDocument();
					dataElement = docFile.createElement(AOXAdESSigner.DETACHED_CONTENT_ELEMENT_NAME);
					uri = null;
					if (mimeType == null) {
						mimeType = XMLConstants.DEFAULT_MIMETYPE;
					}

					dataElement.setAttributeNS(null, ID_IDENTIFIER, contentId);
					dataElement.setAttributeNS(null, AOXAdESSigner.MIMETYPE_STR, mimeType);

					// Si es base 64, lo firmamos indicando como contenido el dato pero, ya que puede
					// poseer un formato particular o caracteres valido pero extranos para el XML,
					// realizamos una decodificacion y recodificacion para asi homogenizar el formato.
					if (AOUtil.isBase64(data)
							&& (XMLConstants.BASE64_ENCODING.equals(encoding) || "base64".equalsIgnoreCase(encoding))) { //$NON-NLS-1$
						XAdESSigner.LOGGER.info(
							"El documento se ha indicado como Base64, se insertara como tal en el XML" //$NON-NLS-1$
						);

						// Adicionalmente, si es un base 64 intentamos obtener el tipo del contenido
						// decodificado para asi reestablecer el MimeType.
						final byte[] decodedData = Base64.decode(new String(data));
						final MimeHelper mimeTypeHelper = new MimeHelper(decodedData);
						final String tempMimeType = mimeTypeHelper.getMimeType();
						mimeType = tempMimeType != null ? tempMimeType : XMLConstants.DEFAULT_MIMETYPE;
						dataElement.setAttributeNS(null, AOXAdESSigner.MIMETYPE_STR, mimeType);
						dataElement.setTextContent(Base64.encode(decodedData));
					}
					else {
						if (XMLConstants.BASE64_ENCODING.equals(encoding)) {
							XAdESSigner.LOGGER.info(
								"El documento se ha indicado como Base64, pero no es un Base64 valido. Se convertira a Base64 antes de insertarlo en el XML y se declarara la transformacion" //$NON-NLS-1$
							);
						}
						else {
							XAdESSigner.LOGGER.info(
								"El documento se considera binario, se convertira a Base64 antes de insertarlo en el XML y se declarara la transformacion" //$NON-NLS-1$
							);
						}

						if (mimeType == XMLConstants.DEFAULT_MIMETYPE) {
							final MimeHelper mimeTypeHelper = new MimeHelper(data);
							final String tempMimeType = mimeTypeHelper.getMimeType();
							mimeType = tempMimeType != null ? tempMimeType : XMLConstants.DEFAULT_MIMETYPE;
							dataElement.setAttributeNS(null, AOXAdESSigner.MIMETYPE_STR, mimeType);
						}

						dataElement.setTextContent(Base64.encode(data));
						wasEncodedToBase64 = true;
					}
					isBase64 = true;
					encoding = XMLConstants.BASE64_ENCODING;
					dataElement.setAttributeNS(null, AOXAdESSigner.ENCODING_STR, encoding);
				}
				catch (final Exception ex) {
					throw new AOException("Error al convertir los datos a base64", ex); //$NON-NLS-1$
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
				docSignature = dbf.newDocumentBuilder().newDocument();
				if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED)) {
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

		final List<Reference> referenceList = new ArrayList<Reference>();
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
		final String referenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$
		final String referenceStyleId = AOXAdESSigner.STYLE_REFERENCE_PREFIX + UUID.randomUUID().toString();

		final List<Transform> transformList = new ArrayList<Transform>();

		// Primero anadimos las transformaciones a medida
		Utils.addCustomTransforms(transformList, extraParams, AOXAdESSigner.XML_SIGNATURE_PREFIX);

		final Transform canonicalizationTransform;
		try {
			canonicalizationTransform = fac.newTransform(
				canonicalizationAlgorithm,
				(TransformParameterSpec) null
			);
		}
		catch (final Exception e1) {
			throw new AOException(
				"No se ha posido crear el canonizador para el algoritmo indicado (" + canonicalizationAlgorithm + "): " + e1, e1 //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		// Solo canonicalizo si es XML
		if (!isBase64) {
			// Las facturas electronicas no se canonicalizan
			if (!facturaeSign) {
				try {
					// Transformada para la canonicalizacion inclusiva
					transformList.add(canonicalizationTransform);
				}
				catch (final Exception e) {
					throw new AOException(
						"No se puede encontrar el algoritmo de canonicalizacion: " + e, e //$NON-NLS-1$
					);
				}
			}
		}
		// Si no era XML y tuve que convertir a Base64 yo mismo declaro la
		// transformacion
		else if (wasEncodedToBase64 && !avoidBase64Transforms) {
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

		// crea una referencia al documento insertado en un nodo Object para la
		// firma enveloping y a el estilo
		XMLObject envelopingObject = null;
		XMLObject envelopingStyleObject = null;

		if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)) {
			try {
				// crea el nuevo elemento Object que contiene el documento a firmar
				final List<XMLStructure> structures = new ArrayList<XMLStructure>(1);

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

				final String objectId = "Object-" + UUID.randomUUID().toString(); //$NON-NLS-1$
				envelopingObject = fac.newXMLObject(structures, objectId, mimeType, encoding);

				// Crea la referencia al nuevo elemento Object o al nodo especifico a firmar
				// si asi se hubiese indicado
				referenceList.add(
					fac.newReference(
						"#" + (nodeToSign != null ? nodeToSign : objectId), //$NON-NLS-1$
						digestMethod,
						transformList,
						XMLConstants.OBJURI,
						referenceId
					)
				);

				// ******************************************************************
				// ************** Hojas de estilo ***********************************
				// ******************************************************************
				if (xmlStyle.getStyleElement() != null) {
					final String objectStyleId = "StyleObject-" + UUID.randomUUID().toString(); //$NON-NLS-1$
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
				// ************** Fin hojas de estilo ***********************************
				// ******************************************************************
			}
			catch (final Exception e) {
				throw new AOException(
					"Error al generar la firma en formato enveloping", e //$NON-NLS-1$
				);
			}

			// ******************************************************************
			// ***** Hojas de estilo para enveloping en Externally Detached *****
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
							Collections.singletonList(canonicalizationTransform),
							XMLConstants.OBJURI,
							referenceStyleId
						)
					);
				}
				catch (final Exception e) {
					XAdESSigner.LOGGER.severe(
						"No ha sido posible anadir la referencia a la hoja de estilo del XML para Enveloping en modo Externally Detached, esta no se firmara: " + e //$NON-NLS-1$
					);
				}
			}
			// ******************************************************************
			// *** Fin hojas de estilo para enveloping en Externally Detached ***
			// ******************************************************************

		}

		// crea una referencia al documento mediante la URI hacia el
		// identificador del nodo CONTENT o el de datos si ya tenia Id
		else if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_DETACHED)) {
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
							tmpUri,
							digestMethod,
							transformList,
							XMLConstants.OBJURI, // Es un nodo de datos a firmar
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
							XMLConstants.OBJURI, // Es un nodo de datos a firmar
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
							Collections.singletonList(canonicalizationTransform),
							XMLConstants.OBJURI,
							referenceStyleId
						)
					);
				}
				catch (final Exception e) {
					XAdESSigner.LOGGER.severe(
						"No ha sido posible anadir la referencia a la hoja remota de estilo del XML para firma Detached, esta no se firmara: " + e //$NON-NLS-1$
					);
				}
			}
			// ******************************************************************
			// *********** Fin hojas de estilo remotas para Detached ************
			// ******************************************************************

		}

		// Crea una referencia al documento mediante la URI externa si la tenemos o usando un Message Digest
		// precalculado si no tenemos otro remedio
		else if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED)) {
			Reference ref = null;
			// No tenemos uri, suponemos que los datos son el message digest
			if (precalculatedHashAlgorithm != null &&
					(uri == null ||
			         uri.getScheme().equals("") || //$NON-NLS-1$
			         uri.getScheme().equals("file"))) { //$NON-NLS-1$

				final DigestMethod dm;
				try {
					dm = fac.newDigestMethod(
						XAdESUtil.getDigestMethodByCommonName(precalculatedHashAlgorithm),
						null
					);
				}
				catch (final Exception e) {
					throw new AOException(
						"No se ha podido crear el metodo de huella digital para la referencia Externally Detached: " + e, e //$NON-NLS-1$
					);
				}
				ref = fac.newReference(
					"",  //$NON-NLS-1$
					dm,
					null,
					XMLConstants.OBJURI,
					referenceId,
					data
				);
			}
			// Tenemos URI y no nos han establecido algoritmo de message digest,
			// por lo que es una referencia externa accesible
			else {
				// Si es una referencia de tipo file:// obtenemos el fichero y
				// creamos una referencia solo con el message digest
				if (uri != null && uri.getScheme().equals("file")) { //$NON-NLS-1$
					try {
						ref = fac.newReference(
							extraParams.getProperty(EXTRAPARAM_URI),
							digestMethod,
							null,
							XMLConstants.OBJURI,
							referenceId,
							MessageDigest.getInstance(
								AOSignConstants.getDigestAlgorithmName(digestMethodAlgorithm)
							).digest(
								data != null ?
									data :
										AOUtil.getDataFromInputStream(
											AOUtil.loadFile(uri)
										)
							)
						);
					}
					catch (final Exception e) {
						throw new AOException(
							"No se ha podido crear la referencia XML a partir de la URI local (" + uri.toASCIIString() + "): " + e, e //$NON-NLS-1$ //$NON-NLS-2$
						);
					}
				}
				// Si es una referencia distinta de file:// suponemos que es dereferenciable de forma universal
				// por lo que dejamos que Java lo haga todo
				else if (uri != null) {
					try {
						ref = fac.newReference(uri.toASCIIString(), digestMethod);
					}
					catch (final Exception e) {
						throw new AOException(
							"No se ha podido crear la referencia Externally Detached, probablemente por no obtenerse el metodo de digest: " + e, e //$NON-NLS-1$
						);
					}
				}
			}
			if (ref == null) {
				throw new AOException(
					"Error al generar la firma Externally Detached, no se ha podido crear la referencia externa" //$NON-NLS-1$
				);
			}
			referenceList.add(ref);

			// *******************************************************
			// **** Hojas de estilo remotas en Externally Detached ***
			// *******************************************************
			if (xmlStyle.getStyleHref() != null && xmlStyle.getStyleElement() == null) {
				// Comprobamos que la URL es valida
				if (xmlStyle.getStyleHref().startsWith(HTTP_PROTOCOL_PREFIX) ||
						xmlStyle.getStyleHref().startsWith(HTTPS_PROTOCOL_PREFIX)) {
					try {
						referenceList.add(
							fac.newReference(
								xmlStyle.getStyleHref(),
								digestMethod,
								Collections.singletonList(canonicalizationTransform),
								XMLConstants.OBJURI,
								referenceStyleId
							)
						);
					}
					catch (final Exception e) {
						XAdESSigner.LOGGER.severe(
							"No ha sido posible anadir la referencia a la hoja de estilo remota del XML para firma Externally Detached, esta no se firmara: " + e //$NON-NLS-1$
						);
					}
				}
				else {
					XAdESSigner.LOGGER.warning(
						"Se necesita una referencia externa HTTP o HTTPS a la hoja de estilo para referenciarla en firmas XML Externally Detached" //$NON-NLS-1$
					);
				}
			}
			// *******************************************************
			// ** Fin hojas de estilo remotas en Externally Detached *
			// *******************************************************

		}

		// Crea una referencia indicando que se trata de una firma enveloped
		else if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED)) {
			try {

				// Transformacion enveloped.
				// La enveloped siempre la primera, para que no se quede sin
				// nodos Signature por haber ejecutado antes otra transformacion
				transformList.add(
					fac.newTransform(
						Transform.ENVELOPED,
						(TransformParameterSpec) null
					)
				);

				// Establecemos que es lo que se firma
				// 1.- Si se especifico un nodo, se firma ese nodo
				// 2.- Si el raiz tiene Id, se firma ese Id
				// 3.- Se firma todo el XML con ""
				if (nodeToSign == null) {
					// Tiene la raiz un Id?
					final String ident = docSignature.getDocumentElement().getAttribute(ID_IDENTIFIER);
					if (ident != null && !"".equals(ident)) { //$NON-NLS-1$
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
								"not(ancestor-or-self::" + AOXAdESSigner.XML_SIGNATURE_PREFIX + ":Signature)", //$NON-NLS-1$ //$NON-NLS-2$
								Collections.singletonMap(
									AOXAdESSigner.XML_SIGNATURE_PREFIX,
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
						XMLConstants.OBJURI,
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
							Collections.singletonList(canonicalizationTransform),
							XMLConstants.OBJURI,
							referenceStyleId
						)
					);
				}
				catch (final Exception e) {
					XAdESSigner.LOGGER.severe(
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
				signatureInsertionNode = XAdESUtil.getFirstElmentFromXPath(envelopedNodeXPath, docSignature.getDocumentElement());
			}
			else if (nodeToSign != null) {
				signatureInsertionNode = CustomUriDereferencer.getElementById(docSignature, nodeToSign);
			}
		}

		// Instancia XADES_EPES
		final XAdES_EPES xades = (XAdES_EPES) XAdES.newInstance(
			XAdES.EPES,                           // XAdES
			xadesNamespace,                       // XAdES NameSpace
			AOXAdESSigner.XADES_SIGNATURE_PREFIX, // XAdES Prefix
			AOXAdESSigner.XML_SIGNATURE_PREFIX,   // XMLDSig Prefix
			digestMethodAlgorithm,                // DigestMethod
			docSignature,                         // Document
			signatureInsertionNode != null ?                     // Nodo donde se inserta la firma (como hijo), si no se indica se usa la raiz
				signatureInsertionNode:
					docSignature.getDocumentElement()
		);

		// SigningCertificate
		xades.setSigningCertificate((X509Certificate) certChain[0]);

		XAdESCommonMetadataUtil.addCommonMetadata(xades, extraParams);

		// DataObjectFormat
		String oid = extraParams.getProperty("contentTypeOid"); //$NON-NLS-1$
		if (oid == null && mimeType != null) {
			try {
				oid = MimeHelper.transformMimeTypeToOid(mimeType);
			}
        	catch (final Exception e) {
				LOGGER.warning("Error en la obtencion del OID del tipo de datos a partir del MimeType: " + e); //$NON-NLS-1$
			}
			// Si no se reconoce el MimeType se habra establecido el por defecto. Evitamos este comportamiento
			if (!MimeHelper.DEFAULT_MIMETYPE.equals(mimeType) && MimeHelper.DEFAULT_CONTENT_OID_DATA.equals(oid)) {
				oid = null;
			}
		}

		final ObjectIdentifierImpl objectIdentifier = oid != null ? new ObjectIdentifierImpl("OIDAsURN", (oid.startsWith("urn:oid:") ? //$NON-NLS-1$ //$NON-NLS-2$
			"" : //$NON-NLS-1$
				"urn:oid:") + oid, null, new ArrayList<String>(0)) : null; //$NON-NLS-1$

		final ArrayList<DataObjectFormat> objectFormats = new ArrayList<DataObjectFormat>();
		final DataObjectFormat objectFormat = new DataObjectFormatImpl(
			null,
			objectIdentifier,
			mimeType,
			encoding,
			"#" + referenceId //$NON-NLS-1$
		);
		objectFormats.add(objectFormat);
		xades.setDataObjectFormats(objectFormats);

		// CommitmentTypeIndications (http://www.w3.org/TR/XAdES/#Syntax_for_XAdES_The_CommitmentTypeIndication_element)
		final List<CommitmentTypeIndication> ctis = XAdESUtil.parseCommitmentTypeIndications(extraParams, referenceId);
		if (ctis != null && ctis.size() > 0) {
			xades.setCommitmentTypeIndications(ctis);
		}

		final AOXMLAdvancedSignature xmlSignature = XAdESUtil.getXmlAdvancedSignature(
			xades,
			signedPropertiesTypeUrl,
			digestMethodAlgorithm,
			canonicalizationAlgorithm
		);

		// en el caso de formato enveloping se inserta el elemento Object con el
		// documento a firmar
		if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)) {
			xmlSignature.addXMLObject(envelopingObject);
			if (envelopingStyleObject != null) {
				xmlSignature.addXMLObject(envelopingStyleObject);
			}
		}

		// *******************************************************
		// *********** Hojas de estilo en Enveloped **************
		// *******************************************************
		if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED) && xmlStyle.getStyleElement() != null) {

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
						Collections.singletonList(canonicalizationTransform),
						XMLConstants.OBJURI, // Es un nodo a firmar
						referenceStyleId
					)
				);
			}
			catch (final Exception e) {
				XAdESSigner.LOGGER.severe(
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

			// Creamos un nodo padre donde insertar el Manifest
			final List<XMLStructure> objectContent = new LinkedList<XMLStructure>();

			final String manifestId = "Manifest-" + UUID.randomUUID().toString(); //$NON-NLS-1$
			objectContent.add(fac.newManifest(new ArrayList<Reference>(referenceList), manifestId));

			final String manifestObjectId = "ManifestObject-" + UUID.randomUUID().toString(); //$NON-NLS-1$
			xmlSignature.addXMLObject(fac.newXMLObject(objectContent, manifestObjectId, null, null));

			// Si usamos un manifest las referencias no van en la firma, sino en el Manifest, y se
			// usa entonces en la firma una unica referencia a este Manifest
			referenceList.clear();
			referenceList.add(
				fac.newReference(
					"#" + manifestId, //$NON-NLS-1$
					digestMethod,
					Collections.singletonList(canonicalizationTransform),
					AOXAdESSigner.MANIFESTURI,
					referenceId
				)
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
				"Signature-" + UUID.randomUUID().toString(), //$NON-NLS-1$
				addKeyInfoKeyValue,
				addKeyInfoKeyName
			);

		}
		catch (final NoSuchAlgorithmException e) {
			throw new UnsupportedOperationException(
				"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "':" + e, e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		catch (final Exception e) {
			throw new AOException("Error al generar la firma XAdES: " + e, e); //$NON-NLS-1$
		}

		// Si se esta realizando una firma enveloping simple no tiene sentido el nodo raiz,
		// asi que sacamos el nodo de firma a un documento aparte
		if (format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)) {
			try {
				if (docSignature.getElementsByTagNameNS(XMLConstants.DSIGNNS,
						AOXAdESSigner.SIGNATURE_TAG).getLength() == 1) {
					final Document newdoc = dbf.newDocumentBuilder().newDocument();
					newdoc.appendChild(
						newdoc.adoptNode(
							docSignature.getElementsByTagNameNS(
								XMLConstants.DSIGNNS,
								AOXAdESSigner.SIGNATURE_TAG
							).item(0)
						)
					);
					docSignature = newdoc;
				}
			}
			catch (final Exception e) {
				XAdESSigner.LOGGER.info(
					"No se ha eliminado el nodo padre '<AFIRMA>': " + e //$NON-NLS-1$
				);
			}
		}

		// Si no es enveloped quito los valores del estilo para que no se inserte la
		// cabecera de hoja de estilo
		return Utils.writeXML(
			docSignature.getDocumentElement(),
			originalXMLProperties,
			format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED) ?
				xmlStyle.getStyleHref() :
					null,
			format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED) ?
				xmlStyle.getStyleType() :
					null
		);

	}

	private XAdESSigner() {
		// No permitimos la instanciacion
	}

}
