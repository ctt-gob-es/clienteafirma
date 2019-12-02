/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades;

import static es.gob.afirma.signers.xades.AOXAdESSigner.DIGEST_METHOD;
import static es.gob.afirma.signers.xades.AOXAdESSigner.SIGNATURE_NODE_NAME;
import static es.gob.afirma.signers.xades.AOXAdESSigner.SIGNATURE_TAG;
import static es.gob.afirma.signers.xades.AOXAdESSigner.STYLE_REFERENCE_PREFIX;
import static es.gob.afirma.signers.xades.AOXAdESSigner.XADESNS;
import static es.gob.afirma.signers.xades.AOXAdESSigner.XADES_SIGNATURE_PREFIX;
import static es.gob.afirma.signers.xades.AOXAdESSigner.XADES_SIGNED_PROPERTIES_TYPE;
import static es.gob.afirma.signers.xades.AOXAdESSigner.XML_SIGNATURE_PREFIX;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URI;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;
import es.uji.crypto.xades.jxades.security.xml.XAdES.DataObjectFormat;
import es.uji.crypto.xades.jxades.security.xml.XAdES.DataObjectFormatImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.ObjectIdentifierImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdES;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdES_EPES;

/** Co-firmador XAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class XAdESCoSigner {

    private static final String ID_IDENTIFIER = "Id"; //$NON-NLS-1$

	private static final Logger	LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private XAdESCoSigner() {
		// No permitimos la instanciacion
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
	 * @param pk Clave privada para la firma
	 * @param certChain Cadena de certificados del firmante
	 * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
	 * @return Cofirma en formato XAdES
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso */
	public static byte[] cosign(final byte[] sign,
			                    final String algorithm,
			                    final PrivateKey pk,
			                    final Certificate[] certChain,
			                    final Properties xParams) throws AOException {

		final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		final Properties extraParams = xParams != null ? xParams: new Properties();

		final String digestMethodAlgorithm = extraParams.getProperty(
		        XAdESExtraParams.REFERENCES_DIGEST_METHOD, DIGEST_METHOD);

		final String canonicalizationAlgorithm = extraParams.getProperty(
		        XAdESExtraParams.CANONICALIZATION_ALGORITHM, CanonicalizationMethod.INCLUSIVE);

		final String xadesNamespace = extraParams.getProperty(
		        XAdESExtraParams.XADES_NAMESPACE, XADESNS);

		final String signedPropertiesTypeUrl = extraParams.getProperty(
		        XAdESExtraParams.SIGNED_PROPERTIES_TYPE_URL, XADES_SIGNED_PROPERTIES_TYPE);

		final boolean addKeyInfoKeyValue = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.ADD_KEY_INFO_KEY_VALUE, Boolean.TRUE.toString()));

		final boolean addKeyInfoKeyName = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.ADD_KEY_INFO_KEY_NAME, Boolean.FALSE.toString()));

		final boolean addKeyInfoX509IssuerSerial = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.ADD_KEY_INFO_X509_ISSUER_SERIAL, Boolean.FALSE.toString()));

		final boolean keepKeyInfoUnsigned = Boolean.parseBoolean(extraParams.getProperty(
		        XAdESExtraParams.KEEP_KEYINFO_UNSIGNED, Boolean.FALSE.toString()));

		final String outputXmlEncoding = extraParams.getProperty(
		        XAdESExtraParams.OUTPUT_XML_ENCODING);

		String oid = extraParams.getProperty(XAdESExtraParams.CONTENT_TYPE_OID);

		String mimeType = extraParams.getProperty(XAdESExtraParams.CONTENT_MIME_TYPE);

		String encoding = extraParams.getProperty(XAdESExtraParams.CONTENT_ENCODING);

		// Dejamos que indiquen "base64" en vez de la URI, hacemos el cambio manualmente
		if ("base64".equalsIgnoreCase(encoding)) { //$NON-NLS-1$
			encoding = XMLConstants.BASE64_ENCODING;
		}

		// Comprobamos que sea una URI
		if (encoding != null && !encoding.isEmpty()) {
			try {
				// No se usa el objeto, solo se crea para ver si saltan excepciones
				new URI(encoding);
			}
			catch(final Exception e) {
				throw new AOException(
					"La codificacion indicada en 'encoding' debe ser una URI: " + e, e //$NON-NLS-1$
				);
			}
		}

		// nueva instancia de DocumentBuilderFactory que permita espacio de
		// nombres (necesario para XML)
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		// Carga el documento XML de firmas y su raiz
		Document docSig;
		Element rootSig;
		try {
			docSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));
			rootSig = docSig.getDocumentElement();

			// Si el documento contiene una firma simple se inserta como raiz el
			// nodo AFIRMA
			if (rootSig.getNodeName().equals(SIGNATURE_NODE_NAME)) {
				docSig = AOXAdESSigner.insertarNodoAfirma(docSig);
				rootSig = docSig.getDocumentElement();
			}
		}
		catch (final Exception e) {
			throw new AOException("No se ha podido leer el documento XML de firmas", e); //$NON-NLS-1$
		}

		// Propiedades del documento XML original
		final Map<String, String> originalXMLProperties = XAdESUtil.getOriginalXMLProperties(
			docSig,
			outputXmlEncoding
		);

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

		// Localizamos la primera firma (primer nodo "Signature") en profundidad en el arbol de firma.
		// Se considera que todos los objetos "Signature" del documento firman (referencian) los mismos
		// objetos, por lo que podemos extraerlos de cualquiera de ellas.
		// Buscamos dentro del SignedInfo de ese Signature todas las referencias que apunten a datos
		// para firmarlas.
		final Element signatureElement = (Element) docSig.
				getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_TAG).item(0);
		final Element signedInfo = (Element) signatureElement.
				getElementsByTagNameNS(XMLConstants.DSIGNNS, "SignedInfo").item(0); //$NON-NLS-1$
		final NodeList referencesNl = signedInfo.
				getElementsByTagNameNS(XMLConstants.DSIGNNS, "Reference"); //$NON-NLS-1$

		// Se deben firmar todas las referencias a datos de la firma que cofirmamos. Para ello
		// comprobaremos sus refencias y, segun sea, se firmara o no:
		// - Un elemento interno de tipo KeyInfo: No se firma
		// - Un elemento interno de tipo SignedProperties: No se firma.
		// - Cualquier otra cosa (URI vacia, manifest, hoja de estilo, elemento externo...): Se firma.
		final List<Node> objectReferencesList = new ArrayList<>();
		for (int i = 0; i < referencesNl.getLength(); i++) {
			final Node currentReference = referencesNl.item(i);

			final NamedNodeMap referenceAttributes = currentReference.getAttributes();

			// Si tiene declarado el tipo de la referencia es de SignedProperties, se ignora,
			// si tiene otro valor (objeto de datos, manifest u hoja de estilo), se agrega,
			// si no esta establecido, se comprueba
			final Node referenceType = referenceAttributes != null ?
					referenceAttributes.getNamedItem("Type") : null; //$NON-NLS-1$

			if (referenceType != null) {
				if (referenceType.getNodeValue() != null &&
						!referenceType.getNodeValue().endsWith("#SignedProperties")) { //$NON-NLS-1$
					objectReferencesList.add(currentReference);
				}
			}
			// Si no se establecio el tipo de referencia, lo comprobamos a partir de la URI
			else {
				final Node referenceUri = referenceAttributes != null ?
						referenceAttributes.getNamedItem("URI") : null; //$NON-NLS-1$

				// Omitimos las referencias cuya
				String uri;
				if (referenceUri == null || (uri = referenceUri.getNodeValue()) == null) {
					throw new AOException("Se ha encontrado una referencia sin URI"); //$NON-NLS-1$
				}

				// Si es una referencia interna, comprobamos que no sea el KeyInfo o el SignedProperties
				if (uri.startsWith("#")) { //$NON-NLS-1$
					final String elementId = uri.substring(1);
					final Node referencedNode = findNodeById(elementId, docSig);
					if (referencedNode == null) {
						throw new AOException("No se ha encontrado el nodo correspondiente a una referencia interna"); //$NON-NLS-1$
					}
					final String nodeName = referencedNode.getNodeName();
					if (!equalsNodeName(nodeName, "KeyInfo") && !equalsNodeName(nodeName, "SignedProperties")) { //$NON-NLS-1$ //$NON-NLS-2$
						objectReferencesList.add(currentReference);
					}
				}
				// Cualquier referencia no interna hay que firmarla
				else {
					objectReferencesList.add(currentReference);
				}
			}
		}

		// Creamos el listado de referencias que deberan aparecer en la firma
		final List<Reference> referenceList = new ArrayList<>();

		// Objeto en donde se almacenaran los datos firmados en caso de tratarse de
		// una firma enveloping
		ObjectIdentifierImpl objectIdentifier = null;
		XMLObject envelopingObject = null;
		boolean isEnveloping = false;
		String referenceId = null;
		for (final Node currentReference : objectReferencesList) {

			// Buscamos las transformaciones declaradas en la Referencia,
			// para anadirlas tambien en la nueva
			final List<Transform> currentTransformList;
			try {
				currentTransformList = Utils.getObjectReferenceTransforms(currentReference, XML_SIGNATURE_PREFIX);
			}
			catch (final NoSuchAlgorithmException e) {
				throw new AOException("Se ha declarado una transformacion personalizada de un tipo no soportado", e); //$NON-NLS-1$
			}
			catch (final InvalidAlgorithmParameterException e) {
				throw new AOException("Se han especificado parametros erroneos para una transformacion personalizada", e); //$NON-NLS-1$
			}

			// Creamos un identificador de referencia para el objeto a firmar y la almacenamos
			// para mantener un listado con todas. En el caso de las hojas de estilo lo creamos con un
			// identificador descriptivo
			final NamedNodeMap referenceAttributes = currentReference.getAttributes();
			if (referenceAttributes.getNamedItem(ID_IDENTIFIER) != null &&
					referenceAttributes.getNamedItem(ID_IDENTIFIER).getNodeValue().startsWith(STYLE_REFERENCE_PREFIX)) {
				referenceId = STYLE_REFERENCE_PREFIX + UUID.randomUUID().toString();
			}
			else {
				referenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$
			}


			// Buscamos y analizamos el nodo de datos para obtener su tipo
			final String referenceUri = ((Element) currentReference).getAttribute("URI"); //$NON-NLS-1$
			String referenceType = ((Element) currentReference).getAttribute("Type"); //$NON-NLS-1$
			if (referenceType != null && referenceType.isEmpty()) {
				referenceType = null;
			}

			// Firmas enveloped
			if ("".equals(referenceUri)) { //$NON-NLS-1$

				if (mimeType == null) {
					mimeType = "text/xml"; //$NON-NLS-1$
				}

				// Creamos la referencia a los datos con las transformaciones de la original
				referenceList.add(
						fac.newReference(
								referenceUri, // Aqui siempre vale ""
								digestMethod,
								currentTransformList,
								XMLConstants.OBJURI,
								referenceId
								)
						);

			}
			// Firmas enveloping y detached
			else {

				final String dataNodeId = referenceUri.substring(referenceUri.startsWith("#") ? 1 : 0); //$NON-NLS-1$
				Element dataObjectElement = null;
				final Element docElement = docSig.getDocumentElement();

				// Comprobamos si el nodo raiz o sus hijos inmediatos son el nodo de datos
				Node nodeAttributeId = docElement.getAttributes() != null ?
						docElement.getAttributes().getNamedItem(ID_IDENTIFIER) : null;
				if (nodeAttributeId != null && dataNodeId.equals(nodeAttributeId.getNodeValue())) {
					dataObjectElement = docElement;
				}
				else {
					// Recorremos los hijos al reves para acceder antes a los datos y las firmas
					final NodeList rootChildNodes = docElement.getChildNodes();
					for (int j = rootChildNodes.getLength() - 1; j >= 0; j--) {

						nodeAttributeId = rootChildNodes.item(j).getAttributes() != null ?
								rootChildNodes.item(j).getAttributes().getNamedItem(ID_IDENTIFIER) :
									null;
								if (nodeAttributeId != null && dataNodeId.equals(nodeAttributeId.getNodeValue())) {
									dataObjectElement = (Element) rootChildNodes.item(j);
									break;
								}

								// Si es un nodo de firma tambien miramos en sus nodos hijos
								if (SIGNATURE_TAG.equals(rootChildNodes.item(j).getLocalName())) {
									final NodeList subChildsNodes = rootChildNodes.item(j).getChildNodes();
									for (int k = subChildsNodes.getLength() - 1; k >= 0; k--) {
										nodeAttributeId = subChildsNodes.item(k).getAttributes() != null ?
												subChildsNodes.item(k).getAttributes().getNamedItem(ID_IDENTIFIER) :
													null;
												if (nodeAttributeId != null && dataNodeId.equals(nodeAttributeId.getNodeValue())) {
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
				if (dataObjectElement != null) {
					if (mimeType == null) {
						mimeType = dataObjectElement.getAttribute("MimeType"); //$NON-NLS-1$
					}
					if (encoding == null) {
						encoding = dataObjectElement.getAttribute("Encoding"); //$NON-NLS-1$
					}
				}

				final NodeList signatureChildNodes = docSig.getElementsByTagNameNS(
						XMLConstants.DSIGNNS, SIGNATURE_TAG
						).item(0).getChildNodes();
				for (int j = 0; j < signatureChildNodes.getLength(); j++) {
					final Node subNode = signatureChildNodes.item(j);
					final NamedNodeMap nnm = subNode.getAttributes();
					if (nnm != null) {
						final Node idAttrNode = nnm.getNamedItem(ID_IDENTIFIER);
						if (idAttrNode != null && dataNodeId.equals(idAttrNode.getNodeValue())) {
							isEnveloping = true;
						}
					}
				}

				if (isEnveloping && dataObjectElement != null) {
					// crea el nuevo elemento Object que con el documento afirmar
					final List<XMLStructure> structures = new ArrayList<>(1);
					structures.add(new DOMStructure(dataObjectElement.getFirstChild().cloneNode(true)));

					final String objectId = "Object-" + UUID.randomUUID().toString(); //$NON-NLS-1$
					envelopingObject = fac.newXMLObject(
							structures,
							objectId,
							mimeType,
							encoding
							);

					// Agregamos la referencia al nuevo objeto de datos
					referenceList.add(
							fac.newReference(
									"#" + objectId, //$NON-NLS-1$
									digestMethod,
									currentTransformList,
									referenceType != null ? referenceType : XMLConstants.OBJURI,
									referenceId
									)
							);

				}
				// Firma detached
				else {
					// Agregamos la referencia a los datos ya existentes
					referenceList.add(
							fac.newReference(
									referenceUri,
									digestMethod,
									currentTransformList,
									referenceType != null ? referenceType : XMLConstants.OBJURI,
									referenceId
									)
							);

				}

			}
			if (oid == null && mimeType != null) {
				try {
					oid = MimeHelper.transformMimeTypeToOid(mimeType);
				} catch (final IOException e) {
					LOGGER.warning("Error en la obtencion del OID del tipo de datos a partir del MimeType: " + e); //$NON-NLS-1$
				}
			}
			if (oid != null) {
				objectIdentifier = new ObjectIdentifierImpl(
						"OIDAsURN", (oid.startsWith("urn:oid:") ? "" : "urn:oid:") + oid, null, new ArrayList<String>(0)); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			}
		}

		final XAdES_EPES xades = (XAdES_EPES) XAdES.newInstance(
				XAdES.EPES,
				xadesNamespace,
				XADES_SIGNATURE_PREFIX,
				XML_SIGNATURE_PREFIX,
				digestMethodAlgorithm,
				rootSig.getOwnerDocument(),
				rootSig
				);

		// establece el certificado
		final X509Certificate cert = (X509Certificate) certChain[0];
		xades.setSigningCertificate(cert);

		XAdESCommonMetadataUtil.addCommonMetadata(xades, extraParams);

		// DataObjectFormat
		if (objectIdentifier != null || mimeType != null || encoding != null) {
			final ArrayList<DataObjectFormat> objectFormats = new ArrayList<>();
			final DataObjectFormat objectFormat = new DataObjectFormatImpl(
				null,
				objectIdentifier,
				mimeType,
				encoding,
				"#" + referenceId //$NON-NLS-1$
			);
			objectFormats.add(objectFormat);
			xades.setDataObjectFormats(objectFormats);
		}

		// crea la firma
		final AOXMLAdvancedSignature xmlSignature = XAdESUtil.getXmlAdvancedSignature(
			xades,
			signedPropertiesTypeUrl,
			digestMethodAlgorithm,
			canonicalizationAlgorithm
		);

		// en el caso de formato enveloping se inserta el elemento Object con el
		// documento a firmar
		if (isEnveloping) {
			xmlSignature.addXMLObject(envelopingObject);
		}

		try {
			final boolean onlySignningCert = Boolean.parseBoolean(
				extraParams.getProperty(
					XAdESExtraParams.INCLUDE_ONLY_SIGNNING_CERTIFICATE,
					Boolean.FALSE.toString()
				)
			);
			if (onlySignningCert) {
				xmlSignature.sign(
						(X509Certificate) certChain[0],
						pk,
						algoUri,
						referenceList,
						"Signature-" + UUID.randomUUID().toString() //$NON-NLS-1$
						);
			}
			else {
				xmlSignature.sign(
					Arrays.asList(certChain),
					pk,
					algoUri,
					referenceList,
					"Signature-" + UUID.randomUUID().toString(), //$NON-NLS-1$
					addKeyInfoKeyValue,
					addKeyInfoKeyName,
					addKeyInfoX509IssuerSerial,
					keepKeyInfoUnsigned
				);
			}
		}
		catch (final NoSuchAlgorithmException e) {
			throw new UnsupportedOperationException(
				"No se soporta el algoritmo de firma '" + algorithm + "': " + e, e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		catch (final Exception e) {
			throw new AOException("Error al generar la cofirma", e); //$NON-NLS-1$
		}

		return Utils.writeXML(
			rootSig,
			originalXMLProperties,
			null,
			null
		);
	}

	/**
	 * Busca un nodo con el identificador especificado.
	 * @param nodeId Identificador del nodo que queremos encontrar.
	 * @param parentElement Nodo padre en el que buscar.
	 * @return Nodo con el identificador indicado o {@code null} si no
	 * se encuentra el nodo o si lo que se encuentra no es un &uacute;nico nodo.
	 */
	private static Node findNodeById(final String nodeId, final Node parentElement) {
		final XPath xpath = XPathFactory.newInstance().newXPath();

		Node node;
		try {
			node = (Node) xpath.evaluate("//*[@Id='" + nodeId + "']", parentElement, XPathConstants.NODE); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Lo encontrado con el Id " + nodeId + " no es un nodo", e); //$NON-NLS-1$ //$NON-NLS-2$
			node = null;
		}
		return node;
	}

	/**
	 * Compara que el nombre de un nodo se corresponda con el que se desea, teniendo en cuenta
	 * que el nombre del nodo puede contener el espacio de nombres XML.
	 * @param nodeName Nombre del nodo que se quiere comprobar.
	 * @param name Nombre que comprobamos.
	 * @return {@code true} si el nodo tiene ese nombre sin contar el espacio de nombres,
	 * {@code false} en caso contrario.
	 */
	private static boolean equalsNodeName(final String nodeName, final String name) {
		return nodeName.equals(name) || nodeName.endsWith(":" + name);
	}
}
