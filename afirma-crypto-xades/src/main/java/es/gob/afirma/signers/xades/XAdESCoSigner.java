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

import static es.gob.afirma.signers.xades.AOXAdESSigner.DIGEST_METHOD;
import static es.gob.afirma.signers.xades.AOXAdESSigner.LOGGER;
import static es.gob.afirma.signers.xades.AOXAdESSigner.SIGNATURE_NODE_NAME;
import static es.gob.afirma.signers.xades.AOXAdESSigner.SIGNATURE_TAG;
import static es.gob.afirma.signers.xades.AOXAdESSigner.STYLE_REFERENCE_PREFIX;
import static es.gob.afirma.signers.xades.AOXAdESSigner.XADESNS;
import static es.gob.afirma.signers.xades.AOXAdESSigner.XADES_SIGNATURE_PREFIX;
import static es.gob.afirma.signers.xades.AOXAdESSigner.XADES_SIGNED_PROPERTIES_TYPE;
import static es.gob.afirma.signers.xades.AOXAdESSigner.XML_SIGNATURE_PREFIX;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;

import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.parsers.DocumentBuilderFactory;

import net.java.xades.security.xml.XAdES.DataObjectFormat;
import net.java.xades.security.xml.XAdES.DataObjectFormatImpl;
import net.java.xades.security.xml.XAdES.ObjectIdentifierImpl;
import net.java.xades.security.xml.XAdES.XAdES;
import net.java.xades.security.xml.XAdES.XAdES_EPES;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;

/** Co-firmador XAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class XAdESCoSigner {

    private static final String ID_IDENTIFIER = "Id"; //$NON-NLS-1$

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
				"referencesDigestMethod", DIGEST_METHOD); //$NON-NLS-1$
		final String canonicalizationAlgorithm = extraParams.getProperty(
				"canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE); //$NON-NLS-1$
		final String xadesNamespace = extraParams.getProperty(
				"xadesNamespace", XADESNS); //$NON-NLS-1$
		final String signedPropertiesTypeUrl = extraParams.getProperty(
				"signedPropertiesTypeUrl", XADES_SIGNED_PROPERTIES_TYPE); //$NON-NLS-1$
		final boolean addKeyInfoKeyValue = Boolean.parseBoolean(extraParams.getProperty(
				"addKeyInfoKeyValue", Boolean.TRUE.toString())); //$NON-NLS-1$
		final boolean addKeyInfoKeyName = Boolean.parseBoolean(extraParams.getProperty(
				"addKeyInfoKeyName", Boolean.FALSE.toString())); //$NON-NLS-1$

		String mimeType = extraParams.getProperty("mimeType"); //$NON-NLS-1$
		String encoding = extraParams.getProperty("encoding"); //$NON-NLS-1$
		if ("base64".equalsIgnoreCase(encoding)) { //$NON-NLS-1$
			encoding = XMLConstants.BASE64_ENCODING;
		}
		String oid = extraParams.getProperty("contentTypeOid"); //$NON-NLS-1$

		ObjectIdentifierImpl objectIdentifier = null;

		// nueva instancia de DocumentBuilderFactory que permita espacio de
		// nombres (necesario para XML)
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		// Propiedades del documento XML original
		final Map<String, String> originalXMLProperties = new Hashtable<String, String>();

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

		final XMLSignatureFactory fac = Utils.getDOMFactory();

		final DigestMethod digestMethod;
		try {
			digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
		}
		catch (final Exception e) {
			throw new AOException("No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// Objeto en donde se almacenaran los datos firmados en caso de tratarse de
		// una firma enveloping
		XMLObject envelopingObject = null;
		boolean isEnveloping = false;

		// Localizamos la primera firma (primer nodo "Signature") en profundidad en el arbol de firma.
		// Se considera que todos los objetos "Signature" del documento firman (referencian) los mismos
		// objetos, por lo que podemos extraerlos de cualquiera de las firmas actuales.
		// Buscamos dentro de ese Signature todas las referencias que apunten a datos para firmarlas
		final List<Reference> referenceList = new ArrayList<Reference>();
		Node currentElement;
		final NodeList nl = ((Element) docSig.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_TAG).item(0)).getElementsByTagNameNS(XMLConstants.DSIGNNS, "Reference"); //$NON-NLS-1$

		// Se considera que la primera referencia de la firma son los datos que debemos firmar, ademas
		// de varias referencias especiales
		String referenceId = null;
		for (int i = 0; i < nl.getLength(); i++) {
			currentElement = nl.item(i);

			// Firmamos la primera referencia (que seran los datos firmados) y las hojas de estilo que
			// tenga asignadas. Las hojas de estilo tendran un identificador que comience por STYLE_REFERENCE_PREFIX.
			// TODO: Identificar las hojas de estilo de un modo generico.
			final NamedNodeMap currentNodeAttributes = currentElement.getAttributes();
			if (i == 0 || currentNodeAttributes.getNamedItem(ID_IDENTIFIER) != null && currentNodeAttributes.getNamedItem(ID_IDENTIFIER)
					.getNodeValue()
					.startsWith(STYLE_REFERENCE_PREFIX)) {

				// Buscamos las transformaciones declaradas en la Referencia,
				// para anadirlas tambien en la nueva
				final List<Transform> currentTransformList;
				try {
					currentTransformList = Utils.getObjectReferenceTransforms(currentElement, XML_SIGNATURE_PREFIX);
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
				if (currentNodeAttributes.getNamedItem(ID_IDENTIFIER) != null && currentNodeAttributes.getNamedItem(ID_IDENTIFIER)
						.getNodeValue()
						.startsWith(STYLE_REFERENCE_PREFIX)) {
					referenceId = STYLE_REFERENCE_PREFIX + UUID.randomUUID().toString();
				}
				else {
					referenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$
				}


				// Buscamos y analizamos el nodo de datos para obtener su tipo
				final String dataXmlUri = ((Element) currentElement).getAttribute("URI"); //$NON-NLS-1$
				// Firmas enveloped
				if ("".equals(dataXmlUri)) { //$NON-NLS-1$
					if (mimeType == null) {
						mimeType = "text/xml"; //$NON-NLS-1$
					}
					if (encoding == null) {
						encoding = docSig.getInputEncoding();
					}

					// Creamos la referencia a los datos con las transformaciones de la original
					referenceList.add(
						fac.newReference(
							((Element) currentElement).getAttribute("URI"), //$NON-NLS-1$
							digestMethod,
							currentTransformList,
							XMLConstants.OBJURI,
							referenceId
						)
					);

				}
				// Firmas enveloping y detached
				else {

					final String dataNodeId = dataXmlUri.substring(dataXmlUri.startsWith("#") ? 1 : 0); //$NON-NLS-1$
					Element dataObjectElement = null;
					final Element docElement = docSig.getDocumentElement();

					// Comprobamos si el nodo raiz o sus hijos inmediatos son el nodo de datos
					Node nodeAttributeId = docElement.getAttributes() != null ? docElement.getAttributes().getNamedItem(ID_IDENTIFIER) : null;
					if (nodeAttributeId != null && dataNodeId.equals(nodeAttributeId.getNodeValue())) {
						dataObjectElement = docElement;
					}
					else {
						// Recorremos los hijos al reves para acceder antes a los datos y las firmas
						final NodeList rootChildNodes = docElement.getChildNodes();
						for (int j = rootChildNodes.getLength() - 1; j >= 0; j--) {

							nodeAttributeId = rootChildNodes.item(j).getAttributes() != null ? rootChildNodes.item(j).getAttributes().getNamedItem(ID_IDENTIFIER) : null;
							if (nodeAttributeId != null && dataNodeId.equals(nodeAttributeId.getNodeValue())) {
								dataObjectElement = (Element) rootChildNodes.item(j);
								break;
							}

							// Si es un nodo de firma tambien miramos en sus nodos hijos
							if (SIGNATURE_TAG.equals(rootChildNodes.item(j).getLocalName())) {
								final NodeList subChildsNodes = rootChildNodes.item(j).getChildNodes();
								for (int k = subChildsNodes.getLength() - 1; k >= 0; k--) {
									nodeAttributeId = subChildsNodes.item(k).getAttributes() != null ? subChildsNodes.item(k).getAttributes().getNamedItem(ID_IDENTIFIER) : null;
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

					final NodeList signatureChildNodes = docSig.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_TAG).item(0).getChildNodes();
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
						final List<XMLStructure> structures = new ArrayList<XMLStructure>(1);
						structures.add(new DOMStructure(dataObjectElement.getFirstChild().cloneNode(true)));

						final String objectId = "Object-" + UUID.randomUUID().toString(); //$NON-NLS-1$
						envelopingObject = fac.newXMLObject(structures, objectId, mimeType, encoding);

						// Agregamos la referencia al nuevo objeto de datos
						referenceList.add(
							fac.newReference(
								"#" + objectId, //$NON-NLS-1$
								digestMethod,
								currentTransformList,
								XMLConstants.OBJURI,
								referenceId
							)
						);

					}
					// Firma detached
					else {
						// Agregamos la referencia a los datos ya existentes
						referenceList.add(
							fac.newReference(
								((Element) currentElement).getAttribute("URI"), //$NON-NLS-1$
								digestMethod,
								currentTransformList,
								XMLConstants.OBJURI,
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
			final ArrayList<DataObjectFormat> objectFormats = new ArrayList<DataObjectFormat>();
			final DataObjectFormat objectFormat = new DataObjectFormatImpl(null,
				objectIdentifier, mimeType, encoding, "#" + referenceId //$NON-NLS-1$
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
					extraParams.getProperty("includeOnlySignningCertificate", Boolean.FALSE.toString()) //$NON-NLS-1$
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
					addKeyInfoKeyName
				);
			}
		}
		catch (final NoSuchAlgorithmException e) {
			throw new UnsupportedOperationException("No se soporta el algoritmo de firma '" + algorithm + "': " + e, e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final Exception e) {
			throw new AOException("Error al generar la cofirma", e); //$NON-NLS-1$
		}

		return Utils.writeXML(rootSig, originalXMLProperties, null, null);
	}
}
