/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades;

import java.net.MalformedURLException;
import java.net.URL;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.uji.crypto.xades.jxades.security.xml.XAdES.CommitmentTypeIdImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.CommitmentTypeIndication;
import es.uji.crypto.xades.jxades.security.xml.XAdES.CommitmentTypeIndicationImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdES_EPES;

/** Utilidades varias para firmas XAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class XAdESUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

	private static final String[] SUPPORTED_XADES_NAMESPACE_URIS = new String[] {
		"http://uri.etsi.org/01903#", //$NON-NLS-1$
	    "http://uri.etsi.org/01903/v1.2.2#", //$NON-NLS-1$
	    "http://uri.etsi.org/01903/v1.3.2#", //$NON-NLS-1$
	    "http://uri.etsi.org/01903/v1.4.1#" //$NON-NLS-1$
	};

	private XAdESUtil() {
		// No permitimos la instanciacion
	}

    /** Comprueba que los nodos de firma proporcionados sean firmas en formato XAdES.
     * @param signNodes Listado de nodos de firma.
     * @return {@code true} cuando todos los nodos sean firmas en este formato. */
    static boolean checkSignNodes(final List<Node> signNodes) {
        for (final Node signNode : signNodes) {
        	int lenCount = 0;
        	for (final String xadesNamespace : SUPPORTED_XADES_NAMESPACE_URIS) {
        		lenCount = lenCount + ((Element) signNode).getElementsByTagNameNS(xadesNamespace, "QualifyingProperties").getLength(); //$NON-NLS-1$
        	}
            if (lenCount == 0) {
                return false;
            }
        }
        return true;
    }

	static AOXMLAdvancedSignature getXmlAdvancedSignature(final XAdES_EPES xades,
			                                              final String signedPropertiesTypeUrl,
			                                              final String digestMethodAlgorithm,
			                                              final String canonicalizationAlgorithm) throws AOException {
		final AOXMLAdvancedSignature xmlSignature;
		try {
			xmlSignature = AOXMLAdvancedSignature.newInstance(xades);
		}
		catch (final Exception e) {
			throw new AOException(
				"No se ha podido instanciar la firma XML Avanzada de JXAdES: " + e, e //$NON-NLS-1$
			);
		}

		// Establecemos el tipo de propiedades firmadas
		xmlSignature.setSignedPropertiesTypeUrl(signedPropertiesTypeUrl);

		try {
			xmlSignature.setDigestMethod(digestMethodAlgorithm);
		}
		catch (final Exception e) {
			throw new AOException(
				"No se ha podido establecer el algoritmo de huella digital: " + e, e //$NON-NLS-1$
			);
		}

		xmlSignature.setCanonicalizationMethod(canonicalizationAlgorithm);

		return xmlSignature;
	}

	static Element getFirstElmentFromXPath(final String xpathExpression, final Element sourceElement) throws AOException {
		final NodeList nodeList;
		try {
			 nodeList = (NodeList)XPathFactory.newInstance().newXPath().evaluate(xpathExpression, sourceElement, XPathConstants.NODESET);
		}
		catch (final XPathExpressionException e1) {
			throw new AOException(
				"No se ha podido evaluar la expresion indicada para la insercion de la firma Enveloped ('" + xpathExpression + "'): " + e1, //$NON-NLS-1$ //$NON-NLS-2$
				e1
			);
		}
		if (nodeList.getLength() < 1) {
			throw new AOException(
				"La expresion indicada para la insercion de la firma Enveloped ('" + xpathExpression + "') no ha devuelto ningun nodo" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		if (nodeList.getLength() > 1) {
			LOGGER.warning(
				"La expresion indicada para la insercion de la firma Enveloped ('" + xpathExpression + "') ha devuelto varios nodos, se usara el primero" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		return (Element) nodeList.item(0);
	}

	/** Obtiene la lista de <i>CommitmentTypeIndication</i> declarados en el fichero de
	 * propiedades de par&aacute;metros adicionales.
	 * @param xParams Par&aacute;metros adicionales para la firma.
	 * @param signedDataId Identificador del nodo a firmar (<i>Data Object</i>).
	 * @return Lista de <i>CommitmentTypeIndication</i> a incluir en la firma XAdES. */
	public static List<CommitmentTypeIndication> parseCommitmentTypeIndications(final Properties xParams,
			                                                                    final String signedDataId) {

		final List<CommitmentTypeIndication> ret = new ArrayList<>();

		if (xParams == null) {
			return ret;
		}
		String tmpStr = xParams.getProperty(XAdESExtraParams.COMMITMENT_TYPE_INDICATIONS);
		if (tmpStr == null) {
			return ret;
		}
		final int nCtis;
		try {
			nCtis = Integer.parseInt(tmpStr);
			if (nCtis < 1) {
				throw new NumberFormatException();
			}
		}
		catch(final Exception e) {
			LOGGER.severe(
				"El parametro adicional 'CommitmentTypeIndications' debe contener un valor numerico entero (el valor actual es " + tmpStr + "), no se anadira el CommitmentTypeIndication: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			return ret;
		}

		String identifier;
		String description;
		ArrayList<String> documentationReferences;
		ArrayList<String> commitmentTypeQualifiers;

		for(int i=0;i<=nCtis;i++) {

			// Identifier
			tmpStr = xParams.getProperty(XAdESExtraParams.COMMITMENT_TYPE_INDICATION_PREFIX + Integer.toString(i) + XAdESExtraParams.COMMITMENT_TYPE_INDICATION_IDENTIFIER);
			if (tmpStr == null) {
				continue;
			}
			identifier = XAdESExtraParams.COMMITMENT_TYPE_IDENTIFIERS.get(tmpStr);
			if (identifier == null)  {
				LOGGER.severe(
					"El identificador del CommitmentTypeIndication " + i + " no es un tipo soportado (" + tmpStr + "), se omitira y se continuara con el siguiente" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				);
				continue;
			}

			// Description
			description = xParams.getProperty(XAdESExtraParams.COMMITMENT_TYPE_INDICATION_PREFIX + Integer.toString(i) + XAdESExtraParams.COMMITMENT_TYPE_INDICATION_DESCRIPTION);

			// DocumentationReferences
			tmpStr = xParams.getProperty(XAdESExtraParams.COMMITMENT_TYPE_INDICATION_PREFIX + Integer.toString(i) + XAdESExtraParams.COMMITMENT_TYPE_INDICATION_DOCUMENTATION_REFERENCE);
			if (tmpStr == null) {
				documentationReferences = new ArrayList<>(0);
			}
			else {
				documentationReferences = new ArrayList<>();
				final String[] docRefs = tmpStr.split(Pattern.quote("|")); //$NON-NLS-1$
				for (final String docRef : docRefs) {
					try {
						documentationReferences.add(new URL(docRef).toString());
					}
					catch (final MalformedURLException e) {
						LOGGER.severe(
							"La referencia documental '" + docRef + "' del CommitmentTypeIndication " + i + " no es una URL, se omitira y se continuara con la siguiente referencia documental: " + e //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
						);
						continue;
					}
				}
			}

			// CommitmentTypeQualifiers
			tmpStr = xParams.getProperty(XAdESExtraParams.COMMITMENT_TYPE_INDICATION_PREFIX + Integer.toString(i) + XAdESExtraParams.COMMITMENT_TYPE_INDICATION_QUALIFIERS);
			if (tmpStr == null) {
				commitmentTypeQualifiers = new ArrayList<>(0);
			}
			else {
				commitmentTypeQualifiers = new ArrayList<>();
				final String[] ctqs = tmpStr.split(Pattern.quote("|")); //$NON-NLS-1$
				for (final String ctq : ctqs) {
					commitmentTypeQualifiers.add(ctq);
				}
			}

			ret.add(
				new CommitmentTypeIndicationImpl(
					new CommitmentTypeIdImpl(
						identifier.startsWith("urn:oid:") ? // OID como URN si el Id es OID, null en otro caso //$NON-NLS-1$
							"OIDAsURN" : //$NON-NLS-1$
								null,
						identifier, // Un OID o una URL
						description, // Descripcion textual (opcional)
						documentationReferences	// Lista de URL (opcional)
					),
					signedDataId != null ?      // Una URI, pero se acepta null
						"#" + signedDataId : //$NON-NLS-1$
							null,
					commitmentTypeQualifiers	// Lista de elementos textuales (opcional)
				)
			);
		}
		return ret;
	}

	static String getDigestMethodByCommonName(final String identifierHashAlgorithm) throws NoSuchAlgorithmException {
		final String normalDigAlgo = AOSignConstants.getDigestAlgorithmName(identifierHashAlgorithm);
		if ("SHA1".equalsIgnoreCase(normalDigAlgo)) { //$NON-NLS-1$
			return DigestMethod.SHA1;
		}
		if ("SHA-256".equalsIgnoreCase(normalDigAlgo)) { //$NON-NLS-1$
			return DigestMethod.SHA256;
		}
		if ("SHA-512".equalsIgnoreCase(normalDigAlgo)) { //$NON-NLS-1$
			return DigestMethod.SHA512;
		}
		throw new NoSuchAlgorithmException("No se soporta el algoritmo: " + normalDigAlgo); //$NON-NLS-1$
	}

	static Element getRootElement(final Document docSignature, final Properties extraParams) {

		final Properties xParams = extraParams != null ? extraParams : new Properties();
		final String nodeName            = xParams.getProperty(XAdESExtraParams.ROOT_XML_NODE_NAME , AOXAdESSigner.AFIRMA);
		final String nodeNamespace       = xParams.getProperty(XAdESExtraParams.ROOT_XML_NODE_NAMESPACE);
		final String nodeNamespacePrefix = xParams.getProperty(XAdESExtraParams.ROOT_XML_NODE_NAMESPACE_PREFIX);

		final Element afirmaRoot;
		if (nodeNamespace == null) {
			afirmaRoot = docSignature.createElement(nodeName);
		}
		else {
			afirmaRoot = docSignature.createElementNS(nodeNamespace, nodeName);
			if (nodeNamespacePrefix != null) {
				afirmaRoot.setAttribute(
					nodeNamespacePrefix.startsWith("xmlns:") ?  nodeNamespacePrefix : "xmlns:" + nodeNamespacePrefix, //$NON-NLS-1$ //$NON-NLS-2$
					nodeNamespace
				);
			}
		}
		afirmaRoot.setAttributeNS(null, XAdESSigner.ID_IDENTIFIER, nodeName + "-Root-" + UUID.randomUUID().toString());  //$NON-NLS-1$

		return afirmaRoot;
	}

	static List<Reference> createManifest(final List<Reference> referenceList,
			                              final XMLSignatureFactory fac,
			                              final AOXMLAdvancedSignature xmlSignature,
			                              final DigestMethod digestMethod,
			                              final Transform canonicalizationTransform,
			                              final String referenceId) {

		// Con Manifest vamos a incluir las referencias de "referencesList" en el Manifest y luego
		// limpiar este mismo "referencesList" incluyendo posteriormente unica referencia al propio
		// Manifest. Como es este "referencesList" lo que se firma, queda ya listo con el Manifest
		// que contiene las referencias que de no usar Manifest estarian en "referencesList".

		// Creamos un nodo padre donde insertar el Manifest
		final List<XMLStructure> objectContent = new LinkedList<>();

		final String manifestId = "Manifest-" + UUID.randomUUID().toString(); //$NON-NLS-1$
		objectContent.add(
			fac.newManifest(
				new ArrayList<>(referenceList),
				manifestId
			)
		);

		final String manifestObjectId = "ManifestObject-" + UUID.nameUUIDFromBytes(referenceId.getBytes()).toString(); //$NON-NLS-1$
		xmlSignature.addXMLObject(
			fac.newXMLObject(
				objectContent, manifestObjectId, null, null
			)
		);

		// Si usamos un manifest las referencias no van en la firma, sino en el Manifest, y se
		// usa entonces en la firma una unica referencia a este Manifest
		referenceList.clear();
		referenceList.add(
			fac.newReference(
				"#" + manifestId, //$NON-NLS-1$
				digestMethod,
				canonicalizationTransform != null ?
					Collections.singletonList(canonicalizationTransform) :
						new ArrayList<Transform>(0),
				AOXAdESSigner.MANIFESTURI,
				"Manifest" + referenceId //$NON-NLS-1$
			)
		);

		return referenceList;
	}

	static Map<String, String> getOriginalXMLProperties(final Document docum,
			                                            final String outputXmlEncoding) {

		final Map<String, String> originalXMLProperties = new Hashtable<>();
		if (docum != null) {

			if (outputXmlEncoding != null) {
				originalXMLProperties.put(
					OutputKeys.ENCODING,
					outputXmlEncoding
				);
			}
			else if (docum.getXmlEncoding() != null) {
				originalXMLProperties.put(
					OutputKeys.ENCODING,
					docum.getXmlEncoding()
				);
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

		}
		return originalXMLProperties;
	}

}
