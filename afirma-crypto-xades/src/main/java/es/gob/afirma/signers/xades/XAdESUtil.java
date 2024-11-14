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
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.UUID;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import javax.xml.crypto.URIDereferencer;
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
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.SigningLTSException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.signers.xml.XMLConstants;
import es.uji.crypto.xades.jxades.security.xml.XAdES.CommitmentTypeIdImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.CommitmentTypeIndication;
import es.uji.crypto.xades.jxades.security.xml.XAdES.CommitmentTypeIndicationImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SigningCertificateV2Info;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdES;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdESBase;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XadesWithBaselineAttributes;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XadesWithBasicAttributes;
import es.uji.crypto.xades.jxades.util.XMLUtils;

/**
 * Utilidades varias para firmas XAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s.
 */
public final class XAdESUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

	private static final String[] SUPPORTED_XADES_NAMESPACE_URIS = new String[] {
		XAdESConstants.NAMESPACE_XADES_NO_VERSION,
	    XAdESConstants.NAMESPACE_XADES_1_2_2,
	    XAdESConstants.NAMESPACE_XADES_1_3_2,
	    XAdESConstants.NAMESPACE_XADES_1_4_1
	};

	private static final String[] SIGNED_PROPERTIES_TYPES = new String[] {
		XAdESConstants.NAMESPACE_XADES_NO_VERSION_SIGNED_PROPERTIES,
		XAdESConstants.NAMESPACE_XADES_1_2_2_SIGNED_PROPERTIES,
		XAdESConstants.NAMESPACE_XADES_1_3_2_SIGNED_PROPERTIES,
		XAdESConstants.NAMESPACE_XADES_1_4_1_SIGNED_PROPERTIES
	};

    private static final String CRYPTO_OPERATION_SIGN = "SIGN"; //$NON-NLS-1$

    /** Transformaci&oacute;n XPATH para eliminar todas las firmas de un XML. */
    private static final String XPATH_ENVELOPED_EQ = "not(ancestor-or-self::%1$s:Signature)"; //$NON-NLS-1$

    /** Transformaci&oacute;n XPATH equivalente a la transformaci&oacute;n enveloped. Elimina la firma actual. */
    private static final String XPATH_ENVELOPED_EQ2 = "count(ancestor-or-self::%1$s:Signature|here()/ancestor::%1$s:Signature[1])>count(ancestor-or-self::%1$s:Signature)"; //$NON-NLS-1$


	private XAdESUtil() {
		// No permitimos la instanciacion
	}

    /**
     * Comprueba que los nodos de firma proporcionados sean firmas en formato XAdES.
     * @param signNodes Listado de nodos de firma.
     * @return {@code true} cuando todos los nodos sean firmas en este formato.
     */
    static boolean checkSignNodes(final List<Node> signNodes) {
        for (final Node signNode : signNodes) {
        	int lenCount = 0;
        	for (final String xadesNamespace : SUPPORTED_XADES_NAMESPACE_URIS) {
        		lenCount = lenCount + ((Element) signNode).getElementsByTagNameNS(xadesNamespace, XAdESConstants.TAG_QUALIFYING_PROPERTIES).getLength();
        	}
            if (lenCount == 0) {
                return false;
            }
        }
        return true;
    }

    /**
     * Comprueba que los nodos de firma proporcionados sean firmas en formato XAdES soportadas. Se
     * considerar&aacute; que est&aacute;n soportadas si no hay mezcla de versiones de firmas
     * XAdES y si la versi&oacute;n declarada est&aacute; soportada. Tambi&eacute;n se comprobara
     * si es una firma de tipo Baseline EN para indicarlo mediante el valor de retorno.
     * @param signNodes Listado de nodos de firma.
     * @return {@code true} En caso de que sea una firma Baseline EN, false en caso contrario.
     * @throws AOFormatFileException Se lanza en caso de que no se pueda identificar la versi&oacute;n
     * de la firma o que esta use una versi&oacute;n de XAdES no v&aacute;lida o no compatible.
     */
    static boolean checkCompatibility(final List<Node> signNodes) throws AOInvalidFormatException {

    	boolean isBaselineENSign = false;

    	// Conjunto con los distintos espacios de nombre XAdES que se han encontrado
    	// a lo largo del listado de nodos de firma
    	final Set<String> xadesNamepaceUris = new HashSet<>();

        for (final Node signNode : signNodes) {

        	final NodeList qualifyingPropsList = ((Element) signNode).getElementsByTagNameNS("*", XAdESConstants.TAG_QUALIFYING_PROPERTIES); //$NON-NLS-1$

        	for (int i = 0 ; i < qualifyingPropsList.getLength() ; i++) {
        		final String namespaceUri = qualifyingPropsList.item(i).getNamespaceURI();

        		boolean existingNamespace = false;

        		for (final String xadesNameSpace : SUPPORTED_XADES_NAMESPACE_URIS) {
        			if (xadesNameSpace.equals(namespaceUri)) {
        				existingNamespace = true;
        				xadesNamepaceUris.add(namespaceUri);
        			}
        		}

            	if (!existingNamespace) {
            		throw new AOInvalidFormatException("Una de las firmas encontradas en el documento contiene una version inexistente de XAdES"); //$NON-NLS-1$
            	}

            	try {
                	final Node signingCertificateNode = ((Element) qualifyingPropsList.item(i)).getChildNodes().item(0).getChildNodes().item(0).getChildNodes().item(1);
                	final String localName = signingCertificateNode.getLocalName();
                	final String signingCertNamespaceUri = signingCertificateNode.getNamespaceURI();

                	if (XAdESConstants.TAG_SIGNING_CERTIFICATE_V2.equals(localName) && namespaceUri.equals(signingCertNamespaceUri)) {
                		isBaselineENSign = true;
                	}
            	} catch(final Exception e) {
            		throw new AOInvalidFormatException("Error al intentar analizar el nodo SigningCertificateV2"); //$NON-NLS-1$
            	}
        	}


        	if (xadesNamepaceUris.size() > 1) {
        		throw new AOInvalidFormatException("El documento contiene firmas con distintas versiones de XAdES"); //$NON-NLS-1$
        	}
        }

        return isBaselineENSign;
    }

    /**
     * Indica si un tipo se corresponde con el que se debe declarar en la referencia a las
     * propiedades firmadas de una firma.
     * @param type Tipo declarado.
     * @return {@code true} si es un tipo SignedProperties, {@code false} en caso contrario.
     */
    static boolean isSignedPropertiesType(final String type) {
    	for (final String signedPropertiesType : SIGNED_PROPERTIES_TYPES) {
    		if (signedPropertiesType.equals(type)) {
    			return true;
    		}
    	}
    	return false;
    }

	static AOXMLAdvancedSignature getXmlAdvancedSignature(final XAdESBase xades,
			                                              final String signedPropertiesTypeUrl,
			                                              final String digestMethodAlgorithm,
			                                              final String canonicalizationAlgorithm,
			                                              final URIDereferencer uriDereferencer) throws AOException {
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

		xmlSignature.setUriDereferencer(uriDereferencer);

		return xmlSignature;
	}

	static Element getFirstElementFromXPath(final String xpathExpression, final Element sourceElement) throws AOException {
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

	/**
	 * Busca un nodo con el atributo 'Id' indicado.
	 * @param nodeId Identificador del nodo que queremos encontrar.
	 * @param currentElement Elemento en el que queremos buscar.
	 * @param omitSignatures Si es {@code true}, se omite la b&uacute;squeda dentro de cualquier
	 * nodo de nombre "Signature", aunque podr&iacute;a referenciarse al propio nodo, {@code false}
	 * en caso contrario.
	 * @return Nodo con el identificador indicado o {@code null} si no
	 * se encuentra el nodo.
	 */
	static Element findElementById(final String nodeId, final Element currentElement, final boolean omitSignatures) {

		// Si es este el nodo, lo devolvemos
		if (nodeId.equals(currentElement.getAttribute(XAdESConstants.ID_IDENTIFIER))) {
			return currentElement;
		}

		// Se podria referenciar a un nodo llamado "Signature", pero omitiriamos
		// la busqueda dentro de cualquier nodo con dicho nombre si asi se indica
		if (omitSignatures && currentElement.getLocalName().equals("Signature")) { //$NON-NLS-1$
			return null;
		}

		// Si no, lo buscamos en cada uno de los hijos, deteniendonos
		// en cuanto se encuentre
		Node item;
		final NodeList childList = currentElement.getChildNodes();
		for (int i = 0; i < childList.getLength(); i++) {
			item = childList.item(i);
			if (item.getNodeType() == Node.ELEMENT_NODE) {
				final Element el = findElementById(nodeId, (Element) item, omitSignatures);
				if (el != null) {
					return el;
				}
			}
		}
		// si no lo encontramos en ninguno de los nodos hijo, devolvemos nulo
		return null;
	}

	/** Obtiene la lista de <i>CommitmentTypeIndication</i> declarados entre los
	 * par&aacute;metros adicionales.
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

		for (int i = 0; i <= nCtis; i++) {

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
		final String nodeName            = xParams.getProperty(XAdESExtraParams.ROOT_XML_NODE_NAME , XAdESConstants.TAG_PARENT_NODE);
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
		afirmaRoot.setAttributeNS(null, XAdESConstants.ID_IDENTIFIER, nodeName + "-Root-" + UUID.randomUUID().toString());  //$NON-NLS-1$

		return afirmaRoot;
	}

	static List<Reference> createManifest(final List<Reference> referenceList,
			                              final XMLSignatureFactory fac,
			                              final AOXMLAdvancedSignature xmlSignature,
			                              final DigestMethod digestMethod,
			                              final Transform canonicalizationTransform,
			                              final String referenceId) {

		// Incluimos las referencias de "referencesList" en el Manifest y luego modificamos el listado
		// para que solo haya una referencia, que sera la del propio manifest. Asi, lo que se firma es
		// el manifest. De no haber usado manifest, lo que se hubiese firmado son las referencias
		// proporcionadas.

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
						XAdESConstants.REFERENCE_TYPE_MANIFEST,
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

	 /** Intenta determinar el prefijo del espacio de nombres de XAdES.
     * @param signatureElement Firma XAdES.
     * @return Prefijo del espacio de nombres o nulo si no se ha
     * establecido prefijo o si no se encuentra el espacio de nombres. */
//    static String guessXAdESNamespacePrefix(final Element el) {
//        final String signatureText = new String(Utils.writeXML(el, null, null, null));
//
//        // Buscamos los espacios de nombres declarados en la firma y despues vemos
//        // si alguno es el de XAdES. En cuanto se detecta uno, se utiliza ese
//        int idx = 0;
//        String ns = null;
//        while (ns == null && (idx = signatureText.indexOf(" xmlns:", idx)) != -1) { //$NON-NLS-1$
//        	final int eqIdx = signatureText.indexOf("=", idx); //$NON-NLS-1$
//        	if (eqIdx != -1) {
//        		final String xadesNsPrefix = signatureText.substring(
//        				eqIdx,
//        				Math.min(eqIdx + "=\"http://uri.etsi.org/".length(), signatureText.length())); //$NON-NLS-1$
//        		if ("=\"http://uri.etsi.org/".equals(xadesNsPrefix)) { //$NON-NLS-1$
//        			ns = signatureText.substring(idx + " xmlns:".length(), eqIdx); //$NON-NLS-1$
//        		}
//        	}
//        	idx++;
//        }
//        return ns;
//    }
//    static String guessXAdESNamespacePrefix(final Element signatureElement) {
//
//    	// Obtenemos la referencia a los atributos firmados de la firma
//    	final Element referenceNode = getSignedPropertiesReference(signatureElement);
//    	if (referenceNode == null) {
//    		return null;
//    	}
//
//    	// Recuperamos el identificador del elementos con los atributos firmados
//    	final String uri = referenceNode.getAttribute("URI"); //$NON-NLS-1$
//    	if (uri == null || !uri.startsWith("#")) { //$NON-NLS-1$
//    		return null;
//    	}
//
//    	// Recuperamos el nodo con los elemento firmados
//    	final String signedPropertiesId = uri.substring(1);
//    	final Element signedPropertiesElement = findElementById(signedPropertiesId, signatureElement, false);
//    	if (signedPropertiesElement == null) {
//    		return null;
//    	}
//
//    	// Obtenemos el prefijo del nodo
//    	return signedPropertiesElement.getPrefix();
//    }

    /**
     * Obtiene la primera firma encontrada en el elemento XML.
     * @param element Elemento XML.
     * @return Primera firma encontrada o nulo si no se encuentra ninguna.
     */
   public static Element getFirstSignatureElement(final Element element) {

    	if (element == null) {
    		return null;
    	}

    	// Localizamos el primer nodo de firma
    	Element signatureElement = null;
    	if (XMLConstants.TAG_SIGNATURE.equals(element.getLocalName())) {
    		signatureElement = element;
    	}
    	else {
    		final NodeList signatures = element.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_SIGNATURE);
    		if (signatures.getLength() > 0) {
    			signatureElement = (Element) signatures.item(0);
    		}
    	}
    	return signatureElement;
    }

    /**
     * Obtiene el elemento con la referencia a los atributos firmados de la firma
     * XAdES indicado.
     * @param signatureElement Elemento "Signature" de una firma XAdES.
     * @return Elemento con la referencia a los atributos firmados o nulo si no se
     * encontr&oacute;.
     */
    static Element getSignedPropertiesReference(final Element signatureElement) {

    	// Obtemos el nodo SignedInfo
    	final Element signedInfoElement = getSignedInfo(signatureElement);
    	if (signedInfoElement == null) {
    		return null;
    	}

    	// Obtenemos las referencias declaradas en la firma
    	final NodeList references = signedInfoElement.getElementsByTagNameNS(
    			XMLConstants.DSIGNNS, XMLConstants.TAG_REFERENCE);

    	// Buscamos entre las referencias hasta encontrar la que declare el tipo
    	// correspondiente a los atributos firmados
    	for (int i = 0; i < references.getLength(); i++) {
    		final Element reference = (Element) references.item(i);
    		final String type = reference.getAttribute("Type"); //$NON-NLS-1$
			if (type != null && !type.isEmpty() && isSignedPropertiesType(type)) {
				return reference;
			}
    	}
    	// Si no se encontro la referencia, se devuelve nulo
    	return null;
    }

	/**
	 * Obtiene el nodo SignedInfo de un elemento de firma individual.
	 * @param signature Elemento de firma.
	 * @return Elemento SignedInfo o {@code null} si no se encuentra.
	 */
    public static Element getSignedInfo(final Element signature) {

    	Element signedInfoElement = null;
    	final NodeList childs = signature.getChildNodes();
    	for (int i = 0; i < childs.getLength() && signedInfoElement == null; i++) {
    		if (childs.item(i).getNodeType() == Node.ELEMENT_NODE
    				&& XMLConstants.DSIGNNS.equals(childs.item(i).getNamespaceURI())
    				&& XMLConstants.TAG_SIGNEDINFO.equals(childs.item(i).getLocalName())) {
    			signedInfoElement = (Element) childs.item(i);
    		}
    	}
    	return signedInfoElement;
    }

    /**
     * Obtiene el elemento SignedProperties de una firma XAdES.
     * @param signatureElement Elemento "Signature" de una firma XAdES.
     * @param signedPropertiesReference Elemento "Reference" que contiene
     * la referencia a los atributos firmados de la firma indicada.
     * @return Elemento "SignedProperties" de una firma XAdES.
     */
    static Element getSignedPropertiesElement(final Element signatureElement, final Element signedPropertiesReference) {

    	// Recuperamos el identificador del elementos con los atributos firmados
    	final String uri = signedPropertiesReference.getAttribute("URI"); //$NON-NLS-1$
    	if (uri == null || !uri.startsWith("#")) { //$NON-NLS-1$
    		return null;
    	}

    	// Recuperamos el nodo con los elemento firmados
    	final String signedPropertiesId = uri.substring(1);

    	return findElementById(signedPropertiesId, signatureElement, false);
    }

    /**
     * Obtiene el elemento SignedProperties de una firma XAdES.
     * @param signatureElement Elemento "Signature" de una firma XAdES.
     * @return Elemento "SignedProperties" de una firma XAdES.
     */
    static Element getSignedPropertiesElement(final Element signatureElement) {

    	// Obtenemos la referencia a los atributos firmados de la firma
    	final Element referenceNode = getSignedPropertiesReference(signatureElement);
    	if (referenceNode == null) {
    		return null;
    	}

    	// Recuperamos el identificador del elementos con los atributos firmados
    	final String uri = referenceNode.getAttribute("URI"); //$NON-NLS-1$
    	if (uri == null || !uri.startsWith("#")) { //$NON-NLS-1$
    		return null;
    	}

    	// Recuperamos el nodo con los elementos firmados
    	final String signedPropertiesId = uri.substring(1);

    	return findElementById(signedPropertiesId, signatureElement, false);
    }

    /**
     * Obtiene el elemento UnsignedProperties de una firma XAdES.
     * @param signatureElement Elemento "Signature" de una firma XAdES.
     * @return Elemento "UnsignedProperties" de una firma XAdES.
     */
    static Element getUnSignedPropertiesElement(final Element signatureElement) {

    	// Obtenemos el elemento de propiedades firmadas
    	final Element signedPropertiesElement = getSignedPropertiesElement(signatureElement);
    	if (signedPropertiesElement == null) {
    		return null;
    	}

    	// El elemento de propiedades sin firmar sera un nodo hermano de este
    	final Element qualifingPropertiesElement = (Element) signedPropertiesElement.getParentNode();
    	return XMLUtils.getChildElementByTagNameNS(
    			qualifingPropertiesElement,
    			XAdESConstants.TAG_UNSIGNED_PROPERTIES,
    			XAdESConstants.NAMESPACE_XADES_1_3_2);
    }

    /**
     * Se obtiene el elemento SignatureMethod de una firma XAdES.
     * @param signatureElement Elemento "Signature" de una firma XAdES.
     * @return Elemento "SignatureMethod" de una firma XAdES.
     */
    public static Element getSignatureMethodElement(final Element signatureElement) {
		final Element signedInfoElement = XAdESUtil.getSignedInfo(signatureElement);
		if (signedInfoElement == null) {
			return null;
		}
		final NodeList signatureMethodList = signedInfoElement.getElementsByTagNameNS(XMLConstants.DSIGNNS, XAdESConstants.TAG_SIGNATURE_METHOD);
		return (Element)signatureMethodList.item(0);
    }


    /**
     * Crea una nueva instancia para firmar.
     * @param profile Perfil de firma XAdES que se quiere generar.
     * @param xadesNamespace Espacio de nombres XAdES.
     * @param xadesSignaturePrefix Prefijo de los elementos XAdES.
     * @param xmlSignaturePrefix Prefijo de los elementos XML.
     * @param digestMethodAlgorithm Algoritmo de huella para la firma firma.
     * @param ownerDocument Documento a firmar.
     * @param rootSig Nodo que se firma.
     * @param signingCertificate Certificado que se va a utilizar si ya se conoce.
     * @return Estructura XAdES en base a la que componer la firma.
     * @throws AOException Cuando ocurre un error durante la composici&oacute;n de los atributos.
     */
	public static XAdESBase newInstance(final String profile, final String xadesNamespace, final String xadesSignaturePrefix,
			final String xmlSignaturePrefix, final String digestMethodAlgorithm, final Document ownerDocument, final Element rootSig,
			final X509Certificate signingCertificate) throws AOException {

		XAdES xadesProfile = XAdES.EPES;
		if (AOSignConstants.SIGN_PROFILE_BASELINE.equalsIgnoreCase(profile)) {
			xadesProfile = XAdES.B_LEVEL;
		}

		final XAdESBase xades = XAdES.newInstance(
				xadesProfile,
				xadesNamespace,
				xadesSignaturePrefix,
				xmlSignaturePrefix,
				digestMethodAlgorithm,
				ownerDocument,
				rootSig);

		// establece el certificado
		if (signingCertificate != null) {
			if (xades instanceof XadesWithBaselineAttributes) {

				// El las firmas B-Level el signingCertificateV2 incluira solo el
				// certificado de firma y no el IssuerSerialV2. Esta es la recomendacion
				// dada en el ETSI EN 319 132-1 V1.1.1, apartado 6.3, anotacion j). Sin
				// embargo, esto hace que algunos validadores conocidos den avisos,
				// cuando no lo hacen si se incluye este atributo
				final SigningCertificateV2Info issuerInfo = null;

//		        final GeneralNames gns = new GeneralNames(new GeneralName(
//		        		X500Name.getInstance(signingCertificate.getIssuerX500Principal().getEncoded())));
//		        final IssuerSerial issuerSerial = new IssuerSerial(gns, signingCertificate.getSerialNumber());
//		        String issuerSerialB64;
//		        try {
//		        	issuerSerialB64 = Base64.encode(issuerSerial.getEncoded());
//		        }
//		        catch (final IOException e) {
//		        	LOGGER.log(Level.WARNING,
//		        			"No se pudo codificar la informacion del IssuerSerial del certificado de firma. Se omitira este campo",  //$NON-NLS-1$
//		        			e);
//		        	issuerSerialB64 = null;
//				}
//				issuerInfo = issuerSerialB64 != null ? new SigningCertificateV2Info(issuerSerialB64) : null;

				((XadesWithBaselineAttributes) xades).setSigningCertificateV2(
						signingCertificate,
						issuerInfo);
			}
			else if (xades instanceof XadesWithBasicAttributes) {
				// El las firmas BES/EPES el signingCertificate incluira el certificado de firma
				// y la referencia al certificado del emisor (IssuerSerial), que sera creada por
				// el propio JXAdES
				((XadesWithBasicAttributes) xades).setSigningCertificate(signingCertificate);
			}
		}

		return xades;
	}

	/**
	 * Indica si un espacio de nombres de XAdES es compatible los perfiles baseline de firma.
	 * Los espacios de nombres compatibles con baseline son los de XAdES 1.3.2 y 1.4.1.
	 * @param xadesNamespace URL del espacio de nombres.
	 * @return {@code true} si es un espacio de nombres compatibles con los perfiles baseline,
	 * {@code false} en caso contrario.
	 */
	static boolean isBaselineCompatible(final String xadesNamespace) {
		return XAdESConstants.NAMESPACE_XADES_1_3_2.equals(xadesNamespace) ||
				XAdESConstants.NAMESPACE_XADES_1_4_1.equals(xadesNamespace);
	}

	/**
     * Obtiene un listado con las referencias a datos de la firma proporcionada.
     * Se considera referencia a datos toda aquella que no sea referencia al
     * SignedProperties ni a un objeto KeyInfo de la firma.
     * @param signatureElement Elemento XML "Signature" de firma.
     * @return Listado con las referencias a datos encontradas.
     */
    public static List<Element> getSignatureDataReferenceList(final Element signatureElement) {

    	// Obtemos el nodo SignedInfo
    	final Element signedInfoElement = getSignedInfo(signatureElement);
    	if (signedInfoElement == null) {
    		return null;
    	}

    	// Obtenemos las referencias declaradas en la firma
    	final NodeList references = signedInfoElement.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_REFERENCE);

    	// Omitimos del listado la referencia a los atributos firmados
    	final List<Element> dataReferences = new ArrayList<>();
    	for (int i = 0; i < references.getLength(); i++) {
    		final Element reference = (Element) references.item(i);
    		final String type = reference.getAttribute("Type"); //$NON-NLS-1$
			if (type != null && !type.isEmpty()) {
				if (!XAdESUtil.isSignedPropertiesType(type)) {
					dataReferences.add(reference);
				}
			}
			// Si no se establecio el tipo de referencia, lo comprobamos a partir de la URI
			else {
				final String uri = reference.getAttribute("URI"); //$NON-NLS-1$

				// Si es una referencia interna, comprobamos que no sea el KeyInfo o el SignedProperties
				if (uri != null && uri.startsWith("#")) { //$NON-NLS-1$
					final String elementId = uri.substring(1);
					final Node referencedNode = XAdESUtil.findElementById(elementId, signatureElement, false);

					// Si el nodo referenciado no esta dentro del nodo de firma, es que es una referencia
					// externa a datos
					if (referencedNode == null) {
						dataReferences.add(reference);
					}
					// Si no, comprobamos que no sea una referencia al KeyInfo o al SignedProperties
					else {
						final String nodeName = referencedNode.getLocalName();
						if (!nodeName.equals("KeyInfo") && !nodeName.equals("SignedProperties")) { //$NON-NLS-1$ //$NON-NLS-2$
							dataReferences.add(reference);
						}
					}
				}
				// Cualquier referencia no interna hay que firmarla
				else {
					dataReferences.add(reference);
				}
			}
    	}

    	return dataReferences;
    }

	/**
     * Indica si la firma contiene una referencia a un manifest.
     * @param signatureElement Elemento XML "Signature" de firma.
     * @return Listado con las referencias a datos encontradas.
     */
    public static boolean hasHashManifestReference(final Element signatureElement) {

    	// Obtemos el nodo SignedInfo
    	final Element signedInfoElement = getSignedInfo(signatureElement);
    	if (signedInfoElement == null) {
    		return false;
    	}

    	// Obtenemos las referencias declaradas en la firma
    	final NodeList references = signedInfoElement.getElementsByTagNameNS(XMLConstants.DSIGNNS, XMLConstants.TAG_REFERENCE);

    	// Omitimos del listado la referencia a los atributos firmados
    	for (int i = 0; i < references.getLength(); i++) {
    		final Element reference = (Element) references.item(i);
    		final String type = reference.getAttribute("Type"); //$NON-NLS-1$
			if (XAdESConstants.REFERENCE_TYPE_MANIFEST.equals(type)) {
				return true;
			}
    	}

    	return false;
    }

    /**
     * Comprueba que el perfil de una firma sea compatible con el perfil con el que se va a multifirmar.
     * En caso de no serlo y haberse solicitado, se le consultar&aacute; si desea continuar o no.
     * @param extraParams Par&aacute;metros de configuraci&oacute;n de la multifirma.
     * @param signatureIsBaselineEN Indica si la firma que se va a multificar es una firma Baseline EN o no.
     */
    public static void checkSignProfile(final Properties extraParams, final boolean signatureIsBaselineEN) {

    	if (Boolean.TRUE.equals(extraParams.get(XAdESExtraParams.CONFIRM_DIFFERENT_PROFILE))) {
    		final String profile = extraParams.getProperty(XAdESExtraParams.PROFILE, AOSignConstants.DEFAULT_SIGN_PROFILE);
    		final boolean baselineENRequested = AOSignConstants.SIGN_PROFILE_BASELINE.equals(profile);
    		if (signatureIsBaselineEN != baselineENRequested){
    			final int option = AOUIFactory.showConfirmDialog(
						null,
						XAdESMessages.getString("AOXAdESSigner.0"), //$NON-NLS-1$
						XAdESMessages.getString("AOXAdESSigner.1"), //$NON-NLS-1$
						AOUIFactory.YES_NO_OPTION,
						AOUIFactory.WARNING_MESSAGE);

    			if (option == 0) {
    				if (signatureIsBaselineEN) {
    					extraParams.put(XAdESExtraParams.PROFILE, AOSignConstants.SIGN_PROFILE_BASELINE);
    				} else {
    					extraParams.put(XAdESExtraParams.PROFILE, AOSignConstants.SIGN_PROFILE_ADVANCED);
    				}
    			}
    		}
    	}
    	else {
			if (signatureIsBaselineEN) {
				extraParams.put(XAdESExtraParams.PROFILE, AOSignConstants.SIGN_PROFILE_BASELINE);
			} else {
				extraParams.put(XAdESExtraParams.PROFILE, AOSignConstants.SIGN_PROFILE_ADVANCED);
			}
    	}
    }

    /**
     * Indica si los datos a firmar son obligatorios para la operaci&oacute;n de firma con la
     * configuraci&oacute;n proporcionada.
     * @param cryptoOperation Operaci&oacute;n criptogr&aacute;fica (SIGN, COSIGN o COUNTERSIGN).
     * @param config Configuraci&oacute;n de firma (extraParams).
     * @return {@code true} si la operaci&oacute;n de firma requiere los datos a firmar,
     * {@code false} en caso de que la configuraci&oacute;n pueda ya proporcionar estos datos.
     */
    public static boolean isDataMandatory(final String cryptoOperation, final Properties config) {

    	// Sera obligatorio que se indiquen los datos de entrada para las cofirmas y contrafirmas
    	// y siempre que el formato no sea Externally Detached y no se trate de una firma manifest
    	return !CRYPTO_OPERATION_SIGN.equalsIgnoreCase(cryptoOperation)
    			|| config == null
    			|| !AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED.equals(config.getProperty(XAdESExtraParams.FORMAT))
    					&& !Boolean.parseBoolean(config.getProperty(XAdESExtraParams.USE_MANIFEST));
    }

    /**
     * Comprueba si alguna de las firmas proporcionadas incluye un sello de archivo.
     * @param signatures Listado de firmas que verificar.
     * @throws SigningLTSException Cuando alguna de las firmas incluye sello de archivo.
     */
    public static void checkArchiveSignatures(final NodeList signatures) throws SigningLTSException {
    	for (int i = 0; i < signatures.getLength(); i++) {
    		final Element signature = (Element) signatures.item(i);
    		checkArchiveSignatures(signature);
    	}
    }

    /**
     * Comprueba si el elemento de firma proporcionado incluye un sello de archivo.
     * @param signature Elemento de firma XML.
     * @throws SigningLTSException Cuando alguna de las firmas incluye sello de archivo.
     */
    public static void checkArchiveSignatures(final Element signature) throws SigningLTSException {
    	final Element unsignedProperties = getUnSignedPropertiesElement(signature);
    	if (unsignedProperties != null) {
    		final Element unsignedSignatureProperties = XMLUtils.getChildElementByTagNameNS(unsignedProperties,
    				XAdESConstants.TAG_UNSIGNED_SIGNATURE_PROPERTIES, XAdESConstants.NAMESPACE_XADES_1_3_2);
    		if (unsignedSignatureProperties != null) {
    			Element archiveTimeStamp = XMLUtils.getChildElementByTagNameNS(unsignedSignatureProperties,
    					XAdESConstants.TAG_ARCHIVE_TIMESTAMP, XAdESConstants.NAMESPACE_XADES_1_4_1);
    			if (archiveTimeStamp != null) {
    				throw new SigningLTSException("Se han encontrado firmas de sello de archivo"); //$NON-NLS-1$
    			}
    			archiveTimeStamp = XMLUtils.getChildElementByTagNameNS(unsignedSignatureProperties,
    					XAdESConstants.TAG_ARCHIVE_TIMESTAMP, XAdESConstants.NAMESPACE_XADES_1_3_2);
    			if (archiveTimeStamp != null) {
    				throw new SigningLTSException("Se han encontrado firmas de sello de archivo"); //$NON-NLS-1$
    			}
    			archiveTimeStamp = XMLUtils.getChildElementByTagNameNS(unsignedSignatureProperties,
    					XAdESConstants.TAG_ARCHIVE_TIMESTAMP, XAdESConstants.NAMESPACE_XADES_1_2_2);
    			if (archiveTimeStamp != null) {
    				throw new SigningLTSException("Se han encontrado firmas de sello de archivo"); //$NON-NLS-1$
    			}
    		}
    	}
    }

    /**
     * Comprueba a trav&eacute;s de las referencias a datos declaradas en una firma si se
     * trata de una firma enveloped.
     * @param signatureElement Elemento con la firma a comprobar.
     * @param references Referencias a datos declaradas en la firma.
     * @return {@code true} si es una firma enveloped, {@code false} en caso contrario.
     */
    public static boolean isSignatureElementEnveloped(final Element signatureElement, final List<Element> references) {

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

    /**
     * Indica si la firma utiliza un manifest para referenciar datos.
     * @param references Referencias a datos encontrados en la firma.
     * @return {@code true} si es una firma con manifest, {@code false} en caso contrario.
     */
    public static boolean isSignatureWithManifest(final List<Element> references) {

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

    /**
     * Indica si la firma a la que pertenecen las referencias usadas es externally detached.
     * @param references Referencias a datos encontrados en la firma.
     * @return {@code true} si la firma es externally detached, {@code false} en caso contrario.
     */
    public static boolean isSignatureElementExternallyDetached(final List<Element> references) {

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

    /**
     * Indica si la firma a la que pertenecen las referencias usadas es internally detached.
     * @param docElement Elemento ra&iacute;z del XML.
     * @param references Referencias a datos encontrados en la firma.
     * @return {@code true} si la firma es internally detached, {@code false} en caso contrario.
     */
    public static boolean isSignatureElementInternallyDetached(final Element docElement, final List<Element> references) {

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

    /**
     * Comprueba a trav&eacute;s de las referencias a datos declaradas en una firma si se
     * trata de una firma enveloping.
     * @param signatureElement Elemento con la firma a comprobar.
     * @param references Referencias a datos declaradas en la firma.
     * @return {@code true} si es una firma enveloping, {@code false} en caso contrario.
     */
    public static boolean isSignatureElementEnveloping(final Element signatureElement, final List<Element> references) {

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
}
