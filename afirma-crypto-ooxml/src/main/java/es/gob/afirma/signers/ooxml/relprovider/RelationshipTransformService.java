/*
 * eID Applet Project.
 * Copyright (C) 2009 FedICT.
 * Copyright (C) 2009 Frank Cornelis.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License version
 * 3.0 as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, see
 * http://www.gnu.org/licenses/.
 */

/*
 * Copyright (C) 2008-2009 FedICT.
 * Copyright (C) 2009 Frank Cornelis.
 *
 * This file is part of the eID Applet Project.
 *
 * Licensed under the Apache License, Version 2.0 (the License).
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package es.gob.afirma.signers.ooxml.relprovider;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.security.InvalidAlgorithmParameterException;
import java.security.spec.AlgorithmParameterSpec;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.XMLConstants;
import javax.xml.crypto.Data;
import javax.xml.crypto.OctetStreamData;
import javax.xml.crypto.XMLCryptoContext;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.TransformException;
import javax.xml.crypto.dsig.TransformService;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.namespace.NamespaceContext;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import es.gob.afirma.core.misc.SecureXmlBuilder;
import es.gob.afirma.core.misc.SecureXmlTransformer;

/** Implementaci&oacute;n JSR105 de la transformaci&oacute;n RelationshipTransform.
 * <a href="http://openiso.org/Ecma/376/Part2/12.2.4#26">http://openiso.org/Ecma/376/Part2/12.2.4#26</a>
 * @author Frank Cornelis */
public final class RelationshipTransformService extends TransformService {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String NAMESPACE_SPEC_NS = "http://www.w3.org/2000/xmlns/"; //$NON-NLS-1$
	private static final String SIGNATURE_SPEC_NS = "http://www.w3.org/2000/09/xmldsig#"; //$NON-NLS-1$

	/** URI de declaraci&oacute;n de la transformaci&oacute;n. */
    public static final String TRANSFORM_URI = "http://schemas.openxmlformats.org/package/2006/RelationshipTransform"; //$NON-NLS-1$

    private final List<String> sourceIds;

    /** Crea el servicio de la transformaci&oacute;n RelationshipTransform. */
    public RelationshipTransformService() {
		this.sourceIds = new LinkedList<>();
    }

    /** {@inheritDoc} */
    @Override
    public void init(final TransformParameterSpec params) throws InvalidAlgorithmParameterException {
        if (!(params instanceof RelationshipTransformParameterSpec)) {
            throw new InvalidAlgorithmParameterException();
        }
        final RelationshipTransformParameterSpec relParams = (RelationshipTransformParameterSpec) params;
        for (final String sourceId : relParams.getSourceIds()) {
            this.sourceIds.add(sourceId);
        }
    }

    /** {@inheritDoc} */
	@Override
    public void init(final XMLStructure parent, final XMLCryptoContext context) throws InvalidAlgorithmParameterException {

        final DOMStructure domParent = (DOMStructure) parent;
        final Node parentNode = domParent.getNode();
        try {
            toString(parentNode);
        }
        catch (final TransformerException e) {
            throw new InvalidAlgorithmParameterException(e);
        }

        NodeList nodeList;
        try {
			final XPath xpath = XPathFactory.newInstance().newXPath();
			xpath.setNamespaceContext(
				new NamespaceContext() {

					// No se parametriza el iterador para mantener compatibilidad entre las distintas versiones de Java
					@SuppressWarnings("rawtypes")
					@Override
					public Iterator getPrefixes(final String namespaceURI) {
						throw new UnsupportedOperationException();
					}

					@Override
					public String getPrefix(final String namespaceURI) {
						throw new UnsupportedOperationException();
					}

					@Override
					public String getNamespaceURI(final String prefix) {
						if (prefix == null) {
							throw new IllegalArgumentException("El prefijo no puede ser nulo"); //$NON-NLS-1$
						}
						if ("xml".equals(prefix)) { //$NON-NLS-1$
							return XMLConstants.XML_NS_URI;
						}
						if ("ds".equals(prefix)) { //$NON-NLS-1$
							return SIGNATURE_SPEC_NS;
						}
						if ("mdssi".equals(prefix)) { //$NON-NLS-1$
							return "http://schemas.openxmlformats.org/package/2006/digital-signature"; //$NON-NLS-1$
						}
						return XMLConstants.NULL_NS_URI;
					}
				}
			);
			final XPathExpression exp = xpath.compile(
				"mdssi:RelationshipReference/@SourceId" //$NON-NLS-1$
			);
			nodeList = (NodeList) exp.evaluate(
				parentNode,
				XPathConstants.NODESET
			);

        }
        catch (final Exception e) {
            LOGGER.severe("Error en la transformacion XPath: " + e); //$NON-NLS-1$
            throw new InvalidAlgorithmParameterException(e);
        }
        if (0 == nodeList.getLength()) {
            LOGGER.warning("no RelationshipReference/@SourceId parameters present"); //$NON-NLS-1$
        }
        for (int nodeIdx = 0; nodeIdx < nodeList.getLength(); nodeIdx++) {
            final Node node = nodeList.item(nodeIdx);
            final String sourceId = node.getTextContent();
            this.sourceIds.add(sourceId);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void marshalParams(final XMLStructure parent, final XMLCryptoContext context) {
        final DOMStructure domParent = (DOMStructure) parent;
        final Node parentNode = domParent.getNode();
        final Element parentElement = (Element) parentNode;
        parentElement.setAttributeNS(
    		NAMESPACE_SPEC_NS,
    		"xmlns:mdssi", //$NON-NLS-1$
    		"http://schemas.openxmlformats.org/package/2006/digital-signature" //$NON-NLS-1$
		);
        final Document document = parentNode.getOwnerDocument();
        for (final String sourceId : this.sourceIds) {
            final Element relationshipReferenceElement =
                    document.createElementNS("http://schemas.openxmlformats.org/package/2006/digital-signature", "mdssi:RelationshipReference"); //$NON-NLS-1$ //$NON-NLS-2$
            relationshipReferenceElement.setAttribute("SourceId", sourceId); //$NON-NLS-1$
            parentElement.appendChild(relationshipReferenceElement);
        }
    }

    /** {@inheritDoc} */
    @Override
	public AlgorithmParameterSpec getParameterSpec() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
	public Data transform(final Data data, final XMLCryptoContext context) throws TransformException {

        final OctetStreamData octetStreamData = (OctetStreamData) data;

        final Document relationshipsDocument;
        try (
    		final InputStream octetStream = octetStreamData.getOctetStream();
		) {
            relationshipsDocument = loadDocument(octetStream);
            octetStream.close();
        }
        catch (final Exception e) {
            throw new TransformException(e.getMessage(), e);
        }
        try {
            toString(relationshipsDocument);
        }
        catch (final TransformerException e) {
            throw new TransformException(e.getMessage(), e);
        }
        final Element nsElement = relationshipsDocument.createElement("ns"); //$NON-NLS-1$
        nsElement.setAttributeNS(NAMESPACE_SPEC_NS, "xmlns:tns", "http://schemas.openxmlformats.org/package/2006/relationships");  //$NON-NLS-1$//$NON-NLS-2$
        final Element relationshipsElement = relationshipsDocument.getDocumentElement();
        final NodeList childNodes = relationshipsElement.getChildNodes();
        for (int nodeIdx = 0; nodeIdx < childNodes.getLength(); nodeIdx++) {
            final Node childNode = childNodes.item(nodeIdx);
            if (Node.ELEMENT_NODE != childNode.getNodeType()) {
                relationshipsElement.removeChild(childNode);
                nodeIdx--;
                continue;
            }
            final Element childElement = (Element) childNode;
            final String idAttribute = childElement.getAttribute("Id"); //$NON-NLS-1$
            if (!this.sourceIds.contains(idAttribute)) {
                relationshipsElement.removeChild(childNode);
                nodeIdx--;
            }
            /*
             * See: ISO/IEC 29500-2:2008(E) - 13.2.4.24 Relationships Transform
             * Algorithm.
             */
            if (null == childElement.getAttributeNode("TargetMode")) { //$NON-NLS-1$
                childElement.setAttribute("TargetMode", "Internal"); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }

        sortRelationshipElements(relationshipsElement);
        try {
            return toOctetStreamData(relationshipsDocument);
        }
        catch (final TransformerException e) {
            throw new TransformException(e.getMessage(), e);
        }
    }

    private static void sortRelationshipElements(final Element relationshipsElement) {
        final List<Element> relationshipElements = new LinkedList<>();
        final NodeList relationshipNodes = relationshipsElement.getElementsByTagName("*"); //$NON-NLS-1$
        final int nodeCount = relationshipNodes.getLength();
        for (int nodeIdx = 0; nodeIdx < nodeCount; nodeIdx++) {
            final Node relationshipNode = relationshipNodes.item(0);
            final Element relationshipElement = (Element) relationshipNode;
            relationshipElements.add(relationshipElement);
            relationshipsElement.removeChild(relationshipNode);
        }
        Collections.sort(relationshipElements, new RelationshipComparator());
        for (final Element relationshipElement : relationshipElements) {
            relationshipsElement.appendChild(relationshipElement);
        }
    }

    private static String toString(final Node dom) throws TransformerException {
        final Source source = new DOMSource(dom);
        final StringWriter stringWriter = new StringWriter();
        final Result result = new StreamResult(stringWriter);
        final Transformer transformer = SecureXmlTransformer.getSecureTransformer();
        /*
         * We have to omit the ?xml declaration if we want to embed the
         * document.
         */
        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes"); //$NON-NLS-1$
        transformer.transform(source, result);
        return stringWriter.getBuffer().toString();
    }

    private static OctetStreamData toOctetStreamData(final Node node) throws TransformerException {
        final Source source = new DOMSource(node);
        final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        final Result result = new StreamResult(outputStream);
        final Transformer transformer = SecureXmlTransformer.getSecureTransformer();
        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes"); //$NON-NLS-1$
        transformer.transform(source, result);
        return new OctetStreamData(new ByteArrayInputStream(outputStream.toByteArray()));
    }

    private static Document loadDocument(final InputStream documentInputStream) throws ParserConfigurationException, SAXException, IOException {
        final InputSource inputSource = new InputSource(documentInputStream);
        return SecureXmlBuilder.getSecureDocumentBuilder().parse(inputSource);
    }

    /** {@inheritDoc} */
    @Override
	public Data transform(final Data data, final XMLCryptoContext context, final OutputStream os) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
	public boolean isFeatureSupported(final String feature) {
        return false;
    }
}
