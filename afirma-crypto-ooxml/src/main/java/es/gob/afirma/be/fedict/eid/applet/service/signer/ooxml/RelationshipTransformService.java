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
 * Licensed under the Apache License, Version 2.0 (the "License");
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

package es.gob.afirma.be.fedict.eid.applet.service.signer.ooxml;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.security.InvalidAlgorithmParameterException;
import java.security.spec.AlgorithmParameterSpec;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.crypto.Data;
import javax.xml.crypto.MarshalException;
import javax.xml.crypto.OctetStreamData;
import javax.xml.crypto.XMLCryptoContext;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.TransformException;
import javax.xml.crypto.dsig.TransformService;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.security.utils.Constants;
import com.sun.org.apache.xpath.internal.XPathAPI;

/** JSR105 implementation of the RelationshipTransform transformation.
 * <p>
 * Specs: http://openiso.org/Ecma/376/Part2/12.2.4#26
 * </p>
 * @author Frank Cornelis */
@SuppressWarnings("restriction")
public class RelationshipTransformService extends TransformService {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    static final String TRANSFORM_URI = "http://schemas.openxmlformats.org/package/2006/RelationshipTransform"; //$NON-NLS-1$

    private final List<String> sourceIds;

    private RelationshipTransformService() {
        super();
        this.sourceIds = new LinkedList<String>();
    }

    @Override
    public void init(final TransformParameterSpec params) throws InvalidAlgorithmParameterException {
        if (false == params instanceof RelationshipTransformParameterSpec) {
            throw new InvalidAlgorithmParameterException();
        }
        final RelationshipTransformParameterSpec relParams = (RelationshipTransformParameterSpec) params;
        for (final String sourceId : relParams.getSourceIds()) {
            this.sourceIds.add(sourceId);
        }
    }

    @Override
    public void init(final XMLStructure parent, final XMLCryptoContext context) throws InvalidAlgorithmParameterException {

        final DOMStructure domParent = (DOMStructure) parent;
        final Node parentNode = domParent.getNode();
        try {
            /* System.out.println("parent: " + */toString(parentNode)/* ) */;
        }
        catch (final TransformerException e) {
            throw new InvalidAlgorithmParameterException();
        }
        final Element nsElement = parentNode.getOwnerDocument().createElement("ns"); //$NON-NLS-1$
        nsElement.setAttributeNS(Constants.NamespaceSpecNS, "xmlns:ds", Constants.SignatureSpecNS); //$NON-NLS-1$
        nsElement.setAttributeNS(Constants.NamespaceSpecNS, "xmlns:mdssi", "http://schemas.openxmlformats.org/package/2006/digital-signature"); //$NON-NLS-1$ //$NON-NLS-2$
        NodeList nodeList;
        try {
            nodeList = XPathAPI.selectNodeList(parentNode, "mdssi:RelationshipReference/@SourceId", nsElement); //$NON-NLS-1$
        }
        catch (final TransformerException e) {
            LOGGER.severe("transformer exception: " + e.getMessage()); //$NON-NLS-1$
            throw new InvalidAlgorithmParameterException();
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

    @Override
    public void marshalParams(final XMLStructure parent, final XMLCryptoContext context) throws MarshalException {
        final DOMStructure domParent = (DOMStructure) parent;
        final Node parentNode = domParent.getNode();
        final Element parentElement = (Element) parentNode;
        parentElement.setAttributeNS(Constants.NamespaceSpecNS, "xmlns:mdssi", "http://schemas.openxmlformats.org/package/2006/digital-signature"); //$NON-NLS-1$ //$NON-NLS-2$
        final Document document = parentNode.getOwnerDocument();
        for (final String sourceId : this.sourceIds) {
            final Element relationshipReferenceElement =
                    document.createElementNS("http://schemas.openxmlformats.org/package/2006/digital-signature", "mdssi:RelationshipReference"); //$NON-NLS-1$ //$NON-NLS-2$
            relationshipReferenceElement.setAttribute("SourceId", sourceId); //$NON-NLS-1$
            parentElement.appendChild(relationshipReferenceElement);
        }
    }

    public AlgorithmParameterSpec getParameterSpec() {
        return null;
    }

    public Data transform(final Data data, final XMLCryptoContext context) throws TransformException {

        final OctetStreamData octetStreamData = (OctetStreamData) data;

        final InputStream octetStream = octetStreamData.getOctetStream();
        Document relationshipsDocument;
        try {
            relationshipsDocument = loadDocument(octetStream);
        }
        catch (final Exception e) {
            throw new TransformException(e.getMessage(), e);
        }
        try {
            /*
             * System.out.println("relationships document: " +
             */toString(relationshipsDocument)/* ) */;
        }
        catch (final TransformerException e) {
            throw new TransformException(e.getMessage(), e);
        }
        final Element nsElement = relationshipsDocument.createElement("ns"); //$NON-NLS-1$
        nsElement.setAttributeNS(Constants.NamespaceSpecNS, "xmlns:tns", "http://schemas.openxmlformats.org/package/2006/relationships");  //$NON-NLS-1$//$NON-NLS-2$
        final Element relationshipsElement = relationshipsDocument.getDocumentElement();
        final NodeList childNodes = relationshipsElement.getChildNodes();
        for (int nodeIdx = 0; nodeIdx < childNodes.getLength(); nodeIdx++) {
            final Node childNode = childNodes.item(nodeIdx);
            if (Node.ELEMENT_NODE != childNode.getNodeType()) {
                // System.out.println("removing node");
                relationshipsElement.removeChild(childNode);
                nodeIdx--;
                continue;
            }
            final Element childElement = (Element) childNode;
            final String idAttribute = childElement.getAttribute("Id"); //$NON-NLS-1$
            if (false == this.sourceIds.contains(idAttribute)) {
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

    private void sortRelationshipElements(final Element relationshipsElement) {
        final List<Element> relationshipElements = new LinkedList<Element>();
        final NodeList relationshipNodes = relationshipsElement.getElementsByTagName("*"); //$NON-NLS-1$
        final int nodeCount = relationshipNodes.getLength();
        for (int nodeIdx = 0; nodeIdx < nodeCount; nodeIdx++) {
            final Node relationshipNode = relationshipNodes.item(0);
            final Element relationshipElement = (Element) relationshipNode;
            // System.out.println("unsorted Id: " +
            // relationshipElement.getAttribute("Id"));
            relationshipElements.add(relationshipElement);
            relationshipsElement.removeChild(relationshipNode);
        }
        Collections.sort(relationshipElements, new RelationshipComparator());
        for (final Element relationshipElement : relationshipElements) {
            // System.out.println("sorted Id: " +
            // relationshipElement.getAttribute("Id"));
            relationshipsElement.appendChild(relationshipElement);
        }
    }

    private String toString(final Node dom) throws TransformerException {
        final Source source = new DOMSource(dom);
        final StringWriter stringWriter = new StringWriter();
        final Result result = new StreamResult(stringWriter);
        final TransformerFactory transformerFactory = TransformerFactory.newInstance();
        final Transformer transformer = transformerFactory.newTransformer();
        /*
         * We have to omit the ?xml declaration if we want to embed the
         * document.
         */
        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes"); //$NON-NLS-1$
        transformer.transform(source, result);
        return stringWriter.getBuffer().toString();
    }

    private OctetStreamData toOctetStreamData(final Node node) throws TransformerException {
        final Source source = new DOMSource(node);
        final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        final Result result = new StreamResult(outputStream);
        final TransformerFactory transformerFactory = TransformerFactory.newInstance();
        final Transformer transformer = transformerFactory.newTransformer();
        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes"); //$NON-NLS-1$
        transformer.transform(source, result);
        // System.out.println("result: " + new
        // String(outputStream.toByteArray()));
        return new OctetStreamData(new ByteArrayInputStream(outputStream.toByteArray()));
    }

    private Document loadDocument(final InputStream documentInputStream) throws ParserConfigurationException, SAXException, IOException {
        final InputSource inputSource = new InputSource(documentInputStream);
        final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);
        final DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
        final Document document = documentBuilder.parse(inputSource);
        return document;
    }

    public Data transform(final Data data, final XMLCryptoContext context, final OutputStream os) throws TransformException {
        // System.out.println("transform(data,context,os)");
        return null;
    }

    public boolean isFeatureSupported(final String feature) {
        // System.out.println("isFeatureSupported(feature)");
        return false;
    }
}
