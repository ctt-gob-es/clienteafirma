/*
 * eID Applet Project.
 * Copyright (C) 2009 FedICT.
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

package es.gob.afirma.signers.ooxml.be.fedict.eid.applet.service.signer.ooxml;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Manifest;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureProperties;
import javax.xml.crypto.dsig.SignatureProperty;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.security.utils.Constants;
import com.sun.org.apache.xpath.internal.XPathAPI;

import es.gob.afirma.signers.ooxml.be.fedict.eid.applet.service.signer.NoCloseInputStream;
import es.gob.afirma.signers.ooxml.be.fedict.eid.applet.service.signer.SignatureFacet;

/** Office OpenXML Signature Facet implementation.
 * @author fcorneli
 * @see "http://msdn.microsoft.com/en-us/library/cc313071.aspx" */
final class OOXMLSignatureFacet implements SignatureFacet {

    private final AbstractOOXMLSignatureService signatureService;
    
    private static final String DIGITAL_SIGNATURE_SCHEMA = "http://schemas.openxmlformats.org/package/2006/digital-signature"; //$NON-NLS-1$

    /** Main constructor.
     * @param signatureService */
    OOXMLSignatureFacet(final AbstractOOXMLSignatureService signatureService) {
        this.signatureService = signatureService;
    }

    public void preSign(final XMLSignatureFactory signatureFactory,
                        final Document document,
                        final String signatureId,
                        final List<X509Certificate> signingCertificateChain,
                        final List<Reference> references,
                        final List<XMLObject> objects) throws NoSuchAlgorithmException, 
                                                              InvalidAlgorithmParameterException, 
                                                              IOException, 
                                                              ParserConfigurationException, 
                                                              SAXException, TransformerException {

        addManifestObject(signatureFactory, document, signatureId, references, objects);

        addSignatureInfo(signatureFactory, document, signatureId, references, objects);
    }

    private void addManifestObject(final XMLSignatureFactory signatureFactory,
                                   final Document document,
                                   final String signatureId,
                                   final List<Reference> references,
                                   final List<XMLObject> objects) throws NoSuchAlgorithmException, 
                                                                         InvalidAlgorithmParameterException, 
                                                                         IOException, 
                                                                         ParserConfigurationException, 
                                                                         SAXException, TransformerException {
        final Manifest manifest = constructManifest(signatureFactory);
        final String objectId = "idPackageObject"; // really has to be this value. //$NON-NLS-1$
        final List<XMLStructure> objectContent = new LinkedList<XMLStructure>();
        objectContent.add(manifest);

        addSignatureTime(signatureFactory, document, signatureId, objectContent);

        objects.add(signatureFactory.newXMLObject(objectContent, objectId, null, null));

        final DigestMethod digestMethod = signatureFactory.newDigestMethod(DigestMethod.SHA1, null);
        final Reference reference = signatureFactory.newReference("#" + objectId, digestMethod, null, "http://www.w3.org/2000/09/xmldsig#Object", null); //$NON-NLS-1$ //$NON-NLS-2$
        references.add(reference);
    }

    private Manifest constructManifest(final XMLSignatureFactory signatureFactory) throws NoSuchAlgorithmException,
                                                                                          InvalidAlgorithmParameterException, 
                                                                                          IOException, 
                                                                                          ParserConfigurationException, 
                                                                                          SAXException, TransformerException {
        final List<Reference> manifestReferences = new LinkedList<Reference>();
        addRelationshipsReferences(signatureFactory, manifestReferences);

        /*
         * Word
         */
        addParts(signatureFactory, "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml", manifestReferences); //$NON-NLS-1$
        addParts(signatureFactory, "application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml", manifestReferences); //$NON-NLS-1$
        addParts(signatureFactory, "application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml", manifestReferences); //$NON-NLS-1$
        addParts(signatureFactory, "application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml", manifestReferences); //$NON-NLS-1$
        addParts(signatureFactory, "application/vnd.openxmlformats-officedocument.theme+xml", manifestReferences); //$NON-NLS-1$
        addParts(signatureFactory, "application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml", manifestReferences); //$NON-NLS-1$

        /*
         * Powerpoint
         */
        addParts(signatureFactory, "application/vnd.openxmlformats-officedocument.presentationml.presentation.main+xml", manifestReferences); //$NON-NLS-1$
        addParts(signatureFactory, "application/vnd.openxmlformats-officedocument.presentationml.slideLayout+xml", manifestReferences); //$NON-NLS-1$
        addParts(signatureFactory, "application/vnd.openxmlformats-officedocument.presentationml.slideMaster+xml", manifestReferences); //$NON-NLS-1$
        addParts(signatureFactory, "application/vnd.openxmlformats-officedocument.presentationml.slide+xml", manifestReferences); //$NON-NLS-1$
        addParts(signatureFactory, "application/vnd.openxmlformats-officedocument.presentationml.tableStyles+xml", manifestReferences); //$NON-NLS-1$

        return signatureFactory.newManifest(manifestReferences);
    }

    private static void addSignatureTime(final XMLSignatureFactory signatureFactory, final Document document, final String signatureId, final List<XMLStructure> objectContent) {
        /*
         * SignatureTime
         */
        final Element signatureTimeElement =
                document.createElementNS(DIGITAL_SIGNATURE_SCHEMA, "mdssi:SignatureTime"); //$NON-NLS-1$ 
        signatureTimeElement.setAttributeNS(Constants.NamespaceSpecNS,
                                            "xmlns:mdssi", //$NON-NLS-1$
                                            DIGITAL_SIGNATURE_SCHEMA); 
        final Element formatElement = document.createElementNS(DIGITAL_SIGNATURE_SCHEMA, "mdssi:Format"); //$NON-NLS-1$ 
        formatElement.setTextContent("YYYY-MM-DDThh:mm:ssTZD"); //$NON-NLS-1$
        signatureTimeElement.appendChild(formatElement);
        final Element valueElement = document.createElementNS(DIGITAL_SIGNATURE_SCHEMA, "mdssi:Value"); //$NON-NLS-1$ 
        valueElement.setTextContent(new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss'Z'").format(new Date())); //$NON-NLS-1$
        signatureTimeElement.appendChild(valueElement);

        final List<XMLStructure> signatureTimeContent = new LinkedList<XMLStructure>();
        signatureTimeContent.add(new DOMStructure(signatureTimeElement));
        final SignatureProperty signatureTimeSignatureProperty =
                signatureFactory.newSignatureProperty(signatureTimeContent, "#" + signatureId, "idSignatureTime"); //$NON-NLS-1$ //$NON-NLS-2$
        final List<SignatureProperty> signaturePropertyContent = new LinkedList<SignatureProperty>();
        signaturePropertyContent.add(signatureTimeSignatureProperty);
        final SignatureProperties signatureProperties =
                signatureFactory.newSignatureProperties(signaturePropertyContent, "id-signature-time-" + UUID.randomUUID().toString()); //$NON-NLS-1$
        objectContent.add(signatureProperties);
    }

    private static void addSignatureInfo(final XMLSignatureFactory signatureFactory,
                                  final Document document,
                                  final String signatureId,
                                  final List<Reference> references,
                                  final List<XMLObject> objects) throws NoSuchAlgorithmException, InvalidAlgorithmParameterException {
        final List<XMLStructure> objectContent = new LinkedList<XMLStructure>();

        final Element signatureInfoElement = document.createElementNS("http://schemas.microsoft.com/office/2006/digsig", "SignatureInfoV1"); //$NON-NLS-1$ //$NON-NLS-2$
        signatureInfoElement.setAttributeNS(Constants.NamespaceSpecNS, "xmlns", "http://schemas.microsoft.com/office/2006/digsig"); //$NON-NLS-1$ //$NON-NLS-2$

        final Element manifestHashAlgorithmElement = document.createElementNS("http://schemas.microsoft.com/office/2006/digsig", "ManifestHashAlgorithm"); //$NON-NLS-1$ //$NON-NLS-2$
        manifestHashAlgorithmElement.setTextContent("http://www.w3.org/2000/09/xmldsig#sha1"); //$NON-NLS-1$
        signatureInfoElement.appendChild(manifestHashAlgorithmElement);

        final List<XMLStructure> signatureInfoContent = new LinkedList<XMLStructure>();
        signatureInfoContent.add(new DOMStructure(signatureInfoElement));
        final SignatureProperty signatureInfoSignatureProperty =
                signatureFactory.newSignatureProperty(signatureInfoContent, "#" + signatureId, "idOfficeV1Details"); //$NON-NLS-1$ //$NON-NLS-2$

        final List<SignatureProperty> signaturePropertyContent = new LinkedList<SignatureProperty>();
        signaturePropertyContent.add(signatureInfoSignatureProperty);
        final SignatureProperties signatureProperties = signatureFactory.newSignatureProperties(signaturePropertyContent, null);
        objectContent.add(signatureProperties);

        final String objectId = "idOfficeObject"; //$NON-NLS-1$
        objects.add(signatureFactory.newXMLObject(objectContent, objectId, null, null));

        final DigestMethod digestMethod = signatureFactory.newDigestMethod(DigestMethod.SHA1, null);
        final Reference reference = signatureFactory.newReference("#" + objectId, digestMethod, null, "http://www.w3.org/2000/09/xmldsig#Object", null); //$NON-NLS-1$ //$NON-NLS-2$
        references.add(reference);
    }

    private void addRelationshipsReferences(final XMLSignatureFactory signatureFactory, final List<Reference> manifestReferences) throws IOException,
                                                                                                                                        ParserConfigurationException,
                                                                                                                                        SAXException,
                                                                                                                                        NoSuchAlgorithmException,
                                                                                                                                        InvalidAlgorithmParameterException {
        final ZipInputStream zipInputStream = new ZipInputStream(new ByteArrayInputStream(this.signatureService.getOfficeOpenXMLDocument()));
        ZipEntry zipEntry;
        while (null != (zipEntry = zipInputStream.getNextEntry())) {
            if (!zipEntry.getName().endsWith(".rels")) { //$NON-NLS-1$
                continue;
            }
            final Document relsDocument = loadDocumentNoClose(zipInputStream);
            addRelationshipsReference(signatureFactory, zipEntry.getName(), relsDocument, manifestReferences);
        }
    }

    private static void addRelationshipsReference(final XMLSignatureFactory signatureFactory,
                                           final String zipEntryName,
                                           final Document relsDocument,
                                           final List<Reference> manifestReferences) throws NoSuchAlgorithmException, InvalidAlgorithmParameterException {

        final RelationshipTransformParameterSpec parameterSpec = new RelationshipTransformParameterSpec();
        final NodeList nodeList = relsDocument.getDocumentElement().getChildNodes();
        for (int nodeIdx = 0; nodeIdx < nodeList.getLength(); nodeIdx++) {
            final Node node = nodeList.item(nodeIdx);
            if (node.getNodeType() != Node.ELEMENT_NODE) {
                continue;
            }
            final Element element = (Element) node;
            final String relationshipType = element.getAttribute("Type"); //$NON-NLS-1$
            /*
             * We skip some relationship types.
             */
            if ("http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties".equals(relationshipType)) { //$NON-NLS-1$
                continue;
            }
            if ("http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties".equals(relationshipType)) { //$NON-NLS-1$
                continue;
            }
            if ("http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/origin".equals(relationshipType)) { //$NON-NLS-1$
                continue;
            }
            if ("http://schemas.openxmlformats.org/package/2006/relationships/metadata/thumbnail".equals(relationshipType)) { //$NON-NLS-1$
                continue;
            }
            if ("http://schemas.openxmlformats.org/officeDocument/2006/relationships/presProps".equals(relationshipType)) { //$NON-NLS-1$
                continue;
            }
            if ("http://schemas.openxmlformats.org/officeDocument/2006/relationships/viewProps".equals(relationshipType)) { //$NON-NLS-1$
                continue;
            }
            final String relationshipId = element.getAttribute("Id"); //$NON-NLS-1$
            parameterSpec.addRelationshipReference(relationshipId);
        }

        final List<Transform> transforms = new LinkedList<Transform>();
        transforms.add(signatureFactory.newTransform(RelationshipTransformService.TRANSFORM_URI, parameterSpec));
        transforms.add(signatureFactory.newTransform("http://www.w3.org/TR/2001/REC-xml-c14n-20010315", (TransformParameterSpec) null)); //$NON-NLS-1$
        final DigestMethod digestMethod = signatureFactory.newDigestMethod(DigestMethod.SHA1, null);
        final Reference reference =
                signatureFactory.newReference("/" + zipEntryName + "?ContentType=application/vnd.openxmlformats-package.relationships+xml", //$NON-NLS-1$ //$NON-NLS-2$
                                              digestMethod,
                                              transforms,
                                              null,
                                              null);

        manifestReferences.add(reference);
    }

    private void addParts(final XMLSignatureFactory signatureFactory, final String contentType, final List<Reference> references) throws NoSuchAlgorithmException,
                                                                                                               InvalidAlgorithmParameterException, IOException, ParserConfigurationException, SAXException, TransformerException {
        final List<String> documentResourceNames = getResourceNames(new ByteArrayInputStream(this.signatureService.getOfficeOpenXMLDocument()), contentType);
        final DigestMethod digestMethod = signatureFactory.newDigestMethod(DigestMethod.SHA1, null);
        for (final String documentResourceName : documentResourceNames) {

            final Reference reference = signatureFactory.newReference("/" + documentResourceName + "?ContentType=" + contentType, digestMethod); //$NON-NLS-1$ //$NON-NLS-2$

            references.add(reference);
        }
    }

    private static List<String> getResourceNames(final InputStream ooxmldoc, final String contentType) throws IOException,
                                                                                   ParserConfigurationException,
                                                                                   SAXException,
                                                                                   TransformerException {
        final List<String> signatureResourceNames = new LinkedList<String>();
        if (null == ooxmldoc) {
            throw new IllegalArgumentException("OOXML document is null"); //$NON-NLS-1$
        }
        final ZipInputStream zipInputStream = new ZipInputStream(ooxmldoc);
        ZipEntry zipEntry;
        while (null != (zipEntry = zipInputStream.getNextEntry())) {
            if (!"[Content_Types].xml".equals(zipEntry.getName())) { //$NON-NLS-1$
                continue;
            }
            final Document contentTypesDocument = loadDocument(zipInputStream);
            final Element nsElement = contentTypesDocument.createElement("ns"); //$NON-NLS-1$
            nsElement.setAttributeNS(Constants.NamespaceSpecNS, "xmlns:tns", "http://schemas.openxmlformats.org/package/2006/content-types"); //$NON-NLS-1$ //$NON-NLS-2$
            final NodeList nodeList =
                    XPathAPI.selectNodeList(contentTypesDocument, "/tns:Types/tns:Override[@ContentType='" + contentType + "']/@PartName", nsElement); //$NON-NLS-1$ //$NON-NLS-2$
            for (int nodeIdx = 0; nodeIdx < nodeList.getLength(); nodeIdx++) {
                String partName = nodeList.item(nodeIdx).getTextContent();
                partName = partName.substring(1); // remove '/'
                signatureResourceNames.add(partName);
            }
            break;
        }
        return signatureResourceNames;
    }

    protected Document loadDocument(final String zipEntryName) throws IOException, ParserConfigurationException, SAXException {
        final Document document = findDocument(zipEntryName);
        if (null != document) {
            return document;
        }
        throw new IOException("ZIP entry not found: " + zipEntryName); //$NON-NLS-1$
    }

    private Document findDocument(final String zipEntryName) throws IOException, ParserConfigurationException, SAXException {
        final ZipInputStream zipInputStream = new ZipInputStream(new ByteArrayInputStream(this.signatureService.getOfficeOpenXMLDocument()));
        ZipEntry zipEntry;
        while (null != (zipEntry = zipInputStream.getNextEntry())) {
            if (!zipEntryName.equals(zipEntry.getName())) {
                continue;
            }
            return loadDocument(zipInputStream);
        }
        return null;
    }

    private static Document loadDocumentNoClose(final InputStream documentInputStream) throws ParserConfigurationException, SAXException, IOException {
        final NoCloseInputStream noCloseInputStream = new NoCloseInputStream(documentInputStream);
        final InputSource inputSource = new InputSource(noCloseInputStream);
        final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);
        final DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
        return documentBuilder.parse(inputSource);
    }

    private static Document loadDocument(final InputStream documentInputStream) throws ParserConfigurationException, SAXException, IOException {
        final InputSource inputSource = new InputSource(documentInputStream);
        final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);
        final DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
        return documentBuilder.parse(inputSource);
    }

    public void postSign(final Element signatureElement, final List<X509Certificate> signingCertificateChain) {
        // empty
    }
}
