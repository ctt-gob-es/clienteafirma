/*
 * eID Applet Project.
 * Copyright (C) 2008-2009 FedICT.
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

package es.gob.afirma.be.fedict.eid.applet.service.signer.ooxml;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.xml.crypto.URIDereferencer;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.security.utils.Constants;
import com.sun.org.apache.xpath.internal.XPathAPI;

import es.gob.afirma.be.fedict.eid.applet.service.signer.AbstractXmlSignatureService;

/** Signature Service implementation for Office OpenXML document format XML
 * signatures.
 * @author Frank Cornelis */
public abstract class AbstractOOXMLSignatureService extends AbstractXmlSignatureService {

    protected AbstractOOXMLSignatureService() {
        addSignatureFacet(new OOXMLSignatureFacet(this));
    }

    @Override
    protected final String getSignatureDescription() {
        return "Office OpenXML Document"; //$NON-NLS-1$
    }

    public final String getFilesDigestAlgorithm() {
        return null;
    }

    @Override
    protected final URIDereferencer getURIDereferencer() {
        return new OOXMLURIDereferencer(getOfficeOpenXMLDocument());
    }

    @Override
    protected final String getCanonicalizationMethod() {
        return CanonicalizationMethod.INCLUSIVE;
    }

    /** Gives back the OOXML to be signed. */
    protected abstract byte[] getOfficeOpenXMLDocument();

    /** Obtiene el fichero OOXMLK firmado.
     * @param signatureData
     * @return Fichero OOXML firmado
     * @throws IOException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws TransformerException
     */
    public final byte[] outputSignedOfficeOpenXMLDocument(final byte[] signatureData) throws IOException,
                                                                                     ParserConfigurationException,
                                                                                     SAXException,
                                                                                     TransformerException {

        final ByteArrayOutputStream signedOOXMLOutputStream = new ByteArrayOutputStream();

        final String signatureZipEntryName = "_xmlsignatures/sig-" + UUID.randomUUID().toString() + ".xml"; //$NON-NLS-1$ //$NON-NLS-2$

        /*
         * Copy the original OOXML content to the signed OOXML package. During
         * copying some files need to changed.
         */
        final ZipOutputStream zipOutputStream = copyOOXMLContent(signatureZipEntryName, signedOOXMLOutputStream);

        // Add the OOXML XML signature file to the OOXML package.
        zipOutputStream.putNextEntry(new ZipEntry(signatureZipEntryName));
        IOUtils.write(signatureData, zipOutputStream);
        zipOutputStream.close();

        return signedOOXMLOutputStream.toByteArray();
    }

    private ZipOutputStream copyOOXMLContent(final String signatureZipEntryName, final OutputStream signedOOXMLOutputStream) throws IOException,
                                                                                                                            ParserConfigurationException,
                                                                                                                            SAXException,
                                                                                                                            TransformerException {
        final ZipOutputStream zipOutputStream = new ZipOutputStream(signedOOXMLOutputStream);
        final ZipInputStream zipInputStream = new ZipInputStream(new ByteArrayInputStream(this.getOfficeOpenXMLDocument()));
        ZipEntry zipEntry;
        boolean hasOriginSigsRels = false;
        while (null != (zipEntry = zipInputStream.getNextEntry())) {
            zipOutputStream.putNextEntry(new ZipEntry(zipEntry.getName()));
            if ("[Content_Types].xml".equals(zipEntry.getName())) { //$NON-NLS-1$
                final Document contentTypesDocument = loadDocumentNoClose(zipInputStream);
                final Element typesElement = contentTypesDocument.getDocumentElement();

                // We need to add an Override element.
                final Element overrideElement =
                        contentTypesDocument.createElementNS("http://schemas.openxmlformats.org/package/2006/content-types", "Override"); //$NON-NLS-1$ //$NON-NLS-2$
                overrideElement.setAttribute("PartName", "/" + signatureZipEntryName); //$NON-NLS-1$ //$NON-NLS-2$
                overrideElement.setAttribute("ContentType", "application/vnd.openxmlformats-package.digital-signature-xmlsignature+xml"); //$NON-NLS-1$ //$NON-NLS-2$
                typesElement.appendChild(overrideElement);

                final Element nsElement = contentTypesDocument.createElement("ns"); //$NON-NLS-1$
                nsElement.setAttributeNS(Constants.NamespaceSpecNS, "xmlns:tns", "http://schemas.openxmlformats.org/package/2006/content-types"); //$NON-NLS-1$ //$NON-NLS-2$
                final NodeList nodeList = XPathAPI.selectNodeList(contentTypesDocument, "/tns:Types/tns:Default[@Extension='sigs']", nsElement); //$NON-NLS-1$
                if (0 == nodeList.getLength()) {
                    // Add Default element for 'sigs' extension.
                    final Element defaultElement =
                            contentTypesDocument.createElementNS("http://schemas.openxmlformats.org/package/2006/content-types", "Default"); //$NON-NLS-1$ //$NON-NLS-2$
                    defaultElement.setAttribute("Extension", "sigs"); //$NON-NLS-1$ //$NON-NLS-2$
                    defaultElement.setAttribute("ContentType", "application/vnd.openxmlformats-package.digital-signature-origin"); //$NON-NLS-1$ //$NON-NLS-2$
                    typesElement.appendChild(defaultElement);
                }

                writeDocumentNoClosing(contentTypesDocument, zipOutputStream, false);
            }
            else if ("_rels/.rels".equals(zipEntry.getName())) { //$NON-NLS-1$
                final Document relsDocument = loadDocumentNoClose(zipInputStream);

                final Element nsElement = relsDocument.createElement("ns"); //$NON-NLS-1$
                nsElement.setAttributeNS(Constants.NamespaceSpecNS, "xmlns:tns", "http://schemas.openxmlformats.org/package/2006/relationships"); //$NON-NLS-1$ //$NON-NLS-2$
                final NodeList nodeList =
                        XPathAPI.selectNodeList(relsDocument,
                                                "/tns:Relationships/tns:Relationship[@Type='http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/origin']", //$NON-NLS-1$
                                                nsElement);
                if (0 == nodeList.getLength()) {
                    final Element relationshipElement =
                            relsDocument.createElementNS("http://schemas.openxmlformats.org/package/2006/relationships", "Relationship"); //$NON-NLS-1$ //$NON-NLS-2$
                    relationshipElement.setAttribute("Id", "rel-id-" + UUID.randomUUID().toString()); //$NON-NLS-1$ //$NON-NLS-2$
                    relationshipElement.setAttribute("Type", "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/origin"); //$NON-NLS-1$ //$NON-NLS-2$
                    relationshipElement.setAttribute("Target", "_xmlsignatures/origin.sigs"); //$NON-NLS-1$ //$NON-NLS-2$

                    relsDocument.getDocumentElement().appendChild(relationshipElement);
                }

                writeDocumentNoClosing(relsDocument, zipOutputStream, false);
            }
            else if (zipEntry.getName().startsWith("_xmlsignatures/_rels/") && zipEntry.getName().endsWith(".rels")) { //$NON-NLS-1$ //$NON-NLS-2$

                hasOriginSigsRels = true;
                final Document originSignRelsDocument = loadDocumentNoClose(zipInputStream);

                final Element relationshipElement =
                        originSignRelsDocument.createElementNS("http://schemas.openxmlformats.org/package/2006/relationships", "Relationship"); //$NON-NLS-1$ //$NON-NLS-2$
                relationshipElement.setAttribute("Id", "rel-" + UUID.randomUUID().toString()); //$NON-NLS-1$ //$NON-NLS-2$
                relationshipElement.setAttribute("Type", "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/signature"); //$NON-NLS-1$ //$NON-NLS-2$

                relationshipElement.setAttribute("Target", FilenameUtils.getName(signatureZipEntryName)); //$NON-NLS-1$
                originSignRelsDocument.getDocumentElement().appendChild(relationshipElement);

                writeDocumentNoClosing(originSignRelsDocument, zipOutputStream, false);
            }
            else {
                IOUtils.copy(zipInputStream, zipOutputStream);
            }
        }

        if (false == hasOriginSigsRels) {
            // Add signature relationships document.
            addOriginSigsRels(signatureZipEntryName, zipOutputStream);
            addOriginSigs(zipOutputStream);
        }

        // Return.
        zipInputStream.close();
        return zipOutputStream;
    }

    private void addOriginSigs(final ZipOutputStream zipOutputStream) throws IOException {
        zipOutputStream.putNextEntry(new ZipEntry("_xmlsignatures/origin.sigs")); //$NON-NLS-1$
    }

    private void addOriginSigsRels(final String signatureZipEntryName, final ZipOutputStream zipOutputStream) throws ParserConfigurationException,
                                                                                                             IOException,
                                                                                                             TransformerException {
        final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);

        final Document originSignRelsDocument = documentBuilderFactory.newDocumentBuilder().newDocument();

        final Element relationshipsElement =
                originSignRelsDocument.createElementNS("http://schemas.openxmlformats.org/package/2006/relationships", "Relationships"); //$NON-NLS-1$ //$NON-NLS-2$
        relationshipsElement.setAttributeNS(Constants.NamespaceSpecNS, "xmlns", "http://schemas.openxmlformats.org/package/2006/relationships"); //$NON-NLS-1$ //$NON-NLS-2$
        originSignRelsDocument.appendChild(relationshipsElement);

        final Element relationshipElement =
                originSignRelsDocument.createElementNS("http://schemas.openxmlformats.org/package/2006/relationships", "Relationship"); //$NON-NLS-1$ //$NON-NLS-2$
        final String relationshipId = "rel-" + UUID.randomUUID().toString(); //$NON-NLS-1$
        relationshipElement.setAttribute("Id", relationshipId); //$NON-NLS-1$
        relationshipElement.setAttribute("Type", "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/signature"); //$NON-NLS-1$ //$NON-NLS-2$

        relationshipElement.setAttribute("Target", FilenameUtils.getName(signatureZipEntryName)); //$NON-NLS-1$
        relationshipsElement.appendChild(relationshipElement);

        zipOutputStream.putNextEntry(new ZipEntry("_xmlsignatures/_rels/origin.sigs.rels")); //$NON-NLS-1$
        writeDocumentNoClosing(originSignRelsDocument, zipOutputStream, false);
    }

}
