/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.ooxml;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.xml.XMLConstants;
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
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.SecureXmlBuilder;
import es.gob.afirma.core.misc.SecureXmlTransformer;

final class OOXMLZipHelper {

	private OOXMLZipHelper() {
		// No permitimos la instanciacion
	}

	private static final String NAMESPACE_SPEC_NS = "http://www.w3.org/2000/xmlns/"; //$NON-NLS-1$

    private static final String RELATIONSHIPS_SCHEMA = "http://schemas.openxmlformats.org/package/2006/relationships"; //$NON-NLS-1$

    static byte[] outputSignedOfficeOpenXMLDocument(final byte[] ooXmlDocument,
    		                                        final byte[] xmlSignatureFile) throws IOException,
                                                                                          ParserConfigurationException,
                                                                                          SAXException,
                                                                                          TransformerException,
                                                                                          XPathExpressionException {
        final ByteArrayOutputStream signedOOXMLOutputStream = new ByteArrayOutputStream();

        final String signatureZipEntryName = "_xmlsignatures/sig-" + UUID.randomUUID().toString() + ".xml"; //$NON-NLS-1$ //$NON-NLS-2$

        // Copiamos el contenido del OOXML original al OOXML firmado
        // Durante el proceso es necesario modificar ciertos ficheros
        try (
	        final ZipOutputStream zipOutputStream = copyOOXMLContent(
	    		ooXmlDocument,
	    		signatureZipEntryName,
	    		signedOOXMLOutputStream
			);
		) {
	        // Anadimos el fichero de firma XML al paquete OOXML
	        zipOutputStream.putNextEntry(new ZipEntry(signatureZipEntryName));
	        if (xmlSignatureFile != null) {
	        	zipOutputStream.write(xmlSignatureFile);
	        }
        }

        return signedOOXMLOutputStream.toByteArray();
    }

	private static ZipOutputStream copyOOXMLContent(final byte[] ooXmlDocument,
    		                                        final String signatureZipEntryName,
    		                                        final OutputStream signedOOXMLOutputStream) throws IOException,
                                                                                                       ParserConfigurationException,
                                                                                                       SAXException,
                                                                                                       TransformerException,
                                                                                                       XPathExpressionException {
        final ZipOutputStream zipOutputStream = new ZipOutputStream(signedOOXMLOutputStream);
        try (
	        final ZipInputStream zipInputStream = new ZipInputStream(
	    		new ByteArrayInputStream(
					ooXmlDocument
				)
			);
		) {
	        ZipEntry zipEntry;
	        boolean hasOriginSigsRels = false;
	        while (null != (zipEntry = zipInputStream.getNextEntry())) {
	            zipOutputStream.putNextEntry(new ZipEntry(zipEntry.getName()));
	            if ("[Content_Types].xml".equals(zipEntry.getName())) { //$NON-NLS-1$
	                final Document contentTypesDocument = loadDocumentNoClose(zipInputStream);
	                final Element typesElement = contentTypesDocument.getDocumentElement();

	                // We need to add an Override element.
	                final Element overrideElement = contentTypesDocument.createElementNS(
	            		"http://schemas.openxmlformats.org/package/2006/content-types", //$NON-NLS-1$
	            		"Override" //$NON-NLS-1$
	        		);
	                overrideElement.setAttribute("PartName", "/" + signatureZipEntryName); //$NON-NLS-1$ //$NON-NLS-2$
	                overrideElement.setAttribute("ContentType", "application/vnd.openxmlformats-package.digital-signature-xmlsignature+xml"); //$NON-NLS-1$ //$NON-NLS-2$
	                typesElement.appendChild(overrideElement);

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
								if ("tns".equals(prefix)) { //$NON-NLS-1$
									return "http://schemas.openxmlformats.org/package/2006/content-types"; //$NON-NLS-1$
								}
								return XMLConstants.NULL_NS_URI;
							}
						}
					);

					final XPathExpression exp = xpath.compile(
						"/tns:Types/tns:Default[@Extension='sigs']" //$NON-NLS-1$
					);
					final NodeList nodeList = (NodeList) exp.evaluate(
						contentTypesDocument,
						XPathConstants.NODESET
					);

	                if (0 == nodeList.getLength()) {
	                    // Add Default element for 'sigs' extension.
	                    final Element defaultElement = contentTypesDocument.createElementNS(
	                		"http://schemas.openxmlformats.org/package/2006/content-types", //$NON-NLS-1$
	                		"Default" //$NON-NLS-1$
	            		);
	                    defaultElement.setAttribute("Extension", "sigs"); //$NON-NLS-1$ //$NON-NLS-2$
	                    defaultElement.setAttribute("ContentType", "application/vnd.openxmlformats-package.digital-signature-origin"); //$NON-NLS-1$ //$NON-NLS-2$
	                    typesElement.appendChild(defaultElement);
	                }

	                writeDocumentNoClosing(contentTypesDocument, zipOutputStream, false);
	            }
	            else if ("_rels/.rels".equals(zipEntry.getName())) { //$NON-NLS-1$
	                final Document relsDocument = loadDocumentNoClose(zipInputStream);

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
								if ("tns".equals(prefix)) { //$NON-NLS-1$
									return RELATIONSHIPS_SCHEMA;
								}
								return XMLConstants.NULL_NS_URI;
							}
						}
					);
					final XPathExpression exp = xpath.compile(
						"/tns:Relationships/tns:Relationship[@Type='http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/origin']" //$NON-NLS-1$
					);
					final NodeList nodeList = (NodeList) exp.evaluate(
						relsDocument,
						XPathConstants.NODESET
					);

	                if (0 == nodeList.getLength()) {
	                    final Element relationshipElement = relsDocument.createElementNS(RELATIONSHIPS_SCHEMA, "Relationship"); //$NON-NLS-1$
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

	                final Element relationshipElement = originSignRelsDocument.createElementNS(RELATIONSHIPS_SCHEMA, "Relationship"); //$NON-NLS-1$
	                relationshipElement.setAttribute("Id", "rel-" + UUID.randomUUID().toString()); //$NON-NLS-1$ //$NON-NLS-2$
	                relationshipElement.setAttribute("Type", "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/signature"); //$NON-NLS-1$ //$NON-NLS-2$
	                relationshipElement.setAttribute("Target", new File(signatureZipEntryName).getName()); //$NON-NLS-1$

	                originSignRelsDocument.getDocumentElement().appendChild(relationshipElement);

	                writeDocumentNoClosing(originSignRelsDocument, zipOutputStream, false);
	            }
	            else {
	            	zipOutputStream.write(AOUtil.getDataFromInputStream(zipInputStream));
	            }
	        }

	        if (!hasOriginSigsRels) {
	            // Add signature relationships document.
	            addOriginSigsRels(signatureZipEntryName, zipOutputStream);
	            addOriginSigs(zipOutputStream);
	        }
        }

        return zipOutputStream;
    }

    private static void addOriginSigs(final ZipOutputStream zipOutputStream) throws IOException {
        zipOutputStream.putNextEntry(new ZipEntry("_xmlsignatures/origin.sigs")); //$NON-NLS-1$
    }

    private static void addOriginSigsRels(final String signatureZipEntryName, final ZipOutputStream zipOutputStream) throws ParserConfigurationException,
                                                                                                             IOException,
                                                                                                             TransformerException {

    	final Document originSignRelsDocument = SecureXmlBuilder.getSecureDocumentBuilder().newDocument();

        final Element relationshipsElement = originSignRelsDocument.createElementNS(RELATIONSHIPS_SCHEMA, "Relationships"); //$NON-NLS-1$
        relationshipsElement.setAttributeNS(NAMESPACE_SPEC_NS, "xmlns", RELATIONSHIPS_SCHEMA); //$NON-NLS-1$
        originSignRelsDocument.appendChild(relationshipsElement);

        final Element relationshipElement = originSignRelsDocument.createElementNS(RELATIONSHIPS_SCHEMA, "Relationship"); //$NON-NLS-1$
        final String relationshipId = "rel-" + UUID.randomUUID().toString(); //$NON-NLS-1$
        relationshipElement.setAttribute("Id", relationshipId); //$NON-NLS-1$
        relationshipElement.setAttribute("Type", "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/signature"); //$NON-NLS-1$ //$NON-NLS-2$

        relationshipElement.setAttribute("Target", new File(signatureZipEntryName).getName()); //$NON-NLS-1$
        relationshipsElement.appendChild(relationshipElement);

        zipOutputStream.putNextEntry(new ZipEntry("_xmlsignatures/_rels/origin.sigs.rels")); //$NON-NLS-1$
        writeDocumentNoClosing(originSignRelsDocument, zipOutputStream, false);
    }

	static Document loadDocumentNoClose(final InputStream documentInputStream) throws ParserConfigurationException,
    																							SAXException,
    																							IOException {

    	try (
			final InputStream is = new NoCloseInputStream(documentInputStream);
		) {
    		return SecureXmlBuilder.getSecureDocumentBuilder().parse(new InputSource(is));
    	}
    }

    static void writeDocumentNoClosing(final Document document,
                                                 final OutputStream documentOutputStream,
                                                 final boolean omitXmlDeclaration) throws TransformerException {
    	try (
			final NoCloseOutputStream outputStream = new NoCloseOutputStream(documentOutputStream);
		) {
    		final Result result = new StreamResult(outputStream);
	        final Transformer xformer = SecureXmlTransformer.getSecureTransformer();
	    	if (omitXmlDeclaration) {
	    		xformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes"); //$NON-NLS-1$
	    	}
	    	final Source source = new DOMSource(document);
	    	xformer.transform(source, result);
    	}
    }

    private static final class NoCloseOutputStream extends FilterOutputStream {

        NoCloseOutputStream(final OutputStream proxy) {
            super(proxy);
        }

        @Override
        public void close() {
            // Nunca cerramos
        }
    }

}
