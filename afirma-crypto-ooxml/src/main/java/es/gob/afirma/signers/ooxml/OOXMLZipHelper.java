package es.gob.afirma.signers.ooxml;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

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
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import es.gob.afirma.core.misc.AOUtil;

@SuppressWarnings("resource")
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
                                                                                          TransformerException {
        final ByteArrayOutputStream signedOOXMLOutputStream = new ByteArrayOutputStream();

        final String signatureZipEntryName = "_xmlsignatures/sig-" + UUID.randomUUID().toString() + ".xml"; //$NON-NLS-1$ //$NON-NLS-2$

        // Copiamos el contenido del OOXML original al OOXML firmado
        // Durante el proceso es necesario modificar ciertos ficheros
        final ZipOutputStream zipOutputStream = copyOOXMLContent(
    		ooXmlDocument,
    		signatureZipEntryName,
    		signedOOXMLOutputStream
		);

        // Anadimos el fichero de firma XML al paquete OOXML
        zipOutputStream.putNextEntry(new ZipEntry(signatureZipEntryName));
        if (xmlSignatureFile != null) {
        	zipOutputStream.write(xmlSignatureFile);
        }
        zipOutputStream.close();

        return signedOOXMLOutputStream.toByteArray();
    }

	private static ZipOutputStream copyOOXMLContent(final byte[] ooXmlDocument,
    		                                        final String signatureZipEntryName,
    		                                        final OutputStream signedOOXMLOutputStream) throws IOException,
                                                                                                       ParserConfigurationException,
                                                                                                       SAXException,
                                                                                                       TransformerException {
        final ZipOutputStream zipOutputStream = new ZipOutputStream(signedOOXMLOutputStream);
        final ZipInputStream zipInputStream = new ZipInputStream(
    		new ByteArrayInputStream(
				ooXmlDocument
			)
		);
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
                nsElement.setAttributeNS(NAMESPACE_SPEC_NS, "xmlns:tns", "http://schemas.openxmlformats.org/package/2006/content-types"); //$NON-NLS-1$ //$NON-NLS-2$
                final NodeList nodeList = com.sun.org.apache.xpath.internal.XPathAPI.selectNodeList(
            		contentTypesDocument,
            		"/tns:Types/tns:Default[@Extension='sigs']", //$NON-NLS-1$
            		nsElement
        		);
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
                nsElement.setAttributeNS(NAMESPACE_SPEC_NS, "xmlns:tns", RELATIONSHIPS_SCHEMA); //$NON-NLS-1$
				final NodeList nodeList = com.sun.org.apache.xpath.internal.XPathAPI.selectNodeList(
            		relsDocument,
                    "/tns:Relationships/tns:Relationship[@Type='http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/origin']", //$NON-NLS-1$
                    nsElement
                );
                if (0 == nodeList.getLength()) {
                    final Element relationshipElement =
                            relsDocument.createElementNS(RELATIONSHIPS_SCHEMA, "Relationship"); //$NON-NLS-1$
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
                        originSignRelsDocument.createElementNS(RELATIONSHIPS_SCHEMA, "Relationship"); //$NON-NLS-1$
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

        // Return.
        zipInputStream.close();
        return zipOutputStream;
    }

    private static void addOriginSigs(final ZipOutputStream zipOutputStream) throws IOException {
        zipOutputStream.putNextEntry(new ZipEntry("_xmlsignatures/origin.sigs")); //$NON-NLS-1$
    }

    private static void addOriginSigsRels(final String signatureZipEntryName, final ZipOutputStream zipOutputStream) throws ParserConfigurationException,
                                                                                                             IOException,
                                                                                                             TransformerException {
        final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);

        final Document originSignRelsDocument = documentBuilderFactory.newDocumentBuilder().newDocument();

        final Element relationshipsElement =
                originSignRelsDocument.createElementNS(RELATIONSHIPS_SCHEMA, "Relationships"); //$NON-NLS-1$
        relationshipsElement.setAttributeNS(NAMESPACE_SPEC_NS, "xmlns", RELATIONSHIPS_SCHEMA); //$NON-NLS-1$
        originSignRelsDocument.appendChild(relationshipsElement);

        final Element relationshipElement =
                originSignRelsDocument.createElementNS(RELATIONSHIPS_SCHEMA, "Relationship"); //$NON-NLS-1$
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
    	final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
    	dbf.setNamespaceAware(true);
    	return dbf.newDocumentBuilder().parse(new InputSource(new NoCloseInputStream(documentInputStream)));
    }

    static void writeDocumentNoClosing(final Document document,
                                                 final OutputStream documentOutputStream,
                                                 final boolean omitXmlDeclaration) throws TransformerException {
    	final NoCloseOutputStream outputStream = new NoCloseOutputStream(documentOutputStream);
    	final Result result = new StreamResult(outputStream);
    	final Transformer xformer = TransformerFactory.newInstance().newTransformer();
    	if (omitXmlDeclaration) {
    		xformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes"); //$NON-NLS-1$
    	}
    	final Source source = new DOMSource(document);
    	xformer.transform(source, result);
    }

    private static final class NoCloseOutputStream extends FilterOutputStream {

        NoCloseOutputStream(final OutputStream proxy) {
            super(proxy);
        }

        @Override
        public void close() throws IOException {
            // Nunca cerramos
        }
    }


}
