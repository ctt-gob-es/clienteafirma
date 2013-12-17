package es.gob.afirma.signers.ooxml;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
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

import es.gob.afirma.signers.ooxml.relprovider.RelationshipTransformParameterSpec;
import es.gob.afirma.signers.ooxml.relprovider.RelationshipTransformService;

final class OOXMLPackageObjectHelper {

	private static final String NAMESPACE_SPEC_NS = "http://www.w3.org/2000/xmlns/"; //$NON-NLS-1$
    private static final String DIGITAL_SIGNATURE_SCHEMA = "http://schemas.openxmlformats.org/package/2006/digital-signature"; //$NON-NLS-1$

    private OOXMLPackageObjectHelper() {
    	// No permitimos la instanciacion
    }

	static XMLObject getPackageObject(final String nodeId,
									  final XMLSignatureFactory fac,
									  final byte[] ooXmlDocument,
			                          final Document document,
			                          final String signatureId) throws NoSuchAlgorithmException,
	                                                                   InvalidAlgorithmParameterException,
	                                                                   IOException,
	                                                                   ParserConfigurationException,
	                                                                   SAXException,
	                                                                   TransformerException {
		final List<XMLStructure> objectContent = new LinkedList<XMLStructure>();
		objectContent.add(constructManifest(fac, ooXmlDocument));

		addSignatureTime(fac, document, signatureId, objectContent);

		return fac.newXMLObject(objectContent, nodeId, null, null);
	}

    private static void addParts(final XMLSignatureFactory fac,
    							 final String contentType,
    		                     final List<Reference> references,
    		                     final byte[] ooXmlDocument) throws NoSuchAlgorithmException,
    		              										    InvalidAlgorithmParameterException,
    		              										    IOException,
    		              										    ParserConfigurationException,
    		              										    SAXException,
    		              										    TransformerException {
    	final List<String> documentResourceNames = getResourceNames(
			new ByteArrayInputStream(
				ooXmlDocument
			),
			contentType
		);
    	final DigestMethod digestMethod = fac.newDigestMethod(DigestMethod.SHA1, null);
    	for (final String documentResourceName : documentResourceNames) {
    		final Reference reference = fac.newReference("/" + documentResourceName + "?ContentType=" + contentType, digestMethod); //$NON-NLS-1$ //$NON-NLS-2$
    		references.add(reference);
    	}
	}

	private static List<String> getResourceNames(final InputStream ooxmldoc,
    		                                     final String contentType) throws IOException,
    		                                     								  ParserConfigurationException,
    		                                     								  SAXException,
    		                                     								  TransformerException {
    	final List<String> signatureResourceNames = new LinkedList<String>();
    	if (null == ooxmldoc) {
    		throw new IllegalArgumentException("El documento OOXML no puede ser nulo"); //$NON-NLS-1$
    	}
    	final ZipInputStream zipInputStream = new ZipInputStream(ooxmldoc);
    	ZipEntry zipEntry;
    	while (null != (zipEntry = zipInputStream.getNextEntry())) {
    		if (!"[Content_Types].xml".equals(zipEntry.getName())) { //$NON-NLS-1$
    			continue;
    		}
    		final Document contentTypesDocument = loadDocument(zipInputStream);
    		final Element nsElement = contentTypesDocument.createElement("ns"); //$NON-NLS-1$
    		nsElement.setAttributeNS(NAMESPACE_SPEC_NS, "xmlns:tns", "http://schemas.openxmlformats.org/package/2006/content-types"); //$NON-NLS-1$ //$NON-NLS-2$
    		@SuppressWarnings("restriction")
			final NodeList nodeList = com.sun.org.apache.xpath.internal.XPathAPI.selectNodeList(
				contentTypesDocument,
				"/tns:Types/tns:Override[@ContentType='" + contentType + "']/@PartName", //$NON-NLS-1$ //$NON-NLS-2$
				nsElement
			);
    		for (int nodeIdx = 0; nodeIdx < nodeList.getLength(); nodeIdx++) {
    			String partName = nodeList.item(nodeIdx).getTextContent();
    			partName = partName.substring(1); // remove '/'
    			signatureResourceNames.add(partName);
    		}
    		break;
    	}
    	return signatureResourceNames;
    }

    private static Document loadDocument(final InputStream documentInputStream) throws ParserConfigurationException, SAXException, IOException {
        final InputSource inputSource = new InputSource(documentInputStream);
        final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);
        final DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
        return documentBuilder.parse(inputSource);
    }

	@SuppressWarnings("resource")
	private static Document loadDocumentNoClose(final InputStream documentInputStream) throws ParserConfigurationException, SAXException, IOException {
        final NoCloseInputStream noCloseInputStream = new NoCloseInputStream(documentInputStream);
        final InputSource inputSource = new InputSource(noCloseInputStream);
        final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(true);
        final DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
        return documentBuilder.parse(inputSource);
    }

    private static void addRelationshipsReference(final XMLSignatureFactory fac,
    		                                      final String zipEntryName,
                                                  final Document relsDocument,
                                                  final List<Reference> manifestReferences) throws NoSuchAlgorithmException,
                                                                                                   InvalidAlgorithmParameterException {
    	final RelationshipTransformParameterSpec parameterSpec = new RelationshipTransformParameterSpec();
    	final NodeList nodeList = relsDocument.getDocumentElement().getChildNodes();
    	for (int nodeIdx = 0; nodeIdx < nodeList.getLength(); nodeIdx++) {
    		final Node node = nodeList.item(nodeIdx);
    		if (node.getNodeType() != Node.ELEMENT_NODE) {
    			continue;
    		}
    		final Element element = (Element) node;
    		final String relationshipType = element.getAttribute("Type"); //$NON-NLS-1$
    		// Obviamos ciertos tipos de relacion
    		if ("http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties".equals(relationshipType) || //$NON-NLS-1$
    		    "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties".equals(relationshipType)   || //$NON-NLS-1$
    		    "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/origin".equals(relationshipType)   || //$NON-NLS-1$
    		    "http://schemas.openxmlformats.org/package/2006/relationships/metadata/thumbnail".equals(relationshipType)         || //$NON-NLS-1$
    		    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/presProps".equals(relationshipType)           || //$NON-NLS-1$
    		    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/viewProps".equals(relationshipType)) {           //$NON-NLS-1$
    			continue;
    		}
    		final String relationshipId = element.getAttribute("Id"); //$NON-NLS-1$
    		parameterSpec.addRelationshipReference(relationshipId);
    	}

    	final List<Transform> transforms = new LinkedList<Transform>();
    	transforms.add(fac.newTransform(RelationshipTransformService.TRANSFORM_URI, parameterSpec));
    	transforms.add(fac.newTransform("http://www.w3.org/TR/2001/REC-xml-c14n-20010315", (TransformParameterSpec) null)); //$NON-NLS-1$
    	final DigestMethod digestMethod = fac.newDigestMethod(DigestMethod.SHA1, null);
    	final Reference reference = fac.newReference(
			"/" + zipEntryName + "?ContentType=application/vnd.openxmlformats-package.relationships+xml", //$NON-NLS-1$ //$NON-NLS-2$
			digestMethod,
			transforms,
			null,
			null
		);

    	manifestReferences.add(reference);
    }

    private static void addRelationshipsReferences(final XMLSignatureFactory fac,
    		                                       final List<Reference> manifestReferences,
    		                                       final byte[] ooXmlDocument) throws IOException,
                                                                                      ParserConfigurationException,
                                                                                      SAXException,                                                             NoSuchAlgorithmException,
                                                                                      InvalidAlgorithmParameterException {
    	final ZipInputStream zipInputStream = new ZipInputStream(
			new ByteArrayInputStream(
				ooXmlDocument
			)
		);
    	ZipEntry zipEntry;
    	while (null != (zipEntry = zipInputStream.getNextEntry())) {
    		if (!zipEntry.getName().endsWith(".rels")) { //$NON-NLS-1$
    			continue;
    		}
    		final Document relsDocument = loadDocumentNoClose(zipInputStream);
    		addRelationshipsReference(fac, zipEntry.getName(), relsDocument, manifestReferences);
    	}
    }

    private static Manifest constructManifest(final XMLSignatureFactory fac,
    		                                  final byte[] ooXmlDocument) throws NoSuchAlgorithmException,
                                                                                 InvalidAlgorithmParameterException,
                                                                                 IOException,
                                                                                 ParserConfigurationException,
                                                                                 SAXException,
                                                                                 TransformerException {
    	final List<Reference> manifestReferences = new LinkedList<Reference>();
    	addRelationshipsReferences(fac, manifestReferences, ooXmlDocument);

		// Word
		addParts(fac, "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml", manifestReferences, ooXmlDocument); //$NON-NLS-1$
		addParts(fac, "application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml", manifestReferences, ooXmlDocument); //$NON-NLS-1$
		addParts(fac, "application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml", manifestReferences, ooXmlDocument); //$NON-NLS-1$
		addParts(fac, "application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml", manifestReferences, ooXmlDocument); //$NON-NLS-1$
		addParts(fac, "application/vnd.openxmlformats-officedocument.theme+xml", manifestReferences, ooXmlDocument); //$NON-NLS-1$
		addParts(fac, "application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml", manifestReferences, ooXmlDocument); //$NON-NLS-1$

		// Powerpoint
		addParts(fac, "application/vnd.openxmlformats-officedocument.presentationml.presentation.main+xml", manifestReferences, ooXmlDocument); //$NON-NLS-1$
		addParts(fac, "application/vnd.openxmlformats-officedocument.presentationml.slideLayout+xml", manifestReferences, ooXmlDocument); //$NON-NLS-1$
		addParts(fac, "application/vnd.openxmlformats-officedocument.presentationml.slideMaster+xml", manifestReferences, ooXmlDocument); //$NON-NLS-1$
		addParts(fac, "application/vnd.openxmlformats-officedocument.presentationml.slide+xml", manifestReferences, ooXmlDocument); //$NON-NLS-1$
		addParts(fac, "application/vnd.openxmlformats-officedocument.presentationml.tableStyles+xml", manifestReferences, ooXmlDocument); //$NON-NLS-1$

		return fac.newManifest(manifestReferences);
	}

    private static void addSignatureTime(final XMLSignatureFactory fac,
    		                             final Document document,
            						     final String signatureId,
            						     final List<XMLStructure> objectContent) {
    	// SignatureTime
    	final Element signatureTimeElement = document.createElementNS(
			DIGITAL_SIGNATURE_SCHEMA,
			"mdssi:SignatureTime" //$NON-NLS-1$
		);
    	signatureTimeElement.setAttributeNS(
			NAMESPACE_SPEC_NS,
			"xmlns:mdssi", //$NON-NLS-1$
			DIGITAL_SIGNATURE_SCHEMA
		);
    	final Element formatElement = document.createElementNS(DIGITAL_SIGNATURE_SCHEMA, "mdssi:Format"); //$NON-NLS-1$
    	formatElement.setTextContent("YYYY-MM-DDThh:mm:ssTZD"); //$NON-NLS-1$
    	signatureTimeElement.appendChild(formatElement);
    	final Element valueElement = document.createElementNS(DIGITAL_SIGNATURE_SCHEMA, "mdssi:Value"); //$NON-NLS-1$
    	valueElement.setTextContent(new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss'Z'").format(new Date())); //$NON-NLS-1$
    	signatureTimeElement.appendChild(valueElement);

    	final List<XMLStructure> signatureTimeContent = new LinkedList<XMLStructure>();
    	signatureTimeContent.add(new DOMStructure(signatureTimeElement));
    	final SignatureProperty signatureTimeSignatureProperty = fac.newSignatureProperty(
			signatureTimeContent,
			"#" + signatureId, //$NON-NLS-1$
			"idSignatureTime" //$NON-NLS-1$
		);
    	final List<SignatureProperty> signaturePropertyContent = new LinkedList<SignatureProperty>();
    	signaturePropertyContent.add(signatureTimeSignatureProperty);
    	final SignatureProperties signatureProperties = fac.newSignatureProperties(
			signaturePropertyContent,
			"id-signature-time-" + UUID.randomUUID().toString() //$NON-NLS-1$
		);
    	objectContent.add(signatureProperties);
    }

}
