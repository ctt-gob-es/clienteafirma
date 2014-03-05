package es.gob.afirma.signers.ooxml;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
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

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.xml.internal.messaging.saaj.util.ByteInputStream;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.ooxml.relprovider.RelationshipTransformParameterSpec;
import es.gob.afirma.signers.ooxml.relprovider.RelationshipTransformService;

final class OOXMLPackageObjectHelper {

	private static final String NAMESPACE_SPEC_NS = "http://www.w3.org/2000/xmlns/"; //$NON-NLS-1$
    private static final String DIGITAL_SIGNATURE_SCHEMA = "http://schemas.openxmlformats.org/package/2006/digital-signature"; //$NON-NLS-1$

    private static enum MS_APPS {
    	WORD("word"), EXCEL("excel"), POWERPOINT("powerpoint");
    	
    	private String name;
    	
    	private MS_APPS(String name) {
			this.name = name;
		}
    	
    	@Override
    	public String toString() {
    		return this.name;
    	}
    }
    
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
	                                                                   SAXException {
		final List<XMLStructure> objectContent = new LinkedList<XMLStructure>();
		objectContent.add(constructManifest(fac, ooXmlDocument));

		addSignatureTime(fac, document, signatureId, objectContent);

		return fac.newXMLObject(objectContent, nodeId, null, null);
	}

    private static void addParts(final XMLSignatureFactory fac,
    		                     ContentTypeManager contentTypeManager, 
    		                     final List<Reference> references,
    		                     final byte[] ooXmlDocument, 
    		                     MS_APPS application, 
    		                     DigestMethod digestMethod) throws IOException {   	
    	final ZipInputStream zipInputStream = new ZipInputStream(
    			new ByteArrayInputStream(
    					ooXmlDocument)
    			);
    	
    	ZipEntry zipEntry;
    	while (null != (zipEntry = zipInputStream.getNextEntry())) {
    		if (!zipEntry.getName().startsWith(application.toString())) {
    			continue;
    		}
    		
    		String contentType = contentTypeManager.getContentType(zipEntry.getName());
    		
    		// Solo se añade la referencia si existe contentType
    		if (contentType != null) {
    			final Reference reference = fac.newReference("/" + zipEntry.getName() + "?ContentType=" + contentType, digestMethod); //$NON-NLS-1$ //$NON-NLS-2$
    			references.add(reference);
    		}
    	}
	}

	private static InputStream getContentTypesXMLInputStream(byte[] ooXmlDocument) throws IOException {
	   	final ZipInputStream zipInputStream = new ZipInputStream(new ByteArrayInputStream(ooXmlDocument));
    	ZipEntry zipEntry;
    	
    	while (null != (zipEntry = zipInputStream.getNextEntry())) {
    		if ("[Content_Types].xml".equals(zipEntry.getName())) { //$NON-NLS-1$
    			return zipInputStream;
    		}
    	}
    	
    	throw new IllegalArgumentException("El documento OOXML es inválido ya que no contiene el fichero [Content_Types].xml"); //$NON-NLS-1$
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
                                                  final List<Reference> manifestReferences, 
                                                  String contentType, 
                                                  DigestMethod digestMethod) throws NoSuchAlgorithmException,
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
    	final Reference reference = fac.newReference(
			"/" + zipEntryName + "?ContentType=" + contentType, //$NON-NLS-1$ //$NON-NLS-2$
			digestMethod,
			transforms,
			null,
			null
		);

    	manifestReferences.add(reference);
    }

    private static void addRelationshipsReferences(final XMLSignatureFactory fac,
    		                                       final List<Reference> manifestReferences,
    		                                       final byte[] ooXmlDocument, 
    		                                       DigestMethod digestMethod) throws IOException,
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
    		String contentType = "application/vnd.openxmlformats-package.relationships+xml";
    		addRelationshipsReference(fac, zipEntry.getName(), relsDocument, manifestReferences, contentType, digestMethod);
    	}
    }

    private static Manifest constructManifest(final XMLSignatureFactory fac,
    		                                  final byte[] ooXmlDocument) throws NoSuchAlgorithmException,
                                                                                 InvalidAlgorithmParameterException,
                                                                                 IOException,
                                                                                 ParserConfigurationException,
                                                                                 SAXException {
    	final DigestMethod digestMethod = fac.newDigestMethod(DigestMethod.SHA512, null);
    	
    	final List<Reference> manifestReferences = new LinkedList<Reference>();
    	addRelationshipsReferences(fac, manifestReferences, ooXmlDocument, digestMethod);

    	// Se obtiene el inputstream del fichero [Content_Types].xml para inicializar el ContentTypeManager
    	InputStream contentXml = getContentTypesXMLInputStream(ooXmlDocument);
    	ContentTypeManager contentTypeManager = new ContentTypeManager(contentXml);
    	
		// Word
		addParts(fac, contentTypeManager, manifestReferences, ooXmlDocument, MS_APPS.WORD, digestMethod); //$NON-NLS-1$

		// Powerpoint
		addParts(fac, contentTypeManager, manifestReferences, ooXmlDocument, MS_APPS.POWERPOINT, digestMethod); //$NON-NLS-1$
		
		// Excel
		addParts(fac, contentTypeManager, manifestReferences, ooXmlDocument, MS_APPS.EXCEL, digestMethod); //$NON-NLS-1$

		contentXml.close();
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
