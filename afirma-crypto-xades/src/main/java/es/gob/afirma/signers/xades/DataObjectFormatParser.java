package es.gob.afirma.signers.xades;

import java.util.ArrayList;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.uji.crypto.xades.jxades.security.xml.XAdES.DataObjectFormat;
import es.uji.crypto.xades.jxades.security.xml.XAdES.DataObjectFormatImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.ObjectIdentifier;
import es.uji.crypto.xades.jxades.security.xml.XAdES.ObjectIdentifierImpl;

public class DataObjectFormatParser {

	private static final String OBJECT_REFERENCE_TAG = "ObjectReference"; //$NON-NLS-1$
	private static final String DESCRIPTION_TAG = "Description"; //$NON-NLS-1$
	private static final String MIMETYPE_TAG = "MimeType"; //$NON-NLS-1$
	private static final String ENCODING_TAG = "Encoding"; //$NON-NLS-1$
	private static final String OBJECT_IDENTIFIER_TAG = "ObjectIdentifier"; //$NON-NLS-1$

	private static final String IDENTIFIER_TAG = "Identifier"; //$NON-NLS-1$
	private static final String QUALIFIER_ATTRIBUTE = "Qualifier"; //$NON-NLS-1$
	private static final String DOCUMENTATION_REFERENCES_TAG = "DocumentationReferences"; //$NON-NLS-1$
	private static final String DOCUMENTATION_REFERENCE_TAG = "DocumentationReference"; //$NON-NLS-1$

	private DataObjectFormatParser() {
		// No se permite la instanciacion
	}

	static DataObjectFormat parseDataObjectFormat(final Element dataObjectFormatElement) {

		final String objectReference = dataObjectFormatElement.getAttribute(OBJECT_REFERENCE_TAG);
		final DataObjectFormatImpl dataObjectFormat = new DataObjectFormatImpl(null, null, null, null, objectReference);

		final NodeList infoNodes = dataObjectFormatElement.getChildNodes();
		for (int i = 0; i < infoNodes.getLength(); i++) {
			final Node infoNode = infoNodes.item(i);
			if (infoNode.getNodeType() == Node.ELEMENT_NODE) {

				switch (infoNode.getLocalName()) {
				case DESCRIPTION_TAG:
					final String description = infoNode.getTextContent();
					if (description != null && !description.isEmpty()) {
						dataObjectFormat.setDescription(description.trim());
					}
					break;
				case MIMETYPE_TAG:
					final String mimeType = infoNode.getTextContent();
					if (mimeType != null && !mimeType.isEmpty()) {
						dataObjectFormat.setMimeType(mimeType);
					}
					break;
				case ENCODING_TAG:
					final String encoding = infoNode.getTextContent();
					if (encoding != null && !encoding.isEmpty()) {
						dataObjectFormat.setEncoding(encoding);
					}
					break;
				case OBJECT_IDENTIFIER_TAG:
					final ObjectIdentifier objectIdentifier = parseObjectIdentifier(infoNode);
					dataObjectFormat.setObjectIdentifier(objectIdentifier);
					break;
				default:
					break;
				}
			}
		}

		return dataObjectFormat;
	}

	private static ObjectIdentifier parseObjectIdentifier(final Node infoNode) {


		String qualifier = null;
		String identifier = null;
		String description = null;
		final ArrayList<String> docReferences = new ArrayList<>(0);

		final NodeList oidInfoNodes = infoNode.getChildNodes();
		for (int i = 0; i < oidInfoNodes.getLength(); i++) {
			final Node oidInfoNode = oidInfoNodes.item(i);
			if (oidInfoNode.getNodeType() == Node.ELEMENT_NODE) {
				switch (oidInfoNode.getLocalName()) {
				case IDENTIFIER_TAG:
					qualifier = ((Element) oidInfoNode).getAttribute(QUALIFIER_ATTRIBUTE);
					identifier = oidInfoNode.getTextContent();
					break;
				case DESCRIPTION_TAG:
					description = oidInfoNode.getTextContent();
					break;
				case DOCUMENTATION_REFERENCES_TAG:
					final NodeList referenceNodes = oidInfoNode.getChildNodes();
					for (int j = 0; j < referenceNodes.getLength(); j++) {
						final Node referenceNode = referenceNodes.item(j);
						if (referenceNode.getNodeType() == Node.ELEMENT_NODE
								&& DOCUMENTATION_REFERENCE_TAG.equals(referenceNode.getLocalName())) {
							final String reference = referenceNode.getTextContent();
							if (reference != null) {
								docReferences.add(reference);
							}
						}
					}
					break;
				default:
					break;
				}
			}
		}

		return new ObjectIdentifierImpl(qualifier, identifier, description, docReferences);
	}
}
