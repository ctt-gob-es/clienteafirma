package es.gob.afirma.signfolder.proxy;

import java.util.Vector;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Analizador de XML para la generaci&oacute;n de objetos de tipo
 * {@link es.gob.afirma.signfolder.proxy.RequestListResponseBean}.
 * @author Carlos Gamuci
 */
final class RequestListResponseParser {

	private static final String LIST_NODE = "list"; //$NON-NLS-1$

	/**
	 * Analiza un documento XML y, en caso de tener el formato correcto, obtiene de &eacute;l un objeto de
	 * tipo {@link es.gob.afirma.signfolder.proxy.RequestListResponseBean}.
	 * @param doc Documento XML.
	 * @return Objeto con los datos del XML.
	 * @throws IllegalArgumentException Cuando el XML no tiene el formato esperado.
	 */
	static RequestListResponseBean parse(final Document doc) {

		if (doc == null) {
			throw new IllegalArgumentException("El documento proporcionado no puede ser nulo");  //$NON-NLS-1$
		}

		if (!LIST_NODE.equalsIgnoreCase(doc.getDocumentElement().getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + LIST_NODE + //$NON-NLS-1$
					"' y aparece: " + doc.getDocumentElement().getNodeName()); //$NON-NLS-1$
		}

		final Vector listSignRequest = new Vector();
		final NodeList requestNodes = doc.getDocumentElement().getChildNodes();
		for (int i = 0; i < requestNodes.getLength(); i++) {
			// Nos aseguramos de procesar solo nodos de tipo Element
			i = XMLUtils.nextNodeElementIndex(requestNodes, i);
			if (i == -1) {
				break;
			}
			listSignRequest.addElement(SignRequestParser.parse(requestNodes.item(i)));
		}

		final SignRequest[] tmpReq = new SignRequest[listSignRequest.size()];
		listSignRequest.copyInto(tmpReq);

		return new RequestListResponseBean(tmpReq);
	}

	private static class SignRequestParser {

		private static final String REQUEST_NODE = "rqt"; //$NON-NLS-1$
		private static final String REFERENCE_NODE = "ref"; //$NON-NLS-1$
		private static final String PRIORITY_NODE = "priority"; //$NON-NLS-1$
		private static final String WORKFLOW_NODE = "workflow"; //$NON-NLS-1$
		private static final String FORWARD_NODE = "forward"; //$NON-NLS-1$
		private static final String SUBJECT_NODE = "subj"; //$NON-NLS-1$
		private static final String SENDER_NODE = "snder"; //$NON-NLS-1$
		private static final String VIEW_NODE = "view"; //$NON-NLS-1$
		private static final String DATE_NODE = "date"; //$NON-NLS-1$
		private static final String DOCUMENTS_NODE = "docs"; //$NON-NLS-1$

		static SignRequest parse(final Node signRequestNode) {

			if (!REQUEST_NODE.equalsIgnoreCase(signRequestNode.getNodeName())) {
				throw new IllegalArgumentException("Se ha encontrado el elemento '" + signRequestNode.getNodeName() + //$NON-NLS-1$
						"' en el listado de peticiones"); //$NON-NLS-1$
			}

			/* Atributos */
			String ref = null;
			int priority = 1;	// Valor por defecto
			boolean workflow = false; // Valor por defecto
			boolean forward = false; // Valor por defecto

			/* Elementos */
			final String subject;
			final String sender;
			final String view;
			final String date;
			final Vector signRequestDocumentsList = new Vector();

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = signRequestNode.getAttributes();
			attributeNode = attributes.getNamedItem(REFERENCE_NODE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						REFERENCE_NODE + "' en un peticion de firma"); //$NON-NLS-1$
			}
			ref = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(PRIORITY_NODE);
			if (attributeNode != null) {
				try {
					priority = Integer.parseInt(attributeNode.getNodeValue());
				} catch (final Exception e) {
					throw new IllegalArgumentException("La prioridad de la peticion con referencia '" + //$NON-NLS-1$
							ref + "' no es valida. Debe ser un valor entero"); //$NON-NLS-1$
				}
			}

			attributeNode = attributes.getNamedItem(WORKFLOW_NODE);
			if (attributeNode != null) {
				try {
					workflow = XMLUtils.parseBoolean(attributeNode.getNodeValue());
				} catch (final Exception e) {
					throw new IllegalArgumentException("El valor del atributo " + WORKFLOW_NODE + //$NON-NLS-1$
							"de la peticion con referencia '" + ref + "' no es valido. Debe ser 'true' o 'false'"); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}

			attributeNode = attributes.getNamedItem(FORWARD_NODE);
			if (attributeNode != null) {
				try {
					forward = XMLUtils.parseBoolean(attributeNode.getNodeValue());
				}
				catch (final Exception e) {
					throw new IllegalArgumentException("El valor del atributo " + FORWARD_NODE + //$NON-NLS-1$
							"de la peticion con referencia '" + ref + "' no es valido. Debe ser 'true' o 'false'"); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}

			// Cargamos los elementos
			int elementIndex = 0;
			final NodeList childNodes = signRequestNode.getChildNodes();
			elementIndex = XMLUtils.nextNodeElementIndex(childNodes, elementIndex);
			if (elementIndex == -1 || !SUBJECT_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("La peticion con referencia '" + ref + //$NON-NLS-1$
						"' no contiene el elemento " + SUBJECT_NODE); //$NON-NLS-1$
			}
			subject = XMLUtils.getTextContent(childNodes.item(elementIndex));

			elementIndex = XMLUtils.nextNodeElementIndex(childNodes, ++elementIndex);
			if (elementIndex == -1 || !SENDER_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("La peticion con referencia '" + ref + //$NON-NLS-1$
						"' no contiene el elemento " + SENDER_NODE); //$NON-NLS-1$
			}
			sender = XMLUtils.getTextContent(childNodes.item(elementIndex));

			elementIndex = XMLUtils.nextNodeElementIndex(childNodes, ++elementIndex);
			if (elementIndex == -1 || !VIEW_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("La peticion con referencia '" + ref + //$NON-NLS-1$
						"' no contiene el elemento " + VIEW_NODE); //$NON-NLS-1$
			}
			view = XMLUtils.getTextContent(childNodes.item(elementIndex));

			elementIndex = XMLUtils.nextNodeElementIndex(childNodes, ++elementIndex);
			if (elementIndex == -1 || !DATE_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("La peticion con referencia '" + ref + //$NON-NLS-1$
						"' no contiene el elemento " + DATE_NODE); //$NON-NLS-1$
			}
			date = XMLUtils.getTextContent(childNodes.item(elementIndex));

			elementIndex = XMLUtils.nextNodeElementIndex(childNodes, ++elementIndex);
			if (elementIndex == -1 || !DOCUMENTS_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("La peticion con referencia '" + ref + //$NON-NLS-1$
						"' no contiene el elemento " + DOCUMENTS_NODE); //$NON-NLS-1$
			}
			final NodeList docsList = childNodes.item(elementIndex).getChildNodes();
			for (int i = 0; i < docsList.getLength(); i++) {
				// Nos aseguramos de procesar solo nodos de tipo Element
				i = XMLUtils.nextNodeElementIndex(docsList, i);
				if (i == -1) {
					break;
				}
				signRequestDocumentsList.addElement(SignRequestDocumentParser.parse(docsList.item(i)));
			}

			final SignRequestDocument[] tmpRet = new SignRequestDocument[signRequestDocumentsList.size()];
			signRequestDocumentsList.copyInto(tmpRet);
			return new SignRequest(ref, subject, sender, view, date, priority, workflow, forward, tmpRet);
		}
	}

	private static final class SignRequestDocumentParser {

		private static final String DOC_NODE = "doc"; //$NON-NLS-1$
		private static final String ID_NODE = "docid"; //$NON-NLS-1$
		private static final String NAME_NODE = "nm"; //$NON-NLS-1$
		private static final String MIMETYPE_NODE = "mmtp"; //$NON-NLS-1$
		private static final String SIGNATURE_FORMAT_NODE = "sgnfmt"; //$NON-NLS-1$

		static SignRequestDocument parse(final Node signRequestDocumentNode) {

			if (!DOC_NODE.equalsIgnoreCase(signRequestDocumentNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						signRequestDocumentNode.getNodeName() + "' en el listado de documentos"); //$NON-NLS-1$
			}

			// Elementos del documento
			final String docId;
			final String name;
			final String mimeType;
			final String signatureFormat;

			// Cargamos los elementos
			int elementIndex = 0;
			final NodeList childNodes = signRequestDocumentNode.getChildNodes();
			elementIndex = XMLUtils.nextNodeElementIndex(childNodes, elementIndex);
			if (elementIndex == -1 || !ID_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("Existe un documento sin el elemento '" + ID_NODE + //$NON-NLS-1$
						"'"); //$NON-NLS-1$
			}
			docId = XMLUtils.getTextContent(childNodes.item(elementIndex));

			elementIndex = XMLUtils.nextNodeElementIndex(childNodes, ++elementIndex);
			if (elementIndex == -1 || !NAME_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("Existe un documento sin el elemento '" + NAME_NODE + //$NON-NLS-1$
						"'"); //$NON-NLS-1$
			}
			name = XMLUtils.getTextContent(childNodes.item(elementIndex));

			elementIndex = XMLUtils.nextNodeElementIndex(childNodes, ++elementIndex);
			if (elementIndex == -1 || !MIMETYPE_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("Existe un documento sin el elemento '" + MIMETYPE_NODE + //$NON-NLS-1$
						"'"); //$NON-NLS-1$
			}
			mimeType = XMLUtils.getTextContent(childNodes.item(elementIndex));

			elementIndex = XMLUtils.nextNodeElementIndex(childNodes, ++elementIndex);
			if (elementIndex == -1 || !SIGNATURE_FORMAT_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("Existe un documento sin el elemento '" + SIGNATURE_FORMAT_NODE + //$NON-NLS-1$
						"'"); //$NON-NLS-1$
			}
			signatureFormat = XMLUtils.getTextContent(childNodes.item(elementIndex));

			return new SignRequestDocument(docId, name, mimeType, signatureFormat);
		}
	}
}
