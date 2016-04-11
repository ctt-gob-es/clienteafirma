package es.gob.afirma.android.signfolder.proxy;

import java.util.Vector;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.android.signfolder.proxy.SignRequest.RequestType;

/** Analizador de XML para la generaci&oacute;n de listas de peticiones de firma.
 * @author Carlos Gamuci */
final class RequestListResponseParser {

	private static final String LIST_NODE = "list"; //$NON-NLS-1$

	private static final String ERROR_NODE = "err"; //$NON-NLS-1$

	private static final String NUM_REQUESTS_ATTRIBUTE = "n"; //$NON-NLS-1$

	private static final String CD_ATTRIBUTE = "cd"; //$NON-NLS-1$

	/** Valor usado por el Portafirmas para indicar que una petici&oacute;n es de firma. */
	private static final String REQUEST_TYPE_SIGN = "FIRMA"; //$NON-NLS-1$

	/** Valor usado por el Portafirmas para indicar que una petici&oacute;n es de visto bueno. */
	private static final String REQUEST_TYPE_APPROVE = "VISTOBUENO"; //$NON-NLS-1$

	/** Analiza un documento XML y, en caso de tener el formato correcto, obtiene una lista de peticiones de firma.
	 * @param doc Documento XML.
	 * @return Objeto con los datos del XML.
	 * @throws IllegalArgumentException Cuando el XML no tiene el formato esperado.
	 */
	static PartialSignRequestsList parse(final Document doc) {

		if (doc == null) {
			throw new IllegalArgumentException("El documento proporcionado no puede ser nulo");  //$NON-NLS-1$
		}

		final Element docElement = doc.getDocumentElement();

		if (ERROR_NODE.equalsIgnoreCase(docElement.getNodeName())) {
			final String errorType = docElement.getAttribute(CD_ATTRIBUTE);
			throw new ServerException("El servicio proxy notifico un error (" + errorType + "): " + XmlUtils.getTextContent(docElement)); //$NON-NLS-1$ //$NON-NLS-2$
		}

		if (!LIST_NODE.equalsIgnoreCase(docElement.getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + LIST_NODE + //$NON-NLS-1$
					"' y aparece: " + doc.getDocumentElement().getNodeName()); //$NON-NLS-1$
		}

		final String numRequestAttrValue = docElement.getAttribute(NUM_REQUESTS_ATTRIBUTE);
		int numRequests;
		try {
			numRequests = numRequestAttrValue == null ? 0 : Integer.parseInt(numRequestAttrValue);
		} catch (final Exception e) {
			numRequests = 0;
		}

		final Vector<SignRequest> listSignRequest = new Vector<SignRequest>();
		final NodeList requestNodes = docElement.getChildNodes();
		for (int i = 0; i < requestNodes.getLength(); i++) {
			// Nos aseguramos de procesar solo nodos de tipo Element
			i = XmlUtils.nextNodeElementIndex(requestNodes, i);
			if (i == -1) {
				break;
			}
			listSignRequest.addElement(SignRequestParser.parse(requestNodes.item(i)));
		}

		return new PartialSignRequestsList(listSignRequest, numRequests);
	}

	private static class SignRequestParser {

		private static final String REQUEST_NODE = "rqt"; //$NON-NLS-1$
		private static final String ID_ATTRIBUTE = "id"; //$NON-NLS-1$
		private static final String PRIORITY_ATTRIBUTE = "priority"; //$NON-NLS-1$
		private static final String WORKFLOW_ATTRIBUTE = "workflow"; //$NON-NLS-1$
		private static final String FORWARD_ATTRIBUTE = "forward"; //$NON-NLS-1$
		private static final String TYPE_ATTRIBUTE = "type"; //$NON-NLS-1$
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
			RequestType type = RequestType.SIGNATURE; // Valor por defecto

			/* Elementos */
			final String subject;
			final String sender;
			final String view;
			final String date;
			final Vector<SignRequestDocument> signRequestDocumentsList = new Vector<SignRequestDocument>();

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = signRequestNode.getAttributes();
			attributeNode = attributes.getNamedItem(ID_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						ID_ATTRIBUTE + "' en un peticion de firma"); //$NON-NLS-1$
			}
			ref = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(PRIORITY_ATTRIBUTE);
			if (attributeNode != null) {
				try {
					priority = Integer.parseInt(attributeNode.getNodeValue());
				} catch (final Exception e) {
					throw new IllegalArgumentException("La prioridad de la peticion con referencia '" + //$NON-NLS-1$
							ref + "' no es valida. Debe ser un valor entero"); //$NON-NLS-1$
				}
			}

			attributeNode = attributes.getNamedItem(WORKFLOW_ATTRIBUTE);
			if (attributeNode != null) {
				try {
					workflow = XmlUtils.parseBoolean(attributeNode.getNodeValue());
				} catch (final Exception e) {
					throw new IllegalArgumentException("El valor del atributo " + WORKFLOW_ATTRIBUTE + //$NON-NLS-1$
							"de la peticion con referencia '" + ref + "' no es valido. Debe ser 'true' o 'false'"); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}

			attributeNode = attributes.getNamedItem(FORWARD_ATTRIBUTE);
			if (attributeNode != null) {
				try {
					forward = XmlUtils.parseBoolean(attributeNode.getNodeValue());
				}
				catch (final Exception e) {
					throw new IllegalArgumentException("El valor del atributo " + FORWARD_ATTRIBUTE + //$NON-NLS-1$
							"de la peticion con referencia '" + ref + "' no es valido. Debe ser 'true' o 'false'"); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}

			attributeNode = attributes.getNamedItem(TYPE_ATTRIBUTE);
			if (attributeNode != null && attributeNode.getNodeValue() != null) {
				if (REQUEST_TYPE_SIGN.equalsIgnoreCase(attributeNode.getNodeValue())) {
					type = RequestType.SIGNATURE;
				} else if (REQUEST_TYPE_APPROVE.equalsIgnoreCase(attributeNode.getNodeValue())) {
					type = RequestType.APPROVE;
				} else {
					type = null;
				}
			}

			// Cargamos los elementos
			int elementIndex = 0;
			final NodeList childNodes = signRequestNode.getChildNodes();
			elementIndex = XmlUtils.nextNodeElementIndex(childNodes, elementIndex);
			if (elementIndex == -1 || !SUBJECT_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("La peticion con referencia '" + ref + //$NON-NLS-1$
						"' no contiene el elemento " + SUBJECT_NODE); //$NON-NLS-1$
			}
			subject = normalizeValue(XmlUtils.getTextContent(childNodes.item(elementIndex)));

			elementIndex = XmlUtils.nextNodeElementIndex(childNodes, ++elementIndex);
			if (elementIndex == -1 || !SENDER_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("La peticion con referencia '" + ref + //$NON-NLS-1$
						"' no contiene el elemento " + SENDER_NODE); //$NON-NLS-1$
			}
			sender = normalizeValue(XmlUtils.getTextContent(childNodes.item(elementIndex)));

			elementIndex = XmlUtils.nextNodeElementIndex(childNodes, ++elementIndex);
			if (elementIndex == -1 || !VIEW_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("La peticion con referencia '" + ref + //$NON-NLS-1$
						"' no contiene el elemento " + VIEW_NODE); //$NON-NLS-1$
			}
			view = XmlUtils.getTextContent(childNodes.item(elementIndex));

			elementIndex = XmlUtils.nextNodeElementIndex(childNodes, ++elementIndex);
			if (elementIndex == -1 || !DATE_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("La peticion con referencia '" + ref + //$NON-NLS-1$
						"' no contiene el elemento " + DATE_NODE); //$NON-NLS-1$
			}
			date = XmlUtils.getTextContent(childNodes.item(elementIndex));

			elementIndex = XmlUtils.nextNodeElementIndex(childNodes, ++elementIndex);
			if (elementIndex == -1 || !DOCUMENTS_NODE.equalsIgnoreCase(childNodes.item(elementIndex).getNodeName())) {
				throw new IllegalArgumentException("La peticion con referencia '" + ref + //$NON-NLS-1$
						"' no contiene el elemento " + DOCUMENTS_NODE); //$NON-NLS-1$
			}
			final NodeList docsList = childNodes.item(elementIndex).getChildNodes();
			for (int i = 0; i < docsList.getLength(); i++) {
				// Nos aseguramos de procesar solo nodos de tipo Element
				i = XmlUtils.nextNodeElementIndex(docsList, i);
				if (i == -1) {
					break;
				}
				signRequestDocumentsList.addElement(SignRequestDocumentParser.parse(docsList.item(i)));
			}

			final SignRequestDocument[] tmpRet = new SignRequestDocument[signRequestDocumentsList.size()];
			signRequestDocumentsList.copyInto(tmpRet);
			return new SignRequest(ref, subject, sender, view, date, priority, workflow, forward, type, tmpRet);
		}
	}

	/**
	 * Deshace los cambios que hizo el proxy para asegurar que el XML est&aacute;ba bien formado.
	 * @param value Valor que normalizar.
	 * @return Valor normalizado.
	 */
	static String normalizeValue(final String value) {
		return value.trim().replace("&_lt;", "<").replace("&_gt;", ">"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	}
}
