package es.gob.afirma.signfolder.proxy;

import java.util.Vector;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Analizador de XML para la generaci&oacute;n de objetos de tipo
 * {@link es.gob.afirma.signfolder.proxy.TriphaseRequestBean} a partir
 * de un XML de petici&oacute;n de prefirma.
 *
 * @author Carlos Gamuci
 */
public class PresignsRequestParser {

	private static final String PRESIGN_REQUEST_NODE = "rqtpre"; //$NON-NLS-1$
	private static final String CERTIFICATE_NODE = "cert"; //$NON-NLS-1$
	private static final String REQUESTS_LIST_NODE = "reqs"; //$NON-NLS-1$

	/**
	 * Analiza un documento XML y, en caso de tener el formato correcto, obtiene de &eacute;l
	 * un objeto de tipo {@link es.gob.afirma.signfolder.proxy.TriphaseRequestBean}.
	 * @param doc Documento XML.
	 * @return Objeto con los datos del XML.
	 * @throws IllegalArgumentException Cuando el XML no tiene el formato esperado.
	 */
	static TriphaseRequestBean parse(final Document doc) {

		if (doc == null) {
			throw new IllegalArgumentException("El documento proporcionado no puede ser nulo");  //$NON-NLS-1$
		}

		if (!PRESIGN_REQUEST_NODE.equalsIgnoreCase(doc.getDocumentElement().getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + //$NON-NLS-1$
					PRESIGN_REQUEST_NODE + "' y aparece: " + //$NON-NLS-1$
					doc.getDocumentElement().getNodeName());
		}

		final NodeList requestNodes = doc.getDocumentElement().getChildNodes();

		// Nos aseguramos de procesar solo nodos de tipo Element
		int nodeIndex = XMLUtils.nextNodeElementIndex(requestNodes, 0);
		if (nodeIndex == -1 || !CERTIFICATE_NODE.equalsIgnoreCase(requestNodes.item(nodeIndex).getNodeName())) {
			throw new IllegalArgumentException("La peticion de prefirmas no contiene el nodo " + //$NON-NLS-1$
					CERTIFICATE_NODE + " con el certificado de firma a utilizar"); //$NON-NLS-1$
		}
		final String certB64Encoded = XMLUtils.getTextContent(requestNodes.item(nodeIndex));

		nodeIndex = XMLUtils.nextNodeElementIndex(requestNodes, ++nodeIndex);
		if (nodeIndex == -1 || !REQUESTS_LIST_NODE.equalsIgnoreCase(requestNodes.item(nodeIndex).getNodeName())) {
			throw new IllegalArgumentException("La peticion de prefirmas no contiene el nodo " + //$NON-NLS-1$
					REQUESTS_LIST_NODE + " con el listado de peticiones de firma de documentos"); //$NON-NLS-1$
		}
		final Vector listPresignRequests = new Vector();
		final NodeList requestsNode = requestNodes.item(nodeIndex).getChildNodes();
		for (int i = 0; i < requestsNode.getLength(); i++) {
			// Nos aseguramos de procesar solo nodos de tipo Element
			i = XMLUtils.nextNodeElementIndex(requestsNode, i);
			if (i == -1) {
				break;
			}
			listPresignRequests.addElement(PresignRequestParser.parse(requestsNode.item(i)));
		}

		final TriphaseRequest[] tmpReqs = new TriphaseRequest[listPresignRequests.size()];
		listPresignRequests.copyInto(tmpReqs);

		return new TriphaseRequestBean(certB64Encoded, tmpReqs);
	}

	private static final class PresignRequestParser {

		private static final String REQUEST_NODE = "req"; //$NON-NLS-1$
		private static final String REFERENCE_ATTRIBUTE = "ref"; //$NON-NLS-1$

		static TriphaseRequest parse(final Node presignRequestNode) {

			if (!REQUEST_NODE.equalsIgnoreCase(presignRequestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						presignRequestNode.getNodeName() + "' en el listado de peticiones"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String ref;
			final Vector listDocumentRequests = new Vector();

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = presignRequestNode.getAttributes();
			attributeNode = attributes.getNamedItem(REFERENCE_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						REFERENCE_ATTRIBUTE + "' en un peticion de prefirma"); //$NON-NLS-1$
			}
			ref = attributeNode.getNodeValue();

			// Cargamos el listado de peticiones
			final NodeList requestsNode = presignRequestNode.getChildNodes();
			for (int i = 0; i < requestsNode.getLength(); i++) {
				// Nos aseguramos de procesar solo nodos de tipo Element
				i = XMLUtils.nextNodeElementIndex(requestsNode, i);
				if (i == -1) {
					break;
				}
				listDocumentRequests.addElement(PresignDocumentRequestParser.parse(requestsNode.item(i)));
			}

			final TriphaseSignDocumentRequest[] tmpReqs = new TriphaseSignDocumentRequest[listDocumentRequests.size()];
			listDocumentRequests.copyInto(tmpReqs);

			return new TriphaseRequest(ref, tmpReqs);
		}
	}

	private static final class PresignDocumentRequestParser {

		private static final String DOCUMENT_REQUEST_NODE = "doc"; //$NON-NLS-1$
		private static final String IDENTIFIER_ATTRIBUTE = "docid"; //$NON-NLS-1$
		private static final String SIGNATURE_FORMAT_ATTRIBUTE = "sigfrmt"; //$NON-NLS-1$

		static TriphaseSignDocumentRequest parse(final Node presignDocumentRequestNode) {

			if (!DOCUMENT_REQUEST_NODE.equalsIgnoreCase(presignDocumentRequestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						presignDocumentRequestNode.getNodeName() +
						"' en el listado de documentos para prefirma"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String docId;
			final String signatureFormat;

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = presignDocumentRequestNode.getAttributes();
			attributeNode = attributes.getNamedItem(IDENTIFIER_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						IDENTIFIER_ATTRIBUTE + "' en una peticion de prefirma de documento"); //$NON-NLS-1$
			}
			docId = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(SIGNATURE_FORMAT_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						SIGNATURE_FORMAT_ATTRIBUTE + "' en una peticion de prefirma de documento"); //$NON-NLS-1$
			}
			signatureFormat = attributeNode.getNodeValue();

			return new TriphaseSignDocumentRequest(docId, signatureFormat);
		}
	}
}
