package es.gob.afirma.signfolder.proxy;

import java.util.Vector;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Analizador de XML para la generaci&oacute;n de objetos de tipo
 * {@link es.gob.afirma.signfolder.proxy.TriphaseResponseBean} a partir
 * de un XML de respuesta de prefirma.
 *
 * @author Carlos Gamuci
 */
public class PresignsResponseParser {

	private static final String PRESIGN_RESPONSE_NODE = "pres"; //$NON-NLS-1$

	/**
	 * Analiza un documento XML y, en caso de tener el formato correcto, obtiene de &eacute;l
	 * un objeto de tipo {@link es.gob.afirma.signfolder.proxy.TriphaseResponseBean}.
	 * @param doc Documento XML.
	 * @return Objeto con los datos del XML.
	 * @throws IllegalArgumentException Cuando el XML no tiene el formato esperado.
	 */
	static TriphaseResponseBean parse(final Document doc) {

		if (doc == null) {
			throw new IllegalArgumentException("El documento proporcionado no puede ser nulo");  //$NON-NLS-1$
		}

		if (!PRESIGN_RESPONSE_NODE.equalsIgnoreCase(doc.getDocumentElement().getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + //$NON-NLS-1$
					PRESIGN_RESPONSE_NODE + "' y aparece: " + //$NON-NLS-1$
					doc.getDocumentElement().getNodeName());
		}

		final Vector listPresignRequests = new Vector();
		final NodeList requestNodes = doc.getDocumentElement().getChildNodes();
		for (int i = 0; i < requestNodes.getLength(); i++) {
			// Nos aseguramos de procesar solo nodos de tipo Element
			i = XMLUtils.nextNodeElementIndex(requestNodes, i);
			if (i == -1) {
				break;
			}
			listPresignRequests.addElement(TriphaseRequestParser.parse(requestNodes.item(i)));
		}

		final TriphaseRequest[] tmpReqs = new TriphaseRequest[listPresignRequests.size()];
		listPresignRequests.copyInto(tmpReqs);

		return new TriphaseResponseBean(tmpReqs);
	}

	private static final class TriphaseRequestParser {

		private static final String REQUEST_NODE = "req"; //$NON-NLS-1$
		private static final String REFERENCE_ATTRIBUTE = "ref"; //$NON-NLS-1$
		private static final String STATUS_ATTRIBUTE = "status"; //$NON-NLS-1$

		static TriphaseRequest parse(final Node presignRequestNode) {

			if (!REQUEST_NODE.equalsIgnoreCase(presignRequestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						presignRequestNode.getNodeName() + "' en el listado de peticiones"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String ref;
			final boolean statusOk;
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

			attributeNode = attributes.getNamedItem(STATUS_ATTRIBUTE);
			// statusOk = true, salvo que la propiedad status tenga el valor "KO"
			statusOk = (attributeNode == null || !"KO".equalsIgnoreCase(attributeNode.getNodeValue())); //$NON-NLS-1$

			// Si la peticion no se ha procesado correctamente se descarta
			if (!statusOk) {
				return new TriphaseRequest(ref, false, null);
			}

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

			return new TriphaseRequest(ref, statusOk, tmpReqs);
		}
	}

	private static final class PresignDocumentRequestParser {

		private static final String DOCUMENT_REQUEST_NODE = "doc"; //$NON-NLS-1$
		private static final String IDENTIFIER_ATTRIBUTE = "docid"; //$NON-NLS-1$
		private static final String SIGNATURE_FORMAT_ATTRIBUTE = "sigfrmt"; //$NON-NLS-1$
		private static final String PARAMS_PRESIGN_NODE = "params"; //$NON-NLS-1$
		private static final String METADATA_PRESIGN_NODE = "meta"; //$NON-NLS-1$
		private static final String PRESIGN_RESULT_NODE = "presign"; //$NON-NLS-1$

		static TriphaseSignDocumentRequest parse(final Node presignDocumentRequestNode) {

			if (!DOCUMENT_REQUEST_NODE.equalsIgnoreCase(presignDocumentRequestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						presignDocumentRequestNode.getNodeName() +
						"' en el listado de documentos para prefirma"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String docId;
			final String signatureFormat;
			String params = null;
			String meta = null;
			final String presign;

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

			// Cargamos la respuesta de la prefirma
			final NodeList presignConfigNodes = presignDocumentRequestNode.getChildNodes();
			int numIndex = XMLUtils.nextNodeElementIndex(presignConfigNodes, 0);

			// Comprobamos el nodo PARAMS_PRESIGN_NODE
			if (numIndex != -1 && PARAMS_PRESIGN_NODE.equalsIgnoreCase(presignConfigNodes.item(numIndex).getNodeName())) {
				params = XMLUtils.getTextContent(presignConfigNodes.item(numIndex));
				numIndex = XMLUtils.nextNodeElementIndex(presignConfigNodes, numIndex);
			}

			// Comprobamos el nodo METADATA_PRESIGN_NODE
			if (numIndex != -1 && METADATA_PRESIGN_NODE.equalsIgnoreCase(presignConfigNodes.item(numIndex).getNodeName())) {
				meta = XMLUtils.getTextContent(presignConfigNodes.item(numIndex));
				numIndex = XMLUtils.nextNodeElementIndex(presignConfigNodes, numIndex);
			}

			// Comprobamos el nodo PRESIGN_RESULT_NODE
			if (numIndex == -1 || PRESIGN_RESULT_NODE.equalsIgnoreCase(presignConfigNodes.item(numIndex).getNodeName())) {
				throw new IllegalArgumentException("No se ha encontrado el nodo " + //$NON-NLS-1$
						PRESIGN_RESULT_NODE +
						" en la respuesta de la peticion de prefirma del documento"); //$NON-NLS-1$
			}

			presign = XMLUtils.getTextContent(presignConfigNodes.item(numIndex));

			return new TriphaseSignDocumentRequest(docId, signatureFormat, params, meta, presign);
		}
	}
}
