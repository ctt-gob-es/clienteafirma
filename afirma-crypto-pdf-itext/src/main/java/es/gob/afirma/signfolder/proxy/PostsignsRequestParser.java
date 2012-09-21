package es.gob.afirma.signfolder.proxy;

import java.util.Vector;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Analizador de XML para la generaci&oacute;n de objetos de tipo
 * {@link es.gob.afirma.signfolder.proxy.TriphaseRequestBean} a partir
 * de un XML de petici&oacute;n de postfirma.
 *
 * @author Carlos Gamuci
 */
public class PostsignsRequestParser {

	private static final String POSTSIGN_REQUEST_NODE = "rqtpost"; //$NON-NLS-1$
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

		if (!POSTSIGN_REQUEST_NODE.equalsIgnoreCase(doc.getDocumentElement().getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + //$NON-NLS-1$
					POSTSIGN_REQUEST_NODE + "' y aparece: " + //$NON-NLS-1$
					doc.getDocumentElement().getNodeName());
		}

		final NodeList requestNodes = doc.getDocumentElement().getChildNodes();

		// Nos aseguramos de procesar solo nodos de tipo Element
		int nodeIndex = XMLUtils.nextNodeElementIndex(requestNodes, 0);
		if (nodeIndex == -1 || !CERTIFICATE_NODE.equalsIgnoreCase(requestNodes.item(nodeIndex).getNodeName())) {
			throw new IllegalArgumentException("La peticion de postfirmas no contiene el nodo " + //$NON-NLS-1$
					CERTIFICATE_NODE + " con el certificado de firma a utilizar"); //$NON-NLS-1$
		}
		final String certB64Encoded = XMLUtils.getTextContent(requestNodes.item(nodeIndex));

		nodeIndex = XMLUtils.nextNodeElementIndex(requestNodes, ++nodeIndex);
		if (nodeIndex == -1 || !REQUESTS_LIST_NODE.equalsIgnoreCase(requestNodes.item(nodeIndex).getNodeName())) {
			throw new IllegalArgumentException("La peticion de postfirmas no contiene el nodo " + //$NON-NLS-1$
					REQUESTS_LIST_NODE + " con el listado de peticiones de firma de documentos"); //$NON-NLS-1$
		}
		final Vector listPostsignRequests = new Vector();
		final NodeList requestsNode = requestNodes.item(nodeIndex).getChildNodes();
		for (int i = 0; i < requestsNode.getLength(); i++) {
			// Nos aseguramos de procesar solo nodos de tipo Element
			i = XMLUtils.nextNodeElementIndex(requestsNode, i);
			if (i == -1) {
				break;
			}
			listPostsignRequests.addElement(TriphaseRequestParser.parse(requestsNode.item(i)));
		}

		final TriphaseRequest[] tmpReqs = new TriphaseRequest[listPostsignRequests.size()];
		listPostsignRequests.copyInto(tmpReqs);

		return new TriphaseRequestBean(certB64Encoded, tmpReqs);
	}

	private static final class TriphaseRequestParser {

		private static final String REQUEST_NODE = "req"; //$NON-NLS-1$
		private static final String REFERENCE_ATTRIBUTE = "ref"; //$NON-NLS-1$
		private static final String STATUS_ATTRIBUTE = "status"; //$NON-NLS-1$

		static TriphaseRequest parse(final Node postsignRequestNode) {

			if (!REQUEST_NODE.equalsIgnoreCase(postsignRequestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						postsignRequestNode.getNodeName() + "' en el listado de peticiones"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String ref;
			final boolean statusOk;
			final Vector listDocumentRequests = new Vector();

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = postsignRequestNode.getAttributes();
			attributeNode = attributes.getNamedItem(REFERENCE_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						REFERENCE_ATTRIBUTE + "' en un peticion de postfirma"); //$NON-NLS-1$
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
			final NodeList requestsNode = postsignRequestNode.getChildNodes();
			for (int i = 0; i < requestsNode.getLength(); i++) {
				// Nos aseguramos de procesar solo nodos de tipo Element
				i = XMLUtils.nextNodeElementIndex(requestsNode, i);
				if (i == -1) {
					break;
				}
				listDocumentRequests.addElement(TriphaseSignDocumentRequestParser.parse(requestsNode.item(i)));
			}

			final TriphaseSignDocumentRequest[] tmpReqs = new TriphaseSignDocumentRequest[listDocumentRequests.size()];
			listDocumentRequests.copyInto(tmpReqs);

			return new TriphaseRequest(ref, statusOk, tmpReqs);
		}
	}

	private static final class TriphaseSignDocumentRequestParser {

		private static final String DOCUMENT_REQUEST_NODE = "doc"; //$NON-NLS-1$
		private static final String IDENTIFIER_ATTRIBUTE = "docid"; //$NON-NLS-1$
		private static final String SIGNATURE_FORMAT_ATTRIBUTE = "sigfrmt"; //$NON-NLS-1$
		private static final String PARAMS_TRIPHASE_NODE = "params"; //$NON-NLS-1$
		private static final String METADATA_TRIPHASE_NODE = "meta"; //$NON-NLS-1$
		private static final String RESULT_TRIPHASE_RESULT_NODE = "result"; //$NON-NLS-1$

		static TriphaseSignDocumentRequest parse(final Node triphaseSignDocumentRequestNode) {

			if (!DOCUMENT_REQUEST_NODE.equalsIgnoreCase(triphaseSignDocumentRequestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						triphaseSignDocumentRequestNode.getNodeName() +
						"' en el listado de documentos para postfirma"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String docId;
			final String signatureFormat;
			String params = null;
			String meta = null;
			final String result;

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = triphaseSignDocumentRequestNode.getAttributes();
			attributeNode = attributes.getNamedItem(IDENTIFIER_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						IDENTIFIER_ATTRIBUTE + "' en una peticion de postfirma de documento"); //$NON-NLS-1$
			}
			docId = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(SIGNATURE_FORMAT_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						SIGNATURE_FORMAT_ATTRIBUTE + "' en una peticion de postfirma de documento"); //$NON-NLS-1$
			}
			signatureFormat = attributeNode.getNodeValue();

			// Cargamos la configuracion y el resultado parcial de la firma trifasica
			final NodeList signConfigNodes = triphaseSignDocumentRequestNode.getChildNodes();
			int numIndex = XMLUtils.nextNodeElementIndex(signConfigNodes, 0);

			// Comprobamos el nodo PARAMS_TRIPHASE_NODE
			if (numIndex != -1 && PARAMS_TRIPHASE_NODE.equalsIgnoreCase(signConfigNodes.item(numIndex).getNodeName())) {
				params = XMLUtils.getTextContent(signConfigNodes.item(numIndex));
				numIndex = XMLUtils.nextNodeElementIndex(signConfigNodes, numIndex);
			}

			// Comprobamos el nodo METADATA_TRIPHASE_NODE
			if (numIndex != -1 && METADATA_TRIPHASE_NODE.equalsIgnoreCase(signConfigNodes.item(numIndex).getNodeName())) {
				meta = XMLUtils.getTextContent(signConfigNodes.item(numIndex));
				numIndex = XMLUtils.nextNodeElementIndex(signConfigNodes, numIndex);
			}

			// Comprobamos el nodo RESULT_TRIPHASE_RESULT_NODE
			if (numIndex == -1 || RESULT_TRIPHASE_RESULT_NODE.equalsIgnoreCase(signConfigNodes.item(numIndex).getNodeName())) {
				throw new IllegalArgumentException("No se ha encontrado el nodo " + //$NON-NLS-1$
						RESULT_TRIPHASE_RESULT_NODE +
						" en la respuesta de la peticion de postfirma del documento"); //$NON-NLS-1$
			}

			result = XMLUtils.getTextContent(signConfigNodes.item(numIndex));

			return new TriphaseSignDocumentRequest(docId, signatureFormat, params, meta, result);
		}
	}
}
