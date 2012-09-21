package es.gob.afirma.signfolder.proxy;

import java.util.Vector;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Analizador de XML para la generaci&oacute;n de objetos de tipo
 * {@link es.gob.afirma.signfolder.proxy.TriphaseRequestBean} a partir
 * de un XML de respuesta de postfirma.
 *
 * @author Carlos Gamuci
 */
public class PostsignsResponseParser {

	private static final String POSTSIGN_RESPONSE_NODE = "posts"; //$NON-NLS-1$

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

		if (!POSTSIGN_RESPONSE_NODE.equalsIgnoreCase(doc.getDocumentElement().getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + //$NON-NLS-1$
					POSTSIGN_RESPONSE_NODE + "' y aparece: " + //$NON-NLS-1$
					doc.getDocumentElement().getNodeName());
		}

		final NodeList requestNodes = doc.getDocumentElement().getChildNodes();
		final Vector listRequests = new Vector();
		for (int i = 0; i < requestNodes.getLength(); i++) {
			// Nos aseguramos de procesar solo nodos de tipo Element
			i = XMLUtils.nextNodeElementIndex(requestNodes, i);
			if (i == -1) {
				break;
			}
			listRequests.addElement(RequestParser.parse(requestNodes.item(i)));
		}

		final TriphaseRequest[] tmpReqs = new TriphaseRequest[listRequests.size()];
		listRequests.copyInto(tmpReqs);

		return new TriphaseResponseBean(tmpReqs);
	}

	private static final class RequestParser {

		private static final String REQUEST_NODE = "req"; //$NON-NLS-1$
		private static final String REFERENCE_ATTRIBUTE = "ref"; //$NON-NLS-1$
		private static final String STATUS_ATTRIBUTE = "status"; //$NON-NLS-1$

		static TriphaseRequest parse(final Node requestNode) {

			if (!REQUEST_NODE.equalsIgnoreCase(requestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						requestNode.getNodeName() + "' en el listado de peticiones"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String ref;
			boolean statusOk = true;

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = requestNode.getAttributes();
			attributeNode = attributes.getNamedItem(REFERENCE_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						REFERENCE_ATTRIBUTE + "' en un peticion de prefirma"); //$NON-NLS-1$
			}
			ref = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(STATUS_ATTRIBUTE);
			// statusOk = true, salvo que la propiedad status tenga el valor "KO"
			statusOk = (attributeNode == null || !"KO".equalsIgnoreCase(attributeNode.getNodeValue())); //$NON-NLS-1$

			return new TriphaseRequest(ref, statusOk, null);
		}
	}
}
