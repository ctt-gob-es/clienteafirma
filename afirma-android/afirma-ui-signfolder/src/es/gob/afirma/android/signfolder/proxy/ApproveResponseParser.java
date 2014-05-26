package es.gob.afirma.android.signfolder.proxy;

import java.util.Vector;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Analizador de XML de respuesta de la petic&oacute;n de visto bueno de solicitudes de firma.
 *
 * @author Carlos Gamuci
 */
public class ApproveResponseParser {

	private static final String APPROVE_RESPONSE_NODE = "apprq"; //$NON-NLS-1$

	private ApproveResponseParser() {
		// No instanciable
	}

	/**
	 * Analiza un documento XML y, en caso de tener el formato correcto, obtiene de &eacute;l
	 * un listado de objetos de tipo {@link es.gob.afirma.android.signfolder.proxy.RequestResult}.
	 * @param doc Documento XML.
	 * @return Objeto con los datos del XML.
	 * @throws IllegalArgumentException Cuando el XML no tiene el formato esperado.
	 */
	static RequestResult[] parse(final Document doc) {

		if (doc == null) {
			throw new IllegalArgumentException("El documento proporcionado no puede ser nulo");  //$NON-NLS-1$
		}

		if (!APPROVE_RESPONSE_NODE.equalsIgnoreCase(doc.getDocumentElement().getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + //$NON-NLS-1$
					APPROVE_RESPONSE_NODE + "' y aparece: " + //$NON-NLS-1$
					doc.getDocumentElement().getNodeName());
		}

		final NodeList requestNodes = doc.getDocumentElement().getChildNodes();
		final Vector<RequestResult> listRequests = new Vector<RequestResult>();
		for (int i = 0; i < requestNodes.getLength(); i++) {
			// Nos aseguramos de procesar solo nodos de tipo Element
			i = XmlUtils.nextNodeElementIndex(requestNodes, i);
			if (i == -1) {
				break;
			}
			listRequests.addElement(ApproveParser.parse(requestNodes.item(i)));
		}

		final RequestResult[] ret = new RequestResult[listRequests.size()];
		listRequests.copyInto(ret);
		return ret;
	}

	private static final class ApproveParser {

		private static final String APPROVE_NODE = "r"; //$NON-NLS-1$
		private static final String ID_ATTRIBUTE = "id"; //$NON-NLS-1$
		private static final String OK_ATTRIBUTE = "ok"; //$NON-NLS-1$

		static RequestResult parse(final Node requestNode) {

			if (!APPROVE_NODE.equalsIgnoreCase(requestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						requestNode.getNodeName() + "' en el listado de peticiones"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String ref;
			boolean ok = true;

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = requestNode.getAttributes();
			attributeNode = attributes.getNamedItem(ID_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						ID_ATTRIBUTE + "' en el resultado de rechazo de peticion"); //$NON-NLS-1$
			}
			ref = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(OK_ATTRIBUTE);
			// ok = true, salvo que la propiedad status tenga el valor "KO"
			ok = (attributeNode == null || !"KO".equalsIgnoreCase(attributeNode.getNodeValue())); //$NON-NLS-1$

			return new RequestResult(ref, ok);
		}
	}
}
