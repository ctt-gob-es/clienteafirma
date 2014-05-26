package es.gob.afirma.android.signfolder.proxy;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import android.util.Log;
import es.gob.afirma.android.signfolder.SFConstants;

/**
 * Analizador de XML para la generaci&oacute;n de un listado de objetos
 * de tipo {@link es.gob.afirma.android.signfolder.proxy.TriphaseRequest} a partir
 * de un XML de respuesta de postfirma.
 *
 * @author Carlos Gamuci
 */
public class PostsignsResponseParser {

	private static final String POSTSIGN_RESPONSE_NODE = "posts"; //$NON-NLS-1$

	private PostsignsResponseParser() {
		// No instanciable
	}

	/**
	 * Analiza un documento XML y, en caso de tener el formato correcto, obtiene de &eacute;l
	 * un listado de objetos de tipo {@link es.gob.afirma.android.signfolder.proxy.TriphaseRequest}.
	 * @param doc Documento XML.
	 * @return Objeto con los datos del XML.
	 * @throws IllegalArgumentException Cuando el XML no tiene el formato esperado.
	 */
	static RequestResult parse(final Document doc) {

		if (doc == null) {
			throw new IllegalArgumentException("El documento proporcionado no puede ser nulo");  //$NON-NLS-1$
		}

		if (!POSTSIGN_RESPONSE_NODE.equalsIgnoreCase(doc.getDocumentElement().getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + //$NON-NLS-1$
					POSTSIGN_RESPONSE_NODE + "' y aparece: " + //$NON-NLS-1$
					doc.getDocumentElement().getNodeName());
		}

		final NodeList requestNodes = doc.getDocumentElement().getChildNodes();
		final Node requestNode = requestNodes.item(XmlUtils.nextNodeElementIndex(requestNodes, 0));
		return RequestResultParser.parse(requestNode);

	}

	private static final class RequestResultParser {

		private static final String REQUEST_NODE = "req"; //$NON-NLS-1$
		private static final String ID_ATTRIBUTE = "id"; //$NON-NLS-1$
		private static final String STATUS_ATTRIBUTE = "status"; //$NON-NLS-1$

		static RequestResult parse(final Node requestNode) {

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
			attributeNode = attributes.getNamedItem(ID_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						ID_ATTRIBUTE + "' en un peticion de prefirma"); //$NON-NLS-1$
			}
			ref = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(STATUS_ATTRIBUTE);
			// statusOk = true, salvo que la propiedad status tenga el valor "KO"
			statusOk = (attributeNode == null || !"KO".equalsIgnoreCase(attributeNode.getNodeValue())); //$NON-NLS-1$

			Log.i(SFConstants.LOG_TAG, "Ref="+ref+"; status=" + statusOk);
			
			return new RequestResult(ref, statusOk);
		}
	}
}
