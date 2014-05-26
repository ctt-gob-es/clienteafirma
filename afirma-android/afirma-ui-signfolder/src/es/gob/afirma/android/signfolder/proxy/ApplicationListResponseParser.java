package es.gob.afirma.android.signfolder.proxy;

import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;


/** Analizador de XML para la obtenci&oacute;n de la lista de aplicaciones activas.
 * @author Carlos Gamuci */
final class ApplicationListResponseParser {

	private static final String APP_LIST_NODE = "appConf"; //$NON-NLS-1$
	private static final String APP_ID_ATTR = "id"; //$NON-NLS-1$
	

	/** Analiza un documento XML y, en caso de tener el formato correcto, obtiene la lista de aplicaciones
	 * que pueden enviar peticiones de firma.
	 * @param doc Documento XML.
	 * @return Objeto con los datos del XML.
	 * @throws IllegalArgumentException Cuando el XML no tiene el formato esperado.
	 */
	static RequestAppConfiguration parse(final Document doc) {

		if (doc == null) {
			throw new IllegalArgumentException("El documento proporcionado no puede ser nulo");  //$NON-NLS-1$
		}

		if (!APP_LIST_NODE.equalsIgnoreCase(doc.getDocumentElement().getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + APP_LIST_NODE + //$NON-NLS-1$
					"' y aparece: " + doc.getDocumentElement().getNodeName()); //$NON-NLS-1$
		}

		final List<String> appIds = new ArrayList<String>();
		final List<String> appNames = new ArrayList<String>();
		final NodeList appNodes = doc.getDocumentElement().getChildNodes();
		for (int i = 0; i < appNodes.getLength(); i++) {
			// Nos aseguramos de procesar solo nodos de tipo Element
			i = XmlUtils.nextNodeElementIndex(appNodes, i);
			if (i == -1) {
				break;
			}
			try {
				appIds.add(appNodes.item(i).getAttributes().getNamedItem(APP_ID_ATTR).getNodeValue());
				appNames.add(appNodes.item(i).getTextContent().trim());
			} catch (Exception e) {
				throw new IllegalArgumentException("Se encontro un nodo de aplicacion no valido: " + e, e); //$NON-NLS-1$
			}
		}

		RequestAppConfiguration config = new RequestAppConfiguration();
		config.setAppIdsList(appIds);
		config.setAppNamesList(appNames);
		
		return config;
	}
}
