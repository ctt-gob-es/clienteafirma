package es.gob.afirma.android.signfolder.proxy;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

/**
 * Analizador de las respuestas de las peticiones de previsualizaci&oacute;n de documentos.
 */
public class PreviewDocumentResponseParser {

	/** Nombre de nodo principal del XML de respuesta de previsualizaci&oacute;n de fichero. */
	private static final String PREVIEW_NODE = "prw"; //$NON-NLS-1$

	private static final String DOCID_ATTR = "docid"; //$NON-NLS-1$
	private static final String FILENAME_NODE = "name"; //$NON-NLS-1$
	private static final String MIMETYPE_NODE = "mmtp"; //$NON-NLS-1$
	private static final String DATA_NODE = "data"; //$NON-NLS-1$

	static DocumentData parse(final Document doc) {

		if (doc == null) {
			throw new IllegalArgumentException("El documento proporcionado no puede ser nulo");  //$NON-NLS-1$
		}

		if (!PREVIEW_NODE.equalsIgnoreCase(doc.getDocumentElement().getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + PREVIEW_NODE + //$NON-NLS-1$
					"' y aparece: " + doc.getDocumentElement().getNodeName()); //$NON-NLS-1$
		}

		final Attr docIdAttr = doc.getDocumentElement().getAttributeNode(DOCID_ATTR);
		if (docIdAttr == null) {
			throw new IllegalArgumentException("No se ha encontrado en la respuesta el atributo '" + DOCID_ATTR + //$NON-NLS-1$
					"' con el identificador del documento para previsualizar"); //$NON-NLS-1$
		}
		final String docid = docIdAttr.getValue();

		final NodeList paramsNodes = doc.getDocumentElement().getChildNodes();

		final String filename;
		final String mimetype;
		final String dataB64;

		// Procesar los nodos de tipo Element
		int index = XmlUtils.nextNodeElementIndex(paramsNodes, 0);
		if (index == -1 || !FILENAME_NODE.equalsIgnoreCase(paramsNodes.item(index).getNodeName())) {
			throw new IllegalArgumentException("Los datos del documento para previsualizar con identificador " + //$NON-NLS-1$
					docid + "' no contiene el elemento " + FILENAME_NODE); //$NON-NLS-1$
		}
		filename =  XmlUtils.getTextContent(paramsNodes.item(index));

		index = XmlUtils.nextNodeElementIndex(paramsNodes, ++index);
		if (index == -1 || !MIMETYPE_NODE.equalsIgnoreCase(paramsNodes.item(index).getNodeName())) {
			throw new IllegalArgumentException("Los datos del documento para previsualizar con identificador " + //$NON-NLS-1$
					docid + "' no contiene el elemento " + MIMETYPE_NODE); //$NON-NLS-1$
		}
		mimetype =  XmlUtils.getTextContent(paramsNodes.item(index));

		index = XmlUtils.nextNodeElementIndex(paramsNodes, ++index);
		if (index == -1 || !DATA_NODE.equalsIgnoreCase(paramsNodes.item(index).getNodeName())) {
			throw new IllegalArgumentException("Los datos del documento para previsualizar con identificador " + //$NON-NLS-1$
					docid + "' no contiene el elemento " + DATA_NODE); //$NON-NLS-1$
		}
		dataB64 =  XmlUtils.getTextContent(paramsNodes.item(index));

		return new DocumentData(docid, filename, mimetype, dataB64);
	}
}
