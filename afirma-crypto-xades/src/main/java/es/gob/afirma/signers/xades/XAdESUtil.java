package es.gob.afirma.signers.xades;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

final class XAdESUtil {

	private XAdESUtil() {
		// No permitimos la instanciacion
	}

	static Element getElementById(final Document doc, final String nodeId) {
		if (doc == null || nodeId == null) {
			return null;
		}
		final Element ret = doc.getElementById(nodeId);
		if (ret != null) {
			return ret;
		}
		// Que no se haya encontrado significa que no hay ningun nodo con un atributo declarado como
		// identificador en el esquema XML con el valor buscado.
		// No obstante, es habitual que el atributo identificador sea "id", "Id" o "ID" pero que no
		// se haya declarado adecuadamente (por ejemplo, con una sentencia del tipo
		// <xsd:attribute name="id" type="xsd:ID"/>).
		// Por esta razon buscamos entre todos los nodos por si hay un atributo id=nodeId y lo damos
		// por bueno
	    final NodeList nodeList = doc.getElementsByTagName("*"); //$NON-NLS-1$
	    for (int i = 0, len = nodeList.getLength(); i < len; i++) {
	        final Node node = nodeList.item(i);
	        if (node.getNodeType() == Node.ELEMENT_NODE) {
	        	// Buscamos un atributo 'Id'
	        	final NamedNodeMap nnm = node.getAttributes();
	        	for (int j = 0; j < nnm.getLength(); ++j) {
	        	    final Node attr = nnm.item(j);
	        	    if ("id".equalsIgnoreCase(attr.getNodeName()) && nodeId.equals(attr.getNodeValue()) && node instanceof Element) { //$NON-NLS-1$
	        	    	System.out.println(attr.getNodeValue());
	        	    	return (Element) node;
	        	    }
	        	}
	        }
	    }

		return null;
	}

}
