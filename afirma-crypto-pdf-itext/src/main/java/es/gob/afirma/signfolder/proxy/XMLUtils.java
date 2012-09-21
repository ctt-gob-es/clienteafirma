package es.gob.afirma.signfolder.proxy;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Clase con m&eacute;todos de utilidad para el parseo de documentos XML mediante DOM
 * manteniendo la compatibilidad con JME.
 * @author Carlos Gamuci
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public class XMLUtils {

	/**
	 * Recupera el &iacute;ndice siguiente nodo de la lista de tipo Element. Empieza a comprobar
	 * los nodos a partir del &iacute;ndice marcado. Si no encuentra un nodo de tipo elemento,
	 * devuelve -1.
	 * @param nodes Listado de nodos.
	 * @param currentIndex &Iacute;ndice del listado a partir del cual se empieza la comprobaci&oacute;n.
	 * @return &Iacute;ndice del siguiente node de tipo Element o -1 si no se encontr&oacute;.
	 */
	static int nextNodeElementIndex(final NodeList nodes, final int currentIndex) {
		Node node;
		int i = currentIndex;
		while (i < nodes.getLength()) {
			node = nodes.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE) {
				return i;
			}
			i++;
		}
		return -1;
	}

	/**
	 * Sustituto para JME de {@code Boolean.parseBoolean()}.
	 */
	static boolean parseBoolean(final String bool) {
		if ("true".equalsIgnoreCase(bool)) { //$NON-NLS-1$
			return true;
		}
		return false;
	}

	/**
	 * Recupera el contenido de un nodo. Se utiliza este m&eacute;todo en lugar del
	 * {@code getTextContent()} de la clase Node para mantener la compatibilidad con JME.
	 * @param node Nodo del que se desea el contenido.
	 * @return Contenido del nodo.
	 */
	static String getTextContent(final Node node) {
		Node child;
		String sContent = node.getNodeValue() != null ? node.getNodeValue() : ""; //$NON-NLS-1$
		final NodeList nodes = node.getChildNodes();
		for(int i = 0; i < nodes.getLength(); i++) {
			child = nodes.item(i);
			sContent += child.getNodeValue() != null ? child.getNodeValue() : ""; //$NON-NLS-1$
			if(nodes.item(i).getChildNodes().getLength() > 0) {
				sContent += getTextContent(nodes.item(i));
			}
		}

		return sContent.trim();
	}
}
