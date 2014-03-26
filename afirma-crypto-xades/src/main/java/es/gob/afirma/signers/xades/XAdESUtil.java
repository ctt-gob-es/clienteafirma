package es.gob.afirma.signers.xades;

import java.util.logging.Logger;

import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;

final class XAdESUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

	private XAdESUtil() {
		// No permitimos la instanciacion
	}

	static Element getFirstElmentFromXPath(final String xpathExpression, final Element sourceElement) throws AOException {
		final NodeList nodeList;
		try {
			 nodeList = (NodeList)XPathFactory.newInstance().newXPath().evaluate(xpathExpression, sourceElement, XPathConstants.NODESET);
		}
		catch (final XPathExpressionException e1) {
			throw new AOException(
				"No se ha podido evaluar la expresion indicada para la insercion de la firma Enveloped ('" + xpathExpression + "'): " + e1, //$NON-NLS-1$ //$NON-NLS-2$
				e1
			);
		}
		if (nodeList.getLength() < 1) {
			throw new AOException(
				"La expresion indicada para la insercion de la firma Enveloped ('" + xpathExpression + "') no ha devuelto ningun nodo" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		if (nodeList.getLength() > 1) {
			LOGGER.warning(
				"La expresion indicada para la insercion de la firma Enveloped ('" + xpathExpression + "') ha devuelto varios nodos, se usara el primero" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		return (Element) nodeList.item(0);
	}

}
