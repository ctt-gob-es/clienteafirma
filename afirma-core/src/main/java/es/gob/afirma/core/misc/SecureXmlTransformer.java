package es.gob.afirma.core.misc;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;

/**
 * Constructor de objetos para transformar un arbol de origen XML en un arbol de resultados.
 */
public class SecureXmlTransformer {

	private static TransformerFactory TRANSFORMER_FACTORY = null;

	/**
	 * Obtiene un transformador de &aacute;boles DOM con el que crear o cargar un XML.
	 * @return Transformador de &aacute;rboles DOM.
	 * @throws TransformerConfigurationException Error al crear el transformador.
	 */
	public static Transformer getSecureTransformer() throws TransformerConfigurationException {
		if (TRANSFORMER_FACTORY == null) {
			TRANSFORMER_FACTORY = TransformerFactory.newInstance();
			try {
				TRANSFORMER_FACTORY.setFeature(SecureXmlConstants.FEATURE_SECURE_PROCESSING, Boolean.TRUE.booleanValue());
				TRANSFORMER_FACTORY.setAttribute(SecureXmlConstants.ACCESS_EXTERNAL_DTD, ""); //$NON-NLS-1$
				TRANSFORMER_FACTORY.setAttribute(SecureXmlConstants.ACCESS_EXTERNAL_STYLESHEET, ""); //$NON-NLS-1$
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").log(Level.WARNING, "No se ha podido establecer el procesado seguro en la factoria XML: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}

		}
		return TRANSFORMER_FACTORY.newTransformer();
	}
}
