package es.gob.afirma.core.misc;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;

/**
 * Constructor de objetos para transformar un árbol de origen XML en un árbol de resultados.
 * Fail-closed: si no se pueden establecer las características de seguridad,
 * se lanza una excepción.
 */
public class SecureXmlTransformer {

	private static TransformerFactory TRANSFORMER_FACTORY = null;

	/**
	 * Obtiene un transformador de árboles DOM con el que crear o cargar un XML.
	 * Fail-closed: si no se pueden establecer las características de seguridad,
	 * se lanza una excepción.
	 * @return Transformador de árboles DOM.
	 * @throws TransformerConfigurationException Error al crear el transformador.
	 * @throws IllegalStateException Si no se pueden establecer las características de seguridad.
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
				Logger.getLogger("es.gob.afirma").log(Level.SEVERE, "No se ha podido establecer el procesado seguro en la factoria XML: " + e); //$NON-NLS-1$ //$NON-NLS-2$
				throw new IllegalStateException("No se pudieron establecer características de seguridad en TransformerFactory", e);
			}

		}
		return TRANSFORMER_FACTORY.newTransformer();
	}
}
