package es.gob.afirma.core.misc;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Constructor de objetos para la carga de documentos XML.
 * Fail-closed: si no se pueden establecer las características de seguridad críticas,
 * se aborta la creación del parser.
 * Para atributos no soportados por la implementación, se loggea pero no falla.
 */
public class SecureXmlBuilder {

	private static DocumentBuilderFactory SECURE_BUILDER_FACTORY = null;

    private static SAXParserFactory SAX_FACTORY = null;

	/**
	 * Obtiene un generador de árboles DOM con el que crear o cargar un XML.
	 * Fail-closed para características de seguridad críticas.
	 * @return Generador de árboles DOM.
	 * @throws ParserConfigurationException Cuando ocurre un error durante la creación.
	 * @throws IllegalStateException Si no se pueden establecer las características de seguridad críticas.
	 */
	public static DocumentBuilder getSecureDocumentBuilder() throws ParserConfigurationException {
		if (SECURE_BUILDER_FACTORY == null) {
			SECURE_BUILDER_FACTORY = DocumentBuilderFactory.newInstance();
			
			// Establecer FEATURE_SECURE_PROCESSING - fail-closed
			try {
				SECURE_BUILDER_FACTORY.setFeature(SecureXmlConstants.FEATURE_SECURE_PROCESSING, Boolean.TRUE.booleanValue());
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").log(Level.SEVERE, "No se ha podido establecer el procesado seguro en la factoria XML: " + e); //$NON-NLS-1$ //$NON-NLS-2$
				throw new IllegalStateException("No se pudo establecer FEATURE_SECURE_PROCESSING en DocumentBuilderFactory", e);
			}

			// Los siguientes atributos deberían establecerse automáticamente la implementación de
			// la biblioteca al habilitar la característica anterior. Por si acaso, los establecemos
			// expresamente. No son críticos si fallan (algunas implementaciones no los soportan).
			final String[] securityProperties = new String[] {
					SecureXmlConstants.ACCESS_EXTERNAL_DTD,
					SecureXmlConstants.ACCESS_EXTERNAL_SCHEMA,
					SecureXmlConstants.ACCESS_EXTERNAL_STYLESHEET
			};
			for (final String securityProperty : securityProperties) {
				try {
					SECURE_BUILDER_FACTORY.setAttribute(securityProperty, ""); //$NON-NLS-1$
				}
				catch (final Exception e) {
					// No son críticos, solo loggeamos
					Logger.getLogger("es.gob.afirma").log(Level.FINE, "No se ha podido establecer una propiedad de seguridad '" + securityProperty + "' en la factoria XML"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
			}

			// DOCTYPE interno se admite (firmas XML legítimas). Bloqueamos entidades y DTD
			// externas (#55). FEATURE_SECURE_PROCESSING limita bombas de entidades.
			try {
				SECURE_BUILDER_FACTORY.setFeature("http://xml.org/sax/features/external-general-entities", false); //$NON-NLS-1$
				SECURE_BUILDER_FACTORY.setFeature("http://xml.org/sax/features/external-parameter-entities", false); //$NON-NLS-1$
				SECURE_BUILDER_FACTORY.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false); //$NON-NLS-1$
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").log(Level.SEVERE, "No se han podido desactivar entidades/DTD externos en DocumentBuilderFactory: " + e); //$NON-NLS-1$ //$NON-NLS-2$
				throw new IllegalStateException("No se pudieron desactivar entidades externas en DocumentBuilderFactory", e);
			}

			SECURE_BUILDER_FACTORY.setValidating(false);
			SECURE_BUILDER_FACTORY.setNamespaceAware(true);
			SECURE_BUILDER_FACTORY.setXIncludeAware(false);
			// Expandimos entidades internas; el EntityResolver rechaza file/http externos.
			SECURE_BUILDER_FACTORY.setExpandEntityReferences(true);
		}
		final DocumentBuilder builder = SECURE_BUILDER_FACTORY.newDocumentBuilder();
		builder.setEntityResolver(REJECT_EXTERNAL_ENTITIES);
		return builder;
	}

	/** Rechaza cualquier referencia SYSTEM/PUBLIC a recurso externo. */
	private static final EntityResolver REJECT_EXTERNAL_ENTITIES = new EntityResolver() {
		@Override
		public InputSource resolveEntity(final String publicId, final String systemId) throws SAXException, IOException {
			if (systemId != null && !systemId.isEmpty()) {
				throw new SAXException("Referencia externa no permitida en XML: " + systemId); //$NON-NLS-1$
			}
			if (publicId != null && !publicId.isEmpty()) {
				throw new SAXException("Referencia externa no permitida en XML"); //$NON-NLS-1$
			}
			return new InputSource(new java.io.StringReader("")); //$NON-NLS-1$
		}
	};

	/**
     * Construye un parser SAX seguro que no accede a recursos externos.
     * Fail-closed: si no se pueden establecer las características de seguridad,
     * se lanza una excepción.
     * @return Factoría segura.
	 * @throws SAXException Cuando ocurre un error de SAX.
	 * @throws ParserConfigurationException Cuando no se puede crear el parser.
     */
	public static SAXParser getSecureSAXParser() throws ParserConfigurationException, SAXException {
		if (SAX_FACTORY == null) {
			SAX_FACTORY = SAXParserFactory.newInstance();
			try {
				SAX_FACTORY.setFeature(SecureXmlConstants.FEATURE_SECURE_PROCESSING, Boolean.TRUE.booleanValue());
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").log(Level.SEVERE, "No se ha podido establecer una característica de seguridad en la factoria XML: " + e); //$NON-NLS-1$
				throw new IllegalStateException("No se pudo establecer FEATURE_SECURE_PROCESSING en SAXParserFactory", e);
			}

			// Desactivamos las características que permiten la carga de elementos externos
			try {
				SAX_FACTORY.setFeature("http://xml.org/sax/features/external-general-entities", false); //$NON-NLS-1$
				SAX_FACTORY.setFeature("http://xml.org/sax/features/external-parameter-entities", false); //$NON-NLS-1$
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").log(Level.SEVERE, "No se ha podido establecer una característica de seguridad en la factoria SAX XML: " + e); //$NON-NLS-1$
				throw new IllegalStateException("No se pudieron desactivar entidades externas en SAXParserFactory", e);
			}

			try {
				SAX_FACTORY.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false); //$NON-NLS-1$
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").log(Level.SEVERE, "No se ha podido desactivar DTD externo en SAX: " + e); //$NON-NLS-1$ //$NON-NLS-2$
				throw new IllegalStateException("No se pudo desactivar DTD externo en SAXParserFactory", e);
			}

			SAX_FACTORY.setValidating(false);
			SAX_FACTORY.setNamespaceAware(true);
		}
		return SAX_FACTORY.newSAXParser();
	}
}
