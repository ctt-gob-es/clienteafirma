/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xml.style;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Properties;
import java.util.logging.Logger;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.SecureXmlBuilder;
import es.gob.afirma.core.ui.AOUIFactory;

/** Elemento de estilo XML (XSL) a firmar.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class XmlStyle {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

    private static final String HTTP_PROTOCOL_PREFIX = "http://"; //$NON-NLS-1$
    private static final String HTTPS_PROTOCOL_PREFIX = "https://"; //$NON-NLS-1$

	private Element element;
	private final String type;
	private final String href;
	private final String encoding;

	/** Crea un estilo XML (XSL) a firmar vac&iacute;o. */
	public XmlStyle() {
		this.element = null;
		this.type = null;
		this.href = null;
		this.encoding = null;
	}

	/** Crea un estilo XML (XSL) a firmar. Descarga la hoja de estilo si se indica una referencia externa.
	 * @param data XML en formato binario
	 * @param headless Indica si deben omitirse las interacciones con el usuario mediante interfaz gr&aacute;fico
	 * @throws IOException Cuando hay errores de entrada / salida
	 * @throws CannotDereferenceException Si no se puede obtener el estilo referenciado
	 * @throws IsInnerlException Si la referencia apunta a un elemento dentro del propio XML
	 * @throws ReferenceIsNotXmlException Cuando el estilo referenciado no est&aacute; en formato XML
	 * @throws javax.xml.transform.TransformerFactoryConfigurationError Cuando hay errores de configuraci&oacute; en la
	 *                                                                  factor&iacute;a de transformaciones */
	public XmlStyle(final byte[] data, final boolean headless) throws IOException,
	                                                                  CannotDereferenceException,
	                                                                  IsInnerlException,
	                                                                  ReferenceIsNotXmlException {
		this(data, headless, true);
	}

	/** Crea un estilo XML (XSL) a firmar.
	 * @param data XML en formato binario
	 * @param headless Indica si deben omitirse las interacciones con el usuario mediante interfaz gr&aacute;fico
	 * @param allowExternal Indica si se deben descargar hojas de estilo remotas.
	 * @throws IOException Cuando hay errores de entrada / salida
	 * @throws CannotDereferenceException Si no se puede obtener el estilo referenciado
	 * @throws IsInnerlException Si la referencia apunta a un elemento dentro del propio XML
	 * @throws ReferenceIsNotXmlException Cuando el estilo referenciado no est&aacute; en formato XML
	 * @throws javax.xml.transform.TransformerFactoryConfigurationError Cuando hay errores de configuraci&oacute; en la
	 *                                                                  factor&iacute;a de transformaciones */
	public XmlStyle(final byte[] data, final boolean headless, final boolean allowExternal)
			throws IOException,
					CannotDereferenceException,
	                IsInnerlException,
	                ReferenceIsNotXmlException {
		final Properties p = getStyleSheetHeader(new String(data));
		this.type = p.getProperty("type"); //$NON-NLS-1$
		this.href = p.getProperty("href"); //$NON-NLS-1$

		if (this.type != null && this.href != null) {

			LOGGER.info(
				"Se ha encontrado una hoja de estilo asociada al XML a firmar: tipo=" + this.type //$NON-NLS-1$
					+ ", referencia=" //$NON-NLS-1$
					+ this.href
			);

			final Document tmpDoc = dereferenceStyleSheet(
				this.href,
				headless,
				allowExternal
			);

			// Cuidado!! Solo rellenamos el Elemento DOM si no es HTTP o HTTPS,
			// porque si es accesible remotamente no necesito el elemento, ya que se
			// firma via referencia Externally Detached
			if (!this.href.startsWith(HTTP_PROTOCOL_PREFIX) &&
				!this.href.startsWith(HTTPS_PROTOCOL_PREFIX)) {
					this.element = tmpDoc.getDocumentElement();
			}
			else {
				this.element = null;
			}

			this.encoding = tmpDoc.getXmlEncoding();

		}
		else {
			this.encoding = null;
			this.element = null;
		}
	}

	/** Establece el Elemento DOM con el estilo.
	 * @param e Elemento DOM con el estilo */
	public void setStyleElement(final Element e) {
		this.element = e;
	}

	/** Obtiene el Elemento DOM con el estilo.
	 * @return Elemento DOM con el estilo */
	public Element getStyleElement() {
		return this.element;
	}

	/** Obtiene el tipo del estilo.
	 * @return Tipo del estilo */
	public String getStyleType() {
		return this.type;
	}

	/** Obtiene la referencia al estilo.
	 * @return Referencia al estilo */
	public String getStyleHref() {
		return this.href;
	}

	/** Obtiene la codificaci&oacute;n del estilo.
	 * @return Codificaci&oacute;n del estilo */
	public String getStyleEncoding() {
		return this.encoding;
	}

    /** Obtiene los par&aacute;metros de la cabecera de definici&oacute;n de la
     * hoja de estilo de un XML.
     * @param inputXML
     *        XML de entrada
     * @return Properties con los par&aacute;metros encontrados en la cabecera,
     *         o un Properties vac&iacute;o si el XML no declaraba una hoja de
     *         estilo
     * @throws IOException Si no se puede analizar adecuadamente la cabecera de estilo */
    private static Properties getStyleSheetHeader(final String inputXML) throws IOException {
        final Properties ret = new Properties();
        if (inputXML == null) {
            return ret;
        }
        final int startPos = inputXML.indexOf("<?xml-stylesheet "); //$NON-NLS-1$
        if (startPos == -1) {
            return ret;
        }

        String xml = inputXML.substring(startPos);
        xml = xml.substring(0, xml.indexOf('>') + 1)
                   .replace("<?xml-stylesheet ", "").replace("?>", "").replace(" ", "\n").replace("\"", "").replace("'", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$

        ret.load(new ByteArrayInputStream(xml.getBytes()));
        return ret;
    }

    /** Dereferencia una hoja de estilo en forma de Documento DOM.
     * @param id Identificador de la hoja de estilo.
     * @param headless <code>true</code> si <b>no</b> se desea que se pregunte al
     *                 usuario para dereferenciar las hojas de estilo enlazadas con rutas locales.
     * @param allowExternal {@code true} para indicar que se descarguen URL remotas, {@code false}
     * 						en caso contrario.
     * @return Documento DOM con la hoja de estilo.
     * @throws CannotDereferenceException Si no se puede dereferenciar.
     * @throws IsInnerlException Si no se puede dereferenciar por ser una referencia local.
     * @throws ReferenceIsNotXmlException Si el objeto dereferenciado no puede transformarse en un
     *                                    Documento DOM. */
    private static Document dereferenceStyleSheet(final String id, final boolean headless, final boolean allowExternal) throws CannotDereferenceException,
                                                                                                  IsInnerlException,
                                                                                                  ReferenceIsNotXmlException {
        if (id == null || id.isEmpty()) {
            throw new CannotDereferenceException("La hoja de estilo era nula o vacia"); //$NON-NLS-1$
        }

        byte[] xml = null;

        // Intentamos dereferenciar directamente, cosa que funciona con
        // http://, https:// y file://

        try {
        	final URI styleURI = AOUtil.createURI(id);
            if (styleURI.getScheme().equalsIgnoreCase("file")) { //$NON-NLS-1$
                throw new UnsupportedOperationException("No se aceptan dereferenciaciones directas con file://"); //$NON-NLS-1$
            }

        	if (!allowExternal) {
        		if (styleURI.getScheme().equalsIgnoreCase("http") //$NON-NLS-1$
        				|| styleURI.getScheme().equalsIgnoreCase("https") //$NON-NLS-1$
        				|| styleURI.getScheme().equalsIgnoreCase("ftp") //$NON-NLS-1$
        				|| styleURI.getScheme().equalsIgnoreCase("ftps")) { //$NON-NLS-1$
                    throw new UnsupportedOperationException("No se acepta la dereferenciacion de direcciones remotas"); //$NON-NLS-1$
                }
        	}

            // AOUtil.loadFile() usa internamente un ByteArrayInputStream, pero externamente es mejor
            // usar siempre try-with-resources para no depender de esta implementacion concreta
            try (final InputStream is = AOUtil.loadFile(styleURI)) {
            	xml = AOUtil.getDataFromInputStream(is);
            }
        }
        catch (final Exception e) {

            // Si no dereferencia puede ser por tres cosas, porque es una referencia interna,
            // porque es una referencia local o porque directamente no se puede dereferenciar

            // Miramos si la referencia es local
            final String[] idParts = id.replace(File.separator, "/").split("/"); //$NON-NLS-1$ //$NON-NLS-2$
            final String fileName = idParts[idParts.length - 1];

            if (fileName.startsWith("#")) { //$NON-NLS-1$
                throw new IsInnerlException(e);
            }
            else if (!headless && id.startsWith("file://")) { //$NON-NLS-1$
            	// Preguntamos al usuario para la dereferenciacion
            	if (AOUIFactory.showConfirmDialog(null,
            			XmlStyleMessages.getString("XmlStyle.5"), //$NON-NLS-1$
            			XmlStyleMessages.getString("XmlStyle.6"), //$NON-NLS-1$
            			AOUIFactory.OK_CANCEL_OPTION,
            			AOUIFactory.INFORMATION_MESSAGE) == AOUIFactory.OK_OPTION) {

            		final File xmlStyleFile;
            		try {
	            		xmlStyleFile = AOUIFactory.getLoadFiles(
            				XmlStyleMessages.getString("XmlStyle.7"), //$NON-NLS-1$
            				null,
            				fileName,
            				null,
            				XmlStyleMessages.getString("XmlStyle.8", fileName), //$NON-NLS-1$
            				false,
            				false,
            				null,
            				null
	            		)[0];
            		}
            		catch(final AOCancelledOperationException ex) {
            			LOGGER.warning("El usuario ha cancelado la seleccion de hoja de estilo: " + ex); //$NON-NLS-1$
            			throw new CannotDereferenceException(
        					"No se ha podido dereferenciar la hoja de estilo: " + ex, e //$NON-NLS-1$
    					);
            		}
            		try (
        				final InputStream is = new FileInputStream(xmlStyleFile);
    				) {
            			xml = AOUtil.getDataFromInputStream(is);
            		}
            		catch (final Exception ex) {
            			throw new CannotDereferenceException(
        					"No se ha podido dereferenciar la hoja de estilo: " + e, ex //$NON-NLS-1$
    					);
            		}
            	}
            }
            else {
                throw new CannotDereferenceException(
            		"No se ha podido dereferenciar la hoja de estilo '" + id + "': " + e, e //$NON-NLS-1$ //$NON-NLS-2$
        		);
            }
        }

        try {
            if (xml != null) {
                return SecureXmlBuilder.getSecureDocumentBuilder().parse(new ByteArrayInputStream(xml));
            }
            throw new CannotDereferenceException("No se ha dereferenciado la hoja de estilo"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            throw new ReferenceIsNotXmlException(e);
        }
    }

}
