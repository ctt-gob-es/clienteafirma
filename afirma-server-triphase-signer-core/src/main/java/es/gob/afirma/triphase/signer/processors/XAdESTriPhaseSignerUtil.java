/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.signer.processors;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.xades.XAdESUtil;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.dereference.CustomUriDereferencer;
import es.gob.afirma.triphase.signer.xades.NodeDelimiter;

final class XAdESTriPhaseSignerUtil {

	private XAdESTriPhaseSignerUtil() {
		// No instanciable
	}

	private static final String DS_NAMESPACE_URL = "http://www.w3.org/2000/09/xmldsig#"; //$NON-NLS-1$
	private static final String SIGNED_PROPERTIES_TYPE_SUFIX = "#SignedProperties"; //$NON-NLS-1$

	private static final String ENVELOPED_ALGORITHM_TRANSFORM = "http://www.w3.org/2000/09/xmldsig#enveloped-signature"; //$NON-NLS-1$

	private static final String USE_MANIFEST = "useManifest"; //$NON-NLS-1$

	private static final String EXTRA_PARAM_FORMAT = "format"; //$NON-NLS-1$

	private static final String REPLACEMENT_TEMPLATE = "====REPLACE-%1$d===="; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String ID_STR = "Id=\""; //$NON-NLS-1$

	/** Inserta en la estructura base XML las partes comunes de la firma XML generada
	 * para los mismos datos y certificados.
	 * @param xmlBase Estructura base XML de la firma.
	 * @param xmlSource Firma XML con las partes comunes que hay que insertar en la estructura base.
	 * @param extraParams Par&aacute;metros de configuraci&oacute;n de la firma.
	 * @return Firma XML completa.
	 * @throws SAXException Cuando se detecta un error de forma al parsear los XML.
	 * @throws IOException Cuando ocurre un error en la lectura de los XML.
	 * @throws ParserConfigurationException Cuando no se puede construir los XML. */
	static byte[] insertCommonParts(final byte[] xmlBase,
			                        final byte[] xmlSource,
			                        final Properties extraParams) throws SAXException,
	                                                                     IOException,
	                                                                     ParserConfigurationException {

		if (extraParams != null && (Boolean.parseBoolean(extraParams.getProperty(USE_MANIFEST))
				|| AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equalsIgnoreCase(
						extraParams.getProperty(EXTRA_PARAM_FORMAT)))) {
			return xmlBase;
		}

		// Obtenemos el listado de referencias a los elementos a ser reemplazados a partir de la firma original
		final Document docBase = getDocumentFromBytes(xmlBase);
		final List<String> docBaseReferences =
				getInmutableReferences(docBase);

		// Obtenemos el listado de referencias a los elementos comunes de la firma recien generada
		final Document docSource = getDocumentFromBytes(xmlSource);
		final List<String> docSourceReferences =
				getInmutableReferences(docSource);

		// Comprobamos que sean el mismo numero de elementos
		if (docBaseReferences.size() != docSourceReferences.size()) {
			throw new IllegalArgumentException(
					"El documento base no tiene las mismas partes comunes que el documento fuente" //$NON-NLS-1$
					);
		}

		// Obtenemos los delimitadores que nos indican cual es el contenido comun que deberemos sacar de la firma
		// recien generada para completar la firma original
		final List<NodeDelimiter> elDeliSource =
				getCommonContentDelimiters(docSourceReferences, docSource);

		// Cargamos ambas firmas como cadenas para poder extraer los elementos tal como son
		StringBuilder base = new StringBuilder(new String(xmlBase, docBase.getXmlEncoding()));
		final StringBuilder source = new StringBuilder(new String(xmlSource, docSource.getXmlEncoding()));

		// Realizamos el reemplazo
		for (int i = 0; i < elDeliSource.size(); i++) {

			final ContentDelimited content = getContent(source, elDeliSource.get(i));

			final String dummyString = String.format(REPLACEMENT_TEMPLATE, Integer.valueOf(i));

			final int idx = base.indexOf(dummyString);

			base = base.replace(
				idx,
				idx + dummyString.length(),
				content.getContent()
			);

		}
		return base.toString().getBytes(docBase.getXmlEncoding());
	}

	/** Elimina de una firma XML el contenido de los nodos referenciados por la propia firma.
	 * En caso de tratarse una firma enveloped o de Manifest, no hace nada.<br>
	 * @param xml XML de firma sin los nodos referenciados. En el caso de firma de manifest,
	 * devuelve la propia entrada.
	 * @param xmlEncoding Encoding del XML.
	 * @param extraParams Par&aacute;metros de configuraci&oacute;n de la firma.
	 * @return XML sin el contenido referenciado. */
	static byte[] removeCommonParts(final byte[] xml, final String xmlEncoding, final Properties extraParams) {

		if (xml == null || xml.length < 1) {
			throw new IllegalArgumentException("El XML de entrada no puede ser nulo ni vacio"); //$NON-NLS-1$
		}

		// No se eliminara nada si se trata de una firma enveloped
		if (extraParams != null && (Boolean.parseBoolean(extraParams.getProperty(USE_MANIFEST))
				|| AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equalsIgnoreCase(
						extraParams.getProperty(EXTRA_PARAM_FORMAT)))) {
			return xml;
		}

		// Cargamos el XML para identificar su codificacion
		final org.w3c.dom.Document doc;
		try {
			doc = XAdESTriPhaseSignerUtil.getDocumentFromBytes(xml);
		}
		catch (final Exception e) {
			LOGGER.warning(
				"No ha podido tratarse la entrada como XML, no se eliminaran las partes comunes: " + e //$NON-NLS-1$
			);
			return xml;
		}

		// Identificamos la codificacion del resultado
		Charset encoding = null;
		if (xmlEncoding != null) {
			try {
				encoding = Charset.forName(xmlEncoding);
			}
			catch (final Exception e) {
				LOGGER.warning("La codificacion indicada para el XML no es valida: " + xmlEncoding); //$NON-NLS-1$
			}
		}
		if (encoding == null && doc.getXmlEncoding() != null) {
			try {
				encoding = Charset.forName(doc.getXmlEncoding());
			}
			catch (final Exception e) {
				LOGGER.warning("La codificacion declarada por el XML no es valida: " + doc.getXmlEncoding()); //$NON-NLS-1$
			}
		}

		StringBuilder ret = new StringBuilder(
				encoding != null
					? new String(xml, encoding)
					: new String(xml));

		final List<NodeDelimiter> delits = cleanContentDelimiters(
			getCommonContentDelimiters(
				getInmutableReferences(doc),
				doc
			),
			ret
		);

		// Reemplazamos todo el contenido que haya entre los delimitadores por
		// una cadena de reemplazo que podamos identificar posteriormente
		for (int i = 0; i < delits.size(); i++) {
			final ContentDelimited content = getContent(ret, delits.get(i));
			ret = ret.replace(
					content.getStartContentIdx(),
					content.getEndContentIdx(),
					String.format(REPLACEMENT_TEMPLATE, Integer.valueOf(i)));
		}

		return encoding != null
				? ret.toString().getBytes(encoding)
				: ret.toString().getBytes();
	}

	/** Limpia la lista de delimitadores de nodos.
	 * Los calculados en base al API XML pueden no corresponder con los reales en el texto
	 * XML por la inclusi&oacute;n de espacios de nombres, por lo que es necesario revisarla
	 * y corregir las discordancias.
	 * @param or Lista original de delimitadores de nodos
	 * @param orXml XML en su forma de texto.
	 * @return Lista de delimitadores de nodos con los delimitadores correctos. */
	private static List<NodeDelimiter> cleanContentDelimiters(final List<NodeDelimiter> or, final StringBuilder orXml) {
		final List<NodeDelimiter> ret = new ArrayList<>(or.size());
		for (final NodeDelimiter del : or) {
			// Las discordancias siempre estan en la apertura del nodo
			final String orDel = del.getOpenTag();
			if (orXml.indexOf(orDel) != -1) {
				ret.add(del);
			}
			else {

				// Obtenemos el texto de inicio del nodo
				final String nodeStart = orDel.substring(
					0,
					orDel.indexOf(' ')
				);

				// Obtenemos todos los indices de ocurrencias del texto de inicio del nodo, por
				// si hay varias ocurrencias
				final List<Integer> indexes = new ArrayList<>();
				for (int index = orXml.indexOf(nodeStart); index >= 0; index = orXml.indexOf(nodeStart, index + 1)) {
				    indexes.add(Integer.valueOf(index));
				}

				String retDel = null;
				for (final Integer beginIndex : indexes) {
					retDel = orXml.substring(
						beginIndex.intValue(),
						orXml.indexOf(">", beginIndex.intValue()) + 1 //$NON-NLS-1$
					);

					// En este punto 'retDel' es un candidato, comprobamos que el Id sea el mismo
					if (retDel.contains(ID_STR)) {
						final String id = orXml.substring(
							orXml.indexOf(ID_STR, beginIndex.intValue()),
							orXml.indexOf("\"",  orXml.indexOf(ID_STR, beginIndex.intValue()) + ID_STR.length()) //$NON-NLS-1$
						);
						if (retDel.contains(id)) {
							break;
						}
					}
					else {
						break;
					}
				}

				if (retDel != null) {
					del.setOpenTag(retDel);
					ret.add(del);
				}
				else {
					LOGGER.warning(
						"No se ha completado la limpieza de nodos a reemplazar, es posible que falle el proceso" //$NON-NLS-1$
					);
					ret.add(del);
				}

			}
		}
		return ret;
	}

	/** Obtiene los nodos delimitadores de todos nodos referenciados por las firmas XML del documento.
	 * @param uris Listado de referencias.
	 * @param doc Documento de firma XML al que pertenecen las referencias.
	 * @return Listado de nodos. */
	private static List<NodeDelimiter> getCommonContentDelimiters(final List<String> uris,
			                                                     final Document doc) {
		final String encoding = doc.getInputEncoding();
		final List<NodeDelimiter> delimiters = new ArrayList<>();
		for (final String uriValue : uris) {
			final Node node = CustomUriDereferencer.getNodeByInternalUriReference(uriValue, doc);
			if (node != null) {
				final String nodeContent = getNodeAsText(node, encoding);
				delimiters.add(getDelimiterTags(nodeContent));
			}
		}
		return delimiters;
	}

	/**
	 * Convierte un nodo a texto.
	 * @param node Nodo que se desea obtener como texto.
	 * @param encoding Codificaci&oacute;n del texto (deber&iacute;a coincidir
	 * con la del XML al que pertenece el nodo).
	 * @return Nodo en forma de texto.
	 */
	private static String getNodeAsText(final Node node, final String encoding) {

		String content;
		final byte[] nodeContent = Utils.writeXML(node, null, null, null);
		if (encoding != null) {
			try {
				content = new String(nodeContent, encoding);
			} catch (final UnsupportedEncodingException e) {
				content = new String(nodeContent);
			}
		}
		else {
			content = new String(nodeContent);
		}

		return content;
	}

	/**
	 * Convierte un nodo a texto.
	 * @param node Nodo que se desea obtener como texto.
	 * @param encoding Codificaci&oacute;n del texto (deber&iacute;a coincidir
	 * con la del XML al que pertenece el nodo).
	 * @return Nodo en forma de texto.
	 */
	private static ContentDelimited getContent(final StringBuilder text, final NodeDelimiter delimiter) {
		final String openTag = delimiter.getOpenTag();
		final String closeTag = delimiter.getCloseTag();
		final String nodeName = delimiter.getNodeName();

		final int openTagIdx = text.indexOf(openTag);
		final int contentIdx = openTagIdx + openTag.length();
		int closeTagIdx = text.indexOf(closeTag, contentIdx);

		// Nos aseguramos de que no hay ningun nodo intermedio que nos lleve
		// a tomar el tag de cierre equivocado
		int intElementIdx = findOpenTag(text, nodeName, contentIdx);
		while (intElementIdx > -1 && intElementIdx < closeTagIdx) {
			// Buscamos el siguiente nodo de cierre
			closeTagIdx = text.indexOf(closeTag, closeTagIdx + closeTag.length());
			// Buscamos nuevos nodos de inicio antes de ese nodo de cierre
			intElementIdx = findOpenTag(text, nodeName, intElementIdx + nodeName.length());
		}

		final String content = text.substring(contentIdx, closeTagIdx);

		return new ContentDelimited(content, openTagIdx + openTag.length());
	}

	/**
	 * Obtiene la posici&oacute;n en un texto de un nodo.
	 * @param text Texto en el que buscar.
	 * @param nodeName Nombre del nodo.
	 * @param startIdx Posicion desde la que empezar la busqueda.
	 * @return Posici&oacute;n del nodo o -1 si no se encuentra.
	 */
	private static int findOpenTag(final StringBuilder text, final String nodeName, final int startIdx) {

		int offsetIdx = startIdx;
		char c;
		int idx;
		do {
			idx = text.indexOf("<" + nodeName, offsetIdx); //$NON-NLS-1$
			offsetIdx = idx + nodeName.length() + 1;
			c = idx != -1 ? text.charAt(offsetIdx) : ' ';
		}
		while (idx != -1 && !Character.isSpaceChar(c) && c != '>');

		return idx;
	}

	/**
	 * Obtiene las etiquetas que de inicio y cierre del fragmento de XML proporcionado.
	 * @param nodeContent Fragmento de XML.
	 * @return Etiquetas de apertura y cierre del XML.
	 */
	private static NodeDelimiter getDelimiterTags(final String nodeContent) {
		return getFirstTagPair(cleanNode(nodeContent));
	}

	/** Construye una lista con la etiqueta XML del nodo por el que empieza el texto XML y la
	 * etiqueta del nodo XML con la que termina.
	 * @param xml Texto XML.
	 * @return Delimitador con la primera y &uacute;ltima etiqueta del XML. */
	private static NodeDelimiter getFirstTagPair(final String xml) {
		if (xml == null) {
			throw new IllegalArgumentException("La entrada no puede ser nula"); //$NON-NLS-1$
		}
		if (!xml.contains("<") || !xml.contains(">")) { //$NON-NLS-1$ //$NON-NLS-2$
			throw new IllegalArgumentException("La entrada no tiene ninguna etiqueta XML"); //$NON-NLS-1$return xml;
		}

		final String openTag = xml.substring(0, xml.indexOf(">") + 1).trim(); //$NON-NLS-1$


		int idx = 0;
		char c;
		do {
			c = openTag.charAt(++idx);
		}
		while (c != '>' && !Character.isSpaceChar(c));
		final String nodeName = openTag.substring(1, idx);

		final String closeTag = xml.substring(xml.lastIndexOf("<")).trim(); //$NON-NLS-1$
		return new NodeDelimiter(nodeName, openTag, closeTag);
	}

	private static Document getDocumentFromBytes(final byte[] data) throws SAXException,
	                                                                       IOException,
	                                                                       ParserConfigurationException {
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        return dbf.newDocumentBuilder().parse(
    		new ByteArrayInputStream(data)
		);
	}

	/** Devuelve las URI de todas las referencias de los nodos "Signature" que no apunten a
	 * "SignedProperties" ni tengan la transformaci&oacute;n enveloped.
	 * @param doc Documento de firma electr&oacute;nica.
	 * @return Listado de referencias. */
	private static List<String> getInmutableReferences(final Document doc) {

		final Element signDoc = doc.getDocumentElement();

		final List<Node> signatureNodes = new ArrayList<>();

		// Comprobamos si el nodo principal es de firma
		if (signDoc.getLocalName().equals("Signature")) { //$NON-NLS-1$
			signatureNodes.add(signDoc);
		}

        // Obtenemos las firmas internas del documento
        final NodeList nl = signDoc.getElementsByTagNameNS(DS_NAMESPACE_URL, "Signature"); //$NON-NLS-1$
        for (int i = 0; i < nl.getLength(); i++) {
        	signatureNodes.add(nl.item(i));
        }

        // Por cada firma buscamos sus referencias
        final List<String> unmutableReferences = new ArrayList<>();
        for (final Node sigs : signatureNodes) {
        	// Buscamos la referencias solo dentro del SignedInfo para evitar problemas con las
        	// referencias de los manifest. Si no encontramos el elemento, omitimos las
        	// referencias de la firma
        	final Element signedInfo = XAdESUtil.getSignedInfo((Element) sigs);
        	if (signedInfo == null) {
        		continue;
        	}

        	final NodeList rf = signedInfo.getElementsByTagNameNS(DS_NAMESPACE_URL, "Reference"); //$NON-NLS-1$
        	for (int j = 0; j < rf.getLength(); j++) {
        		final Element refElement = (Element) rf.item(j);
        		if (!isReferenceToSignedProperties(refElement) && !isEnvelopedReference(refElement)) {
        			final String uri = refElement.getAttribute("URI"); //$NON-NLS-1$
        			if (uri != null && !unmutableReferences.contains(uri)) {
        				unmutableReferences.add(uri);
        			}
        		}
        	}
        }

        return unmutableReferences;
	}

	/**
	 * Indica si la referencia apunta a un elemento SignedProperties.
	 * @param refElement Referencia de la firma.
	 * @return {@code true} si la referencia apunta a un elemento SignedProperties,
	 * {@code false} en caso contrario.
	 */
	private static boolean isReferenceToSignedProperties(final Element refElement) {
		final String type = refElement.getAttribute("Type"); //$NON-NLS-1$
		return type != null && type.endsWith(SIGNED_PROPERTIES_TYPE_SUFIX);
	}

	/**
	 * Identifica si una referencia declara una transformaci&oacute;n envelveloped.
	 * @param element Elemento de la referencia.
	 * @return {@code true} si la referencia declarara una transformaci&oacute;n
	 * enveloped, {@code false} en caso contrario.
	 */
	private static boolean isEnvelopedReference(final Element element) {
		boolean enveloped = false;
		final NodeList transforms = element.getElementsByTagNameNS(DS_NAMESPACE_URL, "Transform"); //$NON-NLS-1$
		for (int k = 0; !enveloped && k < transforms.getLength(); k++) {
			if (ENVELOPED_ALGORITHM_TRANSFORM.equals(
					((Element) transforms.item(k)).getAttribute("Algorithm"))) { //$NON-NLS-1$
				enveloped = true;
			}
		}
		return enveloped;
	}

	/** Elimina de la cadena de texto proporcionada la cabecera de XML si empezaba
	 * por ella y la declaracion de espacios de nombres.
	 * @param xml Texto XML.
	 * @return Texto XML sin cabecera ni declaracion de espacios de nombre. */
	private static String cleanNode(final String xml) {
		if (xml == null) {
			throw new IllegalArgumentException("La entrada no puede ser nula"); //$NON-NLS-1$
		}
		String xmlCleaned = xml;
		if (xml.startsWith("<?xml")) { //$NON-NLS-1$
			xmlCleaned = xmlCleaned.substring(xmlCleaned.indexOf("?>") + "?>".length()).trim(); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// Buscamos y eliminamos las declaraciones de espacios de nombres
		int n1 = 0;
		while ((n1 = xmlCleaned.indexOf(" xmlns", n1)) != -1) { //$NON-NLS-1$

			// Identificamos si la cadena del espacio de nombres se define con comillas dobles o simples
			int n2;
			char sepChar = '\0';
			for (n2 = n1 + " xmlns".length() + 1; sepChar == '\0' && n2 < xmlCleaned.length(); n2++) { //$NON-NLS-1$
				if (xmlCleaned.charAt(n2) == '\"') {
					sepChar = '\"';
				} else if (xmlCleaned.charAt(n2) == '\'') {
					sepChar = '\'';
				}
			}

			// Identificamos donde termina la declaracion del espacio
			n2 = xmlCleaned.indexOf(sepChar, n2 + 1);

			// Componemos la cadena sin la declaracion del espacio
			xmlCleaned = xmlCleaned.substring(0, n1) + xmlCleaned.substring(n2 + 1);
		}
		return xmlCleaned;
	}

	/**
	 * Contenido delimitado dentro de una cadena. Indica cual es el contenido
	 * y donde empieza cada elemento.
	 */
	private static class ContentDelimited {
		final String content;
		final int startContentIdx;
		final int endDelimiterIdx;


		public ContentDelimited(final String content, final int startContentIdx) {
			this.content = content;
			this.startContentIdx = startContentIdx;
			this.endDelimiterIdx = this.startContentIdx + content.length();
		}

		public String getContent() {
			return this.content;
		}

		public int getStartContentIdx() {
			return this.startContentIdx;
		}

		public int getEndContentIdx() {
			return this.endDelimiterIdx;
		}

	}
}
