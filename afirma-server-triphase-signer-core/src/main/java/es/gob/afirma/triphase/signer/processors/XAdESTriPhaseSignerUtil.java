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
import java.util.ArrayList;
import java.util.Arrays;
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
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.dereference.CustomUriDereferencer;

final class XAdESTriPhaseSignerUtil {

	private XAdESTriPhaseSignerUtil() {
		// No instanciable
	}

	private static final String DS_NAMESPACE_URL = "http://www.w3.org/2000/09/xmldsig#"; //$NON-NLS-1$
	private static final String SA_NAMESPACE_URL = "http://uri.etsi.org/01903#SignedProperties"; //$NON-NLS-1$

	private static final String USE_MANIFEST = "useManifest"; //$NON-NLS-1$

	private static final String EXTRA_PARAM_FORMAT = "format"; //$NON-NLS-1$

	private static final String REPLACEMENT_TEMPLATE = "%%%%REPLACE-%1$d%%%%"; //$NON-NLS-1$

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

		final Document docBase = getDocumentFromBytes(xmlBase);
		final List<List<String>> elDeliBase = getCommonContentDelimiters(
			getInmutableReferences(
				docBase
			),
			docBase
		);

		final Document docSource = getDocumentFromBytes(xmlSource);
		final List<List<String>> elDeliSource = getCommonContentDelimiters(
			getInmutableReferences(
				docSource
			),
			docSource
		);

		if (elDeliBase.size() != elDeliSource.size()) {
			throw new IllegalArgumentException(
				"El documento base no tiene las mismas partes comunes que el documento fuente" //$NON-NLS-1$
			);
		}

		String base = new String(xmlBase, docBase.getXmlEncoding());

		final String source = new String(xmlSource, docSource.getXmlEncoding());

		for (int i=0; i<elDeliBase.size(); i++) {
			base = base.replace(
				String.format(REPLACEMENT_TEMPLATE, Integer.valueOf(i)),
				source.substring(
					source.indexOf(elDeliSource.get(i).get(0)) + elDeliSource.get(i).get(0).length(),
					source.indexOf(elDeliSource.get(i).get(1), source.indexOf(elDeliSource.get(i).get(0)) + elDeliSource.get(i).get(0).length())
				)
			);
		}
		return base.getBytes(docBase.getXmlEncoding());
	}

	/** Elimina de una firma XML el contenido de los nodos referenciados por la propia firma.
	 * En caso de tratarse una firma de Manifest, no hace nada.<br>
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

		String ret = null;
		if (doc.getXmlEncoding() != null) {
			try {
				ret = new String(xml, doc.getXmlEncoding());
			}
			catch (final UnsupportedEncodingException e) {
				LOGGER.warning("Error en la codificacion declarada por el XML (" + doc.getXmlEncoding() + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		if (ret == null) {
			ret = new String(xml);
		}

		final List<List<String>> delits = CleanContentDelimiters(
			XAdESTriPhaseSignerUtil.getCommonContentDelimiters(
				XAdESTriPhaseSignerUtil.getInmutableReferences(doc),
				doc
			),
			ret
		);

		for (int i = 0; i < delits.size(); i++) {
			final List<String> delPair = delits.get(i);
			ret = ret.replace(
				ret.substring(
					ret.indexOf(delPair.get(0)) + delPair.get(0).length(),
					ret.indexOf(delPair.get(1), ret.indexOf(delPair.get(0)) + delPair.get(0).length())
				),
				String.format(REPLACEMENT_TEMPLATE, Integer.valueOf(i))
			);
		}

		if (doc.getXmlEncoding() != null) {
			try {
				return ret.getBytes(doc.getXmlEncoding());
			}
			catch (final UnsupportedEncodingException e) {
				LOGGER.warning(
					"Error en la codificacion declarada por el XML ('" + doc.getXmlEncoding() + "'): " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}

		return ret.getBytes();
	}

	/** Limpia la lista de delimitadores de nodos.
	 * Los calculados en base al API XML pueden no corresponder con los reales en el texto
	 * XML por la inclusi&oacute;n de espacios de nombres, por lo que es necesario revisarla
	 * y corregir las discordancias.
	 * @param or Lista original de delimitadores de nodos
	 * @param orXml XML en su forma de texto.
	 * @return Lista de delimitadores de nodos con los delimitadores correctos. */
	private static List<List<String>> CleanContentDelimiters(final List<List<String>> or, final String orXml) {
		final List<List<String>> ret = new ArrayList<>(or.size());
		for (final List<String> del : or) {
			// Las discordancias siempre estan en la apertura del nodo
			final String orDel = del.get(0);
			if (orXml.contains(orDel)) {
				ret.add(del);
			}
			else {

				// Obtenemos el texto de inicio del nodo
				final String nodeStart = orDel.substring(
					0,
					orDel.indexOf(' ')
				);

				// Obtenemos todo los indices de ocurrencias del texto de inicio del nodo, por
				// si hay varias ocurrencias
				final List<Integer> indexes = new ArrayList<>();
				for (int index = orXml.indexOf(nodeStart); index >= 0; index = orXml.indexOf(nodeStart, index + 1)) {
				    indexes.add(Integer.valueOf(index));
				}

				String retDel = null;
				for (final Integer beginIndex : indexes) {
					retDel = orXml.substring(
						beginIndex.intValue(),
						orXml.indexOf('>', beginIndex.intValue()) + 1
					);

					// En este punto 'retDel' es un candidato, comprobamos que el Id sea el mismo
					if (retDel.contains(ID_STR) && retDel.contains(ID_STR)) {
						final String id = orXml.substring(
							orXml.indexOf(ID_STR, beginIndex.intValue()),
							orXml.indexOf('"',  orXml.indexOf(ID_STR, beginIndex.intValue()) + ID_STR.length())
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
					ret.add(Arrays.asList(new String[] { retDel, del.get(1) }));
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
	private static List<List<String>> getCommonContentDelimiters(final List<String> uris,
			                                                     final Document doc) {
		final String encoding = doc.getInputEncoding();
		final List<List<String>> ret = new ArrayList<>();
		for (final String uriValue : uris) {
			final Node node = CustomUriDereferencer.getNodeByInternalUriReference(uriValue, doc);
			if (node != null) {
				if (encoding != null) {
					try {
						ret.add(
							getFirstTagPair(
								removeXmlHeader(
									new String(
										Utils.writeXML(node, null, null, null),
										encoding
									)
								)
							)
						);
					}
					catch (final UnsupportedEncodingException e) {
						ret.add(
							getFirstTagPair(
								removeXmlHeader(
									new String(Utils.writeXML(node, null, null, null))
								)
							)
						);
					}
				}
				else {
					ret.add(
						getFirstTagPair(
							removeXmlHeader(
								new String(Utils.writeXML(node, null, null, null))
							)
						)
					);
				}
			}
		}
		return ret;
	}

	private static Document getDocumentFromBytes(final byte[] data) throws SAXException,
	                                                               IOException,
	                                                               ParserConfigurationException {
		final String xml = new String(data);
		System.out.println(xml);
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        return dbf.newDocumentBuilder().parse(
    		new ByteArrayInputStream(data)
		);
	}

	/** Devuelve todas las referencias, que no apunten a "SignedProperties", de
	 * los nodos "Signature" con los identificadores indicados.
	 * @param doc Documento de firma electr&oacute;nica.
	 * @return Listado de referencias. */
	private static List<String> getInmutableReferences(final Document doc) {

		final Element signDoc = doc.getDocumentElement();

		final List<Node> signatureNodes = new ArrayList<>();
		if (signDoc.getNodeName().equals("Signature") || signDoc.getNodeName().endsWith(":Signature")) { //$NON-NLS-1$ //$NON-NLS-2$
			signatureNodes.add(signDoc);
		}
        // Obtenemos las firmas del documento
        final NodeList nl = signDoc.getElementsByTagNameNS(DS_NAMESPACE_URL, "Signature"); //$NON-NLS-1$
        for(int i = 0; i < nl.getLength(); i++) {
        	signatureNodes.add(nl.item(i));
        }

        // Por cada firma buscamos sus referencias
        final List<String> unmutableReferences = new ArrayList<>();
        for(final Node sigs : signatureNodes) {
        	//final NodeList rf = ((Element) sigs).getElementsByTagNameNS(DS_NAMESPACE_URL, "Reference"); //$NON-NLS-1$
        	final NodeList rf = ((Element) sigs).getElementsByTagName("ds:Reference"); //$NON-NLS-1$
        	for(int j = 0; j < rf.getLength(); j++) {
        		final Node node = rf.item(j);
        		if (!SA_NAMESPACE_URL.equals(((Element) node).getAttribute("Type"))) { //$NON-NLS-1$
        			final String uri = ((Element) node).getAttribute("URI"); //$NON-NLS-1$
        			if (uri != null) {
        				unmutableReferences.add(uri);
        			}
        		}
        	}
        }

        return unmutableReferences;
	}

	/** Elimina de la cadena de texto proporcionada la cabecera de XML si empezaba
	 * por ella.
	 * @param xml Texto XML.
	 * @return Texto XML sin cabecera. */
	private static String removeXmlHeader(final String xml) {
		if (xml == null) {
			throw new IllegalArgumentException("La entrada no puede ser nula"); //$NON-NLS-1$
		}
		if (!xml.startsWith("<?xml")) { //$NON-NLS-1$
			return xml;
		}
		return xml.substring(xml.indexOf("?>") + "?>".length(), xml.length()).trim(); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Construye una lista con la etiqueta XML del nodo por el que empieza el texto XML y la
	 * etiqueta del nodo XML con la que termina.
	 * @param xml Texto XML.
	 * @return Listado de 2 elementos: la primera y &uacute;ltima etiqueta del XML. */
	private static List<String> getFirstTagPair(final String xml) {
		if (xml == null) {
			throw new IllegalArgumentException("La entrada no puede ser nula"); //$NON-NLS-1$
		}
		if (!xml.contains("<") || !xml.contains(">")) { //$NON-NLS-1$ //$NON-NLS-2$
			throw new IllegalArgumentException("La entrada no tiene ninguna etiqueta XML"); //$NON-NLS-1$return xml;
		}
		final List<String> ret = new ArrayList<>(2);
		ret.add(xml.substring(0, xml.indexOf(">") + 1).trim()); //$NON-NLS-1$
		ret.add(xml.substring(xml.lastIndexOf("<")).trim()); //$NON-NLS-1$
		return ret;
	}
}
