/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.SecureXmlBuilder;

/** Mensaje con la informaci&oacute;n requerida para la ejecuci&oacute;n de una
 * operaci&oacute;n trif&aacute;sica. */
public final class TriphaseData {

	/** Datos de una firma trif&aacute;sica individual.
	 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
	public static final class TriSign {

		private final Map<String,String> dict;
		private final String id;
		private final String signatureId;

		/**
		 * Constructor de copia. Crea una firma trif&aacute;sica individual a partir
		 * de otra, de forma completamente inmutable.
		 * @param ts Firma trif&aacute;sica original.
		 */
		public TriSign(final TriSign ts) {
			this.id = ts.getId();
			this.signatureId = ts.getSignatureId();
			this.dict = new ConcurrentHashMap<>(ts.getDict().size());
			final Set<String> keys = ts.getDict().keySet();
			for (final String key : keys) {
				this.dict.put(key, ts.getProperty(key));
			}
		}

		/** Crea los datos de una firma trif&aacute;sica individual.
		 * @param d Propiedades de la firma.
		 * @param i Identificador de la firma. Si es {@code null} se genera uno aleatorio. */
		public TriSign(final Map<String, String> d, final String i) {
			this(d, i, null);
		}

		/**
		 * Crea los datos de una firma trif&aacute;sica individual.
		 * @param d Propiedades de la firma.
		 * @param i Identificador de la firma individual. Si es {@code null} se genera uno aleatorio.
		 * @param signatureId Identificador de la firma global. S&oacute;lo es necesario si varias
		 * firmas individuales pertenecen a la misma firma y es relevante.
		 */
		public TriSign(final Map<String, String> d, final String i, final String signatureId) {
			if (d == null) {
				throw new IllegalArgumentException(
					"El diccionario de propiedades de la firma no puede ser nulo" //$NON-NLS-1$
				);
			}
			this.dict = d;
			this.id = i != null ? i : UUID.randomUUID().toString();
			this.signatureId = signatureId;
		}

		@Override
		public String toString() {
			return "Firma trifasica individual con identificador " + getId(); //$NON-NLS-1$
		}

		/**
		 * Obtiene el identificador individual de la firma.
		 * @return Identificador individual de la firma.
		 */
		public String getId() {
			return this.id;
		}

		/**
		 * Obtiene el identificador de la firma.
		 * @return Identificador de la firma.
		 */
		public String getSignatureId() {
			return this.signatureId;
		}

		/** Obtiene una propiedad de la firma.
		 * @param key Nombre de la propiedad.
		 * @return Propiedad de la firma con el nombre indicado, o <code>null</code> si no hay
		 *         ninguna propiedad con ese nombre. */
		public String getProperty(final String key) {
			return this.dict.get(key);
		}

		/** A&ntilde;ade una nueva propiedad a la firma.
		 * @param key Nombre de la nueva propiedad.
		 * @param value Valor de la nueva propiedad. */
		public void addProperty(final String key, final String value) {
			this.dict.put(key, value);
		}

		/** Elimina una propiedad de la firma.
		 * @param key Nombre de la propiedad a eliminar. */
		public void deleteProperty(final String key) {
			this.dict.remove(key);
		}

		/**
		 * Recupera una copia de los par&aacute;metros de la firma trif&aacute;sica.
		 * @return Copia de los par&aacute;metros.
		 */
		public Map<String,String> getDict() {
			return new ConcurrentHashMap<>(this.dict);
		}
	}

	private final List<TriSign> signs;
	private String format;

	/**
	 * Obtiene el formato de las firmas.
	 * @return Formato de la firma.
	 */
	public String getFormat() {
		return this.format;
	}

	/**
	 * Establece el formato de las firmas.
	 * @param fmt Formato de las firmas.
	 */
	public void setFormat(final String fmt) {
		this.format = fmt;
	}

	/**
	 * Devuelve una firma individual con el identificador dado o <code>null</code> si no hay ninguna
	 * firma con ese identificador.
	 * Si hubiese varias firmas con el mismo identificador se devuleve el primero en encontrarse.
	 * @param signId Identificador de la firma.
	 * @return Firma individual con el identificador dado.
	 */
	public TriSign getTriSign(final String signId) {
		if (signId == null) {
			throw new IllegalArgumentException("El ID de la firma no puede ser nulo"); //$NON-NLS-1$
		}
		for (final TriSign ts : this.signs) {
			if (belongToSignature(signId, ts)) {
				return ts;
			}
		}
		return null;
	}

	/** Devuelve un listado de firmas con el identificador dado o <code>null</code> si no hay ninguna
	 * firma con ese identificador. El listado de firmas obtenidas no es mutable.
	 * @param signId Identificador de la firma.
	 * @return Listado de firmas con el identificador dado. */
	public List<TriSign> getTriSigns(final String signId) {
		if (signId == null) {
			throw new IllegalArgumentException("El ID de la firma no puede ser nulo"); //$NON-NLS-1$
		}
		final List<TriSign> tsl = new ArrayList<>();
		for (final TriSign ts : this.signs) {
			if (belongToSignature(signId, ts)) {
				tsl.add(new TriSign(ts));
			}
		}

		if (tsl.size() == 0) {
			return null;
		}

		return tsl;
	}

	/**
	 * Comprueba si unos datos trif&aacute;sicos pertenecen a una firma con un ID dado.
	 * @param signId Identificador de la firma.
	 * @param ts Datos trif&aacute;sicos.
	 * @return {@code true} si pertenecen a la firma, {@code false} en caso contrario.
	 */
	private static boolean belongToSignature(final String signId, final TriSign ts) {
		// El identificadordebe ser el de la firma trifasica general o el de la particular
		return signId.equals(ts.getSignatureId()) || signId.equals(ts.getId());
	}

	/** Obtiene todas las firmas de la sesi&oacute;n.
	 * @return Lista con todas las firmas de la sesi&oacute;n. */
	public List<TriSign> getTriSigns() {
		return this.signs;
	}

	/** Construye unos datos de sesi&oacute;n trif&aacute;sica vac&iacute;os. */
	public TriphaseData() {
		this.signs = new ArrayList<>();
		this.format = null;
	}

	/** Construye unos datos de sesi&oacute;n trif&aacute;sica indicando una lista de
	 * configuraci&oacute;n de firmas individuales.
	 * @param signs Lista de firmas individuales.. */
	public TriphaseData(final List<TriSign> signs) {
		this(signs, null);
	}

	/** Construye unos datos de sesi&oacute;n trif&aacute;sica indicando una lista de
	 * configuraci&oacute;n de firmas individuales.
	 * @param signs Lista de firmas individuales..
	 * @param fmt Formato de las firmas. */
	public TriphaseData(final List<TriSign> signs, final String fmt) {
		this.signs = signs;
		this.format = fmt;
	}

	/** Agrega la configuraci&oacute;n para una nueva operaci&oacute;n trif&aacute;sica.
	 * @param config Configuraci&oacute;n de la operaci&oacute;n trif&aacute;sica. */
	public void addSignOperation(final TriSign config) {
		this.signs.add(config);
	}

	/** Recupera los datos de una operaci&oacute;n de firma.
	 * @param idx Posici&oacute;n de los datos de firma a recuperar.
	 * @return Datos de firma. */
	public TriSign getSign(final int idx) {
		// Devolvemos la referencia real porque queremos permitir que se modifique
		return this.signs.get(idx);
	}

	/** Indica el n&uacute;mero de operaciones de firma que hay registradas.
	 * @return N&uacute;mero de firmas. */
	public int getSignsCount() {
		return this.signs.size();
	}

	/** Obtiene una sesi&oacute;n de firma trif&aacute;sica a partir de un XML que lo describe.
	 * Un ejemplo de XML podr&iacute;a ser el siguiente:
	 * <pre>
	 * &lt;xml&gt;
	 *  &lt;firmas format="XAdES"&gt;
	 *   &lt;firma Id="001"&gt;
	 *    &lt;param n="NEED_PRE"&gt;true&lt;/param&gt;
	 *    &lt;param n="PRE"&gt;MYICXDAYBgkqhkiG9[...]w0BA=&lt;/param&gt;
	 *    &lt;param n="NEED_DATA"&gt;true&lt;/param&gt;
	 *    &lt;param n="PK1"&gt;EMijB9pJ0lj27Xqov[...]RnCM=&lt;/param&gt;
	 *   &lt;/firma&gt;
	 *  &lt;/firmas&gt;
	 * &lt;/xml&gt;
	 * </pre>
	 * @param xml Texto XML con la informaci&oacute;n del mensaje.
	 * @return Mensaje de datos.
	 * @throws IOException Cuando hay problemas en el tratamiento de datos. */
	public static TriphaseData parser(final byte[] xml) throws IOException {

		if (xml == null) {
			throw new IllegalArgumentException("El XML de entrada no puede ser nulo"); //$NON-NLS-1$
		}

		Document doc;
		try (final InputStream is = new ByteArrayInputStream(xml);) {
			doc = SecureXmlBuilder.getSecureDocumentBuilder().parse(is);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al cargar el fichero XML: " + e + "\n" + LoggerUtil.getTrimBytes(xml)); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			throw new IOException("Error al cargar el fichero XML: " + e, e); //$NON-NLS-1$
		}

		return parser(doc.getDocumentElement());
	}

	/** Obtiene una sesi&oacute;n de firma trif&aacute;sica a partir del nodo
	 * XML que lo describe.
	 * Un ejemplo de XML podr&iacute;a ser el siguiente:
	 * <pre>
	 * &lt;xml&gt;
	 *  &lt;firmas format="XAdES"&gt;
	 *   &lt;firma Id="001"&gt;
	 *    &lt;param n="NEED_PRE"&gt;true&lt;/param&gt;
	 *    &lt;param n="PRE"&gt;MYICXDAYBgkqhkiG9[...]w0BA=&lt;/param&gt;
	 *    &lt;param n="NEED_DATA"&gt;true&lt;/param&gt;
	 *    &lt;param n="PK1"&gt;EMijB9pJ0lj27Xqov[...]RnCM=&lt;/param&gt;
	 *   &lt;/firma&gt;
	 *  &lt;/firmas&gt;
	 * &lt;/xml&gt;
	 * </pre>
	 * @param xml Elemento XML con la informaci&oacute;n del mensaje.
	 * @return Mensaje de datos.
	 * @throws IOException Cuando hay problemas en el tratamiento de datos. */
	public static TriphaseData parser(final Element xml) throws IOException {

		if (xml == null) {
			throw new IllegalArgumentException("El XML de entrada no puede ser nulo"); //$NON-NLS-1$
		}

		final NodeList childNodes = xml.getChildNodes();

		final int idx = nextNodeElementIndex(childNodes, 0);
		final Node rootSignsNode = childNodes.item(idx);

		if (idx == -1 || !"firmas".equalsIgnoreCase(rootSignsNode.getNodeName())) { //$NON-NLS-1$
			throw new IllegalArgumentException("No se encontro el nodo 'firmas' en el XML proporcionado"); //$NON-NLS-1$
		}

		String format = null;
		final NamedNodeMap nnm = rootSignsNode.getAttributes();
		if (nnm != null) {
			final Node tmpNode = nnm.getNamedItem("format"); //$NON-NLS-1$
			if (tmpNode != null) {
				format = tmpNode.getNodeValue();
			}
		}

		final List<TriSign> signsNodes = parseSignsNode(rootSignsNode);

		return new TriphaseData(signsNodes, format);
	}

	/** Analiza el nodo con el listado de firmas.
	 * @param signsNode Nodo con el listado de firmas.
	 * @return Listado con la informaci&oacute;n de cada operaci&oacute;n de firma. */
	private static List<TriSign> parseSignsNode(final Node signsNode) {

		final NodeList childNodes = signsNode.getChildNodes();

		final List<TriSign> signs = new ArrayList<>();
		int idx = nextNodeElementIndex(childNodes, 0);
		while (idx != -1) {
			final Node currentNode = childNodes.item(idx);

			String id = null;
			String signId = null;

			final NamedNodeMap nnm = currentNode.getAttributes();
			if (nnm != null) {
				Node tmpNode = nnm.getNamedItem("Id"); //$NON-NLS-1$
				if (tmpNode != null) {
					id = tmpNode.getNodeValue();
				}
				tmpNode = nnm.getNamedItem("signid"); //$NON-NLS-1$
				if (tmpNode != null) {
					signId = tmpNode.getNodeValue();
				}
			}
			signs.add(
				new TriSign(
					parseParamsListNode(currentNode),
					id,
					signId
				)
			);
			idx = nextNodeElementIndex(childNodes, idx + 1);
		}

		return signs;
	}

	/** Obtiene una lista de par&aacute;metros del XML.
	 * @param paramsNode Nodo con la lista de par&aacute;metros.
	 * @return Mapa con los par&aacute;metro encontrados y sus valores. */
	private static Map<String, String> parseParamsListNode(final Node paramsNode) {

		final NodeList childNodes = paramsNode.getChildNodes();

		final Map<String, String> params = new ConcurrentHashMap<>();
		int idx = nextNodeElementIndex(childNodes, 0);
		while (idx != -1) {
			final Node paramNode = childNodes.item(idx);
			final String key = paramNode.getAttributes().getNamedItem("n").getNodeValue(); //$NON-NLS-1$
			final String value = paramNode.getTextContent().trim();
			params.put(key, value);

			idx = nextNodeElementIndex(childNodes, idx + 1);
		}

		return params;
	}

	/** Recupera el &iacute;ndice del siguiente nodo de la lista de tipo <code>Element</code>.
	 * Empieza a comprobar los nodos a partir del &iacute;ndice marcado. Si no encuentra un
	 * nodo de tipo <i>elemento</i> devuelve -1.
	 * @param nodes Listado de nodos.
	 * @param currentIndex &Iacute;ndice del listado a partir del cual se empieza la comprobaci&oacute;n.
	 * @return &Iacute;ndice del siguiente node de tipo Element o -1 si no se encontr&oacute;. */
	private static int nextNodeElementIndex(final NodeList nodes, final int currentIndex) {
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

	/** Genera un XML con la descripci&oacute;n del mensaje trif&aacute;sico.
	 * @return XML con la descripci&oacute;n. */
	@Override
	public String toString() {

		final StringBuilder builder = new StringBuilder();
		builder.append("<xml>\n"); //$NON-NLS-1$
		builder.append(" <firmas"); //$NON-NLS-1$
		if (this.format != null) {
			builder.append(" format=\""); //$NON-NLS-1$
			builder.append(this.format);
			builder.append("\""); //$NON-NLS-1$
		}
		builder.append(">\n"); //$NON-NLS-1$
		final Iterator<TriSign> firmasIt = this.signs.iterator();
		while (firmasIt.hasNext()) {
			final TriSign signConfig = firmasIt.next();
			builder.append("  <firma"); //$NON-NLS-1$

			if (signConfig.getId() != null) {
				builder.append(" Id=\""); //$NON-NLS-1$
				builder.append(signConfig.getId());
				builder.append("\""); //$NON-NLS-1$
			}
			if (signConfig.getSignatureId() != null) {
				builder.append(" signid=\""); //$NON-NLS-1$
				builder.append(signConfig.getSignatureId());
				builder.append("\""); //$NON-NLS-1$
			}

			builder.append(">\n"); //$NON-NLS-1$
			final Iterator<String> firmaIt = signConfig.getDict().keySet().iterator();
			while (firmaIt.hasNext()) {
				final String p = firmaIt.next();
				builder.append("   <param n=\"") //$NON-NLS-1$
					.append(p)
						.append("\">") //$NON-NLS-1$
							.append(signConfig.getProperty(p))
								.append("</param>\n"); //$NON-NLS-1$
			}
			builder.append("  </firma>\n"); //$NON-NLS-1$
		}
		builder.append(" </firmas>\n"); //$NON-NLS-1$
		builder.append("</xml>"); //$NON-NLS-1$
		return builder.toString();
	}

}
