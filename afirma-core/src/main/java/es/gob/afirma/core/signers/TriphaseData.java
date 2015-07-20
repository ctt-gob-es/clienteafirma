package es.gob.afirma.core.signers;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/** Mensaje con la informaci&oacute;n requerida para la ejecuci&oacute;n de una
 * operaci&oacute;n trif&aacute;sica. */
public final class TriphaseData {

	/** Datos de una firma trif&aacute;sica individual.
	 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
	public static final class TriSign {

		private final Map<String,String> dict;
		private final String id;

		/** Crea los datos de una firma trif&aacute;sica individual.
		 * @param d Propiedades de la firma.
		 * @param i Identificador de la firma. */
		public TriSign(final Map<String,String> d, final String i) {
			if (d == null) {
				throw new IllegalArgumentException(
					"El diccionario de propiedades de la firma no puede ser nulo" //$NON-NLS-1$
				);
			}
			this.dict = d;
			this.id = i;
		}

		/** Obtiene el identificador de la firma.
		 * @return Identificador de la firma. */
		public String getId() {
			return this.id;
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

		Map<String,String> getDict() {
			return this.dict;
		}

	}

	private final List<TriSign> signs;

	/** Construye el mensaje especificando formato y operaci&oacute;n (firma, cofirma o contrafirma). */
	public TriphaseData() {
		this.signs = new ArrayList<TriSign>();
	}

	/** Construye el mensaje especificando la configuraci&oacute;n completa.
	 * @param signs Configuraci&oacute;n espec&iacute;fica. */
	private TriphaseData(final List<TriSign> signs) {
		this.signs = signs;
	}

	/** Agrega la configuracion para una nueva operaci&oacute;n trif&aacute;sica.
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
	 *  &lt;firmas&gt;
	 *   &lt;firma Id=\"001\"&gt;
	 *    &lt;param n="NEED_PRE"&gt;true&lt;/param&gt;
	 *    &lt;param n="PRE"&gt;MYICXDAYBgkqhkiG9[...]w0BA=&lt;/param&gt;
	 *    &lt;param n="NEED_DATA"&gt;true&lt;/param&gt;
	 *    &lt;param n="PK1"&gt;EMijB9pJ0lj27Xqov[---]RnCM=&lt;/param&gt;
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

		final InputStream is = new ByteArrayInputStream(xml);
		Document doc;
		try {
			doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(is);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al cargar el fichero XML: " + e + "\n" + new String(xml)); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			throw new IOException("Error al cargar el fichero XML: " + e, e); //$NON-NLS-1$
		}
		is.close();

		final Element rootElement = doc.getDocumentElement();
		final NodeList childNodes = rootElement.getChildNodes();

		final int idx = nextNodeElementIndex(childNodes, 0);
		if (idx == -1 || !"firmas".equalsIgnoreCase(childNodes.item(idx).getNodeName())) { //$NON-NLS-1$
			throw new IllegalArgumentException("No se encontro el nodo 'firmas' en el XML proporcionado"); //$NON-NLS-1$
		}

		final List<TriSign> signsNodes = parseSignsNode(childNodes.item(idx));

		return new TriphaseData(signsNodes);
	}

	/** Analiza el nodo con el listado de firmas.
	 * @param signsNode Nodo con el listado de firmas.
	 * @return Listado con la informaci&oacute;n de cada operaci&oacute;n de firma. */
	private static List<TriSign> parseSignsNode(final Node signsNode) {

		final NodeList childNodes = signsNode.getChildNodes();

		final List<TriSign> signs = new ArrayList<TriSign>();
		int idx = nextNodeElementIndex(childNodes, 0);
		while (idx != -1) {
			final Node currentNode = childNodes.item(idx);
			String id = null;
			final NamedNodeMap nnm = currentNode.getAttributes();
			if (nnm != null) {
				final Node idNode = nnm.getNamedItem("Id"); //$NON-NLS-1$
				if (idNode != null) {
					id = idNode.getNodeValue();
				}
			}
			signs.add(
				new TriSign(
					parseParamsListNode(currentNode),
					id
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

		final Map<String, String> params = new HashMap<String, String>();
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

	/** Recupera el &iacute;ndice siguiente nodo de la lista de tipo Element. Empieza a comprobar
	 * los nodos a partir del &iacute;ndice marcado. Si no encuentra un nodo de tipo elemento,
	 * devuelve -1.
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
		builder.append(" <firmas>\n"); //$NON-NLS-1$
		final Iterator<TriSign> firmasIt = this.signs.iterator();
		while (firmasIt.hasNext()) {
			final TriSign signConfig = firmasIt.next();
			builder.append("  <firma"); //$NON-NLS-1$
			if (signConfig.getId() != null) {
				builder.append(" Id=\""); //$NON-NLS-1$
				builder.append(signConfig.getId());
				builder.append("\""); //$NON-NLS-1$
			}
			builder.append(">\n"); //$NON-NLS-1$
			final Iterator<String> firmaIt = signConfig.getDict().keySet().iterator();
			while (firmaIt.hasNext()) {
				final String p = firmaIt.next();
				builder.append("   <param n=\"").append(p).append("\">").append(signConfig.getProperty(p)).append("</param>\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
			builder.append("  </firma>\n"); //$NON-NLS-1$
		}
		builder.append(" </firmas>\n"); //$NON-NLS-1$
		builder.append("</xml>"); //$NON-NLS-1$
		return builder.toString();
	}
}
