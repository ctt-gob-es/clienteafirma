
package es.gob.afirma.signers.cadestri.client;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import es.gob.afirma.core.misc.Base64;

/** Datos de contrafirma trif&aacute;sica. */
public final class CounterSignData {

	private String date = null;

	private final List<SingleCounterSignData> counterSigns;

	private CounterSignData(final String date) {
		this.date = date;
		this.counterSigns = new ArrayList<CounterSignData.SingleCounterSignData>();
	}

	private void addCounterSigns(final SingleCounterSignData cs) {
		this.counterSigns.add(cs);
	}

	/** Obtiene el momento de la contrafirma.
	 * @return Fecha (momento) de la contrafirma. */
	public String getDate() {
		return this.date;
	}

	/** Obtiene los datos de todas las contrafirmas realizadas en la misma operaci&oacute;n.
	 * @return Datos de todas las contrafirmas realizadas en la misma operaci&oacute;n. */
	public List<SingleCounterSignData> getCounterSigns() {
		return this.counterSigns;
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder()
		.append("<xml>\n") //$NON-NLS-1$
		.append(" <cs>\n") //$NON-NLS-1$
		.append("  ") //$NON-NLS-1$
		.append(this.date)
		.append('\n')
		.append(" </cs>\n") //$NON-NLS-1$
		.append(" <css>\n"); //$NON-NLS-1$

		for (final SingleCounterSignData cs : this.counterSigns) {
			sb.append("  <dcs>\n") //$NON-NLS-1$
			.append("   <d>\n") //$NON-NLS-1$
			.append("    ") //$NON-NLS-1$
			.append(Base64.encode(cs.getData()))
			.append('\n')
			.append("   </d>\n") //$NON-NLS-1$
			.append("   <dd>\n") //$NON-NLS-1$
			.append("    ") //$NON-NLS-1$
			.append(Base64.encode(cs.getDummyData()))
			.append('\n')
			.append("   </dd>\n") //$NON-NLS-1$
			.append("  </dcs>\n"); //$NON-NLS-1$
		}

		sb.append(" </css>\n") //$NON-NLS-1$
		.append("</xml>"); //$NON-NLS-1$
		return sb.toString();
	}

	/** Obtiene la informaci&oacute;n de una operaci&oacute;n de contrafirma trif&aacute;sica
	 * del XML en donde se define.
	 * @param xml XML con la informaci&oacute;n de la operaci&oacute;n trif&aacute;sica.
	 * @return Informaci&oacute;n de la operaci&oacute;n.
	 * @throws ParserConfigurationException Cuando no puede crearse el parser para el XML.
	 * @throws IOException Cuando ocurre alg&uacute;n error en la lectura de los datos.
	 * @throws SAXException Cuando el XML est&aacute; mal formado. */
	public static CounterSignData parse(final byte[] xml) throws SAXException, IOException, ParserConfigurationException {

		final ByteArrayInputStream bais = new ByteArrayInputStream(xml);
		final Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(bais);
		bais.close();

		final NodeList childNodes = doc.getDocumentElement().getChildNodes();

		int idx;
		idx = nextNodeElementIndex(childNodes, 0);
		if (idx == -1 || !"cs".equalsIgnoreCase(childNodes.item(idx).getNodeName())) { //$NON-NLS-1$
			throw new IllegalArgumentException("No se encontro el nodo 'cs' en el XML proporcionado"); //$NON-NLS-1$
		}
		final Node csNode = childNodes.item(idx);

		final CounterSignData csData = new CounterSignData(csNode.getTextContent().trim());

		idx = nextNodeElementIndex(childNodes, idx + 1);
		if (idx == -1 || !"css".equalsIgnoreCase(childNodes.item(idx).getNodeName())) { //$NON-NLS-1$
			throw new IllegalArgumentException("No se encontro el nodo 'css' en el XML proporcionado"); //$NON-NLS-1$
		}
		final NodeList cssNodes = childNodes.item(idx).getChildNodes();

		idx = -1;
		while ((idx = nextNodeElementIndex(cssNodes, idx + 1)) != -1) {
			csData.addCounterSigns(parseDcs(cssNodes.item(idx)));
		}

		return csData;
	}

	private static SingleCounterSignData parseDcs(final Node dcsNode) throws IOException {

		int idx;
		final NodeList childNodes = dcsNode.getChildNodes();

		idx = nextNodeElementIndex(childNodes, 0);
		if (idx == -1 || !"d".equalsIgnoreCase(childNodes.item(idx).getNodeName())) { //$NON-NLS-1$
			throw new IllegalArgumentException("No se encontro el nodo 'd' en el XML proporcionado"); //$NON-NLS-1$
		}
		final Node dNode = childNodes.item(idx);

		idx = nextNodeElementIndex(childNodes, idx + 1);
		if (idx == -1 || !"dd".equalsIgnoreCase(childNodes.item(idx).getNodeName())) { //$NON-NLS-1$
			throw new IllegalArgumentException("No se encontro el nodo 'dd' en el XML proporcionado"); //$NON-NLS-1$
		}
		final Node ddNode = childNodes.item(idx);

		return new SingleCounterSignData(
				Base64.decode(dNode.getTextContent().trim()),
				Base64.decode(ddNode.getTextContent().trim()));
	}

	/** Recupera el &iacute;ndice siguiente nodo de la lista de tipo Element. Empieza a comprobar
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

	/** Almacena la informacion de una contrafirma concreta (pkcs#1 de la contrafirma
	 * y valor dummy que lo sustituye). */
	public static final class SingleCounterSignData {

		private byte[] data;
		private final byte[] dummyData;

		/** Crea los datos de la contrafirma concreta.
		 * @param d Datos a firmar o firma real de los datos (seg&uacute;n la fase en la que se encuentre el proceso trif&aacute;sico).
		 * @param dd Datos a sustituir por la firma real de los datos. */
		public SingleCounterSignData(final byte[] d, final byte[] dd) {
			this.data = d.clone();
			this.dummyData = dd.clone();
		}

		/** Obtiene los datos a firmar o firma real de los datos (seg&uacute;n la fase en la que se encuentre el proceso trif&aacute;sico).
		 * @return Datos a firmar o firma real de los datos (seg&uacute;n la fase en la que se encuentre el proceso trif&aacute;sico). */
		public byte[] getData() {
			return this.data;
		}

		/** Obtiene los datos a sustituir por la firma real de los datos.
		 * @return Datos a sustituir por la firma real de los datos. */
		public byte[] getDummyData() {
			return this.dummyData;
		}

		/** Establece los datos a firmar o firma real de los datos (seg&uacute;n la fase en la que se encuentre el proceso trif&aacute;sico).
		 * @param data Datos a firmar o firma real de los datos (seg&uacute;n la fase en la que se encuentre el proceso trif&aacute;sico). */
		public void setData(final byte[] data) {
			this.data = data.clone();
		}
	}
}
