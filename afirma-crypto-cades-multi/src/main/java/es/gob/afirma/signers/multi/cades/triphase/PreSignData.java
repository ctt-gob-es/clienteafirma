package es.gob.afirma.signers.multi.cades.triphase;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import es.gob.afirma.core.misc.Base64;

/** Datos de prefirma trif&aacute;sica CAdES. */
public final class PreSignData {

	private Date signDate = null;

	private final List<SinglePreSignData> preSigns;

	private PreSignData(final long dateMilis) {
		this.signDate = new Date(dateMilis);
		this.preSigns = new ArrayList<PreSignData.SinglePreSignData>();
	}

	private void addCounterSigns(final SinglePreSignData cs) {
		this.preSigns.add(cs);
	}

	/** Obtiene la fecha de la firma.
	 * @return Fecha de la firma, que es est&aacute;tica y no var&iacute;a en todo el proceso. */
	public Date getSignDate() {
		return this.signDate;
	}

	/** Obtiene las prefirmas, que consisten en los datos a firmar como PKCS#1
	 * y los datos aleatorios que deben ser sustituidos por las firmas finales o la propia firma PKCS#1,
	 * seg&uacute;n la fase de la firma.
	 * @return Prefirmas. */
	public List<SinglePreSignData> getPreSigns() {
		return this.preSigns;
	}

	/** Obtiene la informaci&oacute;n de una operaci&oacute;n de firma CAdES trif&aacute;sica
	 * del XML en donde se define.
	 * @param xml XML con la informaci&oacute;n de la operaci&oacute;n trif&aacute;sica.
	 * @return Informaci&oacute;n de la operaci&oacute;n.
	 * @throws IOException Si hay problemas con el tratamiento de datos.
	 * @throws ParserConfigurationException Si no se puede obtener el XML apropiado.
	 * @throws SAXException Si hay problemas en el tratamiento del XML. */
	public static PreSignData getInstance(final byte[] xml) throws IOException,
	                                                               SAXException,
	                                                               ParserConfigurationException {

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

		final PreSignData csData = new PreSignData(
			Long.parseLong(
				csNode.getTextContent().trim()
			)
		);

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

	private static SinglePreSignData parseDcs(final Node dcsNode) throws IOException {

		final NodeList childNodes = dcsNode.getChildNodes();

		int idx = nextNodeElementIndex(childNodes, 0);
		if (idx == -1 || !"d".equalsIgnoreCase(childNodes.item(idx).getNodeName())) { //$NON-NLS-1$
			throw new IllegalArgumentException("No se encontro el nodo 'd' en el XML proporcionado"); //$NON-NLS-1$
		}
		final Node dNode = childNodes.item(idx);

		idx = nextNodeElementIndex(childNodes, idx + 1);
		if (idx == -1 || !"dd".equalsIgnoreCase(childNodes.item(idx).getNodeName())) { //$NON-NLS-1$
			throw new IllegalArgumentException("No se encontro el nodo 'dd' en el XML proporcionado"); //$NON-NLS-1$
		}
		final Node ddNode = childNodes.item(idx);

		return new SinglePreSignData(
				Base64.decode(dNode.getTextContent().trim()),
				Base64.decode(ddNode.getTextContent().trim()));
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

	/** Informaci&oacute;n de una prefirma concreta (datos a firmar con PKCS#1
	 * y valores aletorios que hay que sustituir por las firmas finales o la propia firma PKCS#1,
	 * dependiendo de la fase de la firma. */
	public static final class SinglePreSignData {

		private final byte[] data;
		private final byte[] dummyData;

		SinglePreSignData(final byte[] d, final byte[] dd) {
			this.data = d.clone();
			this.dummyData = dd.clone();
		}

		/** Obtiene los datos que deben firmarse con PKCS#1.
		 * @return Datos que deben firmarse con PKCS#1. */
		public byte[] getData() {
			return this.data.clone();
		}

		/** Obtiene los datos aletorios que deben sustituirse por las firmas finales o las firmas
		 * PKCS#1 finales, seg&uacute;n la fase de la firma.
		 * @return Datos aletorios que deben sustituirse por las firmas finales o la propia firma PKCS#1. */
		public byte[] getDummyData() {
			return this.dummyData.clone();
		}
	}

}
