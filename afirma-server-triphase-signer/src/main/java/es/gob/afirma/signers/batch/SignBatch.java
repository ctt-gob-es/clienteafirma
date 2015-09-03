package es.gob.afirma.signers.batch;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.misc.http.DataDownloader;
import es.gob.afirma.core.signers.TriphaseData;

/** Lote de firmas electr&oacute;nicas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public abstract class SignBatch {

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	protected final List<SingleSign> signs;
	protected final SingleSignConstants.SignAlgorithm algorithm;

	private String id;
	String getId() {
		return this.id;
	}
	void setId(final String i) {
		if (i != null) {
			this.id = i;
		}
	}

	static {
		DataDownloader.enableFilesUsage(true);
	}

	protected long concurrentTimeout = Long.MAX_VALUE;

	/** Obtiene el algoritmo de firma.
	 * @return Algoritmo de firma. */
	public SingleSignConstants.SignAlgorithm getSignAlgorithm() {
		return this.algorithm;
	}

	protected boolean stopOnError = false;

	/** Ejecuta el preproceso de firma por lote.
	 * @param certChain Cadena de certificados del firmante.
	 * @return Datos trif&aacute;sicos de pre-firma del lote.
	 * @throws BatchException Si hay errores irrecuperables en el proceso. */
	public abstract String doPreBatch(final X509Certificate[] certChain) throws BatchException;

	/** Ejecuta el postproceso de firma por lote.
	 * @param certChain Cadena de certificados del firmante.
	 * @param td Datos trif&aacute;sicos del preproceso.
	 *           Debe contener los datos de todas y cada una de las firmas del lote.
	 * @return Registro del resultado general del proceso por lote, en un XML (<a href="../doc-files/resultlog-scheme.html">descripci&oacute;n
	 *         del formato</a>).
	 * @throws BatchException Si hay errores irrecuperables en el postproceso. */
	public abstract String doPostBatch(final X509Certificate[] certChain,
                                       final TriphaseData td) throws BatchException;

	/** Crea un lote de firmas a partir de su definici&oacute;n XML.
	 * @param xml XML de definici&oacute;n de lote de firmas (<a href="./doc-files/batch-scheme.html">descripci&oacute;n
	 *            del formato</a>).
	 * @throws IOException Si hay problemas en el tratamiento de datoso en el an&aacute;lisis del XML. */
	protected SignBatch(final byte[] xml) throws IOException {
		if (xml == null || xml.length < 1) {
			throw new IllegalArgumentException(
				"El XML de definicion de lote de firmas no puede ser nulo ni vacio" //$NON-NLS-1$
			);
		}

		final InputStream is = new ByteArrayInputStream(xml);
		Document doc;
		try {
			doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(is);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Error al cargar el fichero XML de definicion de lote: " + e + "\n" + new String(xml) //$NON-NLS-1$ //$NON-NLS-2$
			);
			throw new IOException("Error al cargar el fichero XML de definicion de lote: " + e, e); //$NON-NLS-1$
		}
		is.close();

		final Node signBatchNode = doc.getDocumentElement();
		if (!"signbatch".equalsIgnoreCase(signBatchNode.getNodeName())) { //$NON-NLS-1$
			throw new IllegalArgumentException("No se encontro el nodo 'signbatch' en el XML proporcionado"); //$NON-NLS-1$
		}

		this.stopOnError = true;

		final NamedNodeMap nnm = signBatchNode.getAttributes();
		if (nnm != null) {
			Node tmpNode = nnm.getNamedItem("stoponerror"); //$NON-NLS-1$
			if (tmpNode != null) {
				this.stopOnError = !"false".equalsIgnoreCase(tmpNode.getNodeValue()); //$NON-NLS-1$
			}
			tmpNode = nnm.getNamedItem("algorithm"); //$NON-NLS-1$
			if (tmpNode != null) {
				this.algorithm = SingleSignConstants.SignAlgorithm.getAlgorithm(tmpNode.getNodeValue());
			}
			else {
				throw new IllegalArgumentException(
					"El nodo 'signbatch' debe contener al manos el atributo de algoritmo" //$NON-NLS-1$
				);
			}
			tmpNode = nnm.getNamedItem("concurrenttimeout"); //$NON-NLS-1$
			if (tmpNode != null) {
				try {
					this.concurrentTimeout = Long.parseLong(tmpNode.getNodeValue());
				}
				catch(final Exception e) {
					LOGGER.severe(
						"Se ha especificado un valor invalido para la espera maxima (" + tmpNode.getNodeValue() + "), se usara el valor por defecto (" + Long.MAX_VALUE + "): " + e //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					);
				}
			}

			this.id = UUID.randomUUID().toString();
			tmpNode = nnm.getNamedItem("Id"); //$NON-NLS-1$
			if (tmpNode != null) {
				this.id = tmpNode.getNodeValue();
			}
		}
		else {
			throw new IllegalArgumentException(
				"El nodo 'signbatch' debe contener al manos el atributo de algoritmo" //$NON-NLS-1$
			);
		}

		this.signs = parseSignBatchNode(signBatchNode);

	}

	private static List<SingleSign> parseSignBatchNode(final Node n) throws DOMException, IOException {
		final NodeList childNodes = n.getChildNodes();
		final List<SingleSign> ret = new ArrayList<SingleSign>();
		int idx = nextNodeElementIndex(childNodes, 0);
		while (idx != -1) {
			ret.add(new SingleSign(childNodes.item(idx)));
			idx = nextNodeElementIndex(childNodes, idx + 1);
		}
		return ret;
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

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder(
			"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<signbatch stoponerror=\"" //$NON-NLS-1$
		);
		sb.append(Boolean.toString(this.stopOnError));
		sb.append("\" algorithm=\""); //$NON-NLS-1$
		sb.append(this.algorithm.toString());
		sb.append("\" concurrenttimeout=\""); //$NON-NLS-1$
		sb.append(this.concurrentTimeout);
		sb.append("\" Id=\""); //$NON-NLS-1$
		sb.append(this.id);
		sb.append("\">\n"); //$NON-NLS-1$
		for (final SingleSign ss : this.signs) {
			sb.append(ss.toString());
			sb.append('\n');
		}
		sb.append("</signbatch>\n"); //$NON-NLS-1$
		return sb.toString();
	}

	/** Indica si el proceso por lote debe detenerse cuando se encuentre un error.
	 * @param soe <code>true</code> si el proceso por lote debe detenerse cuando se encuentre un error,
	 *            <code>false</code> si se debe continuar con el siguiente elemento del lote cuando se
	 *            produzca un error. */
	public void setStopOnError(final boolean soe) {
		this.stopOnError = soe;
	}

	protected SignBatch(final List<SingleSign> signatures,
			            final SingleSignConstants.SignAlgorithm algo,
			            final boolean soe) {

		if (signatures == null) {
			throw new IllegalArgumentException(
				"La lista de firmas del lote no puede ser nula" //$NON-NLS-1$
			);
		}
		if (algo == null) {
			throw new IllegalArgumentException(
				"El algoritmo de firma no puede ser nulo" //$NON-NLS-1$
			);
		}
		this.signs = signatures;
		this.stopOnError = soe;
		this.algorithm = algo;
		this.id = UUID.randomUUID().toString();
	}

	protected String getResultLog() {
		// Iniciamos el log de retorno
		final StringBuilder ret = new StringBuilder("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<signs>\n"); //$NON-NLS-1$
		for (final SingleSign ss : this.signs) {
			ret.append(" "); //$NON-NLS-1$
			ret.append(ss.getProcessResult().toString());
			ret.append("\n"); //$NON-NLS-1$
		}
		ret.append("</signs>"); //$NON-NLS-1$
		return ret.toString();
	}

	protected void deleteAllTemps() {
		final TempStore ts = TempStoreFactory.getTempStore();
		for (final SingleSign ss : this.signs) {
			ts.delete(ss, getId());
		}
	}


}
