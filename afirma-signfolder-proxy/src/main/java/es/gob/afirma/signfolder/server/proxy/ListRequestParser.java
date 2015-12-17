package es.gob.afirma.signfolder.server.proxy;

import java.io.ByteArrayInputStream;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.misc.Base64;

/**
 * Analiza un documento XML para obtener una petici&oacute;n de solicitudes de firma.
 * @author Carlos Gamuci
 */
public class ListRequestParser {

	private final static int DEFAULT_PAGE_SIZE = 50;

	private final static String LIST_REQUEST_NODE = "rqtlst"; //$NON-NLS-1$
	private final static String CERT_NODE = "cert"; //$NON-NLS-1$
	private final static String FORMATS_NODE = "fmts"; //$NON-NLS-1$
	private final static String FORMAT_NODE = "fmt"; //$NON-NLS-1$
	private final static String FILTERS_NODE = "fltrs"; //$NON-NLS-1$
	private final static String FILTER_NODE = "fltr"; //$NON-NLS-1$
	private final static String FILTER_KEY_NODE = "key"; //$NON-NLS-1$
	private final static String FILTER_VALUE_NODE = "value"; //$NON-NLS-1$

	private final static String STATE_ATTRIBUTE = "state"; //$NON-NLS-1$
	private final static String PAGE_ATTRIBUTE = "pg"; //$NON-NLS-1$
	private final static String PAGE_SIZE_ATTRIBUTE = "sz"; //$NON-NLS-1$


	private ListRequestParser() {
		// No se permite el constructor por defecto
	}

	/** Analiza un documento XML y, en caso de tener el formato correcto, obtiene de &eacute;l
	 * un objeto de tipo {@link es.gob.afirma.signfolder.server.proxy.ListRequest}.
	 * @param doc Documento XML.
	 * @return Objeto con los datos del XML.
	 * @throws IllegalArgumentException Cuando el XML no tiene el formato esperado.	 */
	static ListRequest parse(final Document doc) {

		if (doc == null) {
			throw new IllegalArgumentException("El documento proporcionado no puede ser nulo");  //$NON-NLS-1$
		}

		if (!LIST_REQUEST_NODE.equalsIgnoreCase(doc.getDocumentElement().getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + //$NON-NLS-1$
					LIST_REQUEST_NODE + "' y aparece: " + //$NON-NLS-1$
					doc.getDocumentElement().getNodeName());
		}

		String state;
		int numPage;
		int pageSize;
		String[] formats = null;
		Map<String, String> filters = null;

		// Configuramos el estado de las peticiones deseadas
		state = doc.getDocumentElement().getAttribute(STATE_ATTRIBUTE);
		if (state == null) {
			state = ListRequest.STATE_UNRESOLVED;
		}

		// Identificamos el numero de pagina solicitado
		final String pageAttr = doc.getDocumentElement().getAttribute(PAGE_ATTRIBUTE);
		numPage = pageAttr == null ? 1 : Integer.parseInt(pageAttr);

		// Configuramos el tamano de las paginas
		final String pageSizeAttr = doc.getDocumentElement().getAttribute(PAGE_SIZE_ATTRIBUTE);
		pageSize = pageSizeAttr == null ? DEFAULT_PAGE_SIZE : Integer.parseInt(pageSizeAttr);


		final byte[] certEncoded;

		// Establecemos el certificado para la autenticacion
		final NodeList requestNodes = doc.getDocumentElement().getChildNodes();
		int nodeIndex = XmlUtils.nextNodeElementIndex(requestNodes, 0);
		if (nodeIndex != -1 && CERT_NODE.equalsIgnoreCase(requestNodes.item(nodeIndex).getNodeName())) {
			try {
				certEncoded = Base64.decode(requestNodes.item(nodeIndex).getTextContent().trim());
			} catch (final Exception e) {
				throw new IllegalArgumentException(
						"No se ha podido obtener la codificacion del certificado a partir del XML: " + e); //$NON-NLS-1$
			}
			nodeIndex = XmlUtils.nextNodeElementIndex(requestNodes, ++nodeIndex);
		} else {
			throw new IllegalArgumentException(
					"No se ha encontrado el certificado para la autenticacion de la peticion de solicitudes de firma"); //$NON-NLS-1$
		}

		// En caso de haber formatos de firma soportados y filtros de peticiones, los establecemos
		if (nodeIndex != -1 && FORMATS_NODE.equalsIgnoreCase(requestNodes.item(nodeIndex).getNodeName())) {
			formats = getFormats(requestNodes.item(nodeIndex).getChildNodes());
			nodeIndex = XmlUtils.nextNodeElementIndex(requestNodes, ++nodeIndex);
		}

		if (nodeIndex != -1 && FILTERS_NODE.equalsIgnoreCase(requestNodes.item(nodeIndex).getNodeName())) {
			filters = getFilters(requestNodes.item(nodeIndex).getChildNodes());
			nodeIndex = XmlUtils.nextNodeElementIndex(requestNodes, ++nodeIndex);
		}

		if (nodeIndex != -1) {
			throw new IllegalArgumentException("Se ha encontrado el nodo '" + //$NON-NLS-1$
					requestNodes.item(nodeIndex).getNodeName() + "' en la peticion de solicitudes de firma"); //$NON-NLS-1$
		}

		return new ListRequest(certEncoded, state, formats, filters, numPage, pageSize);
	}

	/**
	 * Genera un certificado a trav&eacute;s de su codificaci&oacute;n.
	 * @param certEncoded Certificado codificado.
	 * @return Certificado.
	 * @throws CertificateException Cuando no se ha proporcionado un certificado codificado.
	 */
	public static Certificate getCertificate(final String certEncoded) throws CertificateException {
		return CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
				new ByteArrayInputStream(certEncoded.getBytes()));
	}

	/**
	 * Obtiene el listado de nodos con los formatos de firma.
	 * @param nodes Nodos con los formatos de firma.
	 * @return Listado de formatos de firma.
	 */
	private static String[] getFormats(final NodeList nodes) {
		final List<String> formats = new ArrayList<String>();
		for (int i = 0; i < nodes.getLength(); i++) {
			i = XmlUtils.nextNodeElementIndex(nodes, i);
			if (i == -1) {
				break;
			}
			if (!FORMAT_NODE.equalsIgnoreCase(nodes.item(i).getNodeName())) {
				throw new IllegalArgumentException("Se ha encontrado el nodo '" + //$NON-NLS-1$
						nodes.item(i).getNodeName() + "' en el listado de formatos de firma"); //$NON-NLS-1$
			}
			final String format = nodes.item(i).getTextContent();
			if (format != null && format.trim().length() > 0) {
				formats.add(format.trim());
			}
		}
		return formats.toArray(new String[formats.size()]);
	}

	/**
	 * Extrae la configuraci&oacute;n de los filtros de un listado de nodos XML.
	 * @param nodes Listado de nodos.
	 * @return Configuraci&oacute;n de los filtros.
	 */
	private static Map<String, String> getFilters(final NodeList nodes) {

		final Map<String, String> filters = new HashMap<String, String>();
		for (int i = 0; i < nodes.getLength(); i++) {
			i = XmlUtils.nextNodeElementIndex(nodes, i);
			if (i == -1) {
				break;
			}
			if (!FILTER_NODE.equalsIgnoreCase(nodes.item(i).getNodeName())) {
				throw new IllegalArgumentException("Se ha encontrado el nodo '" + //$NON-NLS-1$
						nodes.item(i).getNodeName() + "' en el listado de filtros de solicitudes"); //$NON-NLS-1$
			}
			// Leemos los subnodos con la clave y el valor del filtro
			final NodeList filterParamNodes = nodes.item(i).getChildNodes();
			int j = XmlUtils.nextNodeElementIndex(filterParamNodes, 0);
			if (j == -1) {
				throw new IllegalArgumentException("No se ha encontrado el nodo '" + //$NON-NLS-1$
						FILTER_KEY_NODE + "' con la clave de filtrado"); //$NON-NLS-1$
			}
			if (!FILTER_KEY_NODE.equalsIgnoreCase(filterParamNodes.item(j).getNodeName())) {
				throw new IllegalArgumentException("Se ha encontrado el nodo '" + //$NON-NLS-1$
						filterParamNodes.item(j).getNodeName() + "' en lugar del nodo con clave de filtrado " + //$NON-NLS-1$
						FILTER_KEY_NODE);
			}
			final String key = filterParamNodes.item(j).getTextContent();

			j = XmlUtils.nextNodeElementIndex(filterParamNodes, ++j);
			if (j == -1) {
				throw new IllegalArgumentException("No se ha encontrado el nodo '" + //$NON-NLS-1$
						FILTER_KEY_NODE + "' con el valor de filtrado"); //$NON-NLS-1$
			}
			if (!FILTER_VALUE_NODE.equalsIgnoreCase(filterParamNodes.item(j).getNodeName())) {
				throw new IllegalArgumentException("Se ha encontrado el nodo '" + //$NON-NLS-1$
						filterParamNodes.item(j).getNodeName() + "' en lugar del nodo con valor de filtrado " + //$NON-NLS-1$
						FILTER_VALUE_NODE);
			}
			final String value = filterParamNodes.item(j).getTextContent();

			filters.put(key, value);
		}
		return filters;
	}
}
