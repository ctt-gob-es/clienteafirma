package es.gob.afirma.signfolder.server.proxy;

import java.io.IOException;
import java.security.cert.CertificateException;
import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signfolder.server.proxy.TriphaseSignDocumentRequest.TriphaseConfigDataBean;

/** Analizador de XML para la generaci&oacute;n de objetos de tipo
 * {@link es.gob.afirma.signfolder.server.proxy.TriphaseRequestBean} a partir
 * de un XML de petici&oacute;n de firma trif&aacute;sica. *
 * @author Carlos Gamuci */
final class SignRequestsParser {

	private static final String TRISIGN_REQUEST_NODE = "rqttri"; //$NON-NLS-1$
	private static final String CERTIFICATE_NODE = "cert"; //$NON-NLS-1$
	private static final String REQUESTS_LIST_NODE = "reqs"; //$NON-NLS-1$

	private SignRequestsParser() {
		// No instanciable
	}

	/** Analiza un documento XML y, en caso de tener el formato correcto, obtiene de &eacute;l
	 * un objeto de tipo {@link es.gob.afirma.signfolder.server.proxy.TriphaseRequestBean} con
	 * la informacion correspondiente a un listado de peticiones de firma con varios documentos
	 * cada una.
	 * @param doc Documento XML.
	 * @return Objeto con los datos del XML.
	 * @throws IOException Si ocurren problemas decodificando el certificado desde Base64
	 * @throws CertificateException Si ocurren problemas creando el certificado
	 * @throws IllegalArgumentException Cuando el XML no tiene el formato esperado.	 */
	static TriphaseRequestBean parse(final Document doc) throws CertificateException, IOException {

		if (doc == null) {
			throw new IllegalArgumentException("El documento proporcionado no puede ser nulo");  //$NON-NLS-1$
		}

		if (!TRISIGN_REQUEST_NODE.equalsIgnoreCase(doc.getDocumentElement().getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + //$NON-NLS-1$
					TRISIGN_REQUEST_NODE + "' y aparece: " + //$NON-NLS-1$
					doc.getDocumentElement().getNodeName());
		}

		final NodeList requestNodes = doc.getDocumentElement().getChildNodes();

		// Nos aseguramos de procesar solo nodos de tipo Element
		int nodeIndex = XmlUtils.nextNodeElementIndex(requestNodes, 0);
		if (nodeIndex == -1 || !CERTIFICATE_NODE.equalsIgnoreCase(requestNodes.item(nodeIndex).getNodeName())) {
			throw new IllegalArgumentException("La peticion de firma trifasica no contiene el nodo " + //$NON-NLS-1$
					CERTIFICATE_NODE + " con el certificado de firma a utilizar"); //$NON-NLS-1$
		}

		final byte[] certEncoded;
		try {
			certEncoded = Base64.decode(requestNodes.item(nodeIndex).getTextContent().trim());
		} catch (Exception e) {
			throw new IllegalArgumentException(
					"No se ha podido obtener la codificacion del certificado a partir del XML: " + e); //$NON-NLS-1$
		}
		
		nodeIndex = XmlUtils.nextNodeElementIndex(requestNodes, ++nodeIndex);
		if (nodeIndex == -1 || !REQUESTS_LIST_NODE.equalsIgnoreCase(requestNodes.item(nodeIndex).getNodeName())) {
			throw new IllegalArgumentException("La peticion de firma trifasica no contiene el nodo " + //$NON-NLS-1$
					REQUESTS_LIST_NODE + " con el listado de peticiones de firma de documentos"); //$NON-NLS-1$
		}
		final List<TriphaseRequest> listTrisignRequests = new ArrayList<TriphaseRequest>();
		final NodeList requestsNode = requestNodes.item(nodeIndex).getChildNodes();
		for (int i = 0; i < requestsNode.getLength(); i++) {
			// Nos aseguramos de procesar solo nodos de tipo Element
			i = XmlUtils.nextNodeElementIndex(requestsNode, i);
			if (i == -1) {
				break;
			}
			listTrisignRequests.add(SignRequestParser.parse(requestsNode.item(i)));
		}

		return new TriphaseRequestBean(certEncoded, listTrisignRequests);
	}

	/**
	 * Analizador XML de una petici&oacute;n de firma particular.
	 */
	private static final class SignRequestParser {

		private static final String REQUEST_NODE = "req"; //$NON-NLS-1$
		private static final String ID_ATTRIBUTE = "id"; //$NON-NLS-1$
		private static final String STATUS_ATTRIBUTE = "status"; //$NON-NLS-1$

		static TriphaseRequest parse(final Node trisignRequestNode) {

			if (!REQUEST_NODE.equalsIgnoreCase(trisignRequestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						trisignRequestNode.getNodeName() + "' en el listado de peticiones"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String ref;
			final boolean statusOk;
			final List<TriphaseSignDocumentRequest> listDocumentRequests = new ArrayList<TriphaseSignDocumentRequest>();

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = trisignRequestNode.getAttributes();
			attributeNode = attributes.getNamedItem(ID_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException(
					"No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
					ID_ATTRIBUTE + "' en un peticion de firma trifasica" //$NON-NLS-1$
				);
			}
			ref = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(STATUS_ATTRIBUTE);
			// statusOk = true, salvo que la propiedad status tenga el valor "KO"
			statusOk = (attributeNode == null || !"KO".equalsIgnoreCase(attributeNode.getNodeValue())); //$NON-NLS-1$

			// Si la peticion no se ha procesado correctamente se descarta
			if (!statusOk) {
				return new TriphaseRequest(ref, false, null);
			}
			
			// Cargamos el listado de peticiones
			final NodeList requestsNode = trisignRequestNode.getChildNodes();
			for (int i = 0; i < requestsNode.getLength(); i++) {
				// Nos aseguramos de procesar solo nodos de tipo Element
				i = XmlUtils.nextNodeElementIndex(requestsNode, i);
				if (i == -1) {
					break;
				}
				listDocumentRequests.add(TrisignDocumentRequestParser.parse(requestsNode.item(i)));
			}

			return new TriphaseRequest(ref, statusOk, listDocumentRequests);
		}
	}

	/**
	 * Analizador XML para los datos de solicitud de firma de un documento en particular.
	 */
	private static final class TrisignDocumentRequestParser {

		private static final String DOCUMENT_REQUEST_NODE = "doc"; //$NON-NLS-1$
		private static final String IDENTIFIER_ATTRIBUTE = "docid"; //$NON-NLS-1$
		private static final String CRYPTO_OPERATION_ATTRIBUTE = "cop"; //$NON-NLS-1$
		private static final String SIGNATURE_FORMAT_ATTRIBUTE = "sigfrmt"; //$NON-NLS-1$
		private static final String MESSAGE_DIGEST_ALGORITHM_ATTRIBUTE = "mdalgo"; //$NON-NLS-1$
		private static final String PARAMS_TRIPHASE_NODE = "params"; //$NON-NLS-1$
		private static final String RESULT_TRIPHASE_RESULT_NODE = "result"; //$NON-NLS-1$
		
		/** Operacion criptograficapor defecto. */
		private static final String DEFAULT_CRYPTO_OPERATION = "sign"; //$NON-NLS-1$
		
		static TriphaseSignDocumentRequest parse(final Node trisignDocumentRequestNode) {

			if (!DOCUMENT_REQUEST_NODE.equalsIgnoreCase(trisignDocumentRequestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						trisignDocumentRequestNode.getNodeName() +
						"' en el listado de documentos para firma trifasica"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String docId;
			final String cryptoOperation;
			final String signatureFormat;
			final String messageDigestAlgorithm;

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = trisignDocumentRequestNode.getAttributes();
			attributeNode = attributes.getNamedItem(IDENTIFIER_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						IDENTIFIER_ATTRIBUTE + "' en una peticion de firma trifasica de documento"); //$NON-NLS-1$
			}
			docId = attributeNode.getNodeValue();

			// Operacion criptografica (sign, cosign, countersign-leafs o countersigns-tree)
			attributeNode = attributes.getNamedItem(CRYPTO_OPERATION_ATTRIBUTE);
			cryptoOperation = attributeNode == null ? DEFAULT_CRYPTO_OPERATION : attributeNode.getNodeValue();
			
			attributeNode = attributes.getNamedItem(SIGNATURE_FORMAT_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						SIGNATURE_FORMAT_ATTRIBUTE + "' en una peticion de firma trifasica de documento"); //$NON-NLS-1$
			}
			signatureFormat = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(MESSAGE_DIGEST_ALGORITHM_ATTRIBUTE);
			messageDigestAlgorithm = (attributeNode != null ? attributeNode.getNodeValue() : null);

			String params = null;
			final NodeList docNodeChilds = trisignDocumentRequestNode.getChildNodes();
			if (docNodeChilds == null || docNodeChilds.getLength() == 0) {
				return new TriphaseSignDocumentRequest(docId, signatureFormat, messageDigestAlgorithm, null);
			}
			
			int nodeIndex = XmlUtils.nextNodeElementIndex(docNodeChilds, 0);
			if (nodeIndex > -1 && PARAMS_TRIPHASE_NODE.equalsIgnoreCase(docNodeChilds.item(nodeIndex).getNodeName())) {
				params = docNodeChilds.item(nodeIndex).getTextContent().trim();
				nodeIndex = XmlUtils.nextNodeElementIndex(docNodeChilds, ++nodeIndex);
			}
			
			// Resultado parcial
			TriphaseConfigDataBean partialResult = null;
			if (nodeIndex > -1 && RESULT_TRIPHASE_RESULT_NODE.equalsIgnoreCase(docNodeChilds.item(nodeIndex).getNodeName())) {
				try {
					partialResult = TriphaseConfigDataParser.parse(docNodeChilds.item(nodeIndex).getChildNodes());
				} catch (Exception e) {
					throw new IllegalArgumentException("El resultado parcial no esta correctamente codificado en base64 URL SAFE"); //$NON-NLS-1$
				}
			}

			return new TriphaseSignDocumentRequest(
					docId, cryptoOperation, signatureFormat,
					messageDigestAlgorithm, params, null,
					partialResult);
		}
	}
	
	/**
	 * Analizador XML de los datos de sesi&oacute;n de una firma trif&aacute;sica en particular.
	 */
	private static class TriphaseConfigDataParser {
		
		private static final String ATTRIBUTE_KEY = "k"; //$NON-NLS-1$
		private static final String VALUE_SIGN_COUNT = "sc"; //$NON-NLS-1$
		private static final String VALUE_NEED_PRE = "np"; //$NON-NLS-1$
		private static final String VALUE_NEED_DATA = "nd"; //$NON-NLS-1$
		private static final String VALUE_SESSION_PREFIX = "ss."; //$NON-NLS-1$
		private static final String VALUE_PK1_PREFIX = "pk1."; //$NON-NLS-1$
		private static final String VALUE_PRE_PREFIX = "pre."; //$NON-NLS-1$
		
		static TriphaseConfigDataBean parse (final NodeList params) {

			final TriphaseConfigDataBean config = new TriphaseConfigDataBean();
			try {
				
				int numIndex = 0;
				while ((numIndex = XmlUtils.nextNodeElementIndex(params, numIndex)) > -1) {
					final Element param = (Element) params.item(numIndex);
					String key = param.getAttribute(ATTRIBUTE_KEY);
					if (key == null) {
						throw new IllegalArgumentException("Se ha indicado un parametro de firma trifasica sin clave"); //$NON-NLS-1$
					}
					if (VALUE_SIGN_COUNT.equalsIgnoreCase(key)) {
						config.setSignCount(Integer.valueOf(param.getTextContent().trim()));
					} else if (VALUE_NEED_PRE.equalsIgnoreCase(key)) {
						config.setNeedPreSign(Boolean.valueOf(param.getTextContent().trim()));
					} else if (VALUE_NEED_DATA.equalsIgnoreCase(key)) {
						config.setNeedData(Boolean.valueOf(param.getTextContent().trim()));
					} else if (key.startsWith(VALUE_SESSION_PREFIX)) {
						config.addSession(param.getTextContent().trim());
					} else if (key.startsWith(VALUE_PK1_PREFIX)) {
						config.addPk1(param.getTextContent().trim());
					} else if (key.startsWith(VALUE_PRE_PREFIX)) {
						config.addPreSign(param.getTextContent().trim());
					}
					numIndex++;
				}
			} catch (final Exception e) {
				throw new IllegalArgumentException("Se ha encontrado datos de sesion mal formados", e); //$NON-NLS-1$
			}

			return config;
		}
	}
}
