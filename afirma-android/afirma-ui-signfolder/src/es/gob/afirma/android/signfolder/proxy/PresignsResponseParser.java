package es.gob.afirma.android.signfolder.proxy;

import java.io.IOException;
import java.util.Vector;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import android.util.Log;
import es.gob.afirma.android.signfolder.SFConstants;
import es.gob.afirma.android.signfolder.proxy.TriphaseSignDocumentRequest.TriphaseConfigData;
import es.gob.afirma.core.misc.Base64;

/** Analizador de XML para la generaci&oacute;n de un listado de objetos
 * de tipo {@link es.gob.afirma.android.signfolder.proxy.TriphaseRequest} a partir
 * de un XML de respuesta de prefirma.
 * @author Carlos Gamuci */
public final class PresignsResponseParser {

	private static final String PRESIGN_RESPONSE_NODE = "pres"; //$NON-NLS-1$

	private PresignsResponseParser() {
		// No instanciable
	}

	/**
	 * Analiza un documento XML y, en caso de tener el formato correcto, obtiene de &eacute;l
	 * un listado de objetos {@link es.gob.afirma.android.signfolder.proxy.TriphaseRequest}.
	 * @param doc Documento XML.
	 * @return Objeto con los datos del XML.
	 * @throws IllegalArgumentException Cuando el XML no tiene el formato esperado.
	 */
	static TriphaseRequest[] parse(final Document doc) {

		if (doc == null) {
			throw new IllegalArgumentException("El documento proporcionado no puede ser nulo");  //$NON-NLS-1$
		}

		if (!PRESIGN_RESPONSE_NODE.equalsIgnoreCase(doc.getDocumentElement().getNodeName())) {
			throw new IllegalArgumentException("El elemento raiz del XML debe ser '" + //$NON-NLS-1$
					PRESIGN_RESPONSE_NODE + "' y aparece: " + //$NON-NLS-1$
					doc.getDocumentElement().getNodeName());
		}

		final Vector<TriphaseRequest> listPresignRequests = new Vector<TriphaseRequest>();
		final NodeList requestNodes = doc.getDocumentElement().getChildNodes();
		for (int i = 0; i < requestNodes.getLength(); i++) {
			// Nos aseguramos de procesar solo nodos de tipo Element
			i = XmlUtils.nextNodeElementIndex(requestNodes, i);
			if (i == -1) {
				break;
			}
			listPresignRequests.addElement(TriphaseRequestParser.parse(requestNodes.item(i)));
		}

		final TriphaseRequest[] ret = new TriphaseRequest[listPresignRequests.size()];
		listPresignRequests.copyInto(ret);
		return ret;
	}

	private static final class TriphaseRequestParser {

		private static final String REQUEST_NODE = "req"; //$NON-NLS-1$
		private static final String ID_ATTRIBUTE = "id"; //$NON-NLS-1$
		private static final String STATUS_ATTRIBUTE = "status"; //$NON-NLS-1$
		private static final String EXCEPTION_B64_ATTRIBUTE = "exceptionb64"; //$NON-NLS-1$

		static TriphaseRequest parse(final Node presignRequestNode) {

			if (!REQUEST_NODE.equalsIgnoreCase(presignRequestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						presignRequestNode.getNodeName() + "' en el listado de peticiones"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String ref;
			final boolean statusOk;
			final Vector<TriphaseSignDocumentRequest> listDocumentRequests = new Vector<TriphaseSignDocumentRequest>();

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = presignRequestNode.getAttributes();
			attributeNode = attributes.getNamedItem(ID_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						ID_ATTRIBUTE + "' en un peticion de prefirma"); //$NON-NLS-1$
			}
			ref = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(STATUS_ATTRIBUTE);
			// statusOk = true, salvo que la propiedad status tenga el valor "KO"
			statusOk = attributeNode == null || !"KO".equalsIgnoreCase(attributeNode.getNodeValue()); //$NON-NLS-1$

			attributeNode = attributes.getNamedItem(EXCEPTION_B64_ATTRIBUTE);
			// statusOk = true, salvo que la propiedad status tenga el valor "KO"
			String exception;
			try {
				exception = attributeNode == null ? null : new String(Base64.decode(attributeNode.getNodeValue()));
			} catch (final IOException e) {
				Log.w(SFConstants.LOG_TAG, "No se ha podido descodificar el base 64 de la traza de la excepcion, se usara tal cual");  //$NON-NLS-1$
				exception = attributeNode.getNodeValue();
			}

			// Si la peticion no se ha procesado correctamente se descarta
			if (!statusOk) {
				return new TriphaseRequest(ref, false, exception);
			}

			// Cargamos el listado de peticiones
			final NodeList requestsNode = presignRequestNode.getChildNodes();
			for (int i = 0; i < requestsNode.getLength(); i++) {
				// Nos aseguramos de procesar solo nodos de tipo Element
				i = XmlUtils.nextNodeElementIndex(requestsNode, i);
				if (i == -1) {
					break;
				}
				listDocumentRequests.addElement(PresignDocumentRequestParser.parse(requestsNode.item(i)));
			}

			final TriphaseSignDocumentRequest[] tmpReqs = new TriphaseSignDocumentRequest[listDocumentRequests.size()];
			listDocumentRequests.copyInto(tmpReqs);

			return new TriphaseRequest(ref, statusOk, tmpReqs);
		}
	}

	private static final class PresignDocumentRequestParser {

		private static final String DOCUMENT_REQUEST_NODE = "doc"; //$NON-NLS-1$
		private static final String IDENTIFIER_ATTRIBUTE = "docid"; //$NON-NLS-1$
		private static final String CRYPTO_OPERATION_ATTRIBUTE = "cop"; //$NON-NLS-1$
		private static final String SIGNATURE_FORMAT_ATTRIBUTE = "sigfrmt"; //$NON-NLS-1$
		private static final String MESSAGE_DIGEST_ALGORITHM_ATTRIBUTE = "mdalgo"; //$NON-NLS-1$
		private static final String PARAMS_NODE = "params"; //$NON-NLS-1$
		private static final String RESULT_NODE = "result"; //$NON-NLS-1$

		private static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$
		private static final String CRYPTO_OPERATION_COSIGN = "cosign"; //$NON-NLS-1$
		private static final String CRYPTO_OPERATION_COUNTERSIGN = "countersign"; //$NON-NLS-1$

		private static final String DEFAULT_CRYPTO_OPERATION = CRYPTO_OPERATION_SIGN;

		static TriphaseSignDocumentRequest parse(final Node presignDocumentRequestNode) {

			if (!DOCUMENT_REQUEST_NODE.equalsIgnoreCase(presignDocumentRequestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						presignDocumentRequestNode.getNodeName() +
						"' en el listado de documentos para prefirma"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String docId;
			final String cryptoOperation;
			final String signatureFormat;
			final String messageDigestAlgorithm;
			String params = null;

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = presignDocumentRequestNode.getAttributes();
			attributeNode = attributes.getNamedItem(IDENTIFIER_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						IDENTIFIER_ATTRIBUTE + "' en una peticion de prefirma de documento"); //$NON-NLS-1$
			}
			docId = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(CRYPTO_OPERATION_ATTRIBUTE);
			cryptoOperation = attributeNode != null ? normalizeCriptoOperationName(attributeNode.getNodeValue()) : DEFAULT_CRYPTO_OPERATION;

			attributeNode = attributes.getNamedItem(SIGNATURE_FORMAT_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						SIGNATURE_FORMAT_ATTRIBUTE + "' en una peticion de prefirma de documento"); //$NON-NLS-1$
			}
			signatureFormat = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(MESSAGE_DIGEST_ALGORITHM_ATTRIBUTE);
			messageDigestAlgorithm = attributeNode != null ? attributeNode.getNodeValue() : null;

			// Cargamos la respuesta de la prefirma
			final NodeList presignConfigNodes = presignDocumentRequestNode.getChildNodes();
			int numIndex = XmlUtils.nextNodeElementIndex(presignConfigNodes, 0);

			// Comprobamos el nodo con los parametros
			if (numIndex != -1 && PARAMS_NODE.equalsIgnoreCase(presignConfigNodes.item(numIndex).getNodeName())) {
				params = XmlUtils.getTextContent(presignConfigNodes.item(numIndex));
				numIndex = XmlUtils.nextNodeElementIndex(presignConfigNodes, ++numIndex);
			}

			// Comprobamos el nodo con el resultado parcial
			if (numIndex == -1 || !RESULT_NODE.equalsIgnoreCase(presignConfigNodes.item(numIndex).getNodeName())) {
				throw new IllegalArgumentException("No se ha encontrado el nodo " + //$NON-NLS-1$
						RESULT_NODE +
						" en la respuesta de la peticion de prefirma del documento"); //$NON-NLS-1$
			}

			return new TriphaseSignDocumentRequest(
					docId, cryptoOperation, signatureFormat, messageDigestAlgorithm, params,
					TriphaseConfigDataParser.parse(presignConfigNodes.item(numIndex).getChildNodes()));
		}

		/**
		 * Normaliza varios nombres alternativos para las oepraciones criptogr&aacute;ficas, dejando uno solo
		 * para cada uno de ellos.
		 * @param criptoOperation Operaci&oacute;n criptogr&aacute;fica.
		 * @return Nombre normalizado de la operaci&oacute;n o el mismo si no se reconoce.
		 */
		private static String normalizeCriptoOperationName(final String criptoOperation) {
			String normalizedName = criptoOperation;
			if ("firma".equalsIgnoreCase(criptoOperation)) { //$NON-NLS-1$
				normalizedName = CRYPTO_OPERATION_SIGN;
			} else if ("cofirma".equalsIgnoreCase(criptoOperation)) { //$NON-NLS-1$
				normalizedName = CRYPTO_OPERATION_COSIGN;
			} else if ("contrafirma".equalsIgnoreCase(criptoOperation)) { //$NON-NLS-1$
				normalizedName = CRYPTO_OPERATION_COUNTERSIGN;
			}

			return normalizedName;
		}
	}

	private static class TriphaseConfigDataParser {

		private static final String ATTRIBUTE_KEY = "k"; //$NON-NLS-1$
		private static final String VALUE_SIGN_COUNT = "sc"; //$NON-NLS-1$
		private static final String VALUE_NEED_PRE = "np"; //$NON-NLS-1$
		private static final String VALUE_NEED_DATA = "nd"; //$NON-NLS-1$
		private static final String VALUE_SESSION_PREFIX = "ss."; //$NON-NLS-1$
		private static final String VALUE_PK1_PREFIX = "pk1."; //$NON-NLS-1$
		private static final String VALUE_PRE_PREFIX = "pre."; //$NON-NLS-1$

		static TriphaseConfigData parse (final NodeList params) {

			final TriphaseConfigData config = new TriphaseConfigData();
			try {

				int numIndex = 0;
				while ((numIndex = XmlUtils.nextNodeElementIndex(params, numIndex)) > -1) {
					final Element param = (Element) params.item(numIndex);
					final String key = param.getAttribute(ATTRIBUTE_KEY);
					if (key == null) {
						throw new IllegalArgumentException("Se ha indicado un parametro de firma trifasica sin clave"); //$NON-NLS-1$
					}

					Log.i(SFConstants.LOG_TAG, "Clave: " + key); //$NON-NLS-1$
					Log.i(SFConstants.LOG_TAG, "Valor: " + param.getTextContent()); //$NON-NLS-1$

					if (VALUE_SIGN_COUNT.equalsIgnoreCase(key)) {
						config.setSignCount(Integer.valueOf(param.getTextContent().trim()));
					}
					else if (VALUE_NEED_PRE.equalsIgnoreCase(key)) {
						config.setNeedPreSign(Boolean.valueOf(param.getTextContent().trim()));
					}
					else if (VALUE_NEED_DATA.equalsIgnoreCase(key)) {
						config.setNeedData(Boolean.valueOf(param.getTextContent().trim()));
					}
					else if (key.startsWith(VALUE_SESSION_PREFIX)) {
						config.addSession(param.getTextContent().trim());
					}
					else if (key.startsWith(VALUE_PK1_PREFIX)) {
						config.addPk1(Base64.decode(param.getTextContent().trim()));
					}
					else if (key.startsWith(VALUE_PRE_PREFIX)) {
						Log.i(SFConstants.LOG_TAG, "Elemento " + Integer.parseInt(key.substring(VALUE_PRE_PREFIX.length()))); //$NON-NLS-1$
						Log.i(SFConstants.LOG_TAG, "Contenido " + param.getTextContent().trim()); //$NON-NLS-1$
						config.addPreSign(Base64.decode(param.getTextContent().trim()));
					}

					numIndex++;
				}
			} catch (final IOException e) {
				throw new IllegalArgumentException("Se ha encontrado un Base64 mal formado", e); //$NON-NLS-1$
			}

			return config;
		}
	}
}
