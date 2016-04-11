package es.gob.afirma.signfolder.server.proxy;

import java.io.IOException;
import java.security.cert.CertificateException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.TriphaseData;

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
		} catch (final Exception e) {
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
			statusOk = attributeNode == null || !"KO".equalsIgnoreCase(attributeNode.getNodeValue()); //$NON-NLS-1$

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

		/** Operacion criptografica de firma del Portafirmas. */
		private static final String PORTAFIRMAS_CRYPTO_OPERATION_SIGN = "FIRMAR"; //$NON-NLS-1$
		/** Operacion criptografica de cofirma del Portafirmas. */
		private static final String PORTAFIRMAS_CRYPTO_OPERATION_COSIGN = "COFIRMAR"; //$NON-NLS-1$
		/** Operacion criptografica de contrafirma del Portafirmas. */
		private static final String PORTAFIRMAS_CRYPTO_OPERATION_COUNTERSIGN = "CONTRAFIRMAR"; //$NON-NLS-1$

		/** Operacion criptografica de firma de Afirma. */
		private static final String AFIRMA_CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$
		/** Operacion criptografica de cofirma de Afirma. */
		private static final String AFIRMA_CRYPTO_OPERATION_COSIGN = "cosign"; //$NON-NLS-1$
		/** Operacion criptografica de contrafirma de Afirma. */
		private static final String AFIRMA_CRYPTO_OPERATION_COUNTERSIGN = "countersign"; //$NON-NLS-1$

		/** Formato de firma PDF. */
		private static final String SIGN_FORMAT_PDF = "PDF"; //$NON-NLS-1$
		/** Formato de firma OOXML. */
		private static final String SIGN_FORMAT_OOXML = "OOXML"; //$NON-NLS-1$
		/** Formato de firma ODF. */
		private static final String SIGN_FORMAT_ODF = "ODF"; //$NON-NLS-1$
		/** Formato de firma autom&aacute;tico. */
		private static final String SIGN_FORMAT_AUTO = "AUTO"; //$NON-NLS-1$

		static TriphaseSignDocumentRequest parse(final Node trisignDocumentRequestNode) {

			if (!DOCUMENT_REQUEST_NODE.equalsIgnoreCase(trisignDocumentRequestNode.getNodeName())) {
				throw new IllegalArgumentException("Se encontro un elemento '" + //$NON-NLS-1$
						trisignDocumentRequestNode.getNodeName() +
						"' en el listado de documentos para firma trifasica"); //$NON-NLS-1$
			}

			// Datos de la peticion
			final String docId;
			final String cryptoOperation;
			final String messageDigestAlgorithm;
			String signatureFormat;

			// Cargamos los atributos
			Node attributeNode = null;
			final NamedNodeMap attributes = trisignDocumentRequestNode.getAttributes();
			attributeNode = attributes.getNamedItem(IDENTIFIER_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						IDENTIFIER_ATTRIBUTE + "' en una peticion de firma trifasica de documento"); //$NON-NLS-1$
			}
			docId = attributeNode.getNodeValue();

			// Formato de firma
			attributeNode = attributes.getNamedItem(SIGNATURE_FORMAT_ATTRIBUTE);
			if (attributeNode == null) {
				throw new IllegalArgumentException("No se ha encontrado el atributo obligatorio '" + //$NON-NLS-1$
						SIGNATURE_FORMAT_ATTRIBUTE + "' en una peticion de firma trifasica de documento"); //$NON-NLS-1$
			}
			signatureFormat = attributeNode.getNodeValue();

			attributeNode = attributes.getNamedItem(MESSAGE_DIGEST_ALGORITHM_ATTRIBUTE);
			messageDigestAlgorithm = attributeNode != null ? attributeNode.getNodeValue() : null;

			final NodeList docNodeChilds = trisignDocumentRequestNode.getChildNodes();
			if (docNodeChilds == null || docNodeChilds.getLength() == 0) {
				return new TriphaseSignDocumentRequest(docId, signatureFormat, messageDigestAlgorithm, null);
			}

			String params = null;
			int nodeIndex = XmlUtils.nextNodeElementIndex(docNodeChilds, 0);
			if (nodeIndex > -1 && PARAMS_TRIPHASE_NODE.equalsIgnoreCase(docNodeChilds.item(nodeIndex).getNodeName())) {
				params = docNodeChilds.item(nodeIndex).getTextContent().trim();
				nodeIndex = XmlUtils.nextNodeElementIndex(docNodeChilds, ++nodeIndex);
			}

			// Resultado parcial
			TriphaseData partialResult = null;
			if (nodeIndex > -1 && RESULT_TRIPHASE_RESULT_NODE.equalsIgnoreCase(docNodeChilds.item(nodeIndex).getNodeName())) {
				try {
					partialResult = TriphaseConfigDataParser.parse(docNodeChilds.item(nodeIndex).getChildNodes(), docId);
				} catch (final Exception e) {
					throw new IllegalArgumentException("El resultado parcial no esta correctamente codificado en base64 URL SAFE"); //$NON-NLS-1$
				}
			}

			// La operacion criptografica (sign, cosign, countersign-leafs o countersigns-tree)
			// se siempre 'sign' cuando se declare el formato PDF/PAdES, OOXML o ODF (Logica
			// heredada del Portafirmas web).
			// Si no, se tomara del atributo correspondiente
			if (signatureFormat.equals(SIGN_FORMAT_PDF) ||
				signatureFormat.equals(SIGN_FORMAT_OOXML) ||
				signatureFormat.equals(SIGN_FORMAT_ODF)) {
					cryptoOperation = PORTAFIRMAS_CRYPTO_OPERATION_SIGN;

				if (signatureFormat.equals(SIGN_FORMAT_PDF)) {
					// Configuramos la politica de firma de la AGE v1.9 para PAdES
					params = params == null ? "\n" : params + "\n"; //$NON-NLS-1$ //$NON-NLS-2$
					params += "signatureSubFilter=ETSI.CAdES.detached" + //$NON-NLS-1$
							"\npolicyIdentifier=2.16.724.1.3.1.1.2.1.9" + //$NON-NLS-1$
							"\npolicyIdentifierHash=G7roucf600+f03r/o0bAOQ6WAs0=" + //$NON-NLS-1$
							"\npolicyIdentifierHashAlgorithm=1.3.14.3.2.26" + //$NON-NLS-1$
							"\npolicyQualifier=https://sede.060.gob.es/politica_de_firma_anexo_1.pdf"; //$NON-NLS-1$
				}
			}
			else {
				attributeNode = attributes.getNamedItem(CRYPTO_OPERATION_ATTRIBUTE);
				cryptoOperation = attributeNode == null ?
						PORTAFIRMAS_CRYPTO_OPERATION_SIGN : attributeNode.getNodeValue();
			}

			// Si se indico que la operacion es cofirma o contrafirma, se cambia el formato por AUTO (Logica
			// heredada del Portafirmas web).
			if (PORTAFIRMAS_CRYPTO_OPERATION_COSIGN.equals(cryptoOperation)) {
				signatureFormat = SIGN_FORMAT_AUTO;
			} else if (PORTAFIRMAS_CRYPTO_OPERATION_COUNTERSIGN.equals(cryptoOperation)) {
				signatureFormat = SIGN_FORMAT_AUTO;
				params = params == null ? "\n" : params + "\n"; //$NON-NLS-1$ //$NON-NLS-2$
				params += "target=leafs"; //$NON-NLS-1$
			}

			return new TriphaseSignDocumentRequest(
					docId, normalizeOperationType(cryptoOperation), signatureFormat,
					messageDigestAlgorithm, params, null,
					partialResult);
		}

		/**
		 * Normalizamos el nombre del tipo de operaci&oacute;n criptogr&aacute;fica..
		 * @param operationType Tipo de operaci&oacute;n.
		 * @return Nombre del tipo de operaci&oacute;n normalizado o el mismo de entrada
		 * si no se ha encontrado correspondencia.
		 */
		private static String normalizeOperationType(final String operationType) {
			String normalizedOp = operationType;
			if (PORTAFIRMAS_CRYPTO_OPERATION_SIGN.equalsIgnoreCase(normalizedOp)) {
				normalizedOp = AFIRMA_CRYPTO_OPERATION_SIGN;
			} else if (PORTAFIRMAS_CRYPTO_OPERATION_COSIGN.equalsIgnoreCase(normalizedOp)) {
				normalizedOp = AFIRMA_CRYPTO_OPERATION_COSIGN;
			} else if (PORTAFIRMAS_CRYPTO_OPERATION_COUNTERSIGN.equalsIgnoreCase(normalizedOp)) {
				normalizedOp = AFIRMA_CRYPTO_OPERATION_COUNTERSIGN;
			}

			return normalizedOp;
		}
	}

	/**
	 * Analizador XML de los datos de sesi&oacute;n de una firma trif&aacute;sica en particular.
	 */
	private static class TriphaseConfigDataParser {

		private static final String ATTRIBUTE_KEY = "n"; //$NON-NLS-1$

		static TriphaseData parse (final NodeList params, final String id) {

			final Map<String, String> config = new HashMap<String, String>();
			try {
				int numIndex = 0;
				while ((numIndex = XmlUtils.nextNodeElementIndex(params, numIndex)) > -1) {
					final Element param = (Element) params.item(numIndex);
					final String key = param.getAttribute(ATTRIBUTE_KEY);
					if (key == null) {
						throw new IllegalArgumentException("Se ha indicado un parametro de firma trifasica sin clave"); //$NON-NLS-1$
					}
					config.put(key, param.getTextContent().trim());
					numIndex++;
				}
			} catch (final Exception e) {
				throw new IllegalArgumentException("Se ha encontrado datos de sesion mal formados", e); //$NON-NLS-1$
			}

			return new TriphaseData(Arrays.asList(new TriphaseData.TriSign(config, id)));
		}
	}
}
