package es.gob.afirma.signfolder.server.proxy;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.TriphaseData.TriSign;
import es.gob.afirma.signfolder.server.proxy.TriphaseSignDocumentRequest.TriphaseConfigDataBean;

/**
 * Manejador para el uso est&aacute;tico de las operaciones de prefirma y postfirma.
 */
public class TriSigner {

	/** Identificador de la operaci&oacute;n de prefirma en servidor. */
	private static final String OPERATION_PRESIGN = "pre"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de postfirma en servidor. */
	private static final String OPERATION_POSTSIGN = "post"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n trif&aacute;sica en la URL del servidor de firma. */
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n criptogr&aacute;fica en la URL del servidor de firma. */
	private static final String PARAMETER_NAME_CRYPTO_OPERATION = "cop"; //$NON-NLS-1$

	private static final String HTTP_CGI = "?"; //$NON-NLS-1$
	private static final String HTTP_EQUALS = "="; //$NON-NLS-1$
	private static final String HTTP_AND = "&"; //$NON-NLS-1$

	// Parametros que necesitamos para la URL de las llamadas al servidor de firma
	private static final String PARAMETER_NAME_DOCID = "doc"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_ALGORITHM = "algo"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_FORMAT = "format"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_CERT = "cert"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_EXTRA_PARAM = "params"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_SESSION_DATA = "session"; //$NON-NLS-1$

	/** Indicador de finalizaci&oacute;n correcta de proceso. */
	private static final String SUCCESS = "OK"; //$NON-NLS-1$


	/**
	 * Prefirma el documento de una petici&oacute;n y muta la propia peticion para almacenar en ella
	 * el resultado.
	 * @param docReq Petici&oacute;n de firma de un documento.
	 * @param signerCert Certificado de firma.
	 * @param signServiceUrl URL del servicio de firma.
	 * @throws IOException Cuando no se puede obtener el documento para prefirmar.
	 * @throws AOException Cuando ocurre un error al generar la prefirma.
	 */
	public static void doPreSign(final TriphaseSignDocumentRequest docReq,
			final X509Certificate signerCert,
			final String signServiceUrl) throws IOException, AOException {

		// Empezamos la prefirma
		try {
			// Llamamos a una URL pasando como parametros los datos necesarios para
			// configurar la operacion:
			//  - Operacion trifasica (prefirma o postfirma)
			//  - Operacion criptografica (firma, cofirma o contrafirma)
			//  - Formato de firma
			//  - Algoritmo de firma a utilizar
			//  - Certificado de firma
			//  - Parametros extra de configuracion
			//  - Datos o identificador del documento a firmar
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServiceUrl).append(HTTP_CGI).
			append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_PRESIGN).append(HTTP_AND).
			append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(docReq.getCryptoOperation()).append(HTTP_AND).
			append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(normalizeSignatureFormat(docReq.getSignatureFormat())).append(HTTP_AND).
			append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(digestToSignatureAlgorithmName(docReq.getMessageDigestAlgorithm())).append(HTTP_AND).
			append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(Base64.encode(signerCert.getEncoded(), true)).append(HTTP_AND).
			append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(docReq.getContent());
			if (docReq.getParams() != null) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS).append(docReq.getParams());
			}

			docReq.setPartialResult(parseTriphaseResult(UrlHttpManagerFactory.getInstalledManager().readUrlByPost(urlBuffer.toString())));
			urlBuffer.setLength(0);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante: " + e, e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de prefirma al servidor: " + e, e); //$NON-NLS-1$
		}
	}

	/**
	 * Postfirma el documento de una petici&oacute;n.
	 * @param docReq Petici&oacute;n de firma de un documento.
	 * @param signerCert Certificado de firma.
	 * @param signServiceUrl URL del servicio de firma.
	 * @throws IOException Cuando no se puede obtener el documento para postfirmar.
	 * @throws AOException Cuando ocurre un error al generar la postfirma.
	 */
	public static void doPostSign(final TriphaseSignDocumentRequest docReq,
			final X509Certificate signerCert,
			final String signServiceUrl) throws IOException, AOException {

		final byte[] triSignFinalResult;
		try {
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServiceUrl).append(HTTP_CGI).
			append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_POSTSIGN).append(HTTP_AND).
			append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(docReq.getCryptoOperation()).append(HTTP_AND).
			append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(normalizeSignatureFormat(docReq.getSignatureFormat())).append(HTTP_AND).
			append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(digestToSignatureAlgorithmName(docReq.getMessageDigestAlgorithm())).append(HTTP_AND).
			append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(Base64.encode(signerCert.getEncoded(), true));

			if (docReq.getParams() != null && docReq.getParams().length() > 0) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS).append(docReq.getParams());
			}

			// Datos de sesion en forma de properies codificado en Base64 URL SAFE
			final String sessionData = docReq.getPartialResult().toPropertiesBase64();
			if (sessionData != null) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_SESSION_DATA).append(HTTP_EQUALS)
				.append(sessionData);
			}

			if (docReq.getPartialResult().isNeedData() != null && docReq.getPartialResult().isNeedData().booleanValue()) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(docReq.getContent());
			}

			triSignFinalResult = UrlHttpManagerFactory.getInstalledManager().readUrlByPost(urlBuffer.toString());
			urlBuffer.setLength(0);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante: " + e, e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de postfirma al servidor: " + e, e); //$NON-NLS-1$
		}

		// Analizamos la respuesta del servidor
		final String stringTrimmedResult = new String(triSignFinalResult).trim();
		if (!stringTrimmedResult.startsWith(SUCCESS)) {
			throw new AOException("La firma trifasica no ha finalizado correctamente: " + new String(triSignFinalResult)); //$NON-NLS-1$
		}

		// Los datos no se devuelven, se quedan en el servidor
		try {
			docReq.setResult(Base64.decode(stringTrimmedResult.replace(SUCCESS + " NEWID=", ""), true)); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final IOException e) {
			Logger.getLogger("es.gob.afirma").warning("El resultado de NEWID del servidor no estaba en Base64: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			throw new AOException("El resultado devuelto por el servidor no es correcto", e); //$NON-NLS-1$
		}
	}

	/**
	 * Transforma el nombre de un algoritmo de huella digital en uno de firma que
	 * utilice ese mismo algoritmo de huella digital y un cifrado RSA.
	 * @param digestAlgorithm Algoritmo de huella digital.
	 * @return Nombre del algoritmo de firma.
	 */
	private static String digestToSignatureAlgorithmName(final String digestAlgorithm) {
		return digestAlgorithm.replace("-", "").toUpperCase() + "withRSA";  //$NON-NLS-1$ //$NON-NLS-2$//$NON-NLS-3$
	}

	/**
	 * Normalizamos el nombre del formato de firma.
	 * @param format Formato de firma.
	 * @return Nombre de formato normalizado o el mismo formato de entrada si no se ha encontrado correspondencia.
	 */
	private static String normalizeSignatureFormat(final String format) {
		String normalizeFormat = format;
		if (format.equalsIgnoreCase("pdf") || format.equalsIgnoreCase("pades")) { //$NON-NLS-1$ //$NON-NLS-2$
			normalizeFormat = AOSignConstants.SIGN_FORMAT_PADES;
		} else if (format.equalsIgnoreCase("cades")) { //$NON-NLS-1$
			normalizeFormat = AOSignConstants.SIGN_FORMAT_CADES;
		} else if (format.toLowerCase().contains("xades")) { //$NON-NLS-1$
			normalizeFormat = AOSignConstants.SIGN_FORMAT_XADES;
		}
		return normalizeFormat;
	}

	private static TriphaseConfigDataBean parseTriphaseResult(final byte[] triphaseResult) throws IOException {

		final TriphaseConfigDataBean triphaseConfig;

		final byte[] triphaseResponse = Base64.decode(triphaseResult, 0, triphaseResult.length, true);

		// Comprobamos si la respuesta es de tipo proporties o XML (las 2 variantes que ha sufrido el firmador trifásico)
		if (isXML(triphaseResponse)) {
			triphaseConfig = loadTriphaseResponseAsXML(triphaseResponse);
		}
		else {
			triphaseConfig = loadTriphaseResponseAsProperties(triphaseResponse);
		}

		return triphaseConfig;
	}

	private static boolean isXML(final byte[] triphaseResponse) {
		try {
			DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(triphaseResponse));
		}
		catch (final Exception e) {
			return false;
		}
		return true;
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
	 *    &lt;param n="PK1"&gt;EMijB9pJ0lj27Xqov[...]RnCM=&lt;/param&gt;
	 *   &lt;/firma&gt;
	 *  &lt;/firmas&gt;
	 * &lt;/xml&gt;
	 * </pre>
	 * @param triphaseResponse Texto XML con la informaci&oacute;n del mensaje.
	 * @return Objeto con la informaci&oacute;n trif&aacute;sica de firma.
	 * @throws IOException Cuando hay problemas en el tratamiento de datos. */
	private static TriphaseConfigDataBean loadTriphaseResponseAsXML(final byte[] triphaseResponse) throws IOException {
		if (triphaseResponse == null) {
			throw new IllegalArgumentException("El XML de entrada no puede ser nulo"); //$NON-NLS-1$
		}

		final InputStream is = new ByteArrayInputStream(triphaseResponse);
		Document doc;
		try {
			doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(is);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al cargar la respuesta XML: " + e + "\n" + new String(triphaseResponse)); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			throw new IOException("Error al cargar la respuesta XML: " + e, e); //$NON-NLS-1$
		}
		is.close();

		final Element rootElement = doc.getDocumentElement();
		final NodeList childNodes = rootElement.getChildNodes();

		final int idx = nextNodeElementIndex(childNodes, 0);
		if (idx == -1 || !"firmas".equalsIgnoreCase(childNodes.item(idx).getNodeName())) { //$NON-NLS-1$
			throw new IllegalArgumentException("No se encontro el nodo 'firmas' en el XML proporcionado"); //$NON-NLS-1$
		}

		final List<TriSign> signsNodes = parseSignsNode(childNodes.item(idx));

		final TriphaseConfigDataBean triphaseData = new TriphaseConfigDataBean();

		// Guardamos el numero de firmas
		triphaseData.setSignCount(Integer.valueOf(signsNodes.size()));

		//TODO: Por limitacion del sistema anterior, el parametro needData afectada a todas las firmas.
		// Obtenemos el valor del parametro needData de la primera firma y lo aplicacion a todas ellas.
		final String needData = signsNodes.get(0).getProperty("NEED_DATA"); //$NON-NLS-1$
		if (needData != null) {
			try {
				triphaseData.setNeedData(Boolean.valueOf(needData));
			}
			catch (final Exception e) {
				triphaseData.setNeedData(Boolean.TRUE);
			}
		}

		//TODO: Por limitacion del sistema anterior, el parametro needPre afectada a todas las firmas.
		// Obtenemos el valor del parametro needPre de la primera firma y lo aplicacion a todas ellas.
		final String needPre = signsNodes.get(0).getProperty("NEED_PRE"); //$NON-NLS-1$
		if (needPre != null) {
			try {
				triphaseData.setNeedPreSign(Boolean.valueOf(needPre));
			}
			catch (final Exception e) {
				triphaseData.setNeedPreSign(Boolean.TRUE);
			}
		}

		// Leemos los parametros esenciales de cada firma
		for (final TriSign triSign : signsNodes) {
			if (triSign.getProperty("PK1") != null) { //$NON-NLS-1$
				triphaseData.addPk1(triSign.getProperty("PK1")); //$NON-NLS-1$
			}
			if (triSign.getProperty("PRE") != null) { //$NON-NLS-1$
				triphaseData.addPreSign(triSign.getProperty("PRE")); //$NON-NLS-1$
			}
			if (triSign.getProperty("SESSION") != null) { //$NON-NLS-1$
				triphaseData.addSession(triSign.getProperty("SESSION")); //$NON-NLS-1$
			}
		}

		return triphaseData;
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
				final Node tmpNode = nnm.getNamedItem("Id"); //$NON-NLS-1$
				if (tmpNode != null) {
					id = tmpNode.getNodeValue();
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

	private static TriphaseConfigDataBean loadTriphaseResponseAsProperties(final byte[] triphaseResponse) throws IOException {

		final TriphaseConfigDataBean triphaseConfig = new TriphaseConfigDataBean();

		final Properties resultProperties = new Properties();

		resultProperties.load(new ByteArrayInputStream(triphaseResponse));

		int sc = 1;
		if (resultProperties.containsKey("SIGN_COUNT")) { //$NON-NLS-1$
			sc = Integer.parseInt(resultProperties.getProperty("SIGN_COUNT")); //$NON-NLS-1$
			triphaseConfig.setSignCount(new Integer(sc));
		}
		if (resultProperties.containsKey("NEED_PRE")) { //$NON-NLS-1$
			triphaseConfig.setNeedPreSign(Boolean.valueOf(resultProperties.getProperty("NEED_PRE"))); //$NON-NLS-1$
		}
		if (resultProperties.containsKey("NEED_DATA")) { //$NON-NLS-1$
			triphaseConfig.setNeedData(Boolean.valueOf(resultProperties.getProperty("NEED_DATA"))); //$NON-NLS-1$
		}
		for (int i = 0; i < sc; i++) {
			triphaseConfig.addSession(resultProperties.containsKey("SESSION." + i) ?  //$NON-NLS-1$
					Base64.encode(resultProperties.getProperty("SESSION." + i).getBytes()) :  //$NON-NLS-1$
						null);

			if (resultProperties.containsKey("PRE." + i)) { //$NON-NLS-1$
				triphaseConfig.addPreSign(resultProperties.getProperty("PRE." + i)); //$NON-NLS-1$
			}

			if (resultProperties.containsKey("PK1." + i)) { //$NON-NLS-1$
				triphaseConfig.addPk1(resultProperties.getProperty("PK1." + i)); //$NON-NLS-1$
			}
		}

		return triphaseConfig;
	}
}
