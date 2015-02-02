package es.gob.afirma.android.signfolder.proxy;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import android.util.Log;
import es.gob.afirma.android.network.AndroidUrlHttpManager;
import es.gob.afirma.android.signfolder.SFConstants;
import es.gob.afirma.core.misc.Base64;

/**
 * Gestor de comunicaciones con el servidor de portafirmas m&oacute;vil.
 *
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class CommManager {

	private static final String HTTPS = "https"; //$NON-NLS-1$

	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_DATA = "dat"; //$NON-NLS-1$

	private static final String OPERATION_PRESIGN = "0"; //$NON-NLS-1$
	private static final String OPERATION_POSTSIGN = "1"; //$NON-NLS-1$
	private static final String OPERATION_REQUEST = "2"; //$NON-NLS-1$
	private static final String OPERATION_REJECT = "3"; //$NON-NLS-1$
	private static final String OPERATION_DETAIL = "4"; //$NON-NLS-1$
	private static final String OPERATION_PREVIEW_DOCUMENT = "5"; //$NON-NLS-1$
	private static final String OPERATION_APP_LIST = "6"; //$NON-NLS-1$
	private static final String OPERATION_APPROVE = "7"; //$NON-NLS-1$
	private static final String OPERATION_PREVIEW_SIGN = "8"; //$NON-NLS-1$
	private static final String OPERATION_PREVIEW_REPORT = "9"; //$NON-NLS-1$

	/** Tiempo m&aacute;ximo que se va a esperar por una respuesta del proxy. */
	private static final int DEFAULT_CONNECTION_READ_TIMEOUT = 30000;	//TODO: Configurar

	private DocumentBuilder db;

	private int timeout = DEFAULT_CONNECTION_READ_TIMEOUT;

	private final String signFolderProxyUrl;

	private static CommManager instance = null;

	/** Obtiene una instancia de la clase.
	 * @return Gestor de comunicaciones con el Proxy. */
	public static CommManager getInstance() {
		if (instance == null) {
			instance = new CommManager(AppPreferences.getUrlProxy());
			instance.timeout = AppPreferences.getConnectionreadTimeout();
		}
		return instance;
	}

	private CommManager(final String proxyUrl) {
		this.signFolderProxyUrl = proxyUrl;

		try {
			this.db = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		} catch (final ParserConfigurationException e) {
			Log.e(SFConstants.LOG_TAG,
					"No se ha podido cargar un manejador de XML: " + e.toString()); //$NON-NLS-1$
			e.printStackTrace();
			this.db = null;
		}
	}

	/** Reinicia la confifuraci&oacute;n del gestor. */
	public static void resetConfig() {
		instance = null;
	}

	private static String prepareParam(final String param) {
		return Base64.encode(param.getBytes(), true);
	}

	private String prepareUrl(final String operation, final String dataB64UrlSafe) {

		//TODO: Eliminar
//		try {
//			Log.i(SFConstants.LOG_TAG, "XML para operacion " + operation + ":\n" +
//					new String(Base64.decode(dataB64UrlSafe, true)));
//		}
//		catch (Exception e) {
//			e.printStackTrace();
//		}

		final StringBuffer sb = new StringBuffer(this.signFolderProxyUrl);
		sb.append("?"); //$NON-NLS-1$
		sb.append(PARAMETER_NAME_OPERATION);
		sb.append("="); //$NON-NLS-1$
		sb.append(operation);
		sb.append("&"); //$NON-NLS-1$
		sb.append(PARAMETER_NAME_DATA);
		sb.append("="); //$NON-NLS-1$
		sb.append(dataB64UrlSafe);

		return sb.toString();
	}

	/**
	 * Obtiene la peticiones de firma. Las peticiones devueltas deben cumplir
	 * las siguientes condiciones:
	 * <ul>
	 * <li>Estar en el estado se&ntilde;alado (unresolved, signed o rejected).</li>
	 * <li>Que todos los documentos que contiene se tengan que firmar con los
	 * formatos de firma indicados (s&oacute;lo si se indica alguno)</li>
	 * <li>Que las solicitudes cumplan con los filtros establecidos. Estos
	 * filtros tendran la forma: key=value</li>
	 * </ul>
	 * @param certEncodedB64 Certificado codificado en Base64.
	 * @param signRequestState Estado de las peticiones que se desean obtener.
	 * @param signFormats Listado de formatos de firmas soportados (para que solo se
	 *                    env&iacuteM;n.
	 * @param filters
	 *            Listado de filtros que deben cumplir las peticiones
	 *            recuperadas. Los filtros soportados son:
	 *            <ul>
	 *            <li><b>orderAscDesc:</b> con valor "asc" para que sea orden
	 *            ascendente en la consulta, en cualquier otro caso ser&aacute;
	 *            descendente</li>
	 *            <li><b>initDateFilter:</b> fecha de inicio de las peticiones</li>
	 *            <li><b>endDateFilter:</b> fecha de fin de las peticiones</li>
	 *            <li><b>orderAttribute:</b> par&aacute;metro para ordenar por
	 *            una columna de la petici&oacute;n</li>
	 *            <li><b>searchFilter:</b> busca la cadena introducida en
	 *            cualquier texto de la petici&oacute;n (asunto, referencia,
	 *            etc)</li>
	 *            <li><b>labelFilter:</b> texto con el nombre de una etiqueta.
	 *            Filtra las peticiones en base a esa etiqueta, ej: "IMPORTANTE"
	 *            </li>
	 *            <li><b>applicationFilter:</b> texto con el identificador de
	 *            una aplicaci&oacute;n. Filtra las peticiones en base a la
	 *            aplicaci&oacute;n, ej: "SANCIONES"</li>
	 *            </ul>
	 * @param numPage
	 * @param pageSize
	 * @return Lista de peticiones de firma
	 * @throws SAXException
	 *             Si el XML obtenido del servidor no puede analizarse
	 * @throws IOException
	 *             Si ocurre un error de entrada / salida
	 */
	public PartialSignRequestsList getSignRequests(final String certEncodedB64,
			final String signRequestState, final String[] filters,
			final int numPage, final int pageSize) throws SAXException,
			IOException {

		final String dataB64UrlSafe = prepareParam(XmlRequestsFactory
				.createRequestListRequest(certEncodedB64, signRequestState,
						AppPreferences.getSupportedFormats(), filters, numPage,
						pageSize));

		return RequestListResponseParser.parse(getRemoteDocument(prepareUrl(
				OPERATION_REQUEST, dataB64UrlSafe)));
	}

	/** Inicia la pre-firma remota de las peticiones.
	 * @param request Petici&oacute;n de firma.
	 * @param requests Peticiones a pre-firmar.
	 * @param cert Certificado del firmante.
	 * @return Prefirmas de las peticiones enviadas.
	 * @throws IOException Si ocurre algun error durante el tratamiento de datos.
	 * @throws CertificateEncodingException Si no se puede obtener la codificaci&oacute;n del certificado.
	 * @throws SAXException Si ocurren errores analizando el XML de respuesta. */
	public TriphaseRequest[] preSignRequests(final SignRequest request,
			                                 final X509Certificate cert) throws IOException,
			                                                                    CertificateEncodingException,
			                                                                    SAXException {
		final String dataB64UrlSafe = prepareParam(
			XmlRequestsFactory.createPresignRequest(request, Base64.encode(cert.getEncoded()))
		);

		return PresignsResponseParser.parse(getRemoteDocument(prepareUrl(
				OPERATION_PRESIGN, dataB64UrlSafe)));
	}

	/**
	 * Inicia la post-firma remota de las peticiones.
	 *
	 * @param requests
	 *            Peticiones a post-firmar
	 * @param cert
	 *            Certificado del firmante
	 * @return Listado con el resultado de la operaci&oacute;n de firma de cada
	 *         petici&oacute;n.
	 * @throws IOException
	 *             Si ocurre algun error durante el proceso
	 * @throws CertificateEncodingException
	 *             Cuando el certificado est&aacute; mal codificado.
	 * @throws SAXException
	 *             Si ocurren errores analizando el XML de respuesta
	 */
	public RequestResult postSignRequests(final TriphaseRequest[] requests,
			final X509Certificate cert) throws IOException,
			CertificateEncodingException, SAXException {
		final String dataB64UrlSafe = prepareParam(XmlRequestsFactory
				.createPostsignRequest(requests,
						Base64.encode(cert.getEncoded())));

		return PostsignsResponseParser.parse(getRemoteDocument(prepareUrl(
				OPERATION_POSTSIGN, dataB64UrlSafe)));
	}

	/**
	 * Obtiene los datos de un documento.
	 *
	 * @param requestId
	 *            Identificador de la petici&oacute;n.
	 * @param certB64
	 *            Certificado codificado en base64.
	 * @return Datos del documento.
	 * @throws SAXException
	 *             Cuando se encuentra un XML mal formado.
	 * @throws IOException
	 *             Cuando existe alg&uacute;n problema en la lectura/escritura
	 *             de XML o al recuperar la respuesta del servidor.
	 * @throws CertificateEncodingException
	 *             Si no se puede obtener la codificaci&oacute;n del certificado
	 */
	public RequestDetail getRequestDetail(final String certB64,
			final String requestId) throws SAXException, IOException {

		final String dataB64UrlSafe = prepareParam(XmlRequestsFactory
				.createDetailRequest(certB64, requestId));

		return RequestDetailResponseParser.parse(getRemoteDocument(prepareUrl(
				OPERATION_DETAIL, dataB64UrlSafe)));
	}

	/**
	 * Obtiene el listado de aplicaciones para las que hay peticiones de firma.
	 *
	 * @param certB64
	 *            Certificado codificado en base64.
	 * @return Configuracion de aplicaci&oacute;n.
	 * @throws SAXException
	 *             Cuando se encuentra un XML mal formado.
	 * @throws IOException
	 *             Cuando existe alg&uacute;n problema en la lectura/escritura
	 *             de XML o al recuperar la respuesta del servidor.
	 * @throws CertificateEncodingException
	 *             Si no se puede obtener la codificaci&oacute;n del certificado
	 */
	public RequestAppConfiguration getApplicationList(final String certB64)
			throws SAXException, IOException {

		final String dataB64UrlSafe = prepareParam(
				XmlRequestsFactory.createAppListRequest(certB64));

		return ApplicationListResponseParser.parse(
				getRemoteDocument(prepareUrl(OPERATION_APP_LIST, dataB64UrlSafe)));
	}

	/**
	 * Rechaza las peticiones de firma indicadas.
	 *
	 * @param requestIds
	 *            Identificadores de las peticiones de firma que se quieren
	 *            rechazar.
	 * @param certB64
	 *            Certificado codificado en base64.
	 * @return Resultado de la operacion para cada una de las peticiones de
	 *         firma.
	 * @throws SAXException
	 *             Si el XML obtenido del servidor no puede analizarse
	 * @throws IOException
	 *             Si ocurre un error de entrada / salida
	 * @throws CertificateEncodingException
	 *             Si no se puede obtener la codificaci&oacute;n del certificado
	 */
	public RequestResult[] rejectRequests(final String[] requestIds,
			final String certB64) throws SAXException, IOException {

		final String dataB64UrlSafe = prepareParam(XmlRequestsFactory
				.createRejectRequest(requestIds, certB64));

		return RejectsResponseParser.parse(getRemoteDocument(prepareUrl(
				OPERATION_REJECT, dataB64UrlSafe)));
	}

	/** Obtiene la previsualizaci&oacute;n de un documento.
	 * @param documentId Identificador del documento.
	 * @param filename Nombre del fichero.
	 * @param mimetype MIME-Type del documento.
	 * @param certB64 Certificado codificado en base64.
	 * @return Datos del documento.
	 * @throws SAXException Cuando se encuentra un XML mal formado.
	 * @throws IOException Cuando existe alg&uacute;n problema en la lectura/escritura
	 *                     de XML o al recuperar la respuesta del servidor.
	 * @throws CertificateEncodingException Si no se puede obtener la codificaci&oacute;n del certificado. */
	public DocumentData getPreviewDocument(final String documentId,
			final String filename, final String mimetype,
			final String certB64) throws SAXException, IOException {

		return getPreview(OPERATION_PREVIEW_DOCUMENT, documentId, filename, mimetype, certB64);
	}

	/** Obtiene la previsualizaci&oacute;n de una firma.
	 * @param documentId Identificador del documento.
	 * @param filename Nombre del fichero.
	 * @param mimetype MIME-Type del documento.
	 * @param certB64 Certificado codificado en base64.
	 * @return Datos del documento.
	 * @throws SAXException Cuando se encuentra un XML mal formado.
	 * @throws IOException Cuando existe alg&uacute;n problema en la lectura/escritura
	 *                     de XML o al recuperar la respuesta del servidor.
	 * @throws CertificateEncodingException Si no se puede obtener la codificaci&oacute;n del certificado. */
	public DocumentData getPreviewSign(final String documentId,
			final String filename, final String mimetype,
			final String certB64) throws SAXException, IOException {

		return getPreview(OPERATION_PREVIEW_SIGN, documentId,
				filename, mimetype, certB64);
	}

	/** Obtiene la previsualizaci&oacute;n de un informe de firma.
	 * @param documentId Identificador del documento.
	 * @param filename Nombre del fichero.
	 * @param mimetype MIME-Type del documento.
	 * @param certB64 Certificado codificado en base64.
	 * @return Datos del documento.
	 * @throws SAXException Cuando se encuentra un XML mal formado.
	 * @throws IOException Cuando existe alg&uacute;n problema en la lectura/escritura
	 *                     de XML o al recuperar la respuesta del servidor.
	 * @throws CertificateEncodingException Si no se puede obtener la codificaci&oacute;n del certificado. */
	public DocumentData getPreviewReport(final String documentId,
			final String filename, final String mimetype,
			final String certB64) throws SAXException, IOException {

		return getPreview(OPERATION_PREVIEW_REPORT, documentId,
				filename, mimetype, certB64);
	}

	/** Obtiene la previsualizaci&oacute;n de un documento.
	 * @param operation Identificador del tipo de documento (datos, firma o informe).
	 * @param documentId Identificador del documento.
	 * @param filename Nombre del fichero.
	 * @param mimetype MIME-Type del documento.
	 * @param certB64 Certificado codificado en base64.
	 * @return Datos del documento.
	 * @throws SAXException Cuando se encuentra un XML mal formado.
	 * @throws IOException Cuando existe alg&uacute;n problema en la lectura/escritura
	 *                     de XML o al recuperar la respuesta del servidor.
	 * @throws CertificateEncodingException Si no se puede obtener la codificaci&oacute;n del certificado. */
	public DocumentData getPreview(final String operation,
			final String documentId, final String filename,
			final String mimetype, final String certB64) throws IOException {

		final String dataB64UrlSafe = prepareParam(XmlRequestsFactory
				.createPreviewRequest(documentId, certB64));

		final DocumentData docData = new DocumentData(documentId, filename, mimetype);
		docData.setDataIs(getRemoteDocumentIs(prepareUrl(operation, dataB64UrlSafe)));

		return docData;
	}

	/**
	 * Aprueba peticiones de firma (les da el visto bueno).
	 *
	 * @param requestIds
	 *            Identificador de las peticiones.
	 * @param certB64
	 *            Certificado codificado en base64.
	 * @return Resultado de la operaci&oacute;n.
	 * @throws SAXException
	 *             Cuando se encuentra un XML mal formado.
	 * @throws IOException
	 *             Cuando existe alg&uacute;n problema en la lectura/escritura
	 *             de XML o al recuperar la respuesta del servidor.
	 * @throws CertificateEncodingException
	 *             Si no se puede obtener la codificaci&oacute;n del certificado
	 */
	public RequestResult[] approveRequests(final String[] requestIds,
			final String certB64) throws SAXException, IOException {

		final String dataB64UrlSafe = prepareParam(XmlRequestsFactory
				.createApproveRequest(requestIds, certB64));

		return ApproveResponseParser.parse(getRemoteDocument(prepareUrl(
				OPERATION_APPROVE, dataB64UrlSafe)));
	}

	/**
	 * Descarga un XML remoto de una URL dada.
	 *
	 * @param url
	 *            URL de donde descargar el XML.
	 * @return &Aacute;rbol XML descargado.
	 * @throws IOException
	 *             Error en la lectura del documento.
	 * @throws SAXException
	 *             Error al parsear el XML.
	 */
	private Document getRemoteDocument(final String url) throws SAXException, IOException {

		if (url.startsWith(HTTPS)) {
			try {
				AndroidUrlHttpManager.disableSslChecks();
			}
			catch(final Exception e) {
				Log.w(SFConstants.LOG_TAG,
						"No se ha podido ajustar la confianza SSL, es posible que no se pueda completar la conexion: " + e //$NON-NLS-1$
						);
			}
		}

		final InputStream is = AndroidUrlHttpManager.getRemoteDataByPost(url, this.timeout);

//TODO: Borrar
//		byte[] data = AOUtil.getDataFromInputStream(is);
//		Log.i(SFConstants.LOG_TAG, "XML respuesta:\n" + new String(data));
//		final Document doc = this.db.parse(new ByteArrayInputStream(data));

		final Document doc = this.db.parse(is);
		is.close();

		if (url.startsWith(HTTPS)) {
			AndroidUrlHttpManager.enableSslChecks();
		}
		return doc;
	}


	/**
	 * Obtiene el flujo de entrada de los datos a descargar.
	 * @param url URL de donde descargar los datos.
	 * @return Flujo de datos para la descarga.
	 * @throws IOException
	 *             Error en la lectura del documento.
	 */
	private InputStream getRemoteDocumentIs(final String url) throws IOException {

		if (url.startsWith(HTTPS)) {
			try {
				AndroidUrlHttpManager.disableSslChecks();
			}
			catch(final Exception e) {
				Log.w(SFConstants.LOG_TAG,
						"No se ha podido ajustar la confianza SSL, es posible que no se pueda completar la conexion: " + e //$NON-NLS-1$
						);
			}
		}

		final InputStream is = AndroidUrlHttpManager.getRemoteDataByPost(url, this.timeout);

		if (url.startsWith(HTTPS)) {
			AndroidUrlHttpManager.enableSslChecks();
		}

		Log.d(SFConstants.LOG_TAG, "Se ha obtenido el flujo de entrada de los datos"); //$NON-NLS-1$

		return is;
	}

	/** Verifica si la URL de proxy configurada es correcta.
	 * @return <code>true</code> si es correcta, <code>false</code> si no lo es. */
	public boolean verifyProxyUrl() {

		boolean correctUrl = true;
		if (this.signFolderProxyUrl == null || this.signFolderProxyUrl.trim().length() == 0) {
			correctUrl = false;
		}
		else {
			try {
				new URL(this.signFolderProxyUrl).toString();
			}
			catch (final Exception e) {
				correctUrl = false;
			}
		}
		return correctUrl;
	}
}