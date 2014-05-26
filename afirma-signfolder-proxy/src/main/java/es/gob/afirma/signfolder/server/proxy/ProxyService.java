package es.gob.afirma.signfolder.server.proxy;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.ws.WebServiceException;
import javax.xml.ws.soap.SOAPFaultException;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signfolder.client.MobileApplication;
import es.gob.afirma.signfolder.client.MobileApplicationList;
import es.gob.afirma.signfolder.client.MobileDocSignInfo;
import es.gob.afirma.signfolder.client.MobileDocSignInfoList;
import es.gob.afirma.signfolder.client.MobileDocument;
import es.gob.afirma.signfolder.client.MobileDocumentList;
import es.gob.afirma.signfolder.client.MobileException;
import es.gob.afirma.signfolder.client.MobileRequest;
import es.gob.afirma.signfolder.client.MobileRequestFilter;
import es.gob.afirma.signfolder.client.MobileRequestFilterList;
import es.gob.afirma.signfolder.client.MobileRequestList;
import es.gob.afirma.signfolder.client.MobileService;
import es.gob.afirma.signfolder.client.MobileService_Service;
import es.gob.afirma.signfolder.client.MobileSignLine;
import es.gob.afirma.signfolder.client.MobileStringList;

/** Servicio Web para firma trif&aacute;sica.
 * @author Tom&aacute;s Garc&iacute;a-;er&aacute;s */
@WebServlet(urlPatterns = { "/ProxyService" }, description = "Proxy del portafirmas")
public final class ProxyService extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static final String DEFAULT_CHARSET = "utf-8";  //$NON-NLS-1$
	
	private static final String CONFIG_FILE = "config.properties"; //$NON-NLS-1$

	private static final String KEY_SIGNATURE_SERVICE = "triphase.server.url"; //$NON-NLS-1$
	
	private static final String SIGNATURE_SERVICE_URL = "TRIPHASE_SERVER_URL"; //$NON-NLS-1$
	
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_DATA = "dat"; //$NON-NLS-1$

	private static final String OPERATION_PRESIGN = "0"; //$NON-NLS-1$
	private static final String OPERATION_POSTSIGN = "1"; //$NON-NLS-1$
	private static final String OPERATION_REQUEST = "2"; //$NON-NLS-1$
	private static final String OPERATION_REJECT = "3"; //$NON-NLS-1$
	private static final String OPERATION_DETAIL = "4"; //$NON-NLS-1$
	private static final String OPERATION_DOCUMENT_PREVIEW = "5"; //$NON-NLS-1$
	private static final String OPERATION_CONFIGURING = "6"; //$NON-NLS-1$
	private static final String OPERATION_APPROVE = "7"; //$NON-NLS-1$
	private static final String OPERATION_SIGN_PREVIEW = "8"; //$NON-NLS-1$
	private static final String OPERATION_REPORT_PREVIEW = "9"; //$NON-NLS-1$
	
	private static final String CRYPTO_OPERATION_TYPE_SIGN = "sign"; //$NON-NLS-1$
	private static final String CRYPTO_OPERATION_TYPE_COSIGN = "cosign"; //$NON-NLS-1$
	private static final String CRYPTO_OPERATION_TYPE_COUNTERSIGN = "countersign"; //$NON-NLS-1$

	private static final String DATE_TIME_FORMAT = "dd/MM/yyyy  HH:mm"; //$NON-NLS-1$
	
	/** Tama&ntilde;o de la p&aacute;gina de resultados. */
	private static final String MOBILE_REJECT_DESCRIPTION = "Rechazo desde dispositivo movil"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final DocumentBuilder documentBuilder;

	private final Properties config;
	
	/** Construye un Servlet que sirve operaciones de firma trif&aacute;sica.
	 * @throws ParserConfigurationException Cuando no puede crearse un <code>DocumentBuilder</code> XML */
	@SuppressWarnings("resource")
	public ProxyService() throws ParserConfigurationException {
		this.documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		
		final InputStream configIs = ProxyService.class.getClassLoader().getResourceAsStream(CONFIG_FILE);
		if (configIs == null) {
			throw new RuntimeException("No se encuentra el fichero de configuracion del servicio: " + CONFIG_FILE); //$NON-NLS-1$
		}
		
		try {
			this.config = new Properties();
			this.config.load(configIs);
		} catch (Exception e) {
			try { configIs.close(); } catch (Exception ex) { /* No hacemos nada */ }
			throw new RuntimeException("No se ha podido cargar el fichero de configuracion del servicio: " + CONFIG_FILE); //$NON-NLS-1$
		}
		
		try {
			configIs.close();
		} catch (Exception e) {
			// No hacemos nada
		}
		
		// Si esta configurada la variable SIGNATURE_SERVICE_URL en el sistema, se utiliza en lugar de propiedad
		// interna de la aplicacion
		try {
			String systemSignatureServiceUrl = System.getProperty(SIGNATURE_SERVICE_URL);
			if (systemSignatureServiceUrl != null) {
				this.config.setProperty(KEY_SIGNATURE_SERVICE, systemSignatureServiceUrl);
				LOGGER.info("Se utilizara el servicio de firma configurado en la variable " + SIGNATURE_SERVICE_URL + " del sistema: " + systemSignatureServiceUrl);	 //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		catch (Exception e) {
			LOGGER.warning("No se ha podido recuperar la URL del servicio de firma configurado en la variable " + SIGNATURE_SERVICE_URL + " del sistema: " + e);	 //$NON-NLS-1$ //$NON-NLS-2$
		}
		
		if (!this.config.containsKey(KEY_SIGNATURE_SERVICE)) {
			throw new RuntimeException("No se ha configurado la clave con la URL del servidor de firma: " + KEY_SIGNATURE_SERVICE); //$NON-NLS-1$
		}
	}
	
	/** Realiza una operaci&oacute;n de firma en tres fases.
	 * Acepta los siguientes c&oacute;digos de operaci&oacute;n en el par&aacute;metro <code>op</code>:
	 * <dl>
	 *  <dt>1</dt>
	 *   <dd>Firma</dd>
	 *  <dt>2</dt>
	 *   <dd>Petici&oacute;n de solicitudes</dd>
	 *  <dt>3</dt>
	 *   <dd>Rechazo de solicitudes</dd>
	 *  <dt>4</dt>
	 *   <dd>Detalle</dd>
	 *  <dt>5</dt>
	 *   <dd>Previsualizaci&oacute;n</dd>
	 *  </dl>
	 * @see HttpServlet#service(HttpServletRequest request, HttpServletResponse response) */
	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) {

		final Responser responser;
		try {
			responser = new Responser(response);
		} catch (Exception e) {
			LOGGER.severe("No se puede responder a la peticion: " + e);	 //$NON-NLS-1$
			return;
		}
		
		final String operation = request.getParameter(PARAMETER_NAME_OPERATION);
		if (operation == null) {
			responser.print(ErrorManager.genError(ErrorManager.ERROR_MISSING_OPERATION_NAME, null));
			return;
		}
		
		final String data = request.getParameter(PARAMETER_NAME_DATA);
		if (data == null) {
			responser.print(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA, null));
			return;
		}
		
		byte[] xml;
		try {
			xml = GzipCompressorImpl.gunzip(Base64.decode(data, Base64.URL_SAFE));
		}
		catch(final IOException e) {
			LOGGER.info("Los datos de entrada no estan comprimidos: " + e); //$NON-NLS-1$
			try {
				xml = Base64.decode(data, Base64.URL_SAFE);
			} catch (Exception ex) {
				LOGGER.warning("Los datos de entrada no estan correctamente codificados: " + ex); //$NON-NLS-1$
				return;
			}
		}
		
		final String ret;
		
		try {
			if (OPERATION_PRESIGN.equals(operation)) {
				ret = processPreSigns(xml);
			}
			else if (OPERATION_POSTSIGN.equals(operation)) {
				ret = processPostSigns(xml);
			}
			else if (OPERATION_REQUEST.equals(operation)) {
				ret = processRequestsList(xml);
			}
			else if (OPERATION_REJECT.equals(operation)) {
				ret = processRejects(xml);
			}
			else if (OPERATION_DETAIL.equals(operation)) {
				ret = processRequestDetail(xml);
			}
			else if (OPERATION_DOCUMENT_PREVIEW.equals(operation)) {
				ret = processDocumentPreview(xml);
			}
			else if (OPERATION_CONFIGURING.equals(operation)) {
				ret = processConfigueApp(xml);
			}
			else if (OPERATION_APPROVE.equals(operation)) {
				ret = processApproveRequest(xml);
			}
			else if (OPERATION_SIGN_PREVIEW.equals(operation)) {
				ret = processSignPreview(xml);
			}
			else if (OPERATION_REPORT_PREVIEW.equals(operation)) {
				ret = processSignReportPreview(xml);
			}
			else {
				ret = ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME, null);
			}
		}
		catch (final SAXException e) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_BAD_XML, null) + ": " + e); //$NON-NLS-1$
			LOGGER.severe(getStackTrace(e));
			responser.print(ErrorManager.genError(ErrorManager.ERROR_BAD_XML, null));
			return;
		}
		catch (final CertificateException e) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_BAD_CERTIFICATE, null) + ": " + e); //$NON-NLS-1$
			LOGGER.severe(getStackTrace(e));
			responser.print(ErrorManager.genError(ErrorManager.ERROR_BAD_CERTIFICATE, null));
			return;
		} catch (final MobileException e) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_PORTAFIRMAS, null) + ": " + e); //$NON-NLS-1$
			LOGGER.severe(getStackTrace(e));
			responser.print(ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_PORTAFIRMAS, null));
			return;
		} catch (final IOException e) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_PORTAFIRMAS, null) + ": " + e); //$NON-NLS-1$
			LOGGER.severe(getStackTrace(e));
			responser.print(ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_PORTAFIRMAS, null));
			return;
		} catch (final SOAPFaultException e) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_AUTHENTICATING_REQUEST, null) + ": " + e); //$NON-NLS-1$
			LOGGER.severe(getStackTrace(e));
			responser.print(ErrorManager.genError(ErrorManager.ERROR_AUTHENTICATING_REQUEST, null));
			return;
		} catch (final WebServiceException e) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_SERVICE, null) + ": " + e); //$NON-NLS-1$
			LOGGER.severe(getStackTrace(e));
			responser.print(ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_SERVICE, null));
			return;
		} catch (final Exception e) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_UNKNOWN_ERROR, null) + ": " + e); //$NON-NLS-1$
			responser.print(ErrorManager.genError(ErrorManager.ERROR_UNKNOWN_ERROR, null));
			LOGGER.severe(getStackTrace(e));
			return;
		}
		
		responser.print(ret);
		return;
	}

	private static String getStackTrace(Throwable t) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		t.printStackTrace(new PrintWriter(baos));
		return new String(baos.toByteArray());
	}
	
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		super.doGet(req, resp);
	}
	
	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		super.doPost(req, resp);
	}
	
	/**
	 * Procesa las peticiones de prefirma. Se realiza la prefirma de cada uno de los documentos de las peticiones indicadas.
	 * Si se produce alg&uacute;n error al procesar un documento de alguna de las peticiones, se establece como incorrecta
	 * la petici&oacute;n al completo.
	 * @param xmlData XML con los datos para el proceso de las prefirmas.
	 * @return XML con el resultado a la petici&oacute;n de prefirma.
	 * @throws SAXException Cuando ocurre alg&uacute;n error al procesar los XML.
	 * @throws IOException Cuando ocurre algun problema de comunicaci&oacute;n con el servidor.
	 * @throws CertificateException Cuando ocurre alg&uacute;n problema con el certificado de firma.
	 */
	private String processPreSigns(final byte[] xml) throws SAXException, IOException, CertificateException {

		final Document xmlDoc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final TriphaseRequestBean triRequests = SignRequestsParser.parse(xmlDoc);

		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();
		
		// Prefirmamos cada uno de los documentos de cada una de las peticiones. Si falla la prefirma de un documento, se da por erronea
		// la prefirma de toda la peticion
		for (TriphaseRequest singleRequest : triRequests) {
			
			try {
				MobileDocumentList downloadedDocs = service.getDocumentsToSign(triRequests.getCertificate().getEncoded(), singleRequest.getRef());
				if (singleRequest.size() != downloadedDocs.getDocument().size()) {
					throw new Exception("No se han recuperado tantos documentos para la peticion '" + //$NON-NLS-1$
							singleRequest.getRef() + "'como los indicados en la propia peticion"); //$NON-NLS-1$
				}
				
				// Prefirmamos cada documento de la peticion
				for (TriphaseSignDocumentRequest docRequest : singleRequest) {
					
					LOGGER.info(" == PREFIRMA == ");			
					
					// Buscamos para la prefirma el documento descargado que corresponde para la peticion
					// de firma del documento actual
					for (MobileDocument downloadedDoc : downloadedDocs.getDocument()) {
						if (downloadedDoc.getIdentifier().equals(docRequest.getId())) {

							docRequest.setCryptoOperation(normalizeOperationType(downloadedDoc.getOperationType()));

							// Del servicio remoto obtener los parametros de configuracion, tal como deben pasarse al MiniApplet
							// Lo pasamos a base 64 URL_SAFE para que no afecten al envio de datos 
							docRequest.setParams(Base64.encodeBytes(downloadedDoc.getSignatureParameters().getValue().getBytes(), Base64.URL_SAFE));
							
							final Object content = downloadedDoc.getData().getValue().getContent();
							if (content instanceof ByteArrayInputStream) {
								docRequest.setContent(Base64.encodeBytes(AOUtil.getDataFromInputStream((ByteArrayInputStream) content), Base64.URL_SAFE));
							}
							else if (content instanceof String) {
								docRequest.setContent((String) content);
							}
							else {
								throw new IOException("No se puede manejar el tipo de objeto devuelto por el servicio de prefirma de documentos: " + content); //$NON-NLS-1$
							}
							break;
						}
					}
					if (docRequest.getContent() == null) {
						throw new Exception("No se encontro correlacion entre los documentos declarados en la peticion y los documentos descargados"); //$NON-NLS-1$
					}

					doPreSign(triRequests.getCertificate(), docRequest);
				}
			} catch (final Exception mex) {
				LOGGER.warning("Error en la prefirma de la peticion " + //$NON-NLS-1$
						singleRequest.getRef() + ": " + mex); //$NON-NLS-1$
				singleRequest.setStatusOk(false);
				mex.printStackTrace();
			}
		}
		return XmlResponsesFactory.createPresignResponse(triRequests);
	}

	/**
	 * Normalizamos el nombre del tipo de operaci&oacute;n criptogr&aacute;fica.. 
	 * @param operationType Tipo de operaci&oacute;n.
	 * @return Nombre del tipo de operaci&oacute;n normalizado o el mismo de entrada
	 * si no se ha encontrado correspondencia.
	 */
	private static String normalizeOperationType(final String operationType) {
		String normalizedOp = operationType;
		if ("firmar".equalsIgnoreCase(normalizedOp)) { //$NON-NLS-1$
			normalizedOp = CRYPTO_OPERATION_TYPE_SIGN;
		} else if ("cofirmar".equalsIgnoreCase(normalizedOp)) { //$NON-NLS-1$
			normalizedOp = CRYPTO_OPERATION_TYPE_COSIGN;
		} else if ("contrafirmar".equalsIgnoreCase(normalizedOp)) { //$NON-NLS-1$
			normalizedOp = CRYPTO_OPERATION_TYPE_COUNTERSIGN;
		}
		
		return normalizedOp;
	}
	
	/**
	 * Descarga un documento del portafirmas, lo prefirma y devuelve la informacion generada.
	 * @return Prefirma del documento.
	 * @throws AOException Cuando ocurre un error al realizar la prefirma.
	 * @throws IOException Cuando existe un problema de comunicaci&oacute;n con el servidor de firma.
	 */
	private void doPreSign(final X509Certificate cert, final TriphaseSignDocumentRequest docRequest) throws IOException, AOException {

		TriSigner.doPreSign(docRequest, cert, this.config.getProperty(KEY_SIGNATURE_SERVICE));
	}

	/**
	 * Procesa las peticiones de postfirma. Se realiza la postfirma de cada uno de los documentos de las peticiones indicadas.
	 * Si se produce alg&uacute;n error al procesar un documento de alguna de las peticiones, se establece como incorrecta
	 * la petici&oacute;n al completo.
	 * @param xmlData XML con los datos para el proceso de las prefirmas.
	 * @return XML con el resultado a la petici&oacute;n de prefirma.
	 * @throws SAXException Cuando ocurre alg&uacute;n error al procesar los XML.
	 * @throws IOException Cuando ocurre algun problema de comunicaci&oacute;n con el servidor.
	 * @throws CertificateException Cuando ocurre alg&uacute;n problema con el certificado de firma.
	 */
	private String processPostSigns(final byte[] xml) throws SAXException, IOException, CertificateException {

		final Document xmlDoc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final TriphaseRequestBean triRequests = SignRequestsParser.parse(xmlDoc);

		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();

		// Postfirmamos cada uno de los documentos de cada una de las peticiones. Si falla la
		// postfirma de un solo documento, se da por erronea la postfirma de toda la peticion
		for (TriphaseRequest triRequest : triRequests) {
			
			LOGGER.info(" == POSTFIRMA == ");
			
			// Sustituir. Algunos formatos de firma no requeriran que se vuelva a descargar el
			// documento. Solo los descargaremos si es necesario para al menos una de las firmas.
			
			// Tomamos nota de que firmas requieren el documento original
			final Set<String> requestNeedContent = new HashSet<String>();
			for (TriphaseSignDocumentRequest docRequest: triRequest) {
				if (Boolean.TRUE.equals(docRequest.getPartialResult().isNeedData())) {
					requestNeedContent.add(docRequest.getId());
				}
			}
			
			// Descargamos los documentos originales si los necesitamos
			MobileDocumentList downloadedDocs = null;
			if (!requestNeedContent.isEmpty()) {
				try {
					downloadedDocs = service.getDocumentsToSign(triRequests.getCertificate().getEncoded(), triRequest.getRef());
				} catch (Exception ex) {
					LOGGER.warning("Ocurrio un error al descargar los documentos de la peticion " + triRequest.getRef() + ": " + ex);  //$NON-NLS-1$//$NON-NLS-2$
					triRequest.setStatusOk(false);
					continue;
				}
			}
			
			// Para cada documento, le asignamos su documento (si es necesario) y lo postfirmamos
			try {
				for (TriphaseSignDocumentRequest docRequest : triRequest) {
					
					// Asignamos el documento a la peticion si es necesario
					if (downloadedDocs != null && requestNeedContent.contains(docRequest.getId())) {
						// Buscamos para la postfirma el documento descargado que corresponde para la peticion
						// de firma del documento actual
						for (MobileDocument downloadedDoc : downloadedDocs.getDocument()) {
							if (downloadedDoc.getIdentifier().equals(docRequest.getId())) {
								//								if (content instanceof ByteArrayInputStream) {
								//								docRequest.setContent(Base64.encodeBytes(AOUtil.getDataFromInputStream((ByteArrayInputStream) content), Base64.URL_SAFE));
								//							}
								docRequest.setContent((String) downloadedDoc.getData().getValue().getContent());
							}
						}
					}
					
					LOGGER.info("Parametros en la postfirma:\n" + docRequest.getParams());
					
					
					doPostSign(triRequests.getCertificate(), docRequest);
				}
			} catch (Exception ex) {
				LOGGER.warning("Ocurrio un error al postfirmar un documento: " + ex);  //$NON-NLS-1$
				ex.printStackTrace();
				triRequest.setStatusOk(false);
				continue;
			}
			
			// Guardamos las firmas de todos los documentos de cada peticion
			try {
				service.saveSign(triRequests.getCertificate().getEncoded(),
						triRequest.getRef(), transformToWsParams(triRequest));
			} catch (Exception ex) {
				LOGGER.warning(
						"Ocurrio un error al guardar la peticion de firma " + triRequest.getRef() + //$NON-NLS-1$
						": " + ex); //$NON-NLS-1$
				triRequest.setStatusOk(false);
			}
		}
		
		return XmlResponsesFactory.createPostsignResponse(triRequests);
	}

	/**
	 * Transforma una peticion de tipo TriphaseRequest en un MobileDocSignInfoList.
	 * @param req Petici&oacute;n de firma con el resultado asociado a cada documento.
	 * @return Listado de firmas de documentos.
	 */
	private static MobileDocSignInfoList transformToWsParams(TriphaseRequest req) {
		
		final MobileDocSignInfoList signInfoList = new MobileDocSignInfoList();
		final List<MobileDocSignInfo> list = signInfoList.getMobileDocSignInfo();
		
		MobileDocSignInfo signInfo;
		for (TriphaseSignDocumentRequest docReq : req) {
			signInfo = new MobileDocSignInfo();
			signInfo.setDocumentId(docReq.getId());
			signInfo.setSignFormat(docReq.getSignatureFormat());
			signInfo.setSignature(new DataHandler(
					new ByteArrayDataSource(docReq.getResult(), null)));
			list.add(signInfo);
		}
		
		return signInfoList;
	}
	
	/**
	 * Descarga un documento del portafirmas, lo postfirma y actualiza la petici&oacute;n de firma del
	 * documento pasado por par&aacute;metro con el resultado de la postfirma.
	 * @param cert Certificado de firma.
	 * @param docRequest Petici&oacute;n de firma de un documento.
	 * @throws AOException 
	 * @throws IOException 
	 */
	private void doPostSign(final X509Certificate cert, final TriphaseSignDocumentRequest docRequest) throws IOException, AOException {
		TriSigner.doPostSign(docRequest, cert, this.config.getProperty(KEY_SIGNATURE_SERVICE));
	}

	/**
	 * Procesa la petici&oacute;n de un listado de peticiones de firma.
	 * @param xmlData XML con la solicitud. 
	 * @return XML con la respuesta a la petici&oacute;n.
	 * @throws SAXException Cuando ocurre alg&uacute;n error al procesar los XML.
	 * @throws IOException Cuando ocurre algun errlr al leer el XML.
	 * @throws MobileException Cuando ocurre un error al contactar con el servidor.
	 */
	private String processRequestsList(final byte[] xml) throws SAXException, IOException, MobileException {

		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final ListRequest listRequest = ListRequestParser.parse(doc);
		
		final PartialSignRequestsList signRequests = getRequestsList(listRequest);

		return XmlResponsesFactory.createRequestsListResponse(signRequests);
	}

	/**
	 * Recupera un listado de peticiones del Portafirmas a partir de la solicitud proporcionada.
	 * @param listRequest Solicitud de peticiones de firma.
	 * @return Listado de peticiones.
	 * @throws MobileException Cuando ocurre un error al contactar con el Portafirmas.
	 */
	private static PartialSignRequestsList getRequestsList(final ListRequest listRequest) throws MobileException {

		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();
		
		// Listado de formatos de firma soportados
		final MobileStringList formatsList = new MobileStringList();
		for (String supportedFormat : listRequest.getFormats()) {
			formatsList.getStr().add(supportedFormat);
		}
		
		// Listado de filtros para la consulta
		final MobileRequestFilterList filterList = new MobileRequestFilterList();
		if (listRequest.getFilters() != null) {
			for (String filterKey : listRequest.getFilters().keySet().toArray(new String[listRequest.getFilters().size()])) {
				MobileRequestFilter filter = new MobileRequestFilter();
				filter.setKey(filterKey);
				filter.setValue(listRequest.getFilters().get(filterKey));
				filterList.getRequestFilter().add(filter);
			}
		}
		
		// Solicitud de lista de peticiones
		final MobileRequestList mobileRequestsList = service.queryRequestList(
				listRequest.getCertEncoded(),
				listRequest.getState(),
				Integer.toString(listRequest.getNumPage()),
				Integer.toString(listRequest.getPageSize()),
				formatsList,
				filterList);
		
		final SimpleDateFormat dateFormater = new SimpleDateFormat("dd/MM/yyyy"); //$NON-NLS-1$
		final List<SignRequest> signRequests = new ArrayList<SignRequest>(mobileRequestsList.getSize().intValue());
		for (MobileRequest request : mobileRequestsList.getRequestList()) {
			
			final List<MobileDocument> docList = (request.getDocumentList() != null) ?
					request.getDocumentList().getDocument() : new ArrayList<MobileDocument>(); 
			
			final SignRequestDocument[] docs = new SignRequestDocument[docList.size()];
			
			try {
				for (int j = 0; j < docs.length; j++) {
					final MobileDocument doc = docList.get(j);
					docs[j] = new SignRequestDocument(
							doc.getIdentifier(),
							doc.getName(),
							doc.getSize().getValue(),
							doc.getMime(),
							doc.getOperationType(),
							doc.getSignatureType().getValue().value(),
							doc.getSignAlgorithm().getValue(),
							null); //TODO: Parametros de firma
				}
			} catch (Exception e) {
				final String id = request.getIdentifier() != null ?
						request.getIdentifier().getValue() : "null";  //$NON-NLS-1$
				LOGGER.warning("Se ha encontrado un error al analizar los datos de los documentos de la peticion con ID '" + id + "' y no se mostrara: " + e.toString()); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}
			
			signRequests.add(new SignRequest(
					request.getRequestTagId(),
					request.getSubject().getValue(),
					request.getSenders().getStr().get(0),
					request.getView(),
					dateFormater.format(request.getFentry().getValue().toGregorianCalendar().getTime()),
					request.getImportanceLevel().getValue(),
					request.getWorkflow().getValue().booleanValue(),
					request.getForward().getValue().booleanValue(),
					request.getRequestType(),
					docs
					));
		}

		return new PartialSignRequestsList(
				signRequests.toArray(new SignRequest[signRequests.size()]),
				mobileRequestsList.getSize().intValue());
	}
	
	private String processRejects(final byte[] xml) throws SAXException, IOException {
		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final RejectRequest request = RejectsRequestParser.parse(doc);

		final RequestResult[] requestResults = doReject(request);
		
		return XmlResponsesFactory.createRejectsResponse(requestResults);
	}

	/**
	 * Rechaza el listado de solicitudes indicado en la petici&oacute;n de rechazo.
	 * @param rejectRequest Petici&oacute;n de rechazo.
	 * @return Resultado del rechazo de cada solicitud.
	 * @throws MobileException Cuando se produce alg&uacute;n error durante el rechazo.
	 */
	private static RequestResult[] doReject(final RejectRequest rejectRequest) {

		final MobileService service = new MobileService_Service().getMobileServicePort();

		final List<Boolean> rejectionsResults = new ArrayList<Boolean>();
		for (final String id : rejectRequest) {
			// Si devuelve cualquier texto es que la operacion ha terminado correctamente. Por defecto,
			// devuelve el mismo identificador de la peticion, aunque no es obligatorio
			// Si falla devuelve una excepcion.  
			try {
				service.rejectRequest(rejectRequest.getCertEncoded(), id, MOBILE_REJECT_DESCRIPTION);
				rejectionsResults.add(Boolean.TRUE);
			} catch (final Exception e) {
				LOGGER.warning("Error en el rechazo de la peticion " + id + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				e.printStackTrace();
				rejectionsResults.add(Boolean.FALSE);
			}
		}

		final RequestResult[] result = new RequestResult[rejectRequest.size()];
		for (int i = 0; i < rejectRequest.size(); i++) {
			result[i] = new RequestResult(rejectRequest.get(i), rejectionsResults.get(i).booleanValue());
		}

		return result;
	}

	private String processRequestDetail(final byte[] xml) throws SAXException, IOException, MobileException {
		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final DetailRequest request = DetailRequestParser.parse(doc);

		final Detail requestDetails = getRequestDetail(request);

		return XmlResponsesFactory.createRequestDetailResponse(requestDetails);
	}

	/**
	 * Obtiene el detalle de un solicitud de firma a partir de una petici&oacute;n de detalle.
	 * @param request Petici&oacute;n que debe realizarse.
	 * @return Detalle de la solicitud.
	 * @throws MobileException Cuando ocurre un error al contactar con el Portafirmas.
	 */
	private static Detail getRequestDetail(final DetailRequest request) throws MobileException {

		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();
		
		// Solicitud de lista de peticiones
		final MobileRequest mobileRequest = service.queryRequest(request.getCertEncoded(), request.getRequestId());
				
		// Listado de documentos de la peticion
		final List<MobileDocument> mobileDocs = mobileRequest.getDocumentList().getDocument();
		final SignRequestDocument[] docs = new SignRequestDocument[mobileDocs.size()];
		for (int i = 0; i < mobileDocs.size(); i++) {
			final MobileDocument doc = mobileDocs.get(i);
			docs[i] = new SignRequestDocument(
					doc.getIdentifier(),
					doc.getName(),
					doc.getSize().getValue(),
					doc.getMime(),
					doc.getOperationType(),
					doc.getSignatureType().getValue().value(),
					doc.getSignAlgorithm().getValue(),
					null);	// TODO: Incluir parametros de firma
		}
		
		// Listado de remitentes de la peticion
		//		final Vector signLines = new Vector<String>();
		final List<MobileSignLine> mobileSignLines = mobileRequest.getSignLineList().getMobileSignLine();
		final Vector<String>[] signLines = new Vector[mobileSignLines.size()];
		for (int i = 0; i < signLines.length; i++) {
			signLines[i] = new Vector<String>();
			for (final String line : mobileSignLines.get(i).getMobileSignerList().getValue().getStr()) {
				signLines[i].add(line);
			}
		}
		
		final SimpleDateFormat df = new SimpleDateFormat(DATE_TIME_FORMAT);
		
		// Creamos el objeto de detalle
		final Detail detail = new Detail(mobileRequest.getRequestTagId());
		detail.setApp(mobileRequest.getApplication() != null ? mobileRequest.getApplication().getValue() : ""); //$NON-NLS-1$
		detail.setDate(df.format(mobileRequest.getFentry().getValue().toGregorianCalendar().getTime()));
		detail.setSubject(mobileRequest.getSubject().getValue());
		detail.setWorkflow(mobileRequest.getWorkflow().getValue().booleanValue());
		detail.setForward(mobileRequest.getForward().getValue().booleanValue());
		detail.setPriority(mobileRequest.getImportanceLevel().getValue());
		detail.setType(mobileRequest.getRequestType());
		detail.setRef(mobileRequest.getRef() != null ? mobileRequest.getRef().getValue() : ""); //$NON-NLS-1$
		detail.setSenders(mobileRequest.getSenders().getStr().toArray(new String[mobileRequest.getSenders().getStr().size()]));
		detail.setDocs(docs);
		detail.setSignLines(signLines);

		return detail;
	}
	
	private String processDocumentPreview(final byte[] xml) throws SAXException, IOException, MobileException {
		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final PreviewRequest request = PreviewRequestParser.parse(doc);
		
		return XmlResponsesFactory.createPreviewResponse(previewDocument(request));
	}
	
	private String processSignPreview(final byte[] xml) throws SAXException, IOException, MobileException {
		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final PreviewRequest request = PreviewRequestParser.parse(doc);

		return XmlResponsesFactory.createPreviewResponse(previewSign(request));
	}

	private String processSignReportPreview(final byte[] xml) throws SAXException, IOException, MobileException {
		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final PreviewRequest request = PreviewRequestParser.parse(doc);

		return XmlResponsesFactory.createPreviewResponse(previewSignReport(request));
	}
	
	/**
	 * Recupera los datos para la previsualizaci&oacute;n de un documento a partir del identificador
	 * del documento.
	 * @param request Petici&oacute;n de visualizaci&oacute;n de un documento.
	 * @return Datos necesarios para la previsualizaci&oacute;n.
	 * @throws MobileException Cuando ocurre un error al contactar con el Portafirmas.
	 * @throws IOException Cuando no ha sido posible leer el documento.
	 */
	private static DocumentData previewDocument(final PreviewRequest request) throws MobileException, IOException {
		return buildDocumentData(new MobileService_Service().getMobileServicePort()
				.documentPreview(request.getCertEncoded(), request.getDocId()));
	}
	
	/**
	 * Recupera los datos para la descarga de una firma a partir del hash del documento firmado.
	 * @param request Petici&oacute;n de visualizaci&oacute;n de un documento.
	 * @return Datos necesarios para la previsualizaci&oacute;n.
	 * @throws MobileException Cuando ocurre un error al contactar con el Portafirmas.
	 * @throws IOException Cuando no ha sido posible leer el documento.
	 */
	private static DocumentData previewSign(final PreviewRequest request) throws MobileException, IOException {
		return buildDocumentData(new MobileService_Service().getMobileServicePort()
				.signPreview(request.getCertEncoded(), request.getDocId()));
	}
	
	/**
	 * Recupera los datos para la visualizaci&oacute;n de un informe de firma a partir del hash del
	 * documento firmado.
	 * @param request Petici&oacute;n de visualizaci&oacute;n de un documento.
	 * @return Datos necesarios para la previsualizaci&oacute;n.
	 * @throws MobileException Cuando ocurre un error al contactar con el Portafirmas.
	 * @throws IOException Cuando no ha sido posible leer el documento.
	 */
	private static DocumentData previewSignReport(final PreviewRequest request) throws MobileException, IOException {
	return buildDocumentData(new MobileService_Service().getMobileServicePort()
				.reportPreview(request.getCertEncoded(), request.getDocId()));
	}
	
	/**
	 * Construye un objeto documento para previsualizaci&oacute;n.
	 * @param document Datos del documento.
	 * @return Contenido y metadatos del documento.
	 * @throws IOException Cuando ocurre un error en la lectura de los datos.
	 */
	private static DocumentData buildDocumentData(final MobileDocument document) throws IOException {

		final String contentB64;
		final Object content = document.getData().getValue().getContent();
		if (content instanceof ByteArrayInputStream) {
			contentB64 = Base64.encode(AOUtil.getDataFromInputStream((ByteArrayInputStream) content));
		}
		else if (content instanceof String) {
			contentB64 = (String) content;
		}
		else {
			throw new IOException("No se puede manejar el tipo de objeto devuelto por el servicio de previsualizacion de documentos: " + content); //$NON-NLS-1$
		}

		return new DocumentData(
				document.getIdentifier(),
				document.getName(),
				document.getMime(),
				contentB64);
	}
	
	private String processConfigueApp(final byte[] xml) throws SAXException, IOException, MobileException {
		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final GenericRequest request = ConfigurationRequestParser.parse(doc);

		return XmlResponsesFactory.createConfigurationResponse(loadConfiguration(request));
	}
	
	/**
	 * Recupera los datos de confguracion de la aplicaci&oacute;n. Hasta el momento:
	 * <ul>
	 * <li>Listado de aplicaciones.</li>
	 * </ul>
	 * @param request Datos gen&eacute;ricos necesarios para la petici&oacute;n.
	 * @return Configuraci&oracute;n de la aplicaci&oacute;n.
	 * @throws MobileException Cuando ocurre un error al contactar con el Portafirmas.
	 * @throws IOException Cuando no ha sido posible leer el documento.
	 */
	private static AppConfiguration loadConfiguration(final GenericRequest request) throws MobileException, IOException {

		final MobileService service = new MobileService_Service().getMobileServicePort();
		final MobileApplicationList appList = service.queryApplicationsMobile(request.getCertEncoded());

		final List<String> appIds = new ArrayList<String>();
		final List<String> appNames = new ArrayList<String>();
		for (MobileApplication app : appList.getApplicationList()) {
			appIds.add(app.getId());
			appNames.add(app.getName() != null ? app.getName() : app.getId());
		}

		return new AppConfiguration(appIds, appNames); 
	}
	

	private String processApproveRequest(final byte[] xml) throws SAXException, IOException {
		
		final Document xmlDoc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final ApproveRequestList appRequests = ApproveRequestParser.parse(xmlDoc);

		return XmlResponsesFactory.createApproveRequestsResponse(approveRequests(appRequests));
	}

	private static ApproveRequestList approveRequests(final ApproveRequestList appRequests) {
		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();
		
		for (ApproveRequest appReq : appRequests) {
			try {
				service.approveRequest(appRequests.getCertEncoded(), appReq.getRequestId());
			} catch (MobileException e) {
				appReq.setOk(false);
			}
		}
		return appRequests;
	}
	
	/**
	 * Permite enviar respuestas al cliente de un servicio.
	 */
	private final class Responser {
		
		final PrintWriter out;
		
		/**
		 * Crea el Responser enlaz&aacute;ndolo con una petici&oacute;n concreta al servicio. 
		 * @param response Manejador para el env&iacute;o de la respuesta.
		 * @throws IOException Cuando no se puede escribir una respuesta.
		 */
		public Responser(final HttpServletResponse response) throws IOException {
			response.setCharacterEncoding(DEFAULT_CHARSET);
			this.out = response.getWriter();
		}
		
		/**
		 * Imprime una respuesta en la salida del servicio y cierra el flujo.
		 * @param message Mensaje que imprimir como respuesta.
		 */
		public void print(final String message) {
			this.out.println(message);
			this.out.close();
		}
	}
}
