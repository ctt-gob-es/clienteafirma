package es.gob.afirma.signfolder.server.proxy;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.security.cert.CertificateException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.bind.JAXBElement;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.ws.WebServiceException;
import javax.xml.ws.soap.SOAPFaultException;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.TriphaseData;
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
public final class ProxyService extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static final String DEFAULT_CHARSET = "utf-8";  //$NON-NLS-1$

	private static final String CONFIG_FILE = "config.properties"; //$NON-NLS-1$

	private static final String KEY_SIGNATURE_SERVICE = "triphase.server.url"; //$NON-NLS-1$

	private static final String KEY_FORCED_EXTRAPARAMS = "forced.extraparams"; //$NON-NLS-1$

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

	private static final String CRYPTO_PARAM_NEED_DATA = "NEED_DATA"; //$NON-NLS-1$

	private static final String JAVA_HTTP_PORT_VARIABLE = "tomcat.httpport"; //$NON-NLS-1$
	private static final String TOMCAT_HTTP_PORT_VARIABLE = "${" + JAVA_HTTP_PORT_VARIABLE + "}"; //$NON-NLS-1$ //$NON-NLS-2$

	private static final String DATE_TIME_FORMAT = "dd/MM/yyyy  HH:mm"; //$NON-NLS-1$

	static final Logger LOGGER;

	private final DocumentBuilder documentBuilder;

	private final Properties config;

	private String signatureServiceUrl = null;

	static {
//		final InputStream is = ProxyService.class.getResourceAsStream("/log.properties"); //$NON-NLS-1$
//		try {
//			LogManager.getLogManager().readConfiguration(is);
//		} catch (final Exception e) {
//			Logger.getLogger("es.gob.afirma").log(Level.WARNING, "Error al cargar el fichero de configuracion del log", e); //$NON-NLS-1$ //$NON-NLS-2$
//		} finally {
//			if (is != null) {
//				try { is.close(); } catch (final IOException e) {
//					Logger.getLogger("es.gob.afirma").log(Level.WARNING, "No se pudo cerrar el fichero de configuracion del log", e); //$NON-NLS-1$ //$NON-NLS-2$
//				}
//			}
//		}
//		LOGGER.info("LoggerManager cargado"); //$NON-NLS-1$

		LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	}


	/** Construye un Servlet que sirve operaciones de firma trif&aacute;sica.
	 * @throws ParserConfigurationException Cuando no puede crearse un <code>DocumentBuilder</code> XML */
	public ProxyService() throws ParserConfigurationException {
		this.documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();

		LOGGER.info("Cargamos el fichero de configuracion del Proxy: " + CONFIG_FILE); //$NON-NLS-1$

		final InputStream configIs = ProxyService.class.getClassLoader().getResourceAsStream(CONFIG_FILE);
		if (configIs == null) {
			throw new RuntimeException("No se encuentra el fichero de configuracion del servicio: " + CONFIG_FILE); //$NON-NLS-1$
		}

		try {
			this.config = new Properties();
			this.config.load(configIs);
		} catch (final Exception e) {
			try { configIs.close(); } catch (final Exception ex) { /* No hacemos nada */ }
			throw new RuntimeException("No se ha podido cargar el fichero de configuracion del servicio: " + CONFIG_FILE); //$NON-NLS-1$
		}

		try {
			configIs.close();
		} catch (final Exception e) {
			// No hacemos nada
		}

		LOGGER.info("Las propiedades cargadas del fichero de configuracion son:"); //$NON-NLS-1$
		for (final String key : this.config.keySet().toArray(new String[this.config.size()])) {
			LOGGER.info(key + ": " + this.config.getProperty(key)); //$NON-NLS-1$
		}
		LOGGER.info("---"); //$NON-NLS-1$

		// Expandimos la propiedad


		// Si esta configurada la variable SIGNATURE_SERVICE_URL en el sistema, se utiliza en lugar de propiedad
		// interna de la aplicacion
		try {
			final String systemSignatureServiceUrl = System.getProperty(SIGNATURE_SERVICE_URL);
			if (systemSignatureServiceUrl != null) {
				this.config.setProperty(KEY_SIGNATURE_SERVICE, systemSignatureServiceUrl);
				LOGGER.info("Se sustituye la URL del servicio de firma por la configurada en la propiedad del sistema " + SIGNATURE_SERVICE_URL + " con el valor: " + systemSignatureServiceUrl);	 //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		catch (final Exception e) {
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

		LOGGER.info("Peticion al proxy Portafirmas"); //$NON-NLS-1$

		final Responser responser;
		try {
			responser = new Responser(response);
		} catch (final Exception e) {
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
			LOGGER.severe("No se han proporcionado los datos"); //$NON-NLS-1$
			responser.print(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA, null));
			return;
		}

		byte[] xml;
		try {
			xml = GzipCompressorImpl.gunzip(Base64.decode(data, true));
		}
		catch(final IOException e) {
			LOGGER.info("Los datos de entrada no estan comprimidos: " + e); //$NON-NLS-1$
			try {
				xml = Base64.decode(data, true);
			} catch (final Exception ex) {
				LOGGER.warning("Los datos de entrada no estan correctamente codificados: " + ex); //$NON-NLS-1$
				return;
			}
		}

		LOGGER.info("XML de la peticion:\n" + new String(xml)); //$NON-NLS-1$

		final Object ret;
		try {
			if (OPERATION_PRESIGN.equals(operation)) {
				LOGGER.info("Solicitud de prefirma"); //$NON-NLS-1$
				ret = processPreSigns(xml);
			}
			else if (OPERATION_POSTSIGN.equals(operation)) {
				LOGGER.info("Solicitud de postfirma"); //$NON-NLS-1$
				ret = processPostSigns(xml);
			}
			else if (OPERATION_REQUEST.equals(operation)) {
				LOGGER.info("Solicitud del listado de peticiones"); //$NON-NLS-1$
				ret = processRequestsList(xml);
			}
			else if (OPERATION_REJECT.equals(operation)) {
				LOGGER.info("Solicitud de rechazo peticiones"); //$NON-NLS-1$
				ret = processRejects(xml);
			}
			else if (OPERATION_DETAIL.equals(operation)) {
				LOGGER.info("Solicitud de detalle de una peticion"); //$NON-NLS-1$
				ret = processRequestDetail(xml);
			}
			else if (OPERATION_DOCUMENT_PREVIEW.equals(operation)) {
				LOGGER.info("Solicitud de previsualizacion de un documento"); //$NON-NLS-1$
				ret = processDocumentPreview(xml);
			}
			else if (OPERATION_CONFIGURING.equals(operation)) {
				LOGGER.info("Solicitud de la configuracion"); //$NON-NLS-1$
				ret = processConfigueApp(xml);
			}
			else if (OPERATION_APPROVE.equals(operation)) {
				LOGGER.info("Solicitud de aprobacion de una peticion"); //$NON-NLS-1$
				ret = processApproveRequest(xml);
			}
			else if (OPERATION_SIGN_PREVIEW.equals(operation)) {
				LOGGER.info("Solicitud de previsualizacion de una firma"); //$NON-NLS-1$
				ret = processSignPreview(xml);
			}
			else if (OPERATION_REPORT_PREVIEW.equals(operation)) {
				LOGGER.info("Solicitud de previsualizacion de un informe de firma"); //$NON-NLS-1$
				ret = processSignReportPreview(xml);
			}
			else {
				LOGGER.info("Se ha indicado un codigo de operacion no valido"); //$NON-NLS-1$
				ret = ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME);
			}
		} catch (final SAXException e) {
			LOGGER.log(Level.SEVERE, ErrorManager.genError(ErrorManager.ERROR_BAD_XML) + ": " + e, e); //$NON-NLS-1$
			responser.print(ErrorManager.genError(ErrorManager.ERROR_BAD_XML));
			return;
		} catch (final CertificateException e) {
			LOGGER.log(Level.SEVERE, ErrorManager.genError(ErrorManager.ERROR_BAD_CERTIFICATE) + ": " + e, e); //$NON-NLS-1$
			responser.print(ErrorManager.genError(ErrorManager.ERROR_BAD_CERTIFICATE));
			return;
		} catch (final MobileException e) {
			LOGGER.log(Level.SEVERE, ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_PORTAFIRMAS) + ": " + e, e); //$NON-NLS-1$
			responser.print(ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_PORTAFIRMAS));
			return;
		} catch (final IOException e) {
			LOGGER.log(Level.SEVERE, ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_PORTAFIRMAS) + ": " + e, e); //$NON-NLS-1$
			responser.print(ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_PORTAFIRMAS));
			return;
		} catch (final SOAPFaultException e) {
			LOGGER.log(Level.SEVERE, ErrorManager.genError(ErrorManager.ERROR_AUTHENTICATING_REQUEST) + ": " + e, e); //$NON-NLS-1$
			responser.print(ErrorManager.genError(ErrorManager.ERROR_AUTHENTICATING_REQUEST));
			return;
		} catch (final WebServiceException e) {
			LOGGER.log(Level.SEVERE, ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_SERVICE) + ": " + e, e); //$NON-NLS-1$
			responser.print(ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_SERVICE));
			return;
		} catch (final Exception e) {
			LOGGER.log(Level.SEVERE, ErrorManager.genError(ErrorManager.ERROR_UNKNOWN_ERROR) + ": " + e, e); //$NON-NLS-1$
			responser.print(ErrorManager.genError(ErrorManager.ERROR_UNKNOWN_ERROR));
			return;
		}

		if (ret instanceof InputStream) {
			LOGGER.info("La respuesta es un flujo de datos de salida"); //$NON-NLS-1$
			responser.write((InputStream) ret);
			try {
				((InputStream) ret).close();
			} catch (final IOException e) {
				LOGGER.warning("No se pudo cerrar el flujo de datos: " + e); //$NON-NLS-1$
			}
		}
		else {
			LOGGER.info("XML de respuesta:\n" + ret); //$NON-NLS-1$
			responser.print((String) ret);
		}
		LOGGER.info("Fin peticion ProxyService"); //$NON-NLS-1$
	}

	/**
	 * Procesa las peticiones de prefirma. Se realiza la prefirma de cada uno de los documentos de las peticiones indicadas.
	 * Si se produce alg&uacute;n error al procesar un documento de alguna de las peticiones, se establece como incorrecta
	 * la petici&oacute;n al completo.
	 * @param xml XML con los datos para el proceso de las prefirmas.
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

		LOGGER.info("Procesamos la peticiones que se van a prefirmar"); //$NON-NLS-1$

		// Prefirmamos cada uno de los documentos de cada una de las peticiones. Si falla la prefirma de
		// un documento, se da por erronea la prefirma de toda la peticion
		for (final TriphaseRequest singleRequest : triRequests) {

			try {
				final MobileDocumentList downloadedDocs = service.getDocumentsToSign(triRequests.getCertificate().getEncoded(), singleRequest.getRef());
				LOGGER.info("Recuperamos los documentos de la peticion"); //$NON-NLS-1$
				if (singleRequest.size() != downloadedDocs.getDocument().size()) {
					LOGGER.info("No se han recuperado tantos documentos para la peticion " + singleRequest.getRef() + "' como los indicados en la propia peticion"); //$NON-NLS-1$ //$NON-NLS-2$
					throw new Exception("No se han recuperado tantos documentos para la peticion '" + //$NON-NLS-1$
							singleRequest.getRef() + "' como los indicados en la propia peticion"); //$NON-NLS-1$
				}

				// Prefirmamos cada documento de la peticion
				for (final TriphaseSignDocumentRequest docRequest : singleRequest) {

					LOGGER.info(" == PREFIRMA == "); //$NON-NLS-1$

					// Buscamos para la prefirma el documento descargado que corresponde para la peticion
					// de firma del documento actual
					for (final MobileDocument downloadedDoc : downloadedDocs.getDocument()) {
						if (downloadedDoc.getIdentifier().equals(docRequest.getId())) {

							LOGGER.info(" Procesamos documento con el id: " + downloadedDoc.getIdentifier()); //$NON-NLS-1$

							docRequest.setCryptoOperation(downloadedDoc.getOperationType());

							// Del servicio remoto obtener los parametros de configuracion, tal como deben pasarse al cliente
							// Lo pasamos a base 64 URL_SAFE para que no afecten al envio de datos
							final String extraParams = downloadedDoc.getSignatureParameters() != null ? downloadedDoc.getSignatureParameters().getValue() : null;

							if (extraParams != null) {
								docRequest.setParams(Base64.encode(extraParams.getBytes(), true));
							}

							final DataHandler dataHandler = downloadedDoc.getData() != null ? downloadedDoc.getData().getValue() : null;
							if (dataHandler == null) {
								throw new IllegalArgumentException("No se han recuperado los datos del documento"); //$NON-NLS-1$
							}
							final Object content = dataHandler.getContent();
							if (content instanceof InputStream) {
								docRequest.setContent(Base64.encode(AOUtil.getDataFromInputStream((InputStream) content), true));
							}
							else if (content instanceof String) {
								docRequest.setContent(((String) content).replace('+', '-').replace('/', '_'));
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

					LOGGER.info("Procedemos a realizar la prefirma"); //$NON-NLS-1$
					TriSigner.doPreSign(
							docRequest,
							triRequests.getCertificate(),
							getSignatureServiceUrl(this.config),
							getForcedExtraParams(this.config));
				}
			} catch (final Exception mex) {
				LOGGER.log(Level.SEVERE, "Error en la prefirma de la peticion " + //$NON-NLS-1$
						singleRequest.getRef() + ": " + mex, mex); //$NON-NLS-1$
				singleRequest.setStatusOk(false);
				singleRequest.setThrowable(mex);
			}
		}
		return XmlResponsesFactory.createPresignResponse(triRequests);
	}



	/**
	 * Procesa las peticiones de postfirma. Se realiza la postfirma de cada uno de los documentos de las peticiones indicadas.
	 * Si se produce alg&uacute;n error al procesar un documento de alguna de las peticiones, se establece como incorrecta
	 * la petici&oacute;n al completo.
	 * @param xml XML con los datos para el proceso de las prefirmas.
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

		LOGGER.info("Procesamos la peticiones que se van a postfirmar"); //$NON-NLS-1$

		// Postfirmamos cada uno de los documentos de cada una de las peticiones. Si falla la
		// postfirma de un solo documento, se da por erronea la postfirma de toda la peticion
		for (final TriphaseRequest triRequest : triRequests) {

			LOGGER.info(" == POSTFIRMA == "); //$NON-NLS-1$

			// Sustituir. Algunos formatos de firma no requeriran que se vuelva a descargar el
			// documento. Solo los descargaremos si es necesario para al menos una de las firmas.

			// Tomamos nota de que firmas requieren el documento original
			final Set<String> requestNeedContent = new HashSet<String>();
			for (final TriphaseSignDocumentRequest docRequest: triRequest) {

				final TriphaseData triData = docRequest.getPartialResult();
				if (triData.getSignsCount() > 0 &&
						(!triData.getSign(0).getDict().containsKey(CRYPTO_PARAM_NEED_DATA) ||
						Boolean.parseBoolean(triData.getSign(0).getDict().get(CRYPTO_PARAM_NEED_DATA)))) {
					LOGGER.info("Descargamos el documento '" + docRequest.getId() + "' para su uso en la postfirma"); //$NON-NLS-1$ //$NON-NLS-2$
					requestNeedContent.add(docRequest.getId());
				}
			}

			// Descargamos los documentos originales si los necesitamos
			MobileDocumentList downloadedDocs = null;
			if (!requestNeedContent.isEmpty()) {
				try {
					downloadedDocs = service.getDocumentsToSign(triRequests.getCertificate().getEncoded(), triRequest.getRef());
				} catch (final Exception ex) {
					LOGGER.warning("Ocurrio un error al descargar los documentos de la peticion " + triRequest.getRef() + ": " + ex);  //$NON-NLS-1$//$NON-NLS-2$
					triRequest.setStatusOk(false);
					continue;
				}
			}

			// Para cada documento, le asignamos su documento (si es necesario) y lo postfirmamos
			try {
				for (final TriphaseSignDocumentRequest docRequest : triRequest) {

					// Asignamos el documento a la peticion si es necesario
					if (downloadedDocs != null && requestNeedContent.contains(docRequest.getId())) {
						// Buscamos para la postfirma el documento descargado que corresponde para la peticion
						// de firma del documento actual
						for (final MobileDocument downloadedDoc : downloadedDocs.getDocument()) {
							if (downloadedDoc.getIdentifier().equals(docRequest.getId())) {
								final Object content = downloadedDoc.getData().getValue().getContent();
								if (content instanceof InputStream) {
									docRequest.setContent(Base64.encode(AOUtil.getDataFromInputStream((InputStream) content), true));
								}
								else {
									docRequest.setContent((String) content);
								}
								// Del servicio remoto obtener los parametros de configuracion, tal como deben pasarse al cliente
								// Lo pasamos a base 64 URL_SAFE para que no afecten al envio de datos
								final String extraParams = downloadedDoc.getSignatureParameters() != null ? downloadedDoc.getSignatureParameters().getValue() : null;
								if (extraParams != null) {
									docRequest.setParams(Base64.encode(extraParams.getBytes(), true));
								}
							}
						}
					}

					LOGGER.info("Procedemos a realizar la postfirma"); //$NON-NLS-1$
					TriSigner.doPostSign(
							docRequest,
							triRequests.getCertificate(),
							getSignatureServiceUrl(this.config),
							getForcedExtraParams(this.config));
				}
			} catch (final Exception ex) {
				LOGGER.log(Level.WARNING, "Ocurrio un error al postfirmar un documento: " + ex, ex);  //$NON-NLS-1$
				triRequest.setStatusOk(false);
				continue;
			}

			LOGGER.info("Registramos el resultado en el portafirmas"); //$NON-NLS-1$

			// Guardamos las firmas de todos los documentos de cada peticion
			try {
				service.saveSign(triRequests.getCertificate().getEncoded(),
						triRequest.getRef(), transformToWsParams(triRequest));
			} catch (final Exception ex) {
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
	private static MobileDocSignInfoList transformToWsParams(final TriphaseRequest req) {

		final MobileDocSignInfoList signInfoList = new MobileDocSignInfoList();
		final List<MobileDocSignInfo> list = signInfoList.getMobileDocSignInfo();

		MobileDocSignInfo signInfo;
		for (final TriphaseSignDocumentRequest docReq : req) {
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
	 * Obtiene la URL del servicio de firma, traduciendo las variables utilizadas para
	 * su configuraci&oacute;n si es necesario.
	 * @param conf Configuraci&oacute;n del servicio.
	 * @return URL del servicio trif&aacute;asico de firma.
	 */
	private String getSignatureServiceUrl(final Properties conf) {
		if (this.signatureServiceUrl == null) {
			String url = conf.getProperty(KEY_SIGNATURE_SERVICE);
			if (url.contains(TOMCAT_HTTP_PORT_VARIABLE)) {
				url = url.replace(TOMCAT_HTTP_PORT_VARIABLE, System.getProperty(JAVA_HTTP_PORT_VARIABLE));
			}
			this.signatureServiceUrl = url;
		}

		LOGGER.info("URL del servicio de firma trifasica: " + this.signatureServiceUrl); //$NON-NLS-1$

		return this.signatureServiceUrl;
	}

	/**
	 * Obtiene el listado de par&aacute;metros extra con el que se deben de configurar
	 * todas las firmas (adem&aacute;s de los par&aacute;metros que indiquen cada una
	 * de ellas). Estos par&aacute;etros forzados tienen prioridad sobre los anteriores.
	 * Cada uno de los parametros tendr&aacute;n la forma "clave=valor" y estar&aacute;n
	 * separados entre s&iacute; por punto y coma (';').
	 * @param conf Configuraci&oacute;n del servicio.
	 * @return Listado de par&aacute;metros.
	 */
	private static String getForcedExtraParams(final Properties conf) {
		return conf.getProperty(KEY_FORCED_EXTRAPARAMS);
	}

	/**
	 * Procesa la petici&oacute;n de un listado de peticiones de firma.
	 * @param xml XML con la solicitud.
	 * @return XML con la respuesta a la petici&oacute;n.
	 * @throws SAXException Cuando ocurre alg&uacute;n error al procesar los XML.
	 * @throws IOException Cuando ocurre algun errlr al leer el XML.
	 * @throws MobileException Cuando ocurre un error al contactar con el servidor.
	 */
	private String processRequestsList(final byte[] xml) throws SAXException, IOException, MobileException {

		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final ListRequest listRequest = ListRequestParser.parse(doc);

		LOGGER.info("Solicitamos las peticiones de firma al Portafirmas"); //$NON-NLS-1$

		final PartialSignRequestsList signRequests = getRequestsList(listRequest);

		LOGGER.info("Hemos obtenido las peticiones de firma del Portafirmas"); //$NON-NLS-1$

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
		for (final String supportedFormat : listRequest.getFormats()) {
			formatsList.getStr().add(supportedFormat);
		}

		// Listado de filtros para la consulta
		final MobileRequestFilterList filterList = new MobileRequestFilterList();
		if (listRequest.getFilters() != null) {
			for (final String filterKey : listRequest.getFilters().keySet().toArray(new String[listRequest.getFilters().size()])) {
				final MobileRequestFilter filter = new MobileRequestFilter();
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
		for (final MobileRequest request : mobileRequestsList.getRequestList()) {

			final List<MobileDocument> docList = request.getDocumentList() != null ?
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
							prepareSignatureParamenters(doc.getSignatureParameters()));
				}
			} catch (final Exception e) {
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

	private static String prepareSignatureParamenters(final JAXBElement<String> parameters) {
		if (parameters == null) {
			return null;
		}
		return parameters.getValue();
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
	 */
	private static RequestResult[] doReject(final RejectRequest rejectRequest) {

		LOGGER.info("Se solicita el rechazo de peticiones: " + Integer.toString(rejectRequest.size())); //$NON-NLS-1$

		final MobileService service = new MobileService_Service().getMobileServicePort();

		final List<Boolean> rejectionsResults = new ArrayList<Boolean>();
		for (final String id : rejectRequest) {
			// Si devuelve cualquier texto es que la operacion ha terminado correctamente. Por defecto,
			// devuelve el mismo identificador de la peticion, aunque no es obligatorio
			// Si falla devuelve una excepcion.
			try {
				service.rejectRequest(rejectRequest.getCertEncoded(), id, rejectRequest.getRejectReason());
				rejectionsResults.add(Boolean.TRUE);
			} catch (final Exception e) {
				LOGGER.log(Level.WARNING, "Error en el rechazo de la peticion " + id + ": " + e, e); //$NON-NLS-1$ //$NON-NLS-2$
				rejectionsResults.add(Boolean.FALSE);
			}
		}

		final RequestResult[] result = new RequestResult[rejectRequest.size()];
		for (int i = 0; i < rejectRequest.size(); i++) {
			result[i] = new RequestResult(rejectRequest.get(i), rejectionsResults.get(i).booleanValue());
		}

		LOGGER.info("Se devuelve el resultado del rechazo de peticiones: " + result.length); //$NON-NLS-1$

		return result;
	}

	private String processRequestDetail(final byte[] xml) throws SAXException, IOException, MobileException {
		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final DetailRequest request = DetailRequestParser.parse(doc);

		LOGGER.info("Solicitamos el detalle de una peticion al Portafirmas"); //$NON-NLS-1$

		final Detail requestDetails = getRequestDetail(request);

		LOGGER.info("Hemos obtenido el detalle de la peticion del Portafirmas"); //$NON-NLS-1$

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
		final List<MobileSignLine> mobileSignLines = mobileRequest.getSignLineList().getMobileSignLine();
		final List<String>[] signLines = new ArrayList[mobileSignLines.size()];
		for (int i = 0; i < signLines.length; i++) {
			signLines[i] = new ArrayList<String>();
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

	private InputStream processDocumentPreview(final byte[] xml) throws SAXException, IOException, MobileException {
		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final PreviewRequest request = PreviewRequestParser.parse(doc);

		LOGGER.info("Solicitamos la previsualizacion de un documento al Portafirmas"); //$NON-NLS-1$

		final DocumentData documentData = previewDocument(request);

		LOGGER.info("Hemos obtenido la previsualizacion de un documento del Portafirmas"); //$NON-NLS-1$

		return documentData.getDataIs();
	}

	private InputStream processSignPreview(final byte[] xml) throws SAXException, IOException, MobileException {
		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final PreviewRequest request = PreviewRequestParser.parse(doc);

		LOGGER.info("Solicitamos la previsualizacion de una firma al Portafirmas"); //$NON-NLS-1$

		final DocumentData documentData = previewSign(request);

		LOGGER.info("Hemos obtenido la previsualizacion de una firma del Portafirmas"); //$NON-NLS-1$

		return documentData.getDataIs();
	}

	private InputStream processSignReportPreview(final byte[] xml) throws SAXException, IOException, MobileException {
		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final PreviewRequest request = PreviewRequestParser.parse(doc);

		LOGGER.info("Solicitamos la previsualizacion de un informe de firma al Portafirmas"); //$NON-NLS-1$

		final DocumentData documentData = previewSignReport(request);

		LOGGER.info("Hemos obtenido la previsualizacion de un informe de firma del Portafirmas"); //$NON-NLS-1$

		return documentData.getDataIs();
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

		final InputStream contentIs;
		final Object content = document.getData().getValue().getContent();
		if (content instanceof InputStream) {
			contentIs = (InputStream) content;
		}
		else if (content instanceof String) {
			contentIs = new ByteArrayInputStream(Base64.decode((String) content));
		}
		else {
			throw new IOException("No se puede manejar el tipo de objeto devuelto por el servicio de previsualizacion de documentos: " + content); //$NON-NLS-1$
		}

		return new DocumentData(
				document.getIdentifier(),
				document.getName(),
				document.getMime(),
				contentIs);
	}

	private String processConfigueApp(final byte[] xml) throws SAXException, IOException, MobileException {
		final Document doc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final ConfigurationRequest request = ConfigurationRequestParser.parse(doc);

		LOGGER.info("Solicitamos la configuracion al Portafirmas"); //$NON-NLS-1$

		final AppConfiguration appConfig = loadConfiguration(request);

		LOGGER.info("Hemos obtenido la configuracion del Portafirmas"); //$NON-NLS-1$

		return XmlResponsesFactory.createConfigurationResponse(appConfig);
	}

	/**
	 * Recupera los datos de confguracion de la aplicaci&oacute;n. Hasta el momento:
	 * <ul>
	 * <li>Listado de aplicaciones.</li>
	 * </ul>
	 * @param request Datos gen&eacute;ricos necesarios para la petici&oacute;n.
	 * @return Configuraci&oacute;n de la aplicaci&oacute;n.
	 * @throws MobileException Cuando ocurre un error al contactar con el Portafirmas.
	 * @throws IOException Cuando no ha sido posible leer el documento.
	 */
	private static AppConfiguration loadConfiguration(final ConfigurationRequest request) throws MobileException, IOException {

		final MobileService service = new MobileService_Service().getMobileServicePort();
		final MobileApplicationList appList = service.queryApplicationsMobile(request.getCertEncoded());

		final List<String> appIds = new ArrayList<String>();
		final List<String> appNames = new ArrayList<String>();
		for (final MobileApplication app : appList.getApplicationList()) {
			appIds.add(app.getId());
			appNames.add(app.getName() != null ? app.getName() : app.getId());
		}

		return new AppConfiguration(appIds, appNames);
	}


	private String processApproveRequest(final byte[] xml) throws SAXException, IOException {

		final Document xmlDoc = this.documentBuilder.parse(new ByteArrayInputStream(xml));
		final ApproveRequestList appRequests = ApproveRequestParser.parse(xmlDoc);

		LOGGER.info("Solicitamos la aprobacion de peticiones al Portafirmas"); //$NON-NLS-1$

		final ApproveRequestList approvedList = approveRequests(appRequests);

		LOGGER.info("Hemos obtenido la listad de peticiones aprobadas del Portafirmas"); //$NON-NLS-1$

		return XmlResponsesFactory.createApproveRequestsResponse(approvedList);
	}

	private static ApproveRequestList approveRequests(final ApproveRequestList appRequests) {
		final MobileService_Service mobileService = new MobileService_Service();
		final MobileService service = mobileService.getMobileServicePort();

		for (final ApproveRequest appReq : appRequests) {
			try {
				service.approveRequest(appRequests.getCertEncoded(), appReq.getRequestTagId());
			} catch (final MobileException e) {
				appReq.setOk(false);
			}
		}
		return appRequests;
	}

	/**
	 * Permite enviar respuestas al cliente de un servicio.
	 */
	private final class Responser {

		final HttpServletResponse response;

		/**
		 * Crea el Responser enlaz&aacute;ndolo con una petici&oacute;n concreta al servicio.
		 * @param response Manejador para el env&iacute;o de la respuesta.
		 * @throws IOException Cuando no se puede escribir una respuesta.
		 */
		public Responser(final HttpServletResponse response) throws IOException {
			this.response = response;
		}

		/**
		 * Imprime una respuesta en la salida del servicio y cierra el flujo.
		 * @param message Mensaje que imprimir como respuesta.
		 */
		public void print(final String message) {
			try {
				final OutputStream os = this.response.getOutputStream();
				os.write(message.getBytes(Charset.forName(DEFAULT_CHARSET)));
				os.close();
			}
			catch (final Exception e) {
				LOGGER.info("Error al devolver el resultado al cliente a traves del metodo print: " + e); //$NON-NLS-1$
			}
		}

		public void write(final InputStream message) {
			int n = -1;
			final byte[] buffer = new byte[1024];
			try {
				final OutputStream os = this.response.getOutputStream();
				while ((n = message.read(buffer)) > 0) {
					os.write(buffer, 0, n);
				}
				os.close();
			}
			catch (final Exception e) {
				LOGGER.info("Error al devolver el resultado al cliente a traves del metodo write: " + e); //$NON-NLS-1$
			}
		}
	}
}
