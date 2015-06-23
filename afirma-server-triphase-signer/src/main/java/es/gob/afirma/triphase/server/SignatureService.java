package es.gob.afirma.triphase.server;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.PrintWriter;
import java.lang.reflect.Constructor;
import java.net.URLDecoder;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.triphase.server.document.DocumentManager;

/**
 * Servicio de firma electronica en 3 fases.
 */
public final class SignatureService extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static DocumentManager DOC_MANAGER;

	private static final String URL_DEFAULT_CHARSET = "utf-8"; //$NON-NLS-1$

	private static final String PARAM_NAME_OPERATION = "op"; //$NON-NLS-1$

	private static final String PARAM_VALUE_OPERATION_PRESIGN = "pre"; //$NON-NLS-1$
	private static final String PARAM_VALUE_OPERATION_POSTSIGN = "post"; //$NON-NLS-1$

	private static final String PARAM_NAME_SUB_OPERATION = "cop"; //$NON-NLS-1$

	private static final String PARAM_VALUE_SUB_OPERATION_SIGN = "sign"; //$NON-NLS-1$
	private static final String PARAM_VALUE_SUB_OPERATION_COSIGN = "cosign"; //$NON-NLS-1$
	private static final String PARAM_VALUE_SUB_OPERATION_COUNTERSIGN = "countersign"; //$NON-NLS-1$

	// Parametros que necesitamos para la prefirma
	private static final String PARAM_NAME_DOCID = "doc"; //$NON-NLS-1$
	private static final String PARAM_NAME_ALGORITHM = "algo"; //$NON-NLS-1$
	private static final String PARAM_NAME_FORMAT = "format"; //$NON-NLS-1$
	private static final String PARAM_NAME_CERT = "cert"; //$NON-NLS-1$
	private static final String PARAM_NAME_EXTRA_PARAM = "params"; //$NON-NLS-1$
	private static final String PARAM_NAME_SESSION_DATA = "session"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica los nodos que deben contrafirmarse. */
	private static final String PARAM_NAME_TARGET_TYPE = "target"; //$NON-NLS-1$

	/** Indicador de finalizaci&oacute;n correcta de proceso. */
	private static final String SUCCESS = "OK"; //$NON-NLS-1$

	/** Etiqueta que se envia en la respuesta para el nuevo identificador de documento (el almacenado una vez firmado). */
	private static final String NEW_DOCID_TAG = "NEWID"; //$NON-NLS-1$

	private static final String CONFIG_FILE = "config.properties"; //$NON-NLS-1$

	private static final String CONFIG_PARAM_DOCUMENT_MANAGER_CLASS = "document.manager"; //$NON-NLS-1$
	private static final String CONFIG_PARAM_ALLOW_ORIGIN = "Access-Control-Allow-Origin"; //$NON-NLS-1$

	/** Origines permitidos por defecto desde los que se pueden realizar peticiones al servicio. */
	private static final String CONFIG_DEFAULT_VALUE_ALLOW_ORIGIN = "*"; //$NON-NLS-1$

	private static final Properties config;

	static {
		try {
			final InputStream configIs = SignatureService.class.getClassLoader().getResourceAsStream(CONFIG_FILE);
			if (configIs == null) {
				throw new RuntimeException("No se encuentra el fichero de configuracion del servicio: " + CONFIG_FILE); //$NON-NLS-1$
			}

			config = new Properties();
			config.load(configIs);
			configIs.close();
		}
		catch(final Exception e) {
			throw new RuntimeException("Error en la carga del fichero de propiedades: " + e, e); //$NON-NLS-1$
		}

		if (!config.containsKey(CONFIG_PARAM_DOCUMENT_MANAGER_CLASS)) {
			throw new IllegalArgumentException(
					"No se ha indicado el document manager (" + CONFIG_PARAM_DOCUMENT_MANAGER_CLASS + ") en el fichero de propiedades"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		Class<?> docManagerClass;
		try {
			docManagerClass = Class.forName(config.getProperty(CONFIG_PARAM_DOCUMENT_MANAGER_CLASS));
		} catch (final ClassNotFoundException e) {
			throw new RuntimeException("La clase DocumentManager indicada no existe: " + config.getProperty(CONFIG_PARAM_DOCUMENT_MANAGER_CLASS), e); //$NON-NLS-1$
		}

		try {
			final Constructor<?> docManagerConstructor = docManagerClass.getConstructor(Properties.class);
			DOC_MANAGER = (DocumentManager) docManagerConstructor.newInstance(config);
		} catch (final Exception e) {
			try {
				DOC_MANAGER = (DocumentManager) docManagerClass.newInstance();
			} catch (final Exception e2) {
				throw new RuntimeException("No se ha podido inicializar el DocumentManager. Debe tener un constructor vacio o que reciba un Properties", e); //$NON-NLS-1$
			}
		}

	}

	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) {

		LOGGER.info("Se realiza una peticion de firma trifasica"); //$NON-NLS-1$

		final Map<String, String> parameters = new HashMap<String, String>();
		final String[] params;
		try {
			params = new String(AOUtil.getDataFromInputStream(request.getInputStream())).split("&"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.severe("No se pudieron leer los parametros de la peticion: " + e); //$NON-NLS-1$
			return;
		}

		for (final String param : params) {
			if (param.indexOf('=') != -1) {
				try {
					parameters.put(param.substring(0, param.indexOf('=')), URLDecoder.decode(param.substring(param.indexOf('=') + 1), URL_DEFAULT_CHARSET));
				}
				catch (final Exception e) {
					LOGGER.warning("Error al decodificar un parametro de la peticion: " + e); //$NON-NLS-1$
				}
			}
		}

		String allowOrigin = CONFIG_DEFAULT_VALUE_ALLOW_ORIGIN;
		if (config.contains(CONFIG_PARAM_ALLOW_ORIGIN)) {
			allowOrigin = config.getProperty(CONFIG_PARAM_ALLOW_ORIGIN);
		}

		response.setHeader("Access-Control-Allow-Origin", allowOrigin); //$NON-NLS-1$
		response.setContentType("text/plain"); //$NON-NLS-1$
		response.setCharacterEncoding("utf-8"); //$NON-NLS-1$

		// Obtenemos el codigo de operacion
		PrintWriter out = null;
		try {
			out = response.getWriter();
			final String operation = parameters.get(PARAM_NAME_OPERATION);
			if (operation == null) {
				LOGGER.warning("No se ha indicado la operacion trifasica a realizar"); //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(1));
				out.close();
				return;
			}

			// Obtenemos el codigo de operacion
			//final String subOperation = request.getParameter(PARAM_NAME_SUB_OPERATION);
			final String subOperation = parameters.get(PARAM_NAME_SUB_OPERATION);
			if (subOperation == null || !PARAM_VALUE_SUB_OPERATION_SIGN.equals(subOperation)
					&& !PARAM_VALUE_SUB_OPERATION_COSIGN.equals(subOperation)
					&& !PARAM_VALUE_SUB_OPERATION_COUNTERSIGN.equals(subOperation)) {
				out.print(ErrorManager.getErrorMessage(13));
				out.close();
				return;
			}

			// Obtenemos los parametros adicionales para la firma
			final Properties extraParams = new Properties();
			try {
				if (parameters.containsKey(PARAM_NAME_EXTRA_PARAM)) {
					extraParams.load(
						new ByteArrayInputStream(
							Base64.decode(parameters.get(PARAM_NAME_EXTRA_PARAM).trim(), true)
						)
					);
				}
			}
			catch (final Exception e) {
				LOGGER.severe("El formato de los parametros adicionales suministrado es erroneo: " +  e); //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(6) + ": " + e); //$NON-NLS-1$);
				out.close();
				return;
			}

			// Obtenemos los parametros adicionales para la firma
			byte[] sessionData = null;
			try {
				if (parameters.containsKey(PARAM_NAME_SESSION_DATA)) {
					sessionData = Base64.decode(parameters.get(PARAM_NAME_SESSION_DATA).trim(), true);
				}
			}
			catch (final Exception e) {
				LOGGER.severe("El formato de los datos de sesion suministrados es erroneo: "  + e); //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(6) + ": " + e); //$NON-NLS-1$
				out.close();
				return;
			}

			// Obtenemos el certificado
			final String cert = parameters.get(PARAM_NAME_CERT);
			if (cert == null) {
				LOGGER.warning("No se ha indicado certificado de firma"); //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(5));
				out.close();
				return;
			}
			final X509Certificate signerCert;
			try {
				signerCert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
					new ByteArrayInputStream(Base64.decode(cert, true))
				);
			}
			catch(final Exception e) {
				LOGGER.severe("Error al decodificar el certificado: " + e);  //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(7));
				out.close();
				return;
			}


			byte[] docBytes = null;
			final String docId = parameters.get(PARAM_NAME_DOCID);
			if (docId != null) {
				try {
					docBytes = DOC_MANAGER.getDocument(docId, signerCert, extraParams);
				} catch (final Throwable e) {
					LOGGER.warning("Error al recuperar el documento: " + e); //$NON-NLS-1$
					out.print(ErrorManager.getErrorMessage(14) + ": " + e); //$NON-NLS-1$
					e.printStackTrace();
					out.close();
					return;
				}
			}

			// Obtenemos el algoritmo de firma
			final String algorithm = parameters.get(PARAM_NAME_ALGORITHM);
			if (algorithm == null) {
				LOGGER.warning("No se ha indicado algoritmo de firma"); //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(3));
				out.close();
				return;
			}

			// Obtenemos el formato de firma
			final String format = parameters.get(PARAM_NAME_FORMAT);
			LOGGER.info("Formato de firma seleccionado: " + format); //$NON-NLS-1$
			if (format == null) {
				LOGGER.warning("No se ha indicado formato de firma"); //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(4));
				out.close();
				return;
			}

			// Instanciamos el preprocesador adecuado
			final TriPhasePreProcessor prep;
			if (AOSignConstants.SIGN_FORMAT_PADES.equalsIgnoreCase(format) || AOSignConstants.SIGN_FORMAT_PADES_TRI.equalsIgnoreCase(format)) {
				prep = new PAdESTriPhasePreProcessor();
			}
			else if (AOSignConstants.SIGN_FORMAT_CADES.equalsIgnoreCase(format) || AOSignConstants.SIGN_FORMAT_CADES_TRI.equalsIgnoreCase(format)) {
				prep = new CAdESTriPhasePreProcessor();
			}
			else if (AOSignConstants.SIGN_FORMAT_XADES.equalsIgnoreCase(format) || AOSignConstants.SIGN_FORMAT_XADES_TRI.equalsIgnoreCase(format)) {
				prep = new XAdESTriPhasePreProcessor();
			}
			else {
				LOGGER.severe("Formato de firma no soportado: " + format); //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(8));
				out.close();
				return;
			}

			if (PARAM_VALUE_OPERATION_PRESIGN.equals(operation)) {

				LOGGER.info(" == PREFIRMA en servidor"); //$NON-NLS-1$

				try {
					final byte[] preRes;
					if (PARAM_VALUE_SUB_OPERATION_SIGN.equals(subOperation)) {
						preRes = prep.preProcessPreSign(docBytes, algorithm, signerCert, extraParams);
					}
					else if (PARAM_VALUE_SUB_OPERATION_COSIGN.equals(subOperation)) {
						preRes = prep.preProcessPreCoSign(docBytes, algorithm, signerCert, extraParams);
					}
					else if (PARAM_VALUE_SUB_OPERATION_COUNTERSIGN.equals(subOperation)) {

						CounterSignTarget target = CounterSignTarget.LEAFS;
						if (extraParams.containsKey(PARAM_NAME_TARGET_TYPE)) {
							final String targetValue = extraParams.getProperty(PARAM_NAME_TARGET_TYPE).trim();
							if (CounterSignTarget.TREE.toString().equalsIgnoreCase(targetValue)) {
								target = CounterSignTarget.TREE;
							}
						}

						preRes = prep.preProcessPreCounterSign(docBytes, algorithm, signerCert, extraParams, target);
					}
					else {
						out.close();
						throw new AOException("No se reconoce el codigo de sub-operacion: " + subOperation); //$NON-NLS-1$
					}

					LOGGER.info(" Se calculado el resultado de la prefirma y se devuelve. Numero de bytes: " + preRes.length); //$NON-NLS-1$

					out.print(
						Base64.encode(
							preRes,
							true
						)
					);

					LOGGER.info(" FIN PREFIRMA"); //$NON-NLS-1$
				}
				catch (final Exception e) {
					LOGGER.severe("Error en la prefirma: " + e); //$NON-NLS-1$
					e.printStackTrace();
					out.print(ErrorManager.getErrorMessage(9) + ": " + e); //$NON-NLS-1$
					out.close();
					return;
				}
			}
			else if (PARAM_VALUE_OPERATION_POSTSIGN.equals(operation)) {

				LOGGER.info(" == POSTFIRMA en servidor"); //$NON-NLS-1$

				final byte[] signedDoc;
				try {
					if (PARAM_VALUE_SUB_OPERATION_SIGN.equals(subOperation)) {
						signedDoc = prep.preProcessPostSign(docBytes, algorithm, signerCert, extraParams, sessionData);
					}
					else if (PARAM_VALUE_SUB_OPERATION_COSIGN.equals(subOperation)) {
						signedDoc = prep.preProcessPostCoSign(docBytes, algorithm, signerCert, extraParams, sessionData);
					}
					else if (PARAM_VALUE_SUB_OPERATION_COUNTERSIGN.equals(subOperation)) {

						CounterSignTarget target = CounterSignTarget.LEAFS;
						if (extraParams.containsKey(PARAM_NAME_TARGET_TYPE)) {
							final String targetValue = extraParams.getProperty(PARAM_NAME_TARGET_TYPE).trim();
							if (CounterSignTarget.TREE.toString().equalsIgnoreCase(targetValue)) {
								target = CounterSignTarget.TREE;
							}
						}

						signedDoc = prep.preProcessPostCounterSign(docBytes, algorithm, signerCert, extraParams, sessionData, target);
					}
					else {
						out.close();
						throw new AOException("No se reconoce el codigo de sub-operacion: " + subOperation); //$NON-NLS-1$
					}
				}
				catch (final Exception e) {
					LOGGER.severe("Error en la postfirma: " + e); //$NON-NLS-1$
					out.print(ErrorManager.getErrorMessage(12) + ": " + e); //$NON-NLS-1$
					e.printStackTrace();
					out.close();
					return;
				}

				// Establecemos parametros adicionales que se pueden utilizar para guardar el documento
				extraParams.setProperty(PARAM_NAME_FORMAT, format);

				LOGGER.info(" Se ha calculado el resultado de la postfirma y se devuelve. Numero de bytes: " + signedDoc.length); //$NON-NLS-1$

				// Devolvemos al servidor documental el documento firmado
				final String newDocId;
				try {
					newDocId = DOC_MANAGER.storeDocument(docId, signerCert, signedDoc, extraParams);
				}
				catch(final Throwable e) {
					LOGGER.severe("Error al almacenar el documento: " + e); //$NON-NLS-1$
					out.print(ErrorManager.getErrorMessage(10) + ": " + e); //$NON-NLS-1$
					e.printStackTrace();
					out.close();
					return;
				}

				LOGGER.info(" FIN POSTFIRMA"); //$NON-NLS-1$

				out.println(SUCCESS + " " + NEW_DOCID_TAG + "=" + newDocId); //$NON-NLS-1$ //$NON-NLS-2$
			}
			else {
				out.println(ErrorManager.getErrorMessage(11));
			}
			out.close();
		}
        catch (final Exception e) {
        	LOGGER.severe("No se pude contestar a la peticion: " + e); //$NON-NLS-1$
        	e.printStackTrace();
        	if (out != null) {
        		out.close();
        	}
        	return;
        }

	}
}
