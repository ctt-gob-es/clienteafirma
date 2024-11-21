/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.HttpURLConnection;
import java.net.URLDecoder;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.RuntimeConfigNeededException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOTriphaseException;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.ExtraParamsProcessor;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.xml.XmlDSigProviderHelper;
import es.gob.afirma.triphase.server.cache.DocumentCacheManager;
import es.gob.afirma.triphase.server.document.DocumentManager;
import es.gob.afirma.triphase.signer.processors.PAdESTriPhasePreProcessor;
import es.gob.afirma.triphase.signer.processors.PreProcessorFactory;
import es.gob.afirma.triphase.signer.processors.TriPhasePreProcessor;

/** Servicio de firma electr&oacute;nica en 3 fases. */
public final class SignatureService extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static DocumentManager docManager;
	private static DocumentCacheManager docCacheManager;

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
	private static final String PARAM_NAME_EXTRA_PARAM = "params"; //$NON-NLS-1$
	private static final String PARAM_NAME_SESSION_DATA = "session"; //$NON-NLS-1$
	private static final String PARAM_NAME_CERT = "cert"; //$NON-NLS-1$

	/** Separador que debe usarse para incluir varios certificados dentro del mismo par&aacute;metro. */
	private static final String PARAM_NAME_CERT_SEPARATOR = ","; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica los nodos que deben contrafirmarse. */
	private static final String PARAM_NAME_TARGET_TYPE = "target"; //$NON-NLS-1$

	/** Indicador de finalizaci&oacute;n correcta de proceso. */
	private static final String SUCCESS = "OK NEWID="; //$NON-NLS-1$

	private static final String EXTRA_PARAM_HEADLESS = "headless"; //$NON-NLS-1$

	private static final String EXTRA_PARAM_VALIDATE_PKCS1 = "validatePkcs1"; //$NON-NLS-1$

	/** Algoritmo para el c&aacute;lculo de los valores de integridad. */
	private static final String HMAC_ALGORITHM = "HmacSHA256"; //$NON-NLS-1$

	/** Propiedad de la informacion trifasica en la que se almacenan las prefirmas. */
	private static final String TRIPHASE_PROP_PRESIGN = "PRE"; //$NON-NLS-1$

	/**
	 * Propiedad de la informacion trifasica en la que se almacenan los c&oacute;digos
	 * de verificaci&oacute;n de integridad.
	 */
	private static final String TRIPHASE_PROP_HMAC = "HMAC"; //$NON-NLS-1$

	/** Propiedad dedicada a almacenar el identificador del archivo a escribir o leer de cach&eacute;. */
	private static final String TRIPHASE_PROP_CACHE_ID = "CACHE_ID"; //$NON-NLS-1$

	/** Juego de caracteres usado internamente para la codificaci&oacute;n de textos. */
	private static final Charset CHARSET = StandardCharsets.UTF_8;

	/**
	 * N&uacute;mero de p&aacute;ginas por defecto de un PDF sobre las que
	 * comprobar si se ha producido un PDF Shadow Attack.
	 */
	private static final int DEFAULT_PAGES_TO_CHECK_PSA = 10;

	/** Propiedad que indica si la cach&eacute; est&aacute; activada o no. */
	private static boolean cacheEnabled = false;

	static {

		final Class<?> docManagerClass;
		final String docManagerClassName = ConfigManager.getDocManagerClassName();

		try {
			docManagerClass = Class.forName(docManagerClassName);
		}
		catch (final ClassNotFoundException e) {
			throw new RuntimeException(
					"La clase DocumentManager indicada no existe ("  //$NON-NLS-1$
					+ docManagerClassName +  "): " + e, e //$NON-NLS-1$
					);
		}

		try {
			final Constructor<?> docManagerConstructor = docManagerClass.getConstructor(Properties.class);
			docManager = (DocumentManager) docManagerConstructor.newInstance(ConfigManager.getConfig());
		}
		catch (final Exception e) {
			try {
				docManager = (DocumentManager) docManagerClass.getConstructor().newInstance();
			}
			catch (final Exception e2) {
				throw new RuntimeException(
						"No se ha podido inicializar el DocumentManager. Debe tener un constructor vacio o que reciba un Properties: " + e2, e //$NON-NLS-1$
						);
			}
			try {
				final Method initMethod = docManagerClass.getMethod("init", Properties.class); //$NON-NLS-1$
				initMethod.invoke(docManager, ConfigManager.getConfig());
			} catch (final Exception e2) {
				LOGGER.warning("El DocumentManager no permitir recibir configuracion ni en un contructor ni en un metodo init, asi que no se configurara"); //$NON-NLS-1$
			}
		}

		LOGGER.info("Se usara el siguiente 'DocumentManager' para firma trifasica: " + docManager.getClass().getName()); //$NON-NLS-1$

		cacheEnabled = Boolean.parseBoolean(ConfigManager.isCacheEnabled());

		if (cacheEnabled) {

			final Class<?> docCacheManagerClass;
			String docCacheManagerClassName;
			docCacheManagerClassName = ConfigManager.getDocCacheManagerClassName();

			try {
				docCacheManagerClass = Class.forName(docCacheManagerClassName);
			}
			catch (final ClassNotFoundException e) {
				throw new RuntimeException(
						"La clase DocumentCacheManager indicada no existe ("  //$NON-NLS-1$
						+ docManagerClassName +  "): " + e, e //$NON-NLS-1$
						);
			}

			try {
				final Constructor<?> docCacheManagerConstructor = docCacheManagerClass.getConstructor(Properties.class);
				docCacheManager = (DocumentCacheManager) docCacheManagerConstructor.newInstance(ConfigManager.getConfig());
			}
			catch (final Exception e) {
				try {
					docCacheManager = (DocumentCacheManager) docCacheManagerClass.getConstructor().newInstance();
				}
				catch (final Exception e2) {
					throw new RuntimeException(
							"No se ha podido inicializar el DocumentCacheManager. Debe tener un constructor vacio o que reciba un Properties: " + e2, e //$NON-NLS-1$
							);
				}
			}

			LOGGER.info("Se usara el siguiente 'DocumentCacheManager' para firma trifasica: " + docCacheManager.getClass().getName()); //$NON-NLS-1$
		}

		// Indicamos si se debe instalar el proveedor de firma XML de Apache
		XmlDSigProviderHelper.configureXmlDSigProvider(true, ConfigManager.isProviderApacheConfigured());
	}

	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) {

		LOGGER.info("== INICIO FIRMA TRIFASICA =="); //$NON-NLS-1$

		final Map<String, String> parameters = new HashMap<>();
		final String[] params;
		try (InputStream is = request.getInputStream()) {
			params = new String(AOUtil.getDataFromInputStream(is), URL_DEFAULT_CHARSET).split("&"); //$NON-NLS-1$
		}
		catch (final Exception | Error e) {
			LOGGER.severe("No se pudieron leer los parametros de la peticion: " + e); //$NON-NLS-1$
			try {
				response.sendError(HttpServletResponse.SC_BAD_REQUEST);
			} catch (final IOException e1) {
				LOGGER.log(Level.SEVERE, "No se pudo enviar un error al cliente", e); //$NON-NLS-1$
			}
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

		final String allowOrigin = ConfigManager.getAccessControlAllowOrigin();

		response.setHeader("Access-Control-Allow-Origin", allowOrigin); //$NON-NLS-1$
		response.setContentType("text/plain"); //$NON-NLS-1$
		response.setCharacterEncoding("utf-8"); //$NON-NLS-1$

		// Obtenemos el codigo de operacion
		try (
			final PrintWriter out = response.getWriter();
		) {

			final String operation = parameters.get(PARAM_NAME_OPERATION);
			if (operation == null) {
				LOGGER.severe("No se ha indicado la operacion trifasica a realizar"); //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(1));
				out.flush();
				return;
			}

			// Obtenemos el codigo de operacion
			final String subOperation = parameters.get(PARAM_NAME_SUB_OPERATION);
			if (subOperation == null || !PARAM_VALUE_SUB_OPERATION_SIGN.equalsIgnoreCase(subOperation)
					&& !PARAM_VALUE_SUB_OPERATION_COSIGN.equalsIgnoreCase(subOperation)
					&& !PARAM_VALUE_SUB_OPERATION_COUNTERSIGN.equalsIgnoreCase(subOperation)) {
				out.print(ErrorManager.getErrorMessage(ErrorManager.INVALID_SUBOPERATION));
				out.flush();
				return;
			}


			// Obtenemos el formato de firma
			final String format = parameters.get(PARAM_NAME_FORMAT);
			LOGGER.info("Formato de firma seleccionado: " + format); //$NON-NLS-1$
			if (format == null) {
				LOGGER.warning("No se ha indicado formato de firma"); //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(4));
				out.flush();
				return;
			}

			// Obtenemos los parametros adicionales para la firma
			Properties extraParams = new Properties();
			try {
				if (parameters.containsKey(PARAM_NAME_EXTRA_PARAM)) {
					extraParams = AOUtil.base642Properties(parameters.get(PARAM_NAME_EXTRA_PARAM));
				}
			}
			catch (final Exception e) {
				LOGGER.severe("El formato de los parametros adicionales suministrado es erroneo: " +  e); //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(6) + ": " + e); //$NON-NLS-1$);
				out.flush();
				return;
			}

			// Eliminamos configuraciones que no deseemos que se utilicen extenamente
			extraParams.remove(EXTRA_PARAM_VALIDATE_PKCS1);


			// Introducimos los parametros necesarios para que no se traten
			// de mostrar dialogos en servidor
			extraParams.setProperty(EXTRA_PARAM_HEADLESS, Boolean.TRUE.toString());

			try {
				extraParams = ExtraParamsProcessor.expandProperties(
					extraParams,
					null,
					format
				);
			}
			catch (final Exception e) {
				LOGGER.severe("Se han indicado una politica de firma y un formato incompatibles: "  + e); //$NON-NLS-1$
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
				out.flush();
				return;
			}
			if (sessionData != null) {
				LOGGER.fine("Recibidos los siguientes datos de sesion para '" + operation + "':\n" + new String(sessionData)); //$NON-NLS-1$ //$NON-NLS-2$
			}

			// Obtenemos el certificado
			final String cert = parameters.get(PARAM_NAME_CERT);
			if (cert == null) {
				LOGGER.warning("No se ha indicado certificado de firma"); //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(5));
				out.flush();
				return;
			}

			final String[] receivedCerts = cert.split(PARAM_NAME_CERT_SEPARATOR);
			final X509Certificate[] signerCertChain = new X509Certificate[receivedCerts.length];
			for (int i = 0; i<receivedCerts.length; i++) {
				try {
					signerCertChain[i] =
						(X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
							new ByteArrayInputStream(
								Base64.decode(receivedCerts[i], true)
							)
						)
					;
				}
				catch(final Exception e) {

					LOGGER.log(Level.SEVERE, "Error al decodificar el certificado: " + receivedCerts[i], e);  //$NON-NLS-1$
					out.print(ErrorManager.getErrorMessage(7));
					out.flush();
					return;
				}
			}

			byte[] docBytes = null;

			if (cacheEnabled && sessionData != null) {
				LOGGER.info("Recuperamos el documento de cache"); //$NON-NLS-1$
				try {
					final TriphaseData tr = TriphaseData.parser(sessionData);
					final TriSign preSign = tr.getTriSigns().get(0);
					final String cacheId = preSign.getProperty(TRIPHASE_PROP_CACHE_ID);
					docBytes = docCacheManager.getDocumentFromCache(cacheId);
				}
				catch (final Exception e) {
					LOGGER.log(Level.WARNING, "No se pudo obtener un documento de la cache", e); //$NON-NLS-1$
					docBytes = null;
				}
			}

			final String docId = parameters.get(PARAM_NAME_DOCID);
			if (docId != null && docBytes == null) {
				try {
					LOGGER.info("Recuperamos el documento mediante el DocumentManager"); //$NON-NLS-1$
					docBytes = docManager.getDocument(docId, signerCertChain, extraParams);
					LOGGER.info(
							"Recuperado documento de " + docBytes.length + " octetos"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				catch (final Throwable e) {
					LOGGER.log(Level.WARNING, "Error al recuperar el documento", e); //$NON-NLS-1$
					out.print(ErrorManager.getErrorMessage(14) + ": " + new AOTriphaseException(e.toString(), e)); //$NON-NLS-1$
					out.flush();
					return;
				}

				// XXX: Si se pide una firma XAdES explicita, se firmara el hash de los datos en
				// lugar de los propios datos. Hacemos el cambio nada mas recuperarlos. Si se ha
				// activado la cache, lo que se cachee sera el hash. Esto se deberia eliminar
				// cuando se abandone el soporte de XAdES explicitas.
				if (PARAM_VALUE_SUB_OPERATION_SIGN.equalsIgnoreCase(subOperation) && isXadesExplicitConfigurated(format, extraParams)) {
					LOGGER.warning(
						"Se ha pedido una firma XAdES explicita, este formato dejara de soportarse en proximas versiones" //$NON-NLS-1$
					);
					try {
						docBytes = MessageDigest.getInstance("SHA1").digest(docBytes); //$NON-NLS-1$
						extraParams.setProperty("mimeType", "hash/sha1"); //$NON-NLS-1$ //$NON-NLS-2$
					} catch (final Exception e) {
						LOGGER.warning("Error al generar la huella digital de los datos para firmar como 'XAdES explicit', " //$NON-NLS-1$
							+ "se realizara una firma XAdES corriente: " + e); //$NON-NLS-1$
					}
				}
			}

			// Obtenemos el algoritmo de firma
			String algorithm = parameters.get(PARAM_NAME_ALGORITHM);
			if (algorithm == null) {
				LOGGER.warning("No se ha indicado algoritmo de firma. Se utilizara " + AOSignConstants.DEFAULT_SIGN_ALGO); //$NON-NLS-1$
				algorithm = AOSignConstants.DEFAULT_SIGN_ALGO;
			} else if (algorithm.toUpperCase(Locale.US).startsWith("MD")) { //$NON-NLS-1$
				LOGGER.severe("Las firmas electronicas no permiten huellas digitales MD2 o MD5 (Decision 130/2011 CE)"); //$NON-NLS-1$
				out.print(ErrorManager.getErrorMessage(20));
				out.flush();
				return;
	    	}

			// Instanciamos el preprocesador adecuado
			final TriPhasePreProcessor prep;
			if (AOSignConstants.SIGN_FORMAT_AUTO.equalsIgnoreCase(format)) {
				prep = PreProcessorFactory.getPreProcessor(docBytes);
			}
			else {
				try {
					prep = PreProcessorFactory.getPreProcessor(format);
				}
				catch (final IllegalArgumentException e) {
					LOGGER.severe("Formato de firma no soportado: " + format); //$NON-NLS-1$
					out.print(ErrorManager.getErrorMessage(8));
					out.flush();
					return;
				}
			}

			// Para las firmas PAdES, aplicamos la configuracion especifica para
			// la verificacion de PDF Shadow Attacks
			if (prep instanceof PAdESTriPhasePreProcessor) {
				configurePdfShadowAttackParameters(extraParams);
			}

			// Identificamos el algoritmo de firma apropiado la clave del certificado seleccionado
	        final String signAlgorithm = AOSignConstants.composeSignatureAlgorithmName(algorithm, signerCertChain[0].getPublicKey().getAlgorithm());

			if (PARAM_VALUE_OPERATION_PRESIGN.equalsIgnoreCase(operation)) {

				LOGGER.info(" == PREFIRMA en servidor"); //$NON-NLS-1$

				// Comprobamos si se ha pedido validar las firmas antes de agregarles una nueva
		        final boolean checkSignatures = Boolean.parseBoolean(extraParams.getProperty("checkSignatures")); //$NON-NLS-1$

				TriphaseData preRes;
				try {
					if (PARAM_VALUE_SUB_OPERATION_SIGN.equalsIgnoreCase(subOperation)) {
						preRes = prep.preProcessPreSign(
									docBytes,
									signAlgorithm,
									signerCertChain,
									extraParams,
									checkSignatures
								);
					}
					else if (PARAM_VALUE_SUB_OPERATION_COSIGN.equalsIgnoreCase(subOperation)) {
						preRes = prep.preProcessPreCoSign(
								docBytes,
								signAlgorithm,
								signerCertChain,
								extraParams,
								checkSignatures
							);
					}
					else if (PARAM_VALUE_SUB_OPERATION_COUNTERSIGN.equalsIgnoreCase(subOperation)) {

						CounterSignTarget target = CounterSignTarget.LEAFS;
						if (extraParams.containsKey(PARAM_NAME_TARGET_TYPE)) {
							final String targetValue = extraParams.getProperty(PARAM_NAME_TARGET_TYPE).trim();
							if (CounterSignTarget.TREE.toString().equalsIgnoreCase(targetValue)) {
								target = CounterSignTarget.TREE;
							}
						}
						preRes = prep.preProcessPreCounterSign(
							docBytes,
							signAlgorithm,
							signerCertChain,
							extraParams,
							target,
							checkSignatures
						);
					}
					else {
						throw new AOException("No se reconoce el codigo de sub-operacion: " + subOperation); //$NON-NLS-1$
					}

					LOGGER.info("Se ha calculado el resultado de la prefirma y se devuelve"); //$NON-NLS-1$
				}
				catch (final RuntimeConfigNeededException e) {
					LOGGER.log(Level.SEVERE, "Se requiere intervencion del usuario para la prefirma de los datos", e); //$NON-NLS-1$
					out.print(ErrorManager.getErrorMessage(ErrorManager.CONFIGURATION_NEEDED, e.getRequestorText()) + ": " + e); //$NON-NLS-1$
					out.flush();
					return;
				}
				catch (final Exception e) {
					LOGGER.log(Level.SEVERE, "Error en la prefirma", e); //$NON-NLS-1$
					out.print(ErrorManager.getErrorMessage(ErrorManager.PRESIGN_ERROR) + ": " + e); //$NON-NLS-1$
					out.flush();
					return;
				}

				// Si la propiedad para habilitar el sistema de cache esta habilitada
				// se procedera a la escritura del fichero en cache
				if (cacheEnabled) {
					saveToCache(preRes, docBytes);
				}

				// Si se ha definido una clave HMAC para la comprobacion de integridad de
				// las firmas, agregamos a la respuesta la informacion de integridad que
				// asocie las prefirmas con el certificado de firma
				if (ConfigManager.getHMacKey() != null) {
					try {
						addVerificationCodes(preRes, signerCertChain[0]);
					}
					catch (final Exception e) {
						LOGGER.log(Level.SEVERE, "Error al generar los codigos de verificacion de las firmas: " + e, e); //$NON-NLS-1$
						out.print(ErrorManager.getErrorMessage(ErrorManager.GENERATING_CSV_ERROR) + ": " + e); //$NON-NLS-1$
						out.flush();
						return;
					}
				}

				out.print(
					Base64.encode(
						preRes.toString().getBytes(),
						true
					)
				);

				out.flush();

				LOGGER.info("== FIN PREFIRMA"); //$NON-NLS-1$
			}
			else if (PARAM_VALUE_OPERATION_POSTSIGN.equalsIgnoreCase(operation)) {

				LOGGER.info(" == POSTFIRMA en servidor"); //$NON-NLS-1$

				TriphaseData triphaseData;
				try {
					triphaseData = TriphaseData.parser(sessionData);
				}
				catch (final Exception e) {
					LOGGER.log(Level.SEVERE, "El formato de los parametros de operacion requeridos incorrecto", e); //$NON-NLS-1$
					out.print(ErrorManager.getErrorMessage(ErrorManager.INVALID_DATA_OPERATION_FORMAT) + ": " + e); //$NON-NLS-1$
					out.flush();
					return;
				}

				// Si se ha definido una clave HMAC para la comprobacion de
				// integridad de las firmas, comprobamos que las prefirmas y el
				// certificado de firma no se hayan modificado durante en
				// ningun punto de la operacion y que los PKCS#1 proporcionados
				// esten realizados con ese certificado
				if (ConfigManager.getHMacKey() != null) {
					try {
						checkSignaturesIntegrity(triphaseData, prep, signerCertChain[0]);
					}
					catch (final InvalidVerificationCodeException e) {
						LOGGER.log(Level.SEVERE, "Las prefirmas y/o el certificado obtenido no se corresponden con los generados en la prefirma", e); //$NON-NLS-1$
						out.print(ErrorManager.getErrorMessage(ErrorManager.CHECKING_CSV_ERROR) + ": " + e); //$NON-NLS-1$
						out.flush();
						return;
					}
					catch (final Exception e) {
						LOGGER.log(Level.SEVERE, "Error al comprobar los codigos de verificacion de las firmas", e); //$NON-NLS-1$
						out.print(ErrorManager.getErrorMessage(ErrorManager.CHECKING_CSV_ERROR) + ": " + e); //$NON-NLS-1$
						out.flush();
						return;
					}
				}

				final byte[] signedDoc;
				try {
					if (PARAM_VALUE_SUB_OPERATION_SIGN.equals(subOperation)) {
						signedDoc = prep.preProcessPostSign(
							docBytes,
							signAlgorithm,
							signerCertChain,
							extraParams,
							triphaseData
						);
					}
					else if (PARAM_VALUE_SUB_OPERATION_COSIGN.equals(subOperation)) {
						signedDoc = prep.preProcessPostCoSign(
							docBytes,
							signAlgorithm,
							signerCertChain,
							extraParams,
							triphaseData
						);
					}
					else if (PARAM_VALUE_SUB_OPERATION_COUNTERSIGN.equals(subOperation)) {

						CounterSignTarget target = CounterSignTarget.LEAFS;
						if (extraParams.containsKey(PARAM_NAME_TARGET_TYPE)) {
							final String targetValue = extraParams.getProperty(PARAM_NAME_TARGET_TYPE).trim();
							if (CounterSignTarget.TREE.toString().equalsIgnoreCase(targetValue)) {
								target = CounterSignTarget.TREE;
							}
						}

						signedDoc = prep.preProcessPostCounterSign(
							docBytes,
							signAlgorithm,
							signerCertChain,
							extraParams,
							triphaseData,
							target
						);
					}
					else {
						throw new AOException("No se reconoce el codigo de sub-operacion: " + subOperation); //$NON-NLS-1$
					}
				}
				catch (final RuntimeConfigNeededException e) {
					LOGGER.log(Level.SEVERE, "Se requiere intervencion del usuario para la postfirma de los datos", e); //$NON-NLS-1$
					out.print(ErrorManager.getErrorMessage(ErrorManager.CONFIGURATION_NEEDED) + ":" + e.getRequestorText() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					out.flush();
					return;
				}
				catch (final Exception e) {
					LOGGER.log(Level.SEVERE, "Error en la postfirma: " + e, e); //$NON-NLS-1$
					out.print(ErrorManager.getErrorMessage(ErrorManager.POSTSIGN_ERROR) + ": " + e); //$NON-NLS-1$
					out.flush();
					return;
				}

				// Establecemos parametros adicionales que se pueden utilizar para guardar el documento
				if (!extraParams.containsKey(PARAM_NAME_FORMAT)) {
					extraParams.setProperty(PARAM_NAME_FORMAT, format);
				}

				LOGGER.info(" Se ha calculado el resultado de la postfirma y se devuelve. Numero de bytes: " + signedDoc.length); //$NON-NLS-1$

				// Devolvemos al servidor documental el documento firmado
				LOGGER.info("Almacenamos la firma mediante el DocumentManager"); //$NON-NLS-1$
				final String newDocId;
				try {
					newDocId = docManager.storeDocument(docId, signerCertChain, signedDoc, extraParams);
				}
				catch(final Throwable e) {
					LOGGER.severe("Error al almacenar el documento: " + e); //$NON-NLS-1$
					out.print(ErrorManager.getErrorMessage(10) + ": " + e); //$NON-NLS-1$
					out.flush();
					return;
				}
				LOGGER.info("Documento almacenado"); //$NON-NLS-1$

				out.println(SUCCESS + newDocId);
				out.flush();

				LOGGER.info("== FIN POSTFIRMA"); //$NON-NLS-1$
			}
			else {
				out.println(ErrorManager.getErrorMessage(11));
			}
		}
        catch (final Exception e) {
        	LOGGER.log(Level.SEVERE, "No se pudo contestar a la peticion", e); //$NON-NLS-1$
        	try {
				response.sendError(HttpURLConnection.HTTP_INTERNAL_ERROR, "No se pude contestar a la peticion: " + e); //$NON-NLS-1$
			}
        	catch (final IOException e1) {
        		LOGGER.severe("No se pudo enviar un error HTTP 500: " + e1); //$NON-NLS-1$
			}
        	return;
        }
	}

	private static void configurePdfShadowAttackParameters(final Properties extraParams) {
		if (!Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.ALLOW_SHADOW_ATTACK))) {
			final int maxPagestoCheck = ConfigManager.getMaxPagesToCheckPSA();
			int pagesToCheck = DEFAULT_PAGES_TO_CHECK_PSA;
			if (extraParams.containsKey(PdfExtraParams.PAGES_TO_CHECK_PSA)) {
				final String pagesToCheckProp = extraParams.getProperty(PdfExtraParams.PAGES_TO_CHECK_PSA);
				if (PdfExtraParams.PAGES_TO_CHECK_PSA_VALUE_ALL.equalsIgnoreCase(pagesToCheckProp)) {
					pagesToCheck = Integer.MAX_VALUE;
				}
				else {
					try {
						pagesToCheck = Integer.parseInt(pagesToCheckProp);
					}
					catch (final Exception e) {
						pagesToCheck = DEFAULT_PAGES_TO_CHECK_PSA;
					}
				}
			}
			// Comprobaremos el menor numero de paginas posible que sera el indicado por la aplicacion
			// (el por defecto si no se paso un valor) o el maximo establecido por el servicio
			pagesToCheck = Math.min(pagesToCheck, maxPagestoCheck);
			if (pagesToCheck <= 0) {
				extraParams.setProperty(PdfExtraParams.ALLOW_SHADOW_ATTACK, Boolean.TRUE.toString());
			}
			else {
				extraParams.setProperty(PdfExtraParams.PAGES_TO_CHECK_PSA, Integer.toString(pagesToCheck));
			}
		}
	}

	/**
	 * Agrega a la informaci&oacute;n de firma trif&aacute;sica un c&oacute;digo de verificaci&oacute;n
	 * con el que se podr&aacute; comprobar que la prefirma y el certificado no se han modificado entre
	 * las operaciones de prefirma y postfirma.
	 * @param triphaseData Informaci&oacute;n trif&aacute;sica de la operaci&oacute;n.
	 * @param cert Certificado utilizado para crear la prefirma.
	 * @throws NoSuchAlgorithmException Nunca se deber&iacute;a dar.
	 * @throws InvalidKeyException Cuando la clave para la generaci&oacute;n del c&oacute;digo de
	 * verificaci&oacute;n no sea v&aacute;lida.
	 * @throws CertificateEncodingException Cuando no se puede codificar el certificado.
	 * @throws IllegalStateException Nunca se deber&iacute;a dar.
	 */
	private static void addVerificationCodes(final TriphaseData triphaseData, final X509Certificate cert)
			throws NoSuchAlgorithmException, InvalidKeyException, CertificateEncodingException,
			IllegalStateException {

		// TODO: Integrar la generacion de HMAC con salto
//		SecureRandom srandom = new SecureRandom();
//		byte[] salt = new byte[8];
//		srandom.nextBytes(salt);
//
//		char[] password = ...;
//		SecretKeyFactory factory = SecretKeyFactory.getInstance(algo);
//		KeySpec spec = new PBEKeySpec(password, salt, 10000, 128);
//		SecretKey key = factory.generateSecret(spec);

		final SecretKeySpec key = new SecretKeySpec(ConfigManager.getHMacKey().getBytes(CHARSET), HMAC_ALGORITHM);
		for (final TriSign triSign : triphaseData.getTriSigns()) {

			final String preSign = triSign.getProperty(TRIPHASE_PROP_PRESIGN);

			final Mac mac = Mac.getInstance(HMAC_ALGORITHM);
			mac.init(key);
			mac.update(preSign.getBytes(CHARSET));
			mac.update(ConfigManager.getHMacKey().getBytes(CHARSET));
			mac.update(cert.getEncoded());

			final byte[] hmac = mac.doFinal();
			triSign.addProperty(TRIPHASE_PROP_HMAC, Base64.encode(hmac));
		}
	}

	/**
	 * A&ntilde;ade el fichero en cach&eacute; para m&aacute;s tarde leerlo
	 * @param triphaseData datos de la prefirma
	 * @param docBytes datos a almacenar en la cach&eacute;
	 */
	private static void saveToCache(final TriphaseData triphaseData, final byte [] docBytes) {
		try {
			final String cacheId = docCacheManager.storeDocumentToCache(docBytes);
			triphaseData.getTriSigns().get(0).addProperty(TRIPHASE_PROP_CACHE_ID, cacheId);
		} catch (final IOException e) {
			LOGGER.log(Level.WARNING, "Error en la escritura del fichero en cache", e); //$NON-NLS-1$
		}
	}

	/**
	 * Comprueba que la prefirma y el certificado estuviesen asociados a un mismo proceso de
	 * prefirma anterior validando el c&oacute;digo MAC de verificaci&oacute;n que acompa&ntilde;a
	 * al PKCS#1. Despu&eacute;s, comprueba que con la clave privada de ese certificado se generase
	 * ese PKCS#1.
	 * //TODO: Esto podr&iacute;a ser mas robusto en las firmas XAdES, en la que no se utiliza la
	 * prefirma (par&aacute;metro PRE) para completar la firma, sino el parametro BASE. Habr&iacute;a
	 * que extraer la prefirma del BASE en lugar de coger la que se pasa como par&aacute;metro (que
	 * ya podr&iacute;a dejar de pasarse).
	 * @param triphaseData Informaci&oacute;n de la firma.
	 * @param prep Procesador que compone el formato de firma.
	 * @param cert Certificado que se declara haber usado en la prefirma.
	 * @throws InvalidVerificationCodeException Cuando el PKCS#1 de la firma no se generase con el
	 * certificado indicado o cuando no se pudiese comprobar.
	 * @throws IOException Cuando falla la decodificaci&oacute;n Base 64 de los datos.
	 */
	private static void checkSignaturesIntegrity(final TriphaseData triphaseData, final TriPhasePreProcessor prep, final X509Certificate cert) throws InvalidVerificationCodeException, IOException {

		final SecretKeySpec key = new SecretKeySpec(ConfigManager.getHMacKey().getBytes(CHARSET), HMAC_ALGORITHM);
		for (final TriSign triSign : triphaseData.getTriSigns()) {

			final String verificationHMac = triSign.getProperty(TRIPHASE_PROP_HMAC);
			if (verificationHMac == null) {
				throw new InvalidVerificationCodeException("Alguna de las firmas no contenida el codigo de verificacion"); //$NON-NLS-1$
			}


			final String preSign = triSign.getProperty(TRIPHASE_PROP_PRESIGN);

			//TODO: Integrar la validacion de HMAC con salto
			byte[] hmac;
			try {
				final Mac mac = Mac.getInstance(HMAC_ALGORITHM);
				mac.init(key);
				mac.update(preSign.getBytes(CHARSET));
				mac.update(ConfigManager.getHMacKey().getBytes(CHARSET));
				mac.update(cert.getEncoded());
				hmac = mac.doFinal();
			}
			catch (final Exception e) {
				throw new InvalidVerificationCodeException("No se pudo completar la verificacion de integridad de la firma", e); //$NON-NLS-1$
			}

			if (!Arrays.equals(hmac, Base64.decode(verificationHMac))) {
				throw new InvalidVerificationCodeException("Se ha detectado un error de integridad en los datos de firma"); //$NON-NLS-1$
			}
		}
	}

	private static class InvalidVerificationCodeException extends GeneralSecurityException {
		private static final long serialVersionUID = -4647005073272724194L;
		public InvalidVerificationCodeException(final String msg) {
			super(msg);
		}
		public InvalidVerificationCodeException(final String msg, final Throwable cause) {
			super(msg, cause);
		}
	}

	/**
	 * Identifica cuando se ha configurado una firma con el formato XAdES y la
	 * propiedad {@code mode} con el valor {@code explicit}. Esta no es una firma
	 * correcta pero, por compatibilidad con los tipos de firmas del Applet pesado,
	 * se ha incluido aqu&iacute;.
	 * @param format Formato declarado para la firma.
	 * @param config Par&aacute;metros adicionales declarados para la firma.
	 * @return {@code true} si se configura una firma <i>XAdES explicit</i>,
	 *         {@code false} en caso contrario.
	 * @deprecated Uso temporal hasta que se elimine el soporte de firmas XAdES
	 *             expl&iacute;citas.
	 */
	@Deprecated
	private static boolean isXadesExplicitConfigurated(final String format, final Properties config) {
		return format != null
				&& format.toLowerCase().startsWith("xades") //$NON-NLS-1$
				&& config != null
				&& AOSignConstants.SIGN_MODE_EXPLICIT.equalsIgnoreCase(config.getProperty("mode")); //$NON-NLS-1$
	}
}
