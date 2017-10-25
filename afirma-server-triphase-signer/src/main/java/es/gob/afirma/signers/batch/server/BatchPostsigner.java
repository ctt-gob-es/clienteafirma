/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.BatchConfigManager;
import es.gob.afirma.signers.batch.SignBatch;
import es.gob.afirma.signers.batch.SignBatchConcurrent;
import es.gob.afirma.signers.batch.SignBatchSerial;

/** Realiza la tercera (y &uacute;ltima) fase de un proceso de firma por lote.
 * Servlet implementation class BatchPostsigner
 */
public final class BatchPostsigner extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String CONFIG_FILE = "config.properties"; //$NON-NLS-1$

	/** Variable de entorno que determina el directorio en el que buscar el fichero de configuraci&oacute;n. */
	private static final String ENVIRONMENT_VAR_CONFIG_DIR = "clienteafirma.config.path"; //$NON-NLS-1$

	private static final String BATCH_XML_PARAM = "xml"; //$NON-NLS-1$
	private static final String BATCH_CRT_PARAM = "certs"; //$NON-NLS-1$
	private static final String BATCH_TRI_PARAM = "tridata"; //$NON-NLS-1$

	private static final String CONFIG_PARAM_ALLOW_ORIGIN = "Access-Control-Allow-Origin"; //$NON-NLS-1$

	/** Or&iacute;genes permitidos por defecto desde los que se pueden realizar peticiones al servicio. */
	private static final String CONFIG_DEFAULT_VALUE_ALLOW_ORIGIN = "*"; //$NON-NLS-1$

	private static final Properties config;

	static {
		try {
			InputStream configIs = null;
			String configDir;
			try {
				configDir = System.getProperty(ENVIRONMENT_VAR_CONFIG_DIR);
			}
			catch (final Exception e) {
				LOGGER.warning(
						"No se pudo acceder a la variable de entorno '" + ENVIRONMENT_VAR_CONFIG_DIR + //$NON-NLS-1$
						"' que configura el directorio del fichero de configuracion: " + e);//$NON-NLS-1$
				configDir = null;
			}
			if (configDir != null) {
				final File configFile = new File(configDir, CONFIG_FILE).getCanonicalFile();
				if (!configFile.isFile() || !configFile.canRead()) {
					LOGGER.warning(
							"No se encontro el fichero " + CONFIG_FILE + " en el directorio configurado en la variable " + //$NON-NLS-1$ //$NON-NLS-2$
									ENVIRONMENT_VAR_CONFIG_DIR + ": " + configFile.getAbsolutePath() + //$NON-NLS-1$
									"\nSe buscara en el CLASSPATH."); //$NON-NLS-1$
				}
				else {
					configIs = new FileInputStream(configFile);
				}
			}

			if (configIs == null) {
				configIs = BatchPostsigner.class.getClassLoader().getResourceAsStream(CONFIG_FILE);
			}

			if (configIs == null) {
				throw new IOException("No se encuentra el fichero de configuracion del servicio: " + CONFIG_FILE); //$NON-NLS-1$
			}
			config = new Properties();
			config.load(configIs);
			configIs.close();
		}
		catch(final Exception e) {
			throw new RuntimeException("Error en la carga del fichero de propiedades: " + e, e); //$NON-NLS-1$
		}
	}

	/** Realiza la tercera y &uacute;ltima fase de un proceso de firma por lote.
	 * Debe recibir la definici&oacute;n XML (<a href="../doc-files/batch-scheme.html">descripci&oacute;n
	 * del formato</a>) del lote (exactamente la misma enviada para la primera fase)
	 * en un XML pero convertido a Base64 (puede ser en formato <i>URL Safe</i>) y la cadena de
	 * certificados del firmante (exactamente la misma que la enviada en la primera fase),
	 * convertidos a Base64 (puede ser <i>URL Safe</i>) y separados por punto y coma (<code>;</code>).<br>
	 * Devuelve un XML de resumen de resultado (<a href="../doc-files/resultlog-scheme.html">descripci&oacute;n
	 * del formato</a>)
	 * @see HttpServlet#service(HttpServletRequest request, HttpServletResponse response) */
	@Override
	protected void service(final HttpServletRequest request,
			               final HttpServletResponse response) throws ServletException,
			                                                          IOException {
		final String xml = request.getParameter(BATCH_XML_PARAM);
		if (xml == null) {
			LOGGER.severe("No se ha recibido una definicion de lote en el parametro " + BATCH_XML_PARAM); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"No se ha recibido una definicion de lote en el parametro " + BATCH_XML_PARAM //$NON-NLS-1$
			);
			return;
		}

		final SignBatch batch;
		try {
			final byte[] batchConfig = BatchServerUtil.getSignBatchConfig(xml);
			batch = BatchConfigManager.isConcurrentMode() ?
					new SignBatchConcurrent(batchConfig) :
						new SignBatchSerial(batchConfig);
		}
		catch(final Exception e) {
			LOGGER.severe("La definicion de lote es invalida: " + e); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"La definicion de lote es invalida: " + e //$NON-NLS-1$
			);
			return;
		}

		final String certListUrlSafeBase64 = request.getParameter(BATCH_CRT_PARAM);
		if (certListUrlSafeBase64 == null) {
			LOGGER.severe("No se ha recibido la cadena de certificados del firmante en el parametro " + BATCH_CRT_PARAM); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"No se ha recibido la cadena de certificados del firmante en el parametro " + BATCH_CRT_PARAM //$NON-NLS-1$
			);
			return;
		}

		final X509Certificate[] certs;
		try {
			certs = BatchServerUtil.getCertificates(certListUrlSafeBase64);
		}
		catch (final Exception e) {
			LOGGER.severe("La cadena de certificados del firmante es invalida: " + e); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"La cadena de certificados del firmante es invalida: " + e //$NON-NLS-1$
			);
			return;
		}

		final String triphaseDataAsUrlSafeBase64 = request.getParameter(BATCH_TRI_PARAM);
		if (triphaseDataAsUrlSafeBase64 == null) {
			LOGGER.severe("No se ha recibido el resultado de las firmas cliente en el parametro " + BATCH_TRI_PARAM); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"No se ha recibido el resultado de las firmas cliente en el parametro " + BATCH_TRI_PARAM //$NON-NLS-1$
			);
			return;
		}

		final TriphaseData td;
		try {
			td = BatchServerUtil.getTriphaseData(triphaseDataAsUrlSafeBase64);
		}
		catch(final Exception e) {
			LOGGER.severe("El XML de firmas cliente es invalido: " + e); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_BAD_REQUEST,
				"El XML de firmas cliente es invalido: " + e //$NON-NLS-1$
			);
			return;
		}

		final String ret;
		try {
			ret = batch.doPostBatch(certs, td);
		}
		catch (final Exception e) {
			LOGGER.severe("Error en el postproceso del lote: " + e); //$NON-NLS-1$
			response.sendError(
				HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
				"Error en el postproceso del lote: " + e //$NON-NLS-1$
			);
			return;
		}

		String allowOrigin = CONFIG_DEFAULT_VALUE_ALLOW_ORIGIN;
		if (BatchPostsigner.config.contains(CONFIG_PARAM_ALLOW_ORIGIN)) {
			allowOrigin = BatchPostsigner.config.getProperty(CONFIG_PARAM_ALLOW_ORIGIN);
		}

		response.setHeader("Access-Control-Allow-Origin", allowOrigin); //$NON-NLS-1$
		response.setContentType("text/xml;charset=UTF-8"); //$NON-NLS-1$
		try (
			final PrintWriter writer = response.getWriter();
		) {
			writer.write(ret);
			writer.flush();
		}
	}

}
