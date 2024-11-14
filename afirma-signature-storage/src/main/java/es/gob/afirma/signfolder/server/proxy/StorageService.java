/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signfolder.server.proxy;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.URLDecoder;
import java.util.Hashtable;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/** Servicio de almacenamiento temporal de firmas. &Uacute;til para servir de intermediario en comunicaci&oacute;n
 * entre JavaScript y aplicaciones nativas.
 * @author Tom&aacute;s Garc&iacute;a-;er&aacute;s. */
public final class StorageService extends HttpServlet {

	private static final long serialVersionUID = -3272368448371213403L;

	/** Codificaci&oacute;n de texto. */
	private static final String DEFAULT_ENCODING = "utf-8"; //$NON-NLS-1$

	/** <i>Log</i> para registrar las acciones del servicio. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	/** Nombre del par&aacute;metro con la operaci&oacute;n realizada. */
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro con el identificador del fichero temporal. */
	private static final String PARAMETER_NAME_ID = "id"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro con la versi&oacute;n de la sintaxis de petici&oacute;n utilizada. */
	private static final String PARAMETER_NAME_SYNTAX_VERSION = "v"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro con los datos a firmar. */
	private static final String PARAMETER_NAME_DATA = "dat"; //$NON-NLS-1$

	private static final String OPERATION_STORE = "put"; //$NON-NLS-1$
	private static final String OPERATION_CHECK = "check"; //$NON-NLS-1$
	private static final String SUCCESS = "OK"; //$NON-NLS-1$

	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) throws ServletException, IOException {

		response.setHeader("Access-Control-Allow-Origin", "*"); //$NON-NLS-1$ //$NON-NLS-2$
		response.setContentType("text/plain"); //$NON-NLS-1$
		response.setCharacterEncoding("utf-8"); //$NON-NLS-1$

		final PrintWriter out = response.getWriter();

		// Si solo se queria identificar la operatividad del servicio, respondemos directamente
		if (request.getMethod().equalsIgnoreCase("GET") //$NON-NLS-1$
				&& OPERATION_CHECK.equals(request.getParameter(PARAMETER_NAME_OPERATION))) {
			out.println(SUCCESS);
			out.flush();
			return;
		}

		LOGGER.info(" == INICIO GUARDADO"); //$NON-NLS-1$

		final String operation;
		final String syntaxVersion;
		final String id;
		final String data;

		if (request.getMethod().equalsIgnoreCase("GET")) { //$NON-NLS-1$
			operation     = request.getParameter(PARAMETER_NAME_OPERATION);
			syntaxVersion = request.getParameter(PARAMETER_NAME_SYNTAX_VERSION);
			id            = request.getParameter(PARAMETER_NAME_ID);
			data          = request.getParameter(PARAMETER_NAME_DATA);
		}
		else {

			// Leemos la entrada
			int n;
			final byte[] buffer = new byte[1024];
			final ByteArrayOutputStream baos = new ByteArrayOutputStream();
			final ServletInputStream sis = request.getInputStream();
			while ((n = sis.read(buffer)) > 0) {
				baos.write(buffer, 0, n);
			}
			baos.close();
			sis.close();

			// Separamos los parametros y sus valores
			final Hashtable<String, String> params = new Hashtable<String, String>();
			final String[] urlParams = new String(baos.toByteArray()).split("&"); //$NON-NLS-1$
			for (final String param : urlParams) {
				final int equalsPos = param.indexOf('=');
				if (equalsPos != -1) {
					params.put(param.substring(0, equalsPos), param.substring(equalsPos + 1));
				}
			}

			operation     = params.get(PARAMETER_NAME_OPERATION);
			syntaxVersion = params.get(PARAMETER_NAME_SYNTAX_VERSION);
			id            = params.get(PARAMETER_NAME_ID);
			data          = params.get(PARAMETER_NAME_DATA);
		}

		if (operation == null) {
			LOGGER.warning("No se ha indicado codigo de operacion"); //$NON-NLS-1$
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_OPERATION_NAME));
			out.flush();
			return;
		}

		if (syntaxVersion == null) {
			LOGGER.warning("No se ha indicado la version del formato de llamada"); //$NON-NLS-1$
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_SYNTAX_VERSION));
			out.flush();
			return;
		}

		if (OPERATION_STORE.equalsIgnoreCase(operation)) {
			storeSign(out, id, data);
		}
		else {
			out.println(ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME));
		}
		out.flush();

		LOGGER.info(" == FIN GUARDADO"); //$NON-NLS-1$

		// Antes de salir revisamos todos los ficheros y eliminamos los caducados.
		removeExpiredFiles();
	}

	/** Almacena una firma en servidor.
	 * @param out Respuesta a la petici&oacute;n.
	 * @param id Identificador de los datos a almacenar.
	 * @param data Datos a almacenar.
	 * @throws IOException Cuando ocurre un error al general la respuesta. */
	private static void storeSign(final PrintWriter out,
								  final String id,
								  final String data) throws IOException {
		if (id == null) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID));
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID));
			return;
		}

		LOGGER.info("Se solicita guardar un fichero con el identificador: " + id); //$NON-NLS-1$

		String dataText;
		if (data == null || data.isEmpty()) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA));
			// Si no se indican los datos, se transmite el error en texto plano
			// a traves del fichero generado
			dataText = ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA);
		}
		else {
			dataText = URLDecoder.decode(data, DEFAULT_ENCODING);
			if (StorageConfig.getMaxDataSize() > 0 && dataText.getBytes().length > StorageConfig.getMaxDataSize() && !StorageConfig.DEBUG) {
				LOGGER.warning(
					"El tamano de los datos (" + dataText.getBytes().length + ") es mayor de lo permitido: " + StorageConfig.getMaxDataSize() //$NON-NLS-1$ //$NON-NLS-2$
				);
				dataText = ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA);
			}
		}

		if (!StorageConfig.getTempDir().isDirectory()) {
			StorageConfig.getTempDir().mkdirs();
		}

		final File outFile = new File(StorageConfig.getTempDir(), id);
		try {
			final OutputStream fos = new FileOutputStream(outFile);
			final BufferedOutputStream bos = new BufferedOutputStream(fos);
			bos.write(dataText.getBytes());
			bos.flush();
			bos.close();
			fos.close();
		}
		catch (final IOException e) {
			LOGGER.severe("No se ha podido generar el fichero temporal para el envio de datos a la web: " + e); //$NON-NLS-1$
			out.println(ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_WITH_WEB));
			return;
		}

		LOGGER.info("Se guardo correctamente el fichero: " + outFile.getAbsolutePath()); //$NON-NLS-1$

		out.print(SUCCESS);
	}

	/**
	 * Elimina del directorio temporal todos los ficheros que hayan sobrepasado el tiempo m&aacute;ximo
	 * de vida configurado.
	 */
	private static void removeExpiredFiles() {
		if (StorageConfig.getTempDir() != null && StorageConfig.getTempDir().isDirectory()) {
			if (StorageConfig.DEBUG) {
				// No se limpia el directorio temporal por estar en modo depuracion
				return;
			}
			for (final File file : StorageConfig.getTempDir().listFiles()) {
				try {
					if (file.isFile() && isExpired(file, StorageConfig.getExpirationTime())) {
						file.delete();
					}
				}
				catch(final Exception e) {
					// Suponemos que el fichero ha sido eliminado por otro hilo
					LOGGER.warning(
						"No se ha podido eliminar el fichero '" + file.getAbsolutePath() + "', es probable que se elimine en otro hilo de ejecucion: " + e //$NON-NLS-1$ //$NON-NLS-2$
					);
				}
			}
		}
	}

	private static boolean isExpired(final File file, final long expirationTimeLimit) {
		if (StorageConfig.DEBUG) {
			return false;
		}
		return System.currentTimeMillis() - file.lastModified() > expirationTimeLimit;
	}
}
