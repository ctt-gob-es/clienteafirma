/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signfolder.server.proxy;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/** Servicio de almacenamiento temporal de firmas.
 * &Uacute;til para servir de intermediario en comunicaci&oacute;n entre JavaScript y aplicaciones nativas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class RetrieveService extends HttpServlet {

	private static final long serialVersionUID = -3272368448371213403L;

	/** Log para registrar las acciones del servicio. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	/** Nombre del par&aacute;metro con la operaci&oacute;n realizada. */
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro con el identificador del fichero temporal. */
	private static final String PARAMETER_NAME_ID = "id"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro con la versi&oacute;n de la sintaxis de petici&oacute; utilizada. */
	private static final String PARAMETER_NAME_SYNTAX_VERSION = "v"; //$NON-NLS-1$

	private static final String OPERATION_RETRIEVE = "get"; //$NON-NLS-1$
	private static final String OPERATION_CHECK = "check"; //$NON-NLS-1$
	private static final String SUCCESS = "OK"; //$NON-NLS-1$

	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) throws ServletException, IOException {

		response.setHeader("Access-Control-Allow-Origin", "*"); //$NON-NLS-1$ //$NON-NLS-2$
		response.setContentType("text/plain"); //$NON-NLS-1$
		response.setCharacterEncoding("utf-8"); //$NON-NLS-1$

		final PrintWriter out = response.getWriter();

		final String operation = request.getParameter(PARAMETER_NAME_OPERATION);
		if (operation == null) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_MISSING_OPERATION_NAME));
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_OPERATION_NAME));
			out.flush();
			return;
		}

		// Si solo se queria identificar la operatividad del servicio, respondemos directamente
		if (OPERATION_CHECK.equals(operation)) {
			out.println(SUCCESS);
			out.flush();
			return;
		}

		LOGGER.info(" == INICIO RECUPERACION"); //$NON-NLS-1$

		final String syntaxVersion = request.getParameter(PARAMETER_NAME_SYNTAX_VERSION);
		if (syntaxVersion == null) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_MISSING_SYNTAX_VERSION));
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_SYNTAX_VERSION));
			out.flush();
			return;
		}

		if (OPERATION_RETRIEVE.equalsIgnoreCase(operation)) {
			retrieveSign(out, request);
		}
		else {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME));
			out.println(ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME));
		}
		out.flush();

		LOGGER.info(" == FIN RECUPERACION"); //$NON-NLS-1$

		// Antes de salir revisamos todos los ficheros y eliminamos los caducados.
		removeExpiredFiles();
	}

	/** Recupera los datos del servidor.
	 * @param out Respuesta a la petici&oacute;n.
	 * @param request Petici&oacute;n.
	 * @throws IOException Cuando ocurre un error al general la respuesta. */
	private static void retrieveSign(final PrintWriter out,
			                         final HttpServletRequest request) throws IOException {

		final String id = request.getParameter(PARAMETER_NAME_ID);
		if (id == null) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID));
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID));
			return;
		}

		LOGGER.info("Se solicita el fichero con el identificador: " + id); //$NON-NLS-1$

		final File inFile = new File(RetrieveConfig.getTempDir(), id);

		// No hacemos distincion si el archivo no existe, no es un fichero, no puede leerse o ha caducado
		// para evitar que un atacante conozca su situacion. Lo borramos despues de usarlo
		if (!inFile.isFile() || !inFile.canRead() || isExpired(inFile, RetrieveConfig.getExpirationTime())) {

			if (!inFile.exists()) {
				LOGGER.warning("El fichero con el identificador '" + id + "' no existe: " + inFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$
			}
			else if (!inFile.isFile()) {
				LOGGER.warning("El archivo con el identificador '" + id + "' no es un fichero: " + inFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$
			}
			else if (!inFile.canRead()) {
				LOGGER.warning("El fichero con el identificador '" + id + "' no tiene permisos de lectura: " + inFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$
			}
			else {
				LOGGER.warning("El fichero con el identificador '" + id + "' esta caducado: " + inFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$
			}

			out.println(
				ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA_ID)  + " ('" + id + "')" //$NON-NLS-1$ //$NON-NLS-2$
			);
			// Que el fichero sea de tipo fichero, implica que existe
			if (inFile.isFile() && !RetrieveConfig.DEBUG) {
				inFile.delete();
			}
		}
		else {
			try {
				final InputStream fis = new FileInputStream(inFile);
				out.println(new String(getDataFromInputStream(fis)));
				fis.close();
				LOGGER.info("Se recupera el fichero: " + inFile.getName()); //$NON-NLS-1$
			}
			catch (final IOException e) {
				LOGGER.severe("Error recuperando el fichero " + inFile.getAbsolutePath() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				out.println(ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA));
				return;
			}
			if (!RetrieveConfig.DEBUG) {
				inFile.delete();
			}
		}
	}

	/**
	 * Elimina del directorio temporal todos los ficheros que hayan sobrepasado el tiempo m&aacute;ximo
	 * de vida configurado.
	 */
	private static void removeExpiredFiles() {

		if (RetrieveConfig.DEBUG) {
			// No se limpia el directorio temporal por estar en modo depuracion
			return;
		}

		if (RetrieveConfig.getTempDir() != null && RetrieveConfig.getTempDir().isDirectory()) {
			for (final File file : RetrieveConfig.getTempDir().listFiles()) {
				try {
					if (file.isFile() && isExpired(file, RetrieveConfig.getExpirationTime())) {
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
		if (RetrieveConfig.DEBUG) {
			return false;
		}
		return System.currentTimeMillis() - file.lastModified() > expirationTimeLimit;
	}

	private static final int BUFFER_SIZE = 4096;

	/** Lee un flujo de datos de entrada y los recupera en forma de array de
     * octetos. Este m&eacute;todo consume pero no cierra el flujo de datos de entrada.
     * @param input Flujo de donde se toman los datos.
     * @return Los datos obtenidos del flujo.
     * @throws IOException Cuando ocurre un problema durante la lectura. */
    private static byte[] getDataFromInputStream(final InputStream input) throws IOException {
        if (input == null) {
            return new byte[0];
        }
        int nBytes = 0;
        final byte[] buffer = new byte[BUFFER_SIZE];
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        while ((nBytes = input.read(buffer)) != -1) {
            baos.write(buffer, 0, nBytes);
        }
        return baos.toByteArray();
    }
}