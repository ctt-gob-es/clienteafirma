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
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/** Servicio de almacenamiento temporal de firmas.
 * &Uacute;til para servir de intermediario en comunicaci&oacute;n * entre JavaScript y <i>Apps</i> m&oacute;viles nativas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
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

	/** Fichero de configuraci&oacute;n. */
	private static final String CONFIG_FILE = "configuration.properties"; //$NON-NLS-1$

	private static RetrieveConfig CONFIG;
	static {
		try {
			CONFIG = new RetrieveConfig();
			CONFIG.load(CONFIG_FILE);
		}
		catch (final IOException e) {
			CONFIG = null;
			LOGGER.log(Level.SEVERE, ErrorManager.genError(ErrorManager.ERROR_CONFIGURATION_FILE_PROBLEM), e);
		}
	}

	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) throws ServletException, IOException {

		LOGGER.info("== INICIO DE LA RECUPERACION =="); //$NON-NLS-1$

		final String operation = request.getParameter(PARAMETER_NAME_OPERATION);
		final String syntaxVersion = request.getParameter(PARAMETER_NAME_SYNTAX_VERSION);
		response.setHeader("Access-Control-Allow-Origin", "*"); //$NON-NLS-1$ //$NON-NLS-2$
		response.setContentType("text/plain"); //$NON-NLS-1$
		response.setCharacterEncoding("utf-8"); //$NON-NLS-1$

		final PrintWriter out = response.getWriter();
		if (operation == null) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_MISSING_OPERATION_NAME));
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_OPERATION_NAME));
			out.flush();
			return;
		}
		if (syntaxVersion == null) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_MISSING_SYNTAX_VERSION));
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_SYNTAX_VERSION));
			out.flush();
			return;
		}

		if (OPERATION_RETRIEVE.equalsIgnoreCase(operation)) {
			retrieveSign(out, request, CONFIG);
		}
		else {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME));
			out.println(ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME));
		}
		out.flush();
		LOGGER.info("== FIN DE LA RECUPERACION =="); //$NON-NLS-1$

		// Antes de salir revisamos todos los ficheros y eliminamos los caducados.
		LOGGER.info("Limpiamos el directorio temporal"); //$NON-NLS-1$
		removeExpiredFiles(CONFIG);
		LOGGER.info("Fin de la limpieza"); //$NON-NLS-1$
	}

	/** Recupera la firma del servidor.
	 * @param out Respuesta a la petici&oacute;n.
	 * @param request Petici&oacute;n.
	 * @param config Opciones de configuraci&oacute;n de la operaci&oacute;n.
	 * @throws IOException Cuando ocurre un error al general la respuesta. */
	private static void retrieveSign(final PrintWriter out,
			                         final HttpServletRequest request,
			                         final RetrieveConfig config) throws IOException {

		final String id = request.getParameter(PARAMETER_NAME_ID);
		if (id == null) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID));
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID));
			return;
		}

		LOGGER.info("Se solicita el fichero con el identificador: " + id); //$NON-NLS-1$

		final File inFile = new File(config.getTempDir(), id);

		// No hacemos distincion si el archivo no existe, no es un fichero, no puede leerse o ha caducado
		// para evitar que un atacante conozca su situacion. Lo borramos despues de usarlo
		if (!inFile.exists() || !inFile.isFile() || !inFile.canRead() || isExpired(inFile, config.getExpirationTime())) {

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
			if (inFile.isFile()) {
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
				LOGGER.severe("Error recupendo el fichero '" + inFile.getAbsolutePath() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				out.println(ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA));
				return;
			}
			inFile.delete();
		}
	}

	/** Elimina del directorio temporal todos los ficheros que hayan sobrepasado el tiempo m&aacute;ximo
	 * de vida configurado.
	 * @param config Opciones de configuraci&oacute;n de la operaci&oacute;n. */
	private static void removeExpiredFiles(final RetrieveConfig config) {
		if (config != null && config.getTempDir() != null && config.getTempDir().exists()) {
			for (final File file : config.getTempDir().listFiles()) {
				try {
					if (file.isFile() && isExpired(file, config.getExpirationTime())) {
						LOGGER.fine("Eliminamos el fichero caducado: " + file.getAbsolutePath()); //$NON-NLS-1$
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
		return System.currentTimeMillis() - file.lastModified() > expirationTimeLimit;
	}

	private static final int BUFFER_SIZE = 4096;

	/** Lee un flujo de datos de entrada y los recupera en forma de array de
     * octetos. Este m&eacute;todo consume pero no cierra el flujo de datos de
     * entrada.
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