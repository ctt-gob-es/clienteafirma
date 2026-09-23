/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.server.retriever;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * Servicio de almacenamiento temporal de firmas.
 * &Uacute;til para servir de intermediario en comunicaci&oacute;n entre JavaScript y aplicaciones nativas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s.
 */
public final class RetrieveServiceHandler {

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

	private static final int BUFFER_SIZE = 4096;
	
	private static final Charset DEFAULT_CHARSET = StandardCharsets.UTF_8;
	
	private static Boolean allowExtendedLogs = null;

	public static byte[] process(final Map<String, String> parameters) throws IOException {

		final String operation = parameters.get(PARAMETER_NAME_OPERATION);
		if (operation == null) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_MISSING_OPERATION_NAME));
			return ErrorManager.genError(ErrorManager.ERROR_MISSING_OPERATION_NAME).getBytes(DEFAULT_CHARSET);
		}

		// Si solo se queria identificar la operatividad del servicio, respondemos directamente
		if (OPERATION_CHECK.equals(operation)) {
			return SUCCESS.getBytes(DEFAULT_CHARSET);
		}

		LOGGER.info(" == INICIO RECUPERACION"); //$NON-NLS-1$

		final String syntaxVersion = parameters.get(PARAMETER_NAME_SYNTAX_VERSION);
		if (syntaxVersion == null) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_MISSING_SYNTAX_VERSION));
			return ErrorManager.genError(ErrorManager.ERROR_MISSING_SYNTAX_VERSION).getBytes(DEFAULT_CHARSET);
		}

		if (!OPERATION_RETRIEVE.equalsIgnoreCase(operation)) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME));
			return ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME).getBytes(DEFAULT_CHARSET);
			
		}
		byte[] result = retrieveSign(parameters);

		// Antes de salir revisamos todos los ficheros y eliminamos los caducados.
		removeExpiredFiles();
		
		return result;
	}

	/**
	 * Recupera los datos del servidor.
	 * @param parameters Par&aacute;metros de la petici&oacute;n.
	 * @throws IOException Cuando ocurre un error al general la respuesta.
	 */
	private static byte[] retrieveSign(final Map<String, String> parameters) throws IOException {

		final String id = parameters.get(PARAMETER_NAME_ID);
		if (id == null) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID));
			return ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID).getBytes(DEFAULT_CHARSET);
		}

		if (id.indexOf('.') > -1 || id.indexOf('/') > -1 || id.indexOf('\\') > -1) {
			LOGGER.log(Level.WARNING, "Se han encontrado caracteres no validos en el identificador de datos"); //$NON-NLS-1$
			return ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA_ID).getBytes(DEFAULT_CHARSET);
		}

		LOGGER.info("Se solicita el fichero con el identificador: " + getTrim(id)); //$NON-NLS-1$

		File inFile;
		try {
			inFile = composeTargetFile(RetrieveConfig.getTempDir(), id);
		}
		catch (final SecurityException e) {
			LOGGER.log(Level.WARNING, "Se ha intentado acceder a un fichero fuera del path configurado", e); //$NON-NLS-1$
			return ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA_ID).getBytes(DEFAULT_CHARSET);
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido componer la ruta del fichero solicitado"); //$NON-NLS-1$
			return ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA_ID).getBytes(DEFAULT_CHARSET);
		}

		// No hacemos distincion si el archivo no existe, no es un fichero, es un enlace, no puede leerse o ha caducado
		// para evitar que un atacante conozca su situacion. Lo borramos despues de usarlo
		if (!inFile.isFile() || Files.isSymbolicLink(inFile.toPath()) || !inFile.canRead() || isExpired(inFile, RetrieveConfig.getExpirationTime())) {

			if (!inFile.exists()) {
				LOGGER.warning("El fichero con el identificador '" + getTrim(id) + "' no existe: " + getTrim(inFile.getAbsolutePath())); //$NON-NLS-1$ //$NON-NLS-2$
			}
			else if (!inFile.isFile()) {
				LOGGER.warning("El archivo con el identificador '" + getTrim(id) + "' no es un fichero: " + getTrim(inFile.getAbsolutePath())); //$NON-NLS-1$ //$NON-NLS-2$
			}
			else if (Files.isSymbolicLink(inFile.toPath())) {
				LOGGER.warning("El fichero con el identificador '" + getTrim(id) + "' es un enlace simbolico"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			else if (!inFile.canRead()) {
				LOGGER.warning("El fichero con el identificador '" + getTrim(id) + "' no tiene permisos de lectura: " + getTrim(inFile.getAbsolutePath())); //$NON-NLS-1$ //$NON-NLS-2$
			}
			else {
				LOGGER.warning("El fichero con el identificador '" + getTrim(id) + "' esta caducado: " + getTrim(inFile.getAbsolutePath())); //$NON-NLS-1$ //$NON-NLS-2$
			}

			// Que el fichero sea de tipo fichero, implica que existe
			if (inFile.isFile() && !RetrieveConfig.DEBUG) {
				inFile.delete();
			}
			return ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA_ID).getBytes(DEFAULT_CHARSET);
		}
		
		
		try (final InputStream fis = new FileInputStream(inFile)) {
			LOGGER.info("Se recupera el fichero: " + getTrim(inFile.getName())); //$NON-NLS-1$
			return getDataFromInputStream(fis);
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Error recuperando el fichero " + getTrim(inFile.getAbsolutePath()), e); //$NON-NLS-1$
			return ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA).getBytes(DEFAULT_CHARSET);
		}
		finally {
			if (!RetrieveConfig.DEBUG) {
				inFile.delete();
			}
		}
	}

	/**
	 * Compone el fichero de destino que se debe recuperar.
	 * @param baseDir Directorio base del fichero.
	 * @param filename Nombre del fichero.
	 * @return Fichero de destino.
	 * @throws IOException Cuando no se pueda componer la ruta del fichero.
	 * @throws SecurityException Cuando se trate de cargar un fichero fuera
	 * del directorio base.
	 */
	private static File composeTargetFile(final File baseDir, final String filename)
			throws IOException, SecurityException {

		final File targetFile = new File(baseDir, filename).getCanonicalFile();
		if (!baseDir.equals(targetFile.getParentFile())) {
			throw new SecurityException("El fichero solicitado no esta en el raiz del directorio"); //$NON-NLS-1$
		}

		return targetFile;
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

    /**
     * Limita la cadena a 200 caracteres. En caso de que la propiedad {@code "allow.extended.logs"}
     * est&eacute; activa, se omite el recortarla.
     * @param text Cadena de texto que limitar.
     * @return Cadena tratada.
     */
    public static String getTrim(final String text) {

    	if (text == null) {
    		return null;
    	}

    	if (allowExtendedLogs == null) {
    		allowExtendedLogs = Boolean.valueOf(Boolean.getBoolean("allow.extended.logs")); //$NON-NLS-1$
    	}

    	if (!allowExtendedLogs.booleanValue() && text.length() >= 200) {
			return text.substring(0, 200) + "..."; //$NON-NLS-1$
		}

    	return text;
    }
}