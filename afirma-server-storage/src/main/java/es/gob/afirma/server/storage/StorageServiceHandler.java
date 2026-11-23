/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */
package es.gob.afirma.server.storage;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URLDecoder;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

public class StorageServiceHandler {
	
	/** Log para registrar las acciones del servicio. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$
	
	/** Nombre del par&aacute;metro con la operaci&oacute;n realizada. */
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$
	
	/** Nombre del par&aacute;metro con la versi&oacute;n de la sintaxis de petici&oacute; utilizada. */
	private static final String PARAMETER_NAME_SYNTAX_VERSION = "v"; //$NON-NLS-1$
	
	/** Nombre del par&aacute;metro con el identificador del fichero temporal. */
	private static final String PARAMETER_NAME_ID = "id"; //$NON-NLS-1$
	
	/** Nombre del par&aacute;metro con los datos a firmar. */
	private static final String PARAMETER_NAME_DATA = "dat"; //$NON-NLS-1$
	
	private static final String OPERATION_CHECK = "check"; //$NON-NLS-1$
	private static final String OPERATION_STORE = "put"; //$NON-NLS-1$
	private static final String SUCCESS = "OK"; //$NON-NLS-1$
	
	private static final Charset DEFAULT_CHARSET = StandardCharsets.UTF_8;
	
	/** Codificaci&oacute;n de texto. */
	private static final String DEFAULT_ENCODING = "utf-8"; //$NON-NLS-1$
	
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
		
		LOGGER.info(" == INICIO GUARDADO"); //$NON-NLS-1$

		final String syntaxVersion = parameters.get(PARAMETER_NAME_SYNTAX_VERSION);
		if (syntaxVersion == null) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_MISSING_SYNTAX_VERSION));
			return ErrorManager.genError(ErrorManager.ERROR_MISSING_SYNTAX_VERSION).getBytes(DEFAULT_CHARSET);
		}
		
		if (!OPERATION_STORE.equalsIgnoreCase(operation)) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME));
			return ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME).getBytes(DEFAULT_CHARSET);		
		}
		
		byte[] result = storeSign(parameters);
		
		// Antes de salir revisamos todos los ficheros y eliminamos los caducados.
		removeExpiredFiles();
		
		LOGGER.info(" == FIN GUARDADO"); //$NON-NLS-1$
		
		return result;
		
	}
	
	/** Almacena una firma en servidor.
	 * @param parameters Par&aacute;metros de la petici&oacute;n.
	 * @throws IOException Cuando ocurre un error al general la respuesta. */
	private static byte[] storeSign(final Map<String, String> parameters) throws IOException {
		
		final String id = parameters.get(PARAMETER_NAME_ID);
		if (id == null) {
			LOGGER.warning(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID));
			return ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID).getBytes(DEFAULT_CHARSET);
		}

		if (id.indexOf('.') > -1 || id.indexOf('/') > -1 || id.indexOf('\\') > -1) {
			LOGGER.log(Level.WARNING, "Se han encontrado caracteres no validos en el identificador de datos"); //$NON-NLS-1$
			return ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA_ID).getBytes(DEFAULT_CHARSET);
		}

		LOGGER.info("Se solicita guardar un fichero con el identificador: " + id); //$NON-NLS-1$

		String dataText;
		String data = parameters.get(PARAMETER_NAME_DATA);
		if (data == null || data.isEmpty()) {
			LOGGER.warning("No se le han porcionado los datos al servicio. Se transmite el error a traves del fichero"); //$NON-NLS-1$
			// Si no se indican los datos, se transmite el error en texto plano
			// a traves del fichero generado
			dataText = ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA);
		}
		else {
			dataText = URLDecoder.decode(data, DEFAULT_ENCODING);
			if (StorageConfig.getMaxDataSize() > 0 && dataText.getBytes().length > StorageConfig.getMaxDataSize() && !StorageConfig.DEBUG) {
				LOGGER.warning(
					"El tamano de los datos (" + dataText.getBytes().length + ") es mayor de lo permitido: " + StorageConfig.getMaxDataSize()  //$NON-NLS-1$ //$NON-NLS-2$
					+ ". Se transmite el error a traves del fichero." //$NON-NLS-1$
				);
				dataText = ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA);
			}
		}

		if (!StorageConfig.getTempDir().isDirectory()) {
			StorageConfig.getTempDir().mkdirs();
		}

		File outFile;
		try {
			outFile = composeTargetFile(StorageConfig.getTempDir(), id);
		}
		catch (final SecurityException e) {
			LOGGER.log(Level.WARNING, "Se ha intentado acceder a un fichero fuera del path configurado", e); //$NON-NLS-1$
			return ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA_ID).getBytes(DEFAULT_CHARSET);
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido componer la ruta del fichero a guardar"); //$NON-NLS-1$
			return ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA_ID).getBytes(DEFAULT_CHARSET);
		}

		try (final OutputStream fos = new FileOutputStream(outFile);
			final BufferedOutputStream bos = new BufferedOutputStream(fos); ){
			bos.write(dataText.getBytes());
			bos.flush();
		}
		catch (final IOException e) {
			LOGGER.severe("No se ha podido generar el fichero temporal para el envio de datos a la web: " + e); //$NON-NLS-1$
			return ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_WITH_WEB).getBytes(DEFAULT_CHARSET);
		}

		LOGGER.info("Se guardo correctamente el fichero: " + outFile.getAbsolutePath()); //$NON-NLS-1$

		return SUCCESS.getBytes();
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
