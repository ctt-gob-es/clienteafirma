/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server.cache;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.SecureRandom;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;

/** Implementaci&oacute;n de acceso a gestor documental mediante cache&eacute;
 */
public final class FileSystemCacheManager implements DocumentCacheManager {

	private static final String TMP_DIR = "tmpDir"; //$NON-NLS-1$
	private static final String EXP_TIME = "expTime"; //$NON-NLS-1$
	private static final String MAX_USE_TO_CLEANING = "maxUseToCleaning"; //$NON-NLS-1$

	static String tmpDir = ""; //$NON-NLS-1$

	final long expTime;
	final int maxUseToCleaning;

	private static int uses = 0;

	final static Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Construye la clase de acceso a gestor documental usando sistema de ficheros.
	 * @param config Configuraci&oacute;n del gestor (directorios, etc.) */
	public FileSystemCacheManager(final Properties config) {

		tmpDir = config.getProperty(TMP_DIR);

		final File directory = new File(tmpDir);

		//En caso de que el directorio configurado no exista, se crea una carpeta en el sistema
		if (!directory.isDirectory()) {

			final File newDirectory = new File(System.getProperty("user.dir") //$NON-NLS-1$
					+ File.separator + "temp");  //$NON-NLS-1$

			tmpDir = newDirectory.getAbsolutePath();

			if (!newDirectory.isDirectory()) {
				newDirectory.mkdir();
			}
		}

		this.expTime = Long.parseLong(config.getProperty(EXP_TIME));
		this.maxUseToCleaning = Integer.parseInt(config.getProperty(MAX_USE_TO_CLEANING));
	}

	@Override
	public void cleanCache() throws IOException {
		new ExpiredDocumentsCleanerThread(this.expTime).start();
	}

	/**
	 * M&eacute;todo que borra todos los archivos que hayan expirado en cach&eacute;
	 * @param expirationTime tiempo de expiraci&oacute;n a comprobar para el archivo
	 */
	public static void deleteExpiredDocuments(final long expirationTime) {
		final File directory = new File(tmpDir);

		if (tmpDir != null && directory.isDirectory()) {
			for (final File file : directory.listFiles()) {
				try {
					if (file.isFile() && isExpired(file, expirationTime)) {
						file.delete();
					}
				}
				catch(final Exception e) {
					// Suponemos que el fichero ha sido eliminado por otro hilo
					LOGGER.warning(
							"No se ha podido eliminar el fichero '" + file.getAbsolutePath() //$NON-NLS-1$s
							+ "', es probable que se elimine en otro hilo de ejecucion: " + e //$NON-NLS-1$
							);
				}
			}
		}

	}

	/**
	 * Comprueba si el archivo ha expirado en cach&eacute;
	 * @param file Archivo a comprobar
	 * @param expirationTimeLimit tiempo de exipraci&oacute;n
	 * @return devuelve true si ha expirado y false en caso contrario
	 */
	private static boolean isExpired(final File file, final long expirationTimeLimit) {
		return System.currentTimeMillis() - file.lastModified() > expirationTimeLimit;
	}

	@Override
	public byte[] getDocumentFromCache(final String idCacheFile) throws IOException {

		LOGGER.info("Recuperamos el documento con identificador: " + idCacheFile); //$NON-NLS-1$

		byte[] data = null;

		final File inFile = new File(tmpDir, idCacheFile);

		//Recuperamos el archivo del directorio de cache
		if (!inFile.isFile() || !inFile.canRead() || isExpired(inFile, this.expTime)) {
			return data;
		}

		try (final InputStream fis = new FileInputStream(inFile)) {
			data = AOUtil.getDataFromInputStream(fis);
		}
		catch (final IOException e) {
			LOGGER.warning("Error en la lectura del fichero '" + inFile.getAbsolutePath() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			throw e;
		}

		return data;
	}

	@Override
	public String storeDocumentToCache(final byte[] data) throws IOException {

		String newId = ""; //$NON-NLS-1$
		File file = null;
		boolean fileExist = true;

		// Comprobamos que el archivo a guardar no estara duplicado
		while (fileExist) {

			newId = generateNewId() + ".tmp"; //$NON-NLS-1$
			file = new File(tmpDir, newId);

			if (!file.exists()) {
				fileExist = false;
			}
		}

		// Guardamos el archivo en el directorio indicado en el archivo de configuracion
		try (final OutputStream os = new FileOutputStream(file)) {

			os.write(data);

		} catch (final IOException e) {
			LOGGER.warning("Error en la escritura del fichero '" + tmpDir + newId + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			throw e;
		}

		// Si el numero de usos iguala o supera a la variable, se limpian los archivos expirados
		synchronized (this) {
			if (++uses >= this.maxUseToCleaning) {
				cleanCache();
				uses = 0;
			}
		}

		return newId;
	}

	/**
	 * Genera el nombre para el archivo a guardar en cach&eacute;
	 * @return Nombre aleatorio para el archivo a guardar
	 */
	private static String generateNewId() {
		char c;
		final SecureRandom rnd = new SecureRandom();
		String newId = ""; //$NON-NLS-1$
		for (int i=0; i < 10 ; i++) {
			c = (char)(rnd.nextDouble() * 26.0 + 65.0 );
			newId += c;
		}
		return newId;
	}


	/**
	 * Hilo para la eliminaci&oacute;n de documentos temporales caducados.
	 */
	private static final class ExpiredDocumentsCleanerThread extends Thread {

		private final long timeout;

		/**
		 * Construye el hilo para la eliminaci&oacute;n de documentos temporales caducados.
		 * @param tempTimeout Tiempo de caducidad en milisegundos de los ficheros temporales.
		 */
		public ExpiredDocumentsCleanerThread (final long tempTimeout) {
			this.timeout = tempTimeout;
		}

		@Override
		public void run() {
			deleteExpiredDocuments(this.timeout);
		}
	}
}
