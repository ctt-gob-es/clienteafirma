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

/**
 * Implementaci&oacute;n de cache&eacute; en disco.
 */
public final class FileSystemCacheManager implements DocumentCacheManager {

	private static final String TMP_DIR = "tmpDir"; //$NON-NLS-1$
	private static final String EXP_TIME = "expTime"; //$NON-NLS-1$
	private static final String MAX_USE_TO_CLEANING = "maxUseToCleaning"; //$NON-NLS-1$

	File tmpDir;
	final File defaultTmpDir = new File(System.getProperty("java.io.tmpdir") //$NON-NLS-1$
			+ File.separator + "triphaseSignTemp");  //$NON-NLS-1$

	long expTime;
	final long defaultExpTime = 60000;
	int maxUseToCleaning;
	final int defaultMaxUseToClean = 100;

	private static int uses = 0;

	final static Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Construye la clase de acceso a gestor documental usando sistema de ficheros.
	 * @param config Configuraci&oacute;n del gestor (directorios, etc.) */
	public FileSystemCacheManager(final Properties config) {

		final String tmpDirProp = config.getProperty(TMP_DIR);
		File dirToUse = this.defaultTmpDir;

		//En caso de que el directorio configurado no exista, se crea una carpeta en el sistema
		if (tmpDirProp != null && !tmpDirProp.isEmpty()) {
			final File directory = new File(tmpDirProp);

			if (directory.isDirectory()) {
				dirToUse = directory;
			}
		}

		this.tmpDir = dirToUse;

		//Comprobamos que la propiedad expTime venga correctamente informada
		final String expTimeProp = config.getProperty(EXP_TIME);
		long expTimeToUse = this.defaultExpTime;

		if (expTimeProp != null && !expTimeProp.isEmpty()) {
			try {
				expTimeToUse = Long.parseLong(expTimeProp);
			} catch (final NumberFormatException nfe) {
				LOGGER.warning("Error leyendo la propiedad expTime, se usara el valor por defecto: "  //$NON-NLS-1$
						+ this.defaultExpTime);
			}
		}

		this.expTime = expTimeToUse;

		//Comprobamos que la propiedad maxUseToCleaning venga correctamente informada
		final String maxUseToCleanProp = config.getProperty(MAX_USE_TO_CLEANING);
		int maxCleanToUse = this.defaultMaxUseToClean;

		if (maxUseToCleanProp != null && !maxUseToCleanProp.isEmpty()) {
			try {
				maxCleanToUse = Integer.parseInt(maxUseToCleanProp);
			} catch (final NumberFormatException nfe) {
				LOGGER.warning("Error leyendo la propiedad maxUseToCleaning, se usara el valor por defecto: "  //$NON-NLS-1$
						+ this.defaultMaxUseToClean);
			}
		}

		this.maxUseToCleaning = maxCleanToUse;
	}


	@Override
	public void cleanCache() {
		new ExpiredDocumentsCleanerThread(this.expTime, this.tmpDir).start();
	}

	/**
	 * Comprueba si el archivo ha expirado en cach&eacute;
	 * @param file Archivo a comprobar
	 * @param expirationTimeLimit tiempo de exipraci&oacute;n
	 * @return devuelve true si ha expirado y false en caso contrario
	 */
	static boolean isExpired(final File file, final long expirationTimeLimit) {
		return System.currentTimeMillis() - file.lastModified() > expirationTimeLimit;
	}

	@Override
	public byte[] getDocumentFromCache(final String idCacheFile) {

		LOGGER.info("Recuperamos el documento con identificador: " + idCacheFile); //$NON-NLS-1$

		byte[] data = null;

		final File inFile = new File(this.tmpDir, idCacheFile);

		//Recuperamos el archivo del directorio de cache
		if (!inFile.isFile() || !inFile.canRead() || isExpired(inFile, this.expTime)) {
			return data;
		}

		try (final InputStream fis = new FileInputStream(inFile)) {
			data = AOUtil.getDataFromInputStream(fis);
		}
		catch (final IOException e) {
			LOGGER.warning("Error en la lectura del fichero '" + inFile.getAbsolutePath() + "': " + e);  //$NON-NLS-1$//$NON-NLS-2$
		}

		return data;
	}

	@Override
	public String storeDocumentToCache(final byte[] data){

		String newId = null;
		File file = null;
		boolean fileExist = true;

		// Comprobamos que el archivo a guardar no estara duplicado
		while (fileExist) {

			newId = generateNewId() + ".tmp"; //$NON-NLS-1$
			file = new File(this.tmpDir, newId);

			if (!file.exists()) {
				fileExist = false;
			}
		}

		// Guardamos el archivo en el directorio indicado en el archivo de configuracion
		try (final OutputStream os = new FileOutputStream(file)) {

			os.write(data);

		} catch (final IOException e) {
			LOGGER.warning("Error en la escritura del fichero '" + this.tmpDir + newId + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return newId;
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
		private final File temporaryDir;

		/**
		 * Construye el hilo para la eliminaci&oacute;n de documentos temporales caducados.
		 * @param tempTimeout Tiempo de caducidad en milisegundos de los ficheros temporales.
		 */
		public ExpiredDocumentsCleanerThread (final long tempTimeout, final File tempDir) {
			this.timeout = tempTimeout;
			this.temporaryDir = tempDir;
		}

		@Override
		public void run() {
			deleteExpiredDocuments(this.timeout);
		}

		/**
		 * M&eacute;todo que borra todos los archivos que hayan expirado en cach&eacute;
		 * @param expirationTime tiempo de expiraci&oacute;n a comprobar para el archivo
		 */
		public void deleteExpiredDocuments(final long expirationTime) {

			if (this.temporaryDir != null) {
				for (final File file : this.temporaryDir.listFiles()) {
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
	}
}
