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
import java.nio.file.Files;
import java.security.SecureRandom;
import java.util.Base64;
import java.util.Base64.Encoder;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.triphase.server.ConfigManager;
import es.gob.afirma.triphase.server.FileSystemUtils;

/**
 * Implementaci&oacute;n de cache&eacute; en disco.
 */
public final class FileSystemCacheManager implements DocumentCacheManager {

	private static final String PROP_TMP_DIR = "cache.tmpDir"; //$NON-NLS-1$
	private static final String PROP_EXP_TIME = "cache.expTime"; //$NON-NLS-1$
	private static final String PROP_MAX_USE_TO_CLEANING = "cache.maxUseToCleaning"; //$NON-NLS-1$

	private static final long DEFAULT_EXP_TIME = 60000;
	private static final int DEFAULT_MAX_USE_TO_CLEAN = 100;

	private static final int ID_MAX_SIZE = 60;

    private static final SecureRandom SECURE_RANDOM = new SecureRandom();
    private static final Encoder BASE64_ENCODER = Base64.getUrlEncoder().withoutPadding();


	final static Logger LOGGER = Logger.getLogger(ConfigManager.LOGGER_NAME);

	private static int uses = 0;

	private File cacheDir;
	private long expTime;
	private int maxUseToCleaning;

	/** Construye la clase de acceso a gestor documental usando sistema de ficheros.
	 * @param config Configuraci&oacute;n del gestor (directorios, etc.) */
	public FileSystemCacheManager(final Properties config) {

		// Establecemos el directorio configurado o el por defecto si no existe
		final String tmpDirProp = config.getProperty(PROP_TMP_DIR, ""); //$NON-NLS-1$
		if (!tmpDirProp.isEmpty() && new File(tmpDirProp).isDirectory()) {
			this.cacheDir = new File(tmpDirProp);
		}
		else {
			this.cacheDir = new File(System.getProperty("java.io.tmpdir"), "triphaseSignTemp"); //$NON-NLS-1$ //$NON-NLS-2$
			if (!this.cacheDir.exists()) {
				this.cacheDir.mkdirs();
			}
		}

		try {
			this.cacheDir = this.cacheDir.getCanonicalFile();
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo canonicalizar la ruta del directorio de cache"); //$NON-NLS-1$
		}

		LOGGER.info("Los ficheros cacheados se almacenaran en: " //$NON-NLS-1$
				+ LoggerUtil.getTrimStr(this.cacheDir.getAbsolutePath()));

		// Establecemos el tiempo de caducidad
		try {
			this.expTime = Long.parseLong(config.getProperty(PROP_EXP_TIME));
		}
		catch (final Exception e) {
			LOGGER.warning("Error leyendo la propiedad " + PROP_EXP_TIME  //$NON-NLS-1$
					+ ", se usara el valor por defecto: " + DEFAULT_EXP_TIME);  //$NON-NLS-1$
			this.expTime = DEFAULT_EXP_TIME;
		}

		// Establecemos el numero de accesos que permitimos entre accesos a la cache
		try {
			this.maxUseToCleaning = Integer.parseInt(config.getProperty(PROP_MAX_USE_TO_CLEANING));
		}
		catch (final Exception e) {
			LOGGER.warning("Error leyendo la propiedad " + PROP_MAX_USE_TO_CLEANING //$NON-NLS-1$
					+ ", se usara el valor por defecto: " + DEFAULT_MAX_USE_TO_CLEAN);  //$NON-NLS-1$
			this.maxUseToCleaning = DEFAULT_MAX_USE_TO_CLEAN;
		}
	}


	@Override
	public void cleanCache() {
		new ExpiredDocumentsCleanerThread(this.expTime, this.cacheDir).start();
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
	public byte[] getDocumentFromCache(final String idCacheFile) throws IOException {

		// Excluimos recuperar los datos si el ID es anormalmente grande
		if (idCacheFile.length() > ID_MAX_SIZE) {
			LOGGER.log(Level.WARNING, "Se proporciono un ID de cache demasiado grande y se ignorara"); //$NON-NLS-1$
			return null;
		}

		LOGGER.fine("Recuperamos de la cache el documento con identificador: " + idCacheFile); //$NON-NLS-1$

		byte[] data = null;

		File inFile;
		try {
			inFile = FileSystemUtils.composeTargetFile(this.cacheDir, idCacheFile);
		}
		catch (final SecurityException e) {
			LOGGER.log(Level.WARNING, "Se intento leer un fichero de fuera de la cache", e); //$NON-NLS-1$
			return null;

		}
		catch (final Exception e) {
			throw new IOException("No se pudo componente la ruta del fichero", e); //$NON-NLS-1$
		}

		// Comprobaciones de seguridad
		if (!inFile.isFile() || !inFile.canRead() || isExpired(inFile, this.expTime) || Files.isSymbolicLink(inFile.toPath())) {
			return null;
		}

		// Recuperamos el archivo del directorio de cache
		try (final InputStream fis = new FileInputStream(inFile)) {
			data = AOUtil.getDataFromInputStream(fis);
		}
		catch (final IOException e) {
			throw new IOException("Error al leer de cache el fichero: " + LoggerUtil.getTrimStr(inFile.getName()), e); //$NON-NLS-1$
		}

		// El fichero se elimina de cache tras su uso
		try {
			if (!inFile.delete()) {
				LOGGER.warning(String.format("El fichero %1s no se elimino la de cache", LoggerUtil.getTrimStr(inFile.getName()))); //$NON-NLS-1$
			}
		}
		catch (final Exception e) {
			LOGGER.warning(String.format("El fichero %1s no se pudido eliminar la de cache: %2s", LoggerUtil.getTrimStr(inFile.getName()), e)); //$NON-NLS-1$
		}

		return data;
	}

	@Override
	public String storeDocumentToCache(final byte[] data) throws IOException {

		// Obtenemos un nombre de temporal que no exista ya
		String newId;
		File file;
		do {
			newId = generateNewId();
			file = new File(this.cacheDir, newId);
		} while (file.exists());

		// Guardamos el archivo en el directorio indicado en el archivo de configuracion
		try (final OutputStream os = new FileOutputStream(file)) {
			os.write(data);
		} catch (final IOException e) {
			throw new IOException("Error al escribir en cache el fichero: " + file.getAbsolutePath(), e); //$NON-NLS-1$
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
		final byte[] data = new byte[24];
		SECURE_RANDOM.nextBytes(data);
		return BASE64_ENCODER.encodeToString(data);
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
		 * @param tempDir Directorio en el que se encuentran los temporales.
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
