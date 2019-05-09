/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signfolder.server.proxy;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import java.util.logging.Level;
import java.util.logging.Logger;

/** Configuraci&oacute;n para la gesti&oacute;n del almacenamiento temporal de ficheros en servidor. */
final class RetrieveConfig {

	/** <i>Log</i> para registrar las acciones del servicio. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	/** Clave para la configuraci&oacute;n del directorio para la creacion de ficheros temporales. */
	private static final String TMP_DIR_KEY = "tmpDir"; //$NON-NLS-1$

	private static final String DEBUG_KEY = "debug"; //$NON-NLS-1$

	/** Directorio temporal por defecto. */
	private static String defaultTmpDir;

	/** Milisegundos que, por defecto, tardan los mensajes en caducar. */
	private static final long DEFAULT_EXPIRATION_TIME = 60000; // 1 minuto

	/** Directorio temporal a usar. */
	private static final File TMP_DIR;

	/** Modo de depuraci&oacute;n activo o no, en el que no se borran los ficheros en servidor ni se dan por caducados. */
	static final boolean DEBUG;

	/** Fichero de configuraci&oacute;n. */
	private static final String DEFAULT_CFG_FILE = "configuration.properties"; //$NON-NLS-1$

	/** Clave para la configuraci&oacute;n del tiempo de caducidad de los ficheros temporales. */
	private static final String EXPIRATION_TIME_KEY =  "expTime"; //$NON-NLS-1$

	private static final long EXPIRATION_TIME;

	private static final String ENVIRONMENT_VAR_CONFIG_DIR = "AFIRMA_CONFIG_DIR"; //$NON-NLS-1$

	static {

		InputStream is = null;
		final Properties config = new Properties();
		try {
			final String configDir = System.getProperty(ENVIRONMENT_VAR_CONFIG_DIR);

			if (configDir != null) {
				final File configFile = new File(configDir, DEFAULT_CFG_FILE).getCanonicalFile();
				if (!configFile.isFile() || !configFile.canRead()) {
					LOGGER.warning(
						"No se encontro el fichero " + DEFAULT_CFG_FILE + " en el directorio configurado en la variable " + //$NON-NLS-1$ //$NON-NLS-2$
							ENVIRONMENT_VAR_CONFIG_DIR + " (" + configFile.getAbsolutePath() + //$NON-NLS-1$
								"), se buscara en el CLASSPATH."); //$NON-NLS-1$
				}
				else {
					LOGGER.info("Se carga un fichero de configuracion externo: " + configFile.getAbsolutePath()); //$NON-NLS-1$
					is = new FileInputStream(configFile);
				}
			}

			if (is == null) {
				LOGGER.info("Se carga el fichero de configuracion del classpath"); //$NON-NLS-1$
				is = RetrieveConfig.class.getClassLoader().getResourceAsStream(DEFAULT_CFG_FILE);
			}

			config.load(is);
			is.close();
		}
		catch (final IOException e) {
			if (is != null) {
				try {
					is.close();
				}
				catch (final Exception ex) {
					LOGGER.warning("Error cerrando el flujo de lectura de configuracion: " + ex); //$NON-NLS-1$
				}
			}
			LOGGER.severe(
				"No se ha podido cargar el fichero con las propiedades (" + DEFAULT_CFG_FILE + "), se usaran los valores por defecto: " + e.toString() //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		DEBUG = Boolean.parseBoolean(config.getProperty(DEBUG_KEY));
		if (DEBUG) {
			LOGGER.warning("Modo de depuracion activado, no se borraran los ficheros en servidor"); //$NON-NLS-1$
		}

		try {
			defaultTmpDir = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.warning(
				"El directorio temporal no ha podido determinarse por la variable de entorno 'java.io.tmpdir': " + e //$NON-NLS-1$
			);
			try {
				defaultTmpDir = File.createTempFile("tmp", null).getParentFile().getAbsolutePath(); //$NON-NLS-1$
			}
			catch (final Exception e1) {
				defaultTmpDir = null;
				LOGGER.log(
					Level.WARNING,
					"No se ha podido cargar un directorio temporal por defecto, se debera configurar expresamente en el fichero de propiedades: " + e1, //$NON-NLS-1$
					e1
				);
			}
		}

		File tmpDir = new File(config.getProperty(TMP_DIR_KEY, defaultTmpDir).trim());
		if (!tmpDir.isDirectory() || !tmpDir.canRead()) {
			LOGGER.warning(
				"El directorio temporal indicado en el fichero de propiedades (" + config.getProperty(TMP_DIR_KEY, defaultTmpDir) + ") no existe, se usara el por defecto: " +  defaultTmpDir //$NON-NLS-1$ //$NON-NLS-2$
			);
			tmpDir = new File(defaultTmpDir);
			if (!tmpDir.isDirectory() ||!tmpDir.canRead()) {
				throw new IllegalStateException("No se ha podido definir un directorio temporal"); //$NON-NLS-1$
			}
		}
		TMP_DIR = tmpDir;

		long expTime;
		try {
			expTime = config.containsKey(EXPIRATION_TIME_KEY) ?
				Long.parseLong(config.getProperty(EXPIRATION_TIME_KEY)) :
					DEFAULT_EXPIRATION_TIME;
		}
		catch (final Exception e) {
			LOGGER.warning(
				"Tiempo de expiracion invalido en el fichero de configuracion (" + DEFAULT_CFG_FILE + "), se usara " + DEFAULT_EXPIRATION_TIME + ": " + e //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			);
			expTime = DEFAULT_EXPIRATION_TIME;
		}
		EXPIRATION_TIME = expTime;
	}

	/** Recupera el directorio configurado para la creaci&oacute;n de ficheros temporales o el por defecto.
	 * @return Directorio temporal.
	 * @throws NullPointerException Cuando no se indica la ruta del directorio temporal ni se puede obtener
	 *                              del sistema. */
	static File getTempDir() {
		return TMP_DIR;
	}

	/** Recupera el tiempo en milisegundos que puede almacenarse un fichero antes de considerarse caducado.
	 * @return Tiempo m&aacute;ximo en milisegundos que puede tardarse en recoger un fichero antes de que
	 *         caduque. */
	static long getExpirationTime() {
		return EXPIRATION_TIME;
	}
}
