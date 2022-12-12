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
import java.util.logging.Logger;

/** Configuraci&oacute;n para la gesti&oacute;n del almacenamiento temporal de ficheros en servidor. */
final class RetrieveConfig {

	/** <i>Log</i> para registrar las acciones del servicio. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	/** Variable de entorno que designa el directorio en el que se encuentra el
	 * fichero de configuraci&oacute;n. */
	private static final String ENVIRONMENT_VAR_CONFIG_DIR = "clienteafirma.config.path"; //$NON-NLS-1$

	/** Fichero de configuraci&oacute;n. */
	private static final String CONFIG_FILE = "intermediate_config.properties"; //$NON-NLS-1$

	/** Clave para la configuraci&oacute;n del directorio para la creacion de ficheros temporales. */
	private static final String TMP_DIR_KEY = "tmpDir"; //$NON-NLS-1$

	private static final String DEBUG_KEY = "debug"; //$NON-NLS-1$

	/** Clave para la configuraci&oacute;n del tiempo de caducidad de los ficheros temporales. */
	private static final String EXPIRATION_TIME_KEY =  "expTime"; //$NON-NLS-1$

	private static final String SYS_PROP_PREFIX = "${"; //$NON-NLS-1$

	private static final String SYS_PROP_SUFIX = "}"; //$NON-NLS-1$

	/** Milisegundos que, por defecto, tardan los mensajes en caducar. */
	private static final long DEFAULT_EXPIRATION_TIME = 60000; // 1 minuto

	/** Directorio temporal a usar. */
	private static final File TMP_DIR;

	/** Modo de depuraci&oacute;n activo o no, en el que no se borran los ficheros en servidor ni se dan por caducados. */
	static final boolean DEBUG;

	private static final long EXPIRATION_TIME;

	static {

		InputStream is = null;
		final Properties config = new Properties();
		try {
			final String configDir = System.getProperty(ENVIRONMENT_VAR_CONFIG_DIR);
			if (configDir != null) {
				final File configFile = new File(configDir, CONFIG_FILE).getCanonicalFile();
				if (!configFile.isFile() || !configFile.canRead()) {
					LOGGER.warning(
						"No se encontro o no se pudo leer el fichero " + CONFIG_FILE + //$NON-NLS-1$
						" en el directorio configurado en la variable " + ENVIRONMENT_VAR_CONFIG_DIR + //$NON-NLS-1$
						" (" + configFile.getAbsolutePath() + "), se buscara en el CLASSPATH."); //$NON-NLS-1$ //$NON-NLS-2$
				}
				else {
					LOGGER.info("Se carga un fichero de configuracion externo: " + configFile.getAbsolutePath()); //$NON-NLS-1$
					is = new FileInputStream(configFile);
				}
			}

			if (is == null) {
				LOGGER.info("Se carga el fichero de configuracion del classpath"); //$NON-NLS-1$
				is = RetrieveConfig.class.getClassLoader().getResourceAsStream(CONFIG_FILE);
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
				"No se ha podido cargar el fichero con las propiedades (" + CONFIG_FILE + "), se usaran los valores por defecto: " + e.toString() //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		DEBUG = Boolean.parseBoolean(getProperty(config, DEBUG_KEY, null));
		if (DEBUG) {
			LOGGER.warning("Modo de depuracion activado, no se borraran los ficheros en servidor"); //$NON-NLS-1$
		}

		File tmpDir;
		final String tmpDirName = config.getProperty(TMP_DIR_KEY);
		if (tmpDirName != null && !tmpDirName.isEmpty()) {
			tmpDir = new File(tmpDirName);
			if (!tmpDir.isDirectory()) {
				throw new IllegalStateException(
					"El directorio temporal indicado en el fichero de propiedades no existe: " + config.getProperty(TMP_DIR_KEY) //$NON-NLS-1$
				);
			}
			if (!tmpDir.canRead() || !tmpDir.canWrite()) {
				throw new IllegalStateException(
					"No se tienen permisos sobre el directorio temporal: " + config.getProperty(TMP_DIR_KEY) //$NON-NLS-1$
				);
			}
		}
		else {
			LOGGER.info("No se ha indicado un direcctorio temporal, se usara el por defecto del sistema"); //$NON-NLS-1$
			try {
				tmpDir = new File(System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.warning(
					"El directorio temporal no ha podido determinarse por la variable de entorno 'java.io.tmpdir': " + e //$NON-NLS-1$
				);
				try {
					final File tmpFile = File.createTempFile("tmp", null); //$NON-NLS-1$
					tmpDir = new File(tmpFile.getParentFile().getAbsolutePath());
					tmpFile.deleteOnExit();
				}
				catch (final Exception e1) {
					throw new IllegalStateException(
						"No se ha podido determinar el directorio temporal por defecto", e1 //$NON-NLS-1$
					);
				}
			}
		}
		TMP_DIR = tmpDir;

		long expTime;
		final String expTimeValue = getProperty(config, EXPIRATION_TIME_KEY, null);
		try {
			expTime = expTimeValue != null ?
				Long.parseLong(expTimeValue) :
					DEFAULT_EXPIRATION_TIME;
		}
		catch (final Exception e) {
			LOGGER.warning(
				"Tiempo de expiracion invalido en el fichero de configuracion (" + expTimeValue + "), se usara " + DEFAULT_EXPIRATION_TIME + ": " + e //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
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

	/**
	 * Carga una propiedad de la configuraci&oacute;n traduciendo su contenido por lo indicado
	 * mediante variables de entorno si es necesario.
	 * @param config Configuraci&oacute;n establecida.
	 * @param key Propiedad a recuperar.
	 * @param defaultValue Valor a devolver si la propiedad no est&aacute; asignada. Tambi&eacute;n
	 * se traducir&aacute;n las variables de entorno.
	 * @return Valor de la propiedad o valor por defecto.
	 */
	private static String getProperty(final Properties config, final String key, final String defaultValue) {

		String value = config.getProperty(key, defaultValue);
		if (value != null) {
			value = mapSystemProperties(value);
		}
		return value;
	}

	/**
	 * Mapea las propiedades del sistema que haya en el texto que se referencien de
	 * la forma: ${propiedad}
	 * @param text Texto en el que se pueden encontrar las referencias a las propiedades
	 * del sistema.
	 * @return Cadena con las particulas traducidas a los valores indicados como propiedades
	 * del sistema. Si no se encuentra la propiedad definida, no se modificar&aacute;
	 */
	private static String mapSystemProperties(final String text) {

		if (text == null) {
			return null;
		}

		int pos = -1;
		int pos2 = 0;
		String mappedText = text;
		while ((pos = mappedText.indexOf(SYS_PROP_PREFIX, pos + 1)) > -1 && pos2 > -1) {
			pos2 = mappedText.indexOf(SYS_PROP_SUFIX, pos + SYS_PROP_PREFIX.length());
			if (pos2 > pos) {
				final String prop = mappedText.substring(pos + SYS_PROP_PREFIX.length(), pos2);
				final String value = System.getProperty(prop, null);
				if (value != null) {
					mappedText = mappedText.replace(SYS_PROP_PREFIX + prop + SYS_PROP_SUFIX, value);
				}
			}
		}
		return mappedText;
	}
}
