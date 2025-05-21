/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.triphase.server.SignatureService;

/**
 * Gestiona la configuraci&oacute;n espec&iacute;fica del proceso de firma de lotes.
 */
public class BatchConfigManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String CONFIG_FILE = "signbatch_config.properties"; //$NON-NLS-1$

	private static final String SYS_PROP_PREFIX = "${"; //$NON-NLS-1$

	private static final String SYS_PROP_SUFIX = "}"; //$NON-NLS-1$

	/** Variable de entorno que determina el directorio en el que buscar el fichero de configuraci&oacute;n. */
	private static final String ENVIRONMENT_VAR_CONFIG_DIR = "clienteafirma.config.path"; //$NON-NLS-1$

	private static final int MAX_CONCURRENT_SIGNS = 10;

	private static long CONCURRENT_TIMEOUT = 30;

	private static final String SYS_PROP_DISABLE_SSL = "disableSslChecks"; //$NON-NLS-1$

	/** Propiedad que indica el tama&ntilde;o m&aacute;ximo permitido para un documento */
	private static final String CONFIG_PARAM_MAX_DOC_SIZE = "batch.maxDocSize"; //$NON-NLS-1$

	private static final Properties CONFIG = new Properties();

	private static Boolean CONCURRENT_MODE = null;

	private static Integer CONCURRENT_SIGNS = null;

	private static String[] allowedSources = null;

	private static File tempDir = null;

	private static boolean initialized = false;

	static {

		String configDir;
		try {
			configDir = System.getProperty(ENVIRONMENT_VAR_CONFIG_DIR);
		}
		catch (final Exception e) {
			LOGGER.warning(
				"No se ha podido obtener el directorio del fichero de configuracion: " + e //$NON-NLS-1$
			);
			configDir = null;
		}

		// Cargamos la configuracion del servicio
		final Properties configProperties = loadConfigFile(configDir, CONFIG_FILE);

		// Si se carga el fichero, se establece la configuracion de los servicios
		if (configProperties != null) {
			for (final String k : configProperties.keySet().toArray(new String[0])) {
				CONFIG.setProperty(k, mapSystemProperties(configProperties.getProperty(k)));
			}

			// Establecemos si se deben comprobar los certificados SSL de las conexiones remotas
			final boolean checkSslCerts = Boolean.parseBoolean(
					CONFIG.getProperty("checksslcerts", "true")); //$NON-NLS-1$ //$NON-NLS-2$
			System.setProperty(SYS_PROP_DISABLE_SSL, Boolean.toString(!checkSslCerts));
			initialized = true;
		}
		// Si no, identificamos los servivios de firma de lotes XML como no inicializamos
		else {
			LOGGER.warning("No se ha encontrado el fichero de configuracion del servicio de lotes XML y no se inicializara"); //$NON-NLS-1$
			initialized = false;
		}
	}

	/**
	 * Indica si los servicios de firma de lotes XML est&aacute;n inicializados o no.
	 * @return {@code true} si est&aacute; inicializado, {@code false} en caso contrario.
	 */
	public static boolean isInitialized() {
		return initialized;
	}

	/**
	 * Indica si esta habilitado el modo de ejecuci&oacute;n concurrente.
	 * @return {@code true} si las firmas se ejecutar&aacute;n de forma concurrente,
	 * {@code false} en caso contrario.
	 */
	public static boolean isConcurrentMode() {
		if (CONCURRENT_MODE == null) {
			CONCURRENT_MODE = Boolean.valueOf(CONFIG.getProperty("concurrentmode")); //$NON-NLS-1$
		}
		return CONCURRENT_MODE.booleanValue();
	}

	/**
	 * Devuelve el n&uacute;mero de segundos que deber&aacute;
	 * durar como m&aacute;ximo cada fase de una operaci&oacute;n de firma.
	 * @return tiempo m&aacute;ximo de cada fase
	 */
	public static long getConcurrentTimeout() {
		if(CONFIG.getProperty("concurrenttimeout") != null) { //$NON-NLS-1$
			CONCURRENT_TIMEOUT = Long.parseLong(CONFIG.getProperty("concurrenttimeout")); //$NON-NLS-1$
		}
		return CONCURRENT_TIMEOUT;
	}

	/**
	 * Obtiene el n&uacute;mero m&aacute;ximo de firmas que se pueden procesar de forma
	 * concurrente.
	 * @return N&uacute;mero m&aacute;ximo de firmas que se pueden procesar de forma
	 * concurrente.
	 */
	public static int getMaxCurrentSigns() {
		if (CONCURRENT_SIGNS == null) {
			int n = 0;
			try {
				n = Integer.parseInt(CONFIG.getProperty("maxcurrentsigns", Integer.toString(MAX_CONCURRENT_SIGNS))); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.warning(
					"El valor configurado como numero maximo de firmas concurrentes no es valido (" + //$NON-NLS-1$
						CONFIG.getProperty("maxcurrentsigns") + "), se usara el valor por defecto (" + MAX_CONCURRENT_SIGNS + "): " + e //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				);
				n = MAX_CONCURRENT_SIGNS;
			}
			CONCURRENT_SIGNS = Integer.valueOf(n > 0 && n <= MAX_CONCURRENT_SIGNS ? n : MAX_CONCURRENT_SIGNS);
		}
		return CONCURRENT_SIGNS.intValue();
	}

	/**
	 * Devuelve el tama&ntilde;o m&aacute;ximo en bytes que puede tener un documento del lote.
	 * @return Tama&ntilde;o m&aacute;ximo.
	 */
	public static long getBatchXmlDocSize() {
		try {
			return Long.parseLong(CONFIG.getProperty(CONFIG_PARAM_MAX_DOC_SIZE));
		} catch (final Exception e) {
			return 0;
		}
	}


	/**
	 * Obtiene el listado de fuentes permitidas como origen de los datos. El formato
	 * de las fuentes puede ser una cadena de texto con una URL (admite '*' como
	 * comod&iacute;n) o la cadena "base64".
	 * @return Listado de fuentes.
	 * @throws IllegalStateException Cuando no se ha indicado ninguna fuente.
	 */
	public static String[] getAllowedSources() {
		if (allowedSources == null) {
			final String sources = CONFIG.getProperty("allowedsources"); //$NON-NLS-1$
			if (sources == null || sources.isEmpty()) {
				throw new IllegalStateException(
					"No se ha definido ningun permiso para la carga de datos" //$NON-NLS-1$
				);
			}
			allowedSources = sources.split(";"); //$NON-NLS-1$
		}
		return allowedSources;
	}

	/**
	 * Obtiene el directorio para el guardado de ficheros temporales.
	 * @return Directorio temporal configurado o el por defecto si no se configur&oacute;
	 * ninguno o no era v&aacute;lido.
	 */
	public static File getTempDir() {
		if (tempDir == null) {
			final String defaultDir = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$
			final File f = new File(CONFIG.getProperty("tmpdir", defaultDir)); //$NON-NLS-1$

			if (f.isDirectory() && f.canRead() && f.canWrite()) {
				return f;
			}
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
					"El directorio temporal configurado (" + f.getAbsolutePath() + ") no es valido, se usaran el por defecto: " + defaultDir //$NON-NLS-1$ //$NON-NLS-2$
					);
			tempDir = new File(defaultDir);
		}
		return tempDir;
	}

	/** Intenta cargar un fichero propiedades del directorio proporcionado o, en caso de
	 * no encontrarlo ah&iacute;, se busca en el <i>classpath</i>.
	 * @param configDir Directorio del fichero de configuraci&oacute;n.
	 * @param configFilename Nombre del fichero de propedades.
	 * @return Propiedades cargadas o {@code null} si no se pudo cargar el fichero. */
	private static Properties loadConfigFile(final String configDir, final String configFilename) {

		LOGGER.info("Se cargara el fichero de configuracion " + configFilename); //$NON-NLS-1$

		Properties configProperties = null;

		if (configDir != null) {
			try {
				final File configFile = new File(configDir, configFilename).getCanonicalFile();
				try (final InputStream configIs = new FileInputStream(configFile);) {
					configProperties = new Properties();
					configProperties.load(configIs);
				}
			}
			catch (final Exception e) {
				LOGGER.warning(
						"No se pudo cargar el fichero de configuracion " + configFilename + //$NON-NLS-1$
						" desde el directorio " + configDir + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				configProperties = null;
			}
		}

		if (configProperties == null) {
			LOGGER.info(
				"Se cargara el fichero de configuracion " + configFilename + " desde el CLASSPATH" //$NON-NLS-1$ //$NON-NLS-2$
			);

			try (final InputStream configIs = SignatureService.class.getClassLoader().getResourceAsStream(configFilename);) {
				configProperties = new Properties();
				configProperties.load(configIs);
			}
			catch (final Exception e) {
				LOGGER.warning(
					"No se pudo cargar el fichero de configuracion " + configFilename + " desde el CLASSPATH: " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
				configProperties = null;
			}
		}

		return configProperties;
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
