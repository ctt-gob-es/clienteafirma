/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.server.webstart;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;

/**
 * Gestor de las propiedades del fichero de configuraci&oacute;n del m&oacute;dulo.
 */
class ConfigManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String ENVIRONMENT_VAR_CONFIG_DIR = "clienteafirma.config.path"; //$NON-NLS-1$

	private static final String CONFIG_FILE = "autofirma-jnlp.properties"; //$NON-NLS-1$

	private static final String PROPERTY_CODEBASE = "codebase"; //$NON-NLS-1$

	private static final Properties config;

	static {
		try {
			String configDir;
			try {
				configDir = System.getProperty(ENVIRONMENT_VAR_CONFIG_DIR);
			}
			catch (final Exception e) {
				LOGGER.warning(
						"No se pudo acceder a la variable de entorno '" + ENVIRONMENT_VAR_CONFIG_DIR + //$NON-NLS-1$
						"' que configura el directorio del fichero de configuracion: " + e);//$NON-NLS-1$
				configDir = null;
			}

			if (configDir != null) {
				LOGGER.info("Se cargara el fichero de configuracion desde el directorio: " + configDir); //$NON-NLS-1$
			}
			else {
				LOGGER.info("No se ha configurado el directorio del fichero de configuracion. Se cargara el fichero interno"); //$NON-NLS-1$
			}

			config = new Properties();
			if (configDir != null) {
				final File configFile = new File(configDir, CONFIG_FILE).getCanonicalFile();
				if (!configFile.isFile() || !configFile.canRead()) {
					LOGGER.warning(
							"No se encontro el fichero " + CONFIG_FILE + " en el directorio configurado en la variable " + //$NON-NLS-1$ //$NON-NLS-2$
									ENVIRONMENT_VAR_CONFIG_DIR + ": " + configFile.getAbsolutePath().replace(System.getProperty("user.home"), "USERHOME") + //$NON-NLS-1$
									"\nSe buscara en el CLASSPATH."); //$NON-NLS-1$
				}
				else {
					final InputStream configIs = new FileInputStream(configFile);
					config.load(configIs);
					configIs.close();
				}
			}

			if (config.isEmpty()) {
				final InputStream configIs = ConfigManager.class.getClassLoader().getResourceAsStream(CONFIG_FILE);
				config.load(configIs);
				configIs.close();
			}

			if (config.isEmpty()) {
				throw new IOException("No se encuentra el fichero de configuracion del servicio: " + CONFIG_FILE); //$NON-NLS-1$
			}
		}
		catch(final Exception e) {
			throw new RuntimeException("Error en la carga del fichero de propiedades: " + e, e); //$NON-NLS-1$
		}
	}

	/**
	 * Recupera el CodeBase en el que se encuentra el fichero JAR de AutoFirma WebStart.
	 * @return CodeBase configurado o {@code null} si no est&aacute; definido.
	 */
	public static String getCodeBase() {
		return config.getProperty(PROPERTY_CODEBASE);
	}
}
