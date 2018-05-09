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
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;

/** Configuraci&oacute;n para la gesti&oacute;n del almacenamiento temporal de ficheros en servidor. */
final class StorageConfig {
	
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	/** Clave para la configuraci&oacute;n del directorio para la creacion de ficheros temporales. */
	static final String TMP_DIR_KEY =  "tmpDir"; //$NON-NLS-1$

	/** Directorio temporal por defecto. */
	private static String defaultTmpDir;
	
	private static final File TMP_DIR;

	/** Fichero de configuraci&oacute;n. */
	private static final String CONFIG_FILE = "configuration.properties"; //$NON-NLS-1$

	static {
		
		final Properties config = new Properties();
		try {
			final InputStream is = StorageConfig.class.getClassLoader().getResourceAsStream(CONFIG_FILE);
			config.load(is);
			is.close();
		}
		catch (final IOException e) {
			LOGGER.severe(
				"No se ha podido cargar el fichero con las propiedades (" + CONFIG_FILE + "), se usaran los valores por defecto: " + e.toString() //$NON-NLS-1$ //$NON-NLS-2$
			);
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
				LOGGER.warning(
					"No se ha podido cargar un directorio temporal por defecto, se debera configurar expresamente en el fichero de propiedades: "  + e1 //$NON-NLS-1$
				);
			}
		}

		TMP_DIR = new File(config.getProperty(TMP_DIR_KEY, defaultTmpDir).trim());
	}

	/** Recupera el directorio configurado para la creaci&oacute;n de ficheros temporales o el por defecto.
	 * @return Directorio temporal.
	 * @throws NullPointerException Cuando no se indica la ruta del directorio temporal ni se puede obtener
	 * del sistema. */
	static File getTempDir() {
		return TMP_DIR;
	}

}
