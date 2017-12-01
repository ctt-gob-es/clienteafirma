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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;

/** Configuraci&oacute;n para la gesti&oacute;n del almacenamiento temporal de ficheros en servidor. */
final class StorageConfig {

	/** Clave para la configuraci&oacute;n del directorio para la creacion de ficheros temporales. */
	static final String TMP_DIR_KEY =  "tmpDir"; //$NON-NLS-1$

	/** Directorio temporal por defecto. */
	private static String DEFAULT_TMP_DIR;

	/** Clave para la configuraci&oacute;n del tiempo de caducidad de los ficheros temporales. */
	private static final String EXPIRATION_TIME_KEY =  "expTime"; //$NON-NLS-1$

	/** Milisegundos que, por defecto, tardan los mensajes en caducar. */
	private static final long DEFAULT_EXPIRATION_TIME = 5000; // 5 segundos

	static {
		try {
			DEFAULT_TMP_DIR = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"El directorio temporal no ha podido determinarse por la variable de entorno 'java.io.tmpdir': " + e //$NON-NLS-1$
			);
			try {
				DEFAULT_TMP_DIR = File.createTempFile("tmp", null).getParentFile().getAbsolutePath(); //$NON-NLS-1$
			}
			catch (final IOException e1) {
				e1.printStackTrace();
				DEFAULT_TMP_DIR = null;
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"No se ha podido cargar un directorio temporal por defecto, se debera configurar expresamente en el fichero de propiedades: " + e1//$NON-NLS-1$
				);
			}
		}
	}

	private final Properties config;

	/** Crea el objeto de configuracion para el servicio de almacenamiento. */
	StorageConfig() {
		this.config = new Properties();
	}

	/** Carga del fichero de configuraci&oacute;n.
	 * @param path Ruta del fichero de configuraci&oacute;n (si existe).
	 * @throws FileNotFoundException Cuando no se encuentra el fichero de configuraci&oacute;n.
	 * @throws IOException Cuando ocurre un error durante la lectura del fichero. */
	void load(final String path) throws FileNotFoundException, IOException {
		if (path != null) {
 			try {
 				final InputStream is = StorageConfig.class.getClassLoader().getResourceAsStream(path);
				this.config.load(is);
				is.close();
			}
 			catch (final IOException e) {
				Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
					"No se ha podido cargar el fichero con las propiedades: " + e.toString() //$NON-NLS-1$
				);
			}
		}
	}

	/** Recupera el directorio configurado para la creaci&oacute;n de ficheros temporales o el por defecto.
	 * @return Directorio temporal.
	 * @throws NullPointerException Cuando no se indica la ruta del directorio temporal ni se puede obtener
	 * del sistema. */
	File getTempDir() {
		return new File(this.config.getProperty(TMP_DIR_KEY, DEFAULT_TMP_DIR).trim());
	}

	/** Recupera el tiempo en milisegundos que puede almacenarse un fichero antes de considerarse caducado.
	 * @return Tiempo m&aacute;ximo en milisegundos que puede tardarse en recoger un fichero antes de que
	 *         caduque. */
	long getExpirationTime() {
		try {
			return this.config.containsKey(EXPIRATION_TIME_KEY) ?
				Long.parseLong(this.config.getProperty(EXPIRATION_TIME_KEY)) :
					DEFAULT_EXPIRATION_TIME;
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"Tiempo de expiracion invalido en el fichero de configuracion, se usara" + DEFAULT_EXPIRATION_TIME + ": " + e//$NON-NLS-1$ //$NON-NLS-2$
			);
			return DEFAULT_EXPIRATION_TIME;
		}
	}
}
