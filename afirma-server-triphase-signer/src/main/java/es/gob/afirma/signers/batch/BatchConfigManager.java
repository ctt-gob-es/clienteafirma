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
import java.util.Properties;
import java.util.logging.Logger;

/**
 * Gestiona la configuraci&oacute;n espec&iacute;fica del proceso de firma de lotes.
 */
public class BatchConfigManager {

	private static final int MAX_CONCURRENT_SIGNS = 10;

	private static final Properties p;

	private static Boolean CONCURRENT_MODE = null;

	private static Integer CONCURRENT_SIGNS = null;

	private static String[] ALLOWED_SOURCES = null;

	private static File TEMD_DIR = null;

	static {
		p = new Properties();
		try {
			p.load(BatchConfigManager.class.getResourceAsStream("/signbatch.properties")); //$NON-NLS-1$
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No se ha podido cargar la configuracion del proceso por lotes, se usara el modo no concurrente: " + e //$NON-NLS-1$
			);
		}
	}

	/**
	 * Indica si esta habilitado el modo de ejecuci&oacute;n concurrente.
	 * @return {@code true} si las firmas se ejecutar&aacute;n de forma concurrente,
	 * {@code false} en caso contrario.
	 */
	public static boolean isConcurrentMode() {
		if (CONCURRENT_MODE == null) {
			CONCURRENT_MODE = Boolean.valueOf(p.getProperty("concurrentmode")); //$NON-NLS-1$
		}
		return CONCURRENT_MODE.booleanValue();
	}

	/**
	 * Indica si esta habilitado el modo de ejecuci&oacute;n concurrente.
	 * @return {@code true} si las firmas se ejecutar&aacute;n de forma concurrente,
	 * {@code false} en caso contrario.
	 */
	public static int getMaxCurrentSigns() {
		if (CONCURRENT_SIGNS == null) {
			int n = 0;
			try {
				n = Integer.parseInt(p.getProperty("maxcurrentsigns", Integer.toString(MAX_CONCURRENT_SIGNS))); //$NON-NLS-1$
			}
			catch (final Exception e) {
				n = MAX_CONCURRENT_SIGNS;
			}
			CONCURRENT_SIGNS = new Integer(n > 0 && n <= MAX_CONCURRENT_SIGNS ? n : MAX_CONCURRENT_SIGNS);
		}
		return CONCURRENT_SIGNS.intValue();
	}

	/**
	 * Obtiene el listado de fuentes permitidas como origen de los datos. El formato
	 * de las fuentes puede ser una cadena de texto con una URL (admite '*' como
	 * comod&iacute;n) o la cadena "base64".
	 * @return Listado de fuentes.
	 * @throws IllegalStateException Cuando no se ha indicado ninguna fuente.
	 */
	public static String[] getAllowedSources() {
		if (ALLOWED_SOURCES == null) {
			final String sources = p.getProperty("allowedsources"); //$NON-NLS-1$
			if (sources == null || sources.isEmpty()) {
				throw new IllegalStateException(
						"No se ha definido ningun permiso para la carga de datos" //$NON-NLS-1$
						);
			}
			ALLOWED_SOURCES = sources.split(";"); //$NON-NLS-1$
		}
		return ALLOWED_SOURCES;
	}

	/**
	 * Obtiene el directorio para el guardado de ficheros temporales.
	 * @return Directorio temporal configurado o el por defecto si no se configur&oacute;
	 * ninguno o no era v&aacute;lido.
	 */
	public static File getTempDir() {
		if (TEMD_DIR == null) {
			final String defaultDir = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$
			final File f = new File(p.getProperty("tmpdir", defaultDir)); //$NON-NLS-1$

			if (f.isDirectory() && f.canRead() && f.canWrite()) {
				return f;
			}
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
					"El directorio temporal configurado (" + f.getAbsolutePath() + ") no es valido, se usaran el por defecto: " + defaultDir //$NON-NLS-1$ //$NON-NLS-2$
					);
			TEMD_DIR = new File(defaultDir);
		}
		return TEMD_DIR;
	}
}
