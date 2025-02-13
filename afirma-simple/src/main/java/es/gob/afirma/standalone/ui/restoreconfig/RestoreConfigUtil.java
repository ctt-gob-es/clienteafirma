/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.restoreconfig;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.LoggerUtil;

/**
 * Clase con diversas utilidades necesarias para llevar a cabo la tarea de restauraci&oacute;n de
 * la configuraci&oacute;n de navegadores.
 *
 */
final class RestoreConfigUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	static final String CERT_ALIAS_BROWSER = "Autofirma ROOT"; //$NON-NLS-1$

	private RestoreConfigUtil() {
		// No instanciable
	}

	/** Guarda datos en disco.
	 * @param data Datos a guardar.
	 * @param outDir Directorio local.
	 * @throws IOException Cuando ocurre un error durante el guardado. */
	static void installFile(final byte[] data, final File outDir) throws IOException {
		try (
			final OutputStream configScriptOs = new FileOutputStream(outDir);
			final BufferedOutputStream bos = new BufferedOutputStream(configScriptOs);
		) {
			bos.write(data);
		}
	}

	/** Elimina un directorio con todo su contenido.
	 * @param targetDir Directorio a eliminar. */
	static void deleteDir(final File targetDir) {
		try {
			Files.walkFileTree(
				targetDir.toPath(),
				new SimpleFileVisitor<Path>() {
			         @Override
			         public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
			             Files.delete(file);
			             return FileVisitResult.CONTINUE;
			         }
			         @Override
			         public FileVisitResult postVisitDirectory(final Path dir, final IOException e) throws IOException {
			             if (e != null) {
			            	 throw e;
			             }
			             Files.delete(dir);
		                 return FileVisitResult.CONTINUE;
			         }
			   }
			);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo borrar el directorio '" + LoggerUtil.getCleanUserHomePath(targetDir.getAbsolutePath()) + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/** Recupera el directorio en el que se encuentra la aplicaci&oacute;n actual.
	 * @return Directorio de ejecuci&oacute;n. */
	static File getApplicationDirectory() {
		try {
			return new File(RestoreConfigUtil.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()).getParentFile();
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo localizar el directorio del fichero en ejecucion: " + e); //$NON-NLS-1$
		}
		return null;
	}
}
