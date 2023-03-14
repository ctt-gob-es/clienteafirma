/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.LoggerUtil;

/**
 * Funciones de utilidad para la configuraci&oacute;n de Mac.
 */
public class ConfiguratorMacUtils {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$


	/** Escribe un <i>script</i> en un fichero dado.
	 * @param macScript Fichero de <i>script</i>.
	 * @param script Datos a escribir.
	 * @param append <code>true</code> permite contatenar el contenido del fichero con lo que se va a escribir. <code>false</code> el fichero se sobrescribe.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero. */
	static void writeScriptFile(final StringBuilder script, final File macScript, final boolean append) throws IOException{
		LOGGER.info("Se escribira en fichero el siguiente comando:\n" + script.toString()); //$NON-NLS-1$
		try (final FileOutputStream fout = new FileOutputStream(macScript, append);) {
			fout.write(script.toString().getBytes());
			fout.write("\n".getBytes()); //$NON-NLS-1$
		}
	}

	/**
	 * Da permisos de ejecuci&oacute;n a todos los ficheros de un directorio dado.
	 * @param dir Directorio al que dar permiso.
	 */
	public static void addExexPermissionsToAllFilesOnDirectory(final File dir) {

		for (final File fileEntry : dir.listFiles()) {
			addExexPermissionsToFile(fileEntry);
		}
	}

	/**
	 * Concede permisos POSIX completos a un fichero para todos los usuarios.
	 * @param f Fichero al que se le conceden permisos.
	 */
	public static void addExexPermissionsToFile(final File f) {
		final Set<PosixFilePermission> perms = new HashSet<>();
		perms.add(PosixFilePermission.OWNER_EXECUTE);
		perms.add(PosixFilePermission.GROUP_EXECUTE);
		perms.add(PosixFilePermission.OTHERS_EXECUTE);
		perms.add(PosixFilePermission.OWNER_READ);
		perms.add(PosixFilePermission.GROUP_READ);
		perms.add(PosixFilePermission.OTHERS_READ);
		perms.add(PosixFilePermission.OWNER_WRITE);
		perms.add(PosixFilePermission.GROUP_WRITE);
		perms.add(PosixFilePermission.OTHERS_WRITE);
		try {
			Files.setPosixFilePermissions(
				Paths.get(f.getAbsolutePath()),
				perms
			);
		}
		catch (final Exception e) {
			LOGGER.warning(
				"No se ha podido dar permiso de ejecucion a '" + LoggerUtil.getCleanUserHomePath(f.getAbsolutePath()) + "': " + e//$NON-NLS-1$ //$NON-NLS-2$
			);
		}
	}
}
