/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.so.macos;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Platform;

/**
 * Funciones de utilidad para la configuraci&oacute;n de Mac.
 */
public class MacUtils {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String SCRIPT_NAME = "afirma";//$NON-NLS-1$
	private static final String SCRIPT_EXT = ".sh";//$NON-NLS-1$

	public static File newScriptFile() throws IOException {
		return File.createTempFile(SCRIPT_NAME, SCRIPT_EXT);
	}

	/** Escribe un <i>script</i> en un fichero dado.
	 * @param script Datos a escribir.
	 * @param scriptFile Fichero donde se escribir&aacute; el <i>script</i>.
	 * @param append <code>true</code> permite contatenar el contenido del fichero con lo que se va a escribir. <code>false</code> el fichero se sobrescribe.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero. */
	public static void writeScriptFile(final StringBuilder script, final File scriptFile, final boolean append) throws IOException{
		LOGGER.info("Se escribira en fichero el siguiente comando:\n" + script.toString()); //$NON-NLS-1$
		try (final FileOutputStream fout = new FileOutputStream(scriptFile, append);) {
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
			addAllPermissionsToFile(fileEntry);
		}
	}

	/**
	 * Concede permisos POSIX completos a un fichero para todos los usuarios.
	 * @param f Fichero al que se le conceden permisos.
	 */
	public static void addAllPermissionsToFile(final File f) {
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
				"No se ha podido dar permiso de ejecucion a '" + f.getAbsolutePath() + "': " + e//$NON-NLS-1$ //$NON-NLS-2$
			);
		}
	}


    /** Ejecuta un fichero de scripts.
	 * @param scriptFile Fichero de <i>script</i>.
	 * @param administratorMode <code>true</code> el <i>script</i> se ejecuta con permisos de adminsitrador, <code>false</code> en caso contrario.
	 * @param delete <code>true</code> se borra el fichero despu&eacute;s de haberse ejecutado.
	 * @return El objeto que da como resultado el <i>script</i>.
	 * @throws IOException Excepci&oacute;n lanzada en caso de ocurrir alg&uacute;n error en la ejecuci&oacute;n del <i>script</i>.
     * @throws InterruptedException Cuando el proceso se ve interrumpido (posiblemente por el usuario). */
	public static Object executeScriptFile(final File scriptFile, final boolean administratorMode, final boolean delete) throws IOException, InterruptedException {

		final ShellScript script = new ShellScript(scriptFile, delete);

		try {
			Object o;
			if (administratorMode) {
				LOGGER.info("Se ejecuta con permisos de administrador el script: " + scriptFile); //$NON-NLS-1$
				o = script.runAsAdministrator();
			}
			else {
				LOGGER.info("Se ejecuta el script: " + scriptFile); //$NON-NLS-1$
				o = script.run();
			}
			return o;
		}
		catch (final IOException e) {
			throw new IOException("Error en la ejecucion del script", e); //$NON-NLS-1$
		}
	}

	/** Mata el proceso de AutoFirma cuando estamos en macOS. En el resto de sistemas
	 * no hace nada.
	 * @param processIdText Texto que permite identificar el proceso. */
	public static void closeMacService(final String processIdText) {
		LOGGER.warning("Ejecuto kill"); //$NON-NLS-1$
		final ShellScript script = new ShellScript(
				"kill -9 $(ps -ef | grep " + processIdText + " | awk '{print $2}')"  //$NON-NLS-1$ //$NON-NLS-2$
				);
		try {
			script.run();
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido cerrar la aplicacion: " + e); //$NON-NLS-1$
		}
	}

	/** Coge el foco del sistema en macOS. En el resto de sistemas no hace nada. */
	public static void focusApplication() {
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			final String scriptCode = "tell me to activate"; //$NON-NLS-1$
			final AppleScript script = new AppleScript(scriptCode);
			try {
				script.run();
			}
			catch (final Exception e) {
				LOGGER.warning("Fallo cogiendo el foco en macOS: " + e); //$NON-NLS-1$
			}
		}
	}
}
