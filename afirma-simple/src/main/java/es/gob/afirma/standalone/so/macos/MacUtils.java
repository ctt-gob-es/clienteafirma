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

    /** Ejecuta un fichero de scripts.
	 * @param scriptFile Ruta del fichero de <i>script</i>.
	 * @param administratorMode {@code true} el <i>script</i> se ejecuta con permisos de adminsitrador,
	 * {@code false} en caso contrario.
	 * @param delete {@code true} borra el fichero despu&eacute;s de haberse ejecutado, {@code false} no hace nada.
	 * @return La cadena que da como resultado el <i>script</i>.
	 * @throws IOException Cuando ocurre un error en la ejecuci&oacute;n del <i>script</i>.
     * @throws InterruptedException  Cuando se interrumpe la ejecuci&oacute;n del script (posiblemente por el usuario). */
	public static String executeScriptFile(final File scriptFile, final boolean administratorMode, final boolean delete) throws IOException, InterruptedException {

		final ShellScript script = new ShellScript(scriptFile, delete);
		try {
			String result;
			if (administratorMode) {
				LOGGER.info("Se ejecuta con permisos de administrador el script: " + scriptFile); //$NON-NLS-1$
				result = script.runAsAdministrator();
			}
			else {
				LOGGER.info("Se ejecuta el script: " + scriptFile); //$NON-NLS-1$
				result = script.run();
			}
			return result;
		}
		catch (final IOException e) {
			throw new IOException("Error en la ejecucion del script", e); //$NON-NLS-1$
		}
	}

	/** Mata el proceso intermedio de Autofirma cuando estamos en macOS. El proceso intermedio
	 * no es la aplicaci&oacute;n Java, sino el ejecutable que traslada los par&aacute;metros
	 * proporcionados por el navegador Web a la aplicaci&oacute;n Java.
	 * @param sessionIdText Texto contenido en la llamada al proceso y que permite identificarlo. */
	public static void closeMacService(final String sessionIdText) {

		if (sessionIdText == null) {
			LOGGER.warning("No se ha proporcionado el id del proceso que hay que cerrar"); //$NON-NLS-1$
			return;
		}

		LOGGER.warning("Ejecuto kill"); //$NON-NLS-1$
		final ShellScript script = new ShellScript(
				"kill -9 $(ps -ef | grep " + sessionIdText + " | awk '{print $2}')"  //$NON-NLS-1$ //$NON-NLS-2$
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

	/**
	 * Comprueba si un fichero JAR se encuentra dentro de la estructura de una aplicaci&oacute;n Mac
	 * y devuelve el ejecutable del mismo nombre para su arranque.
	 * @param jarFile Fichero JAR
	 * @return Fichero de ejecuci&oacute;n o {@code null} si no se puede localizar.
	 */
	public static File getMacApp(final File jarFile) {

		final File jarDir = jarFile.getParentFile();
		if (jarDir != null && "JAR".equals(jarDir.getName())) { //$NON-NLS-1$
			final File resourcesDir = jarDir.getParentFile();
			if (resourcesDir != null && "Resources".equals(resourcesDir.getName())) { //$NON-NLS-1$
				final File contentsDir = resourcesDir.getParentFile();
				if (contentsDir != null && "Contents".equals(contentsDir.getName())) { //$NON-NLS-1$
					final File macOSDir = new File(contentsDir, "MacOS"); //$NON-NLS-1$
					if (macOSDir.isDirectory()) {
						final String exeName = jarFile.getName().substring(0, jarFile.getName().length() - ".jar".length()); //$NON-NLS-1$
						final File exeFile = new File(macOSDir, exeName);
						if (exeFile.isFile()) {
							return exeFile;
						}
					}
				}
			}
		}
		return null;
	}
}
