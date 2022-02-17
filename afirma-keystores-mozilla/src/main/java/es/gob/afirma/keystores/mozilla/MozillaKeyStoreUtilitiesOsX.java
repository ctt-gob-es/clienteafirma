/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.mozilla.apple.ShellScript;

/** Utilidades para la gesti&oacute;n de almacenes de claves Mozilla NSS en Apple OS X.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class MozillaKeyStoreUtilitiesOsX {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String LIBRARY_FOLDER = "/usr/local/lib"; //$NON-NLS-1$

	private MozillaKeyStoreUtilitiesOsX() {
		// No dejamos instanciar
	}

	/** Configura adecuadamente el acceso (mediante enlaces simb&oacute;licos) de NSS en OS X.
	 * @param binDir Directorio original de las bibliotecas NSS.
	 * @throws AOException Si ocurre cualquier error durante el proceso. */
	public static void configureMacNSS(final String binDir) throws AOException {

		if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
			return;
		}

		if (binDir == null) {
			LOGGER.severe("El directorio de NSS para configurar proporcionado es nulo, no se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		final String nssBinDir = binDir.endsWith(File.separator) ? binDir : binDir + File.separator;

		// Intentamos la carga, para ver si es necesaria la reconfiguracion
		try {
			System.load(nssBinDir + "libmozglue.dylib"); //$NON-NLS-1$
			System.load(nssBinDir + "libnss3.dylib"); //$NON-NLS-1$
			System.load(nssBinDir + "libsoftokn3.dylib"); //$NON-NLS-1$
			return; // Si funciona salimos sin hacer nada
		}
		catch (final Exception e) {
			LOGGER.warning(
				"No se puede realizar una carga directa de NSS, se crearan enlaces simbolicos: " + e //$NON-NLS-1$
			);
		}
		catch(final UnsatisfiedLinkError e) {
			LOGGER.warning(
				"No se puede realizar una carga directa de NSS, se crearan enlaces simbolicos: " + e //$NON-NLS-1$
			);
		}

		// Si sigue sin funcionar, tendremos que crear enlaces simbolicos de las dependencias en un
		// directorio desde el que se traten de cargar
		final String[] libs = {
				"libmozglue.dylib", // Firefox 11 y superiores //$NON-NLS-1$
				"libmozutils.dylib", // Firefox 9 y 10 //$NON-NLS-1$
				"libnspr4.dylib", //$NON-NLS-1$
				"libplds4.dylib", //$NON-NLS-1$
				"libplc4.dylib", //$NON-NLS-1$
				"libnssutil3.dylib", //$NON-NLS-1$
				"libmozsqlite3.dylib", //$NON-NLS-1$
				"libnss3.dylib" // Detectado en Firefox 30, quizas se introdujo en versiones anteriores //$NON-NLS-1$
		};

		// Creamos via AppleScript enlaces simbolicos en un directorio de bibliotecas para que
		// se tomen de ahi las dependencias
		final StringBuilder sb = new StringBuilder();

		if (!new File(LIBRARY_FOLDER).exists()) {
			sb.append("mkdir ").append(LIBRARY_FOLDER).append("; "); //$NON-NLS-1$ //$NON-NLS-2$
		}

		for (final String lib : libs) {
			if (new File(nssBinDir, lib).exists()) {
				sb.append("ln -s "); //$NON-NLS-1$
				sb.append(nssBinDir);
				sb.append(lib);
				sb.append(" "); //$NON-NLS-1$
				sb.append(LIBRARY_FOLDER);
				sb.append(File.separator);
				sb.append(lib);
				sb.append("; "); //$NON-NLS-1$
			}
		}
		try {
			final ShellScript script = new ShellScript(sb.toString());
			script.runAsAdministrator();
		}
		catch(final Exception e) {
			LOGGER.log(Level.SEVERE, "No se han podido crear los enlaces simbolicos para NSS: " + e, e); //$NON-NLS-1$
			LOGGER.severe("Fallo en el script: " + sb.toString()); //$NON-NLS-1$
		}

		// Y reintentamos la carga, para ver si ha surtido efecto
		try {
			System.load(nssBinDir + "libsoftokn3.dylib"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new AOException("La configuracion de NSS para Mac OS X ha fallado por motivos de seguridad: " + e); //$NON-NLS-1$
		}
		catch(final UnsatisfiedLinkError e) {
			throw new AOException("La configuracion de NSS para Mac OS X ha fallado: " + e); //$NON-NLS-1$
		}
	}

	static String getSystemNSSLibDirMacOsX() throws FileNotFoundException {

		final String[] paths = {
			"/Applications/Firefox.app/Contents/MacOS", //$NON-NLS-1$
			"/lib", //$NON-NLS-1$
			"/usr/lib", //$NON-NLS-1$
			"/usr/lib/nss", //$NON-NLS-1$
			"/Applications/Minefield.app/Contents/MacOS" //$NON-NLS-1$
		};

		String nssLibDir = null;

		for (final String path : paths) {
			if (new File(path, "libsoftokn3.dylib").exists()) { //$NON-NLS-1$
				nssLibDir = path;
				break;
			}
		}

		if (nssLibDir == null) {
			throw new FileNotFoundException("No se ha podido determinar la localizacion de NSS en Mac OS X"); //$NON-NLS-1$
		}

		return nssLibDir;
	}
}
