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
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.mozilla.bintutil.ElfParser;

final class MozillaKeyStoreUtilitiesUnix {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String SOFTOKN3_SO = "libsoftokn3.so"; //$NON-NLS-1$

	private static final String[] NSS_PATHS = getNssPaths();

	private static final String[] SQLITE_LIBS = {
		"mozsqlite3.so",  //$NON-NLS-1$
		"libmozsqlite3.so", //$NON-NLS-1$
		"libsqlite3.so.0", //$NON-NLS-1$
		"libnspr4.so"	 //$NON-NLS-1$
	};

	private MozillaKeyStoreUtilitiesUnix() {
		// No instanciable
	}

	private static String[] getNssPaths() {
		final List<String> nssPaths = new ArrayList<>();
		final String javaArch = Platform.getJavaArch();
		final String systemLibDir = Platform.getSystemLibDir();

		if ("64".equals(javaArch)) { //$NON-NLS-1$
			nssPaths.add("/usr/lib/x86_64-linux-gnu/nss"); //$NON-NLS-1$
			nssPaths.add("/usr/lib/x86_64-linux-gnu"); //$NON-NLS-1$
		}
		else if("32".equals(javaArch)) { //$NON-NLS-1$
			nssPaths.add("/usr/lib/i386-linux-gnu/nss"); /* En algunos Ubuntu y Debian 32 */ //$NON-NLS-1$
			nssPaths.add("/usr/lib/i386-linux-gnu"); //$NON-NLS-1$
		}

		nssPaths.add(systemLibDir + "/nss"); //$NON-NLS-1$
		nssPaths.add(systemLibDir);
		nssPaths.add(systemLibDir + "/firefox"); //$NON-NLS-1$

		// Preserve backwards-compatibility on https://github.com/ctt-gob-es/clienteafirma/issues/27#issuecomment-488402089
		String firefoxVersion = searchLastFirefoxVersion(systemLibDir);
		if (firefoxVersion != null) {
			nssPaths.add(systemLibDir + "/firefox-" + firefoxVersion);  //$NON-NLS-1$
		}
		nssPaths.add(systemLibDir + "/thunderbird"); //$NON-NLS-1$

		if (isDirectory("/lib" + javaArch)) { //$NON-NLS-1$
			nssPaths.add("/lib" + javaArch); //$NON-NLS-1$
		}
		else {
			nssPaths.add("/lib"); //$NON-NLS-1$
		}

		nssPaths.add("/opt/firefox"); //$NON-NLS-1$

		// Preserve backwards-compatibility on https://github.com/ctt-gob-es/clienteafirma/issues/27#issuecomment-488402089
		if (isDirectory("/usr/lib" + javaArch)) { //$NON-NLS-1$
			nssPaths.add("/usr/lib" + javaArch); //$NON-NLS-1$
		}
		else {
			nssPaths.add("/usr/lib"); //$NON-NLS-1$
		}

		firefoxVersion = searchLastFirefoxVersion("/opt"); //$NON-NLS-1$
		if (firefoxVersion != null) {
			nssPaths.add("/opt/firefox-" + firefoxVersion); //$NON-NLS-1$
		}

		nssPaths.add("/opt/fedora-ds/clients/lib"); //$NON-NLS-1$

		// NSS de Chrome cuando no hay NSS de Mozilla de la misma arquitectura
		nssPaths.add("/opt/google/chrome"); //$NON-NLS-1$

		for (int i = nssPaths.size() - 1; i >= 0; i--) {
			if (!isDirectory(nssPaths.get(i))) {
				nssPaths.remove(i);
			}
		}
		return nssPaths.toArray(new String[0]);
	}

	static String getNSSLibDirUnix() throws FileNotFoundException {

		String nssLibDir = null;

		for (final String path : NSS_PATHS) {
			final File tmpFile = new File(path, SOFTOKN3_SO);
			if (tmpFile.isFile() && ElfParser.archMatches(tmpFile)) {
				nssLibDir = path;
				break;
			}
		}

		if (nssLibDir == null) {
			throw new FileNotFoundException("No se ha podido determinar la localizacion de NSS en UNIX"); //$NON-NLS-1$
		}

		LOGGER.info("Se usara el NSS encontrado en '" + nssLibDir + "'"); //$NON-NLS-1$ //$NON-NLS-2$

		for (final String path : NSS_PATHS) {
			final File dir = new File(path);
			for (final String tailingLib: SQLITE_LIBS) {
				final File library = new File(dir, tailingLib);
				if (library.exists()) {
					try {
						System.load(library.getAbsolutePath());
						return nssLibDir;
					}
					catch (final Exception | Error e) {
						LOGGER.warning(
							"Descartamos el NSS situado en '" + LoggerUtil.getCleanUserHomePath(path) + "' porque no puede cargarse adecuadamente: " + e //$NON-NLS-1$ //$NON-NLS-2$
						);
					}
				}
			}
		}
		throw new FileNotFoundException(
			"No se ha podido determinar la localizacion de NSS en UNIX (sqlite)" //$NON-NLS-1$
		);
	}

	/** Busca la &uacute;ltima versi&oacute;n de Firefox instalada en un sistema
	 * Linux o Solaris.
	 * @param startDir Directorio de inicio para la b&uacute;squeda.
	 * @return &Uacute;ltima versi&oacute;n instalada en el sistema. */
	private static String searchLastFirefoxVersion(final String startDir) {

		Version maxVersion = null;
		final File directoryLib = new File(startDir);
		if (directoryLib.isDirectory()) {

			// Tomamos lo numeros de version de firefox identificados
			final List<String> firefoxVersions = new ArrayList<>();
			for (final String filename : directoryLib.list()) {
				if (filename.startsWith("firefox-")) { //$NON-NLS-1$
					firefoxVersions.add(filename.replace("firefox-", "")); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}

			// Calculamos el numero de version mayor
			for (final String versionText : firefoxVersions) {
				Version version;
				try {
					version = new Version(versionText);
				}
				catch (final Exception e) {
					LOGGER.warning(
						"Se encontro un numero de version de Firefox no soportado (" + versionText + "): " + e //$NON-NLS-1$ //$NON-NLS-2$
					);
					continue;
				}
				if (maxVersion == null || version.compareTo(maxVersion) > 0) {
					maxVersion = version;
				}
			}
		}
		return maxVersion != null ? maxVersion.toString() : null;
	}

	/** Recupera el listado de dependencias de la biblioteca "libsoftkn3.so" para
	 * sistemas operativos UNIX (Linux, Solaris). Los nombres apareceran ordenados de tal forma las
	 * bibliotecas no tengan dependencias de otra que no haya aparecido
	 * anterioremente en la lista.
	 * @param nssPath Ruta al directorio de NSS (terminado en barra).
	 * @return Listado con los nombres de las bibliotecas. */
	static String[] getSoftkn3DependenciesUnix(final String nssPath) {
		return new String[] {
			nssPath + "libnspr4.so",      // Firefox 2 y superior //$NON-NLS-1$
			nssPath + "libplds4.so",      // Firefox 2 y superior //$NON-NLS-1$
			nssPath + "libplc4.so",       // Firefox 2 y superior //$NON-NLS-1$
			nssPath + "libnssutil3.so",   // Firefox 2 y superior //$NON-NLS-1$
			nssPath + "libsqlite3.so",    // Firefox 2            //$NON-NLS-1$
			nssPath + "libmozsqlite3.so", // Firefox 3 y superior //$NON-NLS-1$
			nssPath + "libsqlite3.so.0"   // Variante de SQLite en ciertos Debian //$NON-NLS-1$
		};
	}

	/**
	 * Versi&oacute;n software, formado por part&iacute;culas num&eacute;ricas separadas por puntos.
	 */
	private static class Version implements Comparable<MozillaKeyStoreUtilitiesUnix.Version> {
		String text;
		int[] versionsParticles;

		public Version(final String version) {
			this.text = version;
			final String[] subVersions = version.split("\\."); //$NON-NLS-1$
			this.versionsParticles = new int[subVersions.length];
			for (int i = 0; i < subVersions.length; i++) {
				final String subVersion = subVersions[i];
				try {
					this.versionsParticles[i] = Integer.parseInt(subVersion);
				}
				catch (final Exception e) {
					throw new IllegalArgumentException("Identificador de version no soportado (" + version + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}

		public int[] getVersionsParticles() {
			return this.versionsParticles != null ? this.versionsParticles.clone() : null;
		}

		@Override
		public int compareTo(final Version other) {
			if (other == null) {
	            return 1;
			}
			final int[] otherParticles = other.getVersionsParticles();
	        final int length = Math.max(this.versionsParticles.length, otherParticles.length);
	        for (int i = 0; i < length; i++) {
	            final int thisPart = i < this.versionsParticles.length ? this.versionsParticles[i] : 0;
	            final int otherPart = i < otherParticles.length ? otherParticles[i] : 0;
	            if (thisPart < otherPart) {
					return -1;
				}
	            if (thisPart > otherPart) {
					return 1;
				}
	        }
	        return 0;
		}

		@Override
		public String toString() {
			return this.text;
		}
	}

	/** Determina la existencia de un directorio a partir de su ruta completa.
	 * Funciona con enlaces simb&oacute;licos.
	 * @param fullPath Ruta completa del directorio.
	 * @return <code>true</code> si la ruta apunta a un directorio, <code>false</code>
	 *         si apunta a otra cosa o a nada. */
	private static boolean isDirectory(final String fullPath) {
		if (fullPath == null || fullPath.isEmpty()) {
			return false;
		}
		final Path path;
		try {
			path = new File(fullPath).toPath().toRealPath();
		}
		catch (final NoSuchFileException e) {
			return false;
		}
		catch (final IOException e) {
			LOGGER.warning(
				"No se ha podido comprobar la existencia del directorio '" + LoggerUtil.getCleanUserHomePath(fullPath) + "': " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			return false;
		}
		return Files.isDirectory(path);
	}
}
