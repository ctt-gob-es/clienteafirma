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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Platform;

final class MozillaKeyStoreUtilitiesUnix {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String SOFTOKN3_SO = "libsoftokn3.so"; //$NON-NLS-1$

	private static final String[] NSS_PATHS = new String[] {
		"/usr/lib/x86_64-linux-gnu/nss", // Debian 64 //$NON-NLS-1$
		"/usr/lib/x86_64-linux-gnu/",  //$NON-NLS-1$
		"/usr/lib/firefox", //$NON-NLS-1$
		"/usr/lib/firefox-" + searchLastFirefoxVersion("/usr/lib/"), //$NON-NLS-1$ //$NON-NLS-2$
		"/opt/firefox", //$NON-NLS-1$
		"/opt/firefox-" + searchLastFirefoxVersion("/opt/"), //$NON-NLS-1$ //$NON-NLS-2$
		"/lib", //$NON-NLS-1$
		"/usr/lib", //$NON-NLS-1$
		"/usr/lib/nss", //$NON-NLS-1$
		"/usr/lib/i386-linux-gnu/nss", /* En algunos Ubuntu y Debian 32 */ //$NON-NLS-1$
		"/usr/lib/i386-linux-gnu",  //$NON-NLS-1$
		"/opt/fedora-ds/clients/lib", //$NON-NLS-1$
		"/opt/google/chrome", /* NSS de Chrome cuando no hay NSS de Mozilla de la misma arquitectura */ //$NON-NLS-1$
		"/usr/lib/thunderbird", /* Si hay Thunderbird pero no Firefox */ //$NON-NLS-1$
		"/usr/lib64", /* NSS cuando solo hay Firefox de 64 en el sistema */ //$NON-NLS-1$
		"/lib64" // No es comun para NSS, pero si para sus dependencias //$NON-NLS-1$
	};

	private static final String[] SQLITE_LIBS = {
		"mozsqlite3.so",  //$NON-NLS-1$
		"libmozsqlite3.so", //$NON-NLS-1$
		"libsqlite3.so.0", //$NON-NLS-1$
		"libnspr4.so"	 //$NON-NLS-1$
	};

	private MozillaKeyStoreUtilitiesUnix() {
		// No instanciable
	}

	static String getNSSLibDirUnix() throws FileNotFoundException {

		String nssLibDir = null;

		for (final String path : NSS_PATHS) {
			if (new File(path, SOFTOKN3_SO).isFile()) {
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
							"Descartamos el NSS situado en '" + path + "' porque no puede cargarse adecuadamente: " + e //$NON-NLS-1$ //$NON-NLS-2$
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
		final File directoryLib = new File(startDir);
		if (directoryLib.isDirectory()) {
			final String filenames[] = directoryLib.list();
			final List<String> firefoxDirectories = new ArrayList<>();
			if (filenames != null) {
				for (final String filename : filenames) {
					if (filename.startsWith("firefox-")) { //$NON-NLS-1$
						firefoxDirectories.add(filename.replace("firefox-", "")); //$NON-NLS-1$ //$NON-NLS-2$
					}
				}
			}
			if (firefoxDirectories.isEmpty()) {
				return ""; //$NON-NLS-1$
			}
			for (int i = 0; i < firefoxDirectories.size(); i++) {
				try {
					Integer.getInteger(firefoxDirectories.get(i));
				}
				catch (final Exception e) {
					LOGGER.warning(
						"El perfil '" + directoryLib.getAbsolutePath().replace(Platform.getUserHome(), "USERHOME") + "' no tiene declarada su version: " + e //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					);
					firefoxDirectories.remove(i);
				}
			}
			if (firefoxDirectories.size() == 1) {
				return firefoxDirectories.get(0);
			}
			Collections.sort(
				firefoxDirectories,
				new Comparator<String>() {
					/** {@inheritDoc} */
					@Override
					public int compare(final String o1, final String o2) {
						return o1.compareTo(o2);
					}
				}
			);
			return firefoxDirectories.get(0);
		}
		return ""; //$NON-NLS-1$
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

}
