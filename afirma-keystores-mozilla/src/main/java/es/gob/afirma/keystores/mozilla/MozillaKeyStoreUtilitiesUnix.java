/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import es.gob.afirma.core.misc.Platform;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;
import java.util.logging.Logger;

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

		if("64".equals(javaArch)) { //$NON-NLS-1$
			nssPaths.add("/usr/lib/x86_64-linux-gnu/nss"); //$NON-NLS-1$
			nssPaths.add("/usr/lib/x86_64-linux-gnu"); //$NON-NLS-1$
		} else if("32".equals(javaArch)) { //$NON-NLS-1$
			nssPaths.add("/usr/lib/i386-linux-gnu/nss"); /* En algunos Ubuntu y Debian 32 */ //$NON-NLS-1$
			nssPaths.add("/usr/lib/i386-linux-gnu"); //$NON-NLS-1$
		}

		nssPaths.add(systemLibDir + "/nss"); //$NON-NLS-1$
		nssPaths.add(systemLibDir); //$NON-NLS-1$
		nssPaths.add(systemLibDir + "/firefox"); //$NON-NLS-1$

		// Preserve backwards-compatibility on https://github.com/ctt-gob-es/clienteafirma/issues/27#issuecomment-488402089
		nssPaths.add(systemLibDir + "/firefox-");  //$NON-NLS-1$
		nssPaths.add(systemLibDir + "/thunderbird"); //$NON-NLS-1$

		if(new File("/lib" + javaArch).isDirectory()) { //$NON-NLS-1$
			nssPaths.add("/lib" + javaArch); //$NON-NLS-1$
		} else {
			nssPaths.add("/lib"); //$NON-NLS-1$
		}

		nssPaths.add("/opt/firefox"); //$NON-NLS-1$

		// Preserve backwards-compatibility on https://github.com/ctt-gob-es/clienteafirma/issues/27#issuecomment-488402089
		nssPaths.add("/opt/firefox-"); //$NON-NLS-1$

		nssPaths.add("/opt/fedora-ds/clients/lib"); //$NON-NLS-1$
		nssPaths.add("/opt/google/chrome"); /* NSS de Chrome cuando no hay NSS de Mozilla de la misma arquitectura */ //$NON-NLS-1$

		for(int i = nssPaths.size() - 1; i >= 0; i--) {
			if(!new File(nssPaths.get(i)).isDirectory()) {
				nssPaths.remove(i);
			}
		}
		return nssPaths.toArray(new String[0]);
	}

	static String getNSSLibDirUnix() throws FileNotFoundException {

		String nssLibDir = null;

		for (final String path : NSS_PATHS) {
			if (new File(path, SOFTOKN3_SO).exists()){
				nssLibDir = path;
			}
		}
		if (nssLibDir == null) {
			throw new FileNotFoundException("No se ha podido determinar la localizacion de NSS en UNIX"); //$NON-NLS-1$
		}

		for (final String path : NSS_PATHS) {
			final File dir = new File(path);
			for (final String tailingLib: SQLITE_LIBS) {
				final File library = new File(dir, tailingLib);
				if (library.exists()) {
					try {
						System.load(library.getAbsolutePath());
						return nssLibDir;
					}
					catch (final Exception e) {
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
}
