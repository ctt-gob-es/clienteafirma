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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.KeyStoreUtilities;
import es.gob.afirma.keystores.mozilla.bintutil.MsPortableExecutable;
import es.gob.afirma.keystores.mozilla.bintutil.PEParserException;
import es.gob.afirma.keystores.mozilla.bintutil.PeMachineType;

final class MozillaKeyStoreUtilitiesWindows {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String P11_CONFIG_VALID_CHARS = ":\\0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_.\u007E"; //$NON-NLS-1$

	// Bibliotecas Windows de Firefox

	/** Nombre del PKCS#11 NSS en Windows. */
	private static final String SOFTOKN3_DLL = "softokn3.dll"; //$NON-NLS-1$

	private static final String MSVCR100_DLL = "msvcr100.dll"; //$NON-NLS-1$
	private static final String MSVCP100_DLL = "msvcp100.dll"; //$NON-NLS-1$
	private static final String MSVCR120_DLL = "msvcr120.dll"; //$NON-NLS-1$
	private static final String MSVCP120_DLL = "msvcp120.dll"; //$NON-NLS-1$
	private static final String MSVCR140_DLL = "vcruntime140.dll"; //$NON-NLS-1$
	private static final String MSVCP140_DLL = "msvcp140.dll"; //$NON-NLS-1$
	private static final String PLC4_DLL = "plc4.dll"; //$NON-NLS-1$
	private static final String PLDS4_DLL = "plds4.dll"; //$NON-NLS-1$
	private static final String NSPR4_DLL = "nspr4.dll"; //$NON-NLS-1$
	private static final String MOZSQLITE3_DLL = "mozsqlite3.dll"; //$NON-NLS-1$
	private static final String MOZCRT19_DLL = "mozcrt19.dll"; //$NON-NLS-1$
	private static final String NSSUTIL3_DLL = "nssutil3.dll"; //$NON-NLS-1$
	private static final String FREEBL3_DLL = "freebl3.dll"; //$NON-NLS-1$
	private static final String NSSDBM3_DLL = "nssdbm3.dll";  //$NON-NLS-1$
	private static final String SQLITE3_DLL = "sqlite3.dll"; //$NON-NLS-1$

	// Novedades de Firefox 9
	// IMPORTANTE:
	// No se puede cargar el entorno de ejecucion 'msvcr80.dll' porque requiere
	// que el EXE de carga tenga empotrado un MANIFEST adecuado
	private static final String MOZUTILS_DLL = "mozutils.dll"; //$NON-NLS-1$

	// Novedades de Firefox 11
	private static final String MOZGLUE_DLL = "mozglue.dll"; //$NON-NLS-1$

	// Firefox x
	private static final String NSS3_DLL = "nss3.dll"; //$NON-NLS-1$

	private static String appData = null;

	private MozillaKeyStoreUtilitiesWindows() {
		// No permitimos la instanciacion
	}

	/**
	 * Proporciona la ruta corta del directorio de perfil de Firefox con el formato
	 * adecuado para permitir su carga con el NSS de Windows.
	 * @param dir Directorio de NSS.
	 * @return Ruta del directorio formateada o {@code null}
	 */
	static String cleanMozillaUserProfileDirectoryWindows(final String dir) {
		final String shortName = KeyStoreUtilities.getWindowsShortName(dir);
		return shortName != null ? shortName.replace('\\', '/') : null;
	}

	static String getSystemNSSLibDirWindows() throws IOException {

		String dir = KeyStoreUtilities.getWindowsShortName(
			MozillaKeyStoreUtilities.getNssPathFromCompatibilityFile()
		);

		if (dir == null) {
			throw new FileNotFoundException("No se encuentra el directorio de NSS en Windows"); //$NON-NLS-1$
		}

		// Tenemos la ruta del NSS, comprobamos adecuacion por bugs de Java
		boolean illegalChars = false;

		for (final char c : dir.toCharArray()) {
			if (P11_CONFIG_VALID_CHARS.indexOf(c) == -1) {
				illegalChars = true;
				break;
			}
		}

		// Cuidado, el caracter "tilde" (unicode 007E) es valido para perfil de usuario pero no
		// para bibliotecas en java inferior a 6u30
		if (illegalChars) {

			LOGGER.info(
				"La ruta hacia las bibliotecas NSS contiene carcateres ilegales, se copiaran a un temporal: " + dir //$NON-NLS-1$
			);

			// Tenemos una ruta con caracteres ilegales para la configuracion de SunPKCS#11 por el bug 6581254:
			// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6581254
			try {

				// Copiamos las DLL necesarias a un temporal y devolvemos el temporal
				final File tmp;
				// Intentamos usar antes el temporal del sistema, para evitar el del usuario, que puede tener caracteres especiales
				final File tmpDir = new File(
					new File(
						Platform.getSystemLibDir()
					).getParent(),
					"Temp" //$NON-NLS-1$
				);
				if (tmpDir.isDirectory() && tmpDir.canWrite() && tmpDir.canRead()) {
					final Path tempDirPath = Paths.get(tmpDir.getCanonicalPath());
					tmp = Files.createTempDirectory(tempDirPath, "nss").toFile(); //$NON-NLS-1$
				}
				else {
					tmp = Files.createTempDirectory("nss").toFile(); //$NON-NLS-1$
				}

				// Copiamos la biblioteca de acceso y luego sus dependencias. Las dependencias las
				// recuperamos indicando cadena vacia para que nos las devuelva sin path
			    Files.copy(
			    	Paths.get(SOFTOKN3_DLL), // fichero de entrada
			    	Paths.get(tmp.getCanonicalPath()), // directorio de salida con ruta absoluta
			        StandardCopyOption.REPLACE_EXISTING
			    );

			    for (final String dependency : getSoftkn3DependenciesWindows("")) { //$NON-NLS-1$
				    Files.copy(
				    	Paths.get(dependency),// fichero de entrada
				    	Paths.get(tmp.getCanonicalPath()), // directorio de salida con ruta absoluta
				        StandardCopyOption.REPLACE_EXISTING
				    );
				}

				if(new File(tmp, SOFTOKN3_DLL).isFile()) {
					dir = tmp.getCanonicalPath();
				}

				else {
					LOGGER.warning(
						"No se ha podido duplicar NSS encontrado en '" + LoggerUtil.getCleanUserHomePath(dir) + "' a un directorio temporal" //$NON-NLS-1$ //$NON-NLS-2$
						+ "se intentara utilizar el NSS de la aplicacion '" + BundledNssHelper.getBundledNssDirectory() //$NON-NLS-1$
					);
					return BundledNssHelper.getBundledNssDirectory();
				}

			}
			catch (final Exception e) {
				LOGGER.warning(
					"No se ha podido duplicar NSS en un directorio temporal, si esta version de JRE esta afectada por " + //$NON-NLS-1$
						"el error 6581254 de Java es posible que no pueda cargarse. Se intentara utilziar el NSS de la" //$NON-NLS-1$
						+ " aplicacion '" + BundledNssHelper.getBundledNssDirectory() + "': " + e//$NON-NLS-1$ //$NON-NLS-2$
				);
				return BundledNssHelper.getBundledNssDirectory();
			}

		}

		if (dir != null) {
			final File nssP11 = new File(dir, SOFTOKN3_DLL);
			if (!nssP11.isFile()) {
				throw new FileNotFoundException(
					"No se ha encontrado un NSS en Windows para el directorio " + dir //$NON-NLS-1$
				);
			}
			if (!nssP11.canRead()) {
				throw new FileNotFoundException(
					"No se tiene permiso para leer NSS en Windows para el directorio " + dir //$NON-NLS-1$
				);
			}
			try (
				final InputStream fis = new FileInputStream(nssP11)
			) {
				final PeMachineType peArch = new MsPortableExecutable(
					AOUtil.getDataFromInputStream(fis)
				).getPeMachineType();
				final String javaArch = Platform.getJavaArch();
				if ((!peArch.equals(PeMachineType.INTEL_386) || !"32".equals(javaArch)) && (!peArch.equals(PeMachineType.X64) || !"64".equals(javaArch))) { //$NON-NLS-1$ //$NON-NLS-2$
					LOGGER.info(
						"Se usara un NSS local por ser este Java de " + javaArch + " bits y el NSS de sistema para la arquitectura " +  peArch //$NON-NLS-1$ //$NON-NLS-2$
					);
					return BundledNssHelper.getBundledNssDirectory();
				}
				LOGGER.info("Arquitectura del NSS encontrado: " + peArch); //$NON-NLS-1$
			}
			catch(final PEParserException e) {
				LOGGER.warning(
					"No se ha podido analizar la arquitectura del NSS encontrado: " + e //$NON-NLS-1$
				);
			}
			return dir;
		}

		throw new FileNotFoundException("No se ha encontrado un NSS compatible en Windows"); //$NON-NLS-1$

	}

	/** Recupera el listado de dependencias de la biblioteca "softkn3.dll" para el
	 * sistema operativo Windows. Los nombres apareceran ordenados de tal forma las
	 * bibliotecas no tengan dependencias de otra que no haya aparecido
	 * anterioremente en la lista.
	 * @param nssPath Ruta al directorio de NSS (terminado en barra).
	 * @return Listado con los nombres de las bibliotecas. */
	static String[] getSoftkn3DependenciesWindows(final String nssPath) {
		return new String[] {
			nssPath + MSVCR100_DLL,	  // Ciertas versiones, Visual C 10
			nssPath + MSVCP100_DLL,	  // Ciertas versiones, Visual C 10
			nssPath + MSVCR120_DLL,	  // Ciertas versiones, Visual C 12
			nssPath + MSVCP120_DLL,	  // Ciertas versiones, Visual C 12
			nssPath + MSVCR140_DLL,	  // Ciertas versiones, Visual C 14 (Firefox 49 y superior)
			nssPath + MSVCP140_DLL,	  // Ciertas versiones, Visual C 14 (Firefox 49 y superior)
			nssPath + MOZGLUE_DLL,    // Firefox 11
			nssPath + NSS3_DLL,       // Firefox 24
			nssPath + MOZUTILS_DLL,   // Firefox 9 y 10
			nssPath + MOZCRT19_DLL,   // Firefox desde 3 hasta 8
			nssPath + NSPR4_DLL,      // Firefox 2 y superior
			nssPath + PLDS4_DLL,      // Firefox 2 y superior
			nssPath + PLC4_DLL,       // Firefox 2 y superior
			nssPath + NSSUTIL3_DLL,   // Firefox 3 y superior
			nssPath + MOZSQLITE3_DLL, // Firefox 4 y superior
			nssPath + SQLITE3_DLL,    // Firefox 3
			nssPath + NSSDBM3_DLL,    // Firefox 3 y superior
			nssPath + FREEBL3_DLL     // Firefox 3 y superior
		};
	}

	static String getWindowsAppDataDir() {

		// Miramos primero con la variable de entorno
		if (appData == null) {
			final String ret = System.getenv("AppData"); //$NON-NLS-1$
			if (ret != null) {
				LOGGER.info(
					"Se ha comprobado la situacion del directorio 'AppData' de Windows a traves de la variable de entorno" //$NON-NLS-1$
				);
				appData = ret;
			}
		}
		if (appData != null) {
			return appData;
		}

		// Y por ultimo con el directorio por defecto de Windows 7 y Windows 8
		final String probablyPath = "C:\\Users\\" + System.getProperty("user.name") + "\\AppData\\Roaming"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		final File f = new File(probablyPath);
		if (f.isDirectory()) {
			appData = probablyPath;
			LOGGER.info(
				"Se ha comprobado la situacion del directorio 'AppData' de Windows manualmente" //$NON-NLS-1$
			);
			return appData;
		}

		appData = null;
		throw new IllegalStateException("No se ha podido determinar la situacion del directorio 'AppData' de Windows"); //$NON-NLS-1$

	}
}
