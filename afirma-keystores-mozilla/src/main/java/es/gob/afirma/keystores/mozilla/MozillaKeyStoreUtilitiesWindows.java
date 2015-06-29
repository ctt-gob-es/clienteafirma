package es.gob.afirma.keystores.mozilla;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;

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



	private MozillaKeyStoreUtilitiesWindows() {
		// No permitimos la instanciacion
	}

	/** Obtiene el nombre corto (8+3) de un fichero o directorio indicado (con ruta).
	 * @param originalPath Ruta completa hacia el fichero o directorio que queremos pasar a nombre corto.
	 * @return Nombre corto del fichero o directorio con su ruta completa, o la cadena originalmente indicada si no puede
	 *         obtenerse la versi&oacute;n corta */
	static String getShort(final String originalPath) {
		if (originalPath == null || !Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return originalPath;
		}
		final File dir = new File(originalPath);
		if (!dir.exists()) {
			return originalPath;
		}
		try {
			final Process p = new ProcessBuilder(
					"cmd.exe", "/c", "for %f in (\"" + originalPath + "\") do @echo %~sf" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			).start();
			return new String(AOUtil.getDataFromInputStream(p.getInputStream())).trim();
		}
		catch(final Exception e) {
			LOGGER.warning("No se ha podido obtener el nombre corto de " + originalPath + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return originalPath;
	}

	static String cleanMozillaUserProfileDirectoryWindows(final String dir) {
		return getShort(dir).replace('\\', '/');
	}

	static String getSystemNSSLibDirWindows() throws IOException {

		String dir = MozillaKeyStoreUtilities.getNssPathFromCompatibilityFile();

		if (dir == null) {
			throw new FileNotFoundException("No se encuentra el dierctorio de NSS en Windows"); //$NON-NLS-1$
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

			// Tenemos una ruta con caracteres ilegales para la
			// configuracion de SunPKCS#11 por el bug 6581254:
			// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6581254
			try {

				// Copiamos las DLL necesarias a un temporal y devolvemos el temporal
				final File tmp;
				// Intentamos usar antes el temporal del sistema, para evitar el del usuario, que puede tener caracteres especiales
				final File tmpDir = new File(new File(Platform.getSystemLibDir()).getParent(), "Temp"); //$NON-NLS-1$
				if (tmpDir.isDirectory() && tmpDir.canWrite() && tmpDir.canRead()) {
					tmp = File.createTempFile("nss", null, tmpDir); //$NON-NLS-1$
				}
				else {
					tmp = File.createTempFile("nss", null); //$NON-NLS-1$
				}
				tmp.delete();
				if (!tmp.mkdir()) {
					throw new AOException("No se ha creado el directorio temporal"); //$NON-NLS-1$
				}

				// Copiamos la biblioteca de acceso y luego sus dependencias. Las dependencias las
				// recuperamos indicando cadena vacia para que nos las devuelva sin path
				copyFile(new String[] { SOFTOKN3_DLL }, dir, tmp.getCanonicalPath());
				copyFile(getSoftkn3DependenciesWindows(""), dir, tmp.getCanonicalPath()); //$NON-NLS-1$

				dir = tmp.getCanonicalPath();

			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido duplicar NSS en un directorio temporal, si esta version de JRE esta afectada por el error 6581254 de Java es posible que no pueda cargarse: " + e); //$NON-NLS-1$
			}

		}

		if (dir != null) {
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

	private static String appData = null;

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
		if (f.exists() && f.isDirectory()) {
			appData = probablyPath;
			LOGGER.info(
				"Se ha comprobado la situacion del directorio 'AppData' de Windows manualmente" //$NON-NLS-1$
			);
			return appData;
		}

		appData = null;
		throw new IllegalStateException("No se ha podido determinar la situacion del directorio 'AppData' de Windows"); //$NON-NLS-1$

	}

	/** Copia ficheros de un directorio a otro, ignorando los ficheros que no existan.
	 * @param fileNames Nombres de los ficheros a copiar
	 * @param sourceDir Directorio de origen, no debe tener la barra al final
	 * @param destDir Directorio de destino, debe tener la barra al final */
	private static void copyFile(final String[] fileNames, final String sourceDir, final String destDir) {
		if (fileNames !=null) {
			File tmpFile;
			for(final String f : fileNames) {
				tmpFile = new File(sourceDir, f);
				if (tmpFile.exists()) {
					try {
						AOUtil.copyFile(tmpFile, new File(destDir, f));
					}
					catch (final IOException e) {
						LOGGER.warning("No se ha podido copiar '" + f + "' a '" + destDir + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					}
				}
			}
		}
	}

}
