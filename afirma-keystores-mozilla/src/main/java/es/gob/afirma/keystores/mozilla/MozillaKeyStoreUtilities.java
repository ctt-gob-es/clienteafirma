/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.Provider;
import java.security.Security;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.mozilla.AOSecMod.ModuleName;
import es.gob.afirma.keystores.mozilla.shared.SharedNssUtil;

/** Clase con m&eacute;toos de utilidad para la gesti&oacute;n del almac&eacute;n
 * de certificados de Mozilla. */
public final class MozillaKeyStoreUtilities {

	private static final String SUN_PKCS11_PROVIDER_CLASSNAME = "sun.security.pkcs11.SunPKCS11"; //$NON-NLS-1$

	private static final String LIB_NSPR4_SO = "/lib/libnspr4.so"; //$NON-NLS-1$

	private static final String SOFTOKN3_SO = "libsoftokn3.so"; //$NON-NLS-1$

	/** Nombre del PKCS#11 NSS en Windows. */
	private static final String SOFTOKN3_DLL = "softokn3.dll"; //$NON-NLS-1$

	/** Nombre del fichero que declara los m&oacute;dulos de NSS en sustituci&oacute;n a 'secmod.db'. */
	private static final String PKCS11TXT_FILENAME = "pkcs11.txt"; //$NON-NLS-1$

	private static final String AFIRMA_NSS_HOME_ENV = "AFIRMA_NSS_HOME_ENV"; //$NON-NLS-1$

	private static final String AFIRMA_NSS_PROFILES_INI = "AFIRMA_NSS_PROFILES_INI"; //$NON-NLS-1$

	private static final String USE_ENV_VARS = "es.gob.afirma.keystores.mozilla.UseEnvironmentVariables"; //$NON-NLS-1$

	/** Variable (Java) que, cuando se establece a <code>true</code> hace que NSS se abra habilitando la escritura.
	 * Si no se esteblece o se hace con un valor distinto a <code>true</code>, NSS se abre como solo lectura. */
	public static final String ENABLE_NSS_WRITE = "es.gob.afirma.keystores.mozilla.EnableNssWrite"; //$NON-NLS-1$

	/** Nombres del controlador nativo de DNIe en sistemas no-Linux (Windows, OS X, etc.). */
	private static final String[] DNI_P11_NAMES = {
		"libopensc-dnie.dylib", //$NON-NLS-1$
		"libpkcs11-dnie.so", //$NON-NLS-1$
		"usrpkcs11.dll", //$NON-NLS-1$
		"dnie_p11_priv.dll", //$NON-NLS-1$
		"dnie_p11_pub.dll", //$NON-NLS-1$
		"opensc-pkcs11.dll", //$NON-NLS-1$
		"DNIE_P11.dll", //$NON-NLS-1$
		"TIF_P11.dll"//$NON-NLS-1$
	};

	/** M&oacute;dulos de los que se conoce el nombre de su biblioteca para que se agregue
	 * junto al listado de m&oacute;dulos configurados en Mozilla Firefox para intentar
	 * cargarlos. Las propiedades establecidas para cada uno de ellos son:
	 * <ul>
	 *  <li>Nombre descriptivo de la biblioteca.</li>
	 *  <li>Nombre del fichero de la biblioteca.</li>
	 *  <li>Si se desea o no que se intente su carga aunque no se encuentre su blioteca en
	 *      el sistema.</li>
	 * </ul>
	 * Este &uacute;ltimo par&aacute;metro es necesario para la carga de las bibliotecas de la FNMT
	 * ya que Java nos oculta el nombre de fichero en el directorio de 64 bits, pero lo necesita para
	 * la carga. */
	private enum KnownModule {
		ATOS_CARDOS("Atos CardOS (preinstalado)", "siecap11.dll", Platform.OS.WINDOWS, false), //$NON-NLS-1$ //$NON-NLS-2$
		FNMT_64("FNMT-RCM Modulo PKCS#11 64bits", "FNMT_P11_x64.dll", Platform.OS.WINDOWS, true), //$NON-NLS-1$ //$NON-NLS-2$
		FNMT_32("FNMT-RCM Modulo PKCS#11 32bits", "FNMT_P11.dll", Platform.OS.WINDOWS, true); //$NON-NLS-1$ //$NON-NLS-2$

		private String description;
		private String lib;
		private Platform.OS os;
		private boolean forcedLoad;
		KnownModule(final String description, final String lib, final Platform.OS os, final boolean forcedLoad) {
			this.description = description;
			this.lib = lib;
			this.forcedLoad = forcedLoad;
			this.os = os;
		}

		String getDescription() {
			return this.description;
		}
		String getLib() {
			return this.lib;
		}
		boolean isForcedLoad() {
			return this.forcedLoad;
		}
		public Platform.OS getOs() {
			return this.os;
		}
	}

	private MozillaKeyStoreUtilities() {
		// No permitimos la instanciacion
	}

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Directorio con las bibliotecas de NSS necesarias para el acceso al
	 * almac&eacute;n de Mozilla. */
	private static String nssLibDir = null;

	/** Crea las l&iacute;neas de configuraci&oacute;n para el uso de las
	 * bibliotecas NSS como m&oacute;dulo PKCS#11 por el proveedor de Sun.
	 * @param userProfileDirectory Directorio donde se encuentra el perfil de
	 *                             usuario de Mozilla Firefox.
	 * @param libDir Directorio que contiene las bibliotecas NSS.
	 * @return Propiedades de configuracion del proveedor
	 *         PKCS#11 de Sun para acceder al KeyStore de Mozilla v&iacute;a NSS. */
	public static String createPKCS11NSSConfig(final String userProfileDirectory, final String libDir) {

		final String softoknLib;
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			softoknLib = SOFTOKN3_DLL;
		}
		else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			softoknLib = "libsoftokn3.dylib"; //$NON-NLS-1$
		}
		else {
			softoknLib = SOFTOKN3_SO;
		}

		final StringBuilder buffer = new StringBuilder("name=NSSCrypto-AFirma\r\n"); //$NON-NLS-1$

		// Java 1.5 tenia un metodo indocumentado para acceder a NSS,
		// http://docs.sun.com/app/docs/doc/819-3671/gcsoc?a=view

		buffer.append("library=") //$NON-NLS-1$
			.append(libDir)
			.append(java.io.File.separator)
			.append(softoknLib)
			.append("\n") //$NON-NLS-1$
			.append("attributes=compatibility\n") //$NON-NLS-1$
			.append("slot=2\n") //$NON-NLS-1$
			.append("showInfo=false\n") //$NON-NLS-1$
			.append("allowSingleThreadedModules=true\n") //$NON-NLS-1$
			.append("nssArgs=\"") //$NON-NLS-1$
			.append("configdir='") //$NON-NLS-1$
			.append(userProfileDirectory)
			.append("' ") //$NON-NLS-1$
			.append("certPrefix='' ") //$NON-NLS-1$
			.append("keyPrefix='' ") //$NON-NLS-1$
			.append(Boolean.getBoolean(ENABLE_NSS_WRITE) ? "" : "flags='readOnly'") //$NON-NLS-1$ //$NON-NLS-2$
			.append("\""); //$NON-NLS-1$

		return buffer.toString();
	}

	static String getNssPathFromCompatibilityFile() throws IOException {
		final File compatibility = new File(
			getMozillaUserProfileDirectory(),
			"compatibility.ini"  //$NON-NLS-1$
		);
		String dir = null;
		if (compatibility.exists() && compatibility.canRead()) {
			try (
				final InputStream fis = new FileInputStream(compatibility);
				// Cargamos el fichero con la codificacion por defecto (que es la que con mas probabilidad tiene el fichero)
				final BufferedReader br = new BoundedBufferedReader(
					new InputStreamReader(fis),
					512, // Maximo 512 lineas
					4096 // Maximo 4KB por linea
				)
			) {
				String line;
				while ((line = br.readLine()) != null && dir == null) {
				    if (line.startsWith("LastPlatformDir=")) { //$NON-NLS-1$
				    	dir = line.replace("LastPlatformDir=", "").trim(); //$NON-NLS-1$ //$NON-NLS-2$
				    }
				}
			}
		}
		if (dir == null) {
			throw new FileNotFoundException(
				"No se ha podido determinar el directorio de NSS en Windows a partir de 'compatibility.ini' de Firefox" //$NON-NLS-1$
			);
		}
		return dir;
	}

	/** Obtiene el directorio de las bibliotecas NSS (<i>Netscape Security
	 * Services</i>) del sistema.
	 * @return Directorio de las bibliotecas NSS del sistema.
	 * @throws FileNotFoundException Si no se puede encontrar NSS en el sistema.
     * @throws IOException En caso de errores de lectura/escritura. */
	public static String getSystemNSSLibDir() throws IOException {

		if (nssLibDir != null) {
			return nssLibDir;
		}

		// Primero probamos con la variable de entorno, que es comun a todos los sistemas operativos
		if (Boolean.getBoolean(USE_ENV_VARS)) {
			try {
				nssLibDir = System.getenv(AFIRMA_NSS_HOME_ENV);
			}
			catch(final Exception e) {
				LOGGER.warning(
					"No se tiene acceso a la variable de entorno '" + AFIRMA_NSS_HOME_ENV + "': " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
			if (nssLibDir != null) {
				final File nssDir = new File(nssLibDir);
				if (nssDir.isDirectory() && nssDir.canRead()) {
					LOGGER.info(
						"Directorio de NSS determinado a partir de la variable de entorno '" + AFIRMA_NSS_HOME_ENV + "'" //$NON-NLS-1$ //$NON-NLS-2$
					);
				}
				else {
					LOGGER.warning(
						"La variable de entorno '" + AFIRMA_NSS_HOME_ENV + "' apunta a un directorio que no existe o sobre el que no se tienen permisos de lectura, se ignorara" //$NON-NLS-1$ //$NON-NLS-2$
					);
					nssLibDir = null;
				}
			}
		}

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			nssLibDir = MozillaKeyStoreUtilitiesWindows.getSystemNSSLibDirWindows();
		}

		else if (Platform.OS.LINUX.equals(Platform.getOS()) || Platform.OS.SOLARIS.equals(Platform.getOS())) {
			nssLibDir = MozillaKeyStoreUtilitiesUnix.getNSSLibDirUnix();
		}

		else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			nssLibDir = MozillaKeyStoreUtilitiesOsX.getSystemNSSLibDirMacOsX();
		}

		if (nssLibDir == null) {
			throw new FileNotFoundException(
				"No se han encontrado bibliotecas NSS instaladas en su sistema operativo" //$NON-NLS-1$
			);
		}

		return nssLibDir;
	}

	/** Obtiene las rutas completas hacia las bibliotecas (.dll o .so) de los
	 * m&oacute;dulos de seguridad externos (PKCS#11) instalados en Mozilla /
	 * Firefox, indexados por su descripci&oacute;n dentro de un <code>ConcurrentHashMap</code>.
	 * <b>ADVERTENCIA:</b> Los PKCS#11 de DNIe se excluyen siempre de este listado.
	 * @param excludePreferredModules Si se establece a <code>true</code> excluye los m&oacute;dulos PKCS#11
	 *                    del DNIe y CERES, si se establece a <code>false</code> deja estos m&oacute;dulos en
	 *                    caso de que se encontrasen.
	 * @param includeKnownModules Si se establece a <code>true</code> se incluyen m&oacute;dulos PKCS#11 que
	 *                            est&eacute;n en el directorio de bibliotecas del sistema pero no en la
	 *                            base de datos de m&oacute;dulos de Mozilla (<i>secmod.db</i>), si se
	 *                            establece a <code>false</code> se devuelven &uacute;nicamente los
	 *                            m&oacute;dulos PKCS#11 de la base de datos.
	 * @return Nombres de las bibliotecas de los m&oacute;dulos de seguridad de Mozilla NSS / Firefox. */
	static Map<String, String> getMozillaPKCS11Modules(final boolean excludePreferredModules,
			                                           final boolean includeKnownModules) {
		if (!excludePreferredModules) {
			LOGGER.info("Se incluiran los modulos nativos de DNIe/CERES si se encuentran configurados"); //$NON-NLS-1$
		}
		else {
			LOGGER.info("Se excluiran los modulos nativos de DNIe/CERES en favor del controlador 100% Java"); //$NON-NLS-1$
		}

		final String profileDir;
		try {
			profileDir = getMozillaUserProfileDirectory();
		}
		catch (final IOException e) {
			LOGGER.severe(
				"No se ha podido obtener el directorio de perfil de Mozilla para leer la lista de modulos PKCS#11: " + e //$NON-NLS-1$
			);
			return new ConcurrentHashMap<>(0);
		}

		// Comprobamos si tenemos que usar pkcs11.txt o secmod.db
		final File pkcs11Txt = new File(profileDir, PKCS11TXT_FILENAME);
		if ("sql".equals(System.getenv("NSS_DEFAULT_DB_TYPE")) || pkcs11Txt.exists()) { //$NON-NLS-1$ //$NON-NLS-2$
			try {
				final List<ModuleName> modules = Pkcs11Txt.getModules(pkcs11Txt);
				LOGGER.info("Obtenidos los modulos externos de Mozilla desde 'pkcs11.txt'"); //$NON-NLS-1$
				return getPkcs11ModulesFromModuleNames(
					modules,
					includeKnownModules,
					excludePreferredModules
				);
			}
			catch (final IOException e) {
				LOGGER.severe(
					"No se han podido obtener los modulos externos de Mozilla desde 'pkcs11.txt': " + e //$NON-NLS-1$
				);
			}
		}
		// Si NSS_DEFAULT_DB_TYPE != "sql", no existe 'pkcs11.txt' o ha fallado la obtencion de modulos
		// desde 'pkcs11.txt', se usa 'secmod.db'.
		try {
			final List<ModuleName> modules =  AOSecMod.getModules(profileDir);
			LOGGER.info("Obtenidos los modulos externos de Mozilla desde 'secmod.db'"); //$NON-NLS-1$
			return getPkcs11ModulesFromModuleNames(
				modules,
				includeKnownModules,
				excludePreferredModules
			);
		}
		catch (final Exception t) {
			LOGGER.severe(
				"No se han podido obtener los modulos externos de Mozilla desde 'secmod.db': " + t //$NON-NLS-1$
			);
			return new ConcurrentHashMap<>(0);
		}


	}

	/** Obtiene los m&oacute;dulos PKCS#11 a partir de sus descripciones.
	 * @param modules Descripci&oacute;n de los m&oacute;dulos PKCS#11 de NSS.
	 * @param includeKnownModules <code>true</code> si se desea incluir m&oacute;dulos PKCS#11 que comunmente est&aacute;n
	 *                            instalados en un sistema (y solo si realmente lo est&aacute;s) aunque no est&eacute;n
	 *                            en la lista de descripciones proporcionada, <code>false</code> en caso contrario.
	 * @param excludePreferredModules <code>true</code> si se desea excluir los m&oacute;dulos PKCS#11 de DNIe y
	 *								CERES aunque est&eacute;n en la lista de descripciones proporcionada,
	 *								<code>false</code> en caso contrario.
	 * @return M&oacute;dulos PKCS#11. */
	public static Map<String, String> getPkcs11ModulesFromModuleNames(final List<ModuleName> modules,
			                                                          final boolean includeKnownModules,
			                                                          final boolean excludePreferredModules) {

		if (modules == null) {
			return new ConcurrentHashMap<>(0);
		}

		final Map<String, String> modsByDesc = new ConcurrentHashMap<>();

		for (final AOSecMod.ModuleName module : modules) {
			final String moduleLib =  module.getLib();
			if (excludePreferredModules && isDniePkcs11Library(moduleLib)) {
				continue;
			}
			modsByDesc.put(module.getDescription(), moduleLib);
		}

		// Creamos una copia de modsByDesc para evitar problemas de concurrencia
		// (nunca soltara excepciones por usar ConcurrentHashMap, pero no significa
		// que los problemas no ocurran si no se toman medidas).
		final ConcurrentHashMap<String, String> modsByDescCopy = new ConcurrentHashMap<>(modsByDesc);
		// Incluimos si aplica los modulos conocidos (aquellos de los que sin estar configurados sabemos
		// el nombre) y los agregamos. No lo hacemos cuando se trate de una biblioteca preferida y se haya
		// indicado que se excluyan
		if (includeKnownModules) {
			for (final KnownModule knownModule : KnownModule.values()) {
				// Si el modulo no se corresponde con el sistema actual, se omite
				if (!Platform.getOS().equals(knownModule.getOs())) {
					continue;
				}
				if (excludePreferredModules && isDniePkcs11Library(knownModule.getLib())) {
					continue;
				}
				if (!isModuleIncluded(modsByDescCopy, knownModule.getLib())) {
					final String modulePath = getWindowsSystemDirWithFinalSlash() + knownModule.getLib();
					if (knownModule.isForcedLoad() || new File(modulePath).exists()) {
						modsByDesc.put(knownModule.getDescription(), modulePath);
					}
				}
			}
		}

		return purgeStoresTable(modsByDesc);

	}

	/** Carga las dependencias de la biblioteca "softokn3" necesaria para acceder
	 * al almac&eacute;n de certificados. Hacemos esto por precauci&oacute;n ya
	 * que esto se har&iacute;a autom&aacute;ticamente si las dependencias
	 * estuviesen en el PATH del sistema.
	 * @param nssDirectory Directorio en donde se encuentran las bibliotecas de NSS. */
	public static void loadNSSDependencies(final String nssDirectory) {

		final String[] dependList;

		// Comprobamos despues el caso especifico de NSS partido entre /usr/lib y
		// /lib, que se da en versiones de Fedora muy antiguas y solo en 32 bits
		if (Platform.OS.LINUX.equals(Platform.getOS())
                && "32".equals(Platform.getJavaArch()) //$NON-NLS-1$
                && new File("/usr/lib/" + SOFTOKN3_SO).exists() //$NON-NLS-1$
                && new File(LIB_NSPR4_SO).exists()) {
			dependList = new String[] {
				"/lib/libmozglue.so", //$NON-NLS-1$
				"/usr/lib/libmozglue.so", //$NON-NLS-1$
				LIB_NSPR4_SO,
				"/lib/libplds4.so", //$NON-NLS-1$
				"/usr/lib/libplds4.so", //$NON-NLS-1$
				"/lib/libplc4.so", //$NON-NLS-1$
				"/usr/lib/libplc4.so", //$NON-NLS-1$
				"/lib/libnssutil3.so", //$NON-NLS-1$
				"/usr/lib/libnssutil3.so", //$NON-NLS-1$
				"/lib/libsqlite3.so", //$NON-NLS-1$
				"/usr/lib/libsqlite3.so", //$NON-NLS-1$
				"/lib/libmozsqlite3.so", //$NON-NLS-1$
				"/usr/lib/libmozsqlite3.so" //$NON-NLS-1$
			};
			LOGGER.info("Detectada configuracion de NSS mixta entre '/usr/lib' y '/lib'"); //$NON-NLS-1$
		}
		else {
			final String path = nssDirectory + (nssDirectory.endsWith(File.separator) ? "" : File.separator); //$NON-NLS-1$
			dependList = getSoftkn3Dependencies(path);
		}

		for (final String libPath : dependList) {
			if (new File(libPath).exists()) {
				try {
					System.load(libPath);
				}
				catch (final Exception | Error e) {
					LOGGER.log(
						Level.WARNING,
						"Error al cargar la biblioteca " + LoggerUtil.getCleanUserHomePath(libPath) + " para el acceso al almacen de claves de Mozilla: " + e, //$NON-NLS-1$ //$NON-NLS-2$
						e
					);
				}
			}
		}
	}

	/** Recupera el listado de dependencias de la biblioteca "softkn3" para el
	 * sistema operativo en el que se est&aacute; ejecutando la
	 * aplicaci&oacute;n. Los nombres apareceran ordenados de tal forma las
	 * bibliotecas no tengan dependencias de otra que no haya aparecido
	 * anterioremente en la lista.
	 * @param path Ruta al directorio de NSS (terminado en barra).
	 * @return Listado con los nombres de las bibliotecas. */
	private static String[] getSoftkn3Dependencies(final String path) {

		if (path == null) {
			return new String[0];
		}

		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			// En Mac OS X no funciona la precarga de bibliotecas
			return new String[0];
		}

		final String nssPath = !path.endsWith(File.separator) ? path + File.separator : path;

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return MozillaKeyStoreUtilitiesWindows.getSoftkn3DependenciesWindows(nssPath);
		}
		if (Platform.OS.LINUX.equals(Platform.getOS()) || Platform.OS.SOLARIS.equals(Platform.getOS())) {
			return MozillaKeyStoreUtilitiesUnix.getSoftkn3DependenciesUnix(nssPath);
		}

		LOGGER.warning(
				"Plataforma no soportada para la precarga de las bibliotecas NSS: " + Platform.getOS() //$NON-NLS-1$
		);
		return new String[0];
	}

	private static String getProfilesIniPath() {
		String profilesIniPath = null;
		// Miramos primero la variable de entorno 'AFIRMA_PROFILES_INI'
		if (Boolean.getBoolean(USE_ENV_VARS) ||
			Boolean.parseBoolean(System.getenv(USE_ENV_VARS))) {
			try {
				profilesIniPath = System.getenv(AFIRMA_NSS_PROFILES_INI);
				if (profilesIniPath == null) {
					profilesIniPath = System.getProperty(AFIRMA_NSS_PROFILES_INI);
				}
			}
			catch(final Exception e) {
				LOGGER.warning("No se tiene acceso a la variable de entorno '" + AFIRMA_NSS_PROFILES_INI + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (profilesIniPath != null) {

				// Se expande %AppData% en Windows
				final String addDataUpperCase = "%APPDATA%"; //$NON-NLS-1$
				final int appDataIndex = profilesIniPath.toUpperCase().indexOf(addDataUpperCase);
				if (appDataIndex != -1) {
					profilesIniPath = profilesIniPath.replace(
						profilesIniPath.substring(appDataIndex, appDataIndex + addDataUpperCase.length()),
						MozillaKeyStoreUtilitiesWindows.getWindowsAppDataDir()
					);
				}

				final File profilesIniFile = new File(profilesIniPath);
				if (profilesIniFile.isFile() && profilesIniFile.canRead()) {
					LOGGER.info(
						"Fichero de perfiles de Firefox determinado a partir de la variable de entorno '" + AFIRMA_NSS_PROFILES_INI + "'" //$NON-NLS-1$ //$NON-NLS-2$
					);
					return profilesIniPath;
				}
				LOGGER.warning(
					"La variable de entorno '" + AFIRMA_NSS_PROFILES_INI + "' apunta a un fichero que no existe o sobre el que no se tienen permisos de lectura, se ignorara: " + profilesIniPath //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return MozillaKeyStoreUtilitiesWindows.getWindowsAppDataDir() + "\\Mozilla\\Firefox\\profiles.ini"; //$NON-NLS-1$
		}
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			return Platform.getUserHome() + "/Library/Application Support/Firefox/profiles.ini"; //$NON-NLS-1$
		}
		// Linux / UNIX
		if (new File(Platform.getUserHome() + "/snap/firefox/common/.mozilla/firefox/profiles.ini").isFile()) { //$NON-NLS-1$
			return Platform.getUserHome() + "/snap/firefox/common/.mozilla/firefox/profiles.ini"; //$NON-NLS-1$
		}
		return Platform.getUserHome() + "/.mozilla/firefox/profiles.ini"; //$NON-NLS-1$
	}

	/**
	 * Obtiene el directorio del perfil de usuario de Mozilla / Firefox.
	 * @return Ruta completa del directorio del perfil de usuario de Mozilla / Firefox
	 * @throws IOException Cuando no se ha podido identificar el directorio de perfil.
	 */
	public static String getMozillaUserProfileDirectory() throws IOException {
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return getMozillaUserProfileDirectoryWindows(
				getProfilesIniPath()
			);
		}
		// En otros sistemas operativos, cuando falla la obtencion de PROFILES.INI se intenta
		// usar el directorio global de NSS (para tratar ciertas configuraciones de NSS en las
		// que ni siquiera existe el directorio de perfil de Firefox)
		try {
			return NSPreferences.getFireFoxUserProfileDirectory(
				new File(getProfilesIniPath())
			);
		}
		catch(final Exception e) {
			LOGGER.log(
				Level.SEVERE,
				"No ha podido determinarse el perfil de usuario de Mozilla, se intentara usar el global: " + e, //$NON-NLS-1$
				e
			);
		}
		return SharedNssUtil.getSharedUserProfileDirectory();
	}

	/**
	 * Obtiene el directorio del perfil de usuario de Mozilla / Firefox.
	 * @param iniPath Ruta al fichero de perfiles de Firefox.
	 * @return Ruta completa del directorio del perfil de usuario de Mozilla / Firefox.
	 * @throws IOException Cuando no se ha podido identificar el directorio de perfil.
	 */
	public static String getMozillaUserProfileDirectoryWindows(final String iniPath) throws IOException {
		final String dir = NSPreferences.getFireFoxUserProfileDirectory(new File(iniPath));
		if (dir == null) {
			throw new IOException("No se ha encontrado el directorio de perfil de Mozilla"); //$NON-NLS-1$
		}

		return MozillaKeyStoreUtilitiesWindows.cleanMozillaUserProfileDirectoryWindows(dir);
	}

	private static Provider loadNssJava9(final String nssDirectory, final String p11NSSConfigFileContents)
				throws IOException, AOException {

		final Provider p = Security.getProvider("SunPKCS11"); //$NON-NLS-1$
		final File f = File.createTempFile("pkcs11_nss_", ".cfg");  //$NON-NLS-1$//$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(f)
		) {
			fos.write(p11NSSConfigFileContents.getBytes());
		}

		Provider ret;
		try {
			final Method configureMethod = Provider.class.getMethod("configure", String.class); //$NON-NLS-1$
			ret = (Provider) configureMethod.invoke(p, f.getAbsolutePath());
		}
		catch (final Exception | Error e) {

			// No se ha podido cargar el proveedor sin precargar las dependencias
			// Cargamos las dependencias necesarias para la correcta carga
			// del almacen (en Mac se crean enlaces simbolicos)

			LOGGER.warning("NSS no se ha podido iniciar sin precargar sus dependencias: " + e); //$NON-NLS-1$

			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				MozillaKeyStoreUtilitiesOsX.configureMacNSS(nssDirectory);
			}
			else {
				MozillaKeyStoreUtilities.loadNSSDependencies(nssDirectory);
			}
			try {
				final Method configureMethod = Provider.class.getMethod("configure", String.class); //$NON-NLS-1$
				ret = (Provider) configureMethod.invoke(p, f.getAbsolutePath());
			}
			catch (final Exception ex) {

				LOGGER.info(
					"No se ha podido cargar NSS en modo SQLite (con prefijo 'sql:/'), se intentara en modo Berkeley (sin prefijo 'sql:/'): " + ex //$NON-NLS-1$
				);

				// Realizamos un ultimo intento configurando NSS para que utilice la base de
				// datos Berkeley en lugar de SQLite
				try (
					final OutputStream fos = new FileOutputStream(f)
				) {
					fos.write(p11NSSConfigFileContents.replace("sql:/", "").getBytes()); //$NON-NLS-1$ //$NON-NLS-2$
				}

				try {
					final Method configureMethod = Provider.class.getMethod("configure", String.class); //$NON-NLS-1$
					ret = (Provider) configureMethod.invoke(p, f.getAbsolutePath());
				}
				catch (final Exception ex2) {
					throw new AOException("Ocurrio un error al configurar el proveedor de acceso a NSS", ex2); //$NON-NLS-1$
				}
			}
		}
		if(!f.delete()) {
			LOGGER.warning("No se ha podido eliminar el fichero '" + LoggerUtil.getCleanUserHomePath(f.getAbsolutePath())  + "'"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return ret;
	}

	private static Provider loadNssJava8(final String nssDirectory,
			                             final String p11NSSConfigFileContents) throws AOException,
	                                                                                   InstantiationException,
	                                                                                   IllegalAccessException,
	                                                                                   IllegalArgumentException,
	                                                                                   InvocationTargetException,
	                                                                                   NoSuchMethodException,
	                                                                                   SecurityException,
	                                                                                   ClassNotFoundException {
		try {
			return (Provider) Class.forName(SUN_PKCS11_PROVIDER_CLASSNAME)
				.getConstructor(InputStream.class)
					.newInstance(new ByteArrayInputStream(p11NSSConfigFileContents.getBytes()));
		}
		catch (final Exception e) {

			// No se ha podido cargar el proveedor sin precargar las dependencias
			// Cargamos las dependencias necesarias para la correcta carga
			// del almacen (en Mac se crean enlaces simbolicos)

			LOGGER.info("NSS necesita una precarga o tratamiento de sus dependencias: " + e); //$NON-NLS-1$

			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				MozillaKeyStoreUtilitiesOsX.configureMacNSS(nssDirectory);
			}
			else {
				MozillaKeyStoreUtilities.loadNSSDependencies(nssDirectory);
			}

			try {
				return (Provider) Class.forName(SUN_PKCS11_PROVIDER_CLASSNAME)
					.getConstructor(InputStream.class)
						.newInstance(new ByteArrayInputStream(p11NSSConfigFileContents.getBytes()));
			}
			catch (final Exception e2) {
				LOGGER.warning("Ha fallado el segundo intento de carga de NSS: " + e2); //$NON-NLS-1$

				// Un ultimo intento de cargar el proveedor valiendonos de que es posible que
				// las bibliotecas necesarias se hayan cargado tras el ultimo intento.
				//
				// Si los anteriores intentos se hubiesen hecho en modo compartido (SHARED), en este
				// ultimo intento se hace de forma normal. Esto resuelve el problema que se da por
				// la instalacion de G&D SafeSign, que configura el modo SHARED aunque no se use, lo
				// que lleva a engano al mecanismo de deteccion.

				return (Provider) Class.forName(SUN_PKCS11_PROVIDER_CLASSNAME).getConstructor(
					InputStream.class
				).newInstance(
					new ByteArrayInputStream(
						p11NSSConfigFileContents.replace("sql:/", "").getBytes() //$NON-NLS-1$ //$NON-NLS-2$
					)
				);
			}
		}
	}

	static Provider loadNSS(final boolean useSharedNss) throws IOException,
	                                                           AOException,
	                                                           InstantiationException,
	                                                           IllegalAccessException,
	                                                           IllegalArgumentException,
	                                                           InvocationTargetException,
	                                                           NoSuchMethodException,
	                                                           SecurityException,
	                                                           ClassNotFoundException {

		final String nssDirectory = MozillaKeyStoreUtilities.getSystemNSSLibDir();

		LOGGER.info("Directorio de bibliotecas NSS: " + nssDirectory); //$NON-NLS-1$

		String profileDir = useSharedNss ?
			SharedNssUtil.getSharedUserProfileDirectory() :
				MozillaKeyStoreUtilities.getMozillaUserProfileDirectory();

		// Consideramos que se debe cargar el fichero de modulos de NSS en modo de base de datos
		// cuando se encuentra la variable de sistema NSS_DEFAULT_DB_TYPE o se encuentra el fichero pkcs11.txt
		try {
			if ("sql".equals(System.getenv("NSS_DEFAULT_DB_TYPE")) || new File(profileDir, PKCS11TXT_FILENAME).exists()) { //$NON-NLS-1$ //$NON-NLS-2$
				profileDir = "sql:/" + profileDir; //$NON-NLS-1$
			}
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo comprobar si el almacen de claves debia cargase como base de datos: " + e); //$NON-NLS-1$
		}

		final String p11NSSConfig = MozillaKeyStoreUtilities.createPKCS11NSSConfig(
			profileDir,
			nssDirectory
		);

		// Mostramos la configuracion de NSS, sustituyendo la ruta del usuario del
		// registro para evitar mostrar datos personales en el log
		LOGGER.info("Configuracion de NSS para SunPKCS11:\n" + p11NSSConfig.replace(Platform.getUserHome(), "USERHOME")); //$NON-NLS-1$ //$NON-NLS-2$

		final Provider p = AOUtil.isJava9orNewer()
				? loadNssJava9(nssDirectory, p11NSSConfig)
				: loadNssJava8(nssDirectory, p11NSSConfig);

		Security.addProvider(p);

		LOGGER.info(
			"Anadido proveedor PKCS#11 de NSS " + (useSharedNss ? "del sistema" : "de Mozilla") + ": " + p.getName() //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		);
		return p;
	}

	private static boolean isDniePkcs11Library(final String driverName) {
		if (driverName == null || Boolean.getBoolean("es.gob.afirma.keystores.mozilla.disableDnieNativeDriver")) { //$NON-NLS-1$
			return false;
		}
		for (final String libName : DNI_P11_NAMES) {
			if (driverName.toLowerCase().endsWith(libName.toLowerCase())) {
				return true;
			}
		}
		return false;
	}

	private static String getWindowsSystemDirWithFinalSlash() {
		// En sistema operativo extrano devulevo cadena vacia
		if (!Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return ""; //$NON-NLS-1$
		}
		return Platform.getSystemLibDir() + "\\"; //$NON-NLS-1$
	}

	private static boolean isModuleIncluded(final Map<String, String> externalStores, final String moduleName) {
		if (externalStores == null || moduleName == null) {
			throw new IllegalArgumentException(
				"Ni la lista de almacenes ni el modulo a comprobar pueden ser nulos" //$NON-NLS-1$
			);
		}
		for (final String key : externalStores.keySet()) {
			if (externalStores.get(key).toLowerCase().endsWith(moduleName.toLowerCase())) {
				return true;
			}
		}
		return false;
	}

	/** Dada una tabla que indexa por descripci&oacute;n los m&oacute;dulos
	 * PKCS#11, elimina las entradas necesarias para que aparezca una
	 * &uacute;nica vez cada uno de los m&oacute;dulo PKCS#11.
	 * @param table Tabla con las descripciones de los m&oacute;dulos pkcs11 y las
	 *              librer&iacute;as asociadas.
	 * @return Tabla con los duplicados eliminados. */
	private static Map<String, String> purgeStoresTable(final Map<String, String> table) {

		if (table == null) {
			return new ConcurrentHashMap<>(0);
		}

		final Map<String, String> purgedTable = new ConcurrentHashMap<>();
		final Set<String> revisedLibs = new HashSet<>();

		// Creamos una copia de las claves para evitar problemas de concurrencia
		// (nunca soltara exceciones por usar ConcurrentHashMap, pero no significa
		// que los problemas no ocurran si no se toman medidas).
		final String[] keys = table.keySet().toArray(new String[0]);

		String tmpLib;
		for (final String key : keys) {
			tmpLib = table.get(key);
			if (tmpLib.toLowerCase().endsWith(".dll")) { //$NON-NLS-1$
				tmpLib = tmpLib.toLowerCase();
			}

			if (!revisedLibs.contains(tmpLib) && !tmpLib.toLowerCase().contains("nssckbi")) { //$NON-NLS-1$
				purgedTable.put(key, table.get(key));
				revisedLibs.add(tmpLib);
			}
			else {
				LOGGER.warning("Se eliminara el modulo '" + key //$NON-NLS-1$
					+ "' porque ya existe uno con la misma biblioteca o es un modulo de certificados raiz: " //$NON-NLS-1$
						+ table.get(key));
			}
		}

		return purgedTable;
	}

}
