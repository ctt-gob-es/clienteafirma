/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.security.Provider;
import java.security.Security;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.mozilla.AOSecMod.ModuleName;

/** Clase con m&eacute;toos de utilidad para la gesti&oacute;n del almac&eacute;n
 * de certificados de Mozilla. */
final class MozillaKeyStoreUtilities {

	private static final String LIB_NSPR4_SO = "/lib/libnspr4.so"; //$NON-NLS-1$

	private static final String SOFTOKN3_SO = "libsoftokn3.so"; //$NON-NLS-1$

	/** Nombre del PKCS#11 NSS en Windows. */
	private static final String SOFTOKN3_DLL = "softokn3.dll"; //$NON-NLS-1$

	private static final String AFIRMA_NSS_HOME = "AFIRMA_NSS_HOME"; //$NON-NLS-1$

	private static final String AFIRMA_PROFILES_INI = "AFIRMA_PROFILES_INI"; //$NON-NLS-1$

	private static final String USE_ENV_VARS = "es.gob.afirma.keystores.mozilla.UseEnvironmentVariables"; //$NON-NLS-1$

	private static final String[] DNI_P11_NAMES = new String[] {
		"libopensc-dnie.dylib", //$NON-NLS-1$
		"libopensc-dnie.so", //$NON-NLS-1$
		"usrpkcs11.dll", //$NON-NLS-1$
		"dnie_p11_priv.dll", //$NON-NLS-1$
		"dnie_p11_pub.dll", //$NON-NLS-1$
		"opensc-pkcs11.dll", //$NON-NLS-1$
		"libpkcs11-fnmtdnie.so", //$NON-NLS-1$
		"FNMT_P11.dll"//$NON-NLS-1$
	};

	private static final String[][] KNOWN_MODULES = new String[][] {
		new String[] { "Atos CardOS (preinstalado)", "siecap11.dll"    }  //$NON-NLS-1$ //$NON-NLS-2$
	};

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
	 *                             usuario de Mozilla Firefox
	 * @param libDir Directorio que contiene las bibliotecas NSS
	 * @return Fichero con las propiedades de configuracion del proveedor
	 *         PKCS#11 de Sun para acceder al KeyStore de Mozilla v&iacute;a NSS. */
	static String createPKCS11NSSConfigFile(final String userProfileDirectory, final String libDir) {

		final String softoknLib;
		if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
			softoknLib = SOFTOKN3_DLL;
		}
		else if (Platform.getOS().equals(Platform.OS.MACOSX)) {
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
			.append("secmod='secmod.db' ") //$NON-NLS-1$
			.append("flags='readOnly'") //$NON-NLS-1$
			.append("\""); //$NON-NLS-1$

		return buffer.toString();

	}

	static String getNssPathFromCompatibilityFile() throws IOException {
		final File compatibility = new File(getMozillaUserProfileDirectory(), "compatibility.ini");  //$NON-NLS-1$
		if (compatibility.exists() && compatibility.canRead()) {
			final InputStream fis = new FileInputStream(compatibility);
			// Cargamos el fichero con la codificacion por defecto (que es la que con mas probabilidad tiene el fichero)
			final BufferedReader br = new BufferedReader(new InputStreamReader(fis));
			String line;
			String dir = null;
			while ((line = br.readLine()) != null) {
			    if (line.startsWith("LastPlatformDir=")) { //$NON-NLS-1$
			    	dir = line.replace("LastPlatformDir=", "").trim(); //$NON-NLS-1$ //$NON-NLS-2$
					break;
			    }
			}
			br.close();
			if (dir != null) {
				return dir;
			}
		}
		throw new FileNotFoundException("No se ha podido determinar el directorio de NSS en Windows a partir de 'compatibility.ini' de Firefox"); //$NON-NLS-1$
	}

	/** Obtiene el directorio de las bibliotecas NSS (<i>Netscape Security
	 * Services</i>) del sistema.
	 * @return Directorio de las bibliotecas NSS del sistema
	 * @throws FileNotFoundException
	 *         Si no se puede encontrar NSS en el sistema
     * @throws IOException En caso de errores de lectura/escritura */
	private static String getSystemNSSLibDir() throws IOException {

		if (nssLibDir != null) {
			return nssLibDir;
		}

		// Primero probamos con la variable de entorno, que es comun a todos los sistemas operativos
		if (Boolean.getBoolean(USE_ENV_VARS)) {
			try {
				nssLibDir = System.getenv(AFIRMA_NSS_HOME);
			}
			catch(final Exception e) {
				LOGGER.warning("No se tiene acceso a la variable de entorno '" + AFIRMA_NSS_HOME + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (nssLibDir != null) {
				final File nssDir = new File(nssLibDir);
				if (nssDir.isDirectory() && nssDir.canRead()) {
					LOGGER.info("Directorio de NSS determinado a partir de la variable de entorno '" + AFIRMA_NSS_HOME + "'"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				else {
					LOGGER.warning(
						"La variable de entorno '" + AFIRMA_NSS_HOME + "' apunta a un directorio que no existe o sobre el que no se tienen permisos de lectura, se ignorara" //$NON-NLS-1$ //$NON-NLS-2$
					);
					nssLibDir = null;
				}
			}
		}

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			nssLibDir = MozillaKeyStoreUtilitiesWindows.getSystemNSSLibDirWindows();
		}

		else if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.SOLARIS)) {
			nssLibDir = MozillaKeyStoreUtilitiesUnix.getSystemNSSLibDirUnix();
		}

		else if (Platform.getOS().equals(Platform.OS.MACOSX)) {
			nssLibDir = MozillaKeyStoreUtilitiesOsX.getSystemNSSLibDirMacOsX();
		}

		if (nssLibDir != null) {
			return nssLibDir;
		}

		throw new FileNotFoundException(
			"No se han encontrado bibliotecas NSS instaladas en su sistema operativo" //$NON-NLS-1$
		);
	}

	/** Obtiene las rutas completas hacia las bibliotecas (.dll o .so) de los
	 * m&oacute;dulos de seguridad externos (PKCS#11) instalados en Mozilla /
	 * Firefox, indexados por su descripci&oacute;n dentro de un <code>ConcurrentHashMap</code>.
	 * @param excludeDnie Si se establece a <code>true</code> excluye los m&oacute;dulos PKCS#11
	 *                    del DNIe, si se establece a <code>false</code> deja estos m&oacute;dulos en
	 *                    caso de que se encontrasen.
	 * @param includeKnownModules Si se establece a <code>true</code> se incluyen m&oacute;dulos PKCS#11 que
	 *                            est&eacute;n en el directorio de bibliotecas del sistema pero no en la
	 *                            base de datos de m&oacute;dulos de Mozilla (<i>secmod.db</i>), si se
	 *                            establece a <code>false</code> se devuelven &uacute;nicamente los
	 *                            m&oacute;dulos PKCS#11 de la base de datos.
	 * @return Nombres de las bibliotecas de los m&oacute;dulos de seguridad de
	 *         Mozilla / Firefox */
	static Map<String, String> getMozillaPKCS11Modules(final boolean excludeDnie,
			                                           final boolean includeKnownModules) {

		final Map<String, String> modsByDesc = new ConcurrentHashMap<String, String>();
		final List<ModuleName> modules;
		try {
			modules =  AOSecMod.getModules(getMozillaUserProfileDirectory());
		}
		catch (final Exception t) {
			LOGGER.severe("No se han podido obtener los modulos externos de Mozilla, se devolvera una lista vacia o unicamente con el DNIe: " + t); //$NON-NLS-1$
			return new ConcurrentHashMap<String, String>(0);
		}

		for (final AOSecMod.ModuleName module : modules) {
			final String moduleLib =  module.getLib();
			if (excludeDnie && isDniePkcs11Library(moduleLib)) {
				continue;
			}
			modsByDesc.put(module.getDescription(), moduleLib);
		}

		// Creamos una copia de modsByDesc para evitar problemas de concurrencia
		// (nunca soltara exceciones por usar ConcurrentHashMap, pero no significa
		// que los problemas no ocurran si no se toman medidas).
		final ConcurrentHashMap<String, String> modsByDescCopy = new ConcurrentHashMap<String, String>(modsByDesc.size());
		modsByDescCopy.putAll(modsByDesc);

		if (includeKnownModules) {
			for (final String[] knownModules : KNOWN_MODULES) {
				if (!isModuleIncluded(modsByDescCopy, knownModules[1])) {
					final String modulePath = getWindowsSystemDirWithFinalSlash() + knownModules[1];
					if (new File(modulePath).exists()) {
						modsByDesc.put(knownModules[0], knownModules[1]);
					}
				}
			}
		}

		return purgeStoresTable(modsByDesc);
	}

	/** Obtiene el nombre (<i>commonName</i>) de un m&oacute;dulo externo de
	 * Mozilla a partir de su representaci&oacute;n textual. Este m&eacute;todo
	 * es dependiente de la implementaci&oacute;n de <code>toString()</code> de
	 * la clase <code>sun.security.pkcs11.Secmod.Module</code>, ya que no
	 * podemos acceder directamente al atributo <code>slot</code> por ser de
	 * tipo <i>friend</i>:
	 * <pre>
	 *  public String toString() {
	 *    return
	 *    commonName + " (" + type + ", " + libraryName + ", slot " + slot + ")";
	 *  }
	 * </pre>
	 * @param description
	 *        Resultado de una llamada a <code>sun.security.pkcs11.Secmod.Module.toString()</code>
	 * @return Nombre correspondiente al m&oacute;dulo de seguridad */
	static String getMozModuleName(final String description) {
		final int ini = description.indexOf('(');
		if (ini > 0) {
			return description.substring(0, ini).trim();
		}
		return description;
	}

	/** Carga las dependencias de la biblioteca "softokn3" necesaria para acceder
	 * al almac&eacute;n de certificados. Hacemos esto por precauci&oacute;n ya
	 * que esto se har&iacute;a autom&aacute;ticamente si las dependencias
	 * estuviesen en el PATH del sistema.
	 * @param nssDirectory
	 *        Directorio en donde se encuentran las bibliotecas de NSS. */
	static void loadNSSDependencies(final String nssDirectory) {

		final String dependList[];

		// Compobamos despues el caso especifico de NSS partido entre /usr/lib y
		// /lib, que se da en Fedora
		if (Platform.OS.LINUX.equals(Platform.getOS()) && new File("/usr/lib/" + SOFTOKN3_SO).exists() && new File(LIB_NSPR4_SO).exists()) { //$NON-NLS-1$
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
		}
		else {
			final String path = nssDirectory + (nssDirectory.endsWith(File.separator) ? "" : File.separator); //$NON-NLS-1$
			dependList = getSoftkn3Dependencies(path);
		}

		for (final String libPath : dependList) {
			try {
				if (new File(libPath).exists()) {
					System.load(libPath);
				}
			}
			catch (final Error e) {
				LOGGER.warning(
					"Error al cargar la biblioteca " + libPath + " para el acceso al almacen de claves de Mozilla: " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
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

		if (Platform.getOS().equals(Platform.OS.MACOSX)) {
			// En Mac OS X no funciona la precarga de bibliotecas
			return new String[0];
		}

		final String nssPath = !path.endsWith(File.separator) ? path + File.separator : path;

		if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
			return MozillaKeyStoreUtilitiesWindows.getSoftkn3DependenciesWindows(nssPath);
		}
		else if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.SOLARIS)) {
			return new String[] {
				nssPath + "libnspr4.so",      // Firefox 2 y superior //$NON-NLS-1$
				nssPath + "libplds4.so",      // Firefox 2 y superior //$NON-NLS-1$
				nssPath + "libplc4.so",       // Firefox 2 y superior //$NON-NLS-1$
				nssPath + "libnssutil3.so",   // Firefox 2 y superior //$NON-NLS-1$
				nssPath + "libsqlite3.so",    // Firefox 2            //$NON-NLS-1$
				nssPath + "libmozsqlite3.so", // Firefox 3 y superior //$NON-NLS-1$
				nssPath + "libsqlite3.so"     // Variante de SQLite en ciertos Linux / Firefox //$NON-NLS-1$
			};
		}

		LOGGER.warning(
				"Plataforma no soportada para la precarga de las bibliotecas NSS: " + Platform.getOS() //$NON-NLS-1$
		);
		return new String[0];
	}

	private static String getProfilesIniPath() {
		String profilesIniPath = null;
		// Miramos primero la variable de entorno 'AFIRMA_PROFILES_INI'
		if (Boolean.getBoolean(USE_ENV_VARS)) {
			try {
				profilesIniPath = System.getenv(AFIRMA_PROFILES_INI);
				if (profilesIniPath == null) {
					profilesIniPath = System.getProperty(AFIRMA_PROFILES_INI);
				}
			}
			catch(final Exception e) {
				LOGGER.warning("No se tiene acceso a la variable de entorno '" + AFIRMA_PROFILES_INI + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
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
						"Fichero de perfiles de Firefox determinado a partir de la variable de entorno '" + AFIRMA_PROFILES_INI + "'" //$NON-NLS-1$ //$NON-NLS-2$
					);
					return profilesIniPath;
				}
				LOGGER.warning(
					"La variable de entorno '" + AFIRMA_PROFILES_INI + "' apunta a un fichero que no existe o sobre el que no se tienen permisos de lectura, se ignorara: " + profilesIniPath //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return MozillaKeyStoreUtilitiesWindows.getWindowsAppDataDir() + "\\Mozilla\\Firefox\\profiles.ini"; //$NON-NLS-1$
		}
		if (Platform.getOS().equals(Platform.OS.MACOSX)) {
			return Platform.getUserHome() + "/Library/Application Support/Firefox/profiles.ini"; //$NON-NLS-1$
		}
		// Linux / UNIX
		return Platform.getUserHome() + "/.mozilla/firefox/profiles.ini"; //$NON-NLS-1$
	}


	/** Obtiene el directorio del perfil de usuario de Mozilla / Firefox.
	 * @return Ruta completa del directorio del perfil de usuario de Mozilla / Firefox
	 * @throws IOException Cuando hay errores de entrada / salida */
	static String getMozillaUserProfileDirectory() throws IOException {
		final String dir = NSPreferences.getFireFoxUserProfileDirectory(new File(getProfilesIniPath()));
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return MozillaKeyStoreUtilitiesWindows.cleanMozillaUserProfileDirectoryWindows(dir);
		}
		return dir;
	}

	static Provider loadNSS() throws IOException,
	                                 AOException,
	                                 InstantiationException,
	                                 IllegalAccessException,
	                                 InvocationTargetException,
	                                 NoSuchMethodException,
	                                 ClassNotFoundException {

		final String nssDirectory = MozillaKeyStoreUtilities.getSystemNSSLibDir();
		final String p11NSSConfigFile = MozillaKeyStoreUtilities.createPKCS11NSSConfigFile(
			MozillaKeyStoreUtilities.getMozillaUserProfileDirectory(),
			nssDirectory
		);

		// Quitamos el directorio del usuario del registro, para evitar que contenga datos personales
		LOGGER.info("Configuracion de NSS para SunPKCS11:\n" + p11NSSConfigFile.replace(Platform.getUserHome(), "USERHOME")); //$NON-NLS-1$ //$NON-NLS-2$

		Provider p = null;
		try {
			p = (Provider) Class.forName("sun.security.pkcs11.SunPKCS11") //$NON-NLS-1$
			.getConstructor(InputStream.class)
			.newInstance(new ByteArrayInputStream(p11NSSConfigFile.getBytes()));
		}
		catch (final Exception e) {
			// No se ha podido cargar el proveedor sin precargar las dependencias
			// Cargamos las dependencias necesarias para la correcta carga
			// del almacen (en Mac se crean enlaces simbolicos)
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				MozillaKeyStoreUtilitiesOsX.configureMacNSS(nssDirectory);
			}
			else {
				MozillaKeyStoreUtilities.loadNSSDependencies(nssDirectory);
			}

			try {
				p = (Provider) Class.forName("sun.security.pkcs11.SunPKCS11") //$NON-NLS-1$
					.getConstructor(InputStream.class)
					.newInstance(new ByteArrayInputStream(p11NSSConfigFile.getBytes()));
			}
			catch (final Exception e2) {
				// Un ultimo intento de cargar el proveedor valiendonos de que es posible que
				// las bibliotecas necesarias se hayan cargado tras el ultimo intento
				p = (Provider) Class.forName("sun.security.pkcs11.SunPKCS11") //$NON-NLS-1$
					.getConstructor(InputStream.class)
					.newInstance(new ByteArrayInputStream(p11NSSConfigFile.getBytes()));
			}
		}

		Security.addProvider(p);

		LOGGER.info("Proveedor PKCS#11 para Firefox anadido"); //$NON-NLS-1$

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
		final String winDir = System.getenv("SystemRoot"); //$NON-NLS-1$
		if (winDir == null) {
			return ""; //$NON-NLS-1$
		}
		// Si java es 64 bits, no hay duda de que Windows es 64 bits y buscamos bibliotecas
		// de 64 bits
		if ("64".equals(Platform.getJavaArch())) { //$NON-NLS-1$
			return winDir + "\\System32\\";  //$NON-NLS-1$
		}
		if (new File(winDir + "\\SysWOW64\\").exists()) { //$NON-NLS-1$
			return winDir + "\\SysWOW64\\"; //$NON-NLS-1$
		}
		return winDir + "\\System32\\"; //$NON-NLS-1$
	}

	private static boolean isModuleIncluded(final Map<String, String> externalStores, final String moduleName) {
		if (externalStores == null || moduleName == null) {
			throw new IllegalArgumentException("Ni la lista de almacenes ni el modulo a comprobar pueden ser nulos"); //$NON-NLS-1$
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
			return new ConcurrentHashMap<String, String>(0);
		}

		final Map<String, String> purgedTable = new ConcurrentHashMap<String, String>();
		final Set<String> revisedLibs = new HashSet<String>();

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
