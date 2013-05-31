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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.security.Provider;
import java.security.Security;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import javax.script.ScriptEngineManager;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Platform;

/** Clase con m&eacute;toos de utilidad para la gesti&oacute;n del almac&eacute;n
 * de certificados de Mozilla. */
final class MozillaKeyStoreUtilities {

	private static final String LIB_NSPR4_SO = "/lib/libnspr4.so"; //$NON-NLS-1$

	private static final String SOFTOKN3_SO = "libsoftokn3.so"; //$NON-NLS-1$

	private MozillaKeyStoreUtilities() {
		// No permitimos la instanciacion
	}

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Directorio con las bibliotecas de NSS necesarias para el acceso al
	 * almac&eacute;n de Mozilla. */
	private static String nssLibDir = null;

	/** Crea las l&iacute;neas de configuraci&oacute;n para el uso de las
	 * bibliotecas NSS como m&oacute;dulo PKCS#11 por el proveedor de Sun.
	 * @param userProfileDirectory
	 *        Directorio donde se encuentra el perfil de usuario de Mozilla
	 *        Firefox
	 * @param libDir
	 *        Directorio que contiene las bibliotecas NSS
	 * @return Fichero con las propiedades de configuracion del proveedor
	 *         PKCS#11 de Sun para acceder al KeyStore de Mozilla v&iacute;a
	 *         NSS. */
	static String createPKCS11NSSConfigFile(final String userProfileDirectory, final String libDir) {

		String softoknLib = SOFTOKN3_SO;
		if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
			softoknLib = MozillaKeyStoreUtilitiesWindows.getSoftoknLibNameWindows();
		}
		else if (Platform.getOS().equals(Platform.OS.MACOSX)) {
			softoknLib = "libsoftokn3.dylib"; //$NON-NLS-1$
		}

		final StringBuilder buffer = new StringBuilder("name=NSSCrypto-AFirma\r\n"); //$NON-NLS-1$

		// Java 1.5 tenia un metodo indocumentado para acceder a NSS,
		// http://docs.sun.com/app/docs/doc/819-3671/gcsoc?a=view

		// if (System.getProperty("java.version").startsWith("1.5")) {
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
		// //.append("omitInitialize=true");
		// }
		// else {
		// Inicializacion segun Java 6
		// buffer
		// .append("nssLibraryDirectory=").append(libDir).append("\r\n")
		// .append("nssSecmodDirectory=\"").append(userProfileDirectory).append("\"\r\n")
		// //.append("nssUseSecmod=true\r\n") // No es necesario, con usar
		// nssLibraryDirectory o nssSecModDirectory ya se activa el modo NSS
		// .append("nssDbMode=readOnly\r\n")
		// .append("attributes=compatibility\r\n")
		// .append("nssModule=keystore\r\n")
		// //.append("allowSingleThreadedModules=true\r\n");
		// //.append("nssNetscapeDbWorkaround=true\r\n") // Solo si necesitamos
		// crear claves privadas
		// //.append("showInfo=true\r\n")
		//
		// ;
		// }

		// LOGGER.info("Configuracion SunPKCS11 para NSS:\n"
		// + buffer.toString());

		return buffer.toString();

	}

	private static String getSystemNSSLibDirMacOsX() throws FileNotFoundException {

		final String[] paths =
			new String[] {
				"/Applications/Firefox.app/Contents/MacOS", //$NON-NLS-1$
				"/lib", //$NON-NLS-1$
				"/usr/lib", //$NON-NLS-1$
				"/usr/lib/nss", //$NON-NLS-1$
				"/Applications/Minefield.app/Contents/MacOS" //$NON-NLS-1$
		};

		for (final String path : paths) {
			if (new File(path + "/libsoftokn3.dylib").exists()) { //$NON-NLS-1$
				nssLibDir = path;
			}
		}

		if (nssLibDir == null) {
			throw new FileNotFoundException("No se ha podido determinar la localizacion de NSS en Mac OS X"); //$NON-NLS-1$
		}

		return nssLibDir;
	}

	private static String getSystemNSSLibDirUnix() throws FileNotFoundException {

		if (nssLibDir != null && !"".equals(nssLibDir)) { //$NON-NLS-1$
			return nssLibDir;
		}

		// *********************************************************************
		// *********************************************************************
		// Compobamos antes el caso especifico de NSS partido entre /usr/lib y
		// /lib, que se da en Fedora
		if (new File("/usr/lib/" + SOFTOKN3_SO).exists() && new File(LIB_NSPR4_SO).exists()) { //$NON-NLS-1$
			try {
				System.load(LIB_NSPR4_SO);
				nssLibDir = "/usr/lib"; //$NON-NLS-1$
			}
			catch (final Exception e) {
				nssLibDir = null;
				LOGGER.warning(
						"Descartamos el NSS situado entre /lib y /usr/lib porque no puede cargarse adecuadamente: " + e //$NON-NLS-1$
				);
			}
			if (nssLibDir != null) {
				return nssLibDir;
			}
		}
		// *********************************************************************
		// *********************************************************************

		final String[] paths =
			new String[] {
				"/usr/lib/firefox", //$NON-NLS-1$
				"/usr/lib/firefox-" + searchLastFirefoxVersion("/usr/lib/"), //$NON-NLS-1$ //$NON-NLS-2$
				"/opt/firefox", //$NON-NLS-1$
				"/opt/firefox-" + searchLastFirefoxVersion("/opt/"), //$NON-NLS-1$ //$NON-NLS-2$
				"/lib", //$NON-NLS-1$
				"/usr/lib", //$NON-NLS-1$
				"/usr/lib/nss", //$NON-NLS-1$
				"/usr/lib/i386-linux-gnu/nss", /* En algunos Ubuntu */ //$NON-NLS-1$
				"/opt/fedora-ds/clients/lib", //$NON-NLS-1$
				"/opt/google/chrome", /* NSS de Chrome cuando no hay NSS de Mozilla de la misma arquietctura */ //$NON-NLS-1$
				"/usr/lib/thunderbird", /* Si hay Thunderbird pero no Firefox */ //$NON-NLS-1$
				"/usr/lib64" /* NSS cuando solo hay Firefox de 64 en el sistema */ //$NON-NLS-1$
		};

		for (final String path : paths) {
			String tailingLib = "/libnspr4.so"; //$NON-NLS-1$
			if (new File(path + "/mozsqlite3.so").exists()) { //$NON-NLS-1$
				tailingLib = "/mozsqlite3.so"; //$NON-NLS-1$
			}
			else if (new File(path + "/libmozsqlite3.so").exists()) { //$NON-NLS-1$
				tailingLib = "/libmozsqlite3.so"; //$NON-NLS-1$
			}
			if (new File(path + "/" + SOFTOKN3_SO).exists() && new File(path + tailingLib).exists()) { //$NON-NLS-1$
				try {
					System.load(path + tailingLib);
					nssLibDir = path;
				}
				catch (final Exception e) {
					nssLibDir = null;
					LOGGER.warning(
							"Descartamos el NSS situado en '" + path //$NON-NLS-1$
							+ "' porque no puede cargarse adecuadamente: " //$NON-NLS-1$
							+ e
					);
				}
				if (nssLibDir != null) {
					return nssLibDir;
				}
			}
		}

		if (nssLibDir == null) {
			throw new FileNotFoundException("No se ha podido determinar la localizacion de NSS en UNIX"); //$NON-NLS-1$
		}

		return nssLibDir;
	}

	/** Obtiene el directorio de las bibliotecas NSS (<i>Netscape Security
	 * Services</i>) del sistema.
	 * @return Directorio de las bibliotecas NSS del sistema
	 * @throws FileNotFoundException
	 *         Si no se puede encontrar NSS en el sistema */
	private static String getSystemNSSLibDir() throws FileNotFoundException {

		if (nssLibDir != null) {
			return nssLibDir;
		}

		if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
			return MozillaKeyStoreUtilitiesWindows.getSystemNSSLibDirWindows();
		}
		if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.SOLARIS)) {
			return getSystemNSSLibDirUnix();
		}
		if (Platform.getOS().equals(Platform.OS.MACOSX)) {
			return getSystemNSSLibDirMacOsX();
		}

		throw new FileNotFoundException(
				"No se han encontrado bibliotecas NSS instaladas en su sistema operativo" //$NON-NLS-1$
		);
	}

	/** Busca la &uacute;ltima versi&oacute;n de firefox instalada en un sistema
	 * Linux o Solaris
	 * @return &Uacute;ltima versi&oacute;n instalada en el sistema */
	private static String searchLastFirefoxVersion(final String startDir) {
		final File directoryLib = new File(startDir);
		if (directoryLib.isDirectory()) {
			final String filenames[] = directoryLib.list();
			final List<String> firefoxDirectories = new ArrayList<String>();
			for (final String filename : filenames) {
				if (filename.startsWith("firefox-")) { //$NON-NLS-1$
					firefoxDirectories.add(filename.replace("firefox-", "")); //$NON-NLS-1$ //$NON-NLS-2$
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
					firefoxDirectories.remove(i);
				}
			}
			if (firefoxDirectories.size() == 1) {
				return firefoxDirectories.get(0);
			}
			Collections.sort(firefoxDirectories, new Comparator<String>() {
				/** {@inheritDoc} */
				@Override
				public int compare(final String o1, final String o2) {
					return o1.compareTo(o2);
				}
			});
			return firefoxDirectories.get(0);
		}
		return ""; //$NON-NLS-1$
	}

	/** Obtiene las rutas completas hacia las bibliotecas (.dll o .so) de los
	 * m&oacute;dulos de seguridad externos (PKCS#11) instalados en Mozilla /
	 * Firefox, indexados por su descripci&oacute;n dentro de una <code>Hashtable</code>.
	 * @return Nombres de las bibliotecas de los m&oacute;dulos de seguridad de
	 *         Mozilla / Firefox */
	static Map<String, String> getMozillaPKCS11Modules() {
		try {
			final Map<String, String> modsByDesc = new Hashtable<String, String>();
			for (final AOSecMod.ModuleName module : AOSecMod.getModules(getMozillaUserProfileDirectory())) {
				modsByDesc.put(module.getDescription(), module.getLib());
			}
			return purgeStoresTable(modsByDesc); // Eliminamos las entradas que usen la misma biblioteca
		}
		catch (final Exception t) {
			LOGGER.severe("No se han podido obtener los modulos externos de Mozilla, se devolvera una lista vacia o unicamente con el DNIe: " + t); //$NON-NLS-1$
			return new Hashtable<String, String>(0);
		}
	}

	/** Obtiene el nombre (<i>commonName</i>) de un m&oacute;dulo externo de
	 * Mozilla a partir de su representaci&oacute;n textual. Este m&eacute;todo
	 * es dependiente de la implementaci&oacute;n de <code>toString()</code> de
	 * la clase <code>sun.security.pkcs11.Secmod.Module</code>, ya que no
	 * podemos acceder directamente al atributo <code>slot</code> por ser de
	 * tipo <i>friend</i>: <code><pre>
	 * public String toString() {
	 *   return
	 *   commonName + " (" + type + ", " + libraryName + ", slot " + slot + ")";
	 *  }
	 *  </pre></code>
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

	/** Dada una tabla que indexa por descripci&oacute;n los m&oacute;dulos
	 * pkcs11, eliminamos las entradas necesarias para que aparezca una
	 * &uacute;nica vez cada m&oacute;dulo PKCS#11.
	 * @param table
	 *        Tabla con las descripciones de los m&oacute;dulos pkcs11 y las
	 *        librer&iacute;as asociadas.
	 * @return Tabla con los m&oacute;dulos eliminados. */
	private static Map<String, String> purgeStoresTable(final Map<String, String> table) {

		if (table == null) {
			return new Hashtable<String, String>(0);
		}

		final Map<String, String> purgedTable = new Hashtable<String, String>();
		final Set<String> revisedLibs = new HashSet<String>();

		String tmpLib;
		for (final String key : table.keySet()) {
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
	 * @param path
	 *        Ruta al directorio de NSS (terminado en barra).
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
					nssPath + "libnspr4.so",     // Firefox 2 y superior //$NON-NLS-1$
					nssPath + "libplds4.so",     // Firefox 2 y superior //$NON-NLS-1$
					nssPath + "libplc4.so",      // Firefox 2 y superior //$NON-NLS-1$
					nssPath + "libnssutil3.so",  // Firefox 2 y superior //$NON-NLS-1$
					nssPath + "libsqlite3.so",   // Firefox 2            //$NON-NLS-1$
					nssPath + "libmozsqlite3.so" // Firefox 3 y superior //$NON-NLS-1$
			};
		}

		LOGGER.warning(
				"Plataforma no soportada para la precarga de las bibliotecas NSS: " + Platform.getOS() //$NON-NLS-1$
				+ " + Java " //$NON-NLS-1$
				+ Platform.getJavaArch()
				+ " bits" //$NON-NLS-1$
		);
		return new String[0];
	}

	private static String getMozillaUserProfileDirectoryUnix() {
		// Probamos con "profiles.ini" de Firefox
		final File regFile = new File(Platform.getUserHome() + "/.mozilla/firefox/profiles.ini"); //$NON-NLS-1$
		try {
			if (regFile.exists()) {
				return NSPreferences.getFireFoxUserProfileDirectory(regFile);
			}
		}
		catch (final Exception e) {
			LOGGER.severe(
					"Error obteniendo el directorio de perfil de usuario de Firefox (UNIX), se devolvera null: " + e //$NON-NLS-1$
			);
		}
		return null;
	}

	private static String getMozillaUserProfileDirectoryMacOsX() {
		// Si es un Mac OS X, profiles.ini esta en una ruta distinta...
		final File regFile = new File(Platform.getUserHome() + "/Library/Application Support/Firefox/profiles.ini"); //$NON-NLS-1$
		try {
			if (regFile.exists()) {
				return NSPreferences.getFireFoxUserProfileDirectory(regFile);
			}
		}
		catch (final Exception e) {
			LOGGER.severe(
					"Error obteniendo el directorio de perfil de usuario de Firefox (" + regFile.getAbsolutePath() //$NON-NLS-1$
					+ "), se devolvera null: " //$NON-NLS-1$
					+ e
			);
		}
		return null;
	}

	/** Obtiene el directorio del perfil de usuario de Mozilla / Firefox.
	 * @return Ruta completa del directorio del perfil de usuario de Mozilla /
	 *         Firefox */
	static String getMozillaUserProfileDirectory() {
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return MozillaKeyStoreUtilitiesWindows.getMozillaUserProfileDirectoryWindows();
		}

		else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			return getMozillaUserProfileDirectoryMacOsX();
		}
		// No es Windows ni Mac OS X, entonces es UNIX (Linux / Solaris)
		return getMozillaUserProfileDirectoryUnix();
	}

	static void configureMacNSS(final String binDir) throws AOException {

		if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
			return;
		}

		if (binDir == null) {
			LOGGER.severe("El directorio de NSS para configurar proporcionado es nulo, no se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		final String nssBinDir = binDir.endsWith("/") ? binDir : binDir + "/"; //$NON-NLS-1$ //$NON-NLS-2$

		// Intentamos la carga, para ver si es necesaria la reconfiguracion
		try {
			System.load(nssBinDir + "libsoftokn3.dylib"); //$NON-NLS-1$
			return; // Si funciona salimos sin hacer nada
		}
		catch (final Exception e) {
			// Se ignora el error
		}
		catch(final UnsatisfiedLinkError e) {
			// Se ignora el error
		}

		final String[] libs = new String[] {
				"libmozglue.dylib", // Firefox 11 y superiores //$NON-NLS-1$
				"libmozutils.dylib", // Firefox 9 y 10 //$NON-NLS-1$
				"libnspr4.dylib", //$NON-NLS-1$
				"libplds4.dylib", //$NON-NLS-1$
				"libplc4.dylib", //$NON-NLS-1$
				"libnssutil3.dylib", //$NON-NLS-1$
				"libmozsqlite3.dylib" //$NON-NLS-1$
		};

		// Creamos enlaces simbolicos via AppleScript
		final StringBuilder sb = new StringBuilder();
		for (final String lib : libs) {
			if (new File(nssBinDir + lib).exists()) {
				sb.append("ln -s "); //$NON-NLS-1$
				sb.append(nssBinDir);
				sb.append(lib);
				sb.append(" /usr/lib/"); //$NON-NLS-1$
				sb.append(lib);
				sb.append("; "); //$NON-NLS-1$
			}
		}
		try {
			new ScriptEngineManager().getEngineByName("AppleScript").eval("do shell script \"" + sb.toString() + "\" with administrator privileges"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		catch(final Exception e) {
			LOGGER.severe("No se ha podido crear los enlaces simbolicos para NSS: " + e); //$NON-NLS-1$
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

	static Provider loadNSS() throws FileNotFoundException,
	                                 AOException,
	                                 InstantiationException,
	                                 IllegalAccessException,
	                                 InvocationTargetException,
	                                 NoSuchMethodException,
	                                 ClassNotFoundException {

		final String nssDirectory = MozillaKeyStoreUtilities.getSystemNSSLibDir();
		final String p11NSSConfigFile =
			MozillaKeyStoreUtilities.createPKCS11NSSConfigFile(
					MozillaKeyStoreUtilities.getMozillaUserProfileDirectory(),
					nssDirectory
			);

		LOGGER.info("Configuracion de NSS para SunPKCS11:\n" + p11NSSConfigFile); //$NON-NLS-1$

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
				MozillaKeyStoreUtilities.configureMacNSS(nssDirectory);
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
}
