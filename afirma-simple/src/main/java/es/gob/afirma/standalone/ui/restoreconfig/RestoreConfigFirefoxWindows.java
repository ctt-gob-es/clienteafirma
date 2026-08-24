/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.restoreconfig;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.keystores.mozilla.MozillaProfile;
import es.gob.afirma.keystores.mozilla.ProfilesIni;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;

import java.awt.*;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;


/**Contiene la l&oacute;gica para realizar las tareas de restauraci&oacute;n
 * asociadas al navegador Firefox para Windows. */
final class RestoreConfigFirefoxWindows {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int TIMEOUT = 5000;

	private static final String DIR_CERTUTIL = "certutil"; //$NON-NLS-1$

	private static final String MOZILLA_PROFILES_RELATIVE_PATH;
	private static String USERS_PATH;

	private static final String CERTUTIL_EXE = "certutil.exe"; //$NON-NLS-1$
	private static final String FILE_CERTUTIL = "/windows/certutil.windows.zip"; //$NON-NLS-1$

	static {

		// Para Windows XP la ruta de los perfiles de Firefox y de los usuarios es diferente
		String osName;
		try {
			osName = System.getProperty("os.name"); //$NON-NLS-1$
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo obtener el nombre del sistema operativo", e); //$NON-NLS-1$
			osName = ""; //$NON-NLS-1$
		}
		if (osName.contains("XP")) { //$NON-NLS-1$
			MOZILLA_PROFILES_RELATIVE_PATH = "\\Application Data\\Mozilla\\Firefox\\profiles.ini"; //$NON-NLS-1$
			USERS_PATH = "C:\\Documents and Settings\\"; //$NON-NLS-1$
		} else {
			MOZILLA_PROFILES_RELATIVE_PATH = "\\AppData\\Roaming\\Mozilla\\Firefox\\profiles.ini"; //$NON-NLS-1$
			try {
				USERS_PATH = new File(System.getProperty("user.home")).getParentFile().getAbsolutePath() + File.separator; //$NON-NLS-1$;
			} catch (final Exception e) {
				LOGGER.warning("No se ha podido identificar el directorio de usuarios: " + e); //$NON-NLS-1$
				USERS_PATH = "C:/Users/"; //$NON-NLS-1$
			}
		}
	}

	/**
	 * Nombre del usuario por defecto en Windows. Este usuario es el que se usa como base para
	 * crear nuevos usuarios y no se deber&iacute;a tocar.
	 */
	private static final String DEFAULT_WINDOWS_USER_NAME = "Default"; //$NON-NLS-1$

	/** Nombre de subdirectorio dentro del directorio de usuarios que debemos evitar. */
	private static final String ALL_USERS_DIRECTORY = "All Users"; //$NON-NLS-1$

	// Variables para cachear las rutas de los ficheros y directorios utiizados durante la ejecucion de la resturacion
	private List<File> usersDirectories = null;
	private List<MozillaProfile> profileDirectories = null;

	private Component parentComponent;

	RestoreConfigFirefoxWindows() {
		// No instanciable
	}

	/**
	 * Desinstala el certificado ra&iacute;z del almac&eacute;n de confianza de firefox.
	 * @param workingDir Ruta en la que buscar el ejecutable <code>certutil</code>.
	 * @param profile Perfil de usuario de Mozilla Firefox.
	 */
	void uninstallTrustedCertificate(final File workingDir, final MozillaProfile profile, final String certUtilPath)
			throws IOException {

		if (!profile.hasNssKeyStore()) {
			LOGGER.info("Se omite la desinstalacion en el perfil '" + profile.getName() + "' por no haberse inicializado nunca antes"); //$NON-NLS-1$
			return;
		}

		// Inicializamos el perfil para que quede registrada la contrasena del almacen en caso de tenerla
		if (!profile.isPrepared()) {
			RestoreConfigFirefoxCommon.initProfile(workingDir, profile, certUtilPath,  false,false, this.parentComponent);
		}

		// Componemos el comando para eliminar el certificado
		String[] params = getRemoveCertCommand(certUtilPath, profile);

		// Ejecutamos el comando
		try {
			execCertUtilCommand(workingDir, certUtilPath, profile, params, false);
		} catch (InvalidObjectException e) {
			LOGGER.info("No se encontro el certificado anterior en el perfil " + profile.getName()); //$NON-NLS-1$
		}
	}

	/** Elimina la carpeta certutil generada durante el proceso de instalaci&oacute;n.
	 * @param targetDir Directorio en el que se copia certUtil. */
	static void removeConfigurationFiles(final File targetDir) {
		if (!targetDir.exists()) {
			return;
		}
		deleteConfigDir(targetDir);
	}

	private static String escapePath(final String path) {
		if (path == null) {
			throw new IllegalArgumentException(
				"La ruta a 'escapar' no puede ser nula" //$NON-NLS-1$
			);
		}

		if (path.contains(" ")) { //$NON-NLS-1$
			return "\"" + path + "\""; //$NON-NLS-1$ //$NON-NLS-2$
		}
		return path;
	}

	/**
	 * Ejecuta la utilidad Mozilla CertUtil para la instalaci&oacute;n del certificado ra&iacute;z de confianza en Firefox.
	 * @param workingDir Ruta del directorio de trabajo sobre el que tenemos permisos y en el se copia {@code certutil}.
	 * @param certFile Certificado ra&iacute;z.
	 * @param profile Perfil de usuario de Mozilla Firefox.
	 * @throws IOException Cuando ocurre un error en el tratamiento de datos.
     */
	void installTrustedCertificate(final File workingDir,
	                               final File certFile,
	                               final MozillaProfile profile,
								   final String certUtilPath) throws IOException, PasswordProtectedException {

		// Inicializamos el perfil para que quede registrada la contrasena del almacen en caso de tenerla
		if (!profile.isPrepared()) {
			RestoreConfigFirefoxCommon.initProfile(workingDir, profile, certUtilPath, false,false, this.parentComponent);
		}

		if (profile.hasMasterPassword()) {
			throw new PasswordProtectedException("El perfil '" + profile.getName() + "' esta protegido por contrasena"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// Componemos el comando para instalar el certificado
		String[] params = getInstallCertCommand(certUtilPath, profile, certFile);

		// Ejecutamos el comando
		execCertUtilCommand(workingDir, certUtilPath, profile, params, false);
	}

	/**
	 * Compone el comando de CertUtil para instalar un certificado en el almac&eacute;n de confianza de Firefox.
	 * @param certUtilPath Ruta de CertUtil.
	 * @param profile Perfil de usuario de Mozilla Firefox.
	 * @param certFile Fichero del certificado a instalar.
	 * @return Comando de CertUtil para eliminar el certificado del almac&eacute;n de confianza de Firefox.
	 */
	private static String[] getInstallCertCommand(final String certUtilPath, final MozillaProfile profile, final File certFile) {

		final boolean sqlDb = new File(profile.getProfileDir(), "pkcs11.txt").exists(); //$NON-NLS-1$
		final String profileReference = (sqlDb ? "sql:" : "") //$NON-NLS-1$ //$NON-NLS-2$
				+ escapePath(profile.getProfileDir().getAbsolutePath());

		List<String> params = new ArrayList<>();
		params.add(certUtilPath); //$NON-NLS-1$ //$NON-NLS-2$
		params.add("-A"); //$NON-NLS-1$
		params.add("-d"); //$NON-NLS-1$
		params.add(profileReference); //$NON-NLS-1$ //$NON-NLS-2$
		params.add("-i"); //$NON-NLS-1$
		params.add(escapePath(certFile.getAbsolutePath()));
		params.add("-n"); //$NON-NLS-1$
		params.add("\"" + ConfiguratorUtil.CERT_ALIAS + "\""); //$NON-NLS-1$ //$NON-NLS-2$
		params.add("-t"); //$NON-NLS-1$
		params.add("\"C,,\""); //$NON-NLS-1$

		if (profile.getPasswordFile() != null) {
			params.add("-f"); //$NON-NLS-1$
			params.add(escapePath(profile.getPasswordFile().getAbsolutePath()));
		}

		return params.toArray(new String[0]);
	}

	/**
	 * Compone el comando de CertUtil para eliminar un certificado del almac&eacute;n de confianza de Firefox.
	 * @param certUtilPath Ruta de CertUtil.
	 * @param profile Perfil de usuario de Mozilla Firefox.
	 * @return Comando de CertUtil para eliminar el certificado del almac&eacute;n de confianza de Firefox.
	 */
	private static String[] getRemoveCertCommand(final String certUtilPath, final MozillaProfile profile) {

		final boolean sqlDb = new File(profile.getProfileDir(), "pkcs11.txt").exists(); //$NON-NLS-1$
		final String profileReference = (sqlDb ? "sql:" : "") //$NON-NLS-1$ //$NON-NLS-2$
				+ escapePath(profile.getProfileDir().getAbsolutePath());

		List<String> params = new ArrayList<>();
		params.add(certUtilPath); //$NON-NLS-1$ //$NON-NLS-2$
		params.add("-D"); //$NON-NLS-1$
		params.add("-d"); //$NON-NLS-1$
		params.add(profileReference); //$NON-NLS-1$ //$NON-NLS-2$
		params.add("-n"); //$NON-NLS-1$
		params.add("\"" + ConfiguratorUtil.CERT_ALIAS + "\""); //$NON-NLS-1$ //$NON-NLS-2$

		return params.toArray(new String[0]);
	}


	/**
	 * Ejecuta certutil directamente como comando.
	 * @param workingDir Directorio de trabajo.
	 * @param certUtilPath Ruta al ejecutable de CertUtil.
	 * @param profile Perfil de Firefox.
	 * @param commands Comando a ejecutar, con el nombre de comando y sus par&aacute;metros
	 * separados en un array.
	 * @param passwordFailed Indica si la ejecuci&oacute;n del comando ha fallado por un error de contrase&ntilde;a.
	 * @throws InvalidObjectException Cuando no se encuentra el certificado.
	 * @throws IOException Cuando ocurre un error en la ejecuci&oacute;n del comando.
	 * @throws InvalidPasswordException Cuando la contrase&ntilde;a del almac&eacute;n de Firefox es incorrecta.
     */
	private void execCertUtilCommand(final File workingDir, final String certUtilPath, final MozillaProfile profile,
	                                           final String[] commands, final boolean passwordFailed) throws InvalidObjectException, IOException {


		LOGGER.info("Se ejecutara un comando con los siguientes parametros (limpios):\n" + LoggerUtil.getCleanUserHomePath(Arrays.toString(commands))); //$NON-NLS-1$
		final Process process = new ProcessBuilder(commands).start();

		// Temporizador para detener el proceso una vez se sobrepase un tiempo determinado. Esto es necesario
		// porque hay situaciones que bloquean el proceso, como cuando el almacen de claves esta protegido
		// con contrasena
		new KillProcessTimer(TIMEOUT, process).start();

		// Cuando certUtil se ejecuta correctamente no hay salida de ningun tipo, asi que se interpreta
		// cualquier salida como un error
		final StringBuilder buffer = new StringBuilder();
		try (
				final InputStream resIs = process.getInputStream();
				final BufferedReader resReader = new BoundedBufferedReader(
						new InputStreamReader(resIs),
						256, // Maximo 256 lineas de salida
						1024 // Maximo 1024 caracteres por linea
				)
		) {
			String line;
			while ((line = resReader.readLine()) != null) {
				process.destroyForcibly();
				throw new IOException("Error devuelto por certutil en la salida: " + line); //$NON-NLS-1$
			}
		}

		buffer.setLength(0);
		try (
				final InputStream errIs = process.getErrorStream();
				final BufferedReader errReader = new BoundedBufferedReader(
						new InputStreamReader(errIs),
						256, // Maximo 256 lineas de salida
						1024 // Maximo 1024 caracteres por linea
				)
		) {
			String line;
			while ((line = errReader.readLine()) != null) {
				process.destroyForcibly();
				if (line.contains("Enter Password") || line.contains("password")) { //$NON-NLS-1$ //$NON-NLS-2$
					throw new InvalidPasswordException();
				}
				if (line.contains("Unrecognized Object Identifier")
						|| line.contains("could not find certificate")) { //$NON-NLS-1$ //$NON-NLS-2$
					throw new InvalidObjectException("No se ha encotrado el certificado");
				}
				throw new IOException("Error devuelto por certutil en el flujo de error: " + line); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Elimina los ficheros de configuraci&oacute;n de certutil
	 * @param appConfigDir Directorio de instalaci&oacute;n de la aplicaci&oacute;n
	 */
	private static void deleteConfigDir(final File appConfigDir) {
		RestoreConfigUtil.deleteDir(new File(appConfigDir, DIR_CERTUTIL));
	}

	/**
	 * Descomprime y copia los ficheros de configuraci&oacute;n de certutil
	 * @param workingDir Directorio al que descomprimir las herramientas de configuraci&oacute;n
	 * @return Ejecutable de certutil.
	 * @throws IOException Cuando ocurre un error al descomprimir o copiar.
	 */
	static File copyConfigurationFiles(final File workingDir) throws IOException {

		final File certutil = new File(workingDir, DIR_CERTUTIL);
		if (!certutil.exists()) {
			uncompressCertUtil(workingDir);
		}

		final File certutilFile = new File(workingDir,
				DIR_CERTUTIL + File.separator + CERTUTIL_EXE);

		if (!certutilFile.isFile()) {
			throw new IOException("No se encuentra el ejecutable CertUtil para la instalacion en Firefox"); //$NON-NLS-1$
		}

		if (!certutilFile.canExecute()) {
			throw new IOException("No hay permisos de ejecucion para Mozilla CertUtil"); //$NON-NLS-1$
		}

		return certutilFile;
	}

	/** Descomprime el ZIP interno de CertUtil en disco.
	 * @param outDir Directorio local en el que descomprimir.
	 * @throws IOException Cuando ocurre un error al descomprimir.
	 **/
	private static void uncompressCertUtil(final File outDir) throws IOException {
		int n;
		ZipEntry entry;
		final byte[] buffer = new byte[1024];
		try (InputStream inStrean = RestoreConfigFirefoxWindows.class.getResourceAsStream(FILE_CERTUTIL);
			 final ZipInputStream zipIs = new ZipInputStream(inStrean)) {

			while ((entry = zipIs.getNextEntry()) != null) {
				final File outFile = new File(outDir, entry.getName());

				if (!RestoreConfigFirefoxCommon.isAncestorDir(outDir, outFile)) {
					zipIs.closeEntry();
					throw new IOException("Se ha encontrado en el archivo comprimido una ruta que apuntaba fuera del directorio de destino"); //$NON-NLS-1$
				}

				if (entry.isDirectory()) {
					outFile.mkdirs();
				} else {
					if (!outFile.getParentFile().exists()) {
						outFile.getParentFile().mkdirs();
					}
					try (final FileOutputStream outFis = new FileOutputStream(outFile)) {
						while ((n = zipIs.read(buffer)) > 0) {
							outFis.write(buffer, 0, n);
						}
						outFis.flush();
					}
				}
				zipIs.closeEntry();
			}
		}
	}

	/**
	 * Obtiene los directorios de los usuarios activos del sistema.
	 * @return Listado con los directorios activos localizados en el sistema.
	 */
	private List<File> getUsersDirectories() {

		// Si ya se han obtenido previamente, devolvemos la lista cacheada
		if (this.usersDirectories != null) {
			return this.usersDirectories;
		}

		// Obtenemos los directorios de los usuarios activos del sistema
		final List<File> userDirPaths = new ArrayList<>();
		final File usersBaseDir = new File(USERS_PATH);
		final File[] userDirs = usersBaseDir.listFiles((current, name) -> new File(current, name).isDirectory());
		if (userDirs != null) {
			for (final File userDir : userDirs) {

				// Nos saltamos siempre los directorios que sabemos que no son de un usuario activo
				if (DEFAULT_WINDOWS_USER_NAME.equalsIgnoreCase(userDir.getName())
						|| ALL_USERS_DIRECTORY.equalsIgnoreCase(userDir.getName())) {
					continue;
				}

				userDirPaths.add(userDir);
			}
		}

		// Guardamos la lista de directorios para no tener que volver a calcularla
		usersDirectories = userDirPaths;

		return usersDirectories;
	}

	/**
	 * Obtiene los ficheros de definicion de perfiles de Firefox ("profiles.ini") de todos los usuarios.
	 * @return Array de directorios con los perfiles de usuario.
	 */
	private List<File> getMozillaProfilesFiles() {

		// Obtenemos los directorios base de perfiles de todos los usuarios activos del sistema
		final ArrayList<File> fileList = new ArrayList<>();
		List<File> userHomeDirs = getUsersDirectories();
		for (File homeDir : userHomeDirs) {
			File profilesFile = new File(homeDir, MOZILLA_PROFILES_RELATIVE_PATH);
			if (profilesFile.isFile()) {
				fileList.add(profilesFile);
			}
		}

		return fileList;
	}

	/**
	 * Devuelve un listado con todos los directorios de perfil de usuario de Firefox.
	 * @return Listado de directorios de perfil perfiles de usuario de Firefox.
	 */
	List<MozillaProfile> getMozillaProfiles() {

		// Si ya se han obtenido previamente, devolvemos la lista cacheada
		if (this.profileDirectories != null) {
			return this.profileDirectories;
		}

		// Obtenemos los perfiles de todos los usuarios del sistema
		final List<MozillaProfile> profiles = new ArrayList<>();
		for (final File profilesIniFile : getMozillaProfilesFiles()) {
			try {
				ProfilesIni profilesIni = new ProfilesIni(profilesIniFile);
				profiles.addAll(profilesIni.getProfilesList());
			} catch (IOException e) {
				LOGGER.warning("No se pudieron cargar los perfiles de Mozilla del fichero " //$NON-NLS-1$
						+ LoggerUtil.getCleanUserHomePath(profilesIniFile.getAbsolutePath()) + ": " + e); //$NON-NLS-1$
			}
		}

		// Guardamos la lista de perfiles para no tener que volver a calcularla
		this.profileDirectories = profiles;

		return profiles;
	}

	private static final String CUSTOM_PROFILE_PREFERENCES_FILENAME = "user.js"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_FILE_HEADER =
			"// === PROPIEDADES PERSONALIZADAS DE CONFIGURACION ===\r\n"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_ENTERPRISE_ROOTS_HEADER =
			"\r\n// Confianza en los certificados raices del almacen del sistema\r\n"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_ENTERPRISE_ROOTS = "security.enterprise_roots.enabled"; //$NON-NLS-1$

	private static final String BREAK_LINE = "\r\n"; //$NON-NLS-1$

	/**
	 * Configur el que se habilite o deshabilite el uso del almac&eacute;n de cofianza del
	 * sistema operativo como almacen de confianza de Firefox.
	 * @param enable {@code true} para habilitar la confianza en los certificados ra&iacute;z del
	 * almac&eacute;n de confianza del sistema adem&aacute;s de en los suyos propios,
	 * {@code false} en caso contrario.
	 * @throws IOException Cuando no se puede crear o editar la configuraci&oacute;n.
	 * @throws MozillaProfileNotFoundException Cuando no se han encontrado perfiles de Firefox.
	 */
	void configureUseSystemTrustStore(final boolean enable) throws IOException, MozillaProfileNotFoundException {

		// Obtenemos el directorio de usuarios
		configureUseSystemTrustStore(enable, getMozillaProfiles());
	}

	/**
	 * Configura el que se habilite o deshabilite el uso del almac&eacute;n de cofianza del
	 * sistema operativo como almacen de confianza de Firefox.
	 * @param enable {@code true} para habilitar la confianza en los certificados ra&iacute;z del
	 * almac&eacute;n de confianza del sistema adem&aacute;s de en los suyos propios,
	 * {@code false} en caso contrario.
	 * @param profiles Listado de perfiles de Firefox.
	 * @throws IOException Cuando no se puede crear o editar la configuraci&oacute;n.
	 * @throws MozillaProfileNotFoundException Cuando no se han encontrado perfiles de Firefox.
	 */
	static void configureUseSystemTrustStore(final boolean enable, final List<MozillaProfile> profiles) throws IOException, MozillaProfileNotFoundException {

		if (profiles.isEmpty()) {
			throw new MozillaProfileNotFoundException();
		}

		// Las preferencias personalizadas se establecen a traves de un fichero user.js en el
		// directorio de perfil de Firefox. Por cada directorio, comprobamos si existe este
		// fichero. Si no existe, se crea con la propiedad personalizada. Si existe, se modifica
		// el valor que tuviese, o se agrega la propiedad si no estuviera.
		for (final MozillaProfile profile : profiles) {
			final File customPrefsFile = new File(profile.getProfileDir(), CUSTOM_PROFILE_PREFERENCES_FILENAME);

			// Si existe el fichero, comprobamos si existe la propiedad
			if (customPrefsFile.isFile()) {

				// Buscamos la propiedad en el fichero y, si existe, cambiamos su valor
				boolean propertyFound = false;
				final StringBuilder customFileContent = new StringBuilder();
				try (InputStream is = new FileInputStream(customPrefsFile);
						Reader isr = new InputStreamReader(is);
						BufferedReader br = new BufferedReader(isr)) {

					String line;
					while ((line = br.readLine()) != null) {
						if (line.contains(MOZ_PREFERENCE_ENTERPRISE_ROOTS)) {
							propertyFound = true;
							customFileContent.append(getUseSystemTrustStoreConfigContent(enable));
						}
						else {
							customFileContent.append(line).append(BREAK_LINE);
						}
					}
				}

				// Si no existe la linea de configuracion, la agregamos
				if (!propertyFound) {
					customFileContent.append(MOZ_PREFERENCE_ENTERPRISE_ROOTS_HEADER)
						.append(getUseSystemTrustStoreConfigContent(enable));
				}

				// Rescribimos el fichero
				try (OutputStream fos = new FileOutputStream(customPrefsFile)) {
					fos.write(customFileContent.toString().getBytes(StandardCharsets.UTF_8));
				}
			}
			// Si no existe el fichero, lo creamos con la propiedad
			else {
				try (OutputStream fos = new FileOutputStream(customPrefsFile)) {
					final String content = MOZ_PREFERENCE_FILE_HEADER
							+ MOZ_PREFERENCE_ENTERPRISE_ROOTS_HEADER
							+ getUseSystemTrustStoreConfigContent(enable);
					fos.write(content.getBytes(StandardCharsets.UTF_8));
				}
			}
		}
	}

	/**
	 * Obtiene la l&iacute;nea de configuraci&oacute;n para activar o desactivar el uso del
	 * almac&eacute;n de confianza del sistema.
	 * @param enable {@code true} para habilitar el almac&eacute;n de confianza del sistema,
	 * {@code false} para desactivarlo.
	 * @return L&iacute;nea de configuraci&oacute;n.
	 */
	private static String getUseSystemTrustStoreConfigContent(final boolean enable) {
		return "user_pref(\"" + MOZ_PREFERENCE_ENTERPRISE_ROOTS //$NON-NLS-1$
				+ "\", " + enable + ");" + BREAK_LINE; //$NON-NLS-1$ //$NON-NLS-2$
	}

	void setParentComponet(final Component parentComponent) {
		this.parentComponent = parentComponent;
	}
}
