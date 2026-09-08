/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.restoreconfig;

import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.keystores.mozilla.MozillaProfile;
import es.gob.afirma.keystores.mozilla.ProfilesIni;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;
import es.gob.afirma.standalone.so.macos.ShellScript;
import es.gob.afirma.standalone.so.macos.UnixUtils;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;


/**
 * Restaurador de la instalaci&oacute;n de la integraci&oacute;n de la aplicaci&oacute;n con Firefox. */
final class RestoreConfigFirefoxMacOS {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String[] MOZILLA_PROFILES_RELATIVE_PATH = new String[] {
			"/Library/Application Support/firefox/profiles.ini" //$NON-NLS-1$
	};
	private static final String PROFILES_INI_PATH_PREFIX = "Path="; //$NON-NLS-1$

	static final String COMMAND_EXPORT_PATH = "export PATH=$PATH:";//$NON-NLS-1$
	static final String COMMAND_EXPORT_LIBRARY_LD = "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:";//$NON-NLS-1$

	private static final String DIR_CERTUTIL = "certutil"; //$NON-NLS-1$
	private static final String CERTUTIL_EXE = "certutil"; //$NON-NLS-1$;
	private static final String RESOURCE_CERTUTIL = "/osx/certutil.osx.zip"; //$NON-NLS-1$

	// --------------
	// -- Propiedades usadas para configurar FireFox para que confie en los certificados del Llavero
	// --------------

	private static final String CUSTOM_PROFILE_PREFERENCES_FILENAME = "user.js"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_FILE_HEADER =
			"// === PROPIEDADES PERSONALIZADAS DE CONFIGURACION ===\r\n"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_ENTERPRISE_ROOTS_HEADER =
			"\r\n// Confianza en los certificados raices del almacen del sistema\r\n"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_ENTERPRISE_ROOTS = "security.enterprise_roots.enabled"; //$NON-NLS-1$
	private static final String BREAK_LINE = "\r\n"; //$NON-NLS-1$


	private final List<File> usersDirs;
	private final File scriptFile;
	private final RestoreConfigPanel restorePanel;

	private List<MozillaProfile> profilesDirs = null;

	/**
	 * Construye el restaurador de Firefox para macOS.
	 * @param userDirs Listado de directorios de usuario.
	 * @param scriptFile Fichero del script en el que almacenar los comandos para la restauraci&oacute;n.
	 * @param restorePanel Panel en el que mostrar el avance del proceso de restauraci&oacute;n.
	 */
	public RestoreConfigFirefoxMacOS(final List<File> userDirs, File scriptFile, final RestoreConfigPanel restorePanel) {
		this.usersDirs = userDirs;
		this.scriptFile = scriptFile;
		this.restorePanel = restorePanel;
	}

	/**
	 * Instala un certificado en el almac&eacute;n de confianza de Firefox. La instalaci&oacute;n se realiza en los
	 * perfiles indicados por medio de la ejecuci&oacute;n de un script. Es probable que el usuario deba introducir
	 * la contrase&ntilde;a de administrador para permitir la ejecuci&oacute;n del script.
	 * @param workingDir Directorio de trabajo con permisos de escritura para generar ficheros temporales.
	 * @param certFile Fichero del certificado que debemos instalar.
	 * @param certUtilPath Ruta de CertUtil.
	 * @param profile Perfile de usuario de Firefox en el que instalar el certificado.
	 * @throws PasswordProtectedException Cuando el perfil de Firefox est&aacute; protegido con contrase&ntilde;a maestra y no se ha podido introducir.
	 * @throws IOException Cuando no se ha podido copiar CertUtil al directorio de aplicaci&oacute;n.
	 */
	public void installRootCAMozillaKeyStore(final File workingDir, final File certFile,
											 final MozillaProfile profile, final String certUtilPath)
			throws IOException, PasswordProtectedException {

		// Instalamos el certificado en el perfil de Firefox
		this.restorePanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.36", profile.getName())); //$NON-NLS-1$

		if (!profile.isPrepared()) {
			RestoreConfigFirefoxCommon.initProfile(workingDir, profile, certUtilPath, false,false, this.restorePanel);
		}

		if (profile.hasMasterPassword()) {
			throw new PasswordProtectedException("El perfil '" + profile.getName() + "' esta protegido por contrasena"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// Preparamos el comando para instalar el nuevo certificado
		String command = getImportCertCommand(escapePath(certUtilPath), profile, certFile);
		RestoreConfigMacOSXUtils.writeScriptFile(this.scriptFile, command, true);
	}

	/**
	 * Desinstala un certificado en el almac&eacute;n de confianza de Firefox. La instalaci&oacute;n se realiza en los
	 * perfiles indicados por medio de la ejecuci&oacute;n de un script. Es probable que el usuario deba introducir
	 * la contrase&ntilde;a de administrador para permitir la ejecuci&oacute;n del script.
	 * @param workingDir Directorio de trabajo con permisos de escritura para generar ficheros temporales.
	 * @param profile Perfile de usuario de Firefox en el que instalar el certificado.
	 * @param certUtilPath Ruta de CertUtil.
	 * @throws IOException Cuando no se ha podido copiar CertUtil al directorio de aplicaci&oacute;n.
	 */
	public void uninstallRootCAMozillaKeyStore(final File workingDir, final MozillaProfile profile, final String certUtilPath)
			throws IOException {

		if (!profile.hasNssKeyStore()) {
			LOGGER.info("Se omite la desinstalacion en el perfil '" + profile.getName() + "' por no haberse inicializado nunca antes"); //$NON-NLS-1$
			return;
		}

		if (!profile.isPrepared()) {
			RestoreConfigFirefoxCommon.initProfile(workingDir, profile, certUtilPath, false,false, this.restorePanel);
		}

		// Preparamos el comando para la eliminacion del certificado anterior si existiese
		String command = getRemoveCertCommand(escapePath(certUtilPath), profile);
		RestoreConfigMacOSXUtils.writeScriptFile(this.scriptFile, command, true);
	}

	/**
	 * Compone el comando de CertUtil para eliminar un certificado del almac&eacute;n de confianza de Firefox.
	 * @param certUtilPath Ruta de CertUtil.
	 * @param profile Perfil de usuario de Mozilla Firefox.
	 * @return Comando de CertUtil para eliminar el certificado del almac&eacute;n de confianza de Firefox.
	 */
	private static String getRemoveCertCommand(final String certUtilPath, final MozillaProfile profile) {

		final String alias = ConfiguratorUtil.CERT_ALIAS;

		// Si en el directorio del perfil existe el fichero pkcs11.txt entonces se trata
		// de un almacen de certificados compartido SQL
		final boolean sqlDb = new File(profile.getProfileDir(), "pkcs11.txt").exists(); //$NON-NLS-1$
		final String profileRef = (sqlDb ? "sql:" : "") //$NON-NLS-1$ //$NON-NLS-2$
				+ escapePath(profile.getProfileDir().getAbsolutePath());

		return String.format(
				"max=$(%1$s -L -d %2$s %4$s | grep Autofirma | wc -l);" //$NON-NLS-1$
				+ "for ((i=0; i<$max; i++));" //$NON-NLS-1$
				+ "do %1$s -D -d %2$s -n \"%3$s\";" //$NON-NLS-1$
				+ "done", //$NON-NLS-1$
				certUtilPath, profileRef, alias);
	}

	/**
	 * Compone el comando de CertUtil para importar un certificado en el almac&eacute;n de confianza de Firefox.
	 * @param certUtilPath Ruta de CertUtil.
	 * @param profile Perfil de usuario de Mozilla Firefox.
	 * @param certFile Fichero del certificado a importar.
	 * @return Comando de CertUtil para importar el certificado en el almac&eacute;n de confianza de Firefox.
	 */
	private static String getImportCertCommand(final String certUtilPath, final MozillaProfile profile, final File certFile) {

		final String certPath = escapePath(certFile.getAbsolutePath());
		final String profilePath = escapePath(profile.getProfileDir().getAbsolutePath());
		final String alias = ConfiguratorUtil.CERT_ALIAS;

		String passwordParam = profile.getPasswordFile() != null
				? " -f " + escapePath(profile.getPasswordFile().getAbsolutePath())
				: ""; //$NON-NLS-1$ //$NON-NLS-2$

		return String.format(
				"%s -A -d sql:%s -i %s -n \"%s\" %s -t \"C,,\"", //$NON-NLS-1$
				certUtilPath, profilePath, certPath, alias, passwordParam);
	}

	/**
	 * Prepara la version interna de certutil y devuelve la ruta en al que se encuentra.
	 * @param appDir Directorio en el que preparar certUtil.
	 * @return Ruta absoluta del ejecutable de certutil.
	 * @throws IOException Si no se ha podido preparar el ejecutable.
	 */
	String prepareCertUtil(final File appDir) throws IOException {

		// Identificamos el directorio de certutil
		final File certUtilDir = new File(appDir, DIR_CERTUTIL);

		// Copiamos la version interna de certutil al directorio (incluso si ya existia)
		copyCertUtils(certUtilDir);

		// Identificamos el ejecutable
		final File certutilFile = new File(certUtilDir, CERTUTIL_EXE);

		// Comprobamos que el ejecutable exista y tenga permisos de ejecucion
		checkCertutilExe(certutilFile);

		// Agregamos al PATH la ruta del directorio de certutil para que no haya problemas de dependencias
		addCertUtilDirInPath(certUtilDir.getAbsolutePath());

		// Devolvemos la ruta escapada
		return certutilFile.getAbsolutePath();
	}

	/**
	 * Comprueba que el ejecutable de certutil exista y puede ejecutarse.
	 * @param certutilFile Fichero del ejecutable CertUtil.
	 * @throws IOException Se lanza cuando hay un problema con el fichero CertUtil.
	 */
	private static void checkCertutilExe(final File certutilFile) throws IOException {

		if (!certutilFile.isFile()) {
			throw new IOException("No se encuentra el ejecutable CertUtil para la instalacion en Firefox"); //$NON-NLS-1$
		}

		if (!certutilFile.canExecute()) {
			UnixUtils.addExexPermissionsToAllFilesOnDirectory(certutilFile.getParentFile());
		}

		if (!certutilFile.canExecute()) {
			throw new IOException("No hay permisos de ejecucion para Mozilla CertUtil"); //$NON-NLS-1$
		}
	}

	/**
	 * Agrega al PATH la ruta del directorio de certutil.
	 * @param certUtilDir  Ruta del certutil ya escapada.
	 * @throws IOException Si ocurre un problema al escribir el script.
	 */
	private void addCertUtilDirInPath(final String certUtilDir) throws IOException {
		String escapedCertUtilDirPath = escapePath(certUtilDir);
		final String exportPath = COMMAND_EXPORT_PATH + escapedCertUtilDirPath;
		RestoreConfigMacOSXUtils.writeScriptFile(this.scriptFile, exportPath, false);
		final String exportLibraryPath = COMMAND_EXPORT_LIBRARY_LD + escapedCertUtilDirPath;
		RestoreConfigMacOSXUtils.writeScriptFile(this.scriptFile, exportLibraryPath, true);
	}

	 /**
	  * Descomprime y copia los ficheros de configuraci&oacute;n de certutil.
	  * @param certUtilDir Directorio al que descomprimir las herramientas de configuraci&oacute;n.
	  * @throws IOException Cuando ocurre un error al descomprimir o copiar.
	  */
	 private static void copyCertUtils(final File certUtilDir) throws IOException {

		 if (!certUtilDir.exists()) {
			 uncompressInternalZip(RESOURCE_CERTUTIL, certUtilDir.getParentFile());
			 UnixUtils.addExexPermissionsToAllFilesOnDirectory(certUtilDir);
		 }
	 }

	 /**
	  * Descomprime un fichero ZIP de recurso al disco.
	  * @param zipResourcePath Ruta del recurso ZIP.
	  * @param outDir Directorio local en el que descomprimir.
	  * @throws IOException Cuando ocurre un error al descomprimir.
	  **/
	 private static void uncompressInternalZip(final String zipResourcePath, final File outDir) throws IOException {
		 int n;
		 ZipEntry entry;
		 final byte[] buffer = new byte[1024];
		 try (final ZipInputStream zipIs = new ZipInputStream(
				 RestoreConfigFirefoxMacOS.class.getResourceAsStream(zipResourcePath));) {

			 while ((entry = zipIs.getNextEntry()) != null) {
				 final File outFile = new File(outDir, entry.getName()).getCanonicalFile();

				 if (!isParent(outDir, outFile)) {
					 zipIs.closeEntry();
					 throw new IOException("Se ha encontrado en el archivo comprimido una ruta que apuntaba fuera del directorio de destino"); //$NON-NLS-1$
				 }

				 if (entry.isDirectory()) {
					 outFile.mkdirs();
				 }
				 else {
					 if (!outFile.getParentFile().exists()) {
						 outFile.getParentFile().mkdirs();
					 }
					 try (final FileOutputStream outFis = new FileOutputStream(outFile);) {
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
	  * Comprueba que el fichero {@code parentFile} es un directorio padre de la
	  * ruta de {@code childFile}.
	  * @param parentDir Directorio padre.
	  * @param childFile Fichero/directorio hijo.
	  * @return {@code true} cuando el directorio forma parte de la ruta de directorio,
	  * {@code false} en caso contrario.
	  * @throws IOException Cuando no se pueda canonizar el nombre de fichero hijo.
	  */
	 private static boolean isParent(final File parentDir, final File childFile) throws IOException {

		 final File parent = parentDir.getCanonicalFile();
		 File intermediateDir = childFile.getCanonicalFile();
		 while (intermediateDir != null && !intermediateDir.equals(parent)) {
			 intermediateDir = intermediateDir.getParentFile();
		 }
		 return intermediateDir != null;
	 }

	/**
	 * Escapa los espacios en blanco de una ruta de fichero.
	 * @param path Ruta de fichero.
	 * @return Ruta escapada.
	 */
	private static String escapePath(final String path) {
		return path.replace(" ", "\\ "); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * Devuelve un listado con todos los directorios de perfil de usuario de Firefox.
	 * @return Listado de directorios de perfil perfiles de usuario de Firefox.
	 */
	List<MozillaProfile> getMozillaProfiles() {

		// Si ya se han obtenido previamente, devolvemos la lista cacheada
		if (this.profilesDirs != null) {
			return this.profilesDirs;
		}

		// Obtenemos los perfiles de todos los usuarios del sistema
		final List<MozillaProfile> profiles = new ArrayList<>();
		for (final File profilesIniFile : getMozillaProfilesFiles(this.usersDirs)) {
			try {
				ProfilesIni profilesIni = new ProfilesIni(profilesIniFile);
				profiles.addAll(profilesIni.getProfilesList());
			} catch (IOException e) {
				LOGGER.warning("No se pudieron cargar los perfiles de Mozilla del fichero " //$NON-NLS-1$
						+ LoggerUtil.getCleanUserHomePath(profilesIniFile.getAbsolutePath()) + ": " + e); //$NON-NLS-1$
			}
		}

		// Guardamos la lista de perfiles para no tener que volver a calcularla
		this.profilesDirs = profiles;

		return profiles;
	}

	/**
	 * Obtiene los ficheros de definicion de perfiles de Firefox ("profiles.ini") de todos los usuarios.
	 * @param userHomeDirs Listado de directorios de usuario.
	 * @return Array de directorios con los perfiles de usuario.
	 */
	private List<File> getMozillaProfilesFiles(List<File> userHomeDirs) {

		final List<File> fileList = new ArrayList<>();

		// Obtenemos los ficheros de perfiles de todos los usuarios activos del sistema
		for (final File homeDir : userHomeDirs) {
			for (final String profileIniSubPath : MOZILLA_PROFILES_RELATIVE_PATH) {
				final File profilesIniFile = new File(homeDir, profileIniSubPath);
				if (profilesIniFile.isFile()) {
					fileList.add(profilesIniFile);
				}
			}
		}

		return fileList;
	}

	/**
	 * Configura el que se habilite o deshabilite el uso del almac&eacute;n de cofianza del
	 * sistema operativo como almacen de confianza de Firefox.
	 * @param enable {@code true} para habilitar la confianza en los certificados ra&iacute;z del
	 * almac&eacute;n de confianza del sistema adem&aacute;s de en los suyos propios,
	 * {@code false} en caso contrario.
	 * @throws MozillaProfileNotFoundException Cuando no se han encontrado perfiles de Firefox.
	 */
	public void configureUseSystemTrustStore(final boolean enable) throws MozillaProfileNotFoundException {

		// Si no hubiese directorios de perfil de Firefox es que no estaria instalado
		final List <MozillaProfile> mozillaProfiles = getMozillaProfiles();
		if (mozillaProfiles.isEmpty()) {
			throw new MozillaProfileNotFoundException();
		}

		// Las preferencias personalizadas se establecen a traves de un fichero user.js en el
		// directorio de perfil de Firefox. Por cada directorio, comprobamos si existe este
		// fichero. Si no existe, se crea con la propiedad personalizada. Si existe, se modifica
		// el valor que tuviese, o se agrega la propiedad si no estuviera.
		for (final MozillaProfile profile : mozillaProfiles) {

			this.restorePanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.38", profile.getName())); //$NON-NLS-1$
			try {
				configureUseSystemTrustStore(profile, enable);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se pudo editar la confianza de Firefox para el perfil: " + LoggerUtil.getCleanUserHomePath(profile.getName()), e); //$NON-NLS-1$
				this.restorePanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.39", profile.getName())); //$NON-NLS-1$
			}
		}
	}

	private void configureUseSystemTrustStore(final MozillaProfile profile, final boolean enable) throws IOException {
		final File customPrefsFile = new File(profile.getProfileDir(), CUSTOM_PROFILE_PREFERENCES_FILENAME);

		// Si existe el fichero, comprobamos si existe la propiedad
		if (customPrefsFile.isFile()) {

			// Buscamos la propiedad en el fichero y, si existe, cambiamos su valor
			boolean propertyFound = false;
			final StringBuilder customFileContent = new StringBuilder();
			try (InputStream is = new FileInputStream(customPrefsFile);
					Reader isr = new InputStreamReader(is);
					BufferedReader br = new BufferedReader(isr);) {

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

			// Comprobamos que tengamos permiso de escritura sobre el fichero y le concedemos permiso en caso de no tenerlo
			editFile(customPrefsFile, customFileContent.toString().getBytes(StandardCharsets.UTF_8));

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


	/**
	 * Sustituimos el contenido del fichero por uno nuevo.
	 * @param file Fichero a editar.
	 * @param content Nuevo contenido
	 * @throws IOException Cuando no se pueda editar el fichero.
	 */
	private void editFile(final File file, final byte[] content) throws IOException {

		// Si no tenemos permisos de escritura es probablemente porque el perfile fue creado
		// al ejecutarse en modo administrador, así que debemos convertir al usuario en el propietario
		// IMPORTANTE: Este codigo solo se ejecuta sobre los perfiles de Mozilla del usuario
		// activo
		if (!file.canWrite() && !isMine(file)) {
			makeFileMine(file);
		}

		// Rescribimos el fichero
		try (OutputStream fos = new FileOutputStream(file)) {
			fos.write(content);
		}
	}

	/**
	 * Indica si un fichero pertenece al usuario.
	 * @param file Fichero a comprobar.
	 * @return {@code true} si el fichero pertenece al usuario o si no puede comprobarse,
	 * {@code false} en caso contrario.
	 */
	private static boolean isMine(final File file) {

		try {
			final String user = getUser();
			return user == null || user.equals(Files.getOwner(file.toPath()).getName());
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido comprobar el propietario del fichero: " + e); //$NON-NLS-1$
			return true;
		}
	}

	private void makeFileMine(final File file) {

		try {
			final String user = getUser();
			if (user != null) {
				final String filePath = escapePath(file.getAbsolutePath());
				final String command = String.format("chown -f %s %s", user, filePath); //$NON-NLS-1$
				RestoreConfigMacOSXUtils.writeScriptFile(this.scriptFile, command, false);
				final ShellScript script = new ShellScript(this.scriptFile, false);
				script.runAsAdministrator();
			}
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido cambiar el propietario del fichero para obtener el permiso de escritura: " + e); //$NON-NLS-1$
		}
	}

	/**
	 * Devuelve el nombre de usuario.
	 * @return Nombre de usuario.
	 */
	private static String getUser() {
		return System.getenv("USER"); //$NON-NLS-1$
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
}
