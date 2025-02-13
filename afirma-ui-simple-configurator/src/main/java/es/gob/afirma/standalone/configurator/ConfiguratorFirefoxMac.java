/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;

/** Configurador para instalar un certificado SSL de confianza en Mozilla NSS.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s.
 * @author Carlos Gamuci Mill&aacute;n. */
final class ConfiguratorFirefoxMac {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String FILE_AUTOFIRMA_CERTIFICATE = "Autofirma_ROOT.cer"; //$NON-NLS-1$
	private static final String CERTUTIL_RESOURCE = "/osx/certutil.osx.zip"; //$NON-NLS-1$
	private static final String CERTUTIL_RELATIVE_PATH = "certutil/certutil"; //$NON-NLS-1$
	private static final String PROFILES_INI_RELATIVE_PATH = "/Library/Application Support/firefox/profiles.ini";//$NON-NLS-1$

	private static final String COMMAND_EXPORT_PATH = "export PATH=$PATH:";//$NON-NLS-1$
	private static final String COMMAND_EXPORT_LIBRARY_LD = "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:";//$NON-NLS-1$

	private static final String CUSTOM_PROFILE_PREFERENCES_FILENAME = "user.js"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_FILE_HEADER =
			"// === PROPIEDADES PERSONALIZADAS DE CONFIGURACION ===\r\n"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_ENTERPRISE_ROOTS_HEADER =
			"\r\n// Confianza en los certificados raices del almacen del sistema\r\n"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_ENTERPRISE_ROOTS = "security.enterprise_roots.enabled"; //$NON-NLS-1$

	private static final String BREAK_LINE = "\r\n"; //$NON-NLS-1$

	private ConfiguratorFirefoxMac() {
		// No instanciable
	}

	/**
	 * Genera el script de instalaci&oacute;n del certificado en Firefox.
	 * @param appDir Directorio de instalaci&oacute;n.
	 * @param userDirs Directorios de los usuarios del sistema.
	 * @param scriptFile Fichero al que agregar al final el script de desinstalaci&oacute;n.
	 * @param console Consola sobre la que mostrar los mensajes al usuario.
	 * @throws MozillaProfileNotFoundException No se ha encontrado el directorio de perfiles de Mozilla.
	 * @throws IOException Cuando ocurre un error en el tratamiento de datos.
	 */
	static void installOnMozillaKeyStore(final File appDir, final String[] userDirs, final File scriptFile, final Console console)
			throws MozillaProfileNotFoundException, IOException {

		// Preparamos CertUtil para realizar la operacion
		File certutilFile;
		try {
			certutilFile = prepareCertUtil(appDir, scriptFile);
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo preparar CertUtil para la instalacion del certificado SSL en Mozilla Firefox. Se aborta la operacion", e); //$NON-NLS-1$
			return;
		}

		// Obtenemos los directorios de los perfiles de Mozilla
		final File[] profileDirs = getMozillaUsersProfiles(userDirs);

		// Por cada uno de los perfiles, generamos los comandos de instalación del certificado
		// y los ejecutamos
		for (final File profileDir : profileDirs) {

			console.print(Messages.getString("ConfiguratorMacOSX.35", profileDir.getName())); //$NON-NLS-1$

			generateInstallScriptWithCheck(appDir, profileDir, certutilFile, scriptFile);

			try {
				ConfiguratorMacOSX.executeScriptFile(scriptFile, true, false);
			}
			catch (final Exception e) {
				console.print(Messages.getString("ConfiguratorMacOSX.36", profileDir.getName())); //$NON-NLS-1$
				LOGGER.log(Level.WARNING, "No se pudo instalar el certificado en el siguiente perfil de Mozilla: " + profileDir.getName(), e); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Genera el script de desinstalacion de los certificados del almacen de Firefox.
	 * @param appDir Directorio de instalaci&oacute;n.
	 * @param userDirs Directorios de los usuarios del sistema.
	 * @param scriptFile Fichero al que agregar al final el script de desinstalaci&oacute;n.
	 * @throws MozillaProfileNotFoundException No se ha encontrado el directorio de perfiles de Mozilla.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del script.
	 */
	static void uninstallFromMozillaKeyStore(final File appDir, final String[] userDirs, final File scriptFile)
			throws MozillaProfileNotFoundException, IOException {

		// Preparamos CertUtil para realizar la operacion
		File certutilFile;
		try {
			certutilFile = prepareCertUtil(appDir, scriptFile);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo preparar CertUtil para la desinstalacion del certificado SSL en Mozilla Firefox. Se aborta la operacion: " + e); //$NON-NLS-1$
			return;
		}

		// Obtenemos los directorios de los perfiles de Mozilla
		final File[] profileDirs = getMozillaUsersProfiles(userDirs);

		// Por cada uno de los perfiles, generamos los comandos de instalación del certificado
		// y los ejecutamos
		for (final File profileDir : profileDirs) {

			// Generamos el script para eliminar el certificado del almacen de Mozilla
			generateUninstallScript(appDir, profileDir, certutilFile, scriptFile);

			try {
				ConfiguratorMacOSX.executeScriptFile(scriptFile, true, false);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se pudo desinstalar el certificado en el siguiente perfil de Mozilla: " + profileDir.getName(), e); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Genera el script para la instalaci&oacute;n de los certificados. En caso de error,
	 * permite al usuario reintentarlo.
	 * @param appDir Directorio de instalaci&oacute;n.
	 * @param profileDirs Directorios de perfil de Mozilla.
	 * @param certUtilFile Fichero del ejecutable certUtil.
	 * @param scriptFile Fichero al que agregar el script.
	 */
	private static void generateInstallScriptWithCheck (final File appDir,
			                                           final File profileDir,
			                                           final File certUtilFile,
			                                           final File scriptFile) {
		boolean installed = false;
		boolean cancelled = false;
		do {
			try {
				generateInstallScript(appDir, profileDir, certUtilFile, scriptFile);
				installed = true;
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING,
						"No se pudo instalar la CA del certificado SSL para el socket en el almacen de Firefox: " + e, //$NON-NLS-1$
						e);
				final int result = JOptionPane.showConfirmDialog(
						null,
						Messages.getString("ConfiguratorWindows.10"), //$NON-NLS-1$
						Messages.getString("ConfiguratorWindows.1"), //$NON-NLS-1$
						JOptionPane.OK_CANCEL_OPTION,
						JOptionPane.WARNING_MESSAGE);

				if (result == JOptionPane.CANCEL_OPTION) {
					cancelled = true;
					LOGGER.warning(
							"El usuario cancelo la instalacion del certificado SSL para el socket en Firefox: " + e); //$NON-NLS-1$
				}
			}
		} while (!installed && !cancelled);
	}


	/** Ejecuta la utilidad Mozilla CertUtil para la instalaci&oacute;n del certificado
	 * ra&iacute;z de confianza en Firefox.
	 * @param appDir Directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 * @param profileDir Listado de directorio de perfil de usuario de Mozilla Firefox.
	 * @param certUtilFile Fichero del ejecutable certUtil.
	 * @param scriptFile Fichero al que agregar al final el script de desinstalaci&oacute;n.
	 * @throws IOException Cuando ocurre un error en la escritura de los ficheros.
	 */
	private static void generateInstallScript(final File appDir,
			                                    final File profileDir,
		                                        final File certUtilFile,
			                                    final File scriptFile) throws IOException {

		if (!profileDir.isDirectory()) {
			return;
		}

		// Configuramos el script para exportar el PATH para que
		// certUtil encuentre sus dependencias
		final StringBuilder exportPathScript = new StringBuilder();
		exportPathScript.append(COMMAND_EXPORT_PATH);
		exportPathScript.append(certUtilFile.getParentFile().getAbsolutePath());
		ConfiguratorMacUtils.writeScriptFile(exportPathScript, scriptFile, false);

		// Configuramos el script para exportar el LD_LIBRARY_PATH
		// para que certUtil encuentre sus dependencias
		final StringBuilder exportLibraryLdScript = new StringBuilder();
		exportLibraryLdScript.append(COMMAND_EXPORT_LIBRARY_LD);
		exportLibraryLdScript.append(certUtilFile.getParentFile().getAbsolutePath());
		ConfiguratorMacUtils.writeScriptFile(exportLibraryLdScript, scriptFile, true);

		// Si en el directorio del perfil existe el fichero pkcs11.txt entonces se trata
		// de un almacen de certificados compartido SQL
		final boolean sqlDb = new File(profileDir, "pkcs11.txt").exists(); //$NON-NLS-1$

		final StringBuilder installScript = new StringBuilder()
				.append(escapePath(certUtilFile.getAbsolutePath()))
				.append(" -A -d ") //$NON-NLS-1$
				.append(escapePath((sqlDb ? "sql:" : "") + profileDir.getAbsolutePath())) //$NON-NLS-1$ //$NON-NLS-2$
				.append(" -i ") //$NON-NLS-1$
				.append(escapePath(new File(appDir, FILE_AUTOFIRMA_CERTIFICATE).getAbsolutePath()))
				.append(" -n ") //$NON-NLS-1$
				.append("\"").append(ConfiguratorUtil.CERT_ALIAS).append("\"") //$NON-NLS-1$ //$NON-NLS-2$
				.append(" -t  \"C,,\""); //$NON-NLS-1$

		ConfiguratorMacUtils.writeScriptFile(installScript, scriptFile, true);
	}

	/**
	 * Genera el script de desinstalaci&oacute;n de los certificados de la aplicaci&oacute;n.
	 * @param appDir Directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 * @param profileDir Listado de directorio de perfil de usuario de Mozilla Firefox.
	 * @param certUtilFile Fichero del ejecutable certUtil.
	 * @param scriptFile Fichero al final del cual donde se almacenar&aacute; el script de desinstalaci&oacute;n.
	 * @throws IOException Cuando se produce un error al generar el script.
	 */
	private static void generateUninstallScript(final File appDir,
			final File profileDir,
            final File certUtilFile,
			final File scriptFile) throws IOException {

		if (!profileDir.isDirectory()) {
			return;
		}

		// Configuramos el script para exportar el PATH para que
		// certUtil encuentre sus dependencias
		final StringBuilder exportPathScript = new StringBuilder();
		exportPathScript.append(COMMAND_EXPORT_PATH);
		exportPathScript.append(certUtilFile.getParentFile().getAbsolutePath());
		ConfiguratorMacUtils.writeScriptFile(exportPathScript, scriptFile, false);

		// Configuramos el script para exportar el LD_LIBRARY_PATH
		// para que certUtil encuentre sus dependencias
		final StringBuilder exportLibraryLdScript = new StringBuilder();
		exportLibraryLdScript.append(COMMAND_EXPORT_LIBRARY_LD);
		exportLibraryLdScript.append(certUtilFile.getParentFile().getAbsolutePath());
		ConfiguratorMacUtils.writeScriptFile(exportLibraryLdScript, scriptFile, true);

		// Si en el directorio del perfil existe el fichero pkcs11.txt entonces se trata
		// de un almacen de certificados compartido SQL
		final boolean sqlDb = new File(profileDir, "pkcs11.txt").exists(); //$NON-NLS-1$
		final String profileRef = (sqlDb ? "sql:" : "") + profileDir.getAbsolutePath(); //$NON-NLS-1$ //$NON-NLS-2$

		final StringBuilder uninstallScript = new StringBuilder()
				.append("max=$(") //$NON-NLS-1$
				.append(escapePath(certUtilFile.getAbsolutePath()))
				.append(" -L -d ") //$NON-NLS-1$
				.append(escapePath(profileRef))
				.append(" | grep Autofirma | wc -l);for ((i=0; i<$max; i++));do ") //$NON-NLS-1$
				.append(escapePath(certUtilFile.getAbsolutePath()))
				.append(" -D -d ") //$NON-NLS-1$
				.append(escapePath(profileRef))
				.append(" -n \"" + ConfiguratorUtil.CERT_ALIAS + "\";done"); //$NON-NLS-1$ //$NON-NLS-2$

		ConfiguratorMacUtils.writeScriptFile(uninstallScript, scriptFile, true);
	}

	/** Obtiene una referencia a una instancia de CertUtil v&aacute;lida para su ejecuci&oacute;n.
	 * @param appDir Ruta del ejecutable CertUtil.
	 * @param scriptFile <i>Script</i> para exportar el <code>LD_LIBRARY_PATH</code>.
	 * @return certutilFile Fichero ejecutable CertUtil.
	 * @throws IOException Se lanza cuando CertUtil no existe o no se puede ejecutar. */
	private static File prepareCertUtil(final File appDir, final File scriptFile) throws IOException {

		final File certutilFile = new File(appDir, CERTUTIL_RELATIVE_PATH);
		if (!certutilFile.isFile()) {
			ConfiguratorUtil.uncompressResource(CERTUTIL_RESOURCE, appDir);
		}

		if (!certutilFile.isFile()) {
			throw new IOException("No se encuentra CertUtil para la instalacion en Firefox"); //$NON-NLS-1$
		}

		if (!certutilFile.canExecute()) {
			ConfiguratorMacUtils.addExexPermissionsToAllFilesOnDirectory(certutilFile.getParentFile());
		}

		if (!certutilFile.canExecute()) {
			throw new IOException("El ejecutable CertUtil no tiene permisos de ejecucion"); //$NON-NLS-1$
		}

		return certutilFile;
	}

	/** Obtiene los directorios de perfil de Mozilla del sistema.
	 * @param userDirs Directorios de los usuarios de los que obtener los perfiles.
	 * @return Listado de directorios de perfil.
	 * @throws MozillaProfileNotFoundException Cuando no se encuentran directorios de perfil. */
	private static File[] getMozillaUsersProfiles(final String[] userDirs) throws MozillaProfileNotFoundException {

		// Obtenemos el listado de ficheros "profiles.ini"
		final List<File> mozillaUsersProfilesPath = getProfilesIniFiles(userDirs);
		// Si no se encuentra el fichero de perfiles de firefox, abortamos la operacion
		if (mozillaUsersProfilesPath == null) {
			throw new MozillaProfileNotFoundException();

		}

		// Obtenemos todos los perfiles de usuario de Mozilla
		final List<File> profiles = getProfileDirs(mozillaUsersProfilesPath);
		if (profiles.isEmpty()){
			throw new MozillaProfileNotFoundException();
		}

		return profiles.toArray(new File[profiles.size()]);
	}

	/** Devuelve un listado con los ficheros <i>profiles.ini</i> de Mozilla.
	 * @param userDirs Listado de directorios de usuario del sistema.
	 * @return Listado de ficheros <i>profiles.ini</i>. */
	private static List<File> getProfilesIniFiles(final String[] userDirs){
		final List<File> path = new ArrayList<>();
		for (final String userDir : userDirs){
			final File profilesIniFile = new File(userDir, PROFILES_INI_RELATIVE_PATH);
			// comprobamos que el fichero exista
			if (profilesIniFile.exists() && profilesIniFile.isFile()){
				path.add(profilesIniFile);
			}
		}
		return path;
	}

	/** Devuelve un listado de directorios donde se encuentran los perfiles de usuario de Mozilla.
	 * @param profilesIniFiles Listado de directorios que contienen un fichero <i>profiles.ini</i>.
	 * @return Listado de directorios donde se encuentran los perfiles de usuario de Firefox. */
	private static List<File> getProfileDirs(final List<File> profilesIniFiles){
		final String PATH = "Path="; //$NON-NLS-1$
		final List<File> profileDirs = new ArrayList<>();
		for (final File path: profilesIniFiles){
			String line;
			try (
				final InputStream resIs = new FileInputStream(path);
				final BufferedReader resReader = new BoundedBufferedReader(
					new InputStreamReader(resIs),
					256, // Maximo 256 lineas de salida (256 perfiles por "profiles.ini")
					2048 // Maximo 2048 caracteres por linea
				);
			) {
				while ((line = resReader.readLine()) != null) {
					if (line.startsWith(PATH)){
						final File file = new File(
							path.getAbsolutePath().substring(
								0, path.getAbsolutePath().lastIndexOf("/") + 1) + line.substring(PATH.length() //$NON-NLS-1$
							)
						);
						if (file.exists() && file.isDirectory()){
							profileDirs.add(file);
						}
					}
				}
			}
			catch (final Exception e) {
				LOGGER.warning("Error al buscar los directorios de perfiles de Firefox: " + e); //$NON-NLS-1$
			}
		}
		return profileDirs;
	}

	/**
	 * Escapa una ruta de fichero para poder utilizarla como parte de un
	 * comando de consola.
	 * @param path Ruta a escapar.
	 * @return Ruta escapada.
	 */
	private static String escapePath(final String path) {
		if (path == null) {
			throw new IllegalArgumentException("La ruta a 'escapar' no puede ser nula"); //$NON-NLS-1$
		}
		return path.replace(" ", "\\ "); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * Configur el que se habilite o deshabilite el uso del almac&eacute;n de cofianza del
	 * sistema operativo como almacen de confianza de Firefox.
	 * @param enable {@code true} para habilitar la confianza en los certificados ra&iacute;z del
	 * almac&eacute;n de confianza del sistema adem&aacute;s de en los suyos propios,
	 * {@code false} en de que s&oacute;lo se desee confiar en el almac&eacute;n del navegador.
	 * @param userDirs Listado de directorios de de usuarios del sistema.
	 * @param window Consola en la que se mostrar&aacute;n los mensajes de progreso.
	 * @throws IOException Cuando no se puede crear o editar la configuraci&oacute;n.
	 * @throws MozillaProfileNotFoundException Cuando no se han encontrado perfiles de Firefox.
	 */
	static void configureUseSystemTrustStore(final boolean enable, final String[] userDirs, final Console window)
			throws IOException, MozillaProfileNotFoundException {

		// Obtenemos el listado de perfiles de Firefox
		final File[] mozillaProfileDirs = getMozillaUsersProfiles(userDirs);

		if (mozillaProfileDirs == null || mozillaProfileDirs.length == 0) {
			throw new MozillaProfileNotFoundException("No se han encontrado perfiles de Mozilla en el sistema"); //$NON-NLS-1$
		}

		if (enable) {
			window.print(Messages.getString("ConfiguratorWindows.19")); //$NON-NLS-1$
		}
		else {
			window.print(Messages.getString("ConfiguratorWindows.20")); //$NON-NLS-1$
		}

		// Las preferencias personalizadas se establecen a traves de un fichero user.js en el
		// directorio de perfil de Firefox. Por cada directorio, comprobamos si existe este
		// fichero. Si no existe, se crea con la propiedad personalizada. Si existe, se modifica
		// el valor que tuviese, o se agrega la propiedad si no estuviera.
		for (final File profileDir : mozillaProfileDirs) {
			final File customPrefsFile = new File(profileDir, CUSTOM_PROFILE_PREFERENCES_FILENAME);

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
}
