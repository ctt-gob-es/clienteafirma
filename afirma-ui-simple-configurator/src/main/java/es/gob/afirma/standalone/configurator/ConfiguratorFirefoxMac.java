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
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.BoundedBufferedReader;

/** Configurador para instalar un certificado SSL de confianza en Mozilla NSS.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s.
 * @author carlos.gamuci Carlos Gamuci Mill&aacute;n. */
final class ConfiguratorFirefoxMac {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String FILE_AUTOFIRMA_CERTIFICATE = "AutoFirma_ROOT.cer"; //$NON-NLS-1$
	private static final String CERTUTIL_RESOURCE = "/osx/certutil.osx.zip"; //$NON-NLS-1$
	private static final String CERTUTIL_RELATIVE_PATH = "certutil/certutil"; //$NON-NLS-1$
	private static final String PROFILES_INI_RELATIVE_PATH = "/Library/Application Support/firefox/profiles.ini";//$NON-NLS-1$

	private static final String COMMAND_EXPORT_PATH = "export PATH=$PATH:";//$NON-NLS-1$
	private static final String COMMAND_EXPORT_LIBRARY_LD = "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:";//$NON-NLS-1$

	private ConfiguratorFirefoxMac() {
		// No instanciable
	}

	/**
	 * Genera el script de instalaci&oacute;n del certificado en Firefox.
	 * @param appDir Directorio de instalaci&oacute;n.
	 * @param userDirs Directorios de los usuarios del sistema.
	 * @param scriptFile Fichero al que agregar al final el script de desinstalaci&oacute;n.
	 * @throws MozillaProfileNotFoundException No se ha encontrado el directorio de perfiles de Mozilla.
	 * @throws IOException Cuando ocurre un error en el tratamiento de datos.
	 */
	static void createScriptToInstallOnMozillaKeyStore(final File appDir, final String[] userDirs, final File scriptFile)
			throws MozillaProfileNotFoundException, IOException {

		// Obtenemos los directorios de usuario de Mozilla
		final File[] profileDirs = getMozillaUsersProfiles(userDirs);

		// Generamos el script para instalar el certificado en el almacen de Mozilla
		generateInstallScriptWithCheck(appDir, profileDirs, scriptFile);
	}

	/**
	 * Genera el script de desinstalacion de los certificados del almacen de Firefox.
	 * @param appDir Directorio de instalaci&oacute;n.
	 * @param userDirs Directorios de los usuarios del sistema.
	 * @param scriptFile Fichero al que agregar al final el script de desinstalaci&oacute;n.
	 * @throws MozillaProfileNotFoundException No se ha encontrado el directorio de perfiles de Mozilla.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del script.
	 */
	static void createScriptToUnistallFromMozillaKeyStore(final File appDir, final String[] userDirs, final File scriptFile)
			throws MozillaProfileNotFoundException, IOException {

		// Obtenemos los directorios de usuario de Mozilla
		final File[] profileDirs = getMozillaUsersProfiles(userDirs);

		// Generamos el script para eliminar el certificado del almacen de Mozilla
		generateUninstallScript(appDir, profileDirs, scriptFile);
	}

	/**
	 * Genera el script para la instalaci&oacute;n de los certificados. En caso de error,
	 * permite al usuario reintentarlo.
	 * @param appDir Directorio de instalaci&oacute;n.
	 * @param profileDirs Directorios de perfil de Mozilla.
	 * @param scriptFile Fichero al que agregar el script.
	 */
	private static void generateInstallScriptWithCheck (final File appDir,
			                                           final File[] profileDirs,
			                                           final File scriptFile) {
		boolean installed = false;
		boolean cancelled = false;
		do {
			try {
				generateInstallScript(appDir, profileDirs, scriptFile);
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
	 * @param profileDirs Listado de directorios de perfiles de usuario de Mozilla Firefox.
	 * @param scriptFile Fichero al que agregar al final el script de desinstalaci&oacute;n.
	 * @throws IOException Cuando ocurre un error en la escritura de los ficheros.
	 */
	private static void generateInstallScript(final File appDir,
			                                    final File[] profileDirs,
			                                    final File scriptFile) throws IOException {

		File certutilFile;
		try {
			certutilFile = prepareCertUtil(appDir, scriptFile);
		}
		catch (final Exception e) {
			LOGGER.warning("Se omite la configuracion del certificado SSL en Mozilla Firefox: " + e); //$NON-NLS-1$
			return;
		}

		for (final File profileDir : profileDirs) {
			if (!profileDir.isDirectory()) {
				continue;
			}

			// Si en el directorio del perfil existe el fichero pkcs11.txt entonces se trata
			// de un almacen de certificados compartido SQL
			final boolean sqlDb = new File(profileDir, "pkcs11.txt").exists(); //$NON-NLS-1$

			final StringBuilder installScript = new StringBuilder()
					.append(escapePath(certutilFile.getAbsolutePath()))
					.append(" -A -d ") //$NON-NLS-1$
					.append(escapePath((sqlDb ? "sql:" : "") + profileDir.getAbsolutePath())) //$NON-NLS-1$ //$NON-NLS-2$
					.append(" -i ") //$NON-NLS-1$
					.append(escapePath(new File(appDir, FILE_AUTOFIRMA_CERTIFICATE).getAbsolutePath()))
					.append(" -n ") //$NON-NLS-1$
					.append("\"").append(ConfiguratorUtil.CERT_ALIAS).append("\"") //$NON-NLS-1$ //$NON-NLS-2$
					.append(" -t  \"C,,\""); //$NON-NLS-1$

			ConfiguratorMacUtils.writeScriptFile(installScript, scriptFile.getAbsolutePath(), true);
		}
	}

	/**
	 * Genera el script de desinstalaci&oacute;n de los certificados de la aplicaci&oacute;n.
	 * @param appDir Directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 * @param profileDirs Directorios de perfil de Mozilla.
	 * @param scriptFile Fichero al final del cual donde se almacenar&aacute; el script de desinstalaci&oacute;n.
	 * @throws IOException Cuando se produce un error al generar el script.
	 */
	private static void generateUninstallScript(final File appDir,
												final File[] profileDirs,
												final File scriptFile) throws IOException {

		File certutilFile;
		try {
			certutilFile = prepareCertUtil(appDir, scriptFile);
		}
		catch (final Exception e) {
			LOGGER.warning("Se omite la configuracion del certificado SSL en Mozilla Firefox: " + e); //$NON-NLS-1$
			return;
		}

		for (final File profileDir : profileDirs) {
			if (!profileDir.isDirectory()) {
				continue;
			}

			// Si en el directorio del perfil existe el fichero pkcs11.txt entonces se trata
			// de un almacen de certificados compartido SQL
			final boolean sqlDb = new File(profileDir, "pkcs11.txt").exists(); //$NON-NLS-1$
			final String profileRef = (sqlDb ? "sql:" : "") + profileDir.getAbsolutePath(); //$NON-NLS-1$ //$NON-NLS-2$

			final StringBuilder uninstallScript = new StringBuilder()
					.append("max=$(") //$NON-NLS-1$
					.append(escapePath(certutilFile.getAbsolutePath()))
					.append(" -L -d ") //$NON-NLS-1$
					.append(escapePath(profileRef))
					.append(" | grep AutoFirma | wc -l);for ((i=0; i<$max; i++));do ") //$NON-NLS-1$
					.append(escapePath(certutilFile.getAbsolutePath()))
					.append(" -D -d ") //$NON-NLS-1$
					.append(escapePath(profileRef))
					.append(" -n \"SocketAutoFirma\";done"); //$NON-NLS-1$

			ConfiguratorMacUtils.writeScriptFile(uninstallScript, scriptFile.getAbsolutePath(), true);
		}
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

		// Configuramos el script para exportar el PATH para que
		// certUtil encuentre sus dependencias
		final StringBuilder exportPathScript = new StringBuilder();
		exportPathScript.append(COMMAND_EXPORT_PATH);
		exportPathScript.append(certutilFile.getParentFile().getAbsolutePath());
		ConfiguratorMacUtils.writeScriptFile(exportPathScript, scriptFile.getAbsolutePath(), true);

		// Configuramos el script para exportar el LD_LIBRARY_PATH
		// para que certUtil encuentre sus dependencias
		final StringBuilder exportLibraryLdScript = new StringBuilder();
		exportLibraryLdScript.append(COMMAND_EXPORT_LIBRARY_LD);
		exportLibraryLdScript.append(certutilFile.getParentFile().getAbsolutePath());
		ConfiguratorMacUtils.writeScriptFile(exportLibraryLdScript, scriptFile.getAbsolutePath(), true);

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
}
