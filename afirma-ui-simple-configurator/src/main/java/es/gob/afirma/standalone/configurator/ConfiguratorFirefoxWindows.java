/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.awt.*;
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
import java.security.GeneralSecurityException;
import java.security.KeyStoreException;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilitiesWindows;
import es.gob.afirma.keystores.mozilla.MozillaProfile;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;

/** Configurador para instalar un certificado SSL de confianza en Mozilla NSS.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s.
 * @author Carlos Gamuci. */
final class ConfiguratorFirefoxWindows {

	private static final String FILE_AUTOFIRMA_CERTIFICATE = "Autofirma_ROOT.cer"; //$NON-NLS-1$
	private static final String CERTUTIL_DIR = "certutil"; //$NON-NLS-1$
	private static final String CERTUTIL_EXE = "certutil.exe"; //$NON-NLS-1$
	private static final String CERTUTIL_RESOURCE = "/windows/certutil.windows.zip"; //$NON-NLS-1$

	private static final String CUSTOM_PROFILE_PREFERENCES_FILENAME = "user.js"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_FILE_HEADER =
			"// === PROPIEDADES PERSONALIZADAS DE CONFIGURACION ===\r\n"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_ENTERPRISE_ROOTS_HEADER =
			"\r\n// Confianza en los certificados raices del almacen del sistema\r\n"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_ENTERPRISE_ROOTS = "security.enterprise_roots.enabled"; //$NON-NLS-1$

	private static final String BREAK_LINE = "\r\n"; //$NON-NLS-1$

	/** Nombre del usuario por defecto en Windows. Este usuario es el que se usa como base para
	 * crear nuevos usuarios y no se deber&iacute;a tocar. */
	private static final String DEFAULT_WINDOWS_USER_NAME = "Default"; //$NON-NLS-1$

	/** Nombre de subdirectorio dentro del directorio de usuarios que debemos evitar. */
	private static final String ALL_USERS_DIRECTORY = "All Users"; //$NON-NLS-1$

	private static final String ENV_VARIABLE_ALLUSERSPROFILE = "ALLUSERSPROFILE"; //$NON-NLS-1$
	private static final String ENV_VARIABLE_PUBLIC = "PUBLIC"; //$NON-NLS-1$
	private static final String ENV_VARIABLE_SYSTEMDRIVE = "SystemDrive"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/**
	 * A partir de determinado momento CertUtil dejo de reconocer el certificado de confianza
	 * en Windows con el alias con el que se instalaba (CertUtil.CERT_ALIAS) y lo reconocio por
	 * su
	 */
	private static final String CERT_ALIAS = "Autofirma ROOT"; //$NON-NLS-1$

	private static final String PROFILES_INI_RELATIVE_PATH;
	private static String USERS_PATH;

	private MozillaProfile[] mozillaProfiles = null;

	static {

		if (isWindowsXP()) {
			PROFILES_INI_RELATIVE_PATH = "Application Data\\Mozilla\\Firefox\\profiles.ini"; //$NON-NLS-1$
			try {
				USERS_PATH = new File(System.getenv(ENV_VARIABLE_ALLUSERSPROFILE)).getParentFile().getAbsolutePath() + File.separator;
			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido identificar el directorio base sobre el que se instalan los perfiles de Mozilla. Se buscara en 'C:/Documents and Settings': " + e); //$NON-NLS-1$
				USERS_PATH = "C:\\Documents and Settings\\"; //$NON-NLS-1$
			}
		}
		else {
			PROFILES_INI_RELATIVE_PATH = "AppData\\Roaming\\Mozilla\\Firefox\\profiles.ini"; //$NON-NLS-1$
			final String publicUserHome = System.getenv(ENV_VARIABLE_PUBLIC);
			if (publicUserHome != null) {
				USERS_PATH = new File(publicUserHome).getParentFile().getAbsolutePath() + File.separator;
			}
			else {
				try {
					USERS_PATH = new File(System.getProperty("user.home")).getParentFile().getAbsolutePath() + File.separator; //$NON-NLS-1$;
				}
				catch (final Exception e) {
					LOGGER.warning("No se ha podido identificar el directorio de usuarios. Se buscara en 'C:/Users': " + e); //$NON-NLS-1$
					USERS_PATH = System.getenv(ENV_VARIABLE_SYSTEMDRIVE) + "\\Users\\"; //$NON-NLS-1$
				}
			}
		}
	}

	/**
	 * Instala un certificado en el almac&eacute;n de autoridades de confianza de Firefox de
	 * todos los perfiles de todos los usuarios del sistema.
	 * @param appDir Directorio de instalaci&oacute;n.
	 * @param window Consola de instaci&oacute;n donde escribir los mensajes.
	 * @throws MozillaProfileNotFoundException Cuando no se encuentran perfiles de usuario en los que
	 *                                         instalar el certificado.
	 */
	void installCACertOnMozillaKeyStores(final File appDir,
			                                    final Console window) throws MozillaProfileNotFoundException {
		final MozillaProfile[] mozillaProfiles = getAllMozillaProfiles();
		if (mozillaProfiles == null || mozillaProfiles.length == 0) {
			throw new MozillaProfileNotFoundException("No se han encontrado perfiles de Mozilla en el sistema"); //$NON-NLS-1$
		}

		window.print(Messages.getString("ConfiguratorWindows.4")); //$NON-NLS-1$
		try {
			copyCertUtilToDisk(appDir);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo copiar certutil para la instalacion del certificado SSL raiz del almacen de Mozilla Firefox: " + e); //$NON-NLS-1$
		}

		// Si no esta certUtil en el directorio o no puede usarse, se omite la instalacion
		// del certificado SSL en Firefox
		try {
			checkCertUtil(appDir);
		}
		catch(final IOException e) {
			LOGGER.warning("Se omite la configuracion del certificado SSL en Mozilla Firefox: " + e); //$NON-NLS-1$
			return;
		}

		window.print(Messages.getString("ConfiguratorWindows.9")); //$NON-NLS-1$
		boolean firefoxInstallFailed = false;
		for (final MozillaProfile profile : mozillaProfiles) {
			try {
				executeCertUtilToImport(
						appDir,
						profile,
						window
				);
			}
			catch (Exception e) {
				LOGGER.log(Level.WARNING, "No se pudo instalar la CA del certificado SSL para el socket en el perfil " + profile.getName() + " de Firefox: " + e, //$NON-NLS-1$
						e
				);
				firefoxInstallFailed = true;
			}
		}
		if (firefoxInstallFailed) {
			AOUIFactory.showMessageDialog(
					window,
					Messages.getString("ConfiguratorWindows.10"), //$NON-NLS-1$
					Messages.getString("ConfiguratorWindows.11"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE
			);
		}
	}

	/**
	 * Desinstala un certificado de los almacenes de autoridades de confianza
	 * de Firefox de todos los perfiles de todos los usuarios del sistema.
	 * @param appDir Directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 * @param console Consola a trav&eacute;s de la que imprimir los mensajes de usuario.
	 * @param iterations N&uacute;mero de veces que se debe repetir el proceso.
	 */
	void uninstallRootCAMozillaKeyStore(final File appDir, final Console console, final int iterations) {

		if (iterations <= 0) {
			return;
		}

		final MozillaProfile[] mozillaProfiles = getAllMozillaProfiles();
		if (mozillaProfiles == null || mozillaProfiles.length == 0) {
			LOGGER.info("No se han encontrado perfiles de Mozilla en el sistema de los cuales desinstalar"); //$NON-NLS-1$
			return;
		}

		try {
			copyCertUtilToDisk(appDir);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo copiar certutil para la desinstalacion del certificado SSL raiz del almacen de Mozilla Firefox: " + e); //$NON-NLS-1$
		}

		try {
			checkCertUtil(appDir);
		}
		catch (final IOException e) {
			LOGGER.warning("No se pudo encontrar certutil en disco o verificar su funcionamiento: " + e); //$NON-NLS-1$
		}

		for (final MozillaProfile profile : mozillaProfiles) {
			for (int i = 0; i < iterations; i++) {
				try {
					uninstallCACertFromMozillaKeyStore(appDir, profile, console);
				}
				catch (final Exception e) {
					LOGGER.log(Level.WARNING, "No se pudo desinstalar o no se encontro el certificado SSL raiz del almacen de Mozilla Firefox", e); //$NON-NLS-1$
				}
			}
		}
	}

	/** Comprueba si certutil existe y si puede ejecutarse.
	 * @param appDir Directorio de la aplicaci&oacute;n.
	 * @throws IOException Cuando hay un problema con el ejecutable CertUtil. */
	private static void checkCertUtil(final File appDir) throws IOException {

		final File certUtilFile = new File(appDir, CERTUTIL_DIR + File.separator + CERTUTIL_EXE);

		if (!certUtilFile.isFile()) {
			throw new IOException("No se encuentra el ejecutable CertUtil"); //$NON-NLS-1$
		}

		if (!certUtilFile.canExecute()) {
			throw new IOException("CertUtil no tiene permisos de ejecucion"); //$NON-NLS-1$
		}
	}

	/**
	 * Ejecuta la utilidad CertUtil para la instalaci&oacute;n del certificado ra&iacute;z de
	 * confianza SSL en un perfil de Mozilla.
	 * @param appDir Directorio en el que se encuentra el certificado a importar.
	 * @param profile Perfil de Mozilla.
	 * @param console Consola a trav&eacute;s de la que imprimir los mensajes de usuario.
	 * @throws GeneralSecurityException Cuando ocurre un error en la inserci&oacute;n del certificado en el KeyStore. */
	private static void executeCertUtilToImport(final File appDir,
			                                    final MozillaProfile profile,
			                                    final Console console)
			                                    		throws GeneralSecurityException {

		final File certutilExe =
				new File(appDir, CERTUTIL_DIR + File.separator + CERTUTIL_EXE);

		// Si en el directorio del perfil existe el fichero pkcs11.txt entonces se trata
		// de un almacen de certificados SQL
		final boolean sqlDb = new File(profile.getProfileDir(), "pkcs11.txt").exists(); //$NON-NLS-1$

		final String[] certutilCommands = new String[] {
				escapePath(certutilExe.getAbsolutePath()),
				"-A", //$NON-NLS-1$
				"-d", //$NON-NLS-1$
				escapePath((sqlDb ? "sql:" : "") + profile.getProfileDir().getAbsolutePath()), //$NON-NLS-1$ //$NON-NLS-2$
				"-i", //$NON-NLS-1$
				escapePath(new File(appDir, FILE_AUTOFIRMA_CERTIFICATE).getAbsolutePath()), "-n", //$NON-NLS-1$
				"\"" + ConfiguratorUtil.CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$
				"-t", //$NON-NLS-1$
				"\"C,,\"" //$NON-NLS-1$
		};

		try {
			execCertUtilCommandLine(appDir, certutilCommands, console, profile.getName());
		}
		catch (final AOCancelledOperationException e) {
			throw new KeyStoreException(
				"Se cancelo la insercion del Error en la instalacion del certificado de CA en el perfil " + profile.getName() //$NON-NLS-1$
					+ " de Firefox. Es probable que la aplicacion no funcione en este perfil.", e //$NON-NLS-1$
			);
		}
		catch (final Exception e) {
			throw new KeyStoreException(
				"Error en la instalacion del certificado de CA en el perfil " + profile.getName() //$NON-NLS-1$
					+ " de Firefox. Es posible que la aplicacion funcione en su propio perfil. Si desea que la aplicacion se " //$NON-NLS-1$
					+ " ejecute correctamente en todos los perfiles, desinstalela y vuelvala a instalar.", e //$NON-NLS-1$
			);
		}
	}

	/**
	 * Ejecuta la aplicacion Mozilla CertUtil para eliminar el certificado de confianza ra&iacute;z
	 * SSL de Firefox.
	 * @param appDir Directorio padre en el que se encuentra el directorio de certUtil.
	 * @param profile Perfil de Mozilla.
	 * @param console Consola a trav&eacute;s de la que imprimir los mensajes de usuario.
	 * @throws GeneralSecurityException Cuando no se puede ejecutar.
	 */
	private static void uninstallCACertFromMozillaKeyStore(final File appDir, final MozillaProfile profile, final Console console)
			throws GeneralSecurityException {

		final File certutilFile = new File(appDir, CERTUTIL_DIR + File.separator + CERTUTIL_EXE);

		// Si en el directorio del perfil existe el fichero pkcs11.txt entonces se trata
		// de un almacen de certificados SQL
		final boolean sqlDb = new File(profile.getProfileDir(), "pkcs11.txt").exists(); //$NON-NLS-1$
		final String profileReference = (sqlDb ? "sql:" : "") + profile.getProfileDir().getAbsolutePath(); //$NON-NLS-1$ //$NON-NLS-2$

		try {
			deleteCertificate(ConfiguratorUtil.CERT_ALIAS, certutilFile, appDir, profileReference, console, profile.getName());
		}
		catch (final Exception e) {
			LOGGER.warning("No se encontro o no se pudo borrar el certificado '" + ConfiguratorUtil.CERT_ALIAS //$NON-NLS-1$
					+ "' de CA en el perfil de Firefox del usuario '" //$NON-NLS-1$
					+ profile.getProfileDir().getAbsolutePath() + "'. Se buscara con el alias " + CERT_ALIAS); //$NON-NLS-1$

			try {
				deleteCertificate(CERT_ALIAS, certutilFile, appDir, profileReference, console, profile.getName());
			}
			catch (final Exception e2) {
				throw new KeyStoreException("No se encontro o no se pudo borrar el certificado de CA en el perfil de usuario de Firefox " + profile.getProfileDir().getAbsolutePath(), e2); //$NON-NLS-1$
			}
		}
	}

	private static void deleteCertificate(final String alias, final File certutilFile, final File appDir, final String profileReference, final Console console, final String profileName) throws IOException {

		final String[] certutilCommands = new String[] {
				escapePath(certutilFile.getAbsolutePath()),
				"-D", //$NON-NLS-1$
				"-d", //$NON-NLS-1$
				escapePath(profileReference),
				"-n", //$NON-NLS-1$
				"\"" + alias + "\"", //$NON-NLS-1$ //$NON-NLS-2$
		};

		execCertUtilCommandLine(appDir, certutilCommands, console, profileName);
	}

	/**
	 * Ejecuta Mozilla CertUtil como comando del sistema.
	 * @param appDir Directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 * @param command Comando a ejecutar, con el nombre de comando y sus par&aacute;metros separados en un array.
	 * @param console Consola a trav&eacute;s de la que imprimir los mensajes de usuario.
	 * @param profileName Nombre del perfil de Firefox.
	 * @throws IOException Si no se pudo realizar la propia ejecuci&oacute;n.
	 */
	private static void execCertUtilCommandLine(final File appDir, final String[] command, final Console console, final String profileName)
			throws IOException {

		LOGGER.info("Se ejecutara el siguiente comando:\n" + printCommand(command)); //$NON-NLS-1$

		final Process process = new ProcessBuilder(command).start();

		try (final OutputStream resOs = process.getOutputStream()) {
			// Abrimos el canal de entrada para poder identificar si certutil nos pide contrasena
			// maestra e insertarsela en ese caso. Lo abrimos en este punto porque de no hacerlo
			// o hacerlo en el mismo try que el canal de salida, se queda bloqueada la ejecucion de
			// certutil
		}

		// Cuando se instala correctamente no hay salida de ningun tipo, asi que se interpreta
		// cualquier salida como un problema en la instalacion.
		// En caso de necesitar contrasena maestra para instalar los certificados de usuario, se
		// lanzara un error con el texto 'Enter Password or Pin for "NSS Certificate DB":'. En este
		// caso, como con cualquier error, ignoraremos el error, ya que Firefox acepta tambien los
		// certificados de confianza del sistema
		String line;
		try (
				final InputStream resIs = process.getInputStream();
				final BufferedReader resReader = new BoundedBufferedReader(
						new InputStreamReader(resIs),
						256, // Maximo 256 lineas de salida
						1024 // Maximo 1024 caracteres por linea
						)
				) {
			if ((line = resReader.readLine()) != null) {
				process.destroyForcibly();
				throw new IOException("Error durante la ejecucion de certutil (out): " + line); //$NON-NLS-1$
			}
		}

		// Comprobamos que tampoco se haya devuelto ningun mensaje por el canal de error,
		// en cuyo caso la operacion habra finalizado correctamente
		try (
				final InputStream errIs = process.getErrorStream();
				final BufferedReader errReader = new BoundedBufferedReader(
						new InputStreamReader(errIs),
						256, // Maximo 256 lineas de salida
						1024 // Maximo 1024 caracteres por linea
						)
				) {
			if ((line = errReader.readLine()) != null) {
				process.destroyForcibly();
				throw new IOException("Error durante la ejecucion de certutil (err): " + line); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Copia a un directorio, si no esta ya, el ejecutable certutil con sus dependencias.
	 * @param targetDir Directorio en el que copiar certutil.
	 * @throws IOException Cuando no ha sido posible copiar certutil.
	 */
	private static void copyCertUtilToDisk(final File targetDir) throws IOException {
		final File certutil = new File(targetDir, CERTUTIL_DIR);
		if (!certutil.exists()) {
			ConfiguratorUtil.uncompressResource(CERTUTIL_RESOURCE, targetDir);
		}
	}

	/**
	 * Recupera todos los perfiles de Mozilla accesibles del sistema.
	 * @return Listado de perfiles de Mozilla.
	 */
	private MozillaProfile[] getAllMozillaProfiles() {

		if (mozillaProfiles == null) {
			final ArrayList<MozillaProfile> allProfiles = new ArrayList<>();

			for (final File userDir : getUserDirs()) {

				// Si en el directorio de usuario no hay fichero de perfiles de Mozilla,
				// se ignora
				File profilesInitFile = getProfilesFileForUser(userDir);
				if (!profilesInitFile.isFile()) {
					continue;
				}

				LOGGER.info("Se cargan los perfiles de Firefox de: " + new File(userDir, PROFILES_INI_RELATIVE_PATH).getAbsolutePath()); //$NON-NLS-1$

				final MozillaProfile[] userProfiles;
				try {
					userProfiles = MozillaKeyStoreUtilities.getProfiles(profilesInitFile);
				}
				catch (final Exception e) {
					LOGGER.warning("No se pudieron recuperar los directorios de perfil de Mozilla para el usuario " + userDir.getName() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					continue;
				}

				// Agregamos el listado de perfiles al listado global, acortando
				// previamente las URL para permitir su uso con el certutil de Windows
				// y asociandoles el usuario al que corresponden
				for (MozillaProfile userProfile : userProfiles) {
					String shortPath = MozillaKeyStoreUtilitiesWindows.cleanProfilePath(
							userProfile.getProfileDir().getAbsolutePath());
					userProfile.setProfileDir(new File(shortPath));
					userProfile.setUsername(userDir.getName());
					allProfiles.add(userProfile);
				}
			}

			mozillaProfiles = allProfiles.toArray(new MozillaProfile[0]);
		}

		return mozillaProfiles;
	}

	/**
	 * Recupera todos los directorios de usuario de los que extraer perfiles de Mozilla.
	 * @return Listado de directorios de usuario.
	 */
	private static File[] getUserDirs() {

		final File usersBaseDir = new File(USERS_PATH);
		if (!usersBaseDir.isDirectory() || !usersBaseDir.canRead()) {
			return new File[0];
		}
		// Devolvemos los directorios de usuario despues de haber filtrado aquellos que no pueden serlo
		return usersBaseDir.listFiles((file ) -> file.isDirectory()
				&& !ALL_USERS_DIRECTORY.equalsIgnoreCase(file.getName())
				&& !DEFAULT_WINDOWS_USER_NAME.equalsIgnoreCase(file.getName()));
	}

	/**
	 * Obtiene el que deber&iacute;a ser el fichero de perfiles de Firefox de un usuario. Este fichero puede no existir.
	 * @param userHomeDir Directorio home del usuario.
	 * @return Fichero de perfiles.
	 */
	private static File getProfilesFileForUser(File userHomeDir) {
		return new File(userHomeDir, PROFILES_INI_RELATIVE_PATH);
	}

	/**
	 * Comprueba si el sistema operativo es Windows XP.
	 * @return {@code true} si el sistema operativo es Windows XP y {@code false} en caso contrario.
	 */
	private static boolean isWindowsXP() {
		return System.getProperty("os.name") != null && System.getProperty("os.name").contains("XP"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	/**
	 * Escapa una ruta de fichero para poder utilizarla como parte de un
	 * comando de consola.
	 * @param path Ruta a escapar.
	 * @return Ruta escapada.
	 */
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
	 * Compone en una &uacute;nica l&iacute;nea de texto el comando proporcionado.
	 * @param command Listado de part&iacute;culas que componen un comando.
	 * @return Comando completo.
	 */
	private static String printCommand(final String[] command) {
		final StringBuilder sb = new StringBuilder();
		for (final String s : command) {
			sb.append(s);
			sb.append(' ');
		}
		return sb.toString();
	}

	/**
	 * Configur el que se habilite o deshabilite el uso del almac&eacute;n de cofianza del
	 * sistema operativo como almacen de confianza de Firefox.
	 * @param enable {@code true} para habilitar la confianza en los certificados ra&iacute;z del
	 * almac&eacute;n de confianza del sistema adem&aacute;s de en los suyos propios,
	 * {@code false} en de que s&oacute;lo se desee confiar en el almac&eacute;n del navegador.
	 * @param window Consola en la que se mostrar&aacute;n los mensajes de progreso.
	 * @throws IOException Cuando no se puede crear o editar la configuraci&oacute;n.
	 * @throws MozillaProfileNotFoundException Cuando no se han encontrado perfiles de Firefox.
	 */
	void configureUseSystemTrustStore(final boolean enable, final Console window) throws IOException, MozillaProfileNotFoundException {

		// Obtenemos el listado de perfiles de Firefox
		final MozillaProfile[] mozillaProfiles = getAllMozillaProfiles();

		if (mozillaProfiles == null || mozillaProfiles.length == 0) {
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
		for (final MozillaProfile profile : mozillaProfiles) {
			File profileDir = profile.getProfileDir();
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
