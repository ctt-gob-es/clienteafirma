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
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.security.GeneralSecurityException;
import java.security.KeyStoreException;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;

/** Configurador para instalar un certificado SSL de confianza en Mozilla NSS.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
final class ConfiguratorFirefoxWindows {

	private static final String FILE_AUTOFIRMA_CERTIFICATE = "AutoFirma_ROOT.cer"; //$NON-NLS-1$
	private static final String CERTUTIL_DIR = "certutil"; //$NON-NLS-1$
	private static final String CERTUTIL_EXE = "certutil.exe"; //$NON-NLS-1$
	private static final String CERTUTIL_RESOURCE = "/windows/certutil.windows.zip"; //$NON-NLS-1$

	/** Nombre del usuario por defecto en Windows. Este usuario es el que se usa como base para
	 * crear nuevos usuarios y no se deber&iacute;a tocar. */
	private static String DEFAULT_WINDOWS_USER_NAME = "Default"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static String PROFILES_INI_RELATIVE_PATH;
	private static String USERS_PATH;


	static {

		if (isWindowsXP()) {
			PROFILES_INI_RELATIVE_PATH = "Application Data\\Mozilla\\Firefox\\profiles.ini"; //$NON-NLS-1$
			try {
				USERS_PATH = new File(System.getenv("ALLUSERSPROFILE")).getParentFile().getAbsolutePath() + File.separator; //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido identificar el directorio base sobre el que se instalan los perfiles de Mozilla. Se buscara en 'C:/Documents and Settings': " + e); //$NON-NLS-1$
				USERS_PATH = "C:\\Documents and Settings\\"; //$NON-NLS-1$
			}
		}
		else {
			PROFILES_INI_RELATIVE_PATH = "AppData\\Roaming\\Mozilla\\Firefox\\profiles.ini"; //$NON-NLS-1$
			try {
				USERS_PATH = new File(System.getProperty("user.home")).getParentFile().getAbsolutePath() + File.separator; //$NON-NLS-1$;
			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido identificar el directorio de usuarios. Se buscara en 'C:/Users': " + e); //$NON-NLS-1$
				USERS_PATH = "C:\\Users\\"; //$NON-NLS-1$
			}
		}

	}

	private ConfiguratorFirefoxWindows() {
		// No instanciable
	}

	/**
	 * Instala un certificado en el almac&eacute;n de autoridades de confianza de Firefox de
	 * todos los perfiles de todos los usuarios del sistema.
	 * @param appDir Directorio de instalaci&oacute;n.
	 * @throws MozillaProfileNotFoundException Cuando no se encuentran perfiles de usuario en los que
	 * instalar el certificado.
	 * @throws IOException Cuando se produce algun error durante la instalaci&oacute;n.
	 */
	static void installCACertOnMozillaKeyStores(final File appDir, final Console window) throws MozillaProfileNotFoundException,
	                                                                      IOException {

		final File[] mozillaProfileDirs = getAllMozillaProfileDirs();
		if (mozillaProfileDirs == null || mozillaProfileDirs.length == 0) {
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
		for (final File profileDir : mozillaProfileDirs) {
			importCACertOnMozillaKeyStore(appDir, profileDir);
		}

		window.print(Messages.getString("ConfiguratorWindows.7")); //$NON-NLS-1$
		removeCertUtilFromDisk(appDir);
	}

	/**
	 * Desinstala un certificado de los almac&eacute;nes de autoridades de confianza
	 * de Firefox de todos los perfiles de todos los usuarios del sistema.
	 * @param appDir Directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 */
	static void uninstallRootCAMozillaKeyStore(final File appDir) {

		final File[] mozillaProfileDirs = getAllMozillaProfileDirs();
		if (mozillaProfileDirs == null || mozillaProfileDirs.length == 0) {
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
		} catch (final IOException e) {
			LOGGER.warning("No se pudo encontrar certutil en disco o verificar su funcionamiento: " + e); //$NON-NLS-1$
		}

		for (final File profileDir : mozillaProfileDirs) {
			try {
				uninstallCACertFromMozillaKeyStore(appDir, profileDir);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se pudo desinstalar el certificado SSL raiz del almacen de Mozilla Firefox", e); //$NON-NLS-1$
			}
		}

		removeCertUtilFromDisk(appDir);
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
	 * Instala un certificado de CA en el almacen de un perfil de Mozilla.
	 * @param appDir Directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 * @param profilesDir Directorio del perfil de Mozilla.
	 */
	private static void importCACertOnMozillaKeyStore (final File appDir,
			                                           final File profilesDir) {
		boolean installed = false;
		boolean cancelled = false;
		do {
			try {
				executeCertUtilToImport(
					appDir,
					profilesDir
				);
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
						JOptionPane.WARNING_MESSAGE
						);
				if (result == JOptionPane.CANCEL_OPTION) {
					cancelled = true;
					LOGGER.warning(
							"El usuario cancelo la instalacion del certificado SSL para el socket en Firefox: " + e //$NON-NLS-1$
							);
				}
			}
		} while (!installed && !cancelled);
	}


	/** Ejecuta la utilidad CertUtil para la instalaci&oacute;n del certificado ra&iacute;z de
	 * confianza SSL en un perfil de Mozilla.
	 * @param appDir Directorio en el que se encuentra el certificado a importar.
	 * @param profileDir Directorio de perfil de Mozilla.
	 * @throws IOException Cuando ocurre un error en el tratamiento de datos.
	 * @throws GeneralSecurityException Cuando ocurre un error en la inserci&oacute;n del certificado en el KeyStore.
	 */
	private static void executeCertUtilToImport(final File appDir,
			                                    final File profileDir)
			                                    		throws IOException, GeneralSecurityException {

		final File certutilExe =
				new File(appDir, CERTUTIL_DIR + File.separator + CERTUTIL_EXE);

		// Si en el directorio del perfil existe el fichero pkcs11.txt entonces se trata
		// de un almacen de certificados SQL
		final boolean sqlDb = new File(profileDir, "pkcs11.txt").exists(); //$NON-NLS-1$

		final String[] certutilCommands = new String[] {
				escapePath(certutilExe.getAbsolutePath()),
				"-A", //$NON-NLS-1$
				"-d", //$NON-NLS-1$
				escapePath((sqlDb ? "sql:" : "") + profileDir.getAbsolutePath()), //$NON-NLS-1$ //$NON-NLS-2$
				"-i", //$NON-NLS-1$
				escapePath(new File(appDir, FILE_AUTOFIRMA_CERTIFICATE).getAbsolutePath()), "-n", //$NON-NLS-1$
				"\"" + ConfiguratorUtil.CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$
				"-t", //$NON-NLS-1$
				"\"C,,\"" //$NON-NLS-1$
		};

		try {
			execCertUtilCommandLine(certutilCommands);
		}
		catch (final Exception e) {
			throw new KeyStoreException(
				"Error en la instalacion del certificado de CA en el perfiles " + profileDir //$NON-NLS-1$
					+ " de Firefox. Es posible que la aplicacion funcione en su propio perfil. Si desea que la aplicacion se " //$NON-NLS-1$
					+ " ejecute correctamente en todos los perfiles, desinstalela y vuelvala a instalar.", e //$NON-NLS-1$
			);
		}
	}

	/** Ejecuta la aplicacion Mozilla CertUtil para eliminar el certificado de confianza ra&iacute;z
	 * SSL de Firefox.
	 * @param appDir Directorio padre en el que se encuentra el directorio de certUtil.
	 * @throws IOException Cuando no se encuentra o puede leer alguno de los ficheros necesarios.
	 * @throws GeneralSecurityException Cuando no se puede ejecutar. */
	private static void uninstallCACertFromMozillaKeyStore(final File appDir, final File profileDir) throws IOException, GeneralSecurityException {

		final File certutilFile = new File(appDir, CERTUTIL_DIR + File.separator + CERTUTIL_EXE);

		// Si en el directorio del perfil existe el fichero pkcs11.txt entonces se trata
		// de un almacen de certificados SQL
		final boolean sqlDb = new File(profileDir, "pkcs11.txt").exists(); //$NON-NLS-1$
		final String profileReference = (sqlDb ? "sql:" : "") + profileDir.getAbsolutePath(); //$NON-NLS-1$ //$NON-NLS-2$

		final String[] certutilCommands = new String[] {
				escapePath(certutilFile.getAbsolutePath()),
				"-D", //$NON-NLS-1$
				"-d", //$NON-NLS-1$
				escapePath(profileReference),
				"-n", //$NON-NLS-1$
				"\"" + ConfiguratorUtil.CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$
		};

		try {
			execCertUtilCommandLine(certutilCommands);
		}
		catch (final Exception e) {
			throw new KeyStoreException("Error en el borrado del certificado de CA en el perfil de usuario " + profileDir.getAbsolutePath(), e); //$NON-NLS-1$
		}
	}

	/** Ejecuta Mozilla CertUtil como comando del sistema.
	 * @param command Comando a ejecutar, con el nombre de comando y sus par&aacute;metros separados en un array.
	 * @throws IOException Si no se pudo realizar la propia ejecuci&oacute;n. */
	private static void execCertUtilCommandLine(final String[] command)
			throws IOException {

		LOGGER.info("Se ejecutara el siguiente comando:\n" + printCommand(command)); //$NON-NLS-1$

		final Process process = new ProcessBuilder(command).start();
		// Cuando se instala correctamente no hay salida de ningun tipo, asi que se interpreta
		// cualquier salida como un error
		String line;
		try (
				final InputStream resIs = process.getInputStream();
				final BufferedReader resReader = new BoundedBufferedReader(
						new InputStreamReader(resIs),
						256, // Maximo 256 lineas de salida
						1024 // Maximo 1024 caracteres por linea
						);
				) {
			while ((line = resReader.readLine()) != null) {
				throw new IOException("Error durante la ejecucion de certutil: " + line); //$NON-NLS-1$
			}
		}

		try (
				final InputStream errIs = process.getErrorStream();
				final BufferedReader errReader = new BoundedBufferedReader(
						new InputStreamReader(errIs),
						256, // Maximo 256 lineas de salida
						1024 // Maximo 1024 caracteres por linea
						);
				) {
			while ((line = errReader.readLine()) != null) {
				throw new IOException("Error durante la ejecucion de certutil: " + line); //$NON-NLS-1$
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

	/** Elimina de un directorio certutil y sus dependencias.
	 * @param targetDir Directorio en el que se encuentra certutil. */
	private static void removeCertUtilFromDisk(final File targetDir) {
		if (!targetDir.isDirectory()) {
			return;
		}
		ConfiguratorUtil.deleteDir(new File(targetDir, CERTUTIL_DIR));
	}

	/**
	 * Recupera todos los directorios de perfil de Mozilla del sistema.
	 * @return Listado de directorios de perfil de Mozilla.
	 */
	private static File[] getAllMozillaProfileDirs() {

		final ArrayList<File> profileDirs = new ArrayList<>();

		for (final File userDir : getUserDirs()) {

			// Si en el directorio de usuario no hay fichero de perfiles de Mozilla,
			// se ignora
			if (!new File(userDir, PROFILES_INI_RELATIVE_PATH).exists()) {
				continue;
			}

			LOGGER.info("Se usa el perfil de Firefox: " + new File(userDir, PROFILES_INI_RELATIVE_PATH).getAbsolutePath()); //$NON-NLS-1$

			final String profileDir;
			try {
				profileDir = MozillaKeyStoreUtilities.getMozillaUserProfileDirectoryWindows(
						new File(userDir, PROFILES_INI_RELATIVE_PATH).getAbsolutePath());
			}
			catch (final Exception e) {
				LOGGER.warning("No se pudieron recuperar los directorios de perfil de Mozilla para el usuario " + userDir.getName() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}

			profileDirs.add(new File(profileDir));
		}

		return profileDirs.toArray(new File[profileDirs.size()]);
	}

	/**
	 * Recupera todos los directorios de usuario de los que extraer perfiles de Mozilla.
	 * @return Listado de directorios de usuario.
	 */
	private static File[] getUserDirs() {

		final ArrayList<File> usersDirList = new ArrayList<>();
		final File usersBaseDir = new File(USERS_PATH);
		for (final File userDir : usersBaseDir.listFiles()) {

			// Solo procesaremos los directorios e ignoraremos el del usuario por defecto de Windows ya que
			// este se usa de base para crear nuevos usuarios y se podria corromper
			if (!userDir.isDirectory() || DEFAULT_WINDOWS_USER_NAME.equalsIgnoreCase(userDir.getName())) {
				continue;
			}

			usersDirList.add(userDir);
		}

		return usersDirList.toArray(new File[usersDirList.size()]);
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
}
