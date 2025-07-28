/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.restoreconfig;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.swing.Timer;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;
import es.gob.afirma.standalone.so.macos.UnixUtils;


/**Contiene la l&oacute;gica para realizar las tareas de restauraci&oacute;n
 * asociadas al navegador Firefox para Windows y Linux. */
final class RestoreConfigFirefox {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int TIMEOUT = 5000;

	private static final int WITHOUT_TIMEOUT = 0;

	private static final String SSL_ROOT_CERTIFICATE_FILENAME = "Autofirma_ROOT.cer"; //$NON-NLS-1$
	static final String DIR_CERTUTIL = "certutil"; //$NON-NLS-1$
	private static final String LINUX_UNINSTALLSCRIPT_NAME = "uninstallRestore-"; //$NON-NLS-1$
	private static final String LINUX_SCRIPT_NAME = "installRestore-"; //$NON-NLS-1$
	private static final String LINUX_MOZILLA_PATH = "/.mozilla/firefox/profiles.ini";//$NON-NLS-1$
	private static final String UBUNTU_22_MOZILLA_PATH = "/snap/firefox/common/.mozilla/firefox/profiles.ini"; //$NON-NLS-1$
	private static final String NSS_LINUX_CHROME_PATH = "/.pki/nssdb";//$NON-NLS-1$
	//TODO: En Chromium ahora solo se instalara el certificado de confianza para el perfil activo
	private static final String NSS_LINUX_CHROMIUM_PATH = "/snap/chromium/current/.pki/nssdb";//$NON-NLS-1$
	private static final String[] NSS_DIR_SUBPATH = new String[] {
			NSS_LINUX_CHROME_PATH,
			NSS_LINUX_CHROMIUM_PATH
	};
	private static final String LINUX_CHROMIUM_PREFS_PATH = "/.config/chromium/Local State";//$NON-NLS-1$
	private static final String LINUX_CHROME_PREFS_PATH = "/.config/google-chrome/Local State";//$NON-NLS-1$
	private static String WINDOWS_MOZILLA_PATH;
	private static String USERS_WINDOWS_PATH;

	static final String CERTUTIL_EXE;
	private static final String FILE_CERTUTIL;
	private static final String RESOURCE_BASE;

	static {

		if (Platform.getOS() == Platform.OS.WINDOWS) {
			// Para Windows XP la ruta de los perfiles de Firefox y de los usuarios es diferente
			String osName;
			try {
				osName = System.getProperty("os.name"); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se pudo obtener el nombre del sistema operativo", e); //$NON-NLS-1$
				osName = ""; //$NON-NLS-1$
			}
			if (osName.contains("XP")) { //$NON-NLS-1$
				WINDOWS_MOZILLA_PATH = "\\Application Data\\Mozilla\\Firefox\\profiles.ini"; //$NON-NLS-1$
				USERS_WINDOWS_PATH = "C:\\Documents and Settings\\"; //$NON-NLS-1$
			}
			else {
				WINDOWS_MOZILLA_PATH = "\\AppData\\Roaming\\Mozilla\\Firefox\\profiles.ini"; //$NON-NLS-1$
				try {
					USERS_WINDOWS_PATH = new File(System.getProperty("user.home")).getParentFile().getAbsolutePath() + File.separator; //$NON-NLS-1$;
				}
				catch (final Exception e) {
					LOGGER.warning("No se ha podido identificar el directorio de usuarios: " + e); //$NON-NLS-1$
					USERS_WINDOWS_PATH = "C:/Users/"; //$NON-NLS-1$
				}
			}
		}
	}

	/** Nombre del usuario por defecto en Windows. Este usuario es el que se usa como base para
	 * crear nuevos usuarios y no se deber&iacute;a tocar. */
	private static String DEFAULT_WINDOWS_USER_NAME = "Default"; //$NON-NLS-1$

	static {
		switch(Platform.getOS()) {
		case WINDOWS:
			CERTUTIL_EXE = "certutil.exe"; //$NON-NLS-1$
			FILE_CERTUTIL = "certutil.windows.zip"; //$NON-NLS-1$
			RESOURCE_BASE = "/windows/"; //$NON-NLS-1$
			break;
		case LINUX:
			CERTUTIL_EXE = "certutil"; //$NON-NLS-1$
			FILE_CERTUTIL = "certutil.linux.zip"; //$NON-NLS-1$
			RESOURCE_BASE = "/linux/"; //$NON-NLS-1$
			break;
		default:
			throw new IllegalStateException(
					"Sistema operativo no soportado: " + Platform.getOS() //$NON-NLS-1$
					);
		}
	}

	private RestoreConfigFirefox() {
		// No instanciable
	}

	/** Genera el script que elimina el warning al ejecutar Autofirma desde Chrome para LINUX.
	 * En linux genera el script que hay que ejecutar para realizar la instalaci&oacute;n pero no lo ejecuta, de eso se encarga el instalador Debian.
	 * @param targetDir Directorio de instalaci&oacute;n del sistema
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 *  <ul>
	 *   <li>En LINUX contiene el contenido del script a ejecutar.</li>
	 * </ul> */
	private static void createScriptsRemoveExecutionWarningInChrome(final File targetDir, final String userDir, final String browserPath) {
		final String[] commandInstall = {
				"sed", //$NON-NLS-1$
				"s/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false,/g", //$NON-NLS-1$
				escapePath(userDir + browserPath),
				">", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};

		final String[] commandUninstall = {
				"sed", //$NON-NLS-1$
				"s/\\\"afirma\\\":false,//g", //$NON-NLS-1$
				escapePath(userDir + browserPath),
				">", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};

		//Se reemplaza el fichero generado por el original
		final String[] commandCopy = {
				"\\cp", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
				escapePath(userDir + browserPath),
		};

		// Generamos el script de instalacion y desistalacion
		try {

			final StringBuilder sb = new StringBuilder();
			for (final String s : commandInstall) {
				sb.append(s);
				sb.append(' ');
			}

			final StringBuilder uninstall = new StringBuilder();
			for (final String s : commandUninstall) {
				uninstall.append(s);
				uninstall.append(' ');
			}
			uninstall.append("\n"); //$NON-NLS-1$
			sb.append("\n"); //$NON-NLS-1$

			for (final String s : commandCopy) {
				sb.append(s);
				sb.append(' ');
			}
			for (final String s : commandCopy) {
				uninstall.append(s);
				uninstall.append(' ');
			}
			String path;
			String uninstallPath;
			sb.append("\n"); //$NON-NLS-1$
			uninstall.append("\n"); //$NON-NLS-1$

			// Obtenemos la ruta de los scripts
			final Random r = new Random();
			path = new File(targetDir,  LINUX_SCRIPT_NAME + r.nextInt() + ".sh").getAbsolutePath(); //$NON-NLS-1$
			uninstallPath = new File(targetDir, LINUX_UNINSTALLSCRIPT_NAME + r.nextInt() + ".sh").getAbsolutePath(); //$NON-NLS-1$
			final File installScript = new File(path);
			final File uninstallScript = new File(uninstallPath);

			try (
					final FileOutputStream fout = new FileOutputStream(installScript, true);
					final FileOutputStream foutUninstall = new FileOutputStream(
							uninstallScript, true
							);
					) {
				fout.write(sb.toString().getBytes());
				foutUninstall.write(uninstall.toString().getBytes());
			}
		}
		catch (final Exception e) {
			LOGGER.severe(
					"Excepcion en la creacion del script linux para la modificacion del fichero de protocolos de Google Chrome: " + e //$NON-NLS-1$
					);
		}

	}

	/** Genera el <i>script</i> que elimina el warning al ejecutar Autofirma desde Chrome para LINUX.
	 * En Linux genera el <i>script</i> que hay que ejecutar para realizar la instalaci&oacute;n pero no lo ejecuta, de eso se encarga el instalador Debian.
	 * @param targetDir Directorio de instalaci&oacute;n del sistema.
	 * @param usersDirs Directorio donde estan las carpetas de los usuarios. */
	static void removeAppExecutionWarningInChrome(final File targetDir, final List<String> usersDirs) {

		for ( final String userDir : usersDirs) {
			// Montamos el script de instalacion y desinstalacion que
			// incluya el protocolo "afirma" en el fichero Local State
			if ( Platform.OS.LINUX.equals(Platform.getOS()) ) {
				final File fileChrome = new File(escapePath(userDir) + LINUX_CHROME_PREFS_PATH);
				final File fileChromium = new File(escapePath(userDir) + LINUX_CHROMIUM_PREFS_PATH);
				if( fileChrome.isFile() ) {
					createScriptsRemoveExecutionWarningInChrome(targetDir, userDir, LINUX_CHROME_PREFS_PATH);
				}
				if ( fileChromium.isFile() ) {
					createScriptsRemoveExecutionWarningInChrome(targetDir, userDir, LINUX_CHROMIUM_PREFS_PATH);
				}
			}
		}
	}

	/** Instala el certificado en el almac&eacute;n del sistema de Linux (el usado por Chrome).
	 * @param workingDir Directorio en el que se encuentra el subdirectorio de <code>certutil</code>.
	 * @param rootCertFile Fichero del certificado ra&iacute;z a instalar.
	 * @param usersDirs Listado de directorios de usuario.
	 * @throws IOException Cuando ocurre un error en el tratamiento de datos. */
	static void installRootCAChromeKeyStore(final File workingDir,
			                                final File rootCertFile,
			                                final List<String> usersDirs ) throws IOException {

		if ( !Platform.OS.LINUX.equals(Platform.getOS()) ) {
			return;
		}

		final String certUtilPath = getCertUtilPath(workingDir);

		for ( final String userDir : usersDirs) {

			for (final String nssDirSubpath : NSS_DIR_SUBPATH) {

				final File file = new File(escapePath(userDir) + nssDirSubpath);
				if (file.isDirectory()) {
					final String[] certutilCommands = {
							certUtilPath, // 0
							"-d", //$NON-NLS-1$ // 1
							"sql:" + escapePath(userDir) + nssDirSubpath, //$NON-NLS-1$ // 2
							"-A", //$NON-NLS-1$ // 3
							"-n", //$NON-NLS-1$ // 4
							"\"" + ConfiguratorUtil.CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$ // 5
							"-i", //$NON-NLS-1$ // 6
							escapePath(rootCertFile.getAbsolutePath()), // 7
							"-t", //$NON-NLS-1$ // 8
							"\"TCP,TCP,TCP\"" //$NON-NLS-1$ // 9
					};
					execCommandLineCertUtil(workingDir, certutilCommands, true);

				}
			}
		}
	}

	/** Inicia la restauraci&oacute;n del certificado para la comunicaci&oacute;n entre Firefox y Autofirma en Windows.
	 * @param targetDir Directorio de la aplicaci&oacute;n en el que ya se encuentra el certificado.
	 * @throws MozillaProfileNotFoundException Si no se encuentra el directorio de perfil de usuario de Mozilla Firefox.
	 * @throws IOException Cuando hay un error escribiendo o leyendo datos.
	 * @throws KeyStoreException Cuando ocurre un error durante la importaci&oacute;n. */
	static void installRootCAMozillaKeyStore(final File targetDir) throws MozillaProfileNotFoundException, IOException, KeyStoreException {
		installRootCAMozillaKeyStore(targetDir, null);
	}

	/** Inicia la restauraci&oacute;n del certificado para la comunicaci&oacute;n entre Firefox y Autofirma en Windows.
	 * @param targetDir Directorio de la aplicaci&oacute;n.
	 * @param certFile Fichero con el certificado a instalar.
	 * @throws MozillaProfileNotFoundException Si no se encuentra el perfil de usuario de Mozilla Firefox.
	 * @throws IOException Cuando hay errores leyendo o escribiendo datos.
	 * @throws KeyStoreException Cuando ocurre un error durante la importaci&oacute;n. */
	static void installRootCAMozillaKeyStore(final File targetDir, final File certFile)
			throws MozillaProfileNotFoundException, IOException, KeyStoreException {

		final ArrayList<File> firefoxProfilesDir = getFirefoxProfilesDir();
		if (firefoxProfilesDir == null || firefoxProfilesDir.isEmpty()) {
			throw new MozillaProfileNotFoundException();
		}

		Set<File> profiles = null;

		for (final File firefoxDir : firefoxProfilesDir) {
			// En Windows recibimos un unico directorio de perfil, lo convertimos a una estructura Set<File>
			profiles = new HashSet<>(Arrays.asList(firefoxDir.listFiles()));
			try {
				RestoreConfigFirefox.importCARootOnFirefoxKeyStore(targetDir, certFile, profiles);
			}
			catch (final Exception e) {
				LOGGER.warning("No se pudo instalar el certificado de CA en todos los perfiles de Firefox: " + e); //$NON-NLS-1$
			}
		}
	}

	/** Genera el <i>script</i> de instalaci&oacute; del certificado en Firefox para LINUX.
	 * En ambos casos, es necesario crear un <i>script</i> intermedio con el comando <code>certutil</code> y sus argumentos
	 * y posteriormente ejecutarlo como un comando de consola.
	 * @param targetDir Directorio de instalaci&oacute;n del sistema
	 * @param certFile Fichero del certificado que debemos instalar.
	 * @param usersDirs Listado de carpetas de los usuarios.
	 * @throws MozillaProfileNotFoundException No se ha encontrado el directorio de perfiles de Mozilla.
	 * @throws KeyStoreException Cuando ocurre un error durante la importaci&oacute;n. */
	static void installRootCAMozillaKeyStore(final File targetDir,
			                                 final File certFile,
			                                 final List<String> usersDirs ) throws MozillaProfileNotFoundException,
	                                                                               KeyStoreException {

		// dados los usuarios sacamos el directorio de perfiles de mozilla en caso de que lo tengan
		final List <File> mozillaUsersProfilesPath = getMozillaUsersProfilesPath(usersDirs);
		// para cada usuario tenemos sus distintos directorios de perfiles
		final Set <File> profiles = getProfiles(mozillaUsersProfilesPath);
		if (profiles.isEmpty()){
			throw new MozillaProfileNotFoundException();
		}

		RestoreConfigFirefox.importCARootOnFirefoxKeyStore(targetDir, certFile, profiles);
	}


	/**
	 * Desinstala el certificado de firefox para Windows y Linux.
	 * @param targetDir Directorio de instalaci&oacute;n del sistema
	 */
	public static void uninstallRootCAMozillaKeyStore(final File targetDir) {

		try {
			executeCertUtilToDelete(targetDir);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo desinstalar el certificado SSL raiz del almacen de Mozilla Firefox: " + e); //$NON-NLS-1$
		}
	}

	/** Elimina la carpeta certutil generada durante el proceso de instalaci&oacute;n.
	 * @param targetDir Directorio en el que se copia certUtil. */
	static void removeConfigurationFiles(final File targetDir) {
		if (!targetDir.exists()) {
			return;
		}
		RestoreConfigFirefox.deleteConfigDir(targetDir);
	}

	private static String escapePath(final String path) {
		if (path == null) {
			throw new IllegalArgumentException(
				"La ruta a 'escapar' no puede ser nula" //$NON-NLS-1$
			);
		}
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			if (path.contains(" ")) { //$NON-NLS-1$
				return "\"" + path + "\""; //$NON-NLS-1$ //$NON-NLS-2$
			}
			return path;
		}
		return path.replace(" ", "\\ "); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Obtiene el path para la llamada a CertUtil.
	 * @param workingDir Ruta en la que buscar el ejecutable CertUtil.
	 * @return Referencia a CertUtil.
	 * @throws IOException Se lanza cuando hay un problema con el fichero CertUtil. */
	public static String getCertUtilPath(final File workingDir) throws IOException {

		String certUtilPath = null;

		//En linux se trabaja con la dependencia del certutil
		if (Platform.OS.LINUX.equals(Platform.getOS())) {
			certUtilPath = CERTUTIL_EXE;
		}
		else {
			final File certutilFile = new File(workingDir.getAbsolutePath() + File.separator +
					DIR_CERTUTIL + File.separator + CERTUTIL_EXE);

			if (!certutilFile.exists() || !certutilFile.isFile()) {
				throw new IOException("No se encuentra el ejecutable CertUtil para la instalacion en Firefox"); //$NON-NLS-1$
			}

			if (!certutilFile.canExecute()) {
				throw new IOException("No hay permisos de ejecucion para Mozilla CertUtil"); //$NON-NLS-1$
			}

			certUtilPath = certutilFile.getAbsolutePath();
		}

		return certUtilPath;
	}

	/** Ejecuta la utilidad Mozilla CertUtil para la instalaci&oacute;n del certificado ra&iacute;z de confianza en Firefox.
	 * @param workingDir Ruta en la que buscar el ejecutable <code>certutil</code>.
	 * @param certFile Certificado ra&iacute;z.
	 * @param profilesDir Listado de directorios de perfiles de usuario de Mozilla Firefox.
	 * @throws IOException Cuando ocurre un error en el tratamiento de datos.
	 * @throws KeyStoreException Cuando ocurre un error en la inserci&oacute;n del certificado en el KeyStore. */
	private static void executeCertUtilToImport(final File workingDir,
			                                    final File certFile,
			                                    final Set<File> profilesDir) throws IOException,
	                                                                                KeyStoreException {

		final String certUtilPath = escapePath(getCertUtilPath(workingDir));
		boolean error = false;

		File certificateFile = certFile;
		if (certificateFile == null) {
			certificateFile = new File(workingDir, SSL_ROOT_CERTIFICATE_FILENAME);
		}

		// Obtenemos todos los directorios de perfil de Firefox del usuario
		for (final File profileDir : profilesDir) {
			if (!profileDir.isDirectory()) {
				continue;
			}

			final String[] certutilCommands = {
					certUtilPath,
					"-A", //$NON-NLS-1$
					"-d", //$NON-NLS-1$
					"sql:" + escapePath(profileDir.getAbsolutePath()), //$NON-NLS-1$
					"-i", //$NON-NLS-1$
					escapePath(certificateFile.getAbsolutePath()),
					"-n", //$NON-NLS-1$
					"\"" + ConfiguratorUtil.CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$
					"-t", //$NON-NLS-1$
					"\"C,,\"" //$NON-NLS-1$
			};

			error = execCommandLineCertUtil(workingDir, certutilCommands, false);
		}

		if (error) {
			throw new KeyStoreException(
				"Error en la instalacion del certificado de CA en alguno de los perfiles de usuario " //$NON-NLS-1$
					+ "de Firefox. Es posible que la aplicacion funcione en su propio perfil." //$NON-NLS-1$
			);
		}

	}

	/** Prepara los comandos de instalacion con certutil para la instalacion del certificado
	 * SSL y los ejecuta.
	 *  En Linux, se escribiran scripts intermedios que luego se ejecutaran como comandos.
	 *  En Windows se ejecuta certutil directamente como comando.
	 *  @param workingDir Directorio en el que se encuentra el subdirectorio de certutil.
	 *  @param command Comando a ejecutar, con el nombre de comando y sus par&aacute;metros
	 *  separados en un array.
	 *  @param chromeImport Indica si se esta realizando la importacion en el almacen del
	 *  sistema de Linux o en un almacen de Firefox.
	 *  @return <code>true</code> si la ejecuci&oacute;n de CertUtil termin&oacute; con error,
	 *  <code>false</code> si se ejecut&oacute; correctamente.
	 *  @throws IOException Si no se pudo realizar la propia ejecuci&oacute;n.
	 **/
	private static boolean execCommandLineCertUtil(final File workingDir, final String[] command, final boolean chromeImport)
			throws IOException {

		boolean error = false;
		final StringBuilder sb = new StringBuilder();

		for (final String s : command) {
			sb.append(s);
			sb.append(' ');
		}

		// Linux
		if (Platform.OS.LINUX.equals(Platform.getOS())) {
			// Ejecutamos el comando certutil en Linux
			final StringBuilder uninstall = new StringBuilder();
			String path;
			String uninstallPath;

			if (chromeImport) {
				//En Linux tambien se instala para todos los perfiles de
				// usuario del almacen de Chrome
				// Tenemos en command[7] la ruta del fichero .crt, sacamos de
				// ahi la ruta del directorio de instalacion

				uninstall.append(command[0] + ' ');
				uninstall.append("-D" + ' '); //$NON-NLS-1$
				uninstall.append("-d" + ' ');//$NON-NLS-1$
				uninstall.append(command[2] + ' ');
				uninstall.append("-n" + ' ');//$NON-NLS-1$
				uninstall.append(command[5] + ' ');
			}
			else {

				// tenemos en command[5] la ruta del fichero .cer, sacamos de
				// ahi la ruta del directorio de instalacion

				uninstall.append(command[0] + ' ');
				uninstall.append("-D" + ' '); //$NON-NLS-1$
				uninstall.append("-d" + ' ');//$NON-NLS-1$
				uninstall.append(command[3] + ' ');
				uninstall.append("-n" + ' ');//$NON-NLS-1$
				uninstall.append(command[7] + ' ');
			}

			final Random r = new Random();
			path = new File(workingDir, LINUX_SCRIPT_NAME + r.nextInt() + ".sh").getAbsolutePath(); //$NON-NLS-1$
			uninstallPath = new File(workingDir, LINUX_UNINSTALLSCRIPT_NAME + r.nextInt() + ".sh").getAbsolutePath(); //$NON-NLS-1$

			final File installScript = new File(path);
			final File uninstallScript = new File(uninstallPath);

			try (
					final FileOutputStream fout = new FileOutputStream(installScript, true);
					final FileOutputStream foutUninstall = new FileOutputStream(
							uninstallScript, true
							);
					) {
				fout.write(sb.toString().getBytes());
				foutUninstall.write(uninstall.toString().getBytes());
			}
			catch (final Exception e) {
				LOGGER.severe(
						"No se han podido generar los script para la instalacion de los certificados: " + e //$NON-NLS-1$
						);
				return true;
			}

			UnixUtils.addAllPermissionsToFile(uninstallScript);
			UnixUtils.addAllPermissionsToFile(installScript);

			// Primero desinstalamos las posibles versiones previas del certificado
			try {
				execCommand(new String[] {uninstallPath}, WITHOUT_TIMEOUT);
			}
			catch (final Exception e) {
				LOGGER.warning(
						"Error en la ejecucion del script para la desinstalacion del certificado del almacen de confianza: " + e); //$NON-NLS-1$
				return true;
			}

			// Despues, instalamos el certificado de confianza
			try {
				error = execCommand(new String[] {path}, WITHOUT_TIMEOUT);
			}
			catch (final Exception e) {
				LOGGER.severe(
						"Excepcion en la ejecucion del script para la instalacion del certificado en el almacen de confianza: " + e); //$NON-NLS-1$
				return true;
			}

			if (!uninstallScript.delete()) {
				LOGGER.warning("No puedo eliminar el fichero de script: " + uninstallScript.getName()); //$NON-NLS-1$
			}

			if (!installScript.delete()) {
				LOGGER.warning("No puedo eliminar el fichero de script: " + installScript.getName()); //$NON-NLS-1$
			}

			return error;
		}

		// Windows
		LOGGER.info("Se ejecutara el siguiente comando:\n" + sb.toString()); //$NON-NLS-1$
		try {
			error = execCommand(command, TIMEOUT);
		}
		catch (final Exception e) {
			LOGGER.severe(
					"Excepcion en la ejecucion del script para la instalacion del certificado en el almacen de confianza: " + e); //$NON-NLS-1$
			return true;
		}
		return error;
	}

	private static void importCARootOnFirefoxKeyStore (final File workingDir,
													   final File certFile,
			                                           final Set<File> profilesDir) throws KeyStoreException {

		try {
			// Usamos CertUtil para instalar el certificado en Firefox.
			executeCertUtilToImport(workingDir, certFile, profilesDir);

		} catch (final Exception e) {
			LOGGER.warning("No se pudo instalar la CA del certificado SSL para el socket en todos los perfiles de Firefox. Probablemente alguno de los perfiles nunca haya sido iniciado o no se este ejecutando Autofirma como administrador: " + e); //$NON-NLS-1$
			throw new KeyStoreException("Error al instalar la CA de confianza en el almacen de Firefox", e); //$NON-NLS-1$
		}

	}


	/** Ejecuta la aplicacion Mozilla CertUtil para eliminar el certificado de confianza ra&iacute;z
	 * SSL de Firefox.
	 * @param targetDir Directorio padre en el que se encuentra el directorio de certUtil.
	 * @throws IOException Cuando no se encuentra o puede leer alguno de los ficheros necesarios.
	 * @throws GeneralSecurityException Cuando no se puede ejecutar. */
	private static void executeCertUtilToDelete(final File targetDir) throws IOException, GeneralSecurityException {

		final String certUtilPath = getCertUtilPath(targetDir);

		//Se obtienen todos los usuarios para los que se va a desinstalar el certificado en Firefox
		final File usersBaseDir = new File(USERS_WINDOWS_PATH);
		final String[] userDirNames = usersBaseDir.list((current, name) -> new File(current, name).isDirectory());

		// Obtenemos todos los directorios de perfil de Firefox del usuario
		boolean error = false;
		for (final String userDirName : userDirNames) {

			// Nos saltamos siempre el usuario por defecto del sistema para
			// evitar corromperlo
			if (DEFAULT_WINDOWS_USER_NAME.equalsIgnoreCase(userDirName)) {
				continue;
			}

			final String profilesIniPath = USERS_WINDOWS_PATH + userDirName + WINDOWS_MOZILLA_PATH;
			if (new File(profilesIniPath).exists()) {
				LOGGER.info("Se ha encontrado el perfil de Firefox: " + profilesIniPath); //$NON-NLS-1$
				MozillaKeyStoreUtilities.getMozillaUserProfileDirectoryWindows(
						profilesIniPath
						);

				final String userProfile = MozillaKeyStoreUtilities.getMozillaUserProfileDirectoryWindows(
						profilesIniPath);
				if (userProfile == null) {
					LOGGER.warning("No se ha encontrado un directorio de perfil de un usuario"); //$NON-NLS-1$
					error = true;
					continue;
				}

				final File profilesDir = new File(userProfile).getParentFile();
				for (final File profileDir : profilesDir.listFiles()) {
					if (!profileDir.isDirectory()) {
						continue;
					}

					final String[] certutilCommands = {
							"\"" + certUtilPath + "\"", //$NON-NLS-1$ //$NON-NLS-2$
							"-D", //$NON-NLS-1$
							"-d", //$NON-NLS-1$
							"\"sql:" + profileDir.getAbsolutePath() + "\"", //$NON-NLS-1$ //$NON-NLS-2$
							"-n", //$NON-NLS-1$
							"\"" + ConfiguratorUtil.CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$
					};

					boolean detectedError;
					try {
						detectedError = execCommand(certutilCommands, TIMEOUT);
					}
					catch (final Exception e) {
						LOGGER.severe(
								"Excepcion en la ejecucion del script para el borrado del certificado en el almacen de confianza de alguno de los perfiles de Firefox: " + e); //$NON-NLS-1$
						detectedError = true;
					}

					// Si detectamos algun error al ejecutar el comando en alguno de los perfiles,
					// marcamos como que hubo algun error
					if (detectedError) {
						error = true;
					}
				}
			}
		}

		if (error) {
			throw new KeyStoreException("Error en el borrado del certificado de CA en alguno de los perfiles de usuario de Firefox"); //$NON-NLS-1$
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
	 * @throws IOException Cuando ocurre un error al descomprimir o copiar.
	 */
	static void copyConfigurationFiles(final File workingDir) throws IOException {

		final File certutil = new File(workingDir, DIR_CERTUTIL);
		if (!certutil.exists()) {
			uncompressResource(RESOURCE_BASE + FILE_CERTUTIL, workingDir);
		}
	}

	/** Descomprime un fichero ZIP de recurso al disco.
	 * @param resource Ruta del recurso ZIP.
	 * @param outDir Directorio local en el que descomprimir.
	 * @throws IOException Cuando ocurre un error al descomprimir.
	 **/
	private static void uncompressResource(final String resource, final File outDir) throws IOException {
		int n;
		ZipEntry entry;
		final byte[] buffer = new byte[1024];
		try (final ZipInputStream zipIs = new ZipInputStream(
				RestoreConfigFirefox.class.getResourceAsStream(resource));) {
			// en linux el funcionamiento es ligeramente diferente
			if (Platform.OS.LINUX == Platform.getOS()) {

				new File(outDir, DIR_CERTUTIL).mkdirs();

				while ((entry = zipIs.getNextEntry()) != null) {

					final File outFile = new File(outDir, entry.getName()).getCanonicalFile();

					 if (!isParent(outDir, outFile)) {
						 zipIs.closeEntry();
						 throw new IOException("Se ha encontrado en el archivo comprimido una ruta que apuntaba fuera del directorio de destino"); //$NON-NLS-1$
					 }

					try (final OutputStream outFis = new FileOutputStream(outFile);) {
						while ((n = zipIs.read(buffer)) > 0) {
							outFis.write(buffer, 0, n);
						}
						outFis.flush();
					}

					zipIs.closeEntry();
				}
			}
			else {
				while ((entry = zipIs.getNextEntry()) != null) {
					final File outFile = new File(outDir, entry.getName());

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
	 * Obtiene los perfiles de usuarios de Firefox en Windows
	 * @return Array de Files con los perfiles de usuarios de Firefox
	 */
	private static ArrayList<File> getFirefoxProfilesDir() {

		final ArrayList<File> fileList = new ArrayList<>();

		// Se obtienen todos los usuarios para los que se va a instalar el
		// certificado en Firefox
		final File usersBaseDir = new File(USERS_WINDOWS_PATH);
		final String[] userDirNames = usersBaseDir.list((current, name) -> new File(current, name).isDirectory());

		try {
			for(final String userDirName : userDirNames) {
				// Nos saltamos siempre el usuario por defecto del sistema para evitar corromperlo
				if (DEFAULT_WINDOWS_USER_NAME.equalsIgnoreCase(userDirName)) {
					continue;
				}

				if(new File(USERS_WINDOWS_PATH + userDirName + WINDOWS_MOZILLA_PATH).exists()) {
					fileList.add(
							new File(
									MozillaKeyStoreUtilities.getMozillaUserProfileDirectoryWindows(
											USERS_WINDOWS_PATH + userDirName + WINDOWS_MOZILLA_PATH)
							).getParentFile());
					LOGGER.info("Fichero de perfiles de Firefox: " + LoggerUtil.getCleanUserHomePath(USERS_WINDOWS_PATH + userDirName + WINDOWS_MOZILLA_PATH)); //$NON-NLS-1$
				}
			}
		}
		catch (final Exception e) {
			LOGGER.warning("No se encontro el directorio de perfiles de Mozilla Firefox: " + e); //$NON-NLS-1$
		}
		return fileList;
	}

	/** Devuelve un listado con los directorios donde se encuentra el fichero <i>profiles.ini</i>
	 * de firefox en Linux y en Windows.
	 * @param users Listado de usuarios del sistema.
	 * @return Listado de directorios donde se encuentra el fichero <i>profiles.ini</i>. */
	private static List<File> getMozillaUsersProfilesPath(final List<String> users){
		final List<String> pathProfiles = new ArrayList<>();
		final List<File> path = new ArrayList<>();
		if (Platform.OS.LINUX.equals(Platform.getOS())) {
			pathProfiles.add(UBUNTU_22_MOZILLA_PATH);
			pathProfiles.add(LINUX_MOZILLA_PATH);
		}
		else if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			pathProfiles.add(WINDOWS_MOZILLA_PATH);
		}
		else {
			throw new IllegalArgumentException("Sistema operativo no soportado: " + Platform.getOS()); //$NON-NLS-1$
		}
		for (final String usr : users) {
			for (final String pathProfile : pathProfiles) {
				final File mozillaPath = new File(usr, pathProfile);
				if (mozillaPath.isFile()) {
					path.add(mozillaPath);
				}
			}
		}
		return path;
	}

	/** Devuelve un listado de directorios donde se encuentran los perfiles de usuario de firefox.
	 * @param profilesPath Listado de directorios que contienen un fichero <i>profiles.ini</i>.
	 * @return Listado de directorios donde se encuentran los perfiles de usuario de Firefox. */
	private static Set<File> getProfiles(final List<File> profilesPath){
		final String PATH = "Path="; //$NON-NLS-1$
		final Set<File> profile = new HashSet<>();
		for (final File path: profilesPath){
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
								0, path.getAbsolutePath().lastIndexOf(File.separator) + 1) + line.substring(PATH.length()
							)
						);
						if (file.exists() && file.isDirectory()){
							profile.add(file);
						}
					}
				}
			}
			catch (final Exception e) {
				LOGGER.severe("Error al buscar los directorios de perfiles de Firefox: " + e); //$NON-NLS-1$
			}
		}
		return profile;
	}


	/** Ejecuta un comando de consola.
	 * @param command Nombre del comando y sus argumentos
	 * @return {@code true} si la ejecuci&oacute;n devolvi&oacute; alg&uacute;n error, {@code false} en caso contrario.
	 * @throws IOException Si hay problemas ejecutando el comando. */
	private static boolean execCommand(final String[] command, final int timeout) throws IOException {

		LOGGER.info("Se ejecutara el siguiente comando:\n" + Arrays.toString(command)); //$NON-NLS-1$
		final Process process = new ProcessBuilder(command).start();

		// Temporizador para detener el proceso una vez se sobrepase un tiempo determinado. Esto es necesario
		// porque hay situaciones que bloquean el proceso, como cuando el almacen de claves esta protegido
		// con contrasena
		if (timeout > 0) {
			new KillProcessTimer(timeout, process).start();
		}

		// Cuando certUtil se ejecuta correctamente no hay salida de ningun tipo, asi que se interpreta
		// cualquier salida como un error
		final StringBuffer buffer = new StringBuffer();
		try (
			final InputStream resIs = process.getInputStream();
			final BufferedReader resReader = new BoundedBufferedReader(
					new InputStreamReader(resIs),
					256, // Maximo 256 lineas de salida
					1024 // Maximo 1024 caracteres por linea
					);
			) {
			String line;
			while ((line = resReader.readLine()) != null) {
				buffer.append(line).append("\n"); //$NON-NLS-1$
			}
			if (buffer.length() > 0) {
				LOGGER.warning("Error devuelto por certutil en el flujo de salida: " + buffer.toString()); //$NON-NLS-1$
				return true;
			}
		}

		buffer.setLength(0);
		try (
		final InputStream errIs = process.getErrorStream();
		final BufferedReader errReader = new BoundedBufferedReader(
				new InputStreamReader(errIs),
				256, // Maximo 256 lineas de salida
				1024 // Maximo 1024 caracteres por linea
				);
		) {
			String line;
			while ((line = errReader.readLine()) != null) {
				buffer.append(line).append("\n"); //$NON-NLS-1$
			}
			if (buffer.length() > 0) {
				LOGGER.warning("Error devuelto por certutil en el flujo de error: " + buffer.toString()); //$NON-NLS-1$
				return true;
			}
		}

		return false;
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
	static void configureUseSystemTrustStore(final boolean enable) throws IOException, MozillaProfileNotFoundException {

		// Obtenemos el directorio de usuarios
		final List<String> userDirPaths = getUsersDirectories();
		configureUseSystemTrustStore(enable, userDirPaths);
	}

	/**
	 * Configura el que se habilite o deshabilite el uso del almac&eacute;n de cofianza del
	 * sistema operativo como almacen de confianza de Firefox.
	 * @param enable {@code true} para habilitar la confianza en los certificados ra&iacute;z del
	 * almac&eacute;n de confianza del sistema adem&aacute;s de en los suyos propios,
	 * {@code false} en caso contrario.
	 * @param userDirPaths Listado de directorios de los usuarios.
	 * @throws IOException Cuando no se puede crear o editar la configuraci&oacute;n.
	 * @throws MozillaProfileNotFoundException Cuando no se han encontrado perfiles de Firefox.
	 */
	static void configureUseSystemTrustStore(final boolean enable, final List<String> userDirPaths) throws IOException, MozillaProfileNotFoundException {

		// Dados los usuarios sacamos el directorio de perfiles de mozilla en caso de que lo tengan
		final List <File> mozillaUsersProfilesPath = getMozillaUsersProfilesPath(userDirPaths);
		// Para cada usuario tenemos sus distintos directorios de perfiles
		final Set <File> mozillaProfileDirs = getProfiles(mozillaUsersProfilesPath);
		if (mozillaProfileDirs.isEmpty()) {
			throw new MozillaProfileNotFoundException();
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

	private static List<String> getUsersDirectories() {

		final List<String> userDirPaths = new ArrayList<>();
		if (Platform.getOS() == Platform.OS.WINDOWS) {
			final File usersBaseDir = new File(USERS_WINDOWS_PATH);
			final File[] userDirs = usersBaseDir.listFiles((current, name) -> new File(current, name).isDirectory());
			for (final File userDir : userDirs) {
				userDirPaths.add(userDir.getAbsolutePath());
			}
		}

		return userDirPaths;
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

	/**
	 * Temporizador para la interrupci&oacute;n de un proceso una vez excedido un tiempo determinado.
	 */
	private static class KillProcessTimer extends Timer implements ActionListener {

		/** Serial Id. */
		private static final long serialVersionUID = -2527661514649132415L;

		private final Process process;

		public KillProcessTimer(final int delay, final Process process) {
			super(delay, null);
			addActionListener(this);
			this.process = process;
		}

		@Override
		public void actionPerformed(final ActionEvent e) {
			if (this.process != null && this.process.isAlive()) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"Se interrumpe el proceso por sobrepasar el tiempo maximo configurado..."); //$NON-NLS-1$
				// Destruimos el proceso
				this.process.destroy();
			}
			// Detenemos el temporizador
			stop();
		}
	}

}
