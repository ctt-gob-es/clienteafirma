/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.restoreconfig;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.Timer;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.keystores.mozilla.MozillaProfile;
import es.gob.afirma.keystores.mozilla.ProfilesIni;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;
import es.gob.afirma.standalone.so.macos.UnixUtils;


/**Contiene la l&oacute;gica para realizar las tareas de restauraci&oacute;n
 * asociadas al navegador Firefox para Linux. */
final class RestoreConfigFirefoxLinux {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int TIMEOUT = 5000;

	private static final String NSS_CERT_DB_FILENAME = "cert9.db"; //$NON-NLS-1$

	private static final String SCRIPT_NAME = "restore-"; //$NON-NLS-1$
	private static final String[] KNOWN_MOZILLA_PROFILES_INI_SUBPATH = new String[] {
			"/.config/mozilla/firefox/profiles.ini", // Ruta instalador oficial //$NON-NLS-1$,
			"/.mozilla/firefox/profiles.ini", // Ruta por defecto //$NON-NLS-1$,
			"/snap/firefox/common/.mozilla/firefox/profiles.ini" // Ruta en Ubuntu
	};

	/** Listado de rutas conocidas, relativas al directorio de usuario, de almacenes NSS en Linux. */
	private static final String[] NSS_DIR_SUBPATH = new String[] {
			"/.pki/nssdb",	// Ruta clasica del almacen del sistema //$NON-NLS-1$
			"/.local/share/pki/nssdb", // Ruta clasica de almacen del sistema en Ubuntu //$NON-NLS-1$
			"/snap/chromium/current/.local/share/pki/nssdb", // Ruta clasica de Chromium en Ubuntu //$NON-NLS-1$
			"/snap/chromium/current/.pki/nssdb", // Ruta clasica de Chromium en Ubuntu (Antigua) //$NON-NLS-1$
			"/.var/app/org.chromium.Chromium/data/pki/nssdb", // Ruta clasica de Chromium  en Fedora //$NON-NLS-1$
			"/snap/brave/current/.local/share/pki/nssdb", // Ruta clasica de Brave en Ubuntu //$NON-NLS-1$
			"/.var/app/com.brave.Browser/data/pki/nssdb" // Ruta clasica de Brave en Fedora //$NON-NLS-1$
	};

	static final String CERTUTIL_EXE = "certutil"; //$NON-NLS-1$
//	private static final String CERTUTIL_INTERNAL_RESOURCE = "/linux/certutil.linux.zip"; //$NON-NLS-1$

	private final Component parentComponent;

	private List<MozillaProfile> profileDirectories = null;

	RestoreConfigFirefoxLinux(final Component parentComponent) {
		this.parentComponent = parentComponent;
	}

	/**
	 * Instala el certificado en el almac&eacute;n del sistema (el usado por Chrome).
	 * @param workingDir Directorio en el que se encuentra el subdirectorio de <code>certutil</code>.
	 * @param rootCertFile Fichero del certificado ra&iacute;z a instalar.
	 * @param userHomeDirs Listado de directorios de usuario.
	 */
	void installRootCAInNSSKeyStore(final File workingDir,
	                                       final File rootCertFile,
	                                       final List<File> userHomeDirs ) {

		final String certUtilPath = getCertUtilPath();

		for ( final File userHomeDir : userHomeDirs) {

			for (final String nssDirSubpath : NSS_DIR_SUBPATH) {

				try {
					final File nssKeystoreDir = new File(userHomeDir, nssDirSubpath);
					if (nssKeystoreDir.isDirectory() && new File(nssKeystoreDir, NSS_CERT_DB_FILENAME).isFile()) {
						final String[] certutilCommands = {
								certUtilPath,
								"-d", //$NON-NLS-1$
								"sql:" + escapePath(nssKeystoreDir.getAbsolutePath()), //$NON-NLS-1$
								"-A", //$NON-NLS-1$
								"-n", //$NON-NLS-1$
								"\"" + ConfiguratorUtil.CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$
								"-i", //$NON-NLS-1$
								escapePath(rootCertFile.getAbsolutePath()),
								"-t", //$NON-NLS-1$
								"\"C,,\"" //$NON-NLS-1$
						};
						execCertUtilCommand(workingDir, certutilCommands);
					}

				} catch (Exception e) {
					LOGGER.log(Level.WARNING, "No se pudo instalar el certificado en el almacen NSS del usuario " //$NON-NLS-1$
							+ userHomeDir.getName() + " en la ruta " + nssDirSubpath, e); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}
	}


	/**
	 * Instala el certificado en el almac&eacute;n del sistema (el usado por Chrome).
	 * @param workingDir Directorio en el que se encuentra el subdirectorio de <code>certutil</code>.
	 * @param userHomeDirs Listado de directorios de usuario.
	 */
	void uninstallRootCAFromNSSKeystores(final File workingDir,
	                                       final List<File> userHomeDirs ) {

		final String certUtilPath = getCertUtilPath();

		for (final File userHomeDir : userHomeDirs) {

			for (final String nssDirSubpath : NSS_DIR_SUBPATH) {

				try {
					final File nssKeystoreDir = new File(userHomeDir, nssDirSubpath);
					if (nssKeystoreDir.isDirectory() && new File(nssKeystoreDir, NSS_CERT_DB_FILENAME).isFile()) {
                        LOGGER.info("Se va a desinstalar el certificado del almacen NSS del usuario " + userHomeDir.getName() + " en la ruta " + nssDirSubpath); //$NON-NLS-1$ //$NON-NLS-2$
						final String[] certutilCommands = {
								certUtilPath, //$NON-NLS-1$ //$NON-NLS-2$
								"-D", //$NON-NLS-1$
								"-d", //$NON-NLS-1$
								"sql:" + escapePath(nssKeystoreDir.getAbsolutePath()), //$NON-NLS-1$
								"-n", //$NON-NLS-1$
								"\"" + ConfiguratorUtil.CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$
						};
						execCertUtilCommand(workingDir, certutilCommands);
					}
				}
				catch (InvalidObjectException e) {
					LOGGER.info("No se ha encontrado el certificado en el almacen NSS del usuario " //$NON-NLS-1$
							+ userHomeDir.getName() + " en la ruta " + nssDirSubpath + ", se omite su desinstalacion"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				catch (Exception e) {
					LOGGER.log(Level.WARNING, "No se pudo desinstalar el certificado del almacen NSS del usuario " //$NON-NLS-1$
							+ userHomeDir.getName() + " en la ruta " + nssDirSubpath, e); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}
	}

	/** Instala el certificado en un perfil de Firefox.
	 * En ambos casos, es necesario crear un <i>script</i> intermedio con el comando <code>certutil</code> y sus argumentos
	 * y posteriormente ejecutarlo como un comando de consola.
	 * @param workingDir Directorio de instalaci&oacute;n del sistema
	 * @param certFile Fichero del certificado que debemos instalar.
	 * @param profile Perfil de Mozilla.
	 * @throws IOException Cuando ocurre un error durante la importaci&oacute;n. */
	void installRootCAMozillaKeyStore(final File workingDir,
	                                           final File certFile,
	                                           final MozillaProfile profile) throws IOException, PasswordProtectedException {

		// Usamos CertUtil para instalar el certificado en Firefox.
		final String certUtilPath = getCertUtilPath();

		// Inicializamos el perfil para que quede registrada la contrasena del almacen en caso de tenerla
		if (!profile.isPrepared()) {
			RestoreConfigFirefoxCommon.initProfile(workingDir, profile, certUtilPath, true,false, this.parentComponent);
		}

		// Instalamos el nuevo certificado
		String[] certutilCommands = getInstallCertCommand(certUtilPath, profile, certFile);

		try {
			execCertUtilCommand(workingDir, certutilCommands);
		}
		catch (IOException e) {
			// Si el perfil tiene contrasena, puede que se produzca un error debido a que no se haya indicado,
			// pero que en lugar indicarse que tenia contrasena, se de un error indicando que no nos habiamos
			// logueado en el token PKCS#11. Pôr norma general, si se produce un error al instalar en un perfil
			// con contrasena, se asumira que el problema es de que el perfil esta protegido
			if (profile.hasMasterPassword()) {
				throw new PasswordProtectedException("Fallo la instalacion en el perfil de Firefox protegido con contrasena" //$NON-NLS-1$
						+ profile.getName(), e); //$NON-NLS-1$
			}
			throw e;
		}
	}

	/** Desinstala el certificado de un perfil de Firefox.
	 * @param workingDir Directorio de instalaci&oacute;n del sistema
	 * @param profile Perfil de Mozilla.
	 * @throws IOException Cuando ocurre un error durante la desinstalaci&oacute;n. */
	void uninstallRootCAMozillaKeyStore(final File workingDir,
	                                           final MozillaProfile profile) throws IOException {

		if (!profile.hasNssKeyStore()) {
			LOGGER.warning("El perfil de Firefox " + profile.getName() //$NON-NLS-1$
					+ " no tiene un almacén NSS inicializado, se omite la desinstalación del certificado"); //$NON-NLS-1$
			return;
		}

		// Usamos CertUtil para instalar el certificado en Firefox.
		final String certUtilPath = getCertUtilPath();

		// Inicializamos el perfil para que quede registrada la contrasena del almacen en caso de tenerla
		if (!profile.isPrepared()) {
			RestoreConfigFirefoxCommon.initProfile(workingDir, profile, certUtilPath, true,false, this.parentComponent);
		}

		// Desinstalamos el certificado anterior si lo hubiese
		String[] certutilCommands = getRemoveCertCommand(certUtilPath, profile);

		// Ejecutamos el comando
		try {
			execCertUtilCommand(workingDir, certutilCommands);
		}
		catch (InvalidObjectException e) {
			LOGGER.info("No se encontro el certificado anterior en el perfil " + profile.getName()); //$NON-NLS-1$
		}
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

	private static String escapePath(final String path) {
		if (path == null) {
			throw new IllegalArgumentException(
				"La ruta a 'escapar' no puede ser nula" //$NON-NLS-1$
			);
		}
		return path.replace(" ", "\\ "); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * Obtiene el path para la llamada a CertUtil.
	 * @return Referencia a CertUtil.
	 */
	public static String getCertUtilPath() {
		return CERTUTIL_EXE;
	}

	/**
	 * Prepara un script para la ejecucion del comando indicado de certutil y lo ejecuta.
	 * @param workingDir   Directorio en el que se encuentra el subdirectorio de certutil.
	 * @param command      Comando a ejecutar, con el nombre de comando y sus par&aacute;metros
	 *                     separados en un array.
	 * @throws InvalidObjectException Cuando el comando devuelve un error de certificado no encontrado.
	 * @throws IOException Cuando falla la ejecuci&oacute;n del comando.
	 **/
	private static void execCertUtilCommand(final File workingDir, final String[] command)
			throws InvalidObjectException, IOException {

		final StringBuilder sb = new StringBuilder();
		for (final String s : command) {
			sb.append(s);
			sb.append(' ');
		}
		LOGGER.info("Se ejecutara el siguiente comando a traves de un script: " + LoggerUtil.getCleanUserHomePath(sb.toString())); //$NON-NLS-1$

		// Creamos un script temporal para ejecutar el comando de certutil
		final Random r = new Random();
		String path = new File(workingDir, SCRIPT_NAME + r.nextInt() + ".sh").getAbsolutePath(); //$NON-NLS-1$

		final File installScript = new File(path);
		try (
				final FileOutputStream fout = new FileOutputStream(installScript, true)
		) {
			fout.write(sb.toString().getBytes());
		} catch (final Exception e) {
			throw new IOException("Error al generar el script para el uso de certutil", e); //$NON-NLS-1$
		}

		UnixUtils.addAllPermissionsToFile(installScript);

		// Ejecutamos el comando
		try {
			execCommand(new String[] { path });
		}
		catch (final InvalidObjectException e) {
			throw e;
		}
		catch (final InvalidPasswordException e) {
			throw e;
		}
		catch (final Exception e) {
			throw new IOException("Error al ejecutar el script de certutil", e); //$NON-NLS-1$
		}
		finally {
			installScript.delete();
		}
	}

//	/**
//	 * Descomprime y copia los ficheros de configuraci&oacute;n de certutil
//	 * @param workingDir Directorio al que descomprimir las herramientas de configuraci&oacute;n
//	 * @throws IOException Cuando ocurre un error al descomprimir o copiar.
//	 */
//	static void copyConfigurationFiles(final File workingDir) throws IOException {
//
//		final File certutil = new File(workingDir, DIR_CERTUTIL);
//		if (!certutil.exists()) {
//			uncompressResource(RESOURCE_BASE + FILE_CERTUTIL, workingDir);
//		}
//	}
//
//	/** Descomprime un fichero ZIP de recurso al disco.
//	 * @param resource Ruta del recurso ZIP.
//	 * @param outDir Directorio local en el que descomprimir.
//	 * @throws IOException Cuando ocurre un error al descomprimir.
//	 **/
//	private static void uncompressResource(final String resource, final File outDir) throws IOException {
//		int n;
//		ZipEntry entry;
//		final byte[] buffer = new byte[1024];
//		try (final ZipInputStream zipIs = new ZipInputStream(
//				RestoreConfigFirefoxLinux.class.getResourceAsStream(resource));) {
//
//			new File(outDir, DIR_CERTUTIL).mkdirs();
//
//			while ((entry = zipIs.getNextEntry()) != null) {
//
//				final File outFile = new File(outDir, entry.getName()).getCanonicalFile();
//
//				if (!RestoreConfigFirefoxCommon.isAncestorDir(outDir, outFile)) {
//					zipIs.closeEntry();
//					throw new IOException("Se ha encontrado en el archivo comprimido una ruta que apuntaba fuera del directorio de destino"); //$NON-NLS-1$
//				}
//
//				try (final OutputStream outFis = new FileOutputStream(outFile);) {
//					while ((n = zipIs.read(buffer)) > 0) {
//						outFis.write(buffer, 0, n);
//					}
//					outFis.flush();
//				}
//
//				zipIs.closeEntry();
//			}
//		}
//	}

//	/** Devuelve un listado de directorios donde se encuentran los perfiles de usuario de firefox.
//	 * @param profilesPath Listado de directorios que contienen un fichero <i>profiles.ini</i>.
//	 * @return Listado de directorios donde se encuentran los perfiles de usuario de Firefox. */
//	static Set<File> getProfiles(final List<File> usersHomeDirs) {
//		final String PATH = "Path="; //$NON-NLS-1$
//		final Set<File> profile = new HashSet<>();
//		for (final File path: profilesPath){
//			String line;
//			try (
//				final InputStream resIs = new FileInputStream(path);
//				final BufferedReader resReader = new BoundedBufferedReader(
//					new InputStreamReader(resIs),
//					256, // Maximo 256 lineas de salida (256 perfiles por "profiles.ini")
//					2048 // Maximo 2048 caracteres por linea
//				);
//			) {
//				while ((line = resReader.readLine()) != null) {
//					if (line.startsWith(PATH)){
//						final File file = new File(
//							path.getAbsolutePath().substring(
//								0, path.getAbsolutePath().lastIndexOf(File.separator) + 1) + line.substring(PATH.length()
//							)
//						);
//						if (file.exists() && file.isDirectory()){
//							profile.add(file);
//						}
//					}
//				}
//			}
//			catch (final Exception e) {
//				LOGGER.severe("Error al buscar los directorios de perfiles de Firefox: " + e); //$NON-NLS-1$
//			}
//		}
//		return profile;
//	}

	/**
	 * Devuelve un listado con todos los directorios de perfil de usuario de Firefox.
	 * @return Listado de directorios de perfil perfiles de usuario de Firefox.
	 */
	List<MozillaProfile> getMozillaProfiles(final List<File> usersHomeDirs) {

		// Si ya se han obtenido previamente, devolvemos la lista cacheada
		if (this.profileDirectories != null) {
			return this.profileDirectories;
		}

		// Obtenemos los perfiles de todos los usuarios del sistema
		final List<MozillaProfile> profiles = new ArrayList<>();
		for (final File profilesIniFile : getMozillaProfilesFiles(usersHomeDirs)) {
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


	/**
	 * Obtiene los ficheros de definicion de perfiles de Firefox ("profiles.ini") de todos los usuarios.
	 * @param userHomeDirs Listado de directorios de usuario.
	 * @return Array de directorios con los perfiles de usuario.
	 */
	private List<File> getMozillaProfilesFiles(List<File> userHomeDirs) {

		final List<File> fileList = new ArrayList<>();

		// Obtenemos los ficheros de perfiles de todos los usuarios activos del sistema
		for (final File homeDir : userHomeDirs) {
			for (final String profileIniSubPath : KNOWN_MOZILLA_PROFILES_INI_SUBPATH) {
				final File profilesIniFile = new File(homeDir, profileIniSubPath);
				if (profilesIniFile.isFile()) {
					fileList.add(profilesIniFile);
				}
			}
		}

		return fileList;
	}

	/**
	 * Ejecuta un comando de consola.
	 * @param command Nombre del comando y sus argumentos
	 * @throws InvalidObjectException Si el comando devuelve un error de certificado no encontrado.
	 * @throws InvalidPasswordException Si el comando devuelve un error de contrase&ntilde;a.
	 * @throws IOException Si hay problemas ejecutando el comando.
	 */
	private static void execCommand(final String[] command) throws InvalidObjectException, InvalidPasswordException,
			IOException {

		final Process process = new ProcessBuilder(command).start();

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
					);
			) {
			String line;
			while ((line = resReader.readLine()) != null) {
				buffer.append(line).append("\n"); //$NON-NLS-1$
			}
			if (buffer.length() > 0) {
				LOGGER.warning("Error devuelto por certutil en el flujo de salida: " + LoggerUtil.getCleanUserHomePath(buffer.toString())); //$NON-NLS-1$
				throw new IOException("Error devuelto por certutil en el flujo de salida"); //$NON-NLS-1$
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
				String result = buffer.toString();
				LOGGER.warning("Error devuelto por certutil en el flujo de error: " + LoggerUtil.getCleanUserHomePath(result)); //$NON-NLS-1$
				process.destroyForcibly();
				if (result.contains("Unrecognized Object Identifier")
						|| result.contains("could not find certificate")) { //$NON-NLS-1$ //$NON-NLS-2$
					throw new InvalidObjectException("No se ha encontrado el certificado");
				}
				if (result.contains("Enter Password") || result.contains("password")) { //$NON-NLS-1$ //$NON-NLS-2$
					throw new InvalidPasswordException();
				}
				throw new IOException("Error devuelto por certutil en el flujo de error"); //$NON-NLS-1$
			}
		}
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
