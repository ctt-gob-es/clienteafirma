/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.restoreconfig;

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
import java.nio.file.Files;
import java.security.KeyStoreException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;
import es.gob.afirma.standalone.so.macos.ShellScript;
import es.gob.afirma.standalone.so.macos.UnixUtils;


/**
 * Restaurador de la instalaci&oacute;n de la integraci&oacute;n de la aplicaci&oacute;n con Firefox. */
final class RestoreConfigFirefoxMacOS {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String MACOSX_MOZILLA_PATH = "/Library/Application Support/firefox/profiles.ini";//$NON-NLS-1$
	private static final String PROFILES_INI_PATH_PREFIX = "Path="; //$NON-NLS-1$

	private static final String DIR_CERTUTIL = "certutil"; //$NON-NLS-1$
	private static final String CERTUTIL_EXE = "certutil"; //$NON-NLS-1$;
	private static final String RESOURCE_CERTUTIL = "/osx/certutil.osx.zip"; //$NON-NLS-1$

	// ==============
	// == Propiedades usadas para configurar FireFox para que confie en los certificados del Llavero
	// ==============

	private static final String CUSTOM_PROFILE_PREFERENCES_FILENAME = "user.js"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_FILE_HEADER =
			"// === PROPIEDADES PERSONALIZADAS DE CONFIGURACION ===\r\n"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_ENTERPRISE_ROOTS_HEADER =
			"\r\n// Confianza en los certificados raices del almacen del sistema\r\n"; //$NON-NLS-1$
	private static final String MOZ_PREFERENCE_ENTERPRISE_ROOTS = "security.enterprise_roots.enabled"; //$NON-NLS-1$
	private static final String BREAK_LINE = "\r\n"; //$NON-NLS-1$


	private final List<File> usersDirs;
	private final RestoreConfigPanel restorePanel;

	private List<File> profilesDirs = null;

	/**
	 * Construye el restaurador de Firefox para macOS.
	 * @param userDirs Listado de directorios de usuario.
	 */
	public RestoreConfigFirefoxMacOS(final List<File> userDirs, final RestoreConfigPanel restorePanel) {
		this.usersDirs = userDirs;
		this.restorePanel = restorePanel;
	}

	/**
	 * Indica si se encuentran ficheros de perfil de Firefox en el sistema.
	 * @return
	 */
	public boolean hasProfiles() {
		final List<File> profiles = getMozillaProfiles();
		return !profiles.isEmpty();
	}

	/**
	 * Genera el <i>script</i> de instalaci&oacute; del certificado en Firefox para MacOSX y LINUX.
	 * En ambos casos, es necesario crear un <i>script</i> intermedio con el comando <code>certutil</code> y sus argumentos
	 * y posteriormente ejecutarlo como un comando de consola.
	 * @param appDir Directorio de instalaci&oacute;n del sistema
	 * @param rootCaFile Fichero del certificado que debemos instalar.
	 * @param usersDirs Listado de carpetas de los usuarios.
	 * @throws MozillaProfileNotFoundException No se ha encontrado el directorio de perfiles de Mozilla.
	 * @throws KeyStoreException Cuando ocurre un error durante la importaci&oacute;n.
	 * @throws IOException Cuando no se ha podido copiar CertUtil al directorio de aplicaci&oacute;n.
	 */
	public void installRootCAMozillaKeyStore(final File appDir, final File rootCaFile)
			throws MozillaProfileNotFoundException, KeyStoreException, IOException {
		manageMozillaKeyStore(appDir, false, rootCaFile);
	}

	/**
	 * Genera el <i>script</i> de instalaci&oacute; del certificado en Firefox para MacOSX y LINUX.
	 * En ambos casos, es necesario crear un <i>script</i> intermedio con el comando <code>certutil</code> y sus argumentos
	 * y posteriormente ejecutarlo como un comando de consola.
	 * @param appDir Directorio de instalaci&oacute;n del sistema
	 * @param rootCaFile Fichero del certificado que debemos instalar.
	 * @param usersDirs Listado de carpetas de los usuarios.
	 * @throws MozillaProfileNotFoundException No se ha encontrado el directorio de perfiles de Mozilla.
	 * @throws KeyStoreException Cuando ocurre un error durante la importaci&oacute;n.
	 * @throws IOException Cuando no se ha podido copiar CertUtil al directorio de aplicaci&oacute;n.
	 */
	public void uninstallRootCAMozillaKeyStore(final File appDir, final File rootCaFile)
			throws MozillaProfileNotFoundException, KeyStoreException, IOException {
		manageMozillaKeyStore(appDir, true, null);
	}

	/**
	 * Genera el <i>script</i> de instalaci&oacute; del certificado en Firefox para MacOSX y LINUX.
	 * En ambos casos, es necesario crear un <i>script</i> intermedio con el comando <code>certutil</code> y sus argumentos
	 * y posteriormente ejecutarlo como un comando de consola.
	 * @param appDir Directorio de instalaci&oacute;n del sistema
	 * @param rootCaFile Fichero del certificado que debemos instalar.
	 * @param usersDirs Listado de carpetas de los usuarios.
	 * @throws MozillaProfileNotFoundException No se ha encontrado el directorio de perfiles de Mozilla.
	 * @throws KeyStoreException Cuando ocurre un error durante la importaci&oacute;n.
	 * @throws IOException Cuando no se ha podido copiar CertUtil al directorio de aplicaci&oacute;n.
	 */
	public void reinstallRootCAMozillaKeyStore(final File appDir, final File rootCaFile)
			throws MozillaProfileNotFoundException, KeyStoreException, IOException {
		manageMozillaKeyStore(appDir, true, rootCaFile);
	}

	/**
	 * Opera sobre el almacen de Firefox para eliminar y/o instalar el certificado SSL de la aplicaci&oacute;n.
	 * @param appDir Directorio de instalaci&oacute;n del sistema
	 * @param uninstall Indica si se deben desinstalar los certificados ya instalados. Esta operaci&oacute;n se realiza
	 * @param rootCaFile Fichero del certificado que debemos instalar o {@code null} si no queremos installar ninguno.
	 * antes de la de instalaci&oacute;n si procede.
	 * @throws MozillaProfileNotFoundException No se ha encontrado el directorio de perfiles de Mozilla.
	 * @throws KeyStoreException Cuando ocurre un error durante la importaci&oacute;n.
	 * @throws IOException Cuando no se ha podido copiar CertUtil al directorio de aplicaci&oacute;n.
	 */
	private void manageMozillaKeyStore(final File appDir, final boolean uninstall, final File rootCaFile)
			throws MozillaProfileNotFoundException, KeyStoreException, IOException {

		// Obtenemos el listado de directorios de perfil
		final List <File> profileDirs = getMozillaProfiles();
		if (profileDirs.isEmpty()) {
			throw new MozillaProfileNotFoundException();
		}

		// Preparamos CertUtil para hacer los cambios en el almacer
		final File certUtilFile = prepareCertUtil(appDir);

		// Ejecutamos las operaciones solicitada con CertUtil
		try {
			// Usamos CertUtil para instalar el certificado en Firefox.
			executeCertUtil(certUtilFile, uninstall, rootCaFile, profileDirs);

		} catch (final Exception e) {
			throw new KeyStoreException("Error al instalar la CA de confianza en el almacen de Firefox", e); //$NON-NLS-1$
		}
	}


	/** Ejecuta la utilidad Mozilla CertUtil para la instalaci&oacute;n del certificado ra&iacute;z de confianza en Firefox.
	 * @param certUtilFile Fichero ejecutable de <code>certutil</code>.
	 * @param certFile Certificado ra&iacute;z.
	 * @param profilesDir Listado de directorios de perfiles de usuario de Mozilla Firefox.
	 * @throws IOException Cuando ocurre un error en el tratamiento de datos.
	 * @throws KeyStoreException Cuando ocurre un error en la inserci&oacute;n del certificado en el KeyStore. */
	private void executeCertUtil(final File certUtilFile,
			final boolean uninstall, final File certFile,
			final List<File> profilesDir) throws IOException, KeyStoreException {

		boolean error = false;

		// Operamos sobre cada uno de los directorios de perfil de Firefox
		for (final File profileDir : profilesDir) {
			if (!profileDir.isDirectory()) {
				continue;
			}

			this.restorePanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.36", profileDir.getName())); //$NON-NLS-1$
			error = execCommandLineCertUtil(certUtilFile, profileDir, uninstall, certFile);
			if (error) {
				this.restorePanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.37", profileDir.getName())); //$NON-NLS-1$
			}
		}

		if (error) {
			throw new KeyStoreException(
					"Error en la instalacion del certificado de CA en alguno de los perfiles de usuario " //$NON-NLS-1$
					+ "de Firefox. Es posible que la aplicacion funcione en su propio perfil."); //$NON-NLS-1$
		}

	}

	/**
	 * Prepara los comandos de instalacion con certutil para la instalacion del certificado
	 * SSL y los ejecuta.
	 * En MACOSX y Linux, se escribiran scripts intermedios que luego se ejecutaran como comandos.
	 * En Windows se ejecuta certutil directamente como comando.
	 * @param workingDir Directorio en el que se encuentra el subdirectorio de certutil.
	 * @param command Comando a ejecutar, con el nombre de comando y sus par&aacute;metros
	 * separados en un array.
	 * @return <code>true</code> si la ejecuci&oacute;n de CertUtil termin&oacute; con error,
	 * <code>false</code> si se ejecut&oacute; correctamente.
	 * @throws IOException Si no se pudo realizar la propia ejecuci&oacute;n.
	 **/
	private static boolean execCommandLineCertUtil(final File certUtilFile, final File profileDir, final boolean uninstall, final File certFile)
			throws IOException {

		// Escribimos en el script la exportacion los directorios de bibliotecas
		final String certUtilDir = escapePath(certUtilFile.getParent());
		final String exportPath = RestoreConfigMacOSX.EXPORT_PATH + certUtilDir;
		RestoreConfigMacOSX.writeScriptFile(exportPath, false);
		final String exportLibraryPath = RestoreConfigMacOSX.EXPORT_LIBRARY_LD + certUtilDir;
		RestoreConfigMacOSX.writeScriptFile(exportLibraryPath, true);

		// Si nos han pedido desinstalar el certificado actual, introducimos un script para eliminarlo
		if (uninstall) {
			final String command = getRemoveCertCommand(certUtilFile, profileDir);

			System.out.println(command);

			RestoreConfigMacOSX.writeScriptFile(command, true);
		}

		// Escribimos en el script el comando de ejecucion
		if (certFile != null) {

			final String command = getImportCertCommand(certUtilFile, profileDir, certFile);
			RestoreConfigMacOSX.writeScriptFile(command, true);
		}

		// Damos permisos y ejecutamos el script
		final File scriptFile = new File(RestoreConfigMacOSX.mac_script_path);
		try {
			// Ejecutamos sin permisos de administrador asumiendo que solo se podra instalar en los
			// perfiles del usuario creados por el propio usuario. De no hacerlo asi, nos pediria la
			// contrasena de administrador por cada perfil del sistema
			final ShellScript script = new ShellScript(scriptFile, false);
			script.run();
		}
		catch (final Exception e) {
			LOGGER.severe(
					"Error en la instalacion del certificado en el almacen de confianza del perfil " + profileDir.getName() + " de Firefox: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return true;
		}

		return false;
	}


	private static String getRemoveCertCommand(final File certUtilFile, final File profileDir) {

		final String certUtilPath = escapePath(certUtilFile.getAbsolutePath());
		final String profilePath = escapePath(profileDir.getAbsolutePath());
		final String alias = ConfiguratorUtil.CERT_ALIAS;

		// Si en el directorio del perfil existe el fichero pkcs11.txt entonces se trata
		// de un almacen de certificados compartido SQL
		final boolean sqlDb = new File(profileDir, "pkcs11.txt").exists(); //$NON-NLS-1$
		final String profileRef = (sqlDb ? "sql:" : "") + profilePath; //$NON-NLS-1$ //$NON-NLS-2$

		return String.format(
				"max=$(%1$s -L -d %2$s | grep Autofirma | wc -l);" //$NON-NLS-1$
				+ "for ((i=0; i<$max; i++));" //$NON-NLS-1$
				+ "do %1$s -D -d %2$s -n \"%3$s\";" //$NON-NLS-1$
				+ "done", //$NON-NLS-1$
				certUtilPath, profileRef, alias);
	}


	private static String getImportCertCommand(final File certUtilFile, final File profileDir,
			final File certFile) {

		final String certPath = escapePath(certFile.getAbsolutePath());
		final String certUtilPath = escapePath(certUtilFile.getAbsolutePath());
		final String profilePath = escapePath(profileDir.getAbsolutePath());
		final String alias = ConfiguratorUtil.CERT_ALIAS;

		return String.format(
				"%s -A -d sql:%s -i %s -n \"%s\" -t \"C,,\"", //$NON-NLS-1$
				certUtilPath, profilePath, certPath, alias);
	}



	/**
	 * Prepara el ejecutable de CertUtil para su uso.
	 * @param appDir Directorio en el que preparar certUtil.
	 * @return Fichero ejecutable de CertUtil.
	 * @throws IOException Si no se ha podido preparar el ejecutable.
	 */
	private static File prepareCertUtil(final File appDir) throws IOException {

		final File certUtilDir = new File(appDir, DIR_CERTUTIL);
		copyCertUtils(certUtilDir);

		return getCertUtilFile(certUtilDir);
	}

	 /**
	  * Descomprime y copia los ficheros de configuraci&oacute;n de certutil
	  * @param appDir Directorio al que descomprimir las herramientas de configuraci&oacute;n
	  * @throws IOException Cuando ocurre un error al descomprimir o copiar.
	  */
	 private static void copyCertUtils(final File certUtilDir) throws IOException {

		 if (!certUtilDir.exists()) {
			 uncompressResource(RESOURCE_CERTUTIL, certUtilDir.getParentFile());
			 UnixUtils.addExexPermissionsToAllFilesOnDirectory(certUtilDir);
		 }
	 }

	 /**
	  * Descomprime un fichero ZIP de recurso al disco.
	  * @param resource Ruta del recurso ZIP.
	  * @param outDir Directorio local en el que descomprimir.
	  * @throws IOException Cuando ocurre un error al descomprimir.
	  **/
	 private static void uncompressResource(final String resource, final File outDir) throws IOException {
		 int n;
		 ZipEntry entry;
		 final byte[] buffer = new byte[1024];
		 try (final ZipInputStream zipIs = new ZipInputStream(
				 RestoreConfigFirefoxMacOS.class.getResourceAsStream(resource));) {

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

	 /** Obtiene el path para la llamada a CertUtil.
	  * @param appDir Ruta en la que buscar el ejecutable CertUtil.
	  * @return Referencia a CertUtil.
	  * @throws IOException Se lanza cuando hay un problema con el fichero CertUtil. */
	 private static File getCertUtilFile(final File certUtilDir) throws IOException {

		 final File certutilFile = new File(certUtilDir, CERTUTIL_EXE);

		 if (!certutilFile.isFile()) {
			 throw new IOException("No se encuentra el ejecutable CertUtil para la instalacion en Firefox"); //$NON-NLS-1$
		 }

		 if (!certutilFile.canExecute()) {
			 UnixUtils.addExexPermissionsToAllFilesOnDirectory(certutilFile.getParentFile());
		 }

		 if (!certutilFile.canExecute()) {
			 throw new IOException("No hay permisos de ejecucion para Mozilla CertUtil"); //$NON-NLS-1$
		 }

		 return certutilFile;
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
	 * Devuelve el listado de directorios de perfil de Firefox se encuentra el fichero <i>profiles.ini</i>.
	 * @return Listado de directorios de perfil de Firefox.
	 */
	private List<File> getMozillaProfiles() {

		if (this.profilesDirs == null) {
			final String pathProfile = MACOSX_MOZILLA_PATH;

			this.profilesDirs = new ArrayList<>();
			for (final File usr : this.usersDirs){
				final File profilesIniFile = new File(usr, pathProfile);
				if (profilesIniFile.isFile()){
					this.profilesDirs.addAll(loadProfiles(profilesIniFile));
				}
			}
		}
		return this.profilesDirs;
	}

	/**
	 * Devuelve el listado de directorios de perfil de Firefox registrado en un fichero "profiles.ini".
	 * @param profilesIniFile Fichero de perfiles de Firefox.
	 * @return Listado de directorios donde se encuentran los perfiles de usuario de Firefox. */
	private static List<File> loadProfiles(final File profilesIniFile){

		final List<File> profile = new ArrayList<>();
		try (
				final InputStream resIs = new FileInputStream(profilesIniFile);
				final BufferedReader resReader = new BoundedBufferedReader(
						new InputStreamReader(resIs),
						256, // Maximo 256 lineas de salida (256 perfiles por "profiles.ini")
						2048 // Maximo 2048 caracteres por linea
						);
				) {
			String line;
			while ((line = resReader.readLine()) != null) {
				if (line.startsWith(PROFILES_INI_PATH_PREFIX)) {
					final File file = new File(profilesIniFile.getParentFile(), line.substring(PROFILES_INI_PATH_PREFIX.length()));
					if (file.isDirectory()){
						profile.add(file);
					}
				}
			}
		}
		catch (final Exception e) {
			LOGGER.severe("Error al buscar los directorios de perfiles de Firefox: " + e); //$NON-NLS-1$
		}
		return profile;
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
	public void configureUseSystemTrustStore(final boolean enable) throws IOException, MozillaProfileNotFoundException {

		// Si no hubiese directorios de perfil de Firefox es que no estaria instalado
		final List <File> mozillaProfileDirs = getMozillaProfiles();
		if (mozillaProfileDirs.isEmpty()) {
			throw new MozillaProfileNotFoundException();
		}

		// Las preferencias personalizadas se establecen a traves de un fichero user.js en el
		// directorio de perfil de Firefox. Por cada directorio, comprobamos si existe este
		// fichero. Si no existe, se crea con la propiedad personalizada. Si existe, se modifica
		// el valor que tuviese, o se agrega la propiedad si no estuviera.
		for (final File profileDir : mozillaProfileDirs) {

			this.restorePanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.38", profileDir.getName())); //$NON-NLS-1$
			try {
				configureUseSystemTrustStore(profileDir, enable);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se pudo editar la confianza de Firefox para el perfil: " + LoggerUtil.getCleanUserHomePath(profileDir.getName()), e); //$NON-NLS-1$
				this.restorePanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.39", profileDir.getName())); //$NON-NLS-1$
			}
		}
	}

	private static void configureUseSystemTrustStore(final File profileDir, final boolean enable) throws IOException {
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
	private static void editFile(final File file, final byte[] content) throws IOException {

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

	private static void makeFileMine(final File file) {

		try {
			final String user = getUser();
			if (user != null) {
				final String filePath = escapePath(file.getAbsolutePath());
				final String command = String.format("chown -f %s %s", user, filePath); //$NON-NLS-1$
				RestoreConfigMacOSX.writeScriptFile(command, false);
				final File scriptFile = new File(RestoreConfigMacOSX.mac_script_path);
				final ShellScript script = new ShellScript(scriptFile, false);
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
