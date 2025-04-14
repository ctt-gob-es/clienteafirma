/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.restoreconfig;

import java.awt.Component;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;
import es.gob.afirma.standalone.so.macos.ShellScript;
import es.gob.afirma.standalone.so.macos.UnixUtils;
import es.gob.afirma.standalone.ui.restoreconfig.CertUtil.CertPack;

/**
 * Clase que contiene la l&oacute;gica para realizar las tareas de restauraci&oacute;n
 * de la configuraci&oacute;n de navegadores para el sistema operativo MacOsX.
 */
final class RestoreConfigMacOSX implements RestoreConfig {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String KS_FILENAME = "/autofirma.pfx"; //$NON-NLS-1$
	private static final String SSL_CA_CER_FILENAME = "/Autofirma_ROOT.cer";//$NON-NLS-1$
	private static final String SSL_CER_FILENAME = "/autofirma.cer"; //$NON-NLS-1$
	private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$
	private static final String CERT_CN = "127.0.0.1"; //$NON-NLS-1$
	private static final String CERT_CN_ROOT = "Autofirma ROOT"; //$NON-NLS-1$

	private static final String FIND_CERT_HASH_PREFIX = "hash:";  //$NON-NLS-1$

	private static final String MAC_SCRIPT_NAME = "/installCerScript"; //$NON-NLS-1$
	private static final String MAC_SCRIPT_EXT = ".sh"; //$NON-NLS-1$
	static final String EXPORT_PATH = "export PATH=$PATH:";//$NON-NLS-1$
	static final String EXPORT_LIBRARY_LD = "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:";//$NON-NLS-1$

	private static final byte[] DUMMY = "dummy".getBytes(); //$NON-NLS-1$

	private static final String CHANGE_OWN_COMMAND = "chown %USERNAME% \"%DIR%\""; //$NON-NLS-1$


	static String mac_script_path;

	private static List<File> userDirs = null;

	@Override
	public void restore(final RestoreConfigPanel configPanel) {

		// Restauramos solo el pergil actual por problemas de permisos para el resto de usuarios
		userDirs = Arrays.asList(getUserHome());

		// Comprobamos si se debe configurar Firefox para que use el almacen de confianza del sistema
		final boolean firefoxSecurityRoots = configPanel.firefoxIntegrationCb.isSelected();

		// Tomamos como directorio de aplicacion aquel en el que podemos generar
		// los certificados SSL para despues usarlos
		final File appDir = DesktopUtil.getMacOsXAlternativeAppDir();
		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.3", appDir.getAbsolutePath())); //$NON-NLS-1$

		// Nos aseguramos de que el directorio de instalacion tiene permisos para escribir en el directorio de instalacion
		// y establece como directorio de trabajo otro distinto en caso de no tenerlos
		if (!appDir.isDirectory()) {

			// Creamos el directorio con permisos para despues poder crear subdirectorios desde la propia aplicacion
			try {
				final Set<PosixFilePermission> ownerWritable = PosixFilePermissions.fromString("rwxrwxr-x"); //$NON-NLS-1$
				final FileAttribute<?> permissions = PosixFilePermissions.asFileAttribute(ownerWritable);
				Files.createDirectories(appDir.toPath(), permissions);
			}
			catch (final Exception e) {
				appDir.mkdirs();
			}

			// Si no se puede crear el directorio, se aborta la operacion
			if (!appDir.isDirectory()) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.1")); //$NON-NLS-1$
				LOGGER.severe("No se puede crear el directorio '" + LoggerUtil.getCleanUserHomePath(appDir.getAbsolutePath()) + "' se aborta la restauracion"); //$NON-NLS-1$ //$NON-NLS-2$
				return;
			}
		}

		// Preparamos el script de ejecucion para las operaciones
		try {
			prepareScript();
		}
		catch (final Exception e) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.15")); //$NON-NLS-1$
			LOGGER.log(Level.SEVERE, "Error creando script temporal. Se aborta la operacion", e); //$NON-NLS-1$
			return;
		}

		// Cambiamos el propietario del directorio de la aplicacion para que sea el usuario. Sino, este directorio
		// perteneceria si no al administrador y despues el usuario no tendria permisos de escritura sobre el
		changeDirectoryProperty(appDir);

		// Preparamos los certificados SSL que se deben restaurar
		final CertFilesInfo certFiles = prepareSslCertificates(appDir, configPanel);

		// Restauramos los certificados SSL en el almacen del sistema
		if (certFiles != null) {
			restoreSslCertificatesInKeyChain(appDir, certFiles, configPanel);
		}

		// Identicamos si hay perfiles de Firefox en el sistema y los configuramos en tal caso
		final RestoreConfigFirefoxMacOS firefoxRestorer = new RestoreConfigFirefoxMacOS(userDirs, configPanel);
		if (firefoxRestorer.hasProfiles()) {

			// Configuramos o desconfiguramos la confianza de Firefox en el almacen del sistema
			configureFirefoxTrustStore(firefoxRestorer, firefoxSecurityRoots, configPanel);

			// Restauramos los certificados SSL en el almacen de Firefox
			if (certFiles != null) {
				restoreSslCertificatesInFirefox(firefoxRestorer, appDir, certFiles, configPanel);
			}
		}

		// Eliminamos el script de ejecucion para las operaciones
		try {
			removeScript();
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo eliminar el script temporal" + e); //$NON-NLS-1$
			return;
		}
	}

	/**
	 * Instala en el almac&eacute;n de confianza de Firefox el certificado de la CA con la que se gener&oacute; el certificado SSL.
	 * @param firefoxRestorer Restaurador de Firefox.
	 * @param appDir Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
	 * @param certFiles Referencias a los certificados SSL de la aplicaci&oacute;n.
	 * @param configPanel Panel de restauraci&oacute;n de la aplicaci&oacute;n.
	 */
	private static void restoreSslCertificatesInFirefox(final RestoreConfigFirefoxMacOS firefoxRestorer, final File appDir,
			final CertFilesInfo certFiles, final RestoreConfigPanel configPanel) {

		// Obligamos a que se cierre Firefox antes de manipular el certificado en su almacen
		final boolean closed = closeFirefox(configPanel);

		// Si no se ha cerrado el navegador, es muy probable que no se pueda instalar el certificado de confianza,
		// asi que mostramos un mensaje advirtiendolo
		if (!closed) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.45")); //$NON-NLS-1$
		}


		// Se reinstala el certificado raiz en el almacen de Firefox
		try {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.13")); //$NON-NLS-1$

			// Instalar el certificado en Mozilla
			firefoxRestorer.reinstallRootCAMozillaKeyStore(
				appDir,
				certFiles.getSslRootFile()
			);
		}
		catch (final MozillaProfileNotFoundException e) {
			LOGGER.log(Level.WARNING, "No se ha encontrado el perfil de Mozilla en macOS", e); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.12")); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.log(Level.WARNING, "No se pudo configurar certutil para la instalacion en el almacen", e); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.35")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error al instalar los certificados en el almacen de confianza de Firefox", e); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.19")); //$NON-NLS-1$
		}

	}



	/**
	 * Prepara el fichero de Script con el que se ejecutaran las distintas operaciones
	 * de restauraci&oacute;n.
	 * @throws IOException Cuando no se pudo preparar el script.
	 */
	private static void prepareScript() throws IOException {

		// Generamos un fichero que utilizaremos para guardar y ejecutar un script
		mac_script_path = File.createTempFile(MAC_SCRIPT_NAME, MAC_SCRIPT_EXT).getAbsolutePath();


		// Ejecutamos el script de inmediato porque necesitamos estos permisos para seguir. Despues se ejecutara
		// de nuevo con el resto de comandos
		UnixUtils.addAllPermissionsToFile(new File(mac_script_path));
	}

	/**
	 * Elimina el fichero de Script con el que se ejecutaron las distintas operaciones
	 * de restauraci&oacute;n.
	 * @throws IOException Cuando no se pudo eliminar el script.
	 */
	private static void removeScript() throws IOException {
		Files.delete(new File(mac_script_path).toPath());
	}

	/**
	 * Configura Firefox para que conf&iacute;e en el almac&eacute;n de confianza del sistema.
	 * @param firefoxRestorer Restaurador de Firefox.
	 * @param firefoxSecurityRoots Indicador de si Firefox debe estar configurado para confiar
	 * o no en los certificados del almac&eacute;n de confianza del sistema.
	 * @param configPanel Panel de configuraci&oacute;n.
	 */
	private static void configureFirefoxTrustStore(
			final RestoreConfigFirefoxMacOS firefoxRestorer,
			final boolean firefoxSecurityRoots,
			final RestoreConfigPanel configPanel) {

		configPanel.appendMessage(SimpleAfirmaMessages.getString(
				firefoxSecurityRoots ? "RestoreConfigMacOSX.21" : "RestoreConfigMacOSX.22")); //$NON-NLS-1$ //$NON-NLS-2$

		try {
			firefoxRestorer.configureUseSystemTrustStore(firefoxSecurityRoots);
		}
		catch (final MozillaProfileNotFoundException e) {
			LOGGER.info("No se encontraron perfiles de Firefox en los que configurar la confianza en el almacen del sistema"); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.24")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error configurando la confianza de Firefox en el almacen del sistema (activando: " + firefoxSecurityRoots + ")", e); //$NON-NLS-1$ //$NON-NLS-2$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.23")); //$NON-NLS-1$
		}
	}

	/**
	 * Escribe en el script de ejecuci&oacute;n el comando para el cambio de propiedad
	 * de un fichero/directorio a un usuario.
	 * @param file Fichero del que cambiar la propiedad.
	 */
	private static void changeDirectoryProperty(final File file) {

		final String username = System.getenv("USER"); //$NON-NLS-1$
		final String cmd = CHANGE_OWN_COMMAND
				.replace("%DIR%", file.getAbsolutePath()) //$NON-NLS-1$
				.replace("%USERNAME%", username); //$NON-NLS-1$
		try {
			writeScriptFile(mac_script_path, new StringBuilder(cmd), true);
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se ha podido agregar al script el comando para el cambio de propiedad de: " + LoggerUtil.getCleanUserHomePath(file.getAbsolutePath()), e); //$NON-NLS-1$
		}

		try {
			executeScript(mac_script_path, true, false);
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se ha podido cambiar la propiedad del directorio de Autofirma", e); //$NON-NLS-1$
		}
	}

	private static CertFilesInfo prepareSslCertificates(final File appDir, final RestoreConfigPanel configPanel) {

		// Identifico cuales son los ficheros que debe haber en el directorio
		final CertFilesInfo certFiles = buildCertFiles(appDir);

		// Si no existe el almacen con la clave SSL, se genera junto con el resto de ficheros
		if (!certFiles.getSslKeyStoreFile().exists()) {

			try {
				generateSslCerts(appDir, certFiles, configPanel);
			}
			catch (final IOException e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.7")); //$NON-NLS-1$
				LOGGER.log(Level.WARNING, "Error al copiar a disco los certificados SSL. Se aborta la operacion", e); //$NON-NLS-1$
				return null;
			}
			catch (final GeneralSecurityException e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.9")); //$NON-NLS-1$
				LOGGER.log(Level.WARNING, "Error al generar los certificados SSL. Se aborta la operacion", e); //$NON-NLS-1$
				return null;
			}
		}
		// Si existe el almacen, extraemos de el el certificado SSL y la raiz, pisando cualquier otro
		// que hubiese si se diese el caso
		else {
			try {
				exportSslCerts(certFiles);
			}
			catch (final Exception e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.4")); //$NON-NLS-1$
				LOGGER.log(Level.WARNING, "Error al copiar los certificados SSL a disco. Se aborta la operacion", e); //$NON-NLS-1$
				return null;
			}
		}

		return certFiles;
	}

	private static CertFilesInfo buildCertFiles(final File appDir) {
		final CertFilesInfo certFiles = new CertFilesInfo();
		certFiles.setSslRootFile(new File(appDir, SSL_CA_CER_FILENAME));
		certFiles.setSslCertFile(new File(appDir, SSL_CER_FILENAME));
		certFiles.setSslKeyStoreFile(new File(appDir, KS_FILENAME));
		return certFiles;
	}



	/**
	 * Restaura la configuraci&oacute;n de los certificados SSL para la comunicaci&oacute;n
	 * por <i>sockets</i> en el llavero del sistema.
	 * @param appDir Directorio en donde se crearan los ficheros necesarios para
	 *                   llevar a cabo la restauraci&oacute;n.
	 * @param certFiles Referencias a los certificados SSL para la importaci&oacute;n.
	 * @param configPanel Panel de configuraci&oacute;n sobre el que se ir&aacute;n
	 *                    imprimiendo los mensajes con el estado de la operaci&oacute;n.
	 */
	private static void restoreSslCertificatesInKeyChain(final File appDir, final CertFilesInfo certFiles, final RestoreConfigPanel configPanel) {

		// Obtenemos del usuario y probamos la contrasena del Llavero
		byte[] keyChainPhrase = getKeyChainPhrase(configPanel, false);

		// Desinstalamos del llavero los certificados anteriores
		LOGGER.info("Desinstalacion de versiones anteriores del certificado raiz del Llavero del sistema"); //$NON-NLS-1$

		boolean passwordValidated;
		do {
			passwordValidated = true;
			try {
				uninstallRootCAMacOSXKeyStore(keyChainPhrase);
			}
			catch (final InvalidPasswordException e) {
				keyChainPhrase = getKeyChainPhrase(configPanel, true);
				passwordValidated = false;
			}
		} while (!passwordValidated);

		// Se instalan los certificados en el llavero del sistema operativo
		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.6")); //$NON-NLS-1$
		try {
			installTrustedCertsInAppleKeyChain(certFiles.getSslRootFile(), certFiles.getSslCertFile(), keyChainPhrase, configPanel);
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error en la importacion del certificado de confianza en el llavero del sistema operativo", e); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.20")); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
					SimpleAfirmaMessages.getString(
							"RestoreConfigMacOSX.27", //$NON-NLS-1$
							certFiles.getSslRootFile().getAbsolutePath(),
							certFiles.getSslCertFile().getAbsolutePath()),
					SimpleAfirmaMessages.getString("RestoreConfigMacOSX.28"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e);
		}
	}

	/**
	 * Obtiene la contrase&ntilde;a del llavero y lo desbloquea para comprobarla.
	 * @param console Consola en la que mostrar los mensajes al usuario.
	 * @param force Indica si se debe forzar que se introduzca una contrase&ntilde;a.
	 * @return Contrase&ntilde;a del llavero.
	 */
	private static byte[] getKeyChainPhrase(final RestoreConfigPanel console, final boolean force) {

		byte[] phrase = null;
		boolean passwordError = false;
		boolean keyChainUnlocked = false;
		do {

			// La contrasena solo hace falta si no estamos ya en modo administrador, asi que la primera vez probaremos con una generica
			if (!force && phrase == null) {
				phrase = DUMMY;
			}
			else {
				// Solicitamos la contrasena para la instalacion de los certificados
				final String text = passwordError
						? SimpleAfirmaMessages.getString("RestoreConfigMacOSX.30") //$NON-NLS-1$
						: SimpleAfirmaMessages.getString("RestoreConfigMacOSX.29"); //$NON-NLS-1$

				try {
					// Se pone en una linea para evitar que la contrasena se exponda en claro en memoria
					phrase = new String(AOUIFactory.getPassword(text, console)).getBytes(StandardCharsets.UTF_8);
				}
				catch (final AOCancelledOperationException e) {
					LOGGER.info("Se cancelo el dialogo de entrada de contrasena: " + e); //$NON-NLS-1$
					final int option = AOUIFactory.showConfirmDialog(console,
							SimpleAfirmaMessages.getString("RestoreConfigMacOSX.31"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("RestoreConfigMacOSX.32"), //$NON-NLS-1$
							JOptionPane.YES_NO_OPTION,
							JOptionPane.WARNING_MESSAGE);
					if (option == JOptionPane.YES_OPTION) {
						console.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.33")); //$NON-NLS-1$
						throw new AOCancelledOperationException("El usuario cancelo la insercion de la contrasena del llavero"); //$NON-NLS-1$
					}
					continue;
				}
			}

			// Restablecemos el valor
			passwordError = false;

			// Insertamos el certificado raiz
			try {
				unlockAppleKeyChain(phrase);
			}
			catch (final SecurityException e) {

				LOGGER.log(Level.WARNING, "Error de contrasena al desbloquear el Llavero", e); //$NON-NLS-1$
				// La contrasena invalida, pero si era el intento de prueba, no lo tendremos en cuenta
				// y volveremos a empezar la operacion, esta vez pidiendo la contrasena
				if (!Arrays.equals(DUMMY, phrase)) {
					passwordError = true;
				}
				continue;
			}
			catch (final Exception e) {
				LOGGER.log(Level.SEVERE, "No se pudo abrir el Llavero", e); //$NON-NLS-1$
				return DUMMY;
			}

			keyChainUnlocked = true;

		} while(!keyChainUnlocked);

		return phrase;
	}

	/**
	 * Ejecuta el comando para desbloquear el almacen de claves, con el cual ya confirmamos
	 * si la contrase&ntilde;a proporcionada es la correcta.
	 * @param phrase Contrase&ntilde;a del almac&eacute;n.
	 * @throws SecurityException Cuando la contrase&ntilde;a es incorrecta.
	 * @throws IOException, InterruptedException Cuando falla el desbloqueo del almac&eacute;n.
	 */
	private static void unlockAppleKeyChain(final byte[] phrase)
			throws SecurityException, IOException, InterruptedException {
		final List<String> params = new ArrayList<>();
		params.add("security"); //$NON-NLS-1$
		params.add("unlock-keychain"); //$NON-NLS-1$
		params.add("-p"); //$NON-NLS-1$
		params.add(new String(phrase, StandardCharsets.UTF_8));

		final ProcessBuilder builder = new ProcessBuilder(params);
		final Process process = builder.start();

		final int exitValue = process.waitFor();
		if (exitValue != 0) {
			byte[] errorOutput = null;
			try (final InputStream errorStream = process.getErrorStream()) {
				errorOutput = AOUtil.getDataFromInputStream(errorStream);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se pudo leer la salida de error " //$NON-NLS-1$
						+ "del proceso de desbloqueo del Llavero", e); //$NON-NLS-1$
			}
			if (errorOutput != null) {
				String errorMsg = new String(errorOutput);
				// El texto de solicitud de contrasena inicial puede haberse agregado a la salida de error,
				// asi que lo omitimos
				if (errorMsg.startsWith("Password:")) { //$NON-NLS-1$
					errorMsg = errorMsg.substring("Password:".length()); //$NON-NLS-1$
				}
				LOGGER.severe("Salida de error: " + errorMsg); //$NON-NLS-1$
				if (errorMsg.toLowerCase().contains("password")) { //$NON-NLS-1$
					throw new SecurityException("Contrasena incorrecta"); //$NON-NLS-1$
				}
				throw new IOException("Error al desbloquear el llavero"); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Genera y copia a disco los certificados SSL para la comunicaci&oacute;n con la aplicaci&oacute;n.
	 * @param workingDir Directorio en el que almacenar los certificados de la aplicaci&oacute;n.
	 * @param certFiles Ficheros de certificados
	 * @param configPanel Panel de configuraci&oacute;n con las trazas de ejecuci&oacute;n.
	 * @throws IOException Cuando ocurre un error en el proceso de instalaci&oacute;n.
	 * @throws GeneralSecurityException Cuando ocurre un error al generar el certificado SSL.
	 */
	private static void generateSslCerts(final File workingDir, final CertFilesInfo certFiles, final RestoreConfigPanel configPanel)
			throws IOException, GeneralSecurityException {

		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.5")); //$NON-NLS-1$

		// Generamos los certificados de CA y SSL
		final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(
				ConfiguratorUtil.CERT_ALIAS,
				KS_PASSWORD);

		// Eliminamos anteriores copias de los ficheros y los volvemos a generar
		deleteFile(certFiles.getSslRootFile());
		RestoreConfigUtil.installFile(
        		certPack.getCaCertificate().getEncoded(),
        		certFiles.getSslRootFile());

		deleteFile(certFiles.getSslKeyStoreFile());
		RestoreConfigUtil.installFile(
			certPack.getPkcs12(),
			certFiles.getSslKeyStoreFile());

		deleteFile(certFiles.getSslCertFile());
		RestoreConfigUtil.installFile(
				certPack.getSslCertificate().getEncoded(),
				certFiles.getSslCertFile());
	}

	/**
	 * Extrae del almac&eacute;n de claves SSL los certificados SSL y el de su CA.
	 * @param certFiles Ficheros de certificados
	 * @throws IOException Cuando ocurre un error en el proceso de instalaci&oacute;n.
	 * @throws GeneralSecurityException Cuando ocurre un error al cargar el almacen o codificar sus certificados.
	 */
	private static void exportSslCerts(final CertFilesInfo certFiles) throws IOException, GeneralSecurityException {
		final Certificate[] sslCertChain;
		try (FileInputStream fis = new FileInputStream(certFiles.getSslKeyStoreFile())) {
			final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
			ks.load(fis, KS_PASSWORD.toCharArray());
			sslCertChain = ks.getCertificateChain(ks.aliases().nextElement());
		}

		// Guardamos el certificado raiz
		deleteFile(certFiles.getSslRootFile());
		RestoreConfigUtil.installFile(
			sslCertChain[sslCertChain.length - 1].getEncoded(),
			certFiles.getSslRootFile()
		);

		// Guardamos el certificado ssl
		deleteFile(certFiles.getSslCertFile());
		RestoreConfigUtil.installFile(
			sslCertChain[0].getEncoded(),
			certFiles.getSslCertFile()
		);
	}

	/**
	 * Devuelve el directorio del usuario.
	 * @return Directorio del usuario.
	 */
	private static File getUserHome() {
		final String userDir = System.getenv("HOME"); //$NON-NLS-1$
		return new File(userDir);
	}

	/**
	 * Instala en el Llavero de macOS los certificados en los que debe confiar el sistema
	 * para permitir la comunicacion SSL con la aplicaci&oacute;n.
	 * @param caCertFile Certificado ra&iacute;z con el que se genera el certificado SSL.
	 * @param sslCertFile Certificado SSL.
	 * @param keyChainPhrase Contrase&ntilde;a de acceso al Llavero.
	 * @param console Consola en la que mostrar los mensajes al usuario.
	 * @throws IOException Cuando ocurre un error al leer el fichero
	 * @throws InterruptedException Si se interrumpe el proceso de instalaci&oacute;n.
	 * @throws KeyChainException Cuando ocurra un error al insertar el certificado en el almac&eacute;n.
	 * @throws SecurityException Cuando la contrase&ntilde;a introducida de administraci&oacute;n no sea correcta.
	 */
	private static void installTrustedCertsInAppleKeyChain(final File caCertFile,
			final File sslCertFile, final byte[] keyChainPhrase, final RestoreConfigPanel console)
					throws IOException, InterruptedException, KeyChainException, SecurityException {

		// Insertamos el certificado raiz
		installTrustedCertInAppleKeyChain(caCertFile, keyChainPhrase, true);
		console.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.25")); //$NON-NLS-1$

		// Insertamos el certificado SSL
		installTrustedCertInAppleKeyChain(sslCertFile, keyChainPhrase, false);
		console.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.26")); //$NON-NLS-1$
	}

	/**
	 * Instala un certificado en el almacen de certificados de confianza del llavero de macOS.
	 * @param sslCertFile Fichero del certificado SSL.
	 * @param phrase Contrase&ntilde;a del llavero/administrador.
	 * @param isRootCa Indica si el certificado debe instalarse como CA.
	 * @throws IOException Cuando ocurre un error al leer el fichero
	 * @throws InterruptedException Si se interrumpe el proceso de instalaci&oacute;n.
	 * @throws KeyChainException Cuando ocurra un error al insertar el certificado en el almac&eacute;n.
	 * @throws SecurityException Cuando la contrase&ntilde;a introducida de administraci&oacute;n no sea correcta.
	 */
	private static void installTrustedCertInAppleKeyChain(final File sslCertFile, final byte[] phrase, final boolean isRootCa)
			throws IOException, InterruptedException, KeyChainException, SecurityException {

		final List<String> params = new ArrayList<>();
		params.add("sudo"); //$NON-NLS-1$
		params.add("-S"); //$NON-NLS-1$
		params.add("security"); //$NON-NLS-1$
		params.add("-i"); //$NON-NLS-1$
		params.add("add-trusted-cert"); //$NON-NLS-1$
		params.add("-d"); //$NON-NLS-1$
		params.add("-r"); //$NON-NLS-1$
		params.add(isRootCa ? "trustRoot" : "trustAsRoot"); //$NON-NLS-1$ //$NON-NLS-2$
		params.add("-k"); //$NON-NLS-1$
		params.add("/Library/Keychains/System.keychain"); //$NON-NLS-1$
		params.add(sslCertFile.getAbsolutePath());

		final ProcessBuilder builder = new ProcessBuilder(params);
		final Process process = builder.start();

		// Se proporciona la contrasena de administrador
		try (OutputStream os = process.getOutputStream()) {
			os.write(phrase);
			os.flush();
		}

		final int exitValue = process.waitFor();
		if (exitValue != 0) {
			byte[] errorOutput = null;
			try (final InputStream errorStream = process.getErrorStream()) {
				errorOutput = AOUtil.getDataFromInputStream(errorStream);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se pudo leer la salida de error " //$NON-NLS-1$
						+ "del proceso de instalacion del certificado en el llavero", e); //$NON-NLS-1$
			}
			if (errorOutput != null) {
				final String errorMsg = new String(errorOutput);
				LOGGER.severe("Salida de error: " + errorMsg); //$NON-NLS-1$
				if (errorMsg.toLowerCase().contains("password")) { //$NON-NLS-1$
					throw new SecurityException("Contrasena incorrecta"); //$NON-NLS-1$
				}
				throw new KeyChainException("Error al instalar el certificado " + sslCertFile //$NON-NLS-1$
						+ " en el llavero de macOS"); //$NON-NLS-1$
			}
		}
	}

	/** Ejecuta un script en OS X.
	 * @param path Ruta donde se encuentra el <i>script</i>.
	 * @param administratorMode <code>true</code> el <i>script</i> se ejecuta como permisos de adminsitrador, <code>false</code> en caso contrario.
	 * @param delete <code>true</code> se borra el fichero despu&eacute;s de haberse ejecutado.
	 * @return El objeto que da como resultado el <i>script</i>.
	 * @throws IOException Excepci&oacute;n lanzada en caso de ocurrir alg&uacute;n error en la ejecuci&oacute;n del <i>script</i>.
	 * @throws InterruptedException Cuando se interrumpe la ejecuci&oacute;n del script. */
	public static Object executeScript(final String path, final boolean administratorMode, final boolean delete) throws IOException, InterruptedException {

		LOGGER.info("Se ejecuta el fichero: " + LoggerUtil.getCleanUserHomePath(path)); //$NON-NLS-1$

		String result;
		final ShellScript appleScript = new ShellScript(new File(path), delete);
		if (administratorMode) {
			result = appleScript.runAsAdministrator();
		}
		else {
			result = appleScript.run();
		}

		return result;
	}

	/**
	 * Desinstala del Llavero de macOS los certificados de confianza de la aplicaci&oacute;n.
	 * @param keyChainPhrase Contrase&ntilde;a del Llavero.
	 * @throws InvalidPasswordException Cuando la contrase&ntilde;a introducida de administraci&oacute;n no sea correcta.
	 */
	private static void uninstallRootCAMacOSXKeyStore(final byte[] keyChainPhrase) throws InvalidPasswordException {

		LOGGER.info("Desinstalamos los anteriores certificados del almacen"); //$NON-NLS-1$

		// Eliminamos el certificados SSL
		try {
			uninstallTrustedCertFromAppleKeyChain(CERT_CN, keyChainPhrase);
		}
		catch (final InvalidPasswordException e) {
			throw e;
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo eliminar el anterior certificado " +  CERT_CN + " del Llavero", e); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// Eliminamos el certificado raiz
		try {
			uninstallTrustedCertFromAppleKeyChain(CERT_CN_ROOT, keyChainPhrase);
		}
		catch (final InvalidPasswordException e) {
			throw e;
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo eliminar el anterior certificado " +  CERT_CN_ROOT + " del Llavero", e); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/**
	 * Desinstala todos los certificados con el CN indicado del Llavero de macOS.
	 * @param commonName Nombre com&uacute;n del certificado a eliminar.
	 * @param phrase Contrase&ntilde;a del llavero/administrador.
	 * @throws IOException Cuando ocurre un error al leer el fichero
	 * @throws InterruptedException Si se interrumpe el proceso de instalaci&oacute;n.
	 * @throws KeyChainException Cuando ocurra un error al insertar el certificado en el almac&eacute;n.
	 * @throws SecurityException Cuando no se pueda identificar el resultado de la operaci&oacute;n.
	 * @throws InvalidPasswordException Cuando la contrase&ntilde;a introducida de administraci&oacute;n no sea correcta.
	 */
	private static void uninstallTrustedCertFromAppleKeyChain(final String commonName, final byte[] phrase)
			throws IOException, InterruptedException, KeyChainException, SecurityException, InvalidPasswordException {

		// El comando para la eliminacion de certificados exige que se identifique univocamente
		// el certificado a eliminar. Como puede que haya mas de un certificado con el mismo CN,
		// tendremos que buscar cada uno de ellos, sacar su hash y usar este hash en el comando
		// de eliminacion de certificados.

		boolean certFound = false;
		do {
			final List<String> params = new ArrayList<>();
			params.add("sudo"); //$NON-NLS-1$
			params.add("-S"); //$NON-NLS-1$
			params.add("security"); //$NON-NLS-1$
			params.add("find-certificate"); //$NON-NLS-1$
			params.add("-c"); //$NON-NLS-1$
			params.add(commonName);
			params.add("-Z"); //$NON-NLS-1$

			final ProcessBuilder builder = new ProcessBuilder(params);
			final Process process = builder.start();

			// Se proporciona la contrasena de administrador
			try (OutputStream os = process.getOutputStream()) {
				os.write(phrase);
				os.flush();
			}

			// Vamos a buscar en la salida del comando el hash del certificado para usarlo para su eliminacion
			String hash = null;

			// Esperamos a la salida del comando
			final int exitValue = process.waitFor();
			if (exitValue == 0) {
				// Se ha encontrado un certificado con el CN indicado
				certFound = true;

				byte[] output = null;
				try (final InputStream outStream = process.getInputStream()) {
					output = AOUtil.getDataFromInputStream(outStream);
				}
				catch (final Exception e) {
					throw new SecurityException("Error al leer la salida del comando de busqueda de certificados en el Llavero", e); //$NON-NLS-1$
				}
				// Leemos de la salida el hash del certificado. Este se encuentra despues de la
				// cadena "hash:" hasta el final de la linea
				if (output != null) {
					final String msg = new String(output);
					final int startHashIdx = msg.indexOf(FIND_CERT_HASH_PREFIX);
					if (startHashIdx > -1) {
						final int endLineIdx = msg.indexOf("\n", startHashIdx); //$NON-NLS-1$
						if (endLineIdx > -1) {
							hash = msg.substring(startHashIdx + FIND_CERT_HASH_PREFIX.length(), endLineIdx).trim();
						}
					}
				}
			}
			else {
				// No se ha encontrado un certificado con el CN indicado
				certFound = false;

				byte[] errorOutput = null;
				try (final InputStream errorStream = process.getErrorStream()) {
					errorOutput = AOUtil.getDataFromInputStream(errorStream);
				}
				catch (final Exception e) {
					LOGGER.log(Level.WARNING, "No se pudo leer la salida de error " //$NON-NLS-1$
							+ "del proceso de desinstalacion del certificado en el llavero", e); //$NON-NLS-1$
				}
				if (errorOutput != null) {
					String errorMsg = new String(errorOutput);

					// No se encontraron mas instanceias del certificado en el almacen
					if (errorMsg.contains("SecKeychainSearchCopyNext")) { //$NON-NLS-1$
						certFound = false;
					}
					else {
						// El texto de solicitud de contrasena inicial puede haberse agregado a la salida de error,
						// asi que lo omitimos
						if (errorMsg.startsWith("Password:")) { //$NON-NLS-1$
							errorMsg = errorMsg.substring("Password:".length()); //$NON-NLS-1$
						}
						if (errorMsg.toLowerCase().contains("password")) { //$NON-NLS-1$
							throw new InvalidPasswordException();
						}
						LOGGER.severe("Salida de error: " + errorMsg); //$NON-NLS-1$
					}
				}
			}

			// Eliminamos el certificado con el hash encontrado
			if (certFound && hash != null) {
				uninstallTrustedCertFromAppleKeyChainByHash(hash);
			}


		} while (certFound);
	}

	/**
	 * Elimina un certificado del llavero de macOS identific&aacute;ndolo por medio de su hash.
	 * @param hash Hash del certificado a eliminar.
	 * @throws IOException Cuando ocurre un error al leer el fichero
	 * @throws InterruptedException Si se interrumpe el proceso de instalaci&oacute;n.
	 * @throws KeyChainException Cuando ocurra un error al insertar el certificado en el almac&eacute;n.
	 * @throws SecurityException Cuando la contrase&ntilde;a introducida de administraci&oacute;n no sea correcta.
	 */
	private static void uninstallTrustedCertFromAppleKeyChainByHash(final String hash)
			throws IOException, InterruptedException, KeyChainException, SecurityException {

		final List<String> params = new ArrayList<>();
		params.add("sudo"); //$NON-NLS-1$
		params.add("-S"); //$NON-NLS-1$
		params.add("security"); //$NON-NLS-1$
		params.add("delete-certificate"); //$NON-NLS-1$
		params.add("-Z"); //$NON-NLS-1$
		params.add(hash);
		params.add("-t"); //$NON-NLS-1$

		final ProcessBuilder builder = new ProcessBuilder(params);
		final Process process = builder.start();

		final int exitValue = process.waitFor();
		if (exitValue != 0) {
			byte[] errorOutput = null;
			try (final InputStream errorStream = process.getErrorStream()) {
				errorOutput = AOUtil.getDataFromInputStream(errorStream);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se pudo leer la salida de error " //$NON-NLS-1$
						+ "del proceso de desinstalacion del certificado en el llavero", e); //$NON-NLS-1$
			}
			if (errorOutput != null) {
				String errorMsg = new String(errorOutput);
				// El texto de solicitud de contrasena inicial puede haberse agregado a la salida de error,
				// asi que lo omitimos
				if (errorMsg.startsWith("Password:")) { //$NON-NLS-1$
					errorMsg = errorMsg.substring("Password:".length()); //$NON-NLS-1$
				}
				LOGGER.severe("Salida de error: " + errorMsg); //$NON-NLS-1$
				if (errorMsg.toLowerCase().contains("password")) { //$NON-NLS-1$
					throw new SecurityException("Contrasena incorrecta"); //$NON-NLS-1$
				}
				throw new KeyChainException("Error al desinstalar el certificado con el hash " + hash //$NON-NLS-1$
						+ " del llavero de macOS"); //$NON-NLS-1$
			}
		}
	}

	private static void deleteFile(final File file) throws IOException {
		if (file.exists() && !file.delete()) {
			throw new IOException("No puedo eliminar " + file.getName()); //$NON-NLS-1$
		}
	}

	/** Escribe un <i>script</i> en un fichero dado.
	 * @param path Ruta donde se escribir&aacute; el <i>script</i>.
	 * @param data Datos a escribir.
	 * @param append <code>true</code> permite contatenar el contenido del fichero con lo que se va a escribir. <code>false</code> el fichero se sobrescribe.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero. */
	static void writeScriptFile(final String path, final StringBuilder data, final boolean append) throws IOException{
		writeScriptFile(path, data.toString(), append);
	}

	/** Escribe un <i>script</i> en el fichero por defecto.
	 * @param data Datos a escribir.
	 * @param append <code>true</code> permite contatenar el contenido del fichero con lo que se va a escribir. <code>false</code> el fichero se sobreescribe.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero. */
	static void writeScriptFile(final String data, final boolean append) throws IOException {
		writeScriptFile(mac_script_path, data, append);
	}

	/** Escribe un <i>script</i> en un fichero dado.
	 * @param path Ruta donde se escribir&aacute; el <i>script</i>.
	 * @param data Datos a escribir.
	 * @param append <code>true</code> permite contatenar el contenido del fichero con lo que se va a escribir. <code>false</code> el fichero se sobrescribe.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero. */
	static void writeScriptFile(final String path, final String data, final boolean append) throws IOException{
		LOGGER.info("Se escribira en el fichero (" + LoggerUtil.getCleanUserHomePath(path) + ") el siguiente comando:\n" + data); //$NON-NLS-1$ //$NON-NLS-2$
		final File macScript = new File(path);
		try (final FileOutputStream fout = new FileOutputStream(macScript, append);) {
			fout.write((data + "\n").getBytes()); //$NON-NLS-1$
		}
	}

	/**
	 * Pide al usuario que cierre el navegador Mozilla Firefox y no permite continuar hasta que lo hace.
	 * @param parent Componente padre sobre el que mostrar los di&aacute;logos gr&aacute;ficos.
	 * @return Devuelve {@code true} si se cerr&oacute; el navegador, {@code false} en caso contrario.
	 */
	private static boolean closeFirefox(final Component parent) {

		if (isFirefoxOpen()) {
			JOptionPane.showMessageDialog(
					parent,
					SimpleAfirmaMessages.getString("RestoreApplication.7"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreApplication.9"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}

		int option = JOptionPane.OK_OPTION;
		while (option == JOptionPane.OK_OPTION && isFirefoxOpen()) {

			option = JOptionPane.showConfirmDialog(
					parent,
					SimpleAfirmaMessages.getString("RestoreApplication.12"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreApplication.9"), //$NON-NLS-1$
					JOptionPane.OK_CANCEL_OPTION,
					JOptionPane.WARNING_MESSAGE);
		}

		return option == JOptionPane.OK_OPTION;
	}

	/** Detecta si Firefox est&aacute; abierto.
	 * @return <code>true</code> si Firefox est&aacute; abierto,
	 *         <code>false</code> en caso contrario. */
	private static boolean isFirefoxOpen() {

		// Comprobamos si esta abierto el proceos de Firefox
		try {
			return checkProcess("Firefox.app", "FirefoxNightly.app", "FirefoxDeveloperEdition.app"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		catch (final IOException e) {
			LOGGER.warning("No se pudo completar la deteccion del proceso de Firefox. Se considerara que no esta en ejecucion: " + e); //$NON-NLS-1$
		}
		return false;
	}

	private static boolean checkProcess(final String... processNames) throws IOException {

		// Listamos los procesos abiertos y buscamos uno que contenga una cadena identificativa del proceso
		final ProcessBuilder pBuilder = new ProcessBuilder("ps", "aux"); //$NON-NLS-1$ //$NON-NLS-2$
		final Process ps = pBuilder.start();

		try (
				final InputStream resIs = ps.getInputStream();
				final BufferedReader resReader = new BufferedReader(
						new InputStreamReader(resIs));
				) {
			String line;
			while ((line = resReader.readLine()) != null) {
				for (final String processName : processNames) {
					if (line.contains(processName)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/** Copia un recurso desde dentro del JAR hacia una ruta externa.
     * @param pathToResource Carpeta del recurso dentro del JAR.
     * @param resourceName Nombre del recurso a copiar.
     * @param destinationPath Ruta externa destino.
     * @return Ruta completa del recurso copiado.
	 * @throws IOException En cualquier error. */
    static public String exportResource(final String pathToResource,
    		                            final String resourceName,
    		                            final String destinationPath) throws IOException {
        try (
    		final OutputStream resStreamOut = new FileOutputStream(destinationPath + resourceName);
        	final InputStream stream = RestoreConfigMacOSX.class.getResourceAsStream(pathToResource + resourceName)
		) {
            if(stream == null) {
                throw new IOException("No ha podido obtenerse el recurso \"" + resourceName + "\" del JAR."); //$NON-NLS-1$ //$NON-NLS-2$
            }
            int readBytes;
            final byte[] buffer = new byte[4096];
            while ((readBytes = stream.read(buffer)) > 0) {
                resStreamOut.write(buffer, 0, readBytes);
            }
        }
        return destinationPath + resourceName;
    }

	static class InvalidPasswordException extends SecurityException {

		/** Serial Id. */
		private static final long serialVersionUID = -8210913902255499160L;

		// Unicamente usaremos el constructor por defecto
	}
}
