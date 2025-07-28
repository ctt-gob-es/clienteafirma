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
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JLabel;
import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilitiesOsX;
import es.gob.afirma.keystores.mozilla.apple.ShellScript;
import es.gob.afirma.standalone.configurator.CertUtil.CertPack;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.manager.PluginsManager;

/** Configura la instalaci&oacute;n en Mac para la correcta ejecuci&oacute;n de la aplicaci&oacute;n. */
final class ConfiguratorMacOSX implements Configurator {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String KS_FILENAME = "/autofirma.pfx"; //$NON-NLS-1$
	private static final String SSL_CER_FILENAME = "/autofirma.cer"; //$NON-NLS-1$
	private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$
	private static final String CERT_CN = "127.0.0.1"; //$NON-NLS-1$
	private static final String CERT_CN_ROOT = "Autofirma ROOT"; //$NON-NLS-1$
	private static final String MACOSX_CERTIFICATE = "/Autofirma_ROOT.cer";//$NON-NLS-1$
	private static final String GET_USERS_COMMAND = "dscacheutil -q user"; //$NON-NLS-1$
	private static final String USER_DIR_LINE_HEADER = "dir: "; //$NON-NLS-1$
	private static final String USER_DIR_LINE_PREFIX = USER_DIR_LINE_HEADER + "/Users/"; //$NON-NLS-1$
	private static final String GET_USER_SCRIPTS_NAME = "scriptGetUsers";//$NON-NLS-1$
	private static final String SCRIPT_EXT = ".sh";//$NON-NLS-1$
	private static final String MAC_SCRIPT_NAME = "installCerScript"; //$NON-NLS-1$
	private static final String MAC_SCRIPT_EXT = ".sh"; //$NON-NLS-1$

	private static final String FIND_CERT_HASH_PREFIX = "hash:";  //$NON-NLS-1$

	private static final byte[] DUMMY = "dummy".getBytes(); //$NON-NLS-1$

	static File scriptFile;
	//private static File sslCerFile;

    /** Directorios de los usuarios del sistema. */
    private static String[] userDirs = null;

    private final boolean headless;
    private final boolean firefoxSecurityRoots;

    public ConfiguratorMacOSX(final boolean headless, final boolean firefoxSecurityRoots) {
		this.headless = headless;
    	this.firefoxSecurityRoots = firefoxSecurityRoots;
	}

	@Override
	public void configure(final Console console) throws IOException, GeneralSecurityException {

		userDirs = getSystemUsersHomes();

		console.print(Messages.getString("ConfiguratorMacOSX.2")); //$NON-NLS-1$

		final File resourcesDir = getResourcesDirectory(true);

		console.print(Messages.getString("ConfiguratorMacOSX.3") + resourcesDir.getAbsolutePath()); //$NON-NLS-1$

		// Creamos los nuevos certificados SSL y los instalamos en los almacenes de confianza,
		// eliminando versiones anteriores si es necesario
		configureSSL(resourcesDir, console);

		// Si se ha configurado en modo headless, se usaran los parametros de configuracion
		// ya proporcionados y se configurara Firefox para que confie en los certificados
		// raiz del llavero del sistema segun se haya indicado
		boolean needConfigureFirefoxSecurityRoots;
		if (this.headless) {
			needConfigureFirefoxSecurityRoots = this.firefoxSecurityRoots;
		}
		// Si se ha pedido ejecutar con interfaz grafica, le preguntaremos al usuario que desea hacer
		else {
			final int result = JOptionPane.showConfirmDialog(
					console.getParentComponent(),
					Messages.getString("ConfiguratorMacOSX.23"), //$NON-NLS-1$
					Messages.getString("ConfiguratorMacOSX.24"), //$NON-NLS-1$
					JOptionPane.YES_NO_OPTION);
			needConfigureFirefoxSecurityRoots = result == JOptionPane.YES_OPTION;
		}

		if (needConfigureFirefoxSecurityRoots) {
			console.print(Messages.getString("ConfiguratorMacOSX.22")); //$NON-NLS-1$
			try {
				ConfiguratorFirefoxMac.configureUseSystemTrustStore(true, userDirs, console);
			} catch (final MozillaProfileNotFoundException e) {
				console.print(Messages.getString("ConfiguratorMacOSX.21") + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

		console.print(Messages.getString("ConfiguratorMacOSX.8")); //$NON-NLS-1$
		LOGGER.info("Finalizado"); //$NON-NLS-1$
	}

	/** Genera e instala los certificados SSL para la comunicaci&oacute;n con la
	 * aplicaci&oacute;n.
	 * @param appDir Directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 * @param console Consola sobre la que escribir los mensajes de instalaci&oacute;n.
	 * @throws IOException Cuando ocurre un error en el proceso de instalaci&oacute;n.
	 * @throws GeneralSecurityException Cuando ocurre un error al generar el certificado SSL. */
	private static void configureSSL(final File appDir,
			                         final Console console) throws IOException,
	                                                               GeneralSecurityException {
		console.print(Messages.getString("ConfiguratorMacOSX.5")); //$NON-NLS-1$

		// Generamos un fichero que utilizaremos para guardar y ejecutar AppleScripts
		try {
			scriptFile = File.createTempFile(MAC_SCRIPT_NAME, MAC_SCRIPT_EXT);
			ConfiguratorMacUtils.addExexPermissionsToFile(scriptFile);
		}
		catch(final Exception e) {
			console.print(Messages.getString("ConfiguratorMacOSX.18"));  //$NON-NLS-1$
			LOGGER.severe("Error creando script temporal: " + e); //$NON-NLS-1$
			throw new IOException("Error creando script temporal", e); //$NON-NLS-1$
		}

		// Damos permisos sobre el directorio de la aplicacion
		ConfiguratorMacUtils.addExexPermissionsToAllFilesOnDirectory(appDir);

		// Generamos los certificados de CA y SSL
		final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(
			ConfiguratorUtil.CERT_ALIAS,
			KS_PASSWORD
		);

		console.print(Messages.getString("ConfiguratorMacOSX.11")); //$NON-NLS-1$

		// Copiamos a disco los certificados CA y SSL y el almacen SSL<
		final File caCertFile = new File(appDir, MACOSX_CERTIFICATE);
		ConfiguratorUtil.installFile(
				certPack.getCaCertificate().getEncoded(),
				caCertFile);

		ConfiguratorUtil.installFile(
				certPack.getPkcs12(),
				new File(appDir, KS_FILENAME));

		final File sslCertFile = new File(appDir, SSL_CER_FILENAME);
		ConfiguratorUtil.installFile(
				certPack.getSslCertificate().getEncoded(),
				sslCertFile);

		// Cerramos las instancias de firefox que esten abiertas
		closeFirefox();

		// Obtenemos del usuario y probamos la contrasena del Llavero
		byte[] keyChainPhrase = getKeyChainPhrase(console, false);

		// Desinstalamos de los almacenes cualquier certificado anterior generado para este proposito
		console.print(Messages.getString("ConfiguratorMacOSX.15")); //$NON-NLS-1$

		boolean passwordValidated;
		do {
			passwordValidated = true;
			try {
				uninstallProcess(appDir, keyChainPhrase);
			}
			catch (final InvalidPasswordException e) {
				keyChainPhrase = getKeyChainPhrase(console, true);
				passwordValidated = false;
			}
		} while (!passwordValidated);

		// Se instalan los certificados en el almacen de Apple
		final JLabel msgLabel = new JLabel("<html>" + Messages.getString("ConfiguratorMacOSX.20") + "</html>"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		JOptionPane.showMessageDialog(console.getParentComponent(), msgLabel);
		console.print(Messages.getString("ConfiguratorMacOSX.6")); //$NON-NLS-1$

		try {
			installTrustedCertsInAppleKeyChain(caCertFile, sslCertFile, keyChainPhrase, console);
		}
		catch (final Exception e) {
			console.print(Messages.getString("ConfiguratorMacOSX.34")); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
					Messages.getString("ConfiguratorMacOSX.25"), //$NON-NLS-1$
					Messages.getString("ConfiguratorMacOSX.26"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e);
		}

		// Se instalan los certificados en el almacen de Firefox
		console.print(Messages.getString("ConfiguratorMacOSX.13")); //$NON-NLS-1$
		final String[] userHomes = getSystemUsersHomes();
		try {
			ConfiguratorFirefoxMac.installOnMozillaKeyStore(appDir, userHomes, scriptFile, console);
			LOGGER.info("Configuracion de NSS"); //$NON-NLS-1$
			MozillaKeyStoreUtilitiesOsX.configureMacNSS(MozillaKeyStoreUtilities.getSystemNSSLibDir());

			executeScriptFile(scriptFile, true, true);
		}
		catch (final MozillaProfileNotFoundException e) {
			LOGGER.severe("Perfil de Mozilla no encontrado: " + e); //$NON-NLS-1$
			console.print(Messages.getString("ConfiguratorMacOSX.12")); //$NON-NLS-1$
		}
		catch (final AOException e1) {
			LOGGER.severe("La configuracion de NSS para Mac OS X ha fallado: " + e1); //$NON-NLS-1$
		}
		catch (final Exception e1) {
			LOGGER.log(Level.WARNING, "Error en la importacion del certificado de confianza en el almacen de Firefox: " + e1, e1); //$NON-NLS-1$
		}
	}

	/**
	 * Obtiene la contrase&ntilde;a del llavero y lo desbloquea para comprobarla.
	 * @param console Consola en la que mostrar los mensajes al usuario.
	 * @param force Indica si se debe forzar que se introduzca una contrase&ntilde;a.
	 * @return Contrase&ntilde;a del llavero.
	 */
	private static byte[] getKeyChainPhrase(final Console console, final boolean force) {

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
						? Messages.getString("ConfiguratorMacOSX.28") //$NON-NLS-1$
						: Messages.getString("ConfiguratorMacOSX.27"); //$NON-NLS-1$

				try {
					// Se pone en una linea para evitar que la contrasena se exponda en claro en memoria
					phrase = new String(AOUIFactory.getPassword(text, console.getParentComponent())).getBytes(StandardCharsets.UTF_8);
				}
				catch (final AOCancelledOperationException e) {
					LOGGER.info("Se cancelo el dialogo de entrada de contrasena: " + e); //$NON-NLS-1$
					final int option = AOUIFactory.showConfirmDialog(console.getParentComponent(),
							Messages.getString("ConfiguratorMacOSX.29"), //$NON-NLS-1$
							Messages.getString("ConfiguratorMacOSX.30"), //$NON-NLS-1$
							JOptionPane.YES_NO_OPTION,
							JOptionPane.WARNING_MESSAGE);
					if (option == JOptionPane.YES_OPTION) {
						console.print(Messages.getString("ConfiguratorMacOSX.31")); //$NON-NLS-1$
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
			final File sslCertFile, final byte[] keyChainPhrase, final Console console)
					throws IOException, InterruptedException, KeyChainException {

		// Insertamos el certificado raiz
		installTrustedCertInAppleKeyChain(caCertFile, keyChainPhrase, true);
		console.print(Messages.getString("ConfiguratorMacOSX.32")); //$NON-NLS-1$

		// Insertamos el certificado SSL
		installTrustedCertInAppleKeyChain(sslCertFile, keyChainPhrase, false);
		console.print(Messages.getString("ConfiguratorMacOSX.33")); //$NON-NLS-1$
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
				throw new KeyChainException("Error al instalar el certificado " + sslCertFile //$NON-NLS-1$
						+ " en el llavero de macOS"); //$NON-NLS-1$
			}
		}
	}

	@Override
	public void uninstall(final Console console, final PluginsManager pluginsManager) {

		LOGGER.info("Desinstalacion del certificado raiz de los almacenes de MacOSX"); //$NON-NLS-1$

		final File resourcesDir;
		try {
			resourcesDir = getResourcesDirectory(true);
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "No se pudo obtener el directorio de recursos de la aplicacion", e); //$NON-NLS-1$
			return;
		}

		// Obtenemos del usuario y probamos la contrasena del Llavero
		byte[] keyChainPhrase = getKeyChainPhrase(console, false);

		boolean passwordValidated;
		do {
			passwordValidated = true;
			try {
				uninstallProcess(resourcesDir, keyChainPhrase);
			}
			catch (final InvalidPasswordException e) {
				keyChainPhrase = getKeyChainPhrase(console, true);
				passwordValidated = false;
			}
		} while (!passwordValidated);

		// Listamos los plugins instalados
		List<AfirmaPlugin> plugins = null;
		try {
			plugins = pluginsManager.getPluginsLoadedList();
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo obtener el listado de plugins de Autofirma", e); //$NON-NLS-1$
		}

		// Desinstalamos los plugins instalados si los hubiese
		if (plugins != null && !plugins.isEmpty()) {
			LOGGER.info("Desinstalamos los plugins instalados"); //$NON-NLS-1$
			for (final AfirmaPlugin plugin : plugins) {
				try {
					pluginsManager.uninstallPlugin(plugin);
				} catch (final Exception e) {
					LOGGER.log(Level.WARNING, "No se pudo desinstalar el plugin: " + plugin.getInfo().getName(), e); //$NON-NLS-1$
				}
			}
		}

		// Eliminamos si existe el directorio alternativo usado para el guardado de certificados
		// SSL durante el proceso de restauracion de la instalacion
		final File alternativeDir = getAlternativeApplicationDirectory();
		if (alternativeDir.isDirectory()) {
			try {
				Files.walkFileTree(
						alternativeDir.toPath(),
						new HashSet<>(),
						Integer.MAX_VALUE,
						new SimpleFileVisitor<Path>() {
							@Override
							public FileVisitResult visitFile(final Path file, final BasicFileAttributes attr) {
								try {
									Files.delete(file);
								}
								catch (final Exception e) {
									LOGGER.warning("No se pudo eliminar el fichero '" + LoggerUtil.getCleanUserHomePath(file.toAbsolutePath().toString()) + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
								}
								return FileVisitResult.CONTINUE;
							}
							@Override
							public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) {
								try {
									Files.delete(dir);
								} catch (final IOException e) {
									LOGGER.warning("No se pudo eliminar el directorio '" + LoggerUtil.getCleanUserHomePath(dir.toAbsolutePath().toString()) + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
								}
								return FileVisitResult.CONTINUE;
							}
						});
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se ha podido eliminar por completo el directorio alternativo para el certificado SSL", e); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Ejecuta el proceso de desinstalaci&oacute;n. Durante el mismo se desinstalan los certificados
	 * de confianza SSL de los almacenes del sistema.
	 * @param appDir Directorio de instalaci&oacute;n.
	 * @throws InvalidPasswordException Cuando la contrase&ntilde;a introducida de administraci&oacute;n no sea correcta.
	 */
	private static void uninstallProcess(final File appDir, final byte[] keyChainPhrase) throws InvalidPasswordException {

		// Desinstalamos los anteriores certificados SSL del Llavero
		uninstallRootCAMacOSXKeyStore(keyChainPhrase);

		// Identificamos los directorios de los usuarios
		final String[] usersHomes = getSystemUsersHomes();

		// Desinstalamos los certificados del almacen de confianza de firefox de los usuarios localizados
		if (usersHomes != null) {
			try {
				ConfiguratorFirefoxMac.uninstallFromMozillaKeyStore(appDir, usersHomes, scriptFile);
			}
			catch (final MozillaProfileNotFoundException e) {
				LOGGER.info("No se han encontrado perfiles de Mozilla de los que desinstalar: " + e); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.log(Level.SEVERE, "Se ha producido un error durante la desinstalacion: " + e, e); //$NON-NLS-1$
			}
		}
		// Si no hay directorios de usuario, se cancela la desinstalacion de los certificados de firefox
		else {
			LOGGER.info("No se han encontrado directorios de usuario y se cancela la carga de perfiles de firefox de los que deinstalar certificados"); //$NON-NLS-1$
		}
		// Desinstalamos los anteriores certificados SSL del almacen de confianza de Firefox

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
						LOGGER.info("Salida de error: " + errorMsg); //$NON-NLS-1$
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

	private static File getResourcesDirectory() {
		final String userDir = System.getenv("HOME"); //$NON-NLS-1$
		return new File (userDir, "Library/Application Support/Autofirma"); //$NON-NLS-1$
	}

	private static File getResourcesDirectory(final boolean create) throws IOException {

		// Devuelve un directorio en el que copiar y generar los recursos
		// necesarios por la aplicacion
		final File appDir = getResourcesDirectory();
		if (create && !appDir.isDirectory()) {

			if (appDir.mkdirs()) {
				ConfiguratorMacUtils.addExexPermissionsToFile(appDir);
			}

			// Si no hemos podido crearlo, lanzamos una excepcion
			if (!appDir.isDirectory()) {
				throw new IOException("No se ha podido generar el directorio de recursos de la aplicacion: " + appDir.getAbsolutePath()); //$NON-NLS-1$
			}
		}

		return appDir;
	}

	/** Ejecuta un fichero de scripts.
	 * @param script Ruta del fichero de <i>script</i>.
	 * @param administratorMode {@code true} el <i>script</i> se ejecuta con permisos de adminsitrador,
	 * {@code false} en caso contrario.
	 * @param delete {@code true} borra el fichero despu&eacute;s de haberse ejecutado, {@code false} no hace nada.
	 * @return La cadena que da como resultado el <i>script</i>.
	 * @throws IOException Cuando ocurre un error en la ejecuci&oacute;n del <i>script</i>.
     * @throws InterruptedException  Cuando se interrumpe la ejecuci&oacute;n del script (posiblemente por el usuario). */
	public static String executeScriptFile(final File script, final boolean administratorMode, final boolean delete) throws IOException, InterruptedException {

		LOGGER.info("Se ejecuta el script"); //$NON-NLS-1$

		final ShellScript shellScript = new ShellScript(script, delete);
		try {
			String result;
			if (administratorMode) {
				result = shellScript.runAsAdministrator();
			}
			else {
				result = shellScript.run();
			}
			return result;
		}
		catch (final IOException e) {
			throw new IOException("Error en la ejecucion del script via AppleScript: " + e, e); //$NON-NLS-1$
		}
	}

	/**
	 * Pide al usuario que cierre el navegador Mozilla Firefox y no permite continuar hasta que lo hace.
	 */
	private static void closeFirefox() {

		while (isFirefoxOpen()) {
			JOptionPane.showMessageDialog(
					null,
					Messages.getString("ConfiguratorMacOSX.17"), //$NON-NLS-1$
					Messages.getString("ConfiguratorMacOSX.16"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}
	}

	/** Detecta si el proceso de Firefox est&aacute; abierto.
	 * @return <code>true</code> si el proceso de Firefox est&aacute; abierto,
	 *         <code>false</code> en caso contrario. */
	private static boolean isFirefoxOpen() {

		// Listamos los procesos abiertos y buscamos uno que contenga una cadena identificativa de Firefox
		try {
			final ProcessBuilder psProcessBuilder = new ProcessBuilder("ps", "aux"); //$NON-NLS-1$ //$NON-NLS-2$
			final Process ps = psProcessBuilder.start();

			String line;
			try (
					final InputStream resIs = ps.getInputStream();
					final BufferedReader resReader = new BufferedReader(
							new InputStreamReader(resIs));
					) {
				while ((line = resReader.readLine()) != null) {
					if (line.contains("Firefox.app") //$NON-NLS-1$
							|| line.contains("FirefoxNightly.app") //$NON-NLS-1$
							|| line.contains("FirefoxDeveloperEdition.app")) { //$NON-NLS-1$
						return true;
					}
				}
			}
		}
		catch (final IOException e) {
			LOGGER.warning("No se pudo completar la deteccion del proceso de Firefox. Se considerara que no esta en ejecucion: " + e); //$NON-NLS-1$
		}

		return false;
	}

    /**
     * Devuelve un listado con todos los directorios de usuario del sistema.
	 * @return Listado de directorios o {@code null} si no se ha podido cargar.
	 */
	static String[] getSystemUsersHomes() {

		if (userDirs != null) {
			return userDirs;
		}

		try {
			final File getUsersScriptFile = createGetUsersScript();
			final Object o = executeScriptFile(getUsersScriptFile, false, true);
			final Set<String> dirs = new HashSet<>();

			// Leemos de la salida del comando el listado completo de usuarios y determinamos
			// cuales son los usuarios reales. A partir de macOS Sequoia 15.1 se identifican
			// las aplicaciones como usuarios, lo que requiere que se diferencien y nos impiden
			// establecen un tamano de salida razonable para el comando.
			try (	final InputStream resIs = new ByteArrayInputStream(o.toString().getBytes());
					final InputStreamReader isReader = new InputStreamReader(resIs);
					final BufferedReader resReader = new BufferedReader(isReader); ) {
				String line;
				while ((line = resReader.readLine()) != null) {
					if (line.startsWith(USER_DIR_LINE_PREFIX)){
						dirs.add(line.substring(USER_DIR_LINE_HEADER.length()));
					}
				}
			}
			userDirs = dirs.toArray(new String[dirs.size()]);
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error al obtener el listado de usuarios del sistema", e); //$NON-NLS-1$
			userDirs = null;
		}

		return userDirs;
	}

	/** Crea un fichero de <i>script</i> para la obtenci&oacute;n de los usuarios del sistema.
	 * @return <i>Script</i> para la obtenci&oacute;n de los usuarios del sistema.
	 * @throws IOException Cuando no se pueda crear el fichero de <i>script</i>. */
	private static File createGetUsersScript() throws IOException {
		final StringBuilder script = new StringBuilder(GET_USERS_COMMAND);
		final File usersScriptFile = File.createTempFile(GET_USER_SCRIPTS_NAME, SCRIPT_EXT);
		try {
			ConfiguratorMacUtils.writeScriptFile(script, usersScriptFile, true);
		} catch (final IOException e) {
			LOGGER.log(Level.WARNING, "Ha ocurrido un error al escribir en disco el script de obtencion de usuarios: " + e, e); //$NON-NLS-1$
		}
		ConfiguratorMacUtils.addExexPermissionsToFile(usersScriptFile);

		return usersScriptFile;
	}

	@Override
	public File getAplicationDirectory() {
		try {
			return new File(
					ConfiguratorMacOSX.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()
				).getParentFile();
		} catch (final URISyntaxException e) {
			LOGGER.log(Level.WARNING, "No se pudo identificar el directorio de instalacion de la aplicacion (en el que se encuentra el configurador)", e); //$NON-NLS-1$
			return null;
		}
	}

	@Override
	public File getAlternativeApplicationDirectory() {
		return getResourcesDirectory();
	}

	static class InvalidPasswordException extends SecurityException {

		/** Serial Id. */
		private static final long serialVersionUID = -9058805499745499488L;

		// Unicamente usaremos el constructor por defecto
	}
}
