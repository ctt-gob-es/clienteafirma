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
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilitiesOsX;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.so.macos.MacUtils;
import es.gob.afirma.standalone.so.macos.ShellScript;
import es.gob.afirma.standalone.so.macos.UnixUtils;
import es.gob.afirma.standalone.ui.restoreconfig.CertUtil.CertPack;
import es.gob.afirma.standalone.ui.restoreconfig.RestoreConfigFirefox.MozillaProfileNotFoundException;

/**
 * Clase que contiene la l&oacute;gica para realizar las tareas de restauraci&oacute;n
 * de la configuraci&oacute;n de navegadores para el sistema operativo MacOsX.
 */
final class RestoreConfigMacOSX implements RestoreConfig {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String KS_FILENAME = "/autofirma.pfx"; //$NON-NLS-1$
	private static final String SSL_CA_CER_FILENAME = "/AutoFirma_ROOT.cer";//$NON-NLS-1$
	private static final String SSL_CER_FILENAME = "/autofirma.cer"; //$NON-NLS-1$
	private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$
	private static final String CERT_CN = "127.0.0.1"; //$NON-NLS-1$
	private static final String CERT_CN_ROOT = "'AutoFirma ROOT'"; //$NON-NLS-1$
	static final String GET_USER_SCRIPT = "/getUsers.sh";//$NON-NLS-1$
	static final String OSX_GET_USERS_COMMAND = "dscacheutil -q user"; //$NON-NLS-1$
	private final static String USER_DIR_LINE_PREFIX = "dir: "; //$NON-NLS-1$
	static final String MAC_SCRIPT_NAME = "/installCerScript"; //$NON-NLS-1$
	static final String MAC_SCRIPT_EXT = ".sh"; //$NON-NLS-1$
	static final String EXPORT_PATH = "export PATH=$PATH:";//$NON-NLS-1$
	static final String EXPORT_LIBRARY_LD = "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:";//$NON-NLS-1$


	private static final byte[] DUMMY = "dummy".getBytes(); //$NON-NLS-1$

	private static final String CHANGE_OWN_COMMAND = "chown %USERNAME% \"%DIR%\""; //$NON-NLS-1$

	static final String GET_USERS_COMMAND = "dscacheutil -q user"; //$NON-NLS-1$
	private static final String GET_USER_SCRIPTS_NAME = "scriptGetUsers";//$NON-NLS-1$
	private static final String SCRIPT_EXT = ".sh";//$NON-NLS-1$

	static String mac_script_path;

	private static List<String> userDirs = null;

	private static File sslCerFile;

	@Override
	public void restore(final RestoreConfigPanel configPanel) {

		userDirs = getSystemUsersHomes();

		// Comprobamos si se debe configurar Firefox para que use el almacen de confianza del sistema
		final boolean firefoxSecurityRoots = configPanel.firefoxIntegrationCb.isSelected();

		// Tomamos como directorio de aplicacion aquel en el que podemos generar
		// los certificados SSL para despues usarlos
		final File appDir = AutoFirmaUtil.getMacOsXAlternativeAppDir();

		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.3", appDir.getAbsolutePath())); //$NON-NLS-1$

		// Verifica si se tiene permisos para escribir en el directorio de instalacion
		// y establece como directorio de trabajo otro distinto en caso de no tenerlos
		if (!appDir.isDirectory() && !appDir.mkdirs()) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.1")); //$NON-NLS-1$
			LOGGER.severe("No se puede utilizar el directorio alternativo de trabajo por no ser un directorio y no poder crearse"); //$NON-NLS-1$
		}

		// Generamos un fichero que utilizaremos para guardar y ejecutar un script
		try {
			mac_script_path = File.createTempFile(MAC_SCRIPT_NAME, MAC_SCRIPT_EXT).getAbsolutePath();
		}
		catch (final Exception e) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.15")); //$NON-NLS-1$
			LOGGER.log(Level.SEVERE, "Error creando script temporal. Se aborta la operacion", e); //$NON-NLS-1$
		}

		// Cambiamos el propietario del directorio alternativo para que sea el usuario. Sino, este directorio
		// perteneceria si no al administrador y despues el usuario no tendria permisos de escritura sobre el
		final String username = System.getenv("USER"); //$NON-NLS-1$
		changeDirectoryProperty(appDir, username);

		// Ejecutamos el script de inmediato porque necesitamos estos permisos para seguir. Despues se ejecutara
		// de nuevo con el resto de comandos
		UnixUtils.addAllPermissionsToFile(new File(mac_script_path));
		try {
			executeScript(mac_script_path, true, false);
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se ha podido cambiar la propiedad del directorio de AutoFirma", e); //$NON-NLS-1$
		}

		// Iniciamos la restauracion de los certificados SSL
		restoreSslCertificates(appDir, configPanel);

		// Iniciamos la restauracion de la confianza de Chrome en el protocolo afirma
		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.16")); //$NON-NLS-1$
		closeChrome(configPanel);
		RestoreRemoveChromeWarning.removeChromeWarningsMac(appDir, userDirs);

		if (firefoxSecurityRoots) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.21")); //$NON-NLS-1$
		} else {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.22")); //$NON-NLS-1$
		}
		try {
			final List<String> usersDirectories = getSystemUsersHomes();
			RestoreConfigFirefox.configureUseSystemTrustStore(firefoxSecurityRoots, usersDirectories);
		}
		catch (final MozillaProfileNotFoundException e) {
			LOGGER.info("No se encontraron perfiles de Firefox en los que configurar la confianza en el almacen del sistema"); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.24")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error configurando la confianza de Firefox en el almacen del sistema (activando: " + firefoxSecurityRoots + ")", e); //$NON-NLS-1$ //$NON-NLS-2$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.23")); //$NON-NLS-1$
		}

		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.8")); //$NON-NLS-1$
		LOGGER.info("Finalizado" ); //$NON-NLS-1$
	}

	/**
	 * Escribe en el script de ejecuci&oacute;n el comando para el cambio de propiedad
	 * de un fichero/directorio a un usuario.
	 * @param file Fichero del que cambiar la propiedad.
	 * @param username Nombre del usuario al que se le desea asignar la propiedad.
	 */
	private static void changeDirectoryProperty(final File file, final String username) {
		final String cmd = CHANGE_OWN_COMMAND
				.replace("%DIR%", file.getAbsolutePath()) //$NON-NLS-1$
				.replace("%USERNAME%", username); //$NON-NLS-1$
		try {
			writeScriptFile(mac_script_path, new StringBuilder(cmd), true);
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se ha podido agregar al script el comando para el cambio de propiedad de: " + file.getAbsolutePath(), e); //$NON-NLS-1$
		}
	}

	/** Restaura la configuraci&oacute;n de los certificados SSL para la comunicaci&oacute;n
	 * por <i>sockets</i>.
	 * @param appDir Directorio en donde se crearan los ficheros necesarios para
	 *                   llevar a cabo la restauraci&oacute;n.
	 * @param configPanel Panel de configuraci&oacute;n sobre el que se ir&aacute;n
	 *                    imprimiendo los mensajes con el estado de la operaci&oacute;n. */
	private static void restoreSslCertificates(final File appDir, final RestoreConfigPanel configPanel) {

		final File sslRootCertFile;
		final File sslCertFile;
		if (!checkSSLKeyStoreGenerated(appDir)) {
			// Damos permisos al script
			UnixUtils.addExexPermissionsToAllFilesOnDirectory(appDir);

			try {
				configureSSL(appDir, configPanel);
			}
			catch (final IOException e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.7")); //$NON-NLS-1$
				LOGGER.log(Level.SEVERE, "Error al copiar a disco los certificados SSL. Se aborta la operacion", e); //$NON-NLS-1$
				return;
			}
			catch (final GeneralSecurityException e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.9")); //$NON-NLS-1$
				LOGGER.log(Level.SEVERE, "Error al generar los certificados SSL. Se aborta la operacion", e); //$NON-NLS-1$
				return;
			}
			sslRootCertFile = new File(appDir, SSL_CA_CER_FILENAME);
			sslCertFile = new File(appDir, SSL_CER_FILENAME);
		}
		else if (!checkSSLRootCertificateGenerated(appDir)) {
			// Damos permisos al script
			UnixUtils.addExexPermissionsToAllFilesOnDirectory(appDir);

			sslRootCertFile = new File(appDir, SSL_CA_CER_FILENAME);
			sslCertFile = new File(appDir, SSL_CER_FILENAME);
			try {
				final Certificate[] sslCertChain;
				try (FileInputStream fis = new FileInputStream(new File(appDir, KS_FILENAME))) {
					final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
					ks.load(fis, KS_PASSWORD.toCharArray());
					sslCertChain = ks.getCertificateChain(ks.aliases().nextElement());
				}

				// Guardamos el certificado raiz
				RestoreConfigUtil.installFile(
					sslCertChain[sslCertChain.length - 1].getEncoded(),
					sslRootCertFile
				);

				// Guardamos el certificado ssl
				RestoreConfigUtil.installFile(
					sslCertChain[0].getEncoded(),
					sslCertFile
				);
			}
			catch (final Exception e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.4")); //$NON-NLS-1$
				LOGGER.severe("Error al copiar los certificados SSL a disco: " + e); //$NON-NLS-1$
				return;
			}
		}
		else {
			LOGGER.info("El almacen y el certificado raiz existen." ); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.14")); //$NON-NLS-1$
			sslRootCertFile = new File(appDir, SSL_CA_CER_FILENAME);
			sslCertFile = new File(appDir, SSL_CER_FILENAME);

			// El certificado SSL (.cer) siempre se guarda a disco a partir del almacen
			try {
				final Certificate[] sslCertChain;
				try (FileInputStream fis = new FileInputStream(new File(appDir, KS_FILENAME))) {
					final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
					ks.load(fis, KS_PASSWORD.toCharArray());
					sslCertChain = ks.getCertificateChain(ks.aliases().nextElement());
				}

				// Guardamos el certificado SSL
				RestoreConfigUtil.installFile(
					sslCertChain[0].getEncoded(),
					sslCertFile
				);
			}
			catch (final Exception e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.4")); //$NON-NLS-1$
				LOGGER.severe("Error al copiar el certificado SSL a disco: " + e); //$NON-NLS-1$
				return;
			}
		}

		try {
			installRootCA(appDir, sslRootCertFile, sslCertFile, configPanel);
		}
		catch (final SecurityException e) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.10")); //$NON-NLS-1$
			LOGGER.severe("No se tienen permisos para realizar la instalacion de los certificados SSL. Se aborta la operacion: " + e); //$NON-NLS-1$
			return;
		}
		catch (final KeyStoreException | IOException e) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.11")); //$NON-NLS-1$
			LOGGER.log(Level.SEVERE, "Error al instalar los certificados SSL en los almacenes de confianza. Se aborta la operacion: " + e, e); //$NON-NLS-1$
			return;
		}
	}

	/**
	 * Pide al usuario que cierre el navegador Google Chrome y no permite continuar hasta que lo hace.
	 * @param parent Componente padre sobre el que mostrar los di&aacute;logos gr&aacute;ficos.
	 */
	private static void closeChrome(final Component parent) {
		if (isChromeOpen()) {
			JOptionPane.showMessageDialog(
					parent,
					SimpleAfirmaMessages.getString("RestoreAutoFirma.8"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreAutoFirma.9"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}

		int option = JOptionPane.OK_OPTION;
		while (option == JOptionPane.OK_OPTION && isChromeOpen()) {
			option = JOptionPane.showConfirmDialog(
					parent,
					SimpleAfirmaMessages.getString("RestoreAutoFirma.11"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreAutoFirma.9"), //$NON-NLS-1$
					JOptionPane.OK_CANCEL_OPTION);
		}
	}

	/**
	 * Comprueba si ya existe un almac&eacute;n de certificados generado.
	 * @param appConfigDir Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un almacen de certificados SSL,
	 *         {@code false} en caso contrario.
	 */
	private static boolean checkSSLKeyStoreGenerated(final File appConfigDir) {
		return new File(appConfigDir, KS_FILENAME).exists();
	}

	/**
	 * Comprueba si ya existe un certificado ra&iacute;z generado.
	 * @param appDir Directorio de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un certificado ra&iacute;z .cer, {@code false} en caso contrario.
	 */
	private static boolean checkSSLRootCertificateGenerated(final File appDir) {
		return new File(appDir, SSL_CA_CER_FILENAME).exists();
	}

	/**
	 * Genera y copia a disco los certificados SSL para la comunicaci&oacute;n con la aplicaci&oacute;n.
	 * @param workingDir Directorio en el que almacenar los certificados de la aplicaci&oacute;n.
	 * @param configPanel Panel de configuraci&oacute;n con las trazas de ejecuci&oacute;n.
	 * @throws IOException Cuando ocurre un error en el proceso de instalaci&oacute;n.
	 * @throws GeneralSecurityException Cuando ocurre un error al generar el certificado SSL.
	 */
	private static void configureSSL(final File workingDir, final RestoreConfigPanel configPanel) throws IOException, GeneralSecurityException {

		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.5")); //$NON-NLS-1$

		// Generamos los certificados de CA y SSL
		final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(
			RestoreConfigUtil.CERT_ALIAS,
			KS_PASSWORD
		);

		deleteInstalledCertificates(workingDir);

		// Copiamos los certificados CA, SSL y el PKCS#12 a disco
		RestoreConfigUtil.installFile(
        		certPack.getCaCertificate().getEncoded(),
        		new File(workingDir, SSL_CA_CER_FILENAME));

		RestoreConfigUtil.installFile(
			certPack.getPkcs12(),
			new File(workingDir, KS_FILENAME)
		);

		RestoreConfigUtil.installFile(
				certPack.getSslCertificate().getEncoded(),
				new File(workingDir, SSL_CER_FILENAME)
			);
	}

    /** Devuelve un listado con todos los directorios de usuario del sistema.
	 * @return Listado de directorios. */
	private static List<String> getSystemUsersHomes() {

		if (userDirs != null) {
			return userDirs;
		}
		try {
			final File getUsersScriptFile = createGetUsersScript();
			final Object o = MacUtils.executeScriptFile(getUsersScriptFile, false, true);
			if (o == null) {
				userDirs = null;
			}
			else {
			userDirs = new ArrayList<>();
			try (
					final InputStream resIs = new ByteArrayInputStream(o.toString().getBytes());
					final BufferedReader resReader = new BoundedBufferedReader(
							new InputStreamReader(resIs),
							2048, // Maximo 2048 lineas de salida (256 perfiles)
							2048 // Maximo 2048 caracteres por linea
							);
					) {
				String line;
				while ((line = resReader.readLine()) != null) {
					if (line.startsWith(USER_DIR_LINE_PREFIX)){
						userDirs.add(line.substring(USER_DIR_LINE_PREFIX.length()));
					}
				}
			}
		}
		}
		catch (final IOException | InterruptedException e) {
			LOGGER.severe("Error al generar el listado perfiles de Firefox del sistema: " + e); //$NON-NLS-1$
			userDirs = null;
		}

		return userDirs;
	}


	/** Crea un fichero de script para la obtenci&oacute;n de los usuarios del sistema.
	 * @return Fichero de script para la obtenci&oacute;n de los usuarios del sistema.
	 * @throws IOException Cuando no se pueda crear el fichero de script. */
	private static File createGetUsersScript() throws IOException {
		final StringBuilder script = new StringBuilder(GET_USERS_COMMAND);
		final File scriptFile = File.createTempFile(GET_USER_SCRIPTS_NAME, SCRIPT_EXT);
		try {
			MacUtils.writeScriptFile(script, scriptFile, true);
		}
		catch (final IOException e) {
			LOGGER.log(Level.WARNING, "Ha ocurrido un error al generar el script de obtencion de usuarios: " + e, e); //$NON-NLS-1$
		}
		UnixUtils.addAllPermissionsToFile(scriptFile);
		return scriptFile;
	}

	/** Instala los certificados de comunicaci&oacute;n en el almac&eacute;n de Mozilla y Apple.
	 * @param appDir Directorio de aplicaci&oacute;n.
	 * @param rootCertFile Certificado ra&iacute;z a instalar.
	 * @param sslCertFile Certificado SSL a instalar.
	 * @param configPanel Panel de configuraci&oacute;n con las trazas de ejecuci&oacute;n.
	 * @throws IOException Si ocurre alg&uacute;n problema durante el proceso.
	 * @throws SecurityException Cuando no se tengan permisos para realizar la instalaci&oacute;n.
	 * @throws KeyStoreException Cuando ocurre un error durante la importaci&oacute;n.
	 */
	private static void installRootCA(final File appDir, final File rootCertFile, final File sslCertFile, final RestoreConfigPanel configPanel)
			throws IOException, SecurityException, KeyStoreException {

		// Obligamos a que se cierre Firefox antes de manipular el certificado en su almacen
		final boolean closed = closeFirefox(configPanel);

		// Si no se ha cerrado el navegador, es muy probable que no se pueda instalar el certificado de confianza,
		// asi que mostramos un mensaje advirtiendolo
		if (!closed) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.45")); //$NON-NLS-1$
		}

		// Desinstalamos del llavero los certificados anteriores
		LOGGER.info("Desinstalacion de versiones anteriores del certificado raiz del almacen de MacOSX"); //$NON-NLS-1$
		try {
			uninstallRootCAMacOSXKeyStore();
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Se ha producido un error durante la busqueda y desinstalacion de versiones anteriores del certificado SSL en el llavero de macOS: " + e, e); //$NON-NLS-1$
		}

		// Se instalan los certificados en el llavero del sistema operativo
		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.6")); //$NON-NLS-1$
		try {
			installTrustedCertsInAppleKeyChain(rootCertFile, sslCertFile, configPanel);
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error en la importacion del certificado de confianza en el llavero del sistema operativo", e); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.20")); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
					SimpleAfirmaMessages.getString("RestoreConfigMacOSX.27", rootCertFile.getAbsolutePath(), sslCertFile.getAbsolutePath()), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreConfigMacOSX.28"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e);
		}

		// Copiamos en disco certUtil para la configuracion de los certificados en el almacen de Firefox
		RestoreConfigFirefox.copyConfigurationFiles(appDir);

		// Desinstalamos del almacen de Firefox el certificado anterior
		LOGGER.info("Desinstalacion de versiones anteriores del certificado raiz del almacen de Firefox"); //$NON-NLS-1$
		try {
			uninstallRootCAFirefoxKeyStore(appDir);
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Se ha producido un error durante la busqueda y desinstalacion de versiones anteriores del certificado SSL en Firefox: " + e, e); //$NON-NLS-1$
		}


		// Se instala el certificado raiz en el almacen de Firefox
		try {

			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.13")); //$NON-NLS-1$

			// Instalar el certificado en Mozilla
			RestoreConfigFirefox.installRootCAMozillaKeyStore(
				appDir,
				rootCertFile,
				userDirs
			);

			LOGGER.info("Configuracion de NSS"); //$NON-NLS-1$
			MozillaKeyStoreUtilitiesOsX.configureMacNSS(MozillaKeyStoreUtilities.getSystemNSSLibDir());
		}
		catch (final MozillaProfileNotFoundException e) {
			LOGGER.warning("No se ha encontrado el perfil de Mozilla en macOS: " + e); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.12")); //$NON-NLS-1$
		}
		catch (final AOException e1) {
			LOGGER.warning("La configuracion de NSS para macOS X ha fallado: " + e1); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.19")); //$NON-NLS-1$
		}


	}

	private static void installTrustedCertsInAppleKeyChain(final File caCertFile,
			final File sslCertFile, final RestoreConfigPanel console) throws IOException, InterruptedException, KeyChainException {


		byte[] data = null;
		boolean passwordError = false;
		boolean certInstalled = false;
		do {

			// La contrasena solo hace falta si no estamos ya en modo administrador, asi que la primera vez probaremos con una generica
			if (data == null) {
				data = DUMMY;
			}
			else {
				// Solicitamos la contrasena para la instalacion de los certificados
				final String text = passwordError
						? SimpleAfirmaMessages.getString("RestoreConfigMacOSX.30") //$NON-NLS-1$
						: SimpleAfirmaMessages.getString("RestoreConfigMacOSX.29"); //$NON-NLS-1$

				try {
					// Se pone en una linea para evitar que la contrasena se exponda en claro en memoria
					data = new String(AOUIFactory.getPassword(text, console)).getBytes(StandardCharsets.UTF_8);
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
						return;
					}
					continue;
				}
			}

			// Restablecemos el valor
			passwordError = false;

			// Insertamos el certificado raiz
			try {
				installTrustedCertInAppleKeyChain(caCertFile, data, true);
			}
			catch (final SecurityException e) {
				// La contrasena invalida, pero si era el intento de prueba, no lo tendremos en cuenta
				if (!Arrays.equals(DUMMY, data)) {
					passwordError = true;
				}
				continue;
			}
			console.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.25")); //$NON-NLS-1$
			installTrustedCertInAppleKeyChain(sslCertFile, data, false);
			console.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigMacOSX.26")); //$NON-NLS-1$

			certInstalled = true;

		} while(!certInstalled);
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

		LOGGER.info("Se ejecuta el fichero: " + path); //$NON-NLS-1$

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
	 * Agrega al script de ejecuci&oacute;n los comandos para la desinstalaci&oacute;n
	 * del certificado generado del llavero OS X y elimina los links simb&oacute;licos.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero.
	 */
	private static void uninstallRootCAMacOSXKeyStore() throws IOException {
		LOGGER.info("Desinstalamos los certificados y eliminamos los enlaces simbolicos:"); //$NON-NLS-1$
		// Creamos comandos para eliminar enlaces simbolicos de firefox y certificados del llavero
		final String deleteLinks = "ls -ln /usr/local/lib | grep Firefox | awk '{print $9}' | xargs -I {} rm /usr/local/lib/{}"; //$NON-NLS-1$
		final String deleteCaCerts = "security find-certificate -c " + CERT_CN + " -a -Z|grep SHA-1|awk '{ print $NF }' | xargs -I {} security delete-certificate -Z {}"; //$NON-NLS-1$ //$NON-NLS-2$
		final String deleteKsCerts = "security find-certificate -c " + CERT_CN_ROOT + " -a -Z|grep SHA-1|awk '{ print $NF }' | xargs -I {} security delete-certificate -Z {}"; //$NON-NLS-1$ //$NON-NLS-2$
		final StringBuilder sb = new StringBuilder();
		sb.append(deleteLinks);
		sb.append(";"); //$NON-NLS-1$
		sb.append(deleteCaCerts);
		sb.append(";"); //$NON-NLS-1$
		sb.append(deleteKsCerts);
		writeScriptFile(mac_script_path, sb, true);
	}

	/**
	 * Elimina los ficheros de certificado ra&iacute;z y almac&eacute;n SSL del disco
	 * como paso previo a volver a generarlos
	 * @param appDir Directorio en donde se encontran los certificados.
	 * @throws IOException Si hay problemas borrando los ficheros. */
	private static void deleteInstalledCertificates(final File appDir) throws IOException {

		if (checkSSLKeyStoreGenerated(appDir)) {
			final File sslKey = new File(appDir, KS_FILENAME);
			if (!sslKey.delete()) {
				throw new IOException("No puedo eliminar autofirma.pfx"); //$NON-NLS-1$
			}

		}

		if (checkSSLRootCertificateGenerated(appDir)) {
			final File sslRoot = new File(appDir, SSL_CA_CER_FILENAME);
			if (!sslRoot.delete()) {
				throw new IOException("No puedo eliminar AutoFirma_ROOT.cer"); //$NON-NLS-1$
			}

		}
	}

	/**
	 * Desinstala el certificado de CA del almacen de autoridades de confianza de Mozilla Firefox.
	 * @param workingDir Directorio en el que almacenar los ficheros necesarios para la
	 * ejecuci&oacute;n de la funci&oacute;n.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero.
	 */
	private static void uninstallRootCAFirefoxKeyStore(final File workingDir) throws IOException {

		// Generamos script para borrar el almacen certificados firefox
		RestoreConfigFirefox.generateUninstallScriptMac(workingDir, userDirs);
		// Le damos permisos para poder ejecutarlo
		UnixUtils.addExexPermissionsToAllFilesOnDirectory(workingDir);
	}

	/** Escribe un <i>script</i> en un fichero dado.
	 * @param path Ruta donde se escribir&aacute; el <i>script</i>.
	 * @param data Datos a escribir.
	 * @param append <code>true</code> permite contatenar el contenido del fichero con lo que se va a escribir. <code>false</code> el fichero se sobrescribe.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero. */
	static void writeScriptFile(final String path, final StringBuilder data, final boolean append) throws IOException{
		LOGGER.fine("Se escribira en fichero (" + path + ") el siguiente comando:\n" + data.toString()); //$NON-NLS-1$ //$NON-NLS-2$
		final File macScript = new File(path);
		data.append("\n"); //$NON-NLS-1$
		try (final FileOutputStream fout = new FileOutputStream(macScript, append);) {
			fout.write(data.toString().getBytes());
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
					SimpleAfirmaMessages.getString("RestoreAutoFirma.7"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreAutoFirma.9"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}

		int option = JOptionPane.OK_OPTION;
		while (option == JOptionPane.OK_OPTION && isFirefoxOpen()) {

			option = JOptionPane.showConfirmDialog(
					parent,
					SimpleAfirmaMessages.getString("RestoreAutoFirma.12"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreAutoFirma.9"), //$NON-NLS-1$
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
			return checkProcess("Firefox.app"); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.warning("No se pudo completar la deteccion del proceso de Chrome. Se considerara que no esta en ejecucion: " + e); //$NON-NLS-1$
		}
		return false;
	}

	/** Detecta si Chrome est&aacute; abierto.
	 * @return <code>true</code> si Chrome est&aacute; abierto,
	 *         <code>false</code> en caso contrario. */
	private static boolean isChromeOpen() {

		// Comprobamos si esta abierto el proceos de Chrome
		try {
			return checkProcess("Google Chrome.app"); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.warning("No se pudo completar la deteccion del proceso de Chrome. Se considerara que no esta en ejecucion: " + e); //$NON-NLS-1$
		}
		return false;
	}

	private static boolean checkProcess(final String processName) throws IOException {

		// Listamos los procesos abiertos y buscamos uno que contenga una cadena identificativa del proceso
		final ProcessBuilder pBuilder = new ProcessBuilder("ps", "aux"); //$NON-NLS-1$ //$NON-NLS-2$
		final Process ps = pBuilder.start();

		try (
				final InputStream resIs = ps.getInputStream();
				final BufferedReader resReader = new BoundedBufferedReader(
						new InputStreamReader(resIs),
						256, // Maximo 256 lineas de salida
						1024 // Maximo 1024 caracteres por linea
						);
				) {
			String line;
			while ((line = resReader.readLine()) != null) {
				if (line.contains(processName)) {
					return true;
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
}
