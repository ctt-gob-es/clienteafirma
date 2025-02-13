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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.security.KeyStore;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;
import es.gob.afirma.standalone.ui.restoreconfig.CertUtil.CertPack;

/**
 * Clase que contiene la l&oacute;gica para realizar las tareas de restauraci&oacute;n
 * de la configuraci&oacute;n de navegadores para el sistema operativo Linux.
 */
final class RestoreConfigLinux implements RestoreConfig {

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String KS_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
    private static final String FILE_AUTOFIRMA_CERTIFICATE = "Autofirma_ROOT.cer"; //$NON-NLS-1$
    private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$
    private static final String PROTOCOL_HANDLER_CONFIG_FILE = "autofirma.js"; //$NON-NLS-1$
    private static final String DEFAULT_PROTOCOL_HANDLER_CONFIG_DIR = "/etc/firefox/pref"; //$NON-NLS-1$
    static final String EXPORT_PATH = "export PATH=$PATH:"; //$NON-NLS-1$
    static final String EXPORT_LD_LIBRARY ="export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"; //$NON-NLS-1$

	@Override
	public void restore(final RestoreConfigPanel configPanel) {

		final File appDir = RestoreConfigUtil.getApplicationDirectory();

		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.3", appDir.getAbsolutePath())); //$NON-NLS-1$

		// Verifica si se tiene permisos para escribir en el directorio de instalacion
		// y establece como directorio de trabajo otro distinto en caso de no tenerlos
		File workingDir;
		if(Files.isWritable(appDir.toPath())) {
			workingDir = appDir;
		} else {
			try {
				workingDir = getLinuxAlternativeAppDir();
			} catch (final IOException e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.1")); //$NON-NLS-1$
				LOGGER.log(Level.SEVERE, "No se puede utilizar el directorio alternativo de trabajo", e); //$NON-NLS-1$
				return;
			}
		}

		LOGGER.info("Directorio de trabajo: " + LoggerUtil.getCleanUserHomePath(workingDir.getAbsolutePath())); //$NON-NLS-1$

		List<String> usersDir = null;
		try {
			usersDir = getSystemUsersHomes();
		} catch (final IOException e) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.6")); //$NON-NLS-1$
			LOGGER.log(Level.WARNING, "No se puede utilizar el directorio alternativo de trabajo"); //$NON-NLS-1$
		}

		// Se restaura la instalacion de los certificados SSL
		if (usersDir != null) {
			try {
				restoreSslCertificates(appDir, workingDir, usersDir, configPanel);
			}
			catch (final AOCancelledOperationException e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.19")); //$NON-NLS-1$
				return;
			}
		}

		// Se restaura el registro del protocolo afirma
		// Es necesario tener permisos de administrador para modificar el directorio.
		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.15")); //$NON-NLS-1$
		try {
			restoreProtocolHandler(workingDir);
		} catch (final RuntimeException | IOException e) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.16")); //$NON-NLS-1$
			LOGGER.log(Level.WARNING, "Error en la restauracion del protocolo afirma", e); //$NON-NLS-1$
		}
	}

	private static void restoreSslCertificates(final File appDir, final File workingDir, final List<String> usersDir, final RestoreConfigPanel configPanel) {

		File rootCertFile = null;

		// Si no existe el PKCS#12 con el certificado SSL, se genera un certificado de CA
		// y un certificado SSL a partir de el
		if (!checkSSLKeyStoreGenerated(appDir)) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.5")); //$NON-NLS-1$

			// Elimino los certificados que pueda llegar a encontrar para volver a generarlos
			deleteInstalledCertificates(appDir);

			CertPack certPack;
			try {
				certPack = CertUtil.getCertPackForLocalhostSsl(ConfiguratorUtil.CERT_ALIAS, KS_PASSWORD);
			} catch (final Exception e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.2")); //$NON-NLS-1$
				LOGGER.log(Level.SEVERE, "Error al generar los certificados SSL", e); //$NON-NLS-1$
				return;
			}

			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.11")); //$NON-NLS-1$

			try {
				// Generacion del certificado pfx
				RestoreConfigUtil.installFile(certPack.getPkcs12(),
						new File(workingDir, KS_FILENAME));

				// Generacion del certificado raiz .cer
				rootCertFile = new File(workingDir, FILE_AUTOFIRMA_CERTIFICATE);
				RestoreConfigUtil.installFile(certPack.getCaCertificate().getEncoded(),
						rootCertFile);
			}
			catch (final IOException e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.4")); //$NON-NLS-1$
				LOGGER.log(Level.SEVERE, "Error al copiar los certificados SSL a disco", e); //$NON-NLS-1$
				return;
			}
			catch (final Exception e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.4")); //$NON-NLS-1$
				LOGGER.log(Level.SEVERE, "Error al extraer los certificados SSL para copiarlos a disco", e); //$NON-NLS-1$
				return;
			}
		}
		// Si existe el certificado SSL pero no el de CA, se obtiene este del almacen
		// del certificado SSL
		else if (!checkSSLRootCertificateGenerated(appDir)) {
			try {
				Certificate[] sslCertChain = null;
				try (FileInputStream fis = new FileInputStream(new File(appDir, KS_FILENAME))) {
					final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
					ks.load(fis, KS_PASSWORD.toCharArray());
					sslCertChain = ks.getCertificateChain(ks.aliases().nextElement());
				}

				// Generacion del certificado raiz .cer
				rootCertFile = new File(workingDir, FILE_AUTOFIRMA_CERTIFICATE);
				RestoreConfigUtil.installFile(sslCertChain[sslCertChain.length - 1].getEncoded(),
						rootCertFile);
			}
			catch (final Exception e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.4")); //$NON-NLS-1$
				LOGGER.log(Level.SEVERE, "Error al copiar los certificados SSL a disco", e); //$NON-NLS-1$
				return;
			}
		}
		// Si existen ambos no hacemos nada
		else {
			rootCertFile = new File(appDir, FILE_AUTOFIRMA_CERTIFICATE);
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.14")); //$NON-NLS-1$
		}

		// Copiamos a disco CertUtil
		try {
			RestoreConfigFirefox.copyConfigurationFiles(workingDir);
		} catch (final IOException e) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.17")); //$NON-NLS-1$
			LOGGER.log(Level.SEVERE, "No se pudo copiar certUtil al directorio de trabajo. Se omitira la instalacion de los certificados: " + e, e); //$NON-NLS-1$
			return;
		}

		LOGGER.info("Se va a instalar el certificado CA raiz en Google Chrome"); //$NON-NLS-1$
		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.9")); //$NON-NLS-1$
		closeChrome(configPanel);
		try {
			RestoreConfigFirefox.installRootCAChromeKeyStore(workingDir, rootCertFile, usersDir);
		}
		catch (final Exception e) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.10")); //$NON-NLS-1$
			LOGGER.log(Level.WARNING, "Error al instalar el certificado de confianza en el almacen del sistema: " + e, e); //$NON-NLS-1$
		}

		LOGGER.info("Se va a instalar el certificado CA raiz en Mozilla Firefox"); //$NON-NLS-1$
		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.13")); //$NON-NLS-1$

		// Obligamos a que se cierre Firefox antes de manipular el certificado en su almacen
		final boolean closed = closeFirefox(configPanel);

		// Si no se ha cerrado el navegador, es muy probable que no se pueda instalar el certificado de confianza,
		// asi que mostramos un mensaje advirtiendolo
		if (!closed) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.45")); //$NON-NLS-1$
		}

		// Desinstalamos previamente los certificados que haya actualmente
		RestoreConfigFirefox.uninstallRootCAMozillaKeyStore(workingDir);
		try {
			RestoreConfigFirefox.installRootCAMozillaKeyStore(workingDir, rootCertFile, usersDir);
		} catch (final MozillaProfileNotFoundException e) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.12")); //$NON-NLS-1$
			LOGGER.warning("Error al obtener los perfiles de usuario de Mozilla Firefox: " + e); //$NON-NLS-1$
		} catch (final Exception e) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigLinux.18")); //$NON-NLS-1$
			LOGGER.log(Level.WARNING, "Error al instalar el certificado de confianza en el almacen de Firefox: " + e, e); //$NON-NLS-1$
		}
	}

	/** Obtiene los directorios de usuario sobre los que se tengan permiso. Si tenemos permisos
	 * de administrador, devolver&aacute; el listado completo de directorios, si no devolver&aacute;
	 * s&oacute;lo el del usuario local.
	 * @return Listado con directorios de usuarios.
     * @throws IOException Cuando no se puede obtener el listado de directorios. */
	private static List<String> getSystemUsersHomes() throws IOException {

		boolean searchAllUser = true;
		try {
			final Process p = executeProcess("id", "-u"); //$NON-NLS-1$ //$NON-NLS-2$
			p.waitFor();

			final byte[] out = new byte[100];
			try (final InputStream resIs = p.getInputStream();) {
				final int r = resIs.read(out);
				if (r <= 0 || !"0".equals(new String(out, 0, r))) { //$NON-NLS-1$
					searchAllUser = false;
				}
			}
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo determinar si el usuario tiene permisos de administracion: " + e); //$NON-NLS-1$
		}

		// Si no somos administradores, operamos solo sobre el directorio del usuario local
		if (!searchAllUser) {
			final List<String> userDirs = new ArrayList<>();
			try {
				userDirs.add(System.getProperty("user.home")); //$NON-NLS-1$
			}
			catch (final Exception e) {
				throw new IOException("No se pudo identificar el directorio del usuario", e); //$NON-NLS-1$
			}
			return userDirs;
		}

        // Comando para sacar los usuarios del sistema
        final String[] command = {
				"cut", //$NON-NLS-1$
				"-d:", //$NON-NLS-1$
				"-f6", //$NON-NLS-1$
				"/etc/passwd" //$NON-NLS-1$
				};

		try {
			final Process process = executeProcess(command);

			String line;
			// arraylist con todos los directorios de usuario
			final List<String> usersDir = new ArrayList<>();
			try (
					final InputStream resIs = process.getInputStream();
					final BufferedReader resReader = new BoundedBufferedReader(
							new InputStreamReader(resIs),
							2048, // Maximo 256 lineas de salida (256 perfiles)
							2048 // Maximo 2048 caracteres por linea
							);
					) {
				while ((line = resReader.readLine()) != null) {
					if(line.toLowerCase().contains("home/") && !usersDir.contains(line)) { //$NON-NLS-1$
						usersDir.add(line);
					}
				}
			}
			return usersDir;
		}
		catch (final Exception e) {
			LOGGER.severe("Error al obtener el listado de directorios de usuarios del sistema: " + e); //$NON-NLS-1$
			throw new IOException("No se pudo obtener el listado de directorios de usuarios del sistema", e); //$NON-NLS-1$
		}
	}

    /** Comprueba si ya existe un almac&eacute;n de certificados generado.
     * @param appConfigDir Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
     * @return {@code true} si ya existe un almac&eacute;n de certificados SSL, {@code false} en caso contrario. */
    private static boolean checkSSLKeyStoreGenerated(final File appConfigDir) {
        return new File(appConfigDir, KS_FILENAME).exists();
    }

    /** Comprueba si ya existe un certificado ra&iacute;z generado.
	 * @param certsDir Directorio en el que deben encontrarse los certificados.
	 * @return {@code true} si ya existe un certificado ra&iacute;z .cer, {@code false} en caso contrario. */
	private static boolean checkSSLRootCertificateGenerated(final File certsDir) {
		return new File(certsDir, FILE_AUTOFIRMA_CERTIFICATE).exists();
	}

	/** Elimina los ficheros de certificado ra&iacute;z y almac&eacute;n SSL del disco
	 * como paso previo a volver a generarlos.
	 * @param certsDir Ruta del directorio con los certificados. */
	private static void deleteInstalledCertificates(final File certsDir) {

		if (checkSSLKeyStoreGenerated(certsDir) && !new File(certsDir, KS_FILENAME).delete()) {
			LOGGER.warning("No se ha podido eliminar de disco el almacen de claves SSL de Autofirma"); //$NON-NLS-1$
		}

		if (checkSSLRootCertificateGenerated(certsDir) && !new File(certsDir, FILE_AUTOFIRMA_CERTIFICATE).delete()) {
			LOGGER.warning("No se ha podido eliminar el certificado de disco raiz SSL de Autofirma"); //$NON-NLS-1$
		}

	}

	/**
	 * Pide al usuario que cierre el navegador Mozilla Firefox y no permite continuar hasta que lo hace.
	 * @param parent Componente padre sobre el que mostrar los di&aacute;logos gr&aacute;ficos.
	 * @return Devuelve {@code true} si se cerr&oacute; el navegador, {@code false} en caso contrario.
	 */
	private static boolean closeFirefox(final Component parent) {

		if (isProcessRunningLinux("/usr/lib/firefox/firefox").booleanValue()) { //$NON-NLS-1$
			JOptionPane.showMessageDialog(
					parent,
					SimpleAfirmaMessages.getString("RestoreApplication.7"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreApplication.9"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}

		int option = JOptionPane.OK_OPTION;
		while (option == JOptionPane.OK_OPTION
				&& isProcessRunningLinux("/usr/lib/firefox/firefox").booleanValue()) { //$NON-NLS-1$

			option = JOptionPane.showConfirmDialog(
					parent,
					SimpleAfirmaMessages.getString("RestoreApplication.12"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreApplication.9"), //$NON-NLS-1$
					JOptionPane.OK_CANCEL_OPTION,
					JOptionPane.WARNING_MESSAGE);
		}

		return option == JOptionPane.OK_OPTION;
	}

	/**
	 * Pide al usuario que cierre el navegador Google Chrome y no permite continuar hasta que lo hace.
	 * @param parent Componente padre sobre el que mostrar los di&aacute;logos gr&aacute;ficos.
	 */
	private static void closeChrome(final Component parent) {

		if (isProcessRunningLinux("/opt/google/chrome/chrome").booleanValue()) { //$NON-NLS-1$
			JOptionPane.showMessageDialog(
					parent,
					SimpleAfirmaMessages.getString("RestoreApplication.8"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreApplication.9"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}

		int option = JOptionPane.OK_OPTION;
		while (option == JOptionPane.OK_OPTION
				&& isProcessRunningLinux("/opt/google/chrome/chrome").booleanValue()) { //$NON-NLS-1$
			option = JOptionPane.showConfirmDialog(
					parent,
					SimpleAfirmaMessages.getString("RestoreApplication.11"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreApplication.9"), //$NON-NLS-1$
					JOptionPane.OK_CANCEL_OPTION);
		}
	}

	/**
	 * Determina si un proceso est&aacute; corriendo en Linux
	 * @param process Nombre del proceso a buscar mediante comando ps
	 * @return {@code true} si el proceso est&aacute; corriendo {@code false} en caso contrario.
	 */
	private static Boolean isProcessRunningLinux(final String process) {

		String line;
		String pidInfo =""; //$NON-NLS-1$
		Boolean isRunning = Boolean.FALSE;

		Process p;
		try {

			final String[] commands = { "/bin/bash", "-c", "ps -aux"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

			p = executeProcess(commands);

			try (final BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()))) {

				while ((line = input.readLine()) != null) {
					pidInfo += line;
				}
			}

		} catch (final IOException e) {
			LOGGER.severe("Error al detectar si el proceso " + process + " esta activo: " + e.getMessage()); //$NON-NLS-1$ //$NON-NLS-2$
		}

		if(pidInfo.contains(process))
		{
		    isRunning = Boolean.TRUE;
		}

		return isRunning;
	}

	/** Restaura la configuraci&oacute;n del protocolo afirma en Linux
	 * @param workingDir Directorio en el que se puedan copiar ficheros.
	 * @throws IOException Si hay problemas leyendo o escribiendo datos. */
	private static void restoreProtocolHandler(final File workingDir) throws IOException {

		final byte[] configFileContent;
		try (InputStream is = RestoreConfigLinux.class.getResourceAsStream("/linux/" + PROTOCOL_HANDLER_CONFIG_FILE)) { //$NON-NLS-1$
			configFileContent = AOUtil.getDataFromInputStream(is);
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido obtener el contenido del fichero interno " + //$NON-NLS-1$
					PROTOCOL_HANDLER_CONFIG_FILE + ": " + e); //$NON-NLS-1$
			throw new IOException("No se ha podido obtener el contenido del fichero interno " + PROTOCOL_HANDLER_CONFIG_FILE, e); //$NON-NLS-1$
		}

		final File configFile = new File(workingDir, PROTOCOL_HANDLER_CONFIG_FILE);

		try {
			RestoreConfigUtil.installFile(configFileContent, configFile);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo copiar a disco el fichero de configuracion " + PROTOCOL_HANDLER_CONFIG_FILE); //$NON-NLS-1$
			throw new IOException("No se pudo copiar a disco el fichero de configuracion ", e); //$NON-NLS-1$
		}

		// Obtenemos los directorios de bibliotecas e identificamos donde esta instalado
		// firefox para copiar alli el fichero de definicion del protocolo afirmaa
		final String libraryPath = System.getProperty("java.library.path"); //$NON-NLS-1$
		final List<String> paths = new ArrayList<>();
		for (final String path : libraryPath.split(File.pathSeparator)) {
			paths.add(path + File.separator + "firefox" + File.separator + "defaults" + //$NON-NLS-1$ //$NON-NLS-2$
					File.separator + "pref"); //$NON-NLS-1$
		}
		paths.add(DEFAULT_PROTOCOL_HANDLER_CONFIG_DIR);

		// Creamos un script para el copiado del fichero de configuracion del protocolo en
		// todos los directorios en donde encontremos las preferencias de firefox
		final StringBuilder scriptContent = new StringBuilder();
		scriptContent.append("#!/bin/sh\n"); //$NON-NLS-1$
		for (final String firefoxPreferencesPath : paths) {
			if (new File(firefoxPreferencesPath).isDirectory()) {
				scriptContent.append("cp -f \"") .append(configFile.getAbsolutePath()).append("\" \"") //$NON-NLS-1$ //$NON-NLS-2$
				.append(firefoxPreferencesPath + File.separator + PROTOCOL_HANDLER_CONFIG_FILE).append("\"\n"); //$NON-NLS-1$
			}
		}

		// Agregamos la orden de eliminar el fichero de de configuracion original
		scriptContent.append("rm \"").append(configFile.getAbsolutePath()).append("\"\n");  //$NON-NLS-1$//$NON-NLS-2$

		// Guardamos el script en disco
		final File scriptFile = new File(workingDir, "restoreprotocol.sh"); //$NON-NLS-1$
		try (final FileOutputStream fout = new FileOutputStream(scriptFile)) {
			fout.write(scriptContent.toString().getBytes(StandardCharsets.UTF_8));
		}

		// Ejecutamos el script con permisos de administrador y comprobamos su correcta ejecucion
		int result;
		Process process;
		try {
			executeProcess("chmod", "755", scriptFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$
			process = executeProcess("pkexec", scriptFile.getAbsolutePath()); //$NON-NLS-1$
			result = process.waitFor();
		} catch (IOException | InterruptedException e) {
			throw new RuntimeException("Error en la ejecucion del script de restauracion del protocolo", e); //$NON-NLS-1$
		}
		if (result != 0) {
			final byte[] errorResult = AOUtil.getDataFromInputStream(process.getErrorStream());
			LOGGER.warning("Mensaje de error devuelto por el script: " + new String(errorResult)); //$NON-NLS-1$
			throw new RuntimeException("El script de restauracion del protocolo devolvio un resultado incorrecto"); //$NON-NLS-1$
		}

		// Eliminamos la copia del fichero en el directorio de configuracion
		try {
			process = executeProcess("rm", scriptFile.getAbsolutePath()); //$NON-NLS-1$
		} catch (final IOException e) {
			LOGGER.fine("No se pudo eliminar la copia del fichero " + configFile.getName() + " del directorio de trabajo"); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}


	/**
	 * Obtiene un directorio en el que almacenar los ficheros de la aplicaci&oacute;n.
	 * @return Directorio de aplicaci&oacute;n.
	 * @throws IOException Cuando no se puede obtener el directorio alternativo.
	 */
	static File getLinuxAlternativeAppDir() throws IOException {
		String userHome = null;
		try {
			userHome = System.getProperty("user.home"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new IOException("No se ha podido identificar el directorio del usuario para almacenar los ficheros de instalacion", e); //$NON-NLS-1$
		}
		if (userHome == null) {
			throw new IOException("No se encuentra definido el directorio del usuario"); //$NON-NLS-1$
		}

		final File appDir = new File(userHome, ".afirma/Autofirma"); //$NON-NLS-1$
		if (!appDir.isDirectory() && !appDir.mkdirs()) {
			throw new IOException("No ha podido crearse el directorio para los ficheros de aplicacion"); //$NON-NLS-1$
		}
		return appDir;
	}

	static Process executeProcess(final String... command) throws IOException {

		if (LOGGER.isLoggable(Level.INFO)) {
			final StringBuilder buffer = new StringBuilder();
			for (final String particle : command) {
				buffer.append(particle).append(" "); //$NON-NLS-1$
			}
			LOGGER.info("Ejecutamos el comando:\n" + buffer.toString()); //$NON-NLS-1$
		}
		return new ProcessBuilder(command).start();
	}
}
