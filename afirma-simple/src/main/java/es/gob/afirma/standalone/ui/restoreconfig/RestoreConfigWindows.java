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
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.WinReg;

import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;
import es.gob.afirma.standalone.ui.restoreconfig.CertUtil.CertPack;

/**
 * Clase que contiene la l&oacute;gica para realizar las tareas de restauraci&oacute;n
 * de la configuraci&oacute;n de navegadores para el sistema operativo Windows.
 *
 */
final class RestoreConfigWindows implements RestoreConfig {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String SSL_KEYSTORE_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
	private static final String CA_CERTIFICATE_FILENAME = "Autofirma_ROOT.cer"; //$NON-NLS-1$
	private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$

	private static final String ADMIN_EXECUTOR_BAT = "execute.bat"; //$NON-NLS-1$
	private static final String RESTORE_PROTOCOL_BAT = "afirma_register.bat"; //$NON-NLS-1$

	private static final String REPLACE_EXE_DIR = "$$PATH_DIR_EXE$$"; //$NON-NLS-1$
	private static final String REPLACE_PATH_BAT = "$$PATH_BAT$$"; //$NON-NLS-1$

	/**
     * Caracter de salto de l&iacute;nea para los mensajes de la consola de restauraci&oacute;n
     */
	static final String NEWLINE = System.getProperty("line.separator"); //$NON-NLS-1$

	/**
	 * Ruta de operaci&oacute;n de la aplicaci&oacute;n
	 */
	private final static File appDir = RestoreConfigUtil.getApplicationDirectory();

	@Override
	public void restore(final RestoreConfigPanel configPanel) {

		// Identificamos el directorio de instalacion
		LOGGER.info("Ruta de appDir: " + LoggerUtil.getCleanUserHomePath(appDir.getAbsolutePath())); //$NON-NLS-1$
		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.3", appDir.getAbsolutePath())); //$NON-NLS-1$

		// Comprobamos si se debe configurar Firefox para que use el almacen de confianza del sistema
		final boolean firefoxSecurityRoots = configPanel.firefoxIntegrationCb.isSelected();

		// Verifica si se tiene permisos para escribir en el directorio de instalacion
		boolean usingAlternativeDirectory;
		File workingDirectory;
		if (Files.isWritable(appDir.toPath())) {
			workingDirectory = appDir;
			usingAlternativeDirectory = false;
		} else {
			workingDirectory = DesktopUtil.getWindowsAlternativeAppDir();
			usingAlternativeDirectory = true;
		}

		LOGGER.info("Ruta de trabajo:  " + LoggerUtil.getCleanUserHomePath(workingDirectory.getAbsolutePath())); //$NON-NLS-1$
		if (!workingDirectory.exists()) {
			try {
				workingDirectory.mkdirs();
			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido crear el directorio: " + LoggerUtil.getCleanUserHomePath(workingDirectory.getAbsolutePath())); //$NON-NLS-1$
			}
		}

		final boolean needRebuildCerts = isRebuildCertNeeded(appDir);

		// Regeneramos los certificados que sean necesario (raiz y ssl) y los guardamos en disco
		CertificateFile sslRoot;
		if (needRebuildCerts) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.38")); //$NON-NLS-1$
			try {
				sslRoot = rebuildCertificates(configPanel, workingDirectory);
			}
			catch (final Exception e) {
				LOGGER.log(Level.SEVERE, "No se han podido regenerar los certificados necesarios. No se instalaran en los almacenes de confianza", e); //$NON-NLS-1$
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.33")); //$NON-NLS-1$
				sslRoot = null;
			}
		}
		else {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.39")); //$NON-NLS-1$
			try {
				// Si vamos a trabajar desde un directorio distinto al de instalacion,
				// copiamos los certificados
				if (!appDir.equals(workingDirectory)) {
					copyCerts(appDir, workingDirectory);
				}
				// Cargamos el certificado de CA
				sslRoot = loadRootCertificate(workingDirectory);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se ha podido cargar el certificado de CA del directorio de instalacion. No se instalara en los almacenes de confianza", e); //$NON-NLS-1$
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.4")); //$NON-NLS-1$
				sslRoot = null;
			}
		}

		// Instalacion del certificado raiz en Windows
		if (sslRoot != null) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.18")); //$NON-NLS-1$
			installRootCAWindowsKeystore(configPanel, sslRoot);
		}

		// Instalacion del certificado raiz en Firefox
		if (sslRoot != null) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.13")); //$NON-NLS-1$
			installRootCAMozillaKeystore(configPanel, sslRoot, workingDirectory);
		}

		// Si no se han creado directamente los certificados en el directorio alternativo
		// y este existe, los copiamos ahora
		if (!usingAlternativeDirectory) {
			final File alternativeDir = DesktopUtil.getWindowsAlternativeAppDir();
			if (alternativeDir.exists()) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.36")); //$NON-NLS-1$
				try {
					copyCerts(workingDirectory, alternativeDir);

				} catch (final IOException e) {
					LOGGER.log(Level.WARNING, "No se ha podido copiar el almacen del certificado SSL al directorio alternativo de instalacion", e); //$NON-NLS-1$
					configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.37")); //$NON-NLS-1$
				}
			}
		}

		// Registramos el protocolo afirma
		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.24")); //$NON-NLS-1$
		try {
			restoreProtocolRegistry(appDir.getAbsoluteFile(), workingDirectory.getAbsoluteFile());
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error restaurando los valores del protocolo 'afirma'", e); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.25")); //$NON-NLS-1$
		}

		// Configuramos Firefox para que confie o no en los prestadores dados de alta en el almacen de confianza
		// del sistema

		if (firefoxSecurityRoots) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.41")); //$NON-NLS-1$
		} else {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.42")); //$NON-NLS-1$
		}
		try {
			RestoreConfigFirefox.configureUseSystemTrustStore(firefoxSecurityRoots);
		}
		catch (final MozillaProfileNotFoundException e) {
			LOGGER.info("No se encontraron perfiles de Firefox en los que configurar la confianza en el almacen del sistema"); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.44")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error configurando la confianza de Firefox en el almacen del sistema (activando: " + firefoxSecurityRoots + ")", e); //$NON-NLS-1$ //$NON-NLS-2$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.43")); //$NON-NLS-1$
		}
	}

	/**
	 * Copia los certificado SSL y de CA de un directorio a otro.
	 * @param source Directorio en el que se encuentran los ficheros.
	 * @param target Directorio de destino.
	 * @throws IOException Cuando falla el proceso de copia.
	 */
	private static void copyCerts(final File source, final File target) throws IOException {
		Files.copy(
				new File(source, SSL_KEYSTORE_FILENAME).toPath(),
				new File(target, SSL_KEYSTORE_FILENAME).toPath(),
				StandardCopyOption.REPLACE_EXISTING);
		AOFileUtils.setAllPermissions(new File(target, SSL_KEYSTORE_FILENAME));
		Files.copy(
				new File(source, CA_CERTIFICATE_FILENAME).toPath(),
				new File(target, CA_CERTIFICATE_FILENAME).toPath(),
				StandardCopyOption.REPLACE_EXISTING);
		AOFileUtils.setAllPermissions(new File(target, CA_CERTIFICATE_FILENAME));
	}

	/**
	 * Comprueba si ya existe un almac&eacute;n de certificados generado.
	 *
	 * @param installDir
	 *            Directorio de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un almacen de certificados SSL,
	 *         {@code false} en caso contrario.
	 */
	private static boolean checkSSLKeyStoreGenerated(final File installDir) {
		return new File(installDir, SSL_KEYSTORE_FILENAME).exists();
	}

	/**
	 * Comprueba si ya existe un certificado raiz generado.
	 *
	 * @param installDir
	 *            Directorio de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un certificado raiz .cer, {@code false}
	 *         en caso contrario.
	 */
	private static boolean checkSSLRootCertificateGenerated(final File installDir) {
		return new File(installDir, CA_CERTIFICATE_FILENAME).exists();
	}

	/**
	 * Comprueba si un proceso est&aacute; ejecut&aacute;ndose en Windows
	 * @param process Cadena que identifica al proceso
	 * @return ({@code true}}) Si est&aacute; ejecut&aacute;ndose, ({@code false}}) en caso contrario
	 */
	private static Boolean isProcessRunningWindows(final String process) {

		String line;
		String pidInfo = ""; //$NON-NLS-1$
		Boolean isRunning = Boolean.FALSE;

		Process p;
		try {

			final ProcessBuilder pb = new ProcessBuilder(System.getenv("windir") + "\\system32\\" + "tasklist.exe"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

			p = pb.start();

			try (final BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()))) {

				while ((line = input.readLine()) != null) {
					pidInfo += line;
				}
			}

		} catch (final IOException e) {
			LOGGER.severe("Ha ocurrido un error al ejecutar el comando " + process + " en Windows. " + e.getMessage()); //$NON-NLS-1$ //$NON-NLS-2$
		}

		if (pidInfo.contains(process)) {
			isRunning = Boolean.TRUE;
		}

		return isRunning;
	}

	/**
	 * Pide al usuario que cierre el navegador Mozilla Firefox y no permite continuar hasta que lo hace.
	 * @param parent Componente padre sobre el que mostrar los di&aacute;logos gr&aacute;ficos.
	 * @return Devuelve {@code true} si se cerr&oacute; el navegador, {@code false} en caso contrario.
	 */
	private static boolean closeFirefox(final Component parent) {

		if (isProcessRunningWindows("firefox.exe").booleanValue()) { //$NON-NLS-1$
			JOptionPane.showMessageDialog(
					parent,
					SimpleAfirmaMessages.getString("RestoreApplication.7"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreApplication.9"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}

		int option = JOptionPane.OK_OPTION;
		while (option == JOptionPane.OK_OPTION
				&& isProcessRunningWindows("firefox.exe").booleanValue()) { //$NON-NLS-1$

			option = JOptionPane.showConfirmDialog(
					parent,
					SimpleAfirmaMessages.getString("RestoreApplication.12"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreApplication.9"), //$NON-NLS-1$
					JOptionPane.OK_CANCEL_OPTION,
					JOptionPane.WARNING_MESSAGE);
		}

		return option == JOptionPane.OK_OPTION;
	}

	/** Instala el certificado ra&iacute;z CA de Autofirma
	 * en el almac&eacute;n ra&iacute;z de Windows.
	 * @param configPanel Panel de configuraci&oacute;n con las trazas de ejecuci&oacute;n.
	 * @param certFile El certificado a instalar. */
	private static void installRootCAWindowsKeystore(final RestoreConfigPanel configPanel, final CertificateFile certFile) {

		final KeyStore ks;
		try {
			ks = KeyStore.getInstance("Windows-ROOT"); //$NON-NLS-1$
			ks.load(null, null);
		}
		catch (KeyStoreException | NoSuchAlgorithmException | CertificateException | IOException e) {
			LOGGER.severe("No se ha podido cargar el almacen de certificados de confianza de Windows: " + e); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.20")); //$NON-NLS-1$
			return;
		}

		// Comprobamos si el certificado ya esta instalado en el almacen
		try {
			final Certificate currentCert = ks.getCertificate(RestoreConfigUtil.CERT_ALIAS_BROWSER);
			if (currentCert != null && currentCert.equals(certFile.getCert())) {
				LOGGER.info("El certificado raiz ya se encontraba instalado en el almacen del sistema"); //$NON-NLS-1$
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.26")); //$NON-NLS-1$
				return;
			}
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo comprobar si el certificado ya estaba en el almacen: " + e); //$NON-NLS-1$
		}

		// Antes de la instalacion, intentamos desinstalar cualquier otro certificado con el
		// mismo alias que se encuentre en el almacen
		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.22")); //$NON-NLS-1$
		try {
			while (ks.getCertificate(RestoreConfigUtil.CERT_ALIAS_BROWSER) != null) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.35")); //$NON-NLS-1$
				ks.deleteEntry(RestoreConfigUtil.CERT_ALIAS_BROWSER);
			}
		}
		catch (final KeyStoreException ke) {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.34")); //$NON-NLS-1$
			LOGGER.info("No se ha podido eliminar alguna importacion previa del certificado raiz del almacen de Windows: " + ke.getMessage()); //$NON-NLS-1$
		}

		// Instalamos el certificado
		boolean installed = false;
		do {
			try {
				ks.setCertificateEntry(RestoreConfigUtil.CERT_ALIAS_BROWSER, certFile.getCert());
				installed = true;
			}
			catch (final KeyStoreException e) {
				LOGGER.log(Level.WARNING,
						"No se pudo instalar la CA del certificado SSL para el socket en el almacen de Windows", //$NON-NLS-1$
						e
						);
				final int result = JOptionPane.showConfirmDialog(
					null,
					SimpleAfirmaMessages.getString("RestoreConfigWindows.0"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreConfigWindows.1"), //$NON-NLS-1$
					JOptionPane.OK_CANCEL_OPTION,
					JOptionPane.WARNING_MESSAGE
				);
				if (result == JOptionPane.CANCEL_OPTION) {
					LOGGER.severe("El usuario cancelo la instalacion del certificado SSL para el socket: " + e); //$NON-NLS-1$
					configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.32")); //$NON-NLS-1$
					return;
				}
			}
		}
		while (!installed);
	}

	/** Instala el certificado ra&iacute;z CA de Autofirma
	 * en el almac&eacute;n ra&iacute;z de Mozilla.
	 *  @param configPanel Panel de configuraci&oacute;n con las trazas de ejecuci&oacute;n.
	 *  @param certFile El certificado a instalar.
	 *  @param installDir Directorio de instalaci&oacute;n. */
	private static void installRootCAMozillaKeystore(final RestoreConfigPanel configPanel,
			                                         final CertificateFile certFile,
			                                         final File installDir) {
		try {
			// Obligamos a que se cierre Firefox antes de manipular el certificado en su almacen
			final boolean closed = closeFirefox(configPanel);

			// Si no se ha cerrado el navegador, es muy probable que no se pueda instalar el certificado de confianza,
			// asi que mostramos un mensaje advirtiendolo
			if (!closed) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.45")); //$NON-NLS-1$
			}

			// Es necesario copiar a disco certutil
			RestoreConfigFirefox.copyConfigurationFiles(installDir);

			// certutil no lanza ningun error si hay algun problema a partir de Firefox 50

			// Desinstalamos versiones previas
			LOGGER.info("Desinstalamos el certificado raiz del almacen de Firefox"); //$NON-NLS-1$
			RestoreConfigFirefox.uninstallRootCAMozillaKeyStore(installDir);
			// Vuelvo a instalar lo que habia o el nuevo cer generado
			RestoreConfigFirefox.installRootCAMozillaKeyStore(installDir, certFile.getFile());
			// Elimino certutil tras su uso
			RestoreConfigFirefox.removeConfigurationFiles(installDir);
		}
		catch (final IOException | KeyStoreException e) {
			LOGGER.log(Level.SEVERE, "Error instalando el certificado raiz: " + e, e); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.31", installDir.getAbsolutePath())); //$NON-NLS-1$
		}
		catch (final MozillaProfileNotFoundException e) {
			LOGGER.warning("No se ha encontrado el perfil de Mozilla en Windows: " + e); //$NON-NLS-1$
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.12")); //$NON-NLS-1$
		}
	}

	/**
	 * Restaura los valores del protocolo afirma en el registro de Windows.
	 * Se sobreescriben las distintas subkeys con los valores adecuados.
	 * @param installDir Directorio de instalaci&oacute;n en el que copiar los ficheros que sean necesarios.
	 * @param workingDir Directorio de trabajo dependiendo si se tienen permisos de escritura sobre
	 * el directorio de instalaci&oacute;n.
	 * @throws GeneralSecurityException Cuando no se puede actualizar la entrada del protocolo
	 * "afirma" en el registro.
	 */
	private static void restoreProtocolRegistry(final File installDir, final File workingDir) throws GeneralSecurityException {

		LOGGER.info("Vamos a restaurar el protocolo 'afirma'"); //$NON-NLS-1$

		try {
			// Crear la key "afirma" si no existe
			if (!Advapi32Util.registryKeyExists(WinReg.HKEY_CLASSES_ROOT, "afirma")) { //$NON-NLS-1$
				Advapi32Util.registryCreateKey(WinReg.HKEY_CLASSES_ROOT, "afirma"); //$NON-NLS-1$
			}

			// Sobreescribir los valores correctos
			Advapi32Util.registrySetStringValue(WinReg.HKEY_CLASSES_ROOT, "afirma", "", "URL:Afirma Protocol"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			Advapi32Util.registrySetStringValue(WinReg.HKEY_CLASSES_ROOT, "afirma", "URL Protocol", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

			// Crear la key "afirma\\DefaultIcon"
			if (!Advapi32Util.registryKeyExists(WinReg.HKEY_CLASSES_ROOT, "afirma\\DefaultIcon")) { //$NON-NLS-1$
				Advapi32Util.registryCreateKey(WinReg.HKEY_CLASSES_ROOT, "afirma\\DefaultIcon"); //$NON-NLS-1$
			}
			// Sobreescribir los valores correctos
			Advapi32Util.registrySetStringValue(WinReg.HKEY_CLASSES_ROOT, "afirma\\DefaultIcon", "", installDir + "\\ic_firmar.ico"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

			// Crear la key "afirma\\shell\\open\\command" si no existe
			if (!Advapi32Util.registryKeyExists(WinReg.HKEY_CLASSES_ROOT, "afirma\\shell\\open\\command")) { //$NON-NLS-1$
				Advapi32Util.registryCreateKey(WinReg.HKEY_CLASSES_ROOT, "afirma\\shell\\open\\command"); //$NON-NLS-1$
			}
			// Sobreescribir los valores correctos
			Advapi32Util.registrySetStringValue(WinReg.HKEY_CLASSES_ROOT, "afirma\\shell\\open\\command", "", installDir + "\\Autofirma.exe %1"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

		} catch (final Exception | Error e) {

			LOGGER.warning("No se pudo actualizar el registro con los permisos del usuario, se solicita elevar privilegios: " + e); //$NON-NLS-1$

			// No se pudo actualizar el registro con los permisos del usuario,
			// se usara la biblioteca nativa
			final int result = restoreProtocolRegistryByApp(installDir, workingDir.getAbsolutePath());
			if (result != 0) {
				throw new GeneralSecurityException("No se pudo registrar el protocolo afirma. Codigo de error: " + result); //$NON-NLS-1$
			}
		}

		LOGGER.info("Configurado 'afirma' en registro Windows"); //$NON-NLS-1$
	}

	/**
	 * Comprueba si es necesario volver a generar el almac&eacute;n de claves y/o el certificado para
	 * comunicaci&oacute;n SSL y, en caso afirmativo, los genera.
	 * @param configPanel Panel de configuraci&oacute;n con las trazas de ejecuci&oacute;n.
	 * @param installDir Directorio de instalaci&oacute;n en el que se deben encontrar los certificados SSL.
	 * @return El certificado ra&iacute;z.
	 * @throws IOException Cuando ocurre un error al generar o copiar a disco los certificados.
	 */
	private static CertificateFile rebuildCertificates(final RestoreConfigPanel configPanel, final File installDir) throws IOException {
		// SSLKeystore --> pfx, SSLRoot --> cer
		CertificateFile sslRoot = null;

		// Si no existe el pfx, debo generar el certificado raiz y el certificado SSL
		if (!checkSSLKeyStoreGenerated(installDir)) {

			// Eliminando de disco las versiones previas de los certificados
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.16")); //$NON-NLS-1$
			try {
				deleteCertificatesFromDisk(installDir);
			} catch (final IOException e) {
				configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.17")); //$NON-NLS-1$
				LOGGER.log(Level.SEVERE, "Error al eliminar los certificados SSL anteriores de disco: " + e); //$NON-NLS-1$
			}

			// Generamos los certificados
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.5")); //$NON-NLS-1$
			CertPack certPack;
			try {
				certPack = CertUtil.getCertPackForLocalhostSsl(ConfiguratorUtil.CERT_ALIAS, KS_PASSWORD);
				sslRoot = new CertificateFile(certPack.getCaCertificate());
			}
			catch (final GeneralSecurityException e) {
				LOGGER.log(Level.SEVERE, "No se ha podido generar el certificado SSL", e); //$NON-NLS-1$
				throw new IOException("No se ha podido generar el certificado SSL", e); //$NON-NLS-1$
			}

			// Copiamos los certificados a disco
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.11")); //$NON-NLS-1$
			File sslRootFile = new File(installDir, CA_CERTIFICATE_FILENAME);
			try {
				RestoreConfigUtil.installFile(certPack.getPkcs12(), new File(installDir, SSL_KEYSTORE_FILENAME));
				RestoreConfigUtil.installFile(certPack.getCaCertificate().getEncoded(), sslRootFile);
			}
			catch (final Exception e) {
				LOGGER.log(Level.SEVERE, "No se ha podido guardar en disco los certificados SSL. Los almacenaremos en un directorio alternativo", e); //$NON-NLS-1$
				final File alternativeDir = DesktopUtil.getWindowsAlternativeAppDir();
				if (!alternativeDir.isDirectory() && !alternativeDir.mkdirs()) {
					throw new IOException("No se ha podido guardar en disco los certificados SSL. Error al crear el directorio alternativo"); //$NON-NLS-1$
				}
				try {
					sslRootFile = new File(alternativeDir, CA_CERTIFICATE_FILENAME);
					RestoreConfigUtil.installFile(certPack.getPkcs12(), new File(alternativeDir, SSL_KEYSTORE_FILENAME));
					RestoreConfigUtil.installFile(certPack.getCaCertificate().getEncoded(), sslRootFile);
				}
				catch (final Exception ex) {
					LOGGER.log(Level.SEVERE, "No se ha podido guardar en el directorio alternativo los certificados SSL", e); //$NON-NLS-1$
					throw new IOException("Error guardando en el directorio alternativo los certificados SSL", e); //$NON-NLS-1$
				}
			}
			sslRoot.setFile(sslRootFile);
		}
		// Si solo no existe el certificado raiz, lo extraigo del almacen del certificado SSL
		else if (!checkSSLRootCertificateGenerated(installDir)) {

			// Cargo el pfx y extraigo el certificado raiz
			try (FileInputStream fis = new FileInputStream(new File(installDir, SSL_KEYSTORE_FILENAME))) {
				final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
				ks.load(fis, KS_PASSWORD.toCharArray());
				final Certificate[] chain = ks.getCertificateChain(ConfiguratorUtil.CERT_ALIAS);
				sslRoot = new CertificateFile(chain[chain.length - 1]);
			}
			catch(final Exception e) {
				LOGGER.log(Level.SEVERE, "Error al extraer el certificado raiz del PKCS#12", e); //$NON-NLS-1$
				throw new IOException("Error al generar el certificado SSL", e); //$NON-NLS-1$
			}

			// Copio a disco el certificado raiz
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.11")); //$NON-NLS-1$
			File sslRootFile = new File(installDir, CA_CERTIFICATE_FILENAME);
			try {
				RestoreConfigUtil.installFile(sslRoot.getCert().getEncoded(), sslRootFile);
			}
			catch (final Exception e) {
				LOGGER.log(Level.SEVERE, "No se ha podido guardar en disco el certificado raiz SSL. Lo extraemos a un directorio alternativo", e); //$NON-NLS-1$
				final File alternativeDir = DesktopUtil.getWindowsAlternativeAppDir();
				if (!alternativeDir.isDirectory() && !alternativeDir.mkdirs()) {
					throw new IOException("No se ha podido guardar en disco el certificado raiz SSL. Error al crear el directorio alternativo"); //$NON-NLS-1$
				}
				try {
					RestoreConfigUtil.installFile(sslRoot.getCert().getEncoded(), alternativeDir);
					sslRootFile = new File(alternativeDir, CA_CERTIFICATE_FILENAME);
				}
				catch (final Exception ex) {
					LOGGER.log(Level.SEVERE, "No se ha podido guardar en disco el certificado raiz SSL", e); //$NON-NLS-1$
					throw new IOException("Error guardando en disco el certificado raiz SSL", e); //$NON-NLS-1$
				}
			}
			sslRoot.setFile(sslRootFile);
		}
		// Si existen ambos no hago nada
		else {
			configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreConfigWindows.14")); //$NON-NLS-1$
			final File sslRootFile = new File(installDir, CA_CERTIFICATE_FILENAME);
			sslRoot = new CertificateFile(CertUtil.loadCertificate(sslRootFile));
			sslRoot.setFile(sslRootFile);
		}

		return sslRoot;
	}

	/**
	 * Comprueba si es necesario regenerar los certificados SSL (PFX y certificado de CA).
	 * @param certsDir Directorio en el que deben encontrarse los certificados.
	 * @return {@code true} si no se encuentran ya creados los certificados en el directorio,
	 * {@code false} en caso contrario.
	 */
	private static boolean isRebuildCertNeeded(final File certsDir) {
		return !new File(certsDir, CA_CERTIFICATE_FILENAME).isFile() ||
				!new File(certsDir, SSL_KEYSTORE_FILENAME).isFile();
	}

	private static CertificateFile loadRootCertificate(final File caDir) throws IOException {

		final File caFile = new File(caDir, CA_CERTIFICATE_FILENAME);
		final CertificateFile certFile = new CertificateFile(CertUtil.loadCertificate(caFile));
		certFile.setFile(caFile);

		return certFile;
	}

	/** Elimina los ficheros de certificado ra&iacute;z y almac&eacute;n SSL del disco
	 * como paso previo a volver a generarlos.
	 * @param installDir Ruta del directorio de la aplicaci&oacute;n.
	 * @throws IOException Si hay problemas borrando los ficheros. */
	private static void deleteCertificatesFromDisk(final File installDir) throws IOException {

		if (checkSSLKeyStoreGenerated(installDir)) {

			Files.delete(new File(installDir, SSL_KEYSTORE_FILENAME).toPath());
		}

		if (checkSSLRootCertificateGenerated(installDir)) {

			Files.delete(new File(installDir, CA_CERTIFICATE_FILENAME).toPath());
		}
	}

	/**
	 * Restaura la configuraci&oacute;n del protocolo "afirma" para que se invoque a la
	 * aplicaci&oacute;n. Lo hace a trav&eacute;s de una aplicaci&oacute;n externa preparada
	 * para tal fin.
	 * @param installDir Directorio de instalaci&oacute;n donde se puedan crear los ficheros
	 * @param workingDir Directorio de trabajo donde se puedan crear los ficheros
	 * necesarios para ejecutar la operaci&oacute;n.
	 * @return El valor cero en caso de &eacute;xito, -1 si no se proporciona un par&aacute;metro
	 * o cualquier otro c&oacute;digo de error del sistema.
	 * @see <a href="https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx">https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx)</a>
	 */
	private static int restoreProtocolRegistryByApp(final File installDir, final String workingDir) {

		// Copiamos al directorio de instalacion la aplicacion para restaurar el protocolo
		final File batFile = new File(workingDir, RESTORE_PROTOCOL_BAT);
		try (final FileOutputStream os = new FileOutputStream(batFile);
				final InputStream is = RestoreConfigWindows.class.getResourceAsStream("/windows/" + RESTORE_PROTOCOL_BAT);) { //$NON-NLS-1$
			os.write(AOUtil.getDataFromInputStream(is));
			os.flush();
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo copiar a disco el bat de restauracion. Se abortara su ejecucion", e); //$NON-NLS-1$
			return 2; // ERROR_FILE_NOT_FOUND
		}
		AOFileUtils.setAllPermissions(batFile);

		// Copiamos a disco y completamos el script para ejecutar la aplicacion con
		// permisos de administrador
		final File executorFile = new File(workingDir, ADMIN_EXECUTOR_BAT);
		try (final FileOutputStream os = new FileOutputStream(executorFile);
				final InputStream is = RestoreConfigWindows.class.getResourceAsStream("/windows/" + ADMIN_EXECUTOR_BAT);) { //$NON-NLS-1$
			String batchScript = new String(AOUtil.getDataFromInputStream(is));
			batchScript = batchScript
					.replace(REPLACE_PATH_BAT, batFile.getAbsolutePath().replace("\\", "\\\\")) //$NON-NLS-1$ //$NON-NLS-2$
					.replace(REPLACE_EXE_DIR, installDir.getAbsolutePath().replace("\\", "\\\\")); //$NON-NLS-1$ //$NON-NLS-2$
			os.write(batchScript.getBytes());
			os.flush();
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo copiar a disco el bat para la ejecucion de la restauracion como administrador. Se abortara su ejecucion", e); //$NON-NLS-1$
			return 2; // ERROR_FILE_NOT_FOUND
		}
		AOFileUtils.setAllPermissions(executorFile);

		// Ejecumos el script
		int result = -2;
		try {
			final Process process = new ProcessBuilder(executorFile.getAbsolutePath()).start();
			result = process.waitFor();
			process.destroyForcibly();
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error durante la ejecucion del proceso de restauracion del protocolo \"afirma\"", e); //$NON-NLS-1$
		}

		// Esperamos 1 segundo para poder eliminar los ficheros
		try {
			Thread.sleep(1000);
		}
		catch (final InterruptedException e) {
			// No hacemos nada
		}

		// Eliminamos los ficheros
		try {
			Files.delete(executorFile.toPath());
			Files.delete(batFile.toPath());
		}
		catch (final IOException e) {
			LOGGER.log(Level.WARNING, "No se pudo eliminar el ejecutable para el registro del protocolo \"afirma\"", e); //$NON-NLS-1$
		}

		return result;
	}

	static class CertificateFile {

		private final Certificate cert;
		private File file;

		public CertificateFile(final Certificate cert) {
			this.cert = cert;
		}

		public File getFile() {
			return this.file;
		}

		public void setFile(final File file) {
			this.file = file;
		}

		public Certificate getCert() {
			return this.cert;
		}
	}
}
