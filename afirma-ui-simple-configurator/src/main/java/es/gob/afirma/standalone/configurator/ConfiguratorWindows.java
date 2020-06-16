/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitOption;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.cert.Certificate;
import java.util.HashSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.configurator.CertUtil.CertPack;


/** Configura la instalaci&oacute;n en Windows para la correcta ejecuci&oacute;n de AutoFirma. */
final class ConfiguratorWindows implements Configurator {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String KS_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
	private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$
	private static final String FILE_AUTOFIRMA_ROOT_CERTIFICATE = "AutoFirma_ROOT.cer"; //$NON-NLS-1$

	/** Nombre del usuario por defecto en Windows. Este usuario es el que se usa como base para
	 * crear nuevos usuarios y no se deber&iacute;a tocar. */
	private static String DEFAULT_WINDOWS_USER_NAME = "Default"; //$NON-NLS-1$

	// A partir de la version 57 de Chrome cambia el fichero en el que se guardan los protocol handler
	private static final String CHROME_V56_OR_LOWER_CONFIG_FILE = "AppData/Local/Google/Chrome/User Data/Local State"; //$NON-NLS-1$
	private static final String CHROME_V57_OR_HIGHER_CONFIG_FILE = "AppData/Local/Google/Chrome/User Data/Default/Preferences"; //$NON-NLS-1$

	private final boolean jnlpInstance;
	private final boolean firefoxSecurityRoots;

	public ConfiguratorWindows(final boolean jnlpInstance, final boolean firefoxSecurityRoots) {
		this.jnlpInstance = jnlpInstance;
		this.firefoxSecurityRoots = firefoxSecurityRoots;
	}

	@Override
	public void configure(final Console window) throws IOException, GeneralSecurityException {

		window.print(Messages.getString("ConfiguratorWindows.2")); //$NON-NLS-1$

		final File appDir = getApplicationDirectory(this.jnlpInstance);

		window.print(Messages.getString("ConfiguratorWindows.3") + appDir.getAbsolutePath()); //$NON-NLS-1$

		if (!checkSSLKeyStoreGenerated(appDir, this.jnlpInstance)) {
			window.print(Messages.getString("ConfiguratorWindows.5")); //$NON-NLS-1$
			final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(
				ConfiguratorUtil.CERT_ALIAS,
				KS_PASSWORD
			);

			window.print(Messages.getString("ConfiguratorWindows.11")); //$NON-NLS-1$

			//Generacion del certificado pfx
			ConfiguratorUtil.installFile(
				certPack.getPkcs12(),
				new File(appDir, KS_FILENAME)
			);

			//Generacion del certificado raiz .cer
			ConfiguratorUtil.installFile(
					certPack.getCaCertificate().getEncoded(),
					new File(appDir, FILE_AUTOFIRMA_ROOT_CERTIFICATE));

			window.print(Messages.getString("ConfiguratorWindows.9")); //$NON-NLS-1$
			try {
				ConfiguratorFirefoxWindows.installCACertOnMozillaKeyStores(appDir, window);
			}
			catch(final MozillaProfileNotFoundException e) {
				window.print(Messages.getString("ConfiguratorWindows.12") + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}

			if (this.jnlpInstance) {
				JOptionPane.showMessageDialog(window.getParentComponent(), Messages.getString("ConfiguratorWindows.17")); //$NON-NLS-1$
				window.print(Messages.getString("ConfiguratorWindows.6")); //$NON-NLS-1$
				importCARootOnWindowsKeyStore(certPack.getCaCertificate(), CertUtil.ROOT_CERTIFICATE_PRINCIPAL);
			}


			if (this.firefoxSecurityRoots) {
				window.print(Messages.getString("ConfiguratorWindows.22")); //$NON-NLS-1$
				try {
					ConfiguratorFirefoxWindows.configureUseSystemTrustStore(true, window);
				} catch (final MozillaProfileNotFoundException e) {
					window.print(Messages.getString("ConfiguratorWindows.21") + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}
		else {
			window.print(Messages.getString("ConfiguratorWindows.14")); //$NON-NLS-1$
		}

		// Si no se ha cargado mediante JNLP, registramos el protocolo para Google Chrome
		if (!this.jnlpInstance) {
			configureChrome(window, true);
		}

		window.print(Messages.getString("ConfiguratorWindows.8")); //$NON-NLS-1$
	}

	/** Recupera el directorio de la aplicaci&oacute;n, que podr&aacute; variar
	 * seg&uacute;n si est&aacute; instalada o si se trata de un despliegue JNLP.
	 * @param jnlpDeployment <code>true</code> si se trata de un despliegue JNLP,
	 *                       <code>false</code> en caso contrario.
	 * @return Directorio en el que se almacenan los recursos de la aplicaci&oacute;n.
	 */
	private static File getApplicationDirectory(final boolean jnlpDeployment) {

		// Si el despliegue es JNLP seleccionamos un directorio de Windows en el que
		// se puedan crear los ficheros sin permisos especiales
		if (jnlpDeployment) {
			final String commonDir = System.getenv("ALLUSERSPROFILE"); //$NON-NLS-1$
			final File appDir = new File (commonDir, "AutoFirma"); //$NON-NLS-1$
			if (appDir.isDirectory() || appDir.mkdirs()) {
				return appDir;
			}
			return new File(System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
		}

		return ConfiguratorUtil.getApplicationDirectory();
	}

	/** Comprueba si ya existe un almac&eacute;n de certificados generado.
	 * En caso del despliegue JNLP, primero consulta en el directorio por
	 * defecto de instalaci&oacute;n de AutoFirma en el sistema, y despu&eacute;s
	 * el directorio indicado.
	 * @param appDir Directorio de la aplicaci&oacute;n.
	 * @param jnlpDeployment Indica si la funci&oacute;n se ejecuta desde un aplicativo desplegado mediante JNLP.
	 * @return {@code true} si ya existe un almacen de certificados SSL, {@code false} en caso contrario. */
	private static boolean checkSSLKeyStoreGenerated(final File appDir,
			                                         final boolean jnlpDeployment) {

		/*
		// En caso de tratarse de un despliegue JNLP, probamos primeramente
		// a buscar el almacen en el directorio de instalacion por defecto
		// de AutoFirma para evitar tener que volver a generarlo
		if (jnlpDeployment) {
			final File[] defaultDirs = getDefaultInstallationDirs();
			for (final File defaultDir : defaultDirs) {
				if (new File(defaultDir, KS_FILENAME).exists()) {
					return true;
				}
			}
		}
		*/
		return new File(appDir, KS_FILENAME).exists();
	}

//	/**
//	 * Devuelve el listado de directorios en el que com&uacute;nmente se instala
//	 * AutoFirma en este sistema operativo.
//	 * @return Listado de directorios.
//	 */
//	private static File[] getDefaultInstallationDirs() {
//
//		final List<File> dirs = new ArrayList<>();
//		final String subPath = "AutoFirma" + File.separator + "AutoFirma"; //$NON-NLS-1$ //$NON-NLS-2$
//		final String basePath = System.getenv("PROGRAMFILES"); //$NON-NLS-1$
//		if (basePath != null) {
//			dirs.add(new File(basePath, subPath));
//			if (basePath.endsWith(" (x86)")) { //$NON-NLS-1$
//				dirs.add(new File(basePath.substring(0,  basePath.lastIndexOf(" (x86)")), subPath)); //$NON-NLS-1$
//			}
//			else {
//				dirs.add(new File(basePath + " (x86)", subPath)); //$NON-NLS-1$
//			}
//		}
//		return dirs.toArray(new File[dirs.size()]);
//	}

	@Override
	public void uninstall(final Console console) {

		LOGGER.info("Desinstalamos el certificado raiz del almacen de Windows"); //$NON-NLS-1$
		uninstallRootCAWindowsKeyStore(CertUtil.ROOT_CERTIFICATE_PRINCIPAL);

		LOGGER.info("Desinstalamos el certificado raiz del almacen de Firefox"); //$NON-NLS-1$
		ConfiguratorFirefoxWindows.uninstallRootCAMozillaKeyStore(
				getApplicationDirectory(this.jnlpInstance),
				console);

		// Insertamos el protocolo afirma en el fichero de configuracion de Google Chrome
		configureChrome(null, false);

		// Eliminamos el directorio alternativo en el que se instalan los certificados SSL
		// durante el proceso de restauracion de la instalacion
		final File alternativeDir = getWindowsAlternativeAppDir();
		if (alternativeDir.isDirectory()) {
			try {
				Files.walkFileTree(
						alternativeDir.toPath(),
						new HashSet<FileVisitOption>(),
						Integer.MAX_VALUE,
						new SimpleFileVisitor<Path>() {
							@Override
							public FileVisitResult visitFile(final Path file, final BasicFileAttributes attr) {
								try {
									Files.delete(file);
								}
								catch (final Exception e) {
									LOGGER.warning("No se pudo eliminar el fichero: " + file); //$NON-NLS-1$
								}
								return FileVisitResult.CONTINUE;
							}
							@Override
							public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) {
								try {
									Files.delete(dir);
								} catch (final IOException e) {
									LOGGER.warning("No se pudo eliminar el directorio: " + dir); //$NON-NLS-1$
								}
								return FileVisitResult.CONTINUE;
							}
						});
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "No se ha podido eliminar por completo el directorio alternativo para el certificado SSL", e); //$NON-NLS-1$
			}
		}

		// No es necesario eliminar nada mas porque el proceso de desinstalacion de Windows
		// eliminara el directorio de aplicacion con todo su contenido
	}

	/**
	 * Configura el protocolo "afirma" en Chrome para todos los usuarios de Windows.
	 * @param window Consola de salida.
	 * @param installing Indica si se debe configurar ({@code true}) o desconfigurar
	 * ({@code false}) el protocolo "afirma" en Chrome.
	 */
	private static void configureChrome(final Console window, final boolean installing) {

		if (window != null && installing) {
			window.print(Messages.getString("ConfiguratorWindows.16")); //$NON-NLS-1$
		}

		final File usersDir = new File(System.getProperty("user.home")).getParentFile(); //$NON-NLS-1$
		for (final File userDir : usersDir.listFiles()) {
			if (userDir.isDirectory() && !DEFAULT_WINDOWS_USER_NAME.equalsIgnoreCase(userDir.getName())) {
				try {
					final File chromeConfigFileV56OrLower = new File(userDir, CHROME_V56_OR_LOWER_CONFIG_FILE);
					if (chromeConfigFileV56OrLower.isFile() && chromeConfigFileV56OrLower.canWrite()) {
						String config;
						try (final FileInputStream fis = new FileInputStream(chromeConfigFileV56OrLower)) {
							config = new String(AOUtil.getDataFromInputStream(fis), StandardCharsets.UTF_8);
						}
						config = config.replace("\"afirma\":false,", ""); //$NON-NLS-1$ //$NON-NLS-2$
						config = config.replace("\"afirma\":false", ""); //$NON-NLS-1$ //$NON-NLS-2$

						if (installing) {
							config = config.replace("\"protocol_handler\":{\"excluded_schemes\":{", //$NON-NLS-1$
									"\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false,"); //$NON-NLS-1$
							config = config.replace("\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false,}", //$NON-NLS-1$
									"\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false}"); //$NON-NLS-1$
							// En caso de que Google Chrome este recien instalado no encontrara cadena a reemplazar,
							// por lo que directamente insertaremos la cadena nosotros en el lugar correspondiente
							if(!config.contains("excluded_schemes")) { //$NON-NLS-1$
								config = config.replaceAll("last_active_profiles([^,]*),", //$NON-NLS-1$
										"last_active_profiles$1,\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false}},"); //$NON-NLS-1$
							}
						}
						else {
							// Elimina la sintaxis que define los protocolos de confianza si no existe ninguno.
							config = config.replace("\"protocol_handler\":{\"excluded_schemes\":{}},", ""); //$NON-NLS-1$ //$NON-NLS-2$
						}
						try (final FileOutputStream fos = new FileOutputStream(chromeConfigFileV56OrLower)) {
							fos.write(config.getBytes(StandardCharsets.UTF_8));
						}
					}
					final File chromeConfigFileV57OrHigher = new File(userDir, CHROME_V57_OR_HIGHER_CONFIG_FILE);
					if (chromeConfigFileV57OrHigher.isFile() && chromeConfigFileV57OrHigher.canWrite()) {
						String config;
						try (final FileInputStream fis = new FileInputStream(chromeConfigFileV57OrHigher)) {
							config = new String(AOUtil.getDataFromInputStream(fis), StandardCharsets.UTF_8);
						}
						config = config.replace("\"afirma\":false,", ""); //$NON-NLS-1$ //$NON-NLS-2$
						config = config.replace("\"afirma\":false", ""); //$NON-NLS-1$ //$NON-NLS-2$
						if (installing) {
							config = config.replace("\"protocol_handler\":{\"excluded_schemes\":{", //$NON-NLS-1$
									"\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false,"); //$NON-NLS-1$
							config = config.replace("\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false,}", //$NON-NLS-1$
									"\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false}"); //$NON-NLS-1$
							// En caso de que Google Chrome este recien instalado no encontrara cadena a reemplazar,
							// pero si se ha insertado en el fichero Local State del paso anterior se configurara automaticamente
							// en el Default/Preferences cuando se invoque el protocolo afirma
						}
						try (final FileOutputStream fos = new FileOutputStream(chromeConfigFileV57OrHigher)) {
							fos.write(config.getBytes(StandardCharsets.UTF_8));
						}
					}
				}
				catch (final Exception e) {
					if (window != null) {
						window.print(String.format(Messages.getString("ConfiguratorWindows.15"), userDir.getName())); //$NON-NLS-1$
					}
					LOGGER.warning("No se pudo configurar Chrome para el usuario " + userDir + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}
	}

	/**
	 * Instala el certificado SSL en el almac&eacute;n de autoridades de confianza Windows
	 * sin necesidad de tener permisos de administrador.
	 * @param cert Certificado a instalar.
	 * @param principal Principal del certificado.
	 * @throws GeneralSecurityException Cuando no se tiene acceso al almac&eacute;n de
	 * autoridades de certificaci&oacute;n.
	 * @throws IOException Cuando no se pudo cargar el almac&eacute;n.
	 */
	private static void importCARootOnWindowsKeyStore(final Certificate cert, final String principal) throws GeneralSecurityException, IOException {

		final KeyStore ks = KeyStore.getInstance("Windows-ROOT"); //$NON-NLS-1$
		ks.load(null,  null);

		boolean installed = false;
		boolean cancelled = false;
		do {
			try {
				ks.setCertificateEntry(principal, cert);
				installed = true;
			}
			catch (final KeyStoreException e) {
				LOGGER.warning(
						"No se pudo instalar la CA del certificado SSL para el socket en el almacen de Windows: " + e //$NON-NLS-1$
						);
				final int result = JOptionPane.showConfirmDialog(
						null,
						Messages.getString("ConfiguratorWindows.0"), //$NON-NLS-1$
						Messages.getString("ConfiguratorWindows.1"), //$NON-NLS-1$
						JOptionPane.OK_CANCEL_OPTION,
						JOptionPane.WARNING_MESSAGE
						);
				if (result == JOptionPane.CANCEL_OPTION) {
					cancelled = true;
					LOGGER.severe("El usuario cancelo la instalacion del certificado SSL para el socket: " + e); //$NON-NLS-1$
				}
			}
		}
		while (!installed && !cancelled);
	}

	/**
	 * Desinstala un certificado del almacen de confianza de Windows sin
	 * necesidad de tener permisos de administrador.
	 * @param principal Principal del certificado a eliminar.
	 */
	private static void uninstallRootCAWindowsKeyStore(final String principal) {
		try {
			final KeyStore ks = KeyStore.getInstance("Windows-ROOT"); //$NON-NLS-1$
			ks.load(null,  null);
			ks.deleteEntry(principal);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo desinstalar el certificado SSL raiz del almacen de Windows: " + e); //$NON-NLS-1$
		}
	}

	/**
	 * Recupera el directorio de instalaci&oacute;n alternativo en los sistemas Windows.
	 * @return Directorio de instalaci&oacute;n.
	 */
	private static File getWindowsAlternativeAppDir() {
		final String commonDir = System.getenv("ALLUSERSPROFILE"); //$NON-NLS-1$
		return new File (commonDir, "AutoFirma"); //$NON-NLS-1$
	}
}
