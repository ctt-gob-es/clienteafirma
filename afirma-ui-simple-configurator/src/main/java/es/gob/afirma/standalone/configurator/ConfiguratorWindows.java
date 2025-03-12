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
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.cert.Certificate;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.HashSet;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.standalone.configurator.CertUtil.CertPack;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.manager.PluginsManager;


/** Configura la instalaci&oacute;n en Windows para la correcta ejecuci&oacute;n de Autofirma. */
final class ConfiguratorWindows implements Configurator {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String KS_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
	private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$
	private static final String CA_CERT_FILENAME = "Autofirma_ROOT.cer"; //$NON-NLS-1$

	/**
	 * N&uacute;mero de veces que se intentar&aacute; desinstalar el certificado del almac&eacute;n
	 * de Firefox cuando sea necesario hacerlo.
	 */
	private static final int UNINSTALL_CERT_RETRIES = 3;

	private final boolean jnlpInstance;
	private final boolean firefoxSecurityRoots;
	private final String certificatePath;
	private final String keyStorePath;

	public ConfiguratorWindows(final boolean jnlpInstance, final boolean firefoxSecurityRoots, final String certificatePath, final String keyStorePath) {
		this.jnlpInstance = jnlpInstance;
		this.firefoxSecurityRoots = firefoxSecurityRoots;
		this.certificatePath = certificatePath;
		this.keyStorePath = keyStorePath;
	}

	@Override
	public void configure(final Console window) throws IOException, GeneralSecurityException {

		window.print(Messages.getString("ConfiguratorWindows.2")); //$NON-NLS-1$

		final File appDir = getApplicationDirectory(this.jnlpInstance);

		window.print(Messages.getString("ConfiguratorWindows.3") + appDir.getAbsolutePath()); //$NON-NLS-1$

			if (!checkSSLKeyStoreGenerated(appDir)) {

				// Generacion del certificado pfx
				if (!this.keyStorePath.isEmpty() && !this.certificatePath.isEmpty()){

					window.print(Messages.getString("ConfiguratorWindows.25")); //$NON-NLS-1$

					// Copiamos al directorio el almacen con la clave SSL
					try {
						copyFile(
								new File(this.keyStorePath),
								new File (appDir.getAbsolutePath(), KS_FILENAME));
					}
					catch (final Exception e) {
						window.print(Messages.getString("ConfiguratorWindows.23") + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					}

					// Copiamos al directorio el certificado de CA
					try {
						final File certFile = new File(this.certificatePath);
						copyFile(
								certFile,
								new File (appDir.getAbsolutePath(), CA_CERT_FILENAME));
					}
					catch (final Exception e) {
						window.print(Messages.getString("ConfiguratorWindows.24") + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					}
				} else {

					window.print(Messages.getString("ConfiguratorWindows.5")); //$NON-NLS-1$

					// Generamos un certificado de CA y un certificado SSL a partir de el
					final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(
						ConfiguratorUtil.CERT_ALIAS,
						KS_PASSWORD
					);

					window.print(Messages.getString("ConfiguratorWindows.11")); //$NON-NLS-1$

					// Guardamos el PKCS#12 con la clave SSL generada
					ConfiguratorUtil.installFile(
						certPack.getPkcs12(),
						new File(appDir, KS_FILENAME)
					);

					// Guardamos el certificado de CA generado
					ConfiguratorUtil.installFile(
							certPack.getCaCertificate().getEncoded(),
							new File(appDir, CA_CERT_FILENAME));

					// En los despliegues JNLP nunca se proporcionan certificados. Ademas, en estos casos, el
					// instalador no lo habra instalado en el almacen de Windows, asi que lo tendremos que
					// hacer ahora
					if (this.jnlpInstance) {
						JOptionPane.showMessageDialog(window.getParentComponent(), Messages.getString("ConfiguratorWindows.17")); //$NON-NLS-1$
						window.print(Messages.getString("ConfiguratorWindows.6")); //$NON-NLS-1$
						importCARootOnWindowsKeyStore(certPack.getCaCertificate(), CertUtil.ROOT_CERTIFICATE_PRINCIPAL);
					}
				}

				// Intentamos desinstalar cualquier certificado nuestro que pueda haber antes de instalar el nuevo
				window.print(Messages.getString("ConfiguratorWindows.26")); //$NON-NLS-1$
				ConfiguratorFirefoxWindows.uninstallRootCAMozillaKeyStore(appDir, window, UNINSTALL_CERT_RETRIES);

				// Instalamos el certificado de CA en el almacen de confianza de Firefox
				window.print(Messages.getString("ConfiguratorWindows.9")); //$NON-NLS-1$
				try {
					ConfiguratorFirefoxWindows.installCACertOnMozillaKeyStores(appDir, window);
				}
				catch(final MozillaProfileNotFoundException e) {
					window.print(Messages.getString("ConfiguratorWindows.12") + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
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
			final File appDir = new File (commonDir, "Autofirma"); //$NON-NLS-1$
			if (appDir.isDirectory() || appDir.mkdirs()) {
				return appDir;
			}
			return new File(System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
		}

		return ConfiguratorUtil.getApplicationDirectory();
	}

	@Override
	public File getAplicationDirectory() {
		return getApplicationDirectory(false);
	}

	@Override
	public File getAlternativeApplicationDirectory() {
		final String commonDir = System.getenv("ALLUSERSPROFILE"); //$NON-NLS-1$
		return new File (commonDir, "Autofirma"); //$NON-NLS-1$
	}

	/** Comprueba si ya existe un almac&eacute;n de certificados generado.
	 * @param appDir Directorio de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un almacen de certificados SSL, {@code false} en caso contrario. */
	private static boolean checkSSLKeyStoreGenerated(final File appDir) {
		return new File(appDir, KS_FILENAME).exists();
	}

	@Override
	public void uninstall(final Console console, final PluginsManager pluginsManager) {

		LOGGER.info("Desinstalamos el certificado raiz del almacen de Windows"); //$NON-NLS-1$
		uninstallRootCAWindowsKeyStore();

		LOGGER.info("Desinstalamos el certificado raiz del almacen de Firefox"); //$NON-NLS-1$
		ConfiguratorFirefoxWindows.uninstallRootCAMozillaKeyStore(
				getApplicationDirectory(this.jnlpInstance),
				console,
				UNINSTALL_CERT_RETRIES);

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

		// Eliminamos el directorio alternativo en el que se instalan los certificados SSL
		// durante el proceso de restauracion de la instalacion
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
									LOGGER.warning("No se pudo eliminar el fichero: " + LoggerUtil.getCleanUserHomePath(file.toAbsolutePath().toString())); //$NON-NLS-1$
								}
								return FileVisitResult.CONTINUE;
							}
							@Override
							public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) {
								try {
									Files.delete(dir);
								} catch (final IOException e) {
									LOGGER.warning("No se pudo eliminar el directorio: " + LoggerUtil.getCleanUserHomePath(dir.toAbsolutePath().toString())); //$NON-NLS-1$
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
	 * Desinstala el certificado de confianza del almac&eacute;n de confianza de Windows sin
	 * necesidad de tener permisos de administrador. Si se encuentra el certificado en el directorio
	 * de instalaci&oacute;n se eliminara el certificado cuyo alias sea igual al CN de ese
	 * certificado. Si no, se eliminara el certificado con el alias por defecto.
	 */
	private static void uninstallRootCAWindowsKeyStore() {

		KeyStore windowsKs;
		try {
			windowsKs = KeyStore.getInstance("Windows-ROOT"); //$NON-NLS-1$
			windowsKs.load(null,  null);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo acceder al almacen de confianza de Windows: " + e); //$NON-NLS-1$
			return;
		}

		String certAlias;
		final File caCertFile = new File(getApplicationDirectory(false), CA_CERT_FILENAME);
		if (caCertFile.isFile() && caCertFile.canRead()) {
			try (InputStream certInputStream = new FileInputStream(caCertFile)) {
				final CertificateFactory certFactory = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$
				final X509Certificate caCert = (X509Certificate) certFactory.generateCertificate(certInputStream);
				certAlias = windowsKs.getCertificateAlias(caCert);

				if (certAlias != null) {
					LOGGER.info("Se ha encontrado el certificado con el numero de serie: " + caCert.getSerialNumber()); //$NON-NLS-1$
				}
				else {
					certAlias = CertUtil.ROOT_CERTIFICATE_PRINCIPAL;
					LOGGER.info("No se encontro el certificado de CA del directorio de instalacion en el almacen"); //$NON-NLS-1$
				}

			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING,
						"No se pudo cargar el certificado de CA para identificar cual eliminar del almacen de confianza. Se eliminara en base al alias por defecto: " //$NON-NLS-1$
								+ CertUtil.ROOT_CERTIFICATE_PRINCIPAL, e);
				certAlias = CertUtil.ROOT_CERTIFICATE_PRINCIPAL;
			}
		}
		else {
			certAlias = CertUtil.ROOT_CERTIFICATE_PRINCIPAL;
		}

		// Si el certificado esta en el almacen, se elimina
		try {
			if (windowsKs.containsAlias(certAlias)) {
				windowsKs.deleteEntry(certAlias);
			}
			else {
				LOGGER.info("El certificado de CA parece ya haber sido eliminado del almacen"); //$NON-NLS-1$
			}
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo desinstalar el certificado SSL raiz del almacen de confianza de Windows: " + e); //$NON-NLS-1$
		}
	}

	/**
	 * Copia un fichero sobreescribiendo si es necesario.
	 * @param sourceFile Fichero de origen.
	 * @param targetFile Fichero destino.
	 */
	private static void copyFile(final File sourceFile, final File targetFile) {
		try (InputStream in = new FileInputStream(sourceFile);
				OutputStream out = new FileOutputStream(targetFile);) {
			int len;
			final byte[] buf = new byte[1024];
			while ((len = in.read(buf)) > 0) {
				out.write(buf, 0, len);
			}
		} catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Error al copiar el fichero", e); //$NON-NLS-1$
		}
	}
}
