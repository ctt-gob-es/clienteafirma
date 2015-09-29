package es.gob.afirma.standalone.configurator;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.cert.Certificate;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;

/**
 * Configura la instalaci&oacute;n en Windows para la correcta ejecuci&oacute;n de AutoFirma.
 */
public class ConfiguratorWindows implements Configurator {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String AFIRMA_DIR = ".afirma/AutoFirma"; //$NON-NLS-1$
	private static final String RESOURCE_BASE = "/windows/"; //$NON-NLS-1$
	private static final String POWERSHELL_EXE = "powershell"; //$NON-NLS-1$
	private static final String CERTUTIL_EXE = "certutil.exe"; //$NON-NLS-1$

	private static final String FILE_AUTOFIRMA_CERTIFICATE = "autofirma.cer"; //$NON-NLS-1$
	private static final String FILE_PS_PREPARATE_SCRIPT = "prepare.ps1"; //$NON-NLS-1$
	private static final String FILE_PS_GENERATE_SCRIPT = "generate.ps1"; //$NON-NLS-1$
	private static final String FILE_PS_GENERATE_KEYS_SCRIPT = "New-SelfSignedCertificateEx.ps1"; //$NON-NLS-1$
	private static final String FILE_PREPARED = "prepared"; //$NON-NLS-1$
	private static final String FILE_CERTUTIL = "certutil.zip"; //$NON-NLS-1$
	private static final String DIR_CERTUTIL = "certutil"; //$NON-NLS-1$

	private static final String KS_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
	private static final char[] KS_PASSWORD = "654321".toCharArray(); //$NON-NLS-1$
	private static final String CERT_ALIAS = "SocketAutoFirma"; //$NON-NLS-1$
	private static final String ROOT_CA_ALIAS = "SocketAutoFirma Root CA"; //$NON-NLS-1$

	private static final String REPLACE_KS_FILENAME = "%%KS_FILENAME%%"; //$NON-NLS-1$
	private static final String REPLACE_KS_PASSWORD = "%%KS_PASSWORD%%"; //$NON-NLS-1$
	private static final String REPLACE_CERT_ALIAS = "%%CERT_ALIAS%%"; //$NON-NLS-1$

	private static final int WAITING_PERIOD = 2000;

	@Override
	public void configure(final ConfiguratorConsole window) throws IOException, ConfigurationException, GeneralSecurityException {

		window.print(Messages.getString("ConfiguratorWindows.2")); //$NON-NLS-1$

		final File appConfigDir = getAppConfigDir();

		window.print(Messages.getString("ConfiguratorWindows.3") + appConfigDir.getAbsolutePath()); //$NON-NLS-1$

		window.print(Messages.getString("ConfiguratorWindows.4")); //$NON-NLS-1$

		copyConfigurationFiles(appConfigDir);

		if (!checkSSLKeyStoreGenerated(appConfigDir)) {
			window.print(Messages.getString("ConfiguratorWindows.5")); //$NON-NLS-1$

			executePowerShellScript(appConfigDir);

			window.print(Messages.getString("ConfiguratorWindows.6")); //$NON-NLS-1$

			importCARootOnWindowsKeyStore(appConfigDir);

			final File firefoxProfilesDir = getFirefoxProfilesDir();
			if (firefoxProfilesDir != null) {
				window.print(Messages.getString("ConfiguratorWindows.9")); //$NON-NLS-1$

				importCARootOnFirefoxKeyStore(appConfigDir, firefoxProfilesDir);
			}
		}

		window.print(Messages.getString("ConfiguratorWindows.7")); //$NON-NLS-1$

		removeConfigurationFiles(appConfigDir);

		window.print(Messages.getString("ConfiguratorWindows.8")); //$NON-NLS-1$

		cleanWindowsMyKeyStore();
	}

	private static File getAppConfigDir() {

		final File appConfigDir = new File(Platform.getUserHome(), AFIRMA_DIR);
		if (!appConfigDir.exists()) {
			appConfigDir.mkdirs();
		}
		return appConfigDir;
	}

	private static void copyConfigurationFiles(final File appConfigDir) throws IOException {

		final File psPreparateScriptFile = new File(appConfigDir, FILE_PS_PREPARATE_SCRIPT);
		final File psGenerateScriptFile = new File(appConfigDir, FILE_PS_GENERATE_KEYS_SCRIPT);
		final File psConfigScriptFile = new File(appConfigDir, FILE_PS_GENERATE_SCRIPT);

		/// Copiamos los scripts PowerShell
		installResource(RESOURCE_BASE + FILE_PS_PREPARATE_SCRIPT, psPreparateScriptFile);
		installResource(RESOURCE_BASE + FILE_PS_GENERATE_KEYS_SCRIPT, psGenerateScriptFile);

		// Script para la ejecutar la generacion del certificado SSL
		final String[][] replaces = new String[][] {
			{REPLACE_KS_FILENAME, KS_FILENAME},
			{REPLACE_KS_PASSWORD, new String(KS_PASSWORD)},
			{REPLACE_CERT_ALIAS, CERT_ALIAS}
		};
		installResourceWithReplaces(RESOURCE_BASE + FILE_PS_GENERATE_SCRIPT, psConfigScriptFile, replaces);

		uncompressResource(RESOURCE_BASE + FILE_CERTUTIL, appConfigDir);
	}

	/**
	 * Copia un fichero de recurso al disco.
	 * @param resource Ruta del recurso.
	 * @param outDir Directorio local.
	 * @throws IOException Cuando ocurre un error durante la copia.
	 */
	private static void installResource(final String resource, final File outDir) throws IOException {
		int n;
		final byte[] buffer = new byte[1024];
		try (final InputStream configScriptIs = ConfiguratorWindows.class.getResourceAsStream(resource);
				final FileOutputStream configScriptOs = new FileOutputStream(outDir);) {
			while ((n = configScriptIs.read(buffer)) > 0) {
				configScriptOs.write(buffer, 0, n);
			}
			configScriptOs.flush();
		}
	}

	/**
	 * Copia un fichero de recurso al disco, sustituyendo en este las cadenas indicada.
	 * @param resource Ruta del recurso.
	 * @param outDir Directorio local.
	 * @param replaces Array de cadenas en donde elemento contiene otros 2: una cadena a sustituir y aquella por la que sustituirla.
	 * @throws IOException Cuando ocurre un error durante la copia.
	 */
	private static void installResourceWithReplaces(final String resource, final File outDir, final String[][] replaces) throws IOException {
		int n;
		String resourceText;
		final byte[] buffer = new byte[1024];
		try (final InputStream configScriptIs = ConfiguratorWindows.class.getResourceAsStream(resource);
				final ByteArrayOutputStream tempOs = new ByteArrayOutputStream();) {
			while ((n = configScriptIs.read(buffer)) > 0) {
				tempOs.write(buffer, 0, n);
			}

			resourceText = new String(tempOs.toByteArray());
			for (final String[] replacePair : replaces) {
				resourceText = resourceText.replace(replacePair[0], replacePair[1]);
			}
		}

		try (final FileOutputStream configScriptOs = new FileOutputStream(outDir);) {
			configScriptOs.write(resourceText.getBytes());
		}
	}

	/**
	 * Descomprime un fichero ZIP de recurso al disco.
	 * @param resource Ruta del recurso ZIP.
	 * @param outDir Directorio local en el que descomprimir.
	 * @throws IOException Cuando ocurre un error al descomprimir.
	 */
	private static void uncompressResource(final String resource, final File outDir) throws IOException {
		int n;
		ZipEntry entry;
		final byte[] buffer = new byte[1024];
		try (final ZipInputStream zipIs = new ZipInputStream(ConfiguratorWindows.class.getResourceAsStream(resource));) {
			while ((entry = zipIs.getNextEntry()) != null) {
				if (entry.isDirectory()) {
					new File(outDir, entry.getName()).mkdirs();
				}
				else {
					try (final FileOutputStream outFis = new FileOutputStream(new File(outDir, entry.getName()));) {
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
	 * Comprueba si ya existe un almac&eacute;n de certificados generado.
	 * @param appConfigDir Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un almacen de certificados SSL, {@code false} en caso contrario.
	 */
	private static boolean checkSSLKeyStoreGenerated(final File appConfigDir) {
		return new File(appConfigDir, KS_FILENAME).exists();
	}

	/**
	 * Ejecuta el script PowerShell para la generaci&oacute;n del almac&eacute;n de claves SSL.
	 * @throws IOException Cuando ocurre un error
	 * @throws ConfigurationException
	 */
	private static void executePowerShellScript(final File appConfigDir) throws IOException, ConfigurationException {

		// Confirmamos que se han copiado los ficheros necesarios
		final File psPreparateScriptFile = new File(appConfigDir, FILE_PS_PREPARATE_SCRIPT);
		if (!psPreparateScriptFile.exists() || !psPreparateScriptFile.isFile() || !psPreparateScriptFile.canRead()) {
			throw new IOException("No se encuentra o no se puede leer el script para la preparacion inicial: " + psPreparateScriptFile.getAbsolutePath()); //$NON-NLS-1$
		}

		final File psGenerateScriptFile = new File(appConfigDir, FILE_PS_GENERATE_SCRIPT);
		if (!psGenerateScriptFile.exists() || !psGenerateScriptFile.isFile() || !psGenerateScriptFile.canRead()) {
			throw new IOException("No se encuentra o no se puede leer el script para la configuracion de los sockets de AutoFirma: " + psGenerateScriptFile.getAbsolutePath()); //$NON-NLS-1$
		}

		LOGGER.info("Habilitamos el uso de scripts PowerShell"); //$NON-NLS-1$

		// Repetimos el desbloqueo de los scripts hasta conseguirlo
		prepareExecutionsScripts(appConfigDir);

		LOGGER.info("Ejecucion de scripts habilitada. Lanzamos el script de generacion del par de claves SSL"); //$NON-NLS-1$

		new Thread(new GenerateCertificateRunnable(appConfigDir)).start();

		LOGGER.info("Esperamos hastya que se hayan creado las claves SSL"); //$NON-NLS-1$

		waitToSSLKeyStore(appConfigDir);

		if (!checkSSLKeyStoreGenerated(appConfigDir)) {
			throw new ConfigurationException("No se ha genero el almacen de claves tras la ejecucion script"); //$NON-NLS-1$
		}

		restrictExecutionsScripts(appConfigDir);

	}

	/**
	 * Habilita la ejecuci&oacute;n de scripts y se queda esperando hasta que pueda comprobarse
	 * que ha sido as&iacute;.
	 * @param appConfigDir Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
	 */
	private static void prepareExecutionsScripts(final File appConfigDir) {
		do {
			new Thread(new PreparateScriptsRunnable(appConfigDir)).start();

			try {
				Thread.sleep(WAITING_PERIOD);
			} catch (final InterruptedException e) {
				LOGGER.warning("No se puede ejecutar una espera. Se abandona la espera de la habilitacion de scripts para evitar bloqueos: " + e); //$NON-NLS-1$
				break;
			}

		} while (!checkPreparateScripts(appConfigDir));
	}

	/**
	 * Comprueba si ya se ha desbloqueado la ejecuci&oacute;n de scripts.
	 * @param appConfigDir Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
	 * @return {@code true} si ya se ha desbloqueado la ejecuci&oacute;n de scripts, {@code false} en caso contrario.
	 */
	private static boolean checkPreparateScripts(final File appConfigDir) {
		// Si se ha creado el fichero FILE_PREPARED mediante la ejecucion de script, es que los scripts funcionan
		return new File(appConfigDir, FILE_PREPARED).exists();
	}

	/**
	 * Realiza una espera activa hasta que se detecte que se ha creado el almac&eacute;n de certificados SSL.
	 * @param appConfigDir Directorio de configuraci&oacute;n.
	 */
	private static void waitToSSLKeyStore(final File appConfigDir) {
		do {
			try {
				Thread.sleep(WAITING_PERIOD);
			} catch (final InterruptedException e) {
				LOGGER.warning("No se puede ejecutar una espera. Se abandona la espera de la generacion de las claves SSL para evitar bloqueos: " + e); //$NON-NLS-1$
				break;
			}
		} while(!new File(appConfigDir, KS_FILENAME).exists());
	}

	/**
	 * Elimina los ficheros generados durante el proceso de configuraci&oacute;n.
	 * @param appConfigDir Directorio con los ficheros de configuraci&oacute;n.
	 */
	private static void removeConfigurationFiles(final File appConfigDir) {

		if (!appConfigDir.exists()) {
			return;
		}

		deleteFile(new File(appConfigDir, FILE_PS_PREPARATE_SCRIPT));
		deleteFile(new File(appConfigDir, FILE_PS_GENERATE_SCRIPT));
		deleteFile(new File(appConfigDir, FILE_PS_GENERATE_KEYS_SCRIPT));
		deleteFile(new File(appConfigDir, FILE_AUTOFIRMA_CERTIFICATE));
		deleteFile(new File(appConfigDir, FILE_PREPARED));
		deleteDir(new File(appConfigDir, DIR_CERTUTIL));
	}

	/**
	 * Elimina un directorio con todo su contenido.
	 * @param targetDir Directorio a eliminar.
	 */
	private static void deleteDir(final File targetDir) {
		try {
			Files.walkFileTree(targetDir.toPath(), new SimpleFileVisitor<Path>() {
		         @Override
		         public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
		             Files.delete(file);
		             return FileVisitResult.CONTINUE;
		         }
		         @Override
		         public FileVisitResult postVisitDirectory(final Path dir, final IOException e) throws IOException {
		             if (e != null) {
		            	 throw e;
		             }
		             Files.delete(dir);
	                 return FileVisitResult.CONTINUE;
		         }
		     });

		}
		catch (final IOException e) {
			LOGGER.warning("No se pudo borrar el directorio " + targetDir.getAbsolutePath() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	private static void deleteFile(final File file) {
		try {
			Files.deleteIfExists(file.toPath());
		}
		catch (final IOException e) {
			LOGGER.warning("No se pudo borrar tras su uso el fichero " + file.getAbsolutePath()); //$NON-NLS-1$
		}
	}

	private static void importCARootOnWindowsKeyStore(final File appConfigDir) throws GeneralSecurityException, IOException {

		final KeyStore p12Ks;
		try (final FileInputStream fis = new FileInputStream(new File(appConfigDir, KS_FILENAME))) {
			p12Ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
			p12Ks.load(fis, KS_PASSWORD);
		}

		final Certificate cert = p12Ks.getCertificate(p12Ks.aliases().nextElement());

		final KeyStore ks = KeyStore.getInstance("Windows-ROOT"); //$NON-NLS-1$
		ks.load(null,  null);

		boolean installed = false;
		boolean cancelled = false;
		do {
			try {
				ks.setCertificateEntry(ROOT_CA_ALIAS, cert);
				installed = true;
			}
			catch (final KeyStoreException e) {
				LOGGER.warning("No se pudo instalar la CA del certificado SSL para el socket en el almacen de Windows: " + e); //$NON-NLS-1$
				final int result = JOptionPane.showConfirmDialog(
						null,
						Messages.getString("ConfiguratorWindows.0"), //$NON-NLS-1$
						Messages.getString("ConfiguratorWindows.1"), //$NON-NLS-1$
						JOptionPane.OK_CANCEL_OPTION,
						JOptionPane.WARNING_MESSAGE);
				if (result == JOptionPane.CANCEL_OPTION) {
					cancelled = true;
					LOGGER.severe("El usuario cancelo la instalacion del certificado SSL para el socket: " + e); //$NON-NLS-1$
				}
			}
		}
		while (!installed && !cancelled);
	}

	private static File getFirefoxProfilesDir() {
		File profilesDir;
		try {
			profilesDir = new File(MozillaKeyStoreUtilities.getMozillaUserProfileDirectory()).getParentFile();
		}
		catch (final Exception e) {
			LOGGER.warning("No se encontro el directorio de perfiles de Mozilla Firefox: " + e); //$NON-NLS-1$
			profilesDir = null;
		}
		return profilesDir;
	}

	private static void importCARootOnFirefoxKeyStore(final File appConfigDir, final File profilesDir) {

		boolean installed = false;
		boolean cancelled = false;
		do {
			try {
				final KeyStore p12Ks;
				try (final FileInputStream fis = new FileInputStream(new File(appConfigDir, KS_FILENAME))) {
					p12Ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
					p12Ks.load(fis, KS_PASSWORD);
				}

				// Exportamos el certificado de confianza
				final Certificate cert = p12Ks.getCertificate(p12Ks.aliases().nextElement());
				try (final FileOutputStream fos = new FileOutputStream(new File(appConfigDir, FILE_AUTOFIRMA_CERTIFICATE));) {
					fos.write(cert.getEncoded());
				}

				// Usamos CertUtil para instalar el certificado en Firefox
				executeCertUtilToImport(appConfigDir, profilesDir);
				installed = true;
			}
			catch (final Exception e) {
				LOGGER.warning("No se pudo instalar la CA del certificado SSL para el socket en el almacen de Firefox: " + e); //$NON-NLS-1$
				final int result = JOptionPane.showConfirmDialog(
						null,
						Messages.getString("ConfiguratorWindows.10"), //$NON-NLS-1$
						Messages.getString("ConfiguratorWindows.1"), //$NON-NLS-1$
						JOptionPane.OK_CANCEL_OPTION,
						JOptionPane.WARNING_MESSAGE);
				if (result == JOptionPane.CANCEL_OPTION) {
					cancelled = true;
					LOGGER.severe("El usuario cancelo la instalacion del certificado SSL para el socket en Firefox: " + e); //$NON-NLS-1$
				}
			}
		} while (!installed && !cancelled);
	}

	/**
	 * Ejecuta el script PowerShell para la generaci&oacute;n del almac&eacute;n de claves SSL.
	 * @param appConfigDir Directorio de configuraci&oacute;n de AutoFirma.
	 * @param profilesDir Directorio de perfiles de Mozilla Firefox.
	 * @throws IOException Cuando ocurre un error
	 * @throws GeneralSecurityException Cuando ocurre un error en la inserci&oacute;n del certificado en el KeyStore.
	 */
	private static void executeCertUtilToImport(final File appConfigDir, final File profilesDir) throws IOException, GeneralSecurityException {

		final File certutilFile = new File(appConfigDir, DIR_CERTUTIL + File.separator + CERTUTIL_EXE);

		if (!certutilFile.exists() || !certutilFile.isFile() || !certutilFile.canExecute()) {
			throw new IOException("No se encuentra o no se puede leer el ejecutable para la instalacion en Firefox"); //$NON-NLS-1$
		}

		// Obtenemos todos los directorios de perfil de Firefox del usuario
		boolean error = false;
		for (final File profileDir : profilesDir.listFiles()) {
			if (!profileDir.isDirectory()) {
				continue;
			}

			final String[] certutilCommands = new String[] {
					certutilFile.getAbsolutePath(),
					"-A", //$NON-NLS-1$
					"-d", //$NON-NLS-1$
					profileDir.getAbsolutePath(),
					"-i", //$NON-NLS-1$
					new File(appConfigDir, FILE_AUTOFIRMA_CERTIFICATE).getAbsolutePath(),
					"-n", //$NON-NLS-1$
					"\"" + CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$
					"-t", //$NON-NLS-1$
					"\"C,,\"" //$NON-NLS-1$
			};


			final Process process = new ProcessBuilder(certutilCommands).start();

			// Cuando se instala correctamente no hay salida de ningun tipo, asi que se interpreta
			// cualquier salida como un error
			String line;
			try (final InputStream resIs = process.getInputStream();
					final BufferedReader resReader = new BufferedReader(new InputStreamReader(resIs));) {
				while ((line = resReader.readLine()) != null) {
					error = true;
					LOGGER.severe(line);
				}
			}

			try (final InputStream errIs = process.getErrorStream();
					final BufferedReader errReader = new BufferedReader(new InputStreamReader(errIs));) {
				while ((line = errReader.readLine()) != null) {
					error = true;
					LOGGER.severe(line);
				}
			}
		}

		if (error) {
			throw new KeyStoreException("Error la instalacion del certificado de CA en alguno de los perfiles de usuario de Firefox"); //$NON-NLS-1$
		}
	}

	private static void cleanWindowsMyKeyStore() throws GeneralSecurityException, IOException {

		final KeyStore ks = KeyStore.getInstance("Windows-MY"); //$NON-NLS-1$
		ks.load(null,  null);
		ks.deleteEntry(CERT_ALIAS);
	}

	@Override
	public void uninstall() {

		LOGGER.info("Desinstalamos el certificado raiz del almacen de Windows"); //$NON-NLS-1$

		uninstallRootCAWindowsKeyStore();

		LOGGER.info("Desinstalamos el certificado raiz del almacen de Firefox"); //$NON-NLS-1$

		uninstallRootCAMozillaKeyStore();

		final File appConfigDir = getAppConfigDir();

		LOGGER.info("Borramos: " + new File(appConfigDir, KS_FILENAME).getAbsolutePath()); //$NON-NLS-1$

		deleteFile(new File(appConfigDir, KS_FILENAME));

		LOGGER.info("Borramos: " + appConfigDir.getAbsolutePath()); //$NON-NLS-1$

		deleteDir(appConfigDir);
	}

	private static void uninstallRootCAWindowsKeyStore() {
		try {
			final KeyStore ks = KeyStore.getInstance("Windows-ROOT"); //$NON-NLS-1$
			ks.load(null,  null);
			ks.deleteEntry(ROOT_CA_ALIAS);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo desinstalar el certificado SSL raiz del almacen de Windows: " + e); //$NON-NLS-1$
		}
	}

	private static void uninstallRootCAMozillaKeyStore() {

		final File appConfigDir = getAppConfigDir();

		try {
		uncompressResource(RESOURCE_BASE + FILE_CERTUTIL, appConfigDir);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo descomprimir certutil para la desinstalacion del certificado SSL raiz del almacen de Mozilla Firefox. Se aborta la operacion: " + e); //$NON-NLS-1$
		}

		try {
			executeCertUtilToDelete(appConfigDir);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo desinstalar el certificado SSL raiz del almacen de Mozilla Firefox: " + e); //$NON-NLS-1$
		}

		deleteDir(new File(appConfigDir, DIR_CERTUTIL));
	}

	/**
	 * Ejecuta la aplicacion certutil para eliminar el certificado de confianza ra&iacute;z SSL.
	 * @throws IOException Cuando no se encuentra o puede leer alguno de los ficheros necesarios.
	 * @throws GeneralSecurityException Cuando no se puede ejecutar
	 */
	private static void executeCertUtilToDelete(final File appConfigDir) throws IOException, GeneralSecurityException {

		final File certutilFile = new File(appConfigDir, DIR_CERTUTIL + File.separator + CERTUTIL_EXE);

		if (!certutilFile.exists() || !certutilFile.isFile() || !certutilFile.canExecute()) {
			throw new IOException("No se encuentra o no se puede leer el ejecutable para la instalacion en Firefox"); //$NON-NLS-1$
		}

		// Obtenemos todos los directorios de perfil de Firefox del usuario
		boolean error = false;
		final File profilesDir = new File(MozillaKeyStoreUtilities.getMozillaUserProfileDirectory()).getParentFile();
		for (final File profileDir : profilesDir.listFiles()) {
			if (!profileDir.isDirectory()) {
				continue;
			}

			final String[] certutilCommands = new String[] {
					certutilFile.getAbsolutePath(),
					"-D", //$NON-NLS-1$
					"-d", //$NON-NLS-1$
					profileDir.getAbsolutePath(),
					"-n", //$NON-NLS-1$
					"\"" + CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$
			};


			final Process process = new ProcessBuilder(certutilCommands).start();

			// Cuando se instala correctamente no hay salida de ningun tipo, asi que se interpreta
			// cualquier salida como un error
			String line;
			try (final InputStream resIs = process.getInputStream();
					final BufferedReader resReader = new BufferedReader(new InputStreamReader(resIs));) {
				while ((line = resReader.readLine()) != null) {
					error = true;
					LOGGER.severe(line);
				}
			}

			try (final InputStream errIs = process.getErrorStream();
					final BufferedReader errReader = new BufferedReader(new InputStreamReader(errIs));) {
				while ((line = errReader.readLine()) != null) {
					error = true;
					LOGGER.severe(line);
				}
			}
		}

		if (error) {
			throw new KeyStoreException("Error en el borrado del certificado de CA en alguno de los perfiles de usuario de Firefox"); //$NON-NLS-1$
		}
	}

	private static void restrictExecutionsScripts(final File appConfigDir) {
		// Por ultimo, deshabilitamos de nuevo la ejecucion de scripts
		final String[] powerShellCommands = new String[] {
				POWERSHELL_EXE,
				"set-executionpolicy", //$NON-NLS-1$
				"-Scope", //$NON-NLS-1$
				"CurrentUser", //$NON-NLS-1$
				"Restricted" //$NON-NLS-1$
		};

		LOGGER.info("Deshabilitamos el uso de scripts PowerShell"); //$NON-NLS-1$

		try {
			new ProcessBuilder(powerShellCommands).directory(appConfigDir).start();
		} catch (final IOException e) {
			LOGGER.warning("Error al ejecutar el comando para rehabilitar la restriccion de ejecucion de comandos PowerShell: " + e); //$NON-NLS-1$
		}
	}

	/**
	 * Runnable que habilita la ejecuci&oacute;n de scripts PowerShell.
	 */
	private static class PreparateScriptsRunnable implements Runnable {

		private final File appConfigDir;

		public PreparateScriptsRunnable(final File appConfigDir) {
			this.appConfigDir = appConfigDir;
		}

		@Override
		public void run() {

			String[] powerShellCommands = new String[] {
					POWERSHELL_EXE,
					"set-executionpolicy", //$NON-NLS-1$
					"-Scope", //$NON-NLS-1$
					"CurrentUser", //$NON-NLS-1$
					"remotesigned" //$NON-NLS-1$
				};

			try {
				new ProcessBuilder(powerShellCommands).directory(this.appConfigDir).start();
			} catch (final IOException e) {
				LOGGER.warning("Error al ejecutar el script para habilitar la ejecucion de scripts PowerShell: " + e); //$NON-NLS-1$
			}


			// Ejecutamos el script para la comprobacion de la ejecucion de scripts
			final File psConfigScript = new File(this.appConfigDir, FILE_PS_PREPARATE_SCRIPT);
			String psScriptPath;
			try {
				psScriptPath = psConfigScript.getAbsoluteFile().getCanonicalPath();
			}
			catch (final IOException e) {
				LOGGER.warning("No se ha podido reconstruir la ruta del script de configuracion: " + psConfigScript.getAbsolutePath() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				psScriptPath = psConfigScript.getAbsolutePath();
			}

			powerShellCommands = new String[] {
				POWERSHELL_EXE,
				"\"" + psScriptPath + "\"" //$NON-NLS-1$ //$NON-NLS-2$
			};

			try {
				final Process process = new ProcessBuilder(powerShellCommands).directory(this.appConfigDir).start();

				// Un problema entre Java y PowerShell hace que no sea posible en algunos equipos
				// leer la salida del proceso y que se quede este bloqueado por el stream de salida.
				// Lo cerramos para que eso no ocurra.
				process.getOutputStream().close();
			} catch (final IOException e) {
				LOGGER.warning("Error al ejecutar el script para comprobar el permiso de ejecucion de scripts: " + e); //$NON-NLS-1$
			}

		}

	}

	/**
	 * Runnable que lanza el script de generaci&oacute;n de claves SSL.
	 */
	private static class GenerateCertificateRunnable implements Runnable {

		private final File appConfigDir;

		public GenerateCertificateRunnable(final File appConfigDir) {
			this.appConfigDir = appConfigDir;
		}

		@Override
		public void run() {

			final File psConfigScript = new File(this.appConfigDir, FILE_PS_GENERATE_SCRIPT);
			String psScriptPath;
			try {
				psScriptPath = psConfigScript.getAbsoluteFile().getCanonicalPath();
			}
			catch (final IOException e) {
				LOGGER.warning("No se ha podido reconstruir la ruta del script de configuracion: " + psConfigScript.getAbsolutePath() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				psScriptPath = psConfigScript.getAbsolutePath();
			}

			// Ejecutamos el script de generacion del almacen
			final String[] cmds = new String[] {
				POWERSHELL_EXE,
				"\"" + psScriptPath + "\"" //$NON-NLS-1$ //$NON-NLS-2$
			};

			try {
				final Process process = new ProcessBuilder(cmds).directory(this.appConfigDir).start();

				// Un problema entre Java y PowerShell hace que no sea posible en algunos equipos
				// leer la salida del proceso y que se quede este bloqueado por el stream de salida.
				// Lo cerramos para que eso no ocurra.
				process.getOutputStream().close();
			} catch (final IOException e) {
				LOGGER.warning("Error al ejecutar el script para la generacion de las claves SSL: " + e); //$NON-NLS-1$
			}
		}
	}
}
