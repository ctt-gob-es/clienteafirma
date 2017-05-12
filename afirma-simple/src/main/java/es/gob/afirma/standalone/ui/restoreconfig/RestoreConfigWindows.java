package es.gob.afirma.standalone.ui.restoreconfig;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.WinReg;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.restoreconfig.CertUtil.CertPack;
import es.gob.afirma.standalone.ui.restoreconfig.RestoreConfigFirefox.MozillaProfileNotFoundException;

/**
 * Clase que contiene la l&oacute;gica para realizar las tareas de restauraci&oacute;n
 * de la configuraci&oacute;n de navegadores para el sistema operativo Windows.
 *
 */
final class RestoreConfigWindows implements RestoreConfig {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String SSL_KEYSTORE_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
	private static final String CA_CERTIFICATE_FILENAME = "AutoFirma_ROOT.cer"; //$NON-NLS-1$
	private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$

	private static final String RESTORE_PROTOCOL_EXE = "afirma_register.exe"; //$NON-NLS-1$
	private static final String RESTORE_PROTOCOL_BAT = "afirma_register.bat"; //$NON-NLS-1$

	private static final String REPLACE_PATH_EXE = "$$PATH_EXE$$"; //$NON-NLS-1$
	private static final String REPLACE_INSTALL_DIR = "$$INSTALL_DIR$$"; //$NON-NLS-1$

	/**
     * Caracter de salto de l&iacute;nea para los mensajes de la consola de restauraci&oacute;n
     */
	static final String NEWLINE = System.getProperty("line.separator"); //$NON-NLS-1$

	/**
	 * Ruta de operaci&oacute;n de la aplicaci&oacute;n
	 */
	private final static File appDir = RestoreConfigUtil.getApplicationDirectory();

	@Override
	public void restore(final JTextArea textArea) throws IOException, GeneralSecurityException {

		// Identificamos el directorio de instalacion
		LOGGER.info("Ruta de appDir: " + appDir.getAbsolutePath()); //$NON-NLS-1$
		appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.3", appDir.getAbsolutePath())); //$NON-NLS-1$

		// Regeneramos los certificados que sean necesario (raiz y ssl) y los guardamos en disco
		CertificateFile sslRoot;
		try {
			sslRoot = rebuildCertificates(textArea, appDir);
		}
		catch (final Exception e) {
			LOGGER.severe("No se han podido regenerar los certificados necesarios. No se instalaran en los almacenes de confianza: " + e); //$NON-NLS-1$
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.33")); //$NON-NLS-1$
			sslRoot = null;
		}

		// Instalacion del certificado raiz en Windows
		if (sslRoot != null) {
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.18")); //$NON-NLS-1$
			installRootCAWindowsKeystore(textArea, sslRoot);
		}

		// Instalacion del certificado raiz en Firefox
		if (sslRoot != null) {
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.13")); //$NON-NLS-1$
			installRootCAMozillaKeystore(textArea, sslRoot, appDir);
		}

		// Registramos el protocolo afirma
		appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.24")); //$NON-NLS-1$
		try {
			restoreProtocolRegistry(appDir);
		}
		catch (final Exception e) {
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.25")); //$NON-NLS-1$
		}

		// Configuramos el protocolo afirma en Chrome para que no muestre advertencias al llamarlo
		appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.23")); //$NON-NLS-1$
		configureChrome();

		appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.8")); //$NON-NLS-1$
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
	 * Configura el protocolo "afirma" en Chrome para todos los usuarios de
	 * Windows.
	 */
	private static void configureChrome() {

		// Solicitamos el cierre del navegador Google Chrome
		closeChrome();

		RestoreRemoveChromeWarning.removeChromeWarningsWindows(null, true);

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
	 */
	private static void closeFirefox() {

		while (isProcessRunningWindows("firefox.exe").booleanValue()) { //$NON-NLS-1$
			JOptionPane.showMessageDialog(
					null,
					SimpleAfirmaMessages.getString("RestoreAutoFirma.7"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreAutoFirma.9"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}
	}

	/**
	 * Pide al usuario que cierre el navegador Mozilla Firefox y no permite continuar hasta que lo hace.
	 */
	private static void closeChrome() {

		while (isProcessRunningWindows("chrome.exe").booleanValue()) { //$NON-NLS-1$
			JOptionPane.showMessageDialog(
					null,
					SimpleAfirmaMessages.getString("RestoreAutoFirma.8"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreAutoFirma.9"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}
	}

	/**
	 * Instala el certificado ra&iacute;z CA de AutoFirma
	 *  en el almac&eacute;n ra&iacute;z de Windows
	 *  @param cer El certificado a instalar
	 */
	private static void installRootCAWindowsKeystore(final JTextArea textArea, final CertificateFile certFile) {

		KeyStore ks;
		try {
			ks = KeyStore.getInstance("Windows-ROOT"); //$NON-NLS-1$
			ks.load(null, null);
		} catch (KeyStoreException | NoSuchAlgorithmException | CertificateException | IOException e) {
			LOGGER.severe("No se ha podido cargar el almacen de certificados de confianza de Windows: " + e); //$NON-NLS-1$
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.20")); //$NON-NLS-1$
			return;
		}

			// Comprobamos si el certificado ya esta instalado en el almacen
			try {
				final Certificate currentCert = ks.getCertificate(RestoreConfigUtil.CERT_ALIAS_BROWSER);
				if (currentCert != null && currentCert.equals(certFile.getCert())) {
					LOGGER.info("El certificado raiz ya se encontraba instalado en el almacen del sistema"); //$NON-NLS-1$
					appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.26")); //$NON-NLS-1$
					return;
				}
			}
			catch (final Exception e) {
				LOGGER.warning("No se pudo comprobar si el certificado ya estaba en el almacen: " + e); //$NON-NLS-1$
			}

			// Antes de la instalacion, intentamos desinstalar cualquier otro certificado con el
			// mismo alias que se encuentre en el almacen
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.22")); //$NON-NLS-1$
			try {
				while (ks.getCertificate(RestoreConfigUtil.CERT_ALIAS_BROWSER) != null) {
					ks.deleteEntry(RestoreConfigUtil.CERT_ALIAS_BROWSER);
				}
			} catch (final KeyStoreException ke) {
				appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.34")); //$NON-NLS-1$
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
					LOGGER.warning(
							"No se pudo instalar la CA del certificado SSL para el socket en el almacen de Windows: " + e //$NON-NLS-1$
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
						appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.32")); //$NON-NLS-1$
						return;
					}
				}
			}
			while (!installed);
	}

	/**
	 * Instala el certificado ra&iacute;z CA de AutoFirma
	 *  en el almac&eacute;n ra&iacute;z de Mozilla
	 *  @param textArea &Aacute;rea de texto en el que imprimir las trazas para el usuario.
	 *  @param certFile El certificado a instalar.
	 */
	private static void installRootCAMozillaKeystore(final JTextArea textArea, final CertificateFile certFile, final File installDir) {

		try {
			// Obligamos a que se cierre Firefox antes de manipular el
			// certificado en su almacen
			closeFirefox();

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

		} catch (final IOException e) {
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.3")); //$NON-NLS-1$

		} catch (final MozillaProfileNotFoundException e) {
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.12")); //$NON-NLS-1$
		}
	}

	/**
	 * Restaura los valores del protocolo afirma en el registro de Windows.
	 * Se sobreescriben las distintas subkeys con los valores adecuados.
	 * @param workingDir Directorio de trabajo en el que copiar los ficheros que sean necesarios.
	 * @throws GeneralSecurityException Cuando no se puede actualizar la entrada del protocolo
	 * "afirma" en el registro.
	 */
	private static void restoreProtocolRegistry(final File workingDir) throws GeneralSecurityException {

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
			Advapi32Util.registrySetStringValue(WinReg.HKEY_CLASSES_ROOT, "afirma\\DefaultIcon", "", appDir + "\\ic_firmar.ico"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

			// Crear la key "afirma\\shell\\open\\command" si no existe
			if (!Advapi32Util.registryKeyExists(WinReg.HKEY_CLASSES_ROOT, "afirma\\shell\\open\\command")) { //$NON-NLS-1$
				Advapi32Util.registryCreateKey(WinReg.HKEY_CLASSES_ROOT, "afirma\\shell\\open\\command"); //$NON-NLS-1$
			}
			// Sobreescribir los valores correctos
			Advapi32Util.registrySetStringValue(WinReg.HKEY_CLASSES_ROOT, "afirma\\shell\\open\\command", "", appDir + "\\AutoFirma.exe %1"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

		} catch (final Exception e) {

			LOGGER.warning("No se pudo actualizar el registro con los permisos del usuario, se solicita elevar privilegios: " + e); //$NON-NLS-1$

			// No se pudo actualizar el registro con los permisos del usuario,
			// se usara la biblioteca nativa
			final int result = restoreProtocolRegistryByApp(workingDir.getAbsolutePath());

			throw new GeneralSecurityException("No se pudo registrar el protocolo afirma. Codigo de error: " + result); //$NON-NLS-1$
		}

		LOGGER.info("Configurado afirma en registro Windows"); //$NON-NLS-1$
	}

	/**
	 * Comprueba si es necesario volver a generar el almac&eacute;n de claves y/o el certificado para
	 * comunicaci&oacute;n SSL y, en caso afirmativo, los genera.
	 * @param textArea &Aacute;rea de texto de la consola del restaurador.
	 * @param installDir Directorio de instalaci&oacute;n en el que se deben encontrar los certificados SSL.
	 * @return El certificado ra&iacute;z.
	 * @throws IOException Cuando ocurre un error al generar o copiar a disco los certificados.
	 */
	private static CertificateFile rebuildCertificates(final JTextArea textArea, final File installDir) throws IOException {
		// SSLKeystore --> pfx, SSLRoot --> cer
		CertificateFile sslRoot = null;

		// Si no existe el pfx, debo generar el certificado raiz y el certificado SSL
		if (!checkSSLKeyStoreGenerated(installDir)) {

			// Eliminando de disco las versiones previas de los certificados
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.16")); //$NON-NLS-1$
			try {
				deleteCertificatesFromDisk(installDir);
			} catch (final IOException e) {
				appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.17")); //$NON-NLS-1$
				LOGGER.log(Level.SEVERE, "Error al eliminar los certificados SSL anteriores de disco: " + e); //$NON-NLS-1$
			}

			// Generamos los certificados
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.5")); //$NON-NLS-1$
			CertPack certPack;
			try {
				certPack = CertUtil.getCertPackForLocalhostSsl(RestoreConfigUtil.CERT_ALIAS, KS_PASSWORD);
				sslRoot = new CertificateFile(certPack.getCaCertificate());
			}
			catch (final GeneralSecurityException e) {
				LOGGER.severe("No se ha podido generar el certificado SSL: " + e); //$NON-NLS-1$
				throw new IOException("No se ha podido generar el certificado SSL", e); //$NON-NLS-1$
			}

			// Copiamos los certificados a disco
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.11")); //$NON-NLS-1$
			File sslRootFile = new File(installDir, CA_CERTIFICATE_FILENAME);
			try {
				RestoreConfigUtil.installFile(certPack.getPkcs12(), new File(installDir, SSL_KEYSTORE_FILENAME));
				RestoreConfigUtil.installFile(certPack.getCaCertificate().getEncoded(), sslRootFile);
			}
			catch (final Exception e) {
				LOGGER.severe("No se ha podido guardar en disco los certificados SSL. Los almacenaresmos en un directorio alternativo:  " + e); //$NON-NLS-1$
				final File alternativeDir = AutoFirmaUtil.getWindowsAlternativeAppDir();
				if (!alternativeDir.isDirectory() && !alternativeDir.mkdirs()) {
					throw new IOException("No se ha podido guardar en disco los certificados SSL. Error al crear el directorio alternativo"); //$NON-NLS-1$
				}
				try {
					RestoreConfigUtil.installFile(sslRoot.getCert().getEncoded(), alternativeDir);
					RestoreConfigUtil.installFile(certPack.getPkcs12(), new File(alternativeDir, SSL_KEYSTORE_FILENAME));
					RestoreConfigUtil.installFile(certPack.getCaCertificate().getEncoded(), sslRoot.getFile());
					sslRootFile = new File(alternativeDir, CA_CERTIFICATE_FILENAME);
				}
				catch (final Exception ex) {
					LOGGER.severe("No se ha podido guardar en disco los certificados SSL: " + e); //$NON-NLS-1$
					throw new IOException("Error guardando en disco los certificados SSL", e); //$NON-NLS-1$
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
				final Certificate[] chain = ks.getCertificateChain(RestoreConfigUtil.CERT_ALIAS);
				sslRoot = new CertificateFile(chain[chain.length - 1]);
			}
			catch(final Exception e) {
				LOGGER.log(Level.SEVERE, "Error al extraer el certificado raiz del PKCS#12: " + e); //$NON-NLS-1$
				throw new IOException("Error al generar el certificado SSL", e); //$NON-NLS-1$
			}

			// Copio a disco el certificado raiz
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.11")); //$NON-NLS-1$
			File sslRootFile = new File(installDir, CA_CERTIFICATE_FILENAME);
			try {
				RestoreConfigUtil.installFile(sslRoot.getCert().getEncoded(), sslRootFile);
			}
			catch (final Exception e) {
				LOGGER.severe("No se ha podido guardar en disco el certificado raiz SSL. Lo extraemos a un directorio alternativo: " + e); //$NON-NLS-1$
				final File alternativeDir = AutoFirmaUtil.getWindowsAlternativeAppDir();
				if (!alternativeDir.isDirectory() && !alternativeDir.mkdirs()) {
					throw new IOException("No se ha podido guardar en disco el certificado raiz SSL. Error al crear el directorio alternativo"); //$NON-NLS-1$
				}
				try {
					RestoreConfigUtil.installFile(sslRoot.getCert().getEncoded(), alternativeDir);
					sslRootFile = new File(alternativeDir, CA_CERTIFICATE_FILENAME);
				}
				catch (final Exception ex) {
					LOGGER.severe("No se ha podido guardar en disco el certificado raiz SSL: " + e); //$NON-NLS-1$
					throw new IOException("Error guardando en disco el certificado raiz SSL", e); //$NON-NLS-1$
				}
			}
			sslRoot.setFile(sslRootFile);
		}
		// Si existen ambos no hago nada
		else {
			appendMessage(textArea, SimpleAfirmaMessages.getString("RestoreConfigWindows.14")); //$NON-NLS-1$
			final File sslRootFile = new File(installDir, CA_CERTIFICATE_FILENAME);
			sslRoot = new CertificateFile(CertUtil.loadCertificate(sslRootFile));
			sslRoot.setFile(sslRootFile);
		}

		return sslRoot;
	}


	/**
	 * Elimina los ficheros de certificado ra&iacutez y almac&eacute;n SSL del disco
	 * como paso previo a volver a generarlos
	 * @param installDir Ruta del directorio de la aplicaci&oacute;n
	 * @throws IOException
	 */
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
	 * @param workingDir Directorio de trabajo donde se puedan crear los ficheros
	 * necesarios para ejecutar la operaci&oacute;n.
	 * @return El valor cero en caso de &eacute;xito, -1 si no se proporciona un par&aacute;metro
	 * o cualquier otro c&oacute;digo de error del sistema.
	 * @see <a href="https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx)">https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx)</a>
	 */
	private static int restoreProtocolRegistryByApp(String workingDir) {

		// Copiamos al directorio de instalacion la aplicacion para restaurar el protocolo
		final File exeFile = new File(workingDir, RESTORE_PROTOCOL_EXE);
		try (final FileOutputStream os = new FileOutputStream(exeFile);
				final InputStream is = RestoreConfigWindows.class.getResourceAsStream("/windows/" + RESTORE_PROTOCOL_EXE);) { //$NON-NLS-1$
			os.write(AOUtil.getDataFromInputStream(is));
			os.flush();
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo copiar a disco la aplicacion de restauracion. Se abortara su ejecucion: " + e); //$NON-NLS-1$
			return 2; // ERROR_FILE_NOT_FOUND
		}

		// Copiamos a disco y completamos el script para ejecutar la aplicacion con
		// permisos de administrador
		final File batFile = new File(workingDir, RESTORE_PROTOCOL_BAT);
		try (final FileOutputStream os = new FileOutputStream(batFile);
				final InputStream is = RestoreConfigWindows.class.getResourceAsStream("/windows/" + RESTORE_PROTOCOL_BAT);) { //$NON-NLS-1$
			String batchScript = new String(AOUtil.getDataFromInputStream(is));
			batchScript = batchScript
					.replace(REPLACE_PATH_EXE, exeFile.getAbsolutePath().replace("\\", "\\\\")) //$NON-NLS-1$ //$NON-NLS-2$
					.replace(REPLACE_INSTALL_DIR, appDir.getAbsolutePath().replace("\\", "\\\\")); //$NON-NLS-1$ //$NON-NLS-2$
			os.write(batchScript.getBytes());
			os.flush();
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo copiar a disco la aplicacion de restauracion. Se abortara su ejecucion: " + e); //$NON-NLS-1$
			return 2; // ERROR_FILE_NOT_FOUND
		}

		// Ejecumos el script
		int result = -2;
		try {
			final Process process = Runtime.getRuntime().exec(new String[] {
					batFile.getAbsolutePath()
			});
			result = process.waitFor();
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error durante la ejecucion del proceso de restauracion del protocolo \"afirma\": " + e, e); //$NON-NLS-1$
		}

		// Esperamos 1 segundo para poder eliminar los ficheros
		try {
			Thread.sleep(1000);
		} catch (final InterruptedException e) {
			// No hacemos nada
		}

		// Eliminamos los ficheros
		try {
			Files.delete(exeFile.toPath());
			Files.delete(batFile.toPath());
		} catch (final IOException e) {
			LOGGER.warning("No se pudo eliminar el ejecutable para el registro del protocolo \"afirma\": " + e); //$NON-NLS-1$
		}

		return result;
	}

	/**
	 * Imprime una nueva l&iacute;nea en el panel de trazas.
	 * @param textArea Area de texto en el que imprimir el mensaje.
	 * @param message Mensaje de texto.
	 */
	private static void appendMessage(final JTextArea textArea, final String message) {

		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				textArea.append(message + NEWLINE);
				textArea.setCaretPosition(textArea.getDocument().getLength());
			}
		});

	}

	static class CertificateFile {

		private final Certificate cert;
		private File file;

		public CertificateFile(Certificate cert) {
			this.cert = cert;
		}

		public File getFile() {
			return this.file;
		}

		public void setFile(File file) {
			this.file = file;
		}

		public Certificate getCert() {
			return this.cert;
		}
	}
}
