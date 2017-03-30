package es.gob.afirma.standalone.ui.restoreconfig;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.JTextArea;

import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.WinReg;

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

	private static final String KS_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
	private static final String FILE_AUTOFIRMA_CERTIFICATE = "AutoFirma_ROOT.cer"; //$NON-NLS-1$
	private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$

	/**
     * Caracter de salto de l&iacute;nea para los mensajes de la consola de restauraci&oacute;n
     */
	private final String newline = System.getProperty("line.separator"); //$NON-NLS-1$

	/**
	 * Ruta de operaci&oacute;n de la aplicaci&oacute;n
	 */
	final File appDir = RestoreConfigUtil.getApplicationDirectory();

	/* (non-Javadoc)
	 * @see es.gob.afirma.standalone.ui.restoreconfig.RestoreConfig#restore(javax.swing.JTextArea)
	 */
	@Override
	public void restore(final JTextArea taskOutput) throws IOException, GeneralSecurityException {

		appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.2")); //$NON-NLS-1$

		LOGGER.info("Ruta de appDir: " + appDir.getAbsolutePath()); //$NON-NLS-1$

		appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.3") + appDir.getAbsolutePath()); //$NON-NLS-1$

		// Realizamos las comprobaciones para generar si es necesario,
		// los certificados .pfx y/o .cer
		final Certificate sslRoot = restoreCertificateWindows(taskOutput);

		// Instalacion del certificado raiz en Windows.
		installRootCAWindowsKeystore(taskOutput, sslRoot);

		// Instalacion del certificado raiz en Firefox
		try {

			appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.13")); //$NON-NLS-1$

			appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.9")); //$NON-NLS-1$

			// Obligamos a que se cierre Firefox antes de manipular el
			// certificado en su almacen
			closeFirefox();
			// El SO es Windows, es necesario obtener certutil
			RestoreConfigFirefox.copyConfigurationFiles(appDir);

			// En Windows, certutil no necesita privilegios de administrador
			// para eliminar certificados de Firefox, pero si para
			// importarlo.
			// Además, certutil no informa con algún error si hay algun
			// problema.

			// Desinstalamos versiones previas
			LOGGER.info("Desinstalamos el certificado raiz del almacen de Firefox"); //$NON-NLS-1$
			RestoreConfigFirefox.uninstallRootCAMozillaKeyStore(RestoreConfigUtil.getApplicationDirectory());
			// Vuelvo a instalar lo que había o el nuevo cer generado
			RestoreConfigFirefox.installRootCAMozillaKeyStore(appDir);
			// Elimino certutil tras su uso
			RestoreConfigFirefox.removeConfigurationFiles(appDir);

		} catch (IOException e) { 
			appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.3")); //$NON-NLS-1$
			
		} catch (final MozillaProfileNotFoundException e) {
			appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.12")); //$NON-NLS-1$
		}

		if (isAdmin().booleanValue()) {
			// Sobreescribimos los valores del protocolo afirma en el
			// registro de Windows con los valores correctos
			appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.24")); //$NON-NLS-1$
			restoreProtocolRegistry(taskOutput);
		} else {

			JOptionPane.showMessageDialog(null, SimpleAfirmaMessages.getString("RestoreConfigWindows.28"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("RestoreAutoFirma.9"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
			appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.28")); //$NON-NLS-1$
		}

		closeChrome();
		// Insertamos el protocolo afirma en el fichero de configuracion de
		// Google Chrome
		configureChrome(taskOutput);

		appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.8")); //$NON-NLS-1$

	}

	/**
	 * Elimina los ficheros de certificado ra&iacutez y almac&eacute;n SSL del disco
	 * como paso previo a volver a generarlos
	 * @param appDir Ruta del directorio de la aplicaci&oacute;n
	 * @throws IOException
	 */
	private void deleteInstalledCertificates(final File appDir) throws IOException {

		if (checkSSLKeyStoreGenerated(appDir)) {

			Files.delete(new File(appDir, KS_FILENAME).toPath());
		}

		if (checkSSLRootCertificateGenerated(appDir)) {

			Files.delete(new File(appDir, FILE_AUTOFIRMA_CERTIFICATE).toPath());
		}

	}

	/**
	 * Comprueba si ya existe un almac&eacute;n de certificados generado.
	 *
	 * @param appDir
	 *            Directorio de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un almacen de certificados SSL,
	 *         {@code false} en caso contrario.
	 */
	private static boolean checkSSLKeyStoreGenerated(final File appDir) {
		return new File(appDir, KS_FILENAME).exists();
	}

	/**
	 * Comprueba si ya existe un certificado raiz generado.
	 *
	 * @param appDir
	 *            Directorio de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un certificado raiz .cer, {@code false}
	 *         en caso contrario.
	 */
	private static boolean checkSSLRootCertificateGenerated(final File appDir) {
		return new File(appDir, FILE_AUTOFIRMA_CERTIFICATE).exists();
	}


	/**
	 * Configura el protocolo "afirma" en Chrome para todos los usuarios de
	 * Windows.
	 *
	 * @param window
	 *            Consola de salida.
	 * @param installing
	 *            Indica si se debe configurar ({@code true}) o desconfigurar
	 *            ({@code false}) el protocolo "afirma" en Chrome.
	 */
	private void configureChrome(final JTextArea taskOutput) {

		appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.23")); //$NON-NLS-1$
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

			final BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));

			while ((line = input.readLine()) != null) {
				pidInfo += line;
			}

			input.close();

		} catch (final IOException e) {
			LOGGER.severe("Ha ocurrido un error al ejecutar el comando " + process + " en Windows"); //$NON-NLS-1$ //$NON-NLS-2$
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
	 * Concatena un texto a una nueva l&iacute;nea al par&aacute;metro JTextArea
	 * @param taskOutput JTextArea donde el texto es concatenado
	 * @param message Texto a concatenar.
	 */
	private void appendMessage(final JTextArea taskOutput, final String message) {
		taskOutput.append(message + newline);
		taskOutput.setCaretPosition(taskOutput.getDocument().getLength());
	}


	/**
	 * Instala el certificado ra&iacute;z CA de AutoFirma
	 *  en el almac&eacute;n ra&iacute;z de Windows
	 *  @param cer El certificado a instalar
	 */
	private boolean installRootCAWindowsKeystore(final JTextArea taskOutput, Certificate cer) {

		Boolean exito = Boolean.TRUE;

		KeyStore ks;

		try {
			ks = KeyStore.getInstance("Windows-ROOT"); //$NON-NLS-1$
			ks.load(null, null);

			appendMessage(taskOutput,
					SimpleAfirmaMessages.getString("RestoreConfigWindows.18")); //$NON-NLS-1$

			// Importar el certificado solo si no esta instalado ya o si estaba instalado
			// pero se ha debido volver a generar el cer
			if (ks.getCertificate(RestoreConfigUtil.CERT_ALIAS_BROWSER) == null || cer != null) {

				// El certificado no viene cargado porque ya existía el archivo .cer
				// Lo cargo ahora.
				if (cer == null) {

					try {

						cer = loadCertificateFromFile();

					} catch (final IOException e) {

						LOGGER.severe("No ha podido cargarse el certificado .cer desde el archivo"); //$NON-NLS-1$
						appendMessage(taskOutput,
								SimpleAfirmaMessages.getString("RestoreConfigWindows.27",  e.getMessage())); //$NON-NLS-1$
						exito = Boolean.FALSE;
					}
				} else {

					// Se intenta eliminar instalaciones previas del certificado raiz
					try {
						while (ks.getCertificate(RestoreConfigUtil.CERT_ALIAS_BROWSER) != null) {
							ks.deleteEntry(RestoreConfigUtil.CERT_ALIAS_BROWSER);
						}
					} catch (final KeyStoreException ke) {
						appendMessage(taskOutput,
								"No ha podido eliminarse alguna importacion previa del certificado raiz del almacen de Windows"); //$NON-NLS-1$
						LOGGER.info("No ha podido eliminarse alguna importacion previa del certificado raiz del almacen de Windows"); //$NON-NLS-1$
					}
				}

				boolean installed = false;
				boolean cancelled = false;
				do {
					try {
						ks.setCertificateEntry(RestoreConfigUtil.CERT_ALIAS_BROWSER, cer);
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
							cancelled = true;
							LOGGER.severe("El usuario cancelo la instalacion del certificado SSL para el socket: " + e); //$NON-NLS-1$
						}
					}
				}
				while (!installed && !cancelled);
				
				//ks.setCertificateEntry(RestoreConfigUtil.CERT_ALIAS_BROWSER, cer);
				appendMessage(taskOutput,
						SimpleAfirmaMessages.getString("RestoreConfigWindows.19")); //$NON-NLS-1$			
				
			} else {
				appendMessage(taskOutput,
						SimpleAfirmaMessages.getString("RestoreConfigWindows.26")); //$NON-NLS-1$
			}

		} catch (KeyStoreException | NoSuchAlgorithmException | CertificateException | IOException e) {

			LOGGER.severe(e.getStackTrace().toString());
			appendMessage(taskOutput,
					SimpleAfirmaMessages.getString("RestoreConfigWindows.20",  e.getMessage())); //$NON-NLS-1$
			exito = Boolean.FALSE;

		}

		return exito;

	}

	/**
	 * Restaura los valores del protocolo afirma en el registro de Windows.
	 * Se sobreescriben las distintas subkeys con los valores adecuados.
	 */
	private void restoreProtocolRegistry(final JTextArea taskOutput) {

		final File appDir1 = RestoreConfigUtil.getApplicationDirectory();

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
			Advapi32Util.registrySetStringValue(WinReg.HKEY_CLASSES_ROOT, "afirma\\DefaultIcon", "", appDir1 + "\\ic_firmar.ico"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

			// Crear la key "afirma\\shell\\open\\command" si no existe
			if (!Advapi32Util.registryKeyExists(WinReg.HKEY_CLASSES_ROOT, "afirma\\shell\\open\\command")) { //$NON-NLS-1$
				Advapi32Util.registryCreateKey(WinReg.HKEY_CLASSES_ROOT, "afirma\\shell\\open\\command"); //$NON-NLS-1$
			}
			// Sobreescribir los valores correctos
			Advapi32Util.registrySetStringValue(WinReg.HKEY_CLASSES_ROOT, "afirma\\shell\\open\\command", "", appDir1 + "\\AutoFirma.exe %1"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

			LOGGER.info("Configurado afirma en registro Windows"); //$NON-NLS-1$
		} catch (final Exception e) {
			LOGGER.severe("Error al escribir en el registro de Windows: " + e.getMessage()); //$NON-NLS-1$
			appendMessage(taskOutput,
					SimpleAfirmaMessages.getString("RestoreConfigWindows.25",  e.getMessage())); //$NON-NLS-1$
		}

	}

	/**
	 * Comprueba si es necesario volver a generar el almac&eacute;n de claves y/o el certificado para
	 * comunicaci&oacute;n SSL y en caso afirmativo los genera
	 * @param taskOutput Objeto que representa el &aacute;rea de texto de la consola del restaurador
	 * @return El certificado SSL generado o null
	 * @throws IOException
	 * @throws GeneralSecurityException
	 */
	private Certificate restoreCertificateWindows(final JTextArea taskOutput) throws IOException, GeneralSecurityException {
		// SSLKeystore --> pfx, SSLRoot --> cer
		Certificate sslRoot = null;

		// Si existe el .pfx, pero no el .cer, cargo el .pfx y extraigo el .cer
		if (checkSSLKeyStoreGenerated(appDir) && !checkSSLRootCertificateGenerated(appDir)) {

			// Cargo el pfx
			final File sslKeyStoreFile = new File(appDir, KS_FILENAME);
			final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
			final char ksPass[] = KS_PASSWORD.toCharArray();
			ks.load(new FileInputStream(sslKeyStoreFile), ksPass);

			final Certificate[] chain = ks.getCertificateChain(RestoreConfigUtil.CERT_ALIAS);

			// En esta posicion esta en certificado raiz.
			sslRoot = chain[1];

			// Instalo el .cer a partir del .pfx
			RestoreConfigUtil.installFile(sslRoot.getEncoded(), new File(appDir, FILE_AUTOFIRMA_CERTIFICATE));

			// Si no existe el pfx, debo generar ambos
		} else if (!checkSSLKeyStoreGenerated(appDir)) {

			final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(RestoreConfigUtil.CERT_ALIAS, KS_PASSWORD);

			appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.16")); //$NON-NLS-1$

			try {

				deleteInstalledCertificates(appDir);

				appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.5")); //$NON-NLS-1$
				// Instalacion del certificado pfx
				RestoreConfigUtil.installFile(certPack.getPkcs12(), new File(appDir, KS_FILENAME));

				sslRoot = certPack.getCaCertificate();

				appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.11")); //$NON-NLS-1$
				// Instalacion del certificado raiz .cer si no existe
				RestoreConfigUtil.installFile(sslRoot.getEncoded(), new File(appDir, FILE_AUTOFIRMA_CERTIFICATE));

			} catch (final IOException e) {
				appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.17")); //$NON-NLS-1$
				LOGGER.log(Level.SEVERE, e.getMessage());
			}

		// Si existen ambos no hago nada
		} else {
			appendMessage(taskOutput, SimpleAfirmaMessages.getString("RestoreConfigWindows.14")); //$NON-NLS-1$
		}

		return sslRoot;
	}

	private Certificate loadCertificateFromFile() throws CertificateException, IOException {

		FileInputStream fis;
		Certificate cert = null;

		fis = new FileInputStream(new File(appDir, FILE_AUTOFIRMA_CERTIFICATE).getAbsolutePath());

		final BufferedInputStream bis = new BufferedInputStream(fis);

		final CertificateFactory cf = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$

		while (bis.available() > 0) {
			cert = cf.generateCertificate(bis);
		}

		return cert;
	}


	/**
	 * Comprueba si el proceso se est&acute; ejecutando en modo administrador de Windows.
	 * @return ({@code true}}) Si es modo administrador, ({@code false}}) en caso contrario
	 */
	private static Boolean isAdmin() {
		Boolean isAdmin = Boolean.FALSE;

		Process p;
		try {

			final String[] command = new String[] { "reg", //$NON-NLS-1$
					"query", //$NON-NLS-1$
					"\"HKU\\S-1-5-19\"", //$NON-NLS-1$
			};
			ProcessBuilder pb = new ProcessBuilder(command);
						
			p = pb.start();
			p.waitFor();

			final int exitValue = p.exitValue();

			if (0 == exitValue) {
				isAdmin = Boolean.TRUE;
			}

		} catch (final Exception e) {
			LOGGER.severe("Ha ocurrido un error al determinar si AutoFirma se ejecuta como administrador"); //$NON-NLS-1$
		}

		return isAdmin;
	}
}
