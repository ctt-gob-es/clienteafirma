package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.mozilla.apple.ShellScript;

/** Pruebas de instalaci&oacute;n de certificado ra&iacute;z en OS X.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestMacCaInstall {

	private static final String KEYCHAIN_PATH = "/Library/Keychains/System.keychain"; //$NON-NLS-1$

	private static final String OSX_SEC_COMMAND = "security -vi add-trusted-cert -d -r trustRoot -k %KEYCHAIN% %CERT%"; //$NON-NLS-1$

	/** Prueba la instalaci&oacute;n usando un comando de OS X y AppleScript.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita macOS. Este modo de uso deja de funciona con macOS Big Sur
	public void testInstallRootCAWithScript() throws Exception {

		final File f = new File(TestMacCaInstall.class.getResource("/Autofirma_ROOT.cer").toURI()); //$NON-NLS-1$
		final String cmd = OSX_SEC_COMMAND.replace("%KEYCHAIN%", KEYCHAIN_PATH).replace("%CERT%", f.getAbsolutePath().replace(" ", "\\ ")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		System.out.println(cmd);
		System.out.println();

		final ShellScript script = new ShellScript(cmd);
		try {
			final String result = script.runAsAdministrator();
			System.out.println("Contenido: " + result); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new IOException("Error en la importacion de certificado raiz SSL en el llavero de OS X via AppleScript: " + e, e); //$NON-NLS-1$
		}

		try (
				final InputStream is = new FileInputStream(KEYCHAIN_PATH);
		) {
			final KeyStore ks = KeyStore.getInstance("KeychainStore"); //$NON-NLS-1$
			ks.load(is, null);
			final Enumeration<String> aliases = ks.aliases();
			while (aliases.hasMoreElements()) {
				System.out.println(aliases.nextElement());
			}
		}
	}

	/**
	 * Instala un certificado de CA en el llavero mediante un proceso que ejecuta directamente "security"
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testInstallRootCaWithProcess() throws Exception {

		final File f = new File(TestMacCaInstall.class.getResource("/Autofirma_ROOT.cer").toURI()); //$NON-NLS-1$

		final List<String> params = new ArrayList<>();
		params.add("sudo"); //$NON-NLS-1$
		params.add("-S"); //$NON-NLS-1$
		params.add("security"); //$NON-NLS-1$
		params.add("-i"); //$NON-NLS-1$
		params.add("add-trusted-cert"); //$NON-NLS-1$
		params.add("-d"); //$NON-NLS-1$
		params.add("-r"); //$NON-NLS-1$
		params.add("trustRoot"); //$NON-NLS-1$
		params.add("-k"); //$NON-NLS-1$
		params.add("/Library/Keychains/System.keychain"); //$NON-NLS-1$
		params.add(f.getAbsolutePath());

		boolean certImported = false;
		boolean passwordError = false;
		do {

			final ProcessBuilder builder = new ProcessBuilder(params);
			final Process process = builder.start();

			// Se proporciona la contrasena de administrador
			try (OutputStream os = process.getOutputStream()) {
				// Por seguridad, se pone todo en una linea para evitar que la contrasena se exponda en la depuracion
				String text = "Inserte la contrase\u00F1a del administrador para la importaci\u00F3n de los certificados"; //$NON-NLS-1$
				if (passwordError) {
					text = "Contrase\u00F1a incorrecta. " + text; //$NON-NLS-1$
				}
				os.write(new String(AOUIFactory.getPassword(text, null)).getBytes(StandardCharsets.UTF_8));
				//os.write(new String(AOUIFactory.getPassword("Constrase\u00F1a de administrador", null, StandardCharsets.UTF_8.name(), false, null)).getBytes(StandardCharsets.UTF_8));
				os.flush();
			}
			catch (final AOCancelledOperationException e) {
				System.out.println("Se cancelo el dialogo de entrada de contrasena: " + e); //$NON-NLS-1$
				return;
			}

			final int exitValue = process.waitFor();
			System.out.println("exitValue: " + exitValue); //$NON-NLS-1$

			if (exitValue == 0) {
				certImported = true;
			}
			else {
				byte[] errorOutput = null;
				try (final InputStream errorStream = process.getErrorStream()) {
					errorOutput = AOUtil.getDataFromInputStream(errorStream);
				}
				if (errorOutput != null) {
					final String errorMsg = new String(errorOutput);

					System.out.println("Salida de error: " + errorMsg); //$NON-NLS-1$
					if (!errorMsg.toLowerCase().contains("password")) { //$NON-NLS-1$
						return;
					}
					passwordError = true;
				}
			}
		} while(!certImported);

		System.out.println("Fin"); //$NON-NLS-1$
	}

	/**
	 * Instala un certificado SSL en el llavero mediante un proceso que ejecuta directamente "security"
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testInstallSslCertWithProcess() throws Exception {

		final File f = new File(TestMacCaInstall.class.getResource("/127_0_0_1.cer").toURI()); //$NON-NLS-1$

		final List<String> params = new ArrayList<>();
		params.add("sudo"); //$NON-NLS-1$
		params.add("-S"); //$NON-NLS-1$
		params.add("security"); //$NON-NLS-1$
		params.add("-i"); //$NON-NLS-1$
		params.add("add-trusted-cert"); //$NON-NLS-1$
		params.add("-d"); //$NON-NLS-1$
		params.add("-r"); //$NON-NLS-1$
		params.add("trustAsRoot"); //$NON-NLS-1$
		params.add("-k"); //$NON-NLS-1$
		params.add("/Library/Keychains/System.keychain"); //$NON-NLS-1$
		params.add(f.getAbsolutePath());

		boolean certImported = false;
		boolean passwordError = false;
		do {

			// Por seguridad, se pone todo en una linea para evitar que la contrasena se exponda en la depuracion
			String text = "Inserte la contrase\u00F1a del administrador para la importaci\u00F3n de los certificados"; //$NON-NLS-1$
			if (passwordError) {
				text = "Contrase\u00F1a incorrecta. " + text; //$NON-NLS-1$
			}
			byte[] data;
			try {
				data = new String(AOUIFactory.getPassword(text, null)).getBytes(StandardCharsets.UTF_8);
			}
			catch (final AOCancelledOperationException e) {
				System.out.println("Se cancelo el dialogo de entrada de contrasena: " + e); //$NON-NLS-1$
				return;
			}

			final ProcessBuilder builder = new ProcessBuilder(params);
			final Process process = builder.start();

			// Se proporciona la contrasena de administrador
			try (OutputStream os = process.getOutputStream()) {
				os.write(data);
				os.flush();
			}

			final int exitValue = process.waitFor();
			System.out.println("exitValue: " + exitValue); //$NON-NLS-1$

			if (exitValue == 0) {
				certImported = true;
			}
			else {
				byte[] errorOutput = null;
				try (final InputStream errorStream = process.getErrorStream()) {
					errorOutput = AOUtil.getDataFromInputStream(errorStream);
				}
				if (errorOutput != null) {
					final String errorMsg = new String(errorOutput);

					System.out.println("Salida de error: " + errorMsg); //$NON-NLS-1$
					if (!errorMsg.toLowerCase().contains("password")) { //$NON-NLS-1$
						return;
					}
					passwordError = true;
				}
			}
		} while(!certImported);

		System.out.println("Fin"); //$NON-NLS-1$
	}
}
