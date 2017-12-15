package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.util.Enumeration;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.keystores.mozilla.apple.AppleScript;

/** Pruebas de instalaci&oacute;n de certificado ra&iacute;z en OS X.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestMacCaInstall {

	private static final String KEYCHAIN_PATH = "/Library/Keychains/System.keychain"; //$NON-NLS-1$

	private static final String OSX_SEC_COMMAND = "security add-trusted-cert -d -r trustRoot -k %KEYCHAIN% %CERT%"; //$NON-NLS-1$

	/** Prueba la instalaci&oacute;n usando un comando de OS X y AppleScript.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita macOS
	public void testCommandInstall() throws Exception {

		final File f = new File(TestMacCaInstall.class.getResource("/SSL_CERT.cer").toURI()); //$NON-NLS-1$
		final String cmd = OSX_SEC_COMMAND.replace("%KEYCHAIN%", KEYCHAIN_PATH).replace("%CERT%", f.getAbsolutePath().replace(" ", "\\ ")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		System.out.println(cmd);
		System.out.println();

		final AppleScript script = new AppleScript(cmd);
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

}
