package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.util.Enumeration;
import java.util.logging.Logger;

import javax.script.ScriptEngine;
import javax.script.ScriptException;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilitiesOsX;

/** Pruebas de instalaci&oacute;n de certificado ra&iacute;z en OS X.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestMacCaInstall {

	private static final String KEYCHAIN_PATH = "/Library/Keychains/System.keychain"; //$NON-NLS-1$

	private static final String OSX_SEC_COMMAND = "security add-trusted-cert -d -r trustRoot -k %KEYCHAIN% %CERT%"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

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

		final ScriptEngine se = MozillaKeyStoreUtilitiesOsX.getAppleScriptEngine();
		if (se != null) {
			try {
				final Object o = se.eval("do shell script \"" + cmd + "\" with administrator privileges"); //$NON-NLS-1$ //$NON-NLS-2$
				System.out.println("Contenido: " + o.toString()); //$NON-NLS-1$
			}
			catch (final ScriptException e) {
				throw new IOException("Error en la importacion de certificado raiz SSL en el llavero de OS X via AppleScript: " + e, e); //$NON-NLS-1$
			}
		}
		else {
			LOGGER.severe(
				"No se ha podido instanciar el motor AppleScript para instalar la raiz SSL en NSS" //$NON-NLS-1$
			);
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
