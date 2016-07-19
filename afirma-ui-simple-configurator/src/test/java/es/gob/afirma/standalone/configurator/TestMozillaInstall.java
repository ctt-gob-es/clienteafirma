package es.gob.afirma.standalone.configurator;

import java.io.File;

import org.junit.Test;

/** Pruebas de instalaci&oacute;n de certificado ra&iacute;z SSL en Firefox.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestMozillaInstall {

	/** Prueba simple de instalaci&oacute;n de certificado ra&iacute;z SSL en Firefox.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testSimpleFirefoxInstall() throws Exception {
		ConfiguratorFirefox.installRootCAMozillaKeyStore(new File("/var/tmp")); //$NON-NLS-1$
	}

}
