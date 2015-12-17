package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import org.junit.Test;

/** Pruebas de instalaci&oacute;n de certificado ra&iacute;z SSL en Firefox.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestMozillaInstall {

	/** Prueba simple de instalaci&oacute;n de certificado ra&iacute;z SSL en Firefox.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testSimpleFirefoxInstall() throws Exception {
		final X509Certificate cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
			TestMozillaInstall.class.getResourceAsStream("/SSL_CERT.cer") //$NON-NLS-1$
		);
		ConfiguratorFirefox.installRootCAMozillaKeyStore(new File("/var/tmp"), cert); //$NON-NLS-1$
	}

}
