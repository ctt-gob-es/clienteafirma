package es.gob.afirma.core.misc;

import java.io.InputStream;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import org.junit.Test;

import es.gob.afirma.core.keystores.KeyUsage;

/** Pruebas de <i>KeyUsage</i> de certificados.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestKeyUsage {

	private static final String[] TEST_FILES = new String[] {
		"DNIE01.cer", //$NON-NLS-1$
		"DNIE02.cer", //$NON-NLS-1$
		"MDEF01.cer", //$NON-NLS-1$
		"MDEF02.cer", //$NON-NLS-1$
		"MDEF03.cer", //$NON-NLS-1$
		"CERES.cer" //$NON-NLS-1$
	};

	/** Pruebas de impresi&oacute;n en consola del <i>KeyUsage</i>.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCertKeyUsage() throws Exception {
		final CertificateFactory cf = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$
		for (final String certName : TEST_FILES) {
			final X509Certificate c;
			try (
				final InputStream is = ClassLoader.getSystemResourceAsStream(certName)
			) {
				c = (X509Certificate) cf.generateCertificate(is);
			}
			final KeyUsage ku = new KeyUsage(c);
			System.out.println(ku.toString());
		}
	}

}
