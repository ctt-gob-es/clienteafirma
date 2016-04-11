package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.cert.X509Certificate;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.configurator.CertUtil.CertPack;

/** Pruebas de las utilidades de certificados.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCertUtil {

	/** Prueba la generaci&oacute;n del certificado y almac&eacute;n de claves.
	 * @throws Exception Cuando ocurre un error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCertGeneration() throws Exception {
		final CertPack cp = CertUtil.getCertPackForLocalhostSsl("AutoFirma", "654321"); //$NON-NLS-1$ //$NON-NLS-2$
		System.out.println(AOUtil.getCN((X509Certificate) cp.getCaCertificate()));
		System.out.println(AOUtil.getCN((X509Certificate) cp.getSslCertificate()));
		System.out.println();
		System.out.println();
		System.out.println(AOUtil.hexify(cp.getPkcs12(), true));

		try ( OutputStream fos = new FileOutputStream(File.createTempFile("SSLCERT_", ".cer")) ) { //$NON-NLS-1$ //$NON-NLS-2$
			fos.write(cp.getSslCertificate().getEncoded());
		}

		try ( OutputStream fos = new FileOutputStream(File.createTempFile("CACERT_", ".cer")) ) { //$NON-NLS-1$ //$NON-NLS-2$
			fos.write(cp.getCaCertificate().getEncoded());
		}
	}

}
