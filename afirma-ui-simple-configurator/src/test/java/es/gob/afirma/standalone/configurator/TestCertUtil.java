package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;

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
		final CertPack cp = CertUtil.generateSSLCertificate("127.0.0.1", "AutoFirma", "654321".toCharArray(), true); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		System.out.println(cp.getCertificate());
		System.out.println();
		System.out.println();
		System.out.println(AOUtil.hexify(cp.getPkcs12(), true));
	}

	/** prueba de generaci&oacute;n de un certificado SSL gen&eacute;rico.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testGeneralSslCertGeneration() throws Exception {
		final CertPack cp = CertUtil.generateSSLCertificate(
			"localhost", //$NON-NLS-1$
			"alias001", //$NON-NLS-1$
			"1234".toCharArray(),//$NON-NLS-1$
			false,
			"127.0.0.1", //$NON-NLS-1$
			"localhost" //$NON-NLS-1$
		);
		final byte[] pkcs12 = cp.getPkcs12();
		File f = File.createTempFile("PKCS12_", ".pfx"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream os = new FileOutputStream(f);
		) {
			os.write(pkcs12);
		}
		f = File.createTempFile("CERT_", ".cer"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream os = new FileOutputStream(f);
		) {
			os.write(cp.getCertificate().getEncoded());
		}
		System.out.println("Certificado creado en " + f.getAbsolutePath()); //$NON-NLS-1$
	}

}
