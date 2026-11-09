package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.cert.X509Certificate;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.configurator.CertUtil.CertPack;

/** Pruebas de las utilidades de certificados.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestCertUtil {

	/** para pruebas sin JUnit.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String[] args) throws Exception {
		new TestCertUtil().testSslpkcs12Generation();
	}

	/** prueba de generaci&oacute;n de PKCS#12 SSL.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore("Prueba no ejecutada en CI / requiere configuración específica")
	public void testSslpkcs12Generation() throws Exception {
		final CertPack certPack = CertUtil.getCertPackForHostSsl(
            "tomcat",          // Alias del certificado SSL //$NON-NLS-1$
            "12341234",        // Contrasena del PKCS#12 //$NON-NLS-1$
            "127.0.0.1",       // Nombre comun //$NON-NLS-1$
            "127.0.0.1",       // IP del host SSL //$NON-NLS-1$
            new String[] { "appprueba",  "servidorcentral", "clavefirmagiss" },        // Nombre del host SSL //$NON-NLS-1$
            "cn=ca-clavefirma" // Nombre de la CA SSL //$NON-NLS-1$
        );
		final byte[] p12Bytes = certPack.getPkcs12();

		final File p12File = File.createTempFile("PKCS12_SSL_", ".P12"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(p12File);
		) {
			fos.write(p12Bytes);
			fos.flush();
		}
		System.out.println("Fichero PKCS#12: " + p12File.getAbsolutePath());

		final byte[] caCert = certPack.getCaCertificate().getEncoded();

		final File caFile = File.createTempFile("CA_SSL_", ".cer"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(caFile);
		) {
			fos.write(caCert);
			fos.flush();
		}
		System.out.println("Fichero de CA: " + caFile.getAbsolutePath());
	}

	/** Comprueba que los seriales de los certificados generados son positivos y distintos.
	 * Regresi&oacute;n: se generaban con java.util.Random, que puede devolver negativos
	 * (serial inv&aacute;lido seg&uacute;n RFC 5280) y repetirlos. */
	@SuppressWarnings("static-method")
	@Test
	public void testSerialesPositivosYDistintos() throws Exception {
		final CertPack primero = CertUtil.getCertPackForLocalhostSsl("Autofirma", "654321"); //$NON-NLS-1$ //$NON-NLS-2$
		final CertPack segundo = CertUtil.getCertPackForLocalhostSsl("Autofirma", "654321"); //$NON-NLS-1$ //$NON-NLS-2$

		final X509Certificate caPrimero = (X509Certificate) primero.getCaCertificate();
		final X509Certificate sslPrimero = (X509Certificate) primero.getSslCertificate();
		final X509Certificate caSegundo = (X509Certificate) segundo.getCaCertificate();

		Assert.assertEquals(
			"El serial de la CA debe ser positivo", //$NON-NLS-1$
			1,
			caPrimero.getSerialNumber().signum()
		);
		Assert.assertEquals(
			"El serial del certificado SSL debe ser positivo", //$NON-NLS-1$
			1,
			sslPrimero.getSerialNumber().signum()
		);
		Assert.assertNotEquals(
			"Dos generaciones no deben compartir serial de CA", //$NON-NLS-1$
			caPrimero.getSerialNumber(),
			caSegundo.getSerialNumber()
		);
	}

	/** Prueba la generaci&oacute;n del certificado y almac&eacute;n de claves.
	 * @throws Exception Cuando ocurre un error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCertGeneration() throws Exception {
		final CertPack cp = CertUtil.getCertPackForLocalhostSsl("Autofirma", "654321"); //$NON-NLS-1$ //$NON-NLS-2$
		System.out.println(AOUtil.getCN((X509Certificate) cp.getCaCertificate()));
		System.out.println(AOUtil.getCN((X509Certificate) cp.getSslCertificate()));
		System.out.println();
		System.out.println();
		System.out.println(AOUtil.hexify(cp.getPkcs12(), true));

		final File sslCertFile = File.createTempFile("SSLCERT_", ".cer"); //$NON-NLS-1$ //$NON-NLS-2$
		try (final OutputStream fos = new FileOutputStream(sslCertFile)) {
			fos.write(cp.getSslCertificate().getEncoded());
		}
		System.out.println("Certificado SSL: " + sslCertFile.getAbsolutePath()); //$NON-NLS-1$

		final File sslP12File = File.createTempFile("SSLPKCS12_", ".p12"); //$NON-NLS-1$ //$NON-NLS-2$
		try (final OutputStream fos = new FileOutputStream(sslP12File)) {
				fos.write(cp.getPkcs12());
		}
		System.out.println("PKCS#12 SSL: " + sslP12File.getAbsolutePath()); //$NON-NLS-1$

		final File sslCaCertFile = File.createTempFile("CACERT_", ".cer"); //$NON-NLS-1$ //$NON-NLS-2$
		try (final OutputStream fos = new FileOutputStream(sslCaCertFile)) {
			fos.write(cp.getCaCertificate().getEncoded());
		}
		System.out.println("Certificado CA: " + sslCaCertFile.getAbsolutePath()); //$NON-NLS-1$
	}

}
