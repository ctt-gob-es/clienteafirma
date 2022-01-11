package es.gob.afirma.plugin.certvalidation;

import java.io.InputStream;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.plugin.certvalidation.validation.CertificateVerifierFactory;
import es.gob.afirma.plugin.certvalidation.validation.ValidationResult;

/** Pruebas de validaci&oacute;n de certificados.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestCertValidation {

	/** Prueba de certificados FNMT Componentes.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testFnmt() throws Exception {
		final X509Certificate cert;
		try (
			final InputStream is = TestCertValidation.class.getResourceAsStream("/cert_test_fnmt.cer") //$NON-NLS-1$
		) {
			cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
				is
			);
		}
		final ValidationResult vr = CertificateVerifierFactory.getCertificateVerifier(
			cert
		).validateCertificate();
		vr.check();
	}

	/** Prueba de certificados gen&eacute;ricos.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testGen() throws Exception {
		final X509Certificate cert;
		try (
			final InputStream is = TestCertValidation.class.getResourceAsStream("/CERT_ATOS_TEST.cer") //$NON-NLS-1$
		) {
			cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
				is
			);
		}
		final ValidationResult vr = CertificateVerifierFactory.getCertificateVerifier(
			cert
		).validateCertificate();
		vr.check();
	}

}
