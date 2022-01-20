package es.gob.afirma.plugin.certvalidation;

import java.io.InputStream;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateFactory;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.X509Certificate;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.plugin.certvalidation.validation.CertificateRevokedException;
import es.gob.afirma.plugin.certvalidation.validation.CertificateVerifierFactory;
import es.gob.afirma.plugin.certvalidation.validation.ValidationResult;

/** Pruebas de validaci&oacute;n de certificados FNMT.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestFnmtCertValidation {

	private static final String[] ACTIVOS = {
		//"certificadoActivoACUsuarios-Pruebas.cer", //$NON-NLS-1$
		"ejemploADM_SLD_ACTIVO.cer", //$NON-NLS-1$
		"ejemploESPJ_ACTIVO.cer", //$NON-NLS-1$
		"ejemploPJ_ACTIVO.cer", //$NON-NLS-1$
		//"PF_ACTIVO_EIDAS.crt", //$NON-NLS-1$
		"SELLO_ACTIVO_EIDAS_ACAP.crt", //$NON-NLS-1$
		"SOFTWARE_ACTIVO_EIDAS_ACAP.cer", //$NON-NLS-1$
		"TARJETA_ACTIVO_EIDAS_ACAP.cer" //$NON-NLS-1$
	};

	private static final String[] REVOCADOS = {
		//"certificadoRevocadoACUsuarios-Pruebas.cer", //$NON-NLS-1$
		"ejemploADM_SLD_REVOCADO.cer", //$NON-NLS-1$
		"ejemploESPJ_REVOCADO.cer", //$NON-NLS-1$
		"ejemploPJ_REVOCADO.cer", //$NON-NLS-1$
		//"PF_REVOCADO_EIDAS.crt", //$NON-NLS-1$
		//"SELLO_REVOCADO_EIDAS_ACAP.crt", //$NON-NLS-1$
		//"SOFTWARE_REVOCADO_EIDAS_ACAP.cer", //$NON-NLS-1$
		//"TARJETA_REVOCADO_EIDAS_ACAP.cer" //$NON-NLS-1$
	};

	/** Prueba de certificados FNMT revocados.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testRevoked() throws Exception {
		final CertificateFactory cf = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$
		for (final String c : REVOCADOS) {
			final X509Certificate cert;
			try (
				final InputStream is = TestFnmtCertValidation.class.getResourceAsStream("/fnmt/" + c) //$NON-NLS-1$
			) {
				cert = (X509Certificate) cf.generateCertificate(is);
			}
			System.out.println("Probando certificado emitido por '" + AOUtil.getCN(cert.getIssuerX500Principal().toString()) + "': " + c); //$NON-NLS-1$ //$NON-NLS-2$
			final ValidationResult vr = CertificateVerifierFactory.getCertificateVerifier(
				cert
			).validateCertificate();
			try {
				vr.check();
			}
			catch(final CertificateExpiredException | CertificateNotYetValidException e) {
				System.out.println("Caducado: " + e); //$NON-NLS-1$
				System.out.println();
				System.out.println();
				continue;
			}
			catch(final CertificateRevokedException e) {
				System.out.println("Revocado: " + e); //$NON-NLS-1$
				System.out.println();
				System.out.println();
				continue;
			}
			Assert.fail("Deberia estar revocado"); //$NON-NLS-1$
		}
	}

	/** Prueba de certificados FNMT v&aacute;lidos.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testValid() throws Exception {
		final CertificateFactory cf = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$
		for (final String c : ACTIVOS) {
			final X509Certificate cert;
			try (
				final InputStream is = TestFnmtCertValidation.class.getResourceAsStream("/fnmt/" + c) //$NON-NLS-1$
			) {
				cert = (X509Certificate) cf.generateCertificate(is);
			}
			System.out.println("Probando certificado emitido por '" + AOUtil.getCN(cert.getIssuerX500Principal().toString()) + "': " + c); //$NON-NLS-1$ //$NON-NLS-2$
			final ValidationResult vr = CertificateVerifierFactory.getCertificateVerifier(
				cert
			).validateCertificate();
			vr.check();
			System.out.println();
			System.out.println();
		}
	}

}
