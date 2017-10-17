package es.gob.afirma.keystores.filters;

import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Collections;

import org.junit.Assert;
import org.junit.Test;

/** Pruebas del filtro de certificados por identificador de pol&iacute;tica de certificaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestPolicyIdFilter {

	/** Prueba del filtro de certificados por identificador de pol&iacute;tica de certificaci&oacute;n.
	 * @throws Exception en cualquier error. */
	@Test
	@SuppressWarnings("static-method")
	public void testPolicyIdFilterMatch() throws Exception {
		final X509Certificate cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
			ClassLoader.getSystemResourceAsStream("Tomas_DNI_FIRMA.cer") //$NON-NLS-1$
		);
		Assert.assertTrue(new PolicyIdFilter(Collections.singletonList("2.16.724.1.2.2.2.3")).matches(cert)); //$NON-NLS-1$
		Assert.assertFalse(new PolicyIdFilter(Collections.singletonList("2.16.724.1.2.2.2.5")).matches(cert)); //$NON-NLS-1$
	}

}
