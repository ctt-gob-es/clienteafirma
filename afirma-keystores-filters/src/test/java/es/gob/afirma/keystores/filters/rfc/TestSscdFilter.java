package es.gob.afirma.keystores.filters.rfc;

import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import junit.framework.Assert;

import org.junit.Test;

/** Prueba de los filtros por SSCD.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestSscdFilter {

	/** Prueba simple del filtro.
	 * @throws Exception En cualquier error */
	@SuppressWarnings("static-method")
	@Test
	public void testFilter() throws Exception {
		final X509Certificate cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
			ClassLoader.getSystemResourceAsStream("Tomas_DNI_FIRMA.cer") //$NON-NLS-1$
		);
		Assert.assertTrue(
			new SscdFilter().matches(cert)
		);

	}

}
