package es.gob.afirma.keystores.filters.rfc;

import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import org.junit.Assert;
import org.junit.Test;

/** Pruebas de filtros RFC 2254.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestRFC2254CertificateFilter {

	/** Prueba simple de filtro RFC 2254.
	 * @throws Exception en cualquier error. */
	@Test
	@SuppressWarnings("static-method")
	public void TestRFC2254CertificateSimpleFilter() throws Exception {
		final X509Certificate cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
			ClassLoader.getSystemResourceAsStream("Tomas_DNI_FIRMA.cer") //$NON-NLS-1$
		);
		RFC2254CertificateFilter filter = new RFC2254CertificateFilter(
			"SERIALNUMBER=11830960J", //$NON-NLS-1$
			"O=*POLIC*" //$NON-NLS-1$
		);
		Assert.assertTrue(filter.matches(cert));

		filter = new RFC2254CertificateFilter(
			"", //$NON-NLS-1$
			"cn=*QNIE*" //$NON-NLS-1$
		);
		Assert.assertFalse(filter.matches(cert));
	}

}
