package es.gob.afirma.keystores.filters;

import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.filters.rfc.RFC2254CertificateFilter;

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

		RFC2254CertificateFilter filter;

		filter = new RFC2254CertificateFilter(
			"SERIALNUMBER=11830960J", //$NON-NLS-1$
			"O=*POLIC*" //$NON-NLS-1$
		);
		Assert.assertTrue(filter.matches(cert));

		filter = new RFC2254CertificateFilter(
			"", //$NON-NLS-1$
			"cn=*QNIE*" //$NON-NLS-1$
		);
		Assert.assertFalse(filter.matches(cert));

		filter = new RFC2254CertificateFilter(
			null,
			"cn=*DNIE*" //$NON-NLS-1$
		);
		Assert.assertTrue(filter.matches(cert));

		final X509Certificate certApe = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
			ClassLoader.getSystemResourceAsStream("Certicficado_AP_CarlosGG.cer") //$NON-NLS-1$
		);

		filter = new RFC2254CertificateFilter(
			null,
			"cn=*AC Administraci*" //$NON-NLS-1$
		);
		Assert.assertTrue(filter.matches(certApe));
		Assert.assertFalse(filter.matches(cert));
	}

	/** Prueba recursiva por emisor de filtro RFC 2254.
	 * @throws Exception en cualquier error. */
	@Test
	@SuppressWarnings("static-method")
	public void TestRFC2254CertificateRecursiveFilter() throws Exception {
		final RFC2254CertificateFilter filter = new RFC2254CertificateFilter(null, "cn=ANF Global Root CA", true);  //$NON-NLS-1$

		AOKeyStoreManager ksm;
		if (Platform.getOS() == Platform.OS.WINDOWS) {
			ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.WINDOWS, null, "CAPI", null, null); //$NON-NLS-1$
		}
		else if (Platform.getOS() == Platform.OS.MACOSX) {
			ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.APPLE, null, "Apple", null, null); //$NON-NLS-1$
		}
		else if (Platform.getOS() == Platform.OS.LINUX) {
			ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.MOZ_UNI, null, "Mozilla Unificado", null, null); //$NON-NLS-1$
		}
		else {
			Assert.fail("Sistema no identificado"); //$NON-NLS-1$
			return;
		}
		final String[] aceptados = filter.matches(ksm.getAliases(), ksm);
		System.out.println("Aceptados:"); //$NON-NLS-1$
		for (final String a : aceptados) {
			System.out.println(a);
		}
	}

}
