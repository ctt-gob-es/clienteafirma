package es.gob.afirma.keystores.filters;

import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.callbacks.CachePasswordCallback;

/** Pruebas del filtro de certificados por identificador de pol&iacute;tica de certificaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestPseudonymFilter {

	/** Prueba del filtro de certificados por identificador de pol&iacute;tica de certificaci&oacute;n.
	 * @throws Exception en cualquier error. */
	@Test
	@SuppressWarnings("static-method")
	public void testPolicyIdFilterMatch() throws Exception {
		final X509Certificate cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
			ClassLoader.getSystemResourceAsStream("pseu-000.cer") //$NON-NLS-1$
		);
		Assert.assertTrue(new PseudonymFilter().matches(cert));
	}

	/** Prueba del filtro de certificados por identificador de pol&iacute;tica de certificaci&oacute;n.
	 * @throws Exception en cualquier error. */
	@Test
	@SuppressWarnings("static-method")
	@Ignore
	public void testPolicyIdFilter() throws Exception {
		final AOKeyStoreManager ksm  = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			AOKeyStore.DNIEJAVA,
			null,
			"TIFSEU", //$NON-NLS-1$
			new CachePasswordCallback("12345".toCharArray()), //$NON-NLS-1$
			null
		);
		final String[] aliases = ksm.getAliases();
		System.out.println("ORIGINAL"); //$NON-NLS-1$
		for (final String alias : aliases) {
			System.out.println(alias);
		}
		System.out.println();
		System.out.println("FILTRADOS"); //$NON-NLS-1$
		final String[] fileteredAliases = new PseudonymFilter().matches(aliases, ksm);
		for (final String alias : fileteredAliases) {
			System.out.println(alias);
		}
	}



}
