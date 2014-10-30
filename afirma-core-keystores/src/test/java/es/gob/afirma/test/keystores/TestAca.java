package es.gob.afirma.test.keystores;

import java.security.KeyStore;
import java.security.Provider;
import java.util.Enumeration;

import org.junit.Test;

import sun.security.mscapi.SunMSCAPI;

/** Prueba de certificado ACA.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestAca {

	private static final String ACA_ALIAS = "EA=demo.colegiado@cgae.redabogacia.org, CN=NOMBRE COLEGIADO COLEGIADO DEMO - NIF 54962013V, OU=28004 / 2356, O=Consejo General de la Abogacía Española / CGAE / 2000, C=ES, ST=Madrid, OID.2.5.4.12=#130741626F6761646F, OID.2.5.4.5=#1309353439363230313356, OID.2.5.4.42=#130444454D4F, OID.2.5.4.4=#1309434F4C45474941444F, OID.1.3.6.1.4.1.16533.30.1=#1309434F4C45474941444F"; //$NON-NLS-1$

	/** Prueba de certificado ACA directamente en SunMSCAPI.
	 * @throws Exception en cualquier error. */
	@Test
	@SuppressWarnings("static-method")
	public void testAcaRawCapi() throws Exception {
		final Provider p = new SunMSCAPI();
		final KeyStore ks = KeyStore.getInstance("Windows-My", p); //$NON-NLS-1$
		ks.load(null, null);
		final Enumeration<String> aliases = ks.aliases();
		while(aliases.hasMoreElements()) {
			System.out.println(aliases.nextElement());
		}

		System.out.println("Tiene clave: " + ks.isKeyEntry(ACA_ALIAS)); //$NON-NLS-1$

		final KeyStore.Entry ke = ks.getEntry(ACA_ALIAS, null);
		System.out.println("Clase de entrada de clave: " + ke.getClass().getName()); //$NON-NLS-1$
	}

}
