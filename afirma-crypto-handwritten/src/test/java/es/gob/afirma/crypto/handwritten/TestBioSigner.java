package es.gob.afirma.crypto.handwritten;

import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Properties;

import org.junit.Test;

/** Prueba de BioSigner. */
public final class TestBioSigner {

	/** Prueba de BioSigner.
	 * @throws Exception EN cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testBioSigner() throws Exception {

		final X509Certificate cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
			TestBioSigner.class.getResourceAsStream("/democert.cer") //$NON-NLS-1$
		);

		final SignerInfoBean signerData = new SignerInfoBean(
			"Tomas", //$NON-NLS-1$
			"Garcia-Meras", //$NON-NLS-1$
			"Capote", //$NON-NLS-1$
			"12345678Z" //$NON-NLS-1$
		);

		final Properties p = new Properties();
		p.put("tsaURL", "http://psis.catcert.net/psis/catcert/tsp"); //$NON-NLS-1$ //$NON-NLS-2$
		p.put("tsaPolicy", "0.4.0.2023.1.1"); //$NON-NLS-1$ //$NON-NLS-2$
		p.put("tsaRequireCert", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		p.put("tsaHashAlgorithm", "SHA-512"); //$NON-NLS-1$ //$NON-NLS-2$
		p.put("imagePage", "1"); //$NON-NLS-1$ //$NON-NLS-2$
		p.put("PositionOnPageLowerLeftX", "70"); //$NON-NLS-1$ //$NON-NLS-2$
		p.put("PositionOnPageLowerLeftY", "120"); //$NON-NLS-1$ //$NON-NLS-2$
		p.put("PositionOnPageUpperRightX", "160"); //$NON-NLS-1$ //$NON-NLS-2$
		p.put("PositionOnPageUpperRightY", "90"); //$NON-NLS-1$ //$NON-NLS-2$


		new BioSigner().sign(
			null, // Padre
			null, // retrieveUrl
			null, // storeUrl
			"HOLA", // Plantilla HTML //$NON-NLS-1$
			new Rectangle(10, 10, 400, 200),
			cert,
			signerData,
			p
		);
	}

}
