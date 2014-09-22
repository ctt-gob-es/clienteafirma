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
		
		X509Certificate cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate(
			TestBioSigner.class.getResourceAsStream("/democert.cer")
		);
		
		SignerInfoBean signerData = new SignerInfoBean(
			"Tomas", 
			"Garcia-Meras", 
			"Capote", 
			"12345678Z"
		);
		
		Properties p = new Properties();
		
		new BioSigner().sign(
			null, // Padre 
			null, // retrieveUrl 
			null, // storeUrl 
			"HOLA", // Plantilla HTML 
			new Rectangle(10, 10, 400, 200), 
			cert, 
			signerData, 
			p
		);
	}

}
