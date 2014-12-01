package es.gob.afirma.crypto.handwritten;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.Properties;

import org.junit.Test;

/** Prueba de BioSigner. */
public final class TestBioSigner implements SignaturePadListener {

	 /* @param retrieveUrl URL para la recuperac&oacute;n (GET HTTP) del PDF a firmar.
	 * @param storeUrl URL para el almac&eacute;n del documento una vez firmado (HTTP POST,
	 *                 en un par&aacute;metro que se debe llamar <i>data</i>).*/

	/** Prueba de BioSigner.
	 * @throws Exception En cualquier error. */
	@Test
	public void testBioSigner() throws Exception {

//		final X509Certificate cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
//			TestBioSigner.class.getResourceAsStream("/democert.cer") //$NON-NLS-1$
//		);
//
//		final SignerInfoBean signerData = new SignerInfoBean(
//			"Tomas", //$NON-NLS-1$
//			"Garcia-Meras", //$NON-NLS-1$
//			"Capote", //$NON-NLS-1$
//			"12345678Z" //$NON-NLS-1$
//		);

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
			"ID001", // Id //$NON-NLS-1$
			this, // SignaturePadListener
			"HOLA", // Plantilla HTML //$NON-NLS-1$
			new Rectangle(10, 10, 400, 200),
			true
		);
	}

	@Override
	public void signatureFinished(final SignatureResult sr) {
		System.out.println("Firma terminada: " + sr.getSignatureId()); //$NON-NLS-1$
		byte[] image = sr.getSignatureJpegImage();
		try {
			OutputStream os = new FileOutputStream(File.createTempFile("IMG_", ".jpg")); //$NON-NLS-1$ //$NON-NLS-2$
			os.write(image);
			os.flush();
			os.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void signatureCancelled(final String id) {
		System.out.println("Firma cancelada: " + id); //$NON-NLS-1$

	}

	@Override
	public void signatureAborted(final Throwable e, final String id) {
		System.out.println("Firma abortada: " + id); //$NON-NLS-1$
		if (e != null) {
			e.printStackTrace();
		}

	}

}
