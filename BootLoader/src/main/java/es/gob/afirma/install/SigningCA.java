package es.gob.afirma.install;

import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

/**
 * Autoridades de certificacion permitidas para la firma de ficheros JAR y ZIP.
 */
enum SigningCA {
	/** CA de Sun/Oracle con la que se firman las extensiones de Java. */
	SUN ("/resources/sun/JCE_Code_Signing_CA.cer", null),
	/** CA del integrador con la que se firman las dependencias del Cliente. */
	INTEGRATOR ("/resources/integrator_Code_Signing_CA.cer", null /*"/resources/integrator_Code_Signing.cer"*/);

	/**
	 * Define la CA con la ruta interna al JAR del certificado.
	 * @param caCertificatePath Ruta interna de la CA.
	 */
	private SigningCA(final String caCertificatePath, final String sigCertificatePath) {
		try {
			caCert = (X509Certificate) CertificateFactory.getInstance(
				"X.509"
			).generateCertificate(
				AOInstallUtils.class.getResourceAsStream(
					caCertificatePath
				)
			);
		} 
		catch(final Exception e) {
			throw new UnsupportedOperationException(
				"No se ha podido cargar el certificado raiz: " + caCertificatePath, e
			);
		}
		if (sigCertificatePath != null) {
			try {
				sgCert = (X509Certificate) CertificateFactory.getInstance(
					"X.509"
				).generateCertificate(
					AOInstallUtils.class.getResourceAsStream(
						sigCertificatePath
					)
				);
			} 
			catch(final Exception e) {
				throw new UnsupportedOperationException(
					"No se ha podido cargar el certificado firmante: " + sigCertificatePath, e
				);
			}
		}
		else sgCert = null;
	}

	private final X509Certificate caCert;
	private final X509Certificate sgCert;
	
	X509Certificate getCACertificate() {
		return caCert;
	}
	
	X509Certificate getSigningCertificate() {
		return sgCert;
	}
	
}
