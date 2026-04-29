package es.gob.afirma.standalone.configurator.common;

import java.security.cert.X509Certificate;

/**
 * Almacena la informaci&oacute;n de la firma del fichero de configuraci&oacute;n.
 */
class SignatureInfo {

	private final X509Certificate[] signingCertificateChain;

	/**
	 * Construye la informaci&oacute;n de firma.
	 * @param signingCertificateChain Cadena del certificado de firma.
	 */
	SignatureInfo(final X509Certificate[] signingCertificateChain) {
		this.signingCertificateChain = signingCertificateChain;
	}

	X509Certificate[] getSigningCertificateChain() {
		return this.signingCertificateChain;
	}
}
