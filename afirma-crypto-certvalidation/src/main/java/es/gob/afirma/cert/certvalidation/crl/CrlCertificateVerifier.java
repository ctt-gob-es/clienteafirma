package es.gob.afirma.cert.certvalidation.crl;

import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.cert.certvalidation.CertificateVerifier;
import es.gob.afirma.cert.certvalidation.ValidationResult;
import es.gob.afirma.cert.certvalidation.ocsp.OcspCertificateVerifier;

/** Validador de certificados X.509v3 por verificaci&oacute;n de listas de revocaci&oacute;n
 * y de periodo de validez contra el reloj del sistema.
 * Clase cedida por <a href="http://www.yohago.com/">YoHago</a>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CrlCertificateVerifier extends CertificateVerifier {

	private final Properties conf = new Properties();

	/** Construye un validador de certificados por CRL.
	 * @param confFile Fichero de propiedades con las opciones de configuraci&oacute;n */
	public CrlCertificateVerifier(final String confFile) {
		try {
			this.conf.load(CrlCertificateVerifier.class.getResourceAsStream(confFile));
		}
		catch (final Exception e) {
			throw new IllegalArgumentException(
				"No se ha podido cargar la configuracion del servidor (" + confFile + ": " + e, e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		final String issuerCertFile = this.conf.getProperty("issuerCertFile"); //$NON-NLS-1$
		try {
			this.issuerCert = (X509Certificate) CertificateFactory.getInstance(
				"X.509" //$NON-NLS-1$
			).generateCertificate(
				OcspCertificateVerifier.class.getResourceAsStream(issuerCertFile)
			);
		}
		catch (final CertificateException e) {
			throw new IllegalArgumentException(
				"No se ha podido cargar el certificado raiz del emisor (" + issuerCertFile + "): " + e, e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
	}

	@Override
	protected ValidationResult verifyRevocation(final X509Certificate cert) {
		return CrlHelper.verifyCertificateCRLs(cert, this.issuerCert.getPublicKey(), null);
	}

}
