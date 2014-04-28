package es.gob.afirma.cert.certvalidation;

import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.zip.CRC32;

import es.gob.afirma.cert.certvalidation.crl.CrlCertificateVerifier;
import es.gob.afirma.cert.certvalidation.ocsp.OcspCertificateVerifier;

/** Factor&iacute;a para la obtenci&oacute;n de un validador de certificados.
 * Clase cedida por <a href="http://www.yohago.com/">YoHago</a>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CertificateVerifierFactory {

	private static Properties p = null;

	private static final String FACTORY_CONFIGURATION = "/validationfactory.properties"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private CertificateVerifierFactory() {
		// No permitimos la instanciacion
	}

	private static String getIssuerIdentifier(final X509Certificate cert) {
		// Es el CRC del emisor lo que le identifica
		final CRC32 issuerCrc = new CRC32();
		issuerCrc.update(cert.getIssuerX500Principal().getEncoded());
		final long intermediateCrc = issuerCrc.getValue();
		issuerCrc.reset();
		issuerCrc.update(cert.getSigAlgName().getBytes());
		return Long.toHexString(intermediateCrc + issuerCrc.getValue());
	}

	/** Obtiene un validador para el certificado proporcionado.
	 * @param cert Certificado a validar
	 * @return Validador para el certificado proporcionado */
	public static CertificateVerifier getCertificateVerifier(final X509Certificate cert) {
		if (cert == null) {
			throw new IllegalArgumentException("El certificado no puede ser nulo"); //$NON-NLS-1$
		}
		if (p == null) {
			p = new Properties();
			try {
				p.load(CertificateVerifierFactory.class.getResourceAsStream(FACTORY_CONFIGURATION));
			}
			catch (final Exception e) {
				p = null;
				throw new IllegalStateException(
					"No se ha podido cargar la configuracion de la factoria: " + e, e //$NON-NLS-1$
				);
			}
		}

		final String crc = getIssuerIdentifier(cert);
		LOGGER.info("Identificador del emisor del certificado: " + crc); //$NON-NLS-1$

		final String validationProperties = p.getProperty(crc + ".validation.properties"); //$NON-NLS-1$
		final String validationMethod     = p.getProperty(crc + ".validation.type"); //$NON-NLS-1$
		if ("ocsp".equalsIgnoreCase(validationMethod)) { //$NON-NLS-1$
			LOGGER.info("Se usara OCSP para la validacion"); //$NON-NLS-1$
			return new OcspCertificateVerifier(validationProperties);
		}
		else if ("crl".equalsIgnoreCase(validationMethod)) { //$NON-NLS-1$"
			LOGGER.info("Se usaran listas de revocacion para la validacion"); //$NON-NLS-1$
			return new CrlCertificateVerifier(validationProperties);
		}
		throw new IllegalStateException(
			"No se soporta el medio de validacion: " + validationMethod //$NON-NLS-1$
		);
	}

}
