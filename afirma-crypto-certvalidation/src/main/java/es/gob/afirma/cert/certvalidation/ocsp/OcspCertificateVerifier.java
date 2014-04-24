package es.gob.afirma.cert.certvalidation.ocsp;

import java.net.URL;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.cert.certvalidation.CertificateVerifier;
import es.gob.afirma.cert.certvalidation.ValidationResult;


/** Validador de certificados X.509v3 por verificaci&oacute;n de revocaci&oacute;n contra
 * OCSP y de periodo de validez contra el reloj del sistema.
 * Clase cedida por <a href="http://www.yohago.com/">YoHago</a>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class OcspCertificateVerifier extends CertificateVerifier {

	private final Properties conf = new Properties();

	/** Construye un validador de certificados por OCSP.
	 * @param confFile Fichero de propiedades con las opciones de configuraci&oacute;n */
	public OcspCertificateVerifier(final String confFile) {
		try {
			this.conf.load(OcspCertificateVerifier.class.getResourceAsStream(confFile));
		}
		catch (final Exception e) {
			throw new IllegalArgumentException(
				"No se ha podido cargar la configuracion del servidor (" + confFile + "): " + e, e //$NON-NLS-1$ //$NON-NLS-2$
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

		// ***********************************************
		// ******** Hacemos ahora la peticion OCSP *******
		// ***********************************************

		final byte[] ocspRequest;
		if (Boolean.parseBoolean(this.conf.getProperty("signOcspRequest"))) { //$NON-NLS-1$
			// Datos necesarios para la firma de peticiones OCSP
			final PrivateKeyEntry pke;
			try {
				pke = OcspHelper.getSignData(
					this.conf.getProperty("signStore"), //$NON-NLS-1$
					this.conf.getProperty("signStorePass"), //$NON-NLS-1$
					this.conf.getProperty("signAlias") //$NON-NLS-1$
				);
			}
			catch (final Exception e) {
				LOGGER.severe("Error obteniendo los datos de firma de peticiones OCSP: " + e); //$NON-NLS-1$
				return ValidationResult.SERVER_ERROR;
			}

			// Creamos la peticion OCSP ASN.1 firmada
			try {
				ocspRequest = OcspHelper.createSignedOcspRequest(cert, this.issuerCert, pke);
			}
			catch (final Exception e) {
				LOGGER.severe("Error creando la peticion OCSP firmada: " + e); //$NON-NLS-1$
				return ValidationResult.SERVER_ERROR;
			}
		}
		else {
			try {
				ocspRequest = OcspHelper.createOcspRequest(cert, this.issuerCert);
			}
			catch (final Exception e) {
				LOGGER.severe("Error creando la peticion OCSP: " + e); //$NON-NLS-1$
				return ValidationResult.SERVER_ERROR;
			}
		}

		// Enviamos la peticion
		final URL responderUrl;
		try {
			responderUrl = new URL(this.conf.getProperty("responderUrl")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.severe("No se ha configurado una URL de servicio OCSP valida: " + e); //$NON-NLS-1$
			return ValidationResult.SERVER_ERROR;
		}
		final byte[] rawOcspResponse;
		try {
			rawOcspResponse = OcspHelper.sendOcspRequest(responderUrl, ocspRequest);
		}
		catch (final Exception e) {
			LOGGER.severe("Error enviado la peticion OCSP al servidor: " + e); //$NON-NLS-1$
			return ValidationResult.SERVER_ERROR;
		}
		try {
			return OcspHelper.analyzeOcspResponse(rawOcspResponse);
		}
		catch (final Exception e) {
			LOGGER.severe("Error analizando la respuesta del servidor OCSP: " + e); //$NON-NLS-1$
			return ValidationResult.SERVER_ERROR;
		}
	}

}
