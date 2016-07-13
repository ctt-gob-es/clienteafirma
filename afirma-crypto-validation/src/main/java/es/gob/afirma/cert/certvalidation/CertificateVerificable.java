package es.gob.afirma.cert.certvalidation;

import java.security.SignatureException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

/** Interfaz para los validadores de certificado.
 * @author Sergio Mart&iacute;nez Rico. */
public interface CertificateVerificable {
	
	/** Define el certificado X.509v3.
	 * @param cert Certificado a definir. */
	void setSubjectCert(final X509Certificate cert);
	
	/** Define el emisor del certificado X.509v3.
	 * @param cert Certificado del emimsor a definir. */
	void setIssuerCert(final X509Certificate cert);
	
	/** Define las propiedades del certificado X.509v3.
	 * @param properties Propiedades a definir. */
	void setValidationProperties(String properties);

	/** Valida el certificado X.509v3 que se ha proporcionado en el constructor.
	 * @return Resultado de la validaci&oacute;n. */
	ValidationResult validateCertificate();

	/** Valida si el certificado X.509v3 que se ha proporcionado en el constructor est&aacute; revocado.
	 * @param cert Certificado a validar.
	 * @return Resultado de la validaci&oacute;n. */
	abstract ValidationResult verifyRevocation(final X509Certificate cert);

	/** Valida el emisor del certificado X.509v3.
	 * @param cert Certificado a validar.
	 * @exception CertificateException, SignatureException Si el certificado o la firma no son v&aacute;lidos.
	 * @throws SignatureException Fallo en la verificaci&oacute;n del emisor. */
	void verifyIssuer(final X509Certificate cert) throws CertificateException, SignatureException;
	
	/** Valida un certificado X.509v3.
	 * @param cert Certificado a validar.
	 * @return Resultado de la validaci&oacute;n. */
	ValidationResult validateCertificate(final X509Certificate cert);
}
