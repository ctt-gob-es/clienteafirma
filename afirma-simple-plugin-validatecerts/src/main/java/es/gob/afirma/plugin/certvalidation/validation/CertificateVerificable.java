/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.certvalidation.validation;

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
	 * @throws CertificateException Si el certificado o la firma no son v&aacute;lidos.
	 * @throws SignatureException Fallo en la verificaci&oacute;n del emisor. */
	void verifyIssuer(final X509Certificate cert) throws CertificateException, SignatureException;

	/** Valida un certificado X.509v3.
	 * @param cert Certificado a validar.
	 * @return Resultado de la validaci&oacute;n. */
	ValidationResult validateCertificate(final X509Certificate cert);
}
