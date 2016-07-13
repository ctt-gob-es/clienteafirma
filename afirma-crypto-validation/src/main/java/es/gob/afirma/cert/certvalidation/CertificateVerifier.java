/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.cert.certvalidation;

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SignatureException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;

/** Validador gen&eacute;rico de certificados X.509. Como clase base comprueba &uacute;nicamente el
 * periodo de validez contra el reloj del sistema y la firma por parte de la CA.
 * Clase cedida por <a href="http://www.yohago.com/">YoHago</a>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public abstract class CertificateVerifier implements CertificateVerificable {

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private X509Certificate certificate = null;
	private Properties conf = new Properties();
	
	@Override
	public void setSubjectCert(final X509Certificate c) {
		this.certificate = c;
	}

	protected X509Certificate getCertificate() {
		return this.certificate;
	}
	
	protected Properties getValidationProperties() {
		return this.conf;
	}
	
	@Override
	public void setValidationProperties(String confFile) {
		try {
			this.conf.load(CertificateVerifier.class.getResourceAsStream(confFile));
		}
		catch (final Exception e) {
			throw new IllegalArgumentException(
				"No se ha podido cargar la configuracion del servidor (" + confFile + ": " + e, e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		
		final String issuerCertFile = this.conf.getProperty("issuerCertFile"); //$NON-NLS-1$
		if (issuerCertFile != null) {
			try {
				setIssuerCert(
					(X509Certificate) CertificateFactory.getInstance(
						"X.509" //$NON-NLS-1$
					).generateCertificate(
						CertificateVerifier.class.getResourceAsStream(issuerCertFile)
					)
				);
			}
			catch (final CertificateException e) {
				throw new IllegalArgumentException(
					"No se ha podido cargar el certificado raiz del emisor (" + issuerCertFile + "): " + e, e //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}
	}
	
	private X509Certificate issuerCert;
	
	@Override
	public void setIssuerCert(final X509Certificate cert) {
		this.issuerCert = cert;
	}
	protected X509Certificate getIssuerCert() {
		return this.issuerCert;
	}

	/** Valida el certificado X.509v3 que se ha proporcionado en el constructor.
	 * @return Resultado de la validaci&oacute;n */
	@Override
	public ValidationResult validateCertificate() {
		return validateCertificate(this.certificate);
	}

	@Override
	public abstract ValidationResult verifyRevocation(final X509Certificate cert);

	@Override
	public void verifyIssuer(final X509Certificate cert) throws CertificateException, SignatureException {

		if (cert == null) {
			throw new CertificateException("Se ha proporcionado un certificado nulo"); //$NON-NLS-1$
		}

		// Compruebo el Principal X.500 del emisor
		if (!this.issuerCert.getSubjectX500Principal().toString().equals(cert.getIssuerX500Principal().toString())) {
			LOGGER.info(
				"El certificado proporcionado no esta emitido por '" + this.issuerCert.getSubjectX500Principal() + "', sino por '" + cert.getIssuerX500Principal() + "'" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			);
			throw new SignatureException(
				"El certificado proporcionado no esta emitido por '" + this.issuerCert.getSubjectX500Principal() + "', sino por '" + cert.getIssuerX500Principal() + "'" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			);
		}
		LOGGER.info("El certificado a validar ha sido emitido por: " + AOUtil.getCN(cert.getIssuerX500Principal().toString())); //$NON-NLS-1$

        // Compruebo ahora la firma
		try {
			cert.verify(this.issuerCert.getPublicKey());
		}
		catch (final InvalidKeyException e) {
			throw new CertificateException(e);
		}
		catch (final NoSuchAlgorithmException e) {
			throw new CertificateException(e);
		}
		catch (final NoSuchProviderException e) {
			throw new CertificateException(e);
		}
	}

	/** Valida un certificado X.509v3.
	 * @param cert Certificado a validar
	 * @return Resultado de la validaci&oacute;n */
	@Override
	public ValidationResult validateCertificate(final X509Certificate cert) {

		if (cert == null) {
			LOGGER.warning("Se ha proporcionado un certificado a validar nulo"); //$NON-NLS-1$
			return ValidationResult.CORRUPT;
		}

		// Comprobamos que este dentro de su periodo de validez
		if (new java.util.Date().after(cert.getNotAfter())) {
			LOGGER.info(
				"Se ha proporcionado un certificado que caduco en: " + cert.getNotAfter() //$NON-NLS-1$
			);
			return ValidationResult.EXPIRED;
		}
		if (new java.util.Date().before(cert.getNotBefore())) {
			LOGGER.info(
				"Se ha proporcionado un certificado que aun no es valido, lo sera a partir de: " + cert.getNotBefore() //$NON-NLS-1$
			);
			return ValidationResult.NOT_YET_VALID;
		}

		// Comprobamos el emisor
		if (this.issuerCert != null) {
			try {
				verifyIssuer(cert);
			}
			catch(final SignatureException e) {
				return ValidationResult.CA_NOT_SUPPORTED;
			}
			catch(final Exception e) {
				LOGGER.severe("Error durante la verificacion del emisor del certificado: " + e); //$NON-NLS-1$
				return ValidationResult.SERVER_ERROR;
			}
		}

		return verifyRevocation(cert);

	}
}