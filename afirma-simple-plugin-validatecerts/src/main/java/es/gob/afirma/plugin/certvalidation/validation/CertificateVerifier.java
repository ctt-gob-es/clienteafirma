/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.certvalidation.validation;

import java.io.IOException;
import java.io.InputStream;
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

/** Validador gen&eacute;rico de certificados X&#46;509.
 * Como clase base comprueba &uacute;nicamente el
 * periodo de validez contra el reloj del sistema y la firma por parte de la CA.
 * Clase cedida por <a href="http://www.yohago.com/">YoHago</a>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public abstract class CertificateVerifier implements CertificateVerificable {

	/** <code>Logger</code> para uso en esta clase y sus derivadas. */
	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private X509Certificate certificate = null;
	private final Properties conf = new Properties();

	@Override
	public void setSubjectCert(final X509Certificate c) {
		this.certificate = c;
	}

	/** Obtiene el certificado cargado para validar.
	 * @return Certificado cargado para validar. */
	protected X509Certificate getCertificate() {
		return this.certificate;
	}

	/** Obtiene las propiedades de cnfiguraci&oacute;n para la validaci&oacute;n.
	 * @return Propiedades de cnfiguraci&oacute;n para la validaci&oacute;n. */
	protected Properties getValidationProperties() {
		return this.conf;
	}

	@Override
	public void setValidationProperties(final String confFile) {
		if (confFile == null) {
			LOGGER.warning(
				"No hay configuracion especifica de validacion, se usaran los valores por defecto" //$NON-NLS-1$
			);
			return;
		}
		try (
			final InputStream is = CertificateVerifier.class.getResourceAsStream(confFile)
		) {
			this.conf.load(is);
		}
		catch (final Exception e) {
			throw new IllegalArgumentException(
				"No se ha podido cargar la configuracion del verificador (" + confFile + ": " + e, e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		final String issuerCertFile = this.conf.getProperty("issuerCertFile"); //$NON-NLS-1$
		if (issuerCertFile != null) {
			try (
				final InputStream is = CertificateVerifier.class.getResourceAsStream(issuerCertFile)
			) {
				setIssuerCert(
					(X509Certificate) CertificateFactory.getInstance(
						"X.509" //$NON-NLS-1$
					).generateCertificate(is)
				);
			}
			catch (final CertificateException | IOException e) {
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

	/** Obtiene el certificado del emisor del certificado a validar.
	 * @return Certificado del emisor del certificado a validar. */
	protected X509Certificate getIssuerCert() {
		return this.issuerCert;
	}

	/** Valida el certificado X&#46;509v3 que se ha proporcionado en el constructor.
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
		catch (final InvalidKeyException | NoSuchAlgorithmException | NoSuchProviderException e) {
			throw new CertificateException(e);
		}
	}

	/** Valida un certificado X&#46;509v3.
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
				LOGGER.info(
					"Encontrada CA no soportada durante la validacion del certificado: " + e //$NON-NLS-1$
				);
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