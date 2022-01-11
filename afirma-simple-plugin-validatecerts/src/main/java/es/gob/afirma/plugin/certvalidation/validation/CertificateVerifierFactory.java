/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.certvalidation.validation;

import java.io.InputStream;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.zip.CRC32;


/** Factor&iacute;a para la obtenci&oacute;n de un validador de certificados.
 * Clase cedida por <a href="http://www.yohago.com/">YoHago</a>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CertificateVerifierFactory {

	private static Properties p = null;

	private static final String FACTORY_CONFIGURATION = "/es/gob/afirma/plugin/certvalidation/validationfactory.properties"; //$NON-NLS-1$

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
	 * @param cert Certificado a validar.
	 * @return Validador para el certificado proporcionado.
	 * @throws CertificateVerifierFactoryException Si no se conocen mecanismos de validaci&oacute;n
	 *                                             para los certificados del emisor indicado.*/
	public static CertificateVerificable getCertificateVerifier(final X509Certificate cert) throws CertificateVerifierFactoryException {
		if (cert == null) {
			throw new IllegalArgumentException("El certificado no puede ser nulo"); //$NON-NLS-1$
		}
		if (p == null) {
			p = new Properties();
			try (
				final InputStream is = CertificateVerifierFactory.class.getResourceAsStream(FACTORY_CONFIGURATION)
			) {
				p.load(is);
			}
			catch (final Exception e) {
				p = null;
				throw new IllegalStateException(
					"No se ha podido cargar la configuracion de la factoria: " + e, e //$NON-NLS-1$
				);
			}
		}

		String crc = getIssuerIdentifier(cert);
		LOGGER.info("Identificador del emisor del certificado: " + crc); //$NON-NLS-1$

		final String validationClass = p.getProperty(crc + ".validation.type"); //$NON-NLS-1$

		if (p.getProperty(crc + ".validation.properties") == null) { //$NON-NLS-1$
			crc = "default"; //$NON-NLS-1$
		}

		final String validationProperties = p.getProperty(crc + ".validation.properties"); //$NON-NLS-1$

		final CertificateVerificable certVerif;
		if ("ocsp".equalsIgnoreCase(validationClass)) { //$NON-NLS-1$
			certVerif = new OcspCertificateVerifier();
		}
		else if ("crl".equalsIgnoreCase(validationClass) || validationClass == null) { //$NON-NLS-1$
			certVerif = new CrlCertificateVerifier();
		}
		else {
			try {
				final Class<?> certVerifierClass = Class.forName(validationClass);
				certVerif = (CertificateVerificable) certVerifierClass.getConstructor().newInstance();
			}
			catch (final ClassNotFoundException e) {
				LOGGER.warning("No se encuentra la clase validadora: " + e); //$NON-NLS-1$
				throw new CertificateVerifierFactoryException(
					"No se encuentran la clase validadora: " + e, e //$NON-NLS-1$
				);
			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido instanciar el verificador del certificado: " + e); //$NON-NLS-1$
				throw new CertificateVerifierFactoryException(
					"No se ha podido instanciar el verificador del certificado: " + e, e //$NON-NLS-1$
				);
			}
		}
		certVerif.setValidationProperties(validationProperties);
		certVerif.setSubjectCert(cert);
		return certVerif;
	}

}
