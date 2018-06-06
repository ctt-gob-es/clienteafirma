/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.certvalidation.validation;

import java.net.URL;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.List;

/** Validador de certificados X.509v3 por verificaci&oacute;n de revocaci&oacute;n contra
 * OCSP y de periodo de validez contra el reloj del sistema.
 * Clase cedida por <a href="http://www.yohago.com/">YoHago</a>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class OcspCertificateVerifier extends CertificateVerifier {

	/** Construye un validador de certificados por OCSP.
	 * @param cert Certificado inicialmente a validar. */
	@Override
	public void setSubjectCert(final X509Certificate cert) {
		super.setSubjectCert(cert);
		if(getValidationProperties() != null) {
			try {
				getValidationProperties().setProperty(
					"responderUrl", //$NON-NLS-1$
					getBestResponder(OcspHelper.getAIALocations(cert))
				);
			}
			catch (final Exception e) {
				throw new IllegalArgumentException(e);
			}
			getValidationProperties().setProperty("signOcspRequest", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	@Override
	public ValidationResult verifyRevocation(final X509Certificate cert) {

		// ***********************************************
		// ******** Hacemos ahora la peticion OCSP *******
		// ***********************************************

		final byte[] ocspRequest;
		if (Boolean.parseBoolean(getValidationProperties().getProperty("signOcspRequest"))) { //$NON-NLS-1$
			// Datos necesarios para la firma de peticiones OCSP
			final PrivateKeyEntry pke;
			try {
				pke = OcspHelper.getSignData(
					getValidationProperties().getProperty("signStore"), //$NON-NLS-1$
					getValidationProperties().getProperty("signStorePass"), //$NON-NLS-1$
					getValidationProperties().getProperty("signAlias") //$NON-NLS-1$
				);
			}
			catch (final Exception e) {
				LOGGER.severe("Error obteniendo los datos de firma de peticiones OCSP: " + e); //$NON-NLS-1$
				return ValidationResult.SERVER_ERROR;
			}

			// Creamos la peticion OCSP ASN.1 firmada
			try {
				ocspRequest = OcspHelper.createSignedOcspRequest(cert, getIssuerCert(), pke);
			}
			catch (final Exception e) {
				LOGGER.severe("Error creando la peticion OCSP firmada: " + e); //$NON-NLS-1$
				return ValidationResult.SERVER_ERROR;
			}
		}
		else {
			try {
				ocspRequest = OcspHelper.createOcspRequest(
					cert,
					getIssuerCert()
				);
			}
			catch (final Exception e) {
				LOGGER.severe("Error creando la peticion OCSP: " + e); //$NON-NLS-1$
				return ValidationResult.SERVER_ERROR;
			}
		}

		// Enviamos la peticion
		final URL responderUrl;
		try {
			responderUrl = new URL(getValidationProperties().getProperty("responderUrl")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.severe("No se ha configurado una URL de servicio OCSP valida: " + e); //$NON-NLS-1$
			return ValidationResult.SERVER_ERROR;
		}
		LOGGER.info("Se usara el siguiente OCSP para validar el certificado: " + responderUrl); //$NON-NLS-1$
		final byte[] rawOcspResponse;
		try {
			rawOcspResponse = OcspHelper.sendOcspRequest(responderUrl, ocspRequest);
		}
		catch (final Exception e) {
			LOGGER.severe(
				"Error enviado la peticion OCSP al servidor (" + responderUrl + "): " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
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

	private static String getBestResponder(final List<String> responders) {
		if (responders == null || responders.isEmpty()) {
			throw new IllegalArgumentException("No hay servidores OCSP configurados"); //$NON-NLS-1$
		}
		String best = responders.get(0);
		if (responders.size() > 1) {
			for (int i=1;i<responders.size();i++) {
				if (responders.get(i).toLowerCase().contains("ocsp")) { //$NON-NLS-1$
					best = responders.get(i);
				}
			}
		}
		return best;
	}

}