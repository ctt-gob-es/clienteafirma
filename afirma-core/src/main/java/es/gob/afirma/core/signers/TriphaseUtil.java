/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.util.Properties;

import es.gob.afirma.core.misc.Base64;

/** Utilidades generales para firmas trif&aacute;sicas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TriphaseUtil {

    /** Indica, mediante un <code>true</code> o <code>false</code>, que debe incluirse en la firma
     * &uacute;nicamente el certificado utilizado para firmar y no su cadena de certificaci&oacute;n
     * completa. Por defecto, se incluir&aacute; toda la cadena de certificaci&oacute;n
     * (propiedad compartida con CAdES y PAdES). */
    private static final String INCLUDE_ONLY_SIGNNING_CERTIFICATE = "includeOnlySignningCertificate";//$NON-NLS-1$

	/** Separador que debe usarse para incluir varios certificados dentro del mismo par&aacute;metro. */
	private static final String PARAM_NAME_CERT_SEPARATOR = ","; //$NON-NLS-1$

	private TriphaseUtil() {
		// No instanciable
	}

	/** Compone un texto con los certificados de la cadena indicada codificados en Base64 de tipo <i>URL-Safe</i> concatenados
	 * usando el separador por defecto.
	 * @param certChain Cadena de certificados.
	 * @param extraParams Par&aacute;metros adicionales de la firma, para comprobar si hay que incluir la cadena
	 *                     completa de certificados o solo el certificado del firmante.
	 * @return Texto con los certificados de la cadena codificados en Base64 de tipo <i>URL-Safe</i> y concatenados
	 *         usando el separador por defecto.
	 * @throws CertificateEncodingException Si hay problemas obteniendo la codificaci&oacute;n de alg&uacute;n certificado. */
	public static String prepareCertChainParam(final Certificate[] certChain, final Properties extraParams) throws CertificateEncodingException {
		if (certChain == null || certChain.length < 1) {
			throw new IllegalArgumentException(
				"La cadena de certificados no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}
		if (extraParams == null || Boolean.parseBoolean(extraParams.getProperty(INCLUDE_ONLY_SIGNNING_CERTIFICATE, Boolean.FALSE.toString()))) {
			return Base64.encode(certChain[0].getEncoded(), true);
		}
		final StringBuilder sb = new StringBuilder();
		for (final Certificate cert : certChain) {
			sb.append(Base64.encode(cert.getEncoded(), true));
			sb.append(PARAM_NAME_CERT_SEPARATOR);
		}
		final String ret = sb.toString();
		return ret.substring(0, ret.length() - PARAM_NAME_CERT_SEPARATOR.length());
	}

}
