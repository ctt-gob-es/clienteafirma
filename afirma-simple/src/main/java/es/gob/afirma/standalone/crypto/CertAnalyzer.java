/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.crypto;

import java.security.cert.X509Certificate;

/** Analizador de certificados para visualizaci&oacute;n de detalles en pantalla. */
public abstract class CertAnalyzer {

    /** Recupera la informaci&oacute;n necesaria para la visualizaci&oacute;n y
     * el tratamiento del certificado.
     * @param cert Certificado.
     * @return Informaci&oacute;n del certificado. */
    public static CertificateInfo getCertInformation(final X509Certificate cert) {
    	if (cert.getIssuerX500Principal().toString().contains("AC DNIE")) { //$NON-NLS-1$
    		return new DnieCertAnalyzer().analyzeCert(cert);
    	}
    	return new GenericCertAnalyzer().analyzeCert(cert);
    }

    /** Indica si el analizador es capaz de identificar el tipo de certificado.
     * @param cert Certificado del que queremos saber si podemos analizar.
     * @return {@code true} si el analizador puede obtener la informaci&oacute;n necesaria
     *         de este certificado. */
    public abstract boolean isValidCert(X509Certificate cert);

    /** Recupera la informaci&oacute;n necesaria para mostrar el certificado y validarlo.
     * @param cert Certificado que queremos analizar.
     * @return Informaci&oacute;n visual y de validaci&oacute;n del certificado. */
    public abstract CertificateInfo analyzeCert(X509Certificate cert);
}
