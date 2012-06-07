/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.install;

import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

/** Certificados permitidas para la firma de ficheros JAR y ZIP. */
enum SigningCert {
    /** Certificado de Sun/Oracle con el que se firman las extensiones de Java. */
    SUN("/resources/sun/Sun_Microsystem_Inc.cer"), //$NON-NLS-1$
    /** Certificado del integrador con el que se firman las dependencias del Cliente. */
    INTEGRATOR("/resources/integrator_Code_Signing.cer"); //$NON-NLS-1$

    /** Define el certificado con su  ruta interna en el JAR.
     * @param sigCertificatePath Ruta interna del certificado. */
    private SigningCert(final String sigCertificatePath) {
        try {
            this.sgCert =
                (X509Certificate) CertificateFactory.getInstance("X.509") //$NON-NLS-1$
                .generateCertificate(AOInstallUtils.class.getResourceAsStream(sigCertificatePath));
        }
        catch (final Exception e) {
            throw new IllegalArgumentException("No se ha podido cargar el certificado firmante: " + sigCertificatePath, e); //$NON-NLS-1$
        }
    }

    private final X509Certificate sgCert;

    X509Certificate getSigningCertificate() {
        return this.sgCert;
    }

}
