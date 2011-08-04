/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.install;

import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

/** Certificados permitidas para la firma de ficheros JAR y ZIP. */
enum SigningCA {
    /** CA de Sun/Oracle con la que se firman las extensiones de Java. */
    SUN("/resources/sun/JCE_Code_Signing.cer"),
    /** CA del integrador con la que se firman las dependencias del Cliente. */
    INTEGRATOR("/resources/integrator_Code_Signing.cer");

    /** Define la CA con la ruta interna al JAR del certificado.
     * @param caCertificatePath Ruta interna de la CA. */
    private SigningCA(final String sigCertificatePath) {
        try {
            this.sgCert =
                (X509Certificate) CertificateFactory.getInstance("X.509")
                .generateCertificate(AOInstallUtils.class.getResourceAsStream(sigCertificatePath));
        }
        catch (final Exception e) {
            throw new UnsupportedOperationException("No se ha podido cargar el certificado firmante: " + sigCertificatePath, e);
        }
    }

    private final X509Certificate sgCert;

    X509Certificate getSigningCertificate() {
        return this.sgCert;
    }

}
