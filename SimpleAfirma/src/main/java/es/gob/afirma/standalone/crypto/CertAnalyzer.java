/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone.crypto;

import java.security.cert.X509Certificate;
import java.util.logging.Logger;

/**
 * Analizador de certificados para visualizaci&oacute;n de detalles en pantalla.
 *
 */
public abstract class CertAnalyzer {

    private static final String[] analyzers = new String[] {
            "es.gob.afirma.standalone.crypto.DnieCertAnalyzer",   //$NON-NLS-1$
            "es.gob.afirma.standalone.crypto.GenericCertAnalyzer" //$NON-NLS-1$
    };

    /** Recupera la informaci&oacute;n necesaria para la visualizaci&oacute;n y
     * el tratamiento del certificado.
     * @param cert Certificado.
     * @return Informaci&oacute;n del certificado. */
    public static CertificateInfo getCertInformation(X509Certificate cert) {
        for (final String analyzerClassName : analyzers) {
            try {
                final Class<?> analyzerClass = Class.forName(analyzerClassName);
                final CertAnalyzer analyzer = CertAnalyzer.class.cast(analyzerClass.newInstance());
                if (analyzer.isValidCert(cert)) {
                    return analyzer.analyzeCert(cert);
                }
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("No se pudo cargar un analizador de certificados: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        return null;
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
