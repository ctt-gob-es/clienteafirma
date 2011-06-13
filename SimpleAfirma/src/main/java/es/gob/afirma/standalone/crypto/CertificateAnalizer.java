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

public abstract class CertificateAnalizer {

    private static final String[] analizers = new String[] {
            "es.gob.afirma.standalone.crypto.DnieCertAnalizer", "es.gob.afirma.standalone.crypto.GenericCertAnalizer"
    };

    /** Recupera la informaci&oacute;n necesaria para la visualizaci&oacute;n y
     * el tratamiento del certificado.
     * @param cert Certificado.
     * @return Informaci&oacute;n del certificado. */
    public static CertificateInfo getCertInformation(X509Certificate cert) {

        CertificateInfo certInfo = null;
        for (String analizerClassName : analizers) {
            try {
                Class<?> analizerClass = Class.forName(analizerClassName);
                CertificateAnalizer analizer = CertificateAnalizer.class.cast(analizerClass.newInstance());

                if (analizer.isValidCert(cert)) {
                    certInfo = analizer.analizeCert(cert);
                    break;
                }
            }
            catch (Exception e) {
                Logger.getLogger("es.gob.afirma").warning("No se pudo cargar el analizador de certificados del DNIe: " + e);
            }
        }

        return certInfo;
    }

    /** Indica si el analizador es capaz de identificar el tipo de certificado.
     * @param cert Certificado del que queremos saber si podemos analizar.
     * @return {@code true} si el analizador puede obtener la informaci&oacute;n necesaria
     *         de este certificado. */
    public abstract boolean isValidCert(X509Certificate cert);

    /** Recupera la informaci&oacute;n necesaria para mostrar el certificado y validarlo.
     * @param cert Certificado que queremos analizar.
     * @return Informaci&oacute;n visual y de validaci&oacute;n del certificado. */
    public abstract CertificateInfo analizeCert(X509Certificate cert);
}
