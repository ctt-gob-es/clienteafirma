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

import javax.swing.Icon;

/** Informaci&oacute;n para la visualizaci&oacute;n y validaci&oacute;n del certificado.
 * @author Carlos gamuci Mill&aacute;n */
public final class CertificateInfo {

    /** Configuraci&oacute;n del OCSP para la validaci&oacute;n del certificado. */
    private OCSPConfig ocspConfig = null;

    /** Icono ilustrativo del Certificado. */
    private Icon icon = null;

    /** Texto descriptivo del certificado. */
    private String descriptionText;

    /** Construye el objeto con la informaci&oacute;n del certificado.
     * @param description Texto descriptivo del certificado. */
    public CertificateInfo(final String description) {
        if (description == null || "".equals(description)) {
            this.descriptionText = "Certificado X509v3";
        }
        else {
            this.descriptionText = "<html><table><tr><td valign=\"middle\"><a href=\"http://certinfo\">" + description + "</a></td></tr></table></html>";
        }
    }

    public OCSPConfig getOcspConfig() {
        return this.ocspConfig;
    }

    public void setOcspConfig(OCSPConfig ocspConfig) {
        this.ocspConfig = ocspConfig;
    }

    public Icon getIcon() {
        return this.icon;
    }

    public void setIcon(Icon icon) {
        this.icon = icon;
    }

    public String getDescriptionText() {
        return this.descriptionText;
    }
}
