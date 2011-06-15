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
    private final OCSPConfig ocspConfig;

    /** Icono ilustrativo del Certificado. */
    private final Icon icon;
    
    private final String iconTooltip;

    /** Texto descriptivo del certificado. */
    private String descriptionText;

    /** Construye el objeto con la informaci&oacute;n del certificado.
     * @param description Texto descriptivo del certificado. 
     * @param ocsp Configuraci&oacute;n de OCSP para la validaci&oacute;n del certificado
     * @param i Icono para el certificado
     * @param iTooltip <i>Tooltip</i> para el icono del certificado */
    public CertificateInfo(final String description, final OCSPConfig ocsp, final Icon i, final String iTooltip) {
        if (description == null || "".equals(description)) {
            this.descriptionText = "Certificado generico X.509v3";
        }
        else {
            this.descriptionText = "<html><br><a href=\"http://certinfo\">" + description + "</a></html>";
        }
        this.ocspConfig = ocsp;
        this.icon = i;
        this.iconTooltip = iTooltip;
    }

    /**
     * Obtiene la configuraci&oacute;n OCSP para validar el certificado.
     * @return Configuraci&oacute;n OCSP para validar el certificado
     */
    public OCSPConfig getOcspConfig() {
        return this.ocspConfig;
    }

    
    /** Obtiene el icono del certificado.
     * @return Icono del certificado
     */
    public Icon getIcon() {
        return this.icon;
    }
    
    /** Obtiene el texto del <i>tooltip</i> para el icono del certificado.
     * @return Texto del <i>tooltip</i> para el icono del certificado.
     */
    public String getIconTooltip() {
    	return this.iconTooltip;
    }

    /** Obtiene un texto descriptivo del certificado.
     * @return Descripci&oacute;n del certificado
     */
    public String getDescriptionText() {
        return this.descriptionText;
    }
}
