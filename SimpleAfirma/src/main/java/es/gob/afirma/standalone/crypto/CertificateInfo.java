package es.gob.afirma.standalone.crypto;

import java.awt.Image;

/**
 * Informaci&oacute;n para la visualizaci&oacute;n y validaci&oacute;n del certificado.
 * @author Carlos gamuci Mill&aacute;n
 */
public final class CertificateInfo {
 
    /** Configuraci&oacute;n del OCSP para la validaci&oacute;n del certificado. */
    private OCSPConfig ocspConfig = null;
    
    /** Icono ilustrativo del Certificado. */
    private Image icon = null;
    
    /** Texto descriptivo del certificado. */
    private String descriptionText;
    
    /**
     * Construye el objeto con la informaci&oacute;n del certificado.
     * @param description Texto descriptivo del certificado.
     */
    public CertificateInfo(final String description) {
        if (description == null) {
        	this.descriptionText = "Certificado X509v3";
        }
        else{
        	this.descriptionText = description;
        }
    }

    public OCSPConfig getOcspConfig() {
        return this.ocspConfig;
    }

    public void setOcspConfig(OCSPConfig ocspConfig) {
        this.ocspConfig = ocspConfig;
    }

    public Image getIcon() {
        return this.icon;
    }

    public void setIcon(Image icon) {
        this.icon = icon;
    }

    public String getDescriptionText() {
        return this.descriptionText;
    }
}
