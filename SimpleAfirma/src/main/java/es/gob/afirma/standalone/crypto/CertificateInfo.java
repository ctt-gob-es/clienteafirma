package es.gob.afirma.standalone.crypto;

import java.awt.Image;

/**
 * Informaci&oacute;n para la visualizaci&oacute;n y validaci&oacute;n del certificado.
 * @author Carlos gamuci Mill&aacute;n
 */
public class CertificateInfo {

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
    public CertificateInfo(String description) {
        if (description == null)
            throw new NullPointerException("Debe introducirse un texto descriptor del certificado");
        
        this.descriptionText = description;
    }

    public OCSPConfig getOcspConfig() {
        return ocspConfig;
    }

    public void setOcspConfig(OCSPConfig ocspConfig) {
        this.ocspConfig = ocspConfig;
    }

    public Image getIcon() {
        return icon;
    }

    public void setIcon(Image icon) {
        this.icon = icon;
    }

    public String getDescriptionText() {
        return descriptionText;
    }
}
