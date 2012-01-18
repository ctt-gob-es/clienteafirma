/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.ui.visor.crypto;

import java.security.cert.X509Certificate;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.util.AOCertVerifier;

/** Informaci&oacute;n para la visualizaci&oacute;n y validaci&oacute;n del certificado.
 * @author Carlos gamuci Mill&aacute;n */
public final class CertificateInfo {

    /** Configuraci&oacute;n del OCSP para la validaci&oacute;n del certificado. */
    private final AOCertVerifier ocspConfig;

    /** Icono ilustrativo del Certificado. */
    private final Icon icon;

    private final String iconTooltip;

    /** Texto descriptivo del certificado. */
    private String descriptionText;

    /** Construye el objeto con la informaci&oacute;n del certificado.
     * @param cert Certificado al cual se refierre la informaci&oacute;n
     * @param description Texto descriptivo del certificado.
     * @param ocsp Configuraci&oacute;n de OCSP para la validaci&oacute;n del certificado
     * @param i Icono para el certificado
     * @param iTooltip <i>Tooltip</i> para el icono del certificado */
    CertificateInfo(final X509Certificate cert, final String description, final AOCertVerifier ocsp, final Icon i, final String iTooltip) {

    	if (description == null || "".equals(description)) {  //$NON-NLS-1$
        	if (cert == null) {
        		this.descriptionText = Messages.getString("CertificateInfo.0"); //$NON-NLS-1$
        	}
        	else {
        		this.descriptionText = "<html>" + ((Platform.OS.MACOSX.equals(Platform.getOS())) ? Constants.HTML_SALTO_LINEA : "") + Messages.getString("CertificateInfo.1") + ": <a href=\"#\">" + AOUtil.getCN(cert) + "</a>. " + Messages.getString("CertificateInfo.2") + ": <a href=\"#\">" + AOUtil.getCN(cert.getIssuerX500Principal().toString()) + "</a>" + "</html>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$
        	}
        }
        else {
            this.descriptionText = "<html>" + ((Platform.OS.MACOSX.equals(Platform.getOS())) ? Constants.HTML_SALTO_LINEA : "") + "<a href=\"#\">" + description + "</a>" + "</html>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
        }

    	this.ocspConfig = ocsp;

        if (i == null) {
        	this.icon = new ImageIcon(this.getClass().getResource("/resources/images/default_cert_ico.png")); //$NON-NLS-1$
        }
        else {
        	this.icon = i;
        }

        if (iTooltip == null) {
        	this.iconTooltip = Messages.getString("CertificateInfo.3"); //$NON-NLS-1$
        }
        else {
        	this.iconTooltip = iTooltip;
        }

    }

    /**
     * Obtiene la configuraci&oacute;n OCSP para validar el certificado.
     * @return Configuraci&oacute;n OCSP para validar el certificado
     */
    public AOCertVerifier getCertVerifier() {
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
