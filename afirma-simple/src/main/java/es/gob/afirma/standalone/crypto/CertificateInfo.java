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

import javax.swing.Icon;
import javax.swing.ImageIcon;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Informaci&oacute;n para la visualizaci&oacute;n y validaci&oacute;n del certificado.
 * @author Carlos Gamuci Mill&aacute;n */
public final class CertificateInfo {

    /** Icono ilustrativo del Certificado. */
    private final Icon icon;

    private final String iconTooltip;

    /** Nombre com&uacute;n del propietario del certificado. */
    private String holderName;

    /** Nombre com&uacute;n del emisor. */
    private String issuerName;

    /** Construye el objeto con la informaci&oacute;n del certificado.
     * @param cert Certificado al cual se refierre la informaci&oacute;n
     * @param holderName Texto con el titular del certificado.
     * @param i Icono para el certificado
     * @param iTooltip <i>Tooltip</i> para el icono del certificado */
    CertificateInfo(final X509Certificate cert,
    		        final String holderName,
    		        final Icon i,
    		        final String iTooltip) {

    	if (holderName != null && !holderName.isEmpty()) {
    		this.holderName = holderName;
    	}
    	else if (cert != null) {
    		this.holderName = AOUtil.getCN(cert);
    	}
    	else {
    		this.holderName = null;
    	}

    	if (cert != null) {
    		this.issuerName = AOUtil.getCN(cert.getIssuerX500Principal().toString());
    	}
    	else {
        	this.issuerName = null;
        }

        if (i != null) {
        	this.icon = i;
        }
        else {
        	this.icon = new ImageIcon(this.getClass().getResource("/resources/default_cert_ico.png")); //$NON-NLS-1$
        }

        if (iTooltip != null) {
        	this.iconTooltip = iTooltip;
        }
        else {
        	this.iconTooltip = SimpleAfirmaMessages.getString("CertificateInfo.3"); //$NON-NLS-1$
        }
    }

    /** Obtiene el icono del certificado.
     * @return Icono del certificado. */
    public Icon getIcon() {
        return this.icon;
    }

    /** Obtiene el texto del <i>tooltip</i> para el icono del certificado.
     * @return Texto del <i>tooltip</i> para el icono del certificado. */
    public String getIconTooltip() {
    	return this.iconTooltip;
    }

    /**
     * Obtiene el nombre del titular del certificado.
     * @return Texto con el titular del certificado.
     */
	public String getHolderName() {
		return this.holderName;
	}

	/**
	 * Obtiene el nombre del emisor del certificado.
	 * @return Texto con el nombre del emisor del certificado.
	 */
	public String getIssuerName() {
		return this.issuerName;
	}

}
