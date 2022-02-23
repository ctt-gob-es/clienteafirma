/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.security.cert.X509Certificate;

/**
 * Implementaci&oacute;n para gestionar eventos en links para certificados.
 * @author Jos&eacute;s Montero Rivero.
 * */
public final class CertInfoLabelLinkImpl implements LabelLinkListener{

    private final X509Certificate cert;

    /**
     * Crea un nuevo gestor para certificados.
     * @param cert Certificado que se debe abrir al pulsar sobre la URL.
     */
    public CertInfoLabelLinkImpl (final X509Certificate cert) {
    	this.cert = cert;
    }

	@Override
	public void openLink() {
		SignDataPanel.openCertificate(this.cert);
	}

}
