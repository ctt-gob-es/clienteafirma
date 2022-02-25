/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Desktop;
import java.net.URI;
import java.net.URISyntaxException;

import javax.swing.JOptionPane;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * Implementaci&oacute;n para gestionar eventos en URLs.
 * @author Jos&eacute;s Montero Rivero.
 * */
public final class URLLabelLinkImpl implements LabelLinkListener{

    private URI url = null;

    /**
     * Crea un nuevo gestor para URLs.
     * @param url URL que se debe abrir.
     */
    public URLLabelLinkImpl (final String url) {
    	try {
			this.url = new URI(url);
		} catch (final URISyntaxException e) {
			AOUIFactory.showErrorMessage(
					SimpleAfirmaMessages.getString("SignResultPanel.0") + SimpleAfirmaMessages.getString("SignResultPanel.33"), //$NON-NLS-1$ //$NON-NLS-2$
		            SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
		            JOptionPane.ERROR_MESSAGE,
		            e
		        );
		}
    }

	@Override
	public void openLink() {
		try {
			Desktop.getDesktop().browse(this.url);
		} catch (final Exception ex) {
			AOUIFactory.showErrorMessage(
					SimpleAfirmaMessages.getString("SignResultPanel.0") + SimpleAfirmaMessages.getString("SignResultPanel.33"), //$NON-NLS-1$ //$NON-NLS-2$
		            SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
		            JOptionPane.ERROR_MESSAGE,
		            ex
		        );
		}
	}

}
