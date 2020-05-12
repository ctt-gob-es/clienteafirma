/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Image;

import javax.swing.JComponent;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

enum FileType {

	PDF(
		"icon_pdf_large.png", //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.0"), //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.9") //$NON-NLS-1$
	),
	BINARY(
		"icon_binary_large.png", //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.12"), //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.11") //$NON-NLS-1$
	),
	XML(
		"icon_xml_large.png", //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.8"), //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.10") //$NON-NLS-1$
	),
	FACTURAE(
		"icon_facturae_large.png", //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.17"), //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.20") //$NON-NLS-1$
	),
	SIGN_CADES(
		"icon_sign_large.png", //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.6") + " (CAdES)", //$NON-NLS-1$ //$NON-NLS-2$
		SimpleAfirmaMessages.getString("SignPanel.39") + " (CAdES)" //$NON-NLS-1$ //$NON-NLS-2$
	),
	SIGN_XADES(
		"icon_sign_large.png", //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.6") + " (XAdES)", //$NON-NLS-1$ //$NON-NLS-2$
		SimpleAfirmaMessages.getString("SignPanel.39") + " (XAdES)" //$NON-NLS-1$ //$NON-NLS-2$
	),
	OOXML(
		"icon_office_win_large.png", //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.38"), //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.37") //$NON-NLS-1$
	),
	ODF(
		"icon_openoffice_large.png", //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.32"), //$NON-NLS-1$
		SimpleAfirmaMessages.getString("SignPanel.31") //$NON-NLS-1$
	);

	private final String fileDescription;
	private final Component fileTypeScalableIcon;

	private FileType(final String iconFile, final String tooltip, final String description) {
		this.fileDescription = description;
		this.fileTypeScalableIcon = createScalablePane(
			iconFile,
			tooltip
		);
	}

	private static JComponent createScalablePane(final String iconFile, final String iconTooltip) {

		final Image icon = ImageLoader.loadImage(iconFile);
        final ScalablePane scalableIcon = new ScalablePane(icon, true);
        scalableIcon.setFocusable(false);
        scalableIcon.setToolTipText(iconTooltip);
        scalableIcon.setBackground(new Color(255, 255, 255, 0));

        return scalableIcon;
	}

	Component getIcon() {
		return this.fileTypeScalableIcon;
	}

	String getFileDescription() {
		return this.fileDescription;
	}
}
