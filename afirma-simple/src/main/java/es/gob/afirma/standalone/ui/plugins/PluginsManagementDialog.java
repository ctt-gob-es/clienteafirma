/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.plugins;

import java.awt.Dimension;
import java.awt.Frame;

import javax.swing.JDialog;
import javax.swing.WindowConstants;

import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * Di&aacute;logo con las opciones de gesti&oacute;n de plugins.
 */
public final class PluginsManagementDialog extends JDialog {

	/** Serial Id. */
	private static final long serialVersionUID = -2374733164838573024L;

	/** Constructor del panel de configuraci&oacute;n de plugins.
	 * @param parent Componente padre del panel.
	 * @param modal Indica si la ventana debe ser modal.
	 */
	private PluginsManagementDialog(final Frame parent, final boolean modal) {
		super(parent, modal);
		setTitle(SimpleAfirmaMessages.getString("MainMenu.37")); //$NON-NLS-1$
		add(new PluginsManagementPanel(this));
		final double screenHeight = LookAndFeelManager.getScreenSize().getHeight();
		final Dimension preferedFrameSize = new Dimension(600, (int) Math.min(550, screenHeight * 0.8));
		setSize(preferedFrameSize);
		setLocationRelativeTo(parent);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	}

	/** Muestra el di&aacute;logo.
	 * @param parent Componente padre sobre el que mostrar el panel.
	 * @param modal Modalidad del di&aacute;logo. */
	public static void show(final Frame parent, final boolean modal) {
		new PluginsManagementDialog(parent, modal).setVisible(true);
	}

}
