/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.GridLayout;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;

/** Panel para preguntar si se desea cerrar la aplicaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class ClosePanel extends JPanel {

	private static final long serialVersionUID = -2848106901208270337L;

	private final JCheckBox ask = new JCheckBox(SimpleAfirmaMessages.getString("ClosePanel.0")); //$NON-NLS-1$
	JCheckBox getAskCheckBox() {
		return this.ask;
	}

	/** Construye el panel para preguntar si se desea cerrar la aplicaci&oacute;n. */
	public ClosePanel() {
		setLayout(new GridLayout(0,1));
		add(new JLabel(SimpleAfirmaMessages.getString("SimpleAfirma.47"))); //$NON-NLS-1$
		this.ask.setSelected(PreferencesManager.getBoolean(
			PreferencesManager.PREFERENCE_GENERAL_OMIT_ASKONCLOSE)
		);
		this.ask.addItemListener(
			e -> PreferencesManager.putBoolean(
				PreferencesManager.PREFERENCE_GENERAL_OMIT_ASKONCLOSE,
				ClosePanel.this.getAskCheckBox().isSelected()
			)
		);
		this.ask.setMnemonic('m');
		add(this.ask);
	}

}
