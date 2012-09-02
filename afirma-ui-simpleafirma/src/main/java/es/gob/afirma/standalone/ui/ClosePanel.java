/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.GridLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.prefs.Preferences;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import es.gob.afirma.standalone.Messages;
import es.gob.afirma.standalone.PreferencesNames;

/** Panel para preguntar si se desea cerrar la aplicaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class ClosePanel extends JPanel {

	private static final long serialVersionUID = -2848106901208270337L;

	static final Preferences PREFERENCES = Preferences.userRoot();

	private final JCheckBox ask = new JCheckBox(Messages.getString("ClosePanel.0")); //$NON-NLS-1$
	JCheckBox getAskCheckBox() {
		return this.ask;
	}

	/** Construye el panel para preguntar si se desea cerrar la aplicaci&oacute;n. */
	public ClosePanel() {
		setLayout(new GridLayout(0,1));
		add(new JLabel(Messages.getString("SimpleAfirma.47"))); //$NON-NLS-1$
		this.ask.setSelected(PREFERENCES.getBoolean(PreferencesNames.PREFERENCE_OMIT_ASKONCLOSE, false));
		this.ask.addItemListener(new ItemListener() {
			/** {@inheritDoc} */
			@Override
			public void itemStateChanged(final ItemEvent e) {
				PREFERENCES.putBoolean(PreferencesNames.PREFERENCE_OMIT_ASKONCLOSE, ClosePanel.this.getAskCheckBox().isSelected());
			}
		});
		add(this.ask);
	}

}
