/* Copyright (C) 2022 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Panel para el di&aacute;logo de configuraci&oacute; del listado de dominios seguros. */
final class AddIntelligentCardPanel extends JPanel {

	private static final long serialVersionUID = -6040435120676908406L;
	private static final int PREFERRED_WIDTH = 620;
	private static final int PREFERRED_HEIGHT = 210;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private SecureDomainsHandler eventsHandler;

	/** Constructor. */
	AddIntelligentCardPanel() {
		//this.eventsHandler = new SecureDomainsHandler(this);
		createUI();
        new Thread(() -> AddIntelligentCardPanel.this.eventsHandler.loadViewData()).start();
	}

	/**
	 * Construimos visualmente el panel.
	 */
	private void createUI() {
		setLayout(new GridBagLayout());
		setMinimumSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));

		final JLabel cardnameLbl = new JLabel(SimpleAfirmaMessages.getString("AddIntelligentCardDialog.1")); //$NON-NLS-1$
		cardnameLbl.setFocusable(true);

		final JTextField cardNameTxt = new JTextField();
		cardNameTxt.setFocusable(true);

		final JLabel controllerNameLbl = new JLabel(SimpleAfirmaMessages.getString("AddIntelligentCardDialog.2")); //$NON-NLS-1$
		controllerNameLbl.setFocusable(true);

		final JTextField controllerNameTxt = new JTextField();
		controllerNameLbl.setFocusable(true);

		final JButton selectFileButton = new JButton(SimpleAfirmaMessages.getString("AddIntelligentCardDialog.3")); //$NON-NLS-1$
		selectFileButton.setFocusable(true);

		final JButton connectCardButton = new JButton(SimpleAfirmaMessages.getString("AddIntelligentCardDialog.4")); //$NON-NLS-1$
		connectCardButton.setFocusable(true);

		// Colocamos los componentes en el panel
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridx = 0;
		c.gridy = 0;
		this.add(cardnameLbl, c);
		c.insets = new Insets(0, 0, 0, 0);
		c.gridy++;
		this.add(cardNameTxt, c);
		c.gridy++;
		this.add(controllerNameLbl, c);
		c.gridy++;
		this.add(controllerNameTxt, c);
		c.gridx++;
		this.add(selectFileButton, c);
		c.gridx = 0;
		c.gridy++;
		this.add(connectCardButton, c);
	}

	/**
	 * Guarda los datos establecidos en el panel.
	 * @throws ConfigurationException Cuando los datos introducidos no son validos.
	 */
	void saveData() throws ConfigurationException {

		this.eventsHandler.saveViewData();
	}

}
