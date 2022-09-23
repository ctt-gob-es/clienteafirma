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

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Panel para el di&aacute;logo de configuraci&oacute; del listado de dominios seguros. */
final class SecureDomainsPanel extends JPanel {

	private static final long serialVersionUID = -6040435120676908406L;
	private static final int PREFERRED_WIDTH = 420;
	private static final int PREFERRED_HEIGHT = 210;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private JTextArea secureDomainsListTA = new JTextArea(10, 30);

	private final SecureDomainsHandler eventsHandler;

	/** Constructor. */
	SecureDomainsPanel() {
		this.eventsHandler = new SecureDomainsHandler(this);
		createUI();
        new Thread(() -> SecureDomainsPanel.this.eventsHandler.loadViewData()).start();
	}

	/**
	 * Construimos visualmente el panel.
	 */
	private void createUI() {
		setLayout(new GridBagLayout());
		setMinimumSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));
		getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SecureDomainsDialog.2")); //$NON-NLS-1$

		final JScrollPane scrollPane = new JScrollPane(this.secureDomainsListTA);

		final JLabel generalInfoLabel = new JLabel(SimpleAfirmaMessages.getString("SecureDomainsDialog.2")); //$NON-NLS-1$
		generalInfoLabel.getAccessibleContext().setAccessibleDescription("SecureDomainsDialog.2"); //$NON-NLS-1$
		generalInfoLabel.setFocusable(true);

		// Construimos las estructuras y componentes internos
		final JLabel formatInfoLabel = new JLabel(SimpleAfirmaMessages.getString("SecureDomainsDialog.3")); //$NON-NLS-1$
		formatInfoLabel.getAccessibleContext().setAccessibleDescription("SecureDomainsDialog.3"); //$NON-NLS-1$
		formatInfoLabel.setFocusable(true);

		// Colocamos los componentes en el panel
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridx = 0;
		c.gridy = 0;
		this.add(generalInfoLabel, c);
		c.gridy++;
		c.insets = new Insets(10, 0, 10, 0);
		this.add(scrollPane, c);
		c.insets = new Insets(0, 0, 0, 0);
		c.gridy++;
		this.add(formatInfoLabel, c);
	}

	/**
	 * Guarda los datos establecidos en el panel.
	 * @throws ConfigurationException Cuando los datos introducidos no son validos.
	 */
	void saveData() throws ConfigurationException {

		this.eventsHandler.saveViewData();
	}

	public JTextArea getSecureDomainsListTA() {
		return this.secureDomainsListTA;
	}

	public void setSecureDomainsListTA(final JTextArea secureDomainsListTA) {
		this.secureDomainsListTA = secureDomainsListTA;
	}

}
