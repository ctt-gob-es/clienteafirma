/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.logging.Logger;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.text.JTextComponent;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Panel para el di&aacute;logo de configuraci&oacute; de proxy. */
final class ProxyPanel extends JPanel {

	private static final long serialVersionUID = -5919574790093890970L;
	private static final int PREFERRED_WIDTH = 420;
	private static final int PREFERRED_HEIGHT = 210;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final JTextField hostTF = new JTextField();
	private final JTextField portTF = new JTextField();
	private final JTextField usernameProxy = new JTextField();
	private final JPasswordField passwordProxy = new JPasswordField();
	private final JTextArea excludedUrlsTA = new JTextArea();
	private final JRadioButton noProxyRb = new JRadioButton(SimpleAfirmaMessages.getString("ProxyDialog.22")); //$NON-NLS-1$
	private final JRadioButton systemProxyRb = new JRadioButton(SimpleAfirmaMessages.getString("ProxyDialog.23")); //$NON-NLS-1$
	private final JRadioButton manualProxyRb = new JRadioButton(SimpleAfirmaMessages.getString("ProxyDialog.24")); //$NON-NLS-1$

	private final JButton checkConnectionBtn = new JButton(SimpleAfirmaMessages.getString("ProxyDialog.8")); //$NON-NLS-1$

	private final JButton detectProxyBtn = new JButton(SimpleAfirmaMessages.getString("ProxyDialog.11")); //$NON-NLS-1$

	private final ProxyPanelHandler eventsHandler;

	private JPanel manualProxyPanel;


	/** Constructor. */
	ProxyPanel() {
		this.eventsHandler = new ProxyPanelHandler(this);
		createUI();
		this.eventsHandler.registerComponents();

        new Thread(() -> ProxyPanel.this.eventsHandler.loadViewData()).start();
	}

	/**
	 * Construimos visualmente el panel.
	 */
	private void createUI() {
		setLayout(new GridBagLayout());
		setMinimumSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));
		getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("ProxyDialog.12") //$NON-NLS-1$
    	);

		// Construimos las estructuras y componentes internos
		final JLabel infoLabel = new JLabel(SimpleAfirmaMessages.getString("ProxyDialog.15")); //$NON-NLS-1$

		final ButtonGroup configBg = new ButtonGroup();
		configBg.add(this.noProxyRb);
		configBg.add(this.systemProxyRb);
		configBg.add(this.manualProxyRb);

		this.manualProxyPanel = createManualProxyPanel();

		// Colocamos los componentes en el panel
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridx = 0;
		c.gridy = 0;
		this.add(infoLabel, c);
		c.gridy++;
		c.insets = new Insets(10, 0, 0, 0);
		this.add(this.noProxyRb, c);
		c.gridy++;
		this.add(this.systemProxyRb, c);
		c.gridy++;
		this.add(this.manualProxyRb, c);
		c.gridy++;
		c.insets = new Insets(10, 25, 0, 0);
		this.add(this.manualProxyPanel, c);
	}

	private JPanel createManualProxyPanel() {

		final JPanel panel = new JPanel(new GridBagLayout());

		// host
		final JLabel hostLabel = new JLabel(SimpleAfirmaMessages.getString("ProxyDialog.5")); //$NON-NLS-1$
		hostLabel.setLabelFor(this.hostTF);

		// Puerto
		final JLabel portLabel = new JLabel(SimpleAfirmaMessages.getString("ProxyDialog.7")); //$NON-NLS-1$
		portLabel.setLabelFor(this.portTF);

		// User name
		final JLabel usernameLabel = new JLabel(SimpleAfirmaMessages.getString("ProxyDialog.16")); //$NON-NLS-1$
		usernameLabel.setLabelFor(this.usernameProxy);

		// Password
		final JLabel passwordLabel = new JLabel(SimpleAfirmaMessages.getString("ProxyDialog.17")); //$NON-NLS-1$
		passwordLabel.setLabelFor(this.passwordProxy);

		// Listado de URLs exluidas
		final JLabel excludedUrlsLabel = new JLabel(SimpleAfirmaMessages.getString("ProxyDialog.20")); //$NON-NLS-1$
		excludedUrlsLabel.setLabelFor(this.excludedUrlsTA);
		final JLabel excludedUrlsInfoLabel = new JLabel(SimpleAfirmaMessages.getString("ProxyDialog.21")); //$NON-NLS-1$
		excludedUrlsInfoLabel.setEnabled(false);

		final JScrollPane excludedUrlsSp = new JScrollPane(
				this.excludedUrlsTA,
				ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);


		final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEADING));
		buttonPanel.add(this.checkConnectionBtn);

		// Configuramos los aspectos visibles de los componentes
		this.checkConnectionBtn.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("ProxyDialog.6")); //$NON-NLS-1$
		this.checkConnectionBtn.setMnemonic('V');
		this.detectProxyBtn.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("ProxyDialog.13")); //$NON-NLS-1$
		this.detectProxyBtn.setMnemonic('A');

		// Colocamos los componentes
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridy = 0;
		c.gridx = 0;
		c.insets = new Insets(4, 0, 0, 0);
		panel.add(hostLabel, c);
		c.weightx = 0.8;
		c.gridx++;
		panel.add(this.hostTF,c);
		c.weightx = 0.0;
		c.gridx++;
		c.insets = new Insets(4, 11, 0, 0);
		panel.add(portLabel, c);
		c.weightx = 0.2;
		c.gridx++;
		c.insets = new Insets(4, 0, 0, 0);
		panel.add(this.portTF, c);
		c.weightx = 0.0;
		c.gridx++;
		c.insets = new Insets(4, 11, 0, 0);
		panel.add(this.detectProxyBtn, c);
		c.weightx = 0.0;
		c.gridy++;
		c.gridx = 0;
		c.insets = new Insets(4, 0, 0, 0);
		panel.add(usernameLabel, c);
		c.weightx = 1.0;
		c.gridx++;
		c.gridwidth = 4;
		panel.add(this.usernameProxy, c);
		c.weightx = 0.0;
		c.gridy++;
		c.gridx = 0;
		c.gridwidth = 1;
		panel.add(passwordLabel, c);
		c.weightx = 1.0;
		c.gridx++;
		c.gridwidth = 4;
		panel.add(this.passwordProxy, c);
		c.gridy++;
		c.gridx = 0;
		c.gridwidth = 5;
		c.insets = new Insets(8, 0, 0, 0);
		panel.add(excludedUrlsLabel, c);
		c.gridy++;
		c.ipady = 40;
		c.insets = new Insets(4, 0, 0, 0);
		panel.add(excludedUrlsSp, c);
		c.gridy++;
		c.ipady = 0;
		panel.add(excludedUrlsInfoLabel, c);
		c.gridy++;
		c.insets = new Insets(0, 0, 0, 0);
		panel.add(buttonPanel, c);

		return panel;
	}

	/**
	 * Guarda los datos establecidos en el panel.
	 * @throws ConfigurationException Cuando los datos introducidos no son validos.
	 */
	void saveData() throws ConfigurationException {
		this.eventsHandler.saveViewData();
	}


	JPanel getManualProxyPanel() {
		return this.manualProxyPanel;
	}

	/** Obtiene el RadioButton para indicar que no se desea utilizar proxy.
	 * @return Bot&oacute;n de radio. */
	JRadioButton getNoProxyRb() {
		return this.noProxyRb;
	}

	/** Obtiene el RadioButton para indicar que no se desea utilizar la configuraci&oacute;n de
	 * proxy del sistema.
	 * @return Bot&oacute;n de radio. */
	JRadioButton getSystemProxyRb() {
		return this.systemProxyRb;
	}

	/** Obtiene el RadioButton para indicar que no se desea utilizar una configuraci&oacute;n
	 * espec&iacute;fica de proxy.
	 * @return Bot&oacute;n de radio. */
	JRadioButton getManualProxyRb() {
		return this.manualProxyRb;
	}

	/** Obtiene el valor del campo de texto definido para el host.
	 * @return Valor del campo de texto definido para el host.*/
	JTextComponent getHostField() {
		return this.hostTF;
	}

	/** Obtiene el valor del campo de texto definido para el puerto.
	 * @return Valor del campo de texto definido para el puerto.*/
	JTextComponent getPortField() {
		return this.portTF;
	}

	JTextComponent getUsernameField() {
		return this.usernameProxy;
	}

	JTextComponent getPasswordField() {
		return this.passwordProxy;
	}

	JTextComponent getExcludedUrlsField() {
		return this.excludedUrlsTA;
	}

	JButton getCheckConnectionButton() {
		return this.checkConnectionBtn;
	}

	JButton getDetectProxyButton() {
		return this.detectProxyBtn;
	}
}
