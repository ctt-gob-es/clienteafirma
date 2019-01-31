/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyListener;
import java.security.GeneralSecurityException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.JMulticardUtilities;
import es.gob.afirma.standalone.ProxyUtil;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.updater.Updater;

final class PreferencesPanelGeneral extends JScrollPane {

	private static final long serialVersionUID = 5442844766530064610L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final PreferencesPanel preferencesPanel;

	PreferencesPanel getPrefPanel() {
		return this.preferencesPanel;
	}

	private final JComboBox<String> signatureAlgorithms = new JComboBox<>();

	private final JCheckBox avoidAskForClose = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.36")); //$NON-NLS-1$

	private final JCheckBox hideDniStartScreen = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.81")); //$NON-NLS-1$

	private final JCheckBox checkForUpdates = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.87")); //$NON-NLS-1$

	private final JCheckBox sendAnalytics = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.89")); //$NON-NLS-1$

	private final JCheckBox enableJMulticard = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.165")); //$NON-NLS-1$

	private final JCheckBox massiveOverwrite = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.160")); //$NON-NLS-1$

	private final DisposableInterface disposableInterface;
	DisposableInterface getDisposableInterface() {
		return this.disposableInterface;
	}

	/** Atributo para gestionar el bloqueo de propiedades. */
	private boolean blocked;

	PreferencesPanelGeneral(final KeyListener keyListener,
			                final ItemListener modificationListener,
			                final DisposableInterface di,
			                final PreferencesPanel prefPanel,
			                final boolean blocked) {
		this.disposableInterface = di;
		this.preferencesPanel = prefPanel;

		this.blocked = blocked;

		createUI(keyListener, modificationListener);
	}

	private void createUI(final KeyListener keyListener,
				  final ItemListener modificationListener) {

		final JPanel mainPanel = new JPanel(new GridBagLayout());

		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.weightx = 1.0;
		gbc.gridx = 0;
		gbc.gridy = 0;

		final JPanel signConfigPanel = new JPanel(new GridBagLayout());
		signConfigPanel.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.108") //$NON-NLS-1$
			)
		);

		final GridBagConstraints signConstraint = new GridBagConstraints();
		signConstraint.fill = GridBagConstraints.HORIZONTAL;
		signConstraint.weightx = 1.0;
		signConstraint.gridy = 0;
		signConstraint.insets = new Insets(0, 0, 0, 0);

		final JButton importConfigFromFileButton = new JButton(
			SimpleAfirmaMessages.getString("PreferencesPanel.107") //$NON-NLS-1$
		);

		importConfigFromFileButton.setMnemonic('I');
		importConfigFromFileButton.addActionListener(
			ae -> {
				if ((ae.getModifiers() & ActionEvent.ALT_MASK) != 0) {
					final String url = (String) AOUIFactory.showInputDialog(
						getParent(),
						SimpleAfirmaMessages.getString("PreferencesPanel.109"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PreferencesPanel.110"), //$NON-NLS-1$
						JOptionPane.QUESTION_MESSAGE,
						null,
						null,
						null
					);
					if (url == null || url.trim().isEmpty()) {
						return;
					}
					try {
						PreferencesPlistHandler.importPreferencesFromUrl(url, isBlocked());
					}
					catch(final Exception e) {
						LOGGER.log(
							Level.SEVERE,
							"Error importando la configuracion desde red (" + url + "): " + e, //$NON-NLS-1$ //$NON-NLS-2$
							e
						);
						AOUIFactory.showErrorMessage(
							getParent(),
							SimpleAfirmaMessages.getString("PreferencesPanel.116"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("PreferencesPanel.117"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE
						);
						return;
					}
				}
				else {
					final String configFilePath;
					try {
						configFilePath = AOUIFactory.getLoadFiles(
							SimpleAfirmaMessages.getString("PreferencesPanel.86"), //$NON-NLS-1$
							null,
							null,
							new String[] { "afconfig" }, //$NON-NLS-1$
							SimpleAfirmaMessages.getString("PreferencesPanel.111"), //$NON-NLS-1$
							false,
							false,
							AutoFirmaUtil.getDefaultDialogsIcon(),
							PreferencesPanelGeneral.this
						)[0].getAbsolutePath();
					}
					catch(final AOCancelledOperationException ex) {
						// Operacion cancelada por el usuario
						return;
					}
					PreferencesPlistHandler.importPreferences(configFilePath, getParent(), isBlocked());
				}
				AOUIFactory.showMessageDialog(
						getParent(),
						SimpleAfirmaMessages.getString("PreferencesPanel.142"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PreferencesPanel.143"), //$NON-NLS-1$
						JOptionPane.INFORMATION_MESSAGE
					);
				getDisposableInterface().disposeInterface();
			}
		);
		importConfigFromFileButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.112") //$NON-NLS-1$
		);

		importConfigFromFileButton.setEnabled(!this.blocked);

		final JButton restoreConfigFromFileButton = new JButton(
			SimpleAfirmaMessages.getString("PreferencesPanel.135") //$NON-NLS-1$
		);

		restoreConfigFromFileButton.setMnemonic('R');
		restoreConfigFromFileButton.addActionListener(ae -> {
			if (AOUIFactory.showConfirmDialog(getParent(), SimpleAfirmaMessages.getString("PreferencesPanel.140"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("PreferencesPanel.139"), //$NON-NLS-1$
					JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {

				loadDefaultPreferences();
			}
		});
		restoreConfigFromFileButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.136") //$NON-NLS-1$
		);

		restoreConfigFromFileButton.setEnabled(!this.blocked);

		final JPanel panel = new JPanel();
		panel.setLayout(new GridBagLayout());

		final GridBagConstraints panelConstraint = new GridBagConstraints();
		panelConstraint.fill = GridBagConstraints.HORIZONTAL;
		panelConstraint.weightx = 1.0;
		panelConstraint.gridx = 0;
		panel.add(importConfigFromFileButton, panelConstraint);
		panelConstraint.gridx++;
		panel.add(restoreConfigFromFileButton, panelConstraint);

		signConfigPanel.add(panel, signConstraint);

		signConstraint.insets = new Insets(5, 7, 3, 7);
		signConstraint.anchor = GridBagConstraints.LINE_START;

		signConstraint.gridy++;

		this.avoidAskForClose.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.44") //$NON-NLS-1$
		);
		this.avoidAskForClose.setMnemonic('N');
		this.avoidAskForClose.addItemListener(modificationListener);
		this.avoidAskForClose.addKeyListener(keyListener);
		signConfigPanel.add(this.avoidAskForClose, signConstraint);

		signConstraint.gridy++;

		this.hideDniStartScreen.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.82") //$NON-NLS-1$
		);
		this.hideDniStartScreen.setMnemonic('D');
		this.hideDniStartScreen.addItemListener(modificationListener);
		this.hideDniStartScreen.addKeyListener(keyListener);
		signConfigPanel.add(this.hideDniStartScreen, signConstraint);

		signConstraint.gridy++;

		// Solo se buscaran actualizaciones automaticamente en Windows
		if (SimpleAfirma.isUpdatesEnabled() && Platform.OS.WINDOWS.equals(Platform.getOS())) {
			this.checkForUpdates.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.88") //$NON-NLS-1$
			);
			this.checkForUpdates.setMnemonic('B');
			this.checkForUpdates.addItemListener(modificationListener);
			this.checkForUpdates.addKeyListener(keyListener);
			signConfigPanel.add(this.checkForUpdates, signConstraint);
			signConstraint.gridy++;
		}

		this.sendAnalytics.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.90") //$NON-NLS-1$
		);
		this.sendAnalytics.setMnemonic('t');
		this.sendAnalytics.addItemListener(modificationListener);
		this.sendAnalytics.addKeyListener(keyListener);
		signConfigPanel.add(this.sendAnalytics, signConstraint);
		signConstraint.gridy++;

		// En Windows, se dara la posibilidad de configurar el comportamiento de
		// JMulticard. Para el resto de sistemas, es obligatorio su uso
		if (Platform.getOS() == Platform.OS.WINDOWS) {
			this.enableJMulticard.getAccessibleContext().setAccessibleDescription(
					SimpleAfirmaMessages.getString("PreferencesPanel.166")); //$NON-NLS-1$
			this.enableJMulticard.setMnemonic('j');
			this.enableJMulticard.addItemListener(modificationListener);
			this.enableJMulticard.addKeyListener(keyListener);
			signConfigPanel.add(this.enableJMulticard, signConstraint);
		}


		mainPanel.add(signConfigPanel, gbc);

		final JPanel innerPanel = new JPanel(new GridBagLayout());
		final JPanel signGeneralPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		signGeneralPanel.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.17") //$NON-NLS-1$
			)
		);

		final GridBagConstraints c = new GridBagConstraints();
		c.gridy = 0;
		c.insets = new Insets(0, 7, 4, 7);
		c.anchor = GridBagConstraints.LINE_START;

		final JLabel signatureAlgorithmsLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.18")); //$NON-NLS-1$

		this.signatureAlgorithms.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.46") //$NON-NLS-1$
		);
		this.signatureAlgorithms.addItemListener(modificationListener);
		this.signatureAlgorithms.addKeyListener(keyListener);
		this.signatureAlgorithms.setModel(
			new DefaultComboBoxModel<>(
				new String[] {
					"SHA1withRSA", //$NON-NLS-1$
					"SHA512withRSA", //$NON-NLS-1$
					"SHA384withRSA", //$NON-NLS-1$
					"SHA256withRSA" //$NON-NLS-1$
				}
			)
		);
		signatureAlgorithmsLabel.setLabelFor(this.signatureAlgorithms);
		this.signatureAlgorithms.setEnabled(!isBlocked());

		signGeneralPanel.add(innerPanel);

		c.gridx = 0;
		innerPanel.add(signatureAlgorithmsLabel, c);
		c.gridx = 1;
		innerPanel.add(this.signatureAlgorithms, c);

		final JLabel configureFormatsLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.161")); //$NON-NLS-1$
		final JButton configureFormatsButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.162")); //$NON-NLS-1$
		configureFormatsButton.setMnemonic('o');
		configureFormatsButton.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.163") //$NON-NLS-1$
		);
		configureFormatsButton.addActionListener(
				ae -> openFormatsDlg(getParent(), isBlocked())
			);
		configureFormatsLabel.setLabelFor(configureFormatsButton);

		c.gridy++;
		c.gridx = 0;
		innerPanel.add(configureFormatsLabel, c);
		c.gridx = 1;
		innerPanel.add(configureFormatsButton, c);

		final JPanel massiveSignaturePanel = new JPanel(new FlowLayout(FlowLayout.LEADING));
		massiveSignaturePanel.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.159") //$NON-NLS-1$
			)
		);

		this.massiveOverwrite.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.160") //$NON-NLS-1$
				);
		this.massiveOverwrite.setMnemonic('S');
		this.massiveOverwrite.addItemListener(modificationListener);
		this.massiveOverwrite.addKeyListener(keyListener);

		massiveSignaturePanel.add(this.massiveOverwrite);

		final JPanel netConfigPanel = new JPanel(new FlowLayout(FlowLayout.LEADING));
		netConfigPanel.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.125") //$NON-NLS-1$
			)
		);

		final JButton proxyConfigButton = new JButton(
			SimpleAfirmaMessages.getString("PreferencesPanel.126") //$NON-NLS-1$
		);

		proxyConfigButton.setMnemonic('y');
		proxyConfigButton.addActionListener(
			ae -> changeProxyDlg(getParent())
		);
		proxyConfigButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.127") //$NON-NLS-1$
		);

		final JLabel proxyLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.128")); //$NON-NLS-1$
		proxyLabel.setLabelFor(proxyConfigButton);

		netConfigPanel.add(proxyLabel);
		netConfigPanel.add(proxyConfigButton);


		gbc.gridy++;
		mainPanel.add(signGeneralPanel, gbc);
		gbc.gridy++;
		mainPanel.add(massiveSignaturePanel, gbc);
		gbc.gridy++;
		mainPanel.add(netConfigPanel, gbc);
		gbc.weighty = 1.0;
		gbc.gridy++;
		mainPanel.add(new JPanel(), gbc);

		loadPreferences();

		setViewportView(mainPanel);
	}

	/** Di&aacute;logo para cambiar la configuraci&oacute;n del <i>proxy</i>.
	 * @param container Contenedor en el que se define el di&aacute;logo. */
    public static void changeProxyDlg(final Container container) {

    	// Cursor en espera
    	container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    	final ProxyPanel proxyDlg = new ProxyPanel();

    	// Cursor por defecto
    	container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

    	if(AOUIFactory.showConfirmDialog(
				container,
				proxyDlg,
				SimpleAfirmaMessages.getString("ProxyDialog.0"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION,
				JOptionPane.DEFAULT_OPTION
		) == JOptionPane.OK_OPTION) {

			if (proxyDlg.isProxySelected()) {
				final String host = proxyDlg.getHost();
				final String port = proxyDlg.getPort();

				if(host == null || host == "") { //$NON-NLS-1$
					AOUIFactory.showErrorMessage(
						null,
						SimpleAfirmaMessages.getString("ProxyDialog.1"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("ProxyDialog.2"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
					changeProxyDlg(container);
					LOGGER.warning("El host no puede ser nulo o vacio"); //$NON-NLS-1$
				}
				else if(port == null || port == "") { //$NON-NLS-1$
					AOUIFactory.showErrorMessage(
						null,
						SimpleAfirmaMessages.getString("ProxyDialog.3"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("ProxyDialog.2"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
					changeProxyDlg(container);
					LOGGER.warning("El puerto no puede ser nulo, vacio o tener mas de 4 digitos"); //$NON-NLS-1$
				}
				else {
					PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_HOST, host);
					PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_PORT, port);

					// Si no se establece usuario, nos aseguramos de eliminar el actual. Si se establece, lo guardamos.
					if (proxyDlg.getUsername() == null || proxyDlg.getUsername().isEmpty()) {
						PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_USERNAME);
					}
					else {
						PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_USERNAME, proxyDlg.getUsername());
					}

					// Si no se establece contrasena, nos aseguramos de eliminar la actual. Si se establece,
					// la guardamos cifrada.
					final char[] password = proxyDlg.getPassword();
					if (password == null || password.length == 0) {
						PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD);
					}
					else {
						try {
							final String cipheredPwd = ProxyUtil.cipherPassword(password);
							if (cipheredPwd != null) {
								PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD, cipheredPwd);
							}
						}
						catch (final GeneralSecurityException e) {
							LOGGER.severe("Error cifrando la contrasena del Proxy: " + e); //$NON-NLS-1$
							JOptionPane.showMessageDialog(container, SimpleAfirmaMessages.getString("ProxyDialog.19")); //$NON-NLS-1$);
							PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD, ""); //$NON-NLS-1$
						}
					}
				}
			}
			else {
				PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_HOST);
				PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_PORT);
				PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_USERNAME);
				PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD);
			}

			PreferencesManager.putBoolean(
				PreferencesManager.PREFERENCE_GENERAL_PROXY_SELECTED,
				proxyDlg.isProxySelected()
			);

			// Aplicamos los valores tanto si el checkbox esta marcado o no, en un caso lo establecera y en en otro lo
			// eliminara
			ProxyUtil.setProxySettings();
    	}
    }

    /**
     * Abre el dialogo de configuracion de los formatos de firma configurados para cada
     * tipo de fichero.
     * @param parent Componente sobre el que mostrar el di&aacute;logo.
     * @param blocked Indica si la configuraci&oacute;n del di&aacute;logo debe aparecer bloqueada.
     */
    private static void openFormatsDlg(final Component parent, final boolean blocked) {

    	final DefaultFormatPanel formatPanel = new DefaultFormatPanel();
    	formatPanel.loadPreferences();
    	formatPanel.setBlocked(blocked);

    	if(AOUIFactory.showConfirmDialog(
    			parent,
    			formatPanel,
    			SimpleAfirmaMessages.getString("PreferencesPanel.164"), //$NON-NLS-1$
    			JOptionPane.OK_CANCEL_OPTION,
    			JOptionPane.DEFAULT_OPTION
    			) == JOptionPane.OK_OPTION) {
    		formatPanel.savePreferences();
    	}
	}

    /**
	 * Indica si el panel permite o no la edici&oacute;n de sus valores.
	 * @return {@code true} si est&aacute; bloqueado y no permite la edici&oacute;n,
	 * {@code false} en caso contrario.
	 */
	public boolean isBlocked() {
		return this.blocked;
	}

	/**
	 * Establece si deben bloquearse las opciones de configuraci&oacute;n del panel.
	 * @param blocked {@code true} si las opciones de configuraci&oacute;n deben bloquearse,
	 * {@code false} en caso contrario.
	 */
	public void setBlocked(final boolean blocked) {
		this.blocked = blocked;
	}


	void savePreferences() {
		// Opciones varias
		PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_SIGNATURE_ALGORITHM, this.signatureAlgorithms.getSelectedItem().toString());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_OMIT_ASKONCLOSE, this.avoidAskForClose.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN, this.hideDniStartScreen.isSelected());
		if (SimpleAfirma.isUpdatesEnabled()) {
			PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_UPDATECHECK, this.checkForUpdates.isSelected());
		}
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_USEANALYTICS, this.sendAnalytics.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_ENABLED_JMULTICARD, this.enableJMulticard.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_MASSIVE_OVERWRITE, this.massiveOverwrite.isSelected());
	}

	void loadPreferences() {

		this.signatureAlgorithms.setSelectedItem(
			PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_SIGNATURE_ALGORITHM)
		);
		this.avoidAskForClose.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_OMIT_ASKONCLOSE));
		this.hideDniStartScreen.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN));

		if (
			Boolean.getBoolean(Updater.AUTOFIRMA_AVOID_UPDATE_CHECK) ||
			Boolean.parseBoolean(System.getenv(Updater.AUTOFIRMA_AVOID_UPDATE_CHECK)) ||
			!SimpleAfirma.isUpdatesEnabled()
		) {
			this.checkForUpdates.setSelected(false);
			this.checkForUpdates.setEnabled(false);
		}
		else {
			this.checkForUpdates.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_UPDATECHECK));
		}

		if (Boolean.getBoolean(SimpleAfirma.DO_NOT_SEND_ANALYTICS) ||
				Boolean.parseBoolean(System.getenv(SimpleAfirma.DO_NOT_SEND_ANALYTICS_ENV))) {
			this.sendAnalytics.setSelected(false);
			this.sendAnalytics.setEnabled(false);
		}
		else {
			this.sendAnalytics.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_USEANALYTICS));
		}

		if (JMulticardUtilities.isJMulticardConfigurateBySystem()) {
			this.enableJMulticard.setSelected(false);
			this.enableJMulticard.setEnabled(false);
		}
		else {
			this.enableJMulticard.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_ENABLED_JMULTICARD));
		}

		this.massiveOverwrite.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_MASSIVE_OVERWRITE));
	}

	/** Carga las opciones de configuraci&oacute;n por defecto del panel general
	 * desde un fichero externo de preferencias. */
	void loadDefaultPreferences() {

		try {
			PreferencesManager.clearAll();
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo restaurar la configuracion de la aplicacion: " + e); //$NON-NLS-1$
		}
		loadPreferences();
		getDisposableInterface().disposeInterface();
	}

}

