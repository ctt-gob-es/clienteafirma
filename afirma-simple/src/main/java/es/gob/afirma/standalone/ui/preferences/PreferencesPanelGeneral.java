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
import java.nio.charset.StandardCharsets;
import java.util.Collections;
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
import es.gob.afirma.core.misc.http.DataDownloader;
import es.gob.afirma.core.prefs.KeyStorePreferencesManager;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.GenericFileFilter;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.HttpManager;
import es.gob.afirma.standalone.JMulticardUtilities;
import es.gob.afirma.standalone.ProxyUtil;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager.PreferencesSource;
import es.gob.afirma.standalone.configurator.common.PreferencesPlistHandler;

final class PreferencesPanelGeneral extends JScrollPane {

	private static final long serialVersionUID = 5442844766530064610L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final PreferencesPanel preferencesPanel;

	PreferencesPanel getPrefPanel() {
		return this.preferencesPanel;
	}

	private final JComboBox<String> signatureAlgorithms = new JComboBox<>();

	private final JCheckBox avoidAskForClose = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.36")); //$NON-NLS-1$

	private final JCheckBox checkForUpdates = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.87")); //$NON-NLS-1$

	private final JCheckBox enableJMulticard = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.165")); //$NON-NLS-1$

	private final JCheckBox optimizedForVdi = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.190")); //$NON-NLS-1$

	private final JCheckBox confirmToSign = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.179")); //$NON-NLS-1$

	private final JCheckBox allowSignInvalidSignatures = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.178")); //$NON-NLS-1$

	private final JCheckBox massiveOverwrite = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.160")); //$NON-NLS-1$

	private final JCheckBox secureConnections = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.173")); //$NON-NLS-1$

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

		getVerticalScrollBar().setUnitIncrement(16);

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
						final byte[] xmlData = DataDownloader.downloadData(url);
						PreferencesPlistHandler.importUserPreferencesFromXml(new String(xmlData, StandardCharsets.UTF_8), isBlocked());
					}
					catch(final Exception e) {
						LOGGER.log(
							Level.SEVERE,
							"Error importando la configuracion desde red (" + url + "): " + e, //$NON-NLS-1$ //$NON-NLS-2$
							e
						);
						AOUIFactory.showErrorMessage(
							SimpleAfirmaMessages.getString("PreferencesPanel.116"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("PreferencesPanel.117"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE,
							e
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
							DesktopUtil.getDefaultDialogsIcon(),
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

		final JButton exportConfigToFileButton = new JButton(
				SimpleAfirmaMessages.getString("PreferencesPanel.196") //$NON-NLS-1$
		);

		exportConfigToFileButton.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.197") //$NON-NLS-1$
			);

		exportConfigToFileButton.setMnemonic('G');
		exportConfigToFileButton.addActionListener(
				ae -> {
						try {
							final String xmlPrefs = PreferencesPlistHandler.exportPreferencesToXml();
							AOUIFactory.getSaveDataToFile(xmlPrefs.getBytes(),
									SimpleAfirmaMessages.getString("PreferencesPanel.198"), //$NON-NLS-1$
									null,
									SimpleAfirmaMessages.getString("PreferencesPanel.201"), //$NON-NLS-1$
				    				Collections.singletonList(
				    						new GenericFileFilter(
				    								new String [] {"afconfig"}, //$NON-NLS-1$
				    								SimpleAfirmaMessages.getString("PreferencesPanel.111") //$NON-NLS-1$
				    								)
				    						),
									PreferencesPanelGeneral.this);
							AOUIFactory.showMessageDialog(
									getParent(),
									SimpleAfirmaMessages.getString("PreferencesPanel.199"), //$NON-NLS-1$
									SimpleAfirmaMessages.getString("PreferencesPanel.200"), //$NON-NLS-1$
									JOptionPane.INFORMATION_MESSAGE
								);
						} catch(final AOCancelledOperationException ex) {
							// Operacion cancelada por el usuario
							return;
						} catch (final Exception e) {
							AOUIFactory.showErrorMessage(
									SimpleAfirmaMessages.getString("PreferencesPanel.116"), //$NON-NLS-1$
									SimpleAfirmaMessages.getString("PreferencesPanel.117"), //$NON-NLS-1$
									JOptionPane.ERROR_MESSAGE,
									e
							);
							return;
						}
				}
			);

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
		panelConstraint.weightx = 0.5;
		panelConstraint.gridx = 0;
		panel.add(importConfigFromFileButton, panelConstraint);
		panelConstraint.gridx++;
		panel.add(exportConfigToFileButton, panelConstraint);
		panelConstraint.gridx++;
		panel.add(restoreConfigFromFileButton, panelConstraint);

		signConfigPanel.add(panel, signConstraint);

		signConstraint.insets = new Insets(5, 7, 3, 7);
		signConstraint.anchor = GridBagConstraints.LINE_START;

		signConstraint.gridy++;

		this.avoidAskForClose.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.avoidAskForClose.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.44") //$NON-NLS-1$
		);
		this.avoidAskForClose.addItemListener(modificationListener);
		this.avoidAskForClose.addKeyListener(keyListener);
		signConfigPanel.add(this.avoidAskForClose, signConstraint);

		signConstraint.gridy++;

		// Solo mostramos el check de buscar actualizaciones si esta habilitado
		// su uso
		if (SimpleAfirma.isUpdatesEnabled()) {
			this.checkForUpdates.setEnabled(!this.blocked);
			this.checkForUpdates.getAccessibleContext().setAccessibleName(
					SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
			);
			this.checkForUpdates.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.88") //$NON-NLS-1$
			);
			this.checkForUpdates.addItemListener(modificationListener);
			this.checkForUpdates.addKeyListener(keyListener);
			signConfigPanel.add(this.checkForUpdates, signConstraint);
			signConstraint.gridy++;
		}

		// En Windows, se dara la posibilidad de configurar el comportamiento de
		// JMulticard. Para el resto de sistemas, es obligatorio su uso
		if (Platform.getOS() == Platform.OS.WINDOWS || Platform.getOS() == Platform.OS.LINUX) {
			this.enableJMulticard.setEnabled(!this.blocked);
			this.enableJMulticard.getAccessibleContext().setAccessibleName(
					SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
			);
			this.enableJMulticard.getAccessibleContext().setAccessibleDescription(
					SimpleAfirmaMessages.getString("PreferencesPanel.166")); //$NON-NLS-1$
			this.enableJMulticard.addItemListener(modificationListener);
			this.enableJMulticard.addKeyListener(keyListener);
			signConfigPanel.add(this.enableJMulticard, signConstraint);

			signConstraint.gridy++;
		}

		// En Windows, se dara la posibilidad de configurar el comportamiento de
		// JMulticard. Para el resto de sistemas, es obligatorio su uso
		if (Platform.getOS() == Platform.OS.WINDOWS) {
			this.optimizedForVdi.getAccessibleContext().setAccessibleDescription(
					SimpleAfirmaMessages.getString("PreferencesPanel.191")); //$NON-NLS-1$
			this.optimizedForVdi.addItemListener(modificationListener);
			this.optimizedForVdi.addKeyListener(keyListener);
			signConfigPanel.add(this.optimizedForVdi, signConstraint);

			signConstraint.gridy++;
		}

		mainPanel.add(signConfigPanel, gbc);

		final JPanel signGeneralPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		signGeneralPanel.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.17") //$NON-NLS-1$
			)
		);

		final GridBagConstraints c = new GridBagConstraints();
		c.gridy = 0;
		c.insets = new Insets(0, 0, 4, 7);
		c.anchor = GridBagConstraints.LINE_START;

		final JLabel signatureAlgorithmsLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.18")); //$NON-NLS-1$

		this.signatureAlgorithms.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.183") //$NON-NLS-1$
		);
		this.signatureAlgorithms.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.46") //$NON-NLS-1$
		);
		this.signatureAlgorithms.addItemListener(modificationListener);
		this.signatureAlgorithms.addKeyListener(keyListener);
		this.signatureAlgorithms.setModel(
			new DefaultComboBoxModel<>(
				new String[] {
					"SHA1", //$NON-NLS-1$
					"SHA512", //$NON-NLS-1$
					"SHA384", //$NON-NLS-1$
					"SHA256" //$NON-NLS-1$
				}
			)
		);
		signatureAlgorithmsLabel.setLabelFor(this.signatureAlgorithms);
		this.signatureAlgorithms.setEnabled(!isBlocked());

		final JPanel innerPanel = new JPanel(new GridBagLayout());
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

		c.gridx = 0;
		c.gridwidth = 2;
		c.insets = new Insets(4, 0, 3, 7);

		c.gridy++;

		this.confirmToSign.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.confirmToSign.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.180")); //$NON-NLS-1$
		this.confirmToSign.addItemListener(modificationListener);
		this.confirmToSign.addKeyListener(keyListener);
		innerPanel.add(this.confirmToSign, c);

		c.gridy++;

		this.allowSignInvalidSignatures.setEnabled(!this.blocked);
		this.allowSignInvalidSignatures.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.allowSignInvalidSignatures.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.178")); //$NON-NLS-1$
		this.allowSignInvalidSignatures.addItemListener(modificationListener);
		this.allowSignInvalidSignatures.addKeyListener(keyListener);
		innerPanel.add(this.allowSignInvalidSignatures, c);

		c.gridy++;

		this.massiveOverwrite.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.massiveOverwrite.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.160")); //$NON-NLS-1$
		this.massiveOverwrite.addItemListener(modificationListener);
		this.massiveOverwrite.addKeyListener(keyListener);
		innerPanel.add(this.massiveOverwrite, c);

		final JPanel netConfigPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		netConfigPanel.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.125") //$NON-NLS-1$
			)
		);

		final JPanel netConfigInnerPanel = new JPanel(new GridBagLayout());
		netConfigPanel.add(netConfigInnerPanel);

		this.secureConnections.setEnabled(!this.blocked);
		this.secureConnections.addItemListener(modificationListener);
		this.secureConnections.addKeyListener(keyListener);
		this.secureConnections.setToolTipText(
			SimpleAfirmaMessages.getString("PreferencesPanel.174") //$NON-NLS-1$
		);
		this.secureConnections.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.secureConnections.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.174") //$NON-NLS-1$
		);

		final JButton secureDomainsBtn = new JButton(
				SimpleAfirmaMessages.getString("PreferencesPanel.185") //$NON-NLS-1$
			);

		secureDomainsBtn.setMnemonic('D');
		secureDomainsBtn.addActionListener(
				ae -> changeSecureDomainsDlg(getParent())
			);
		secureDomainsBtn.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.186") //$NON-NLS-1$
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

		final JLabel proxyLabel = new JLabel("<html>" + //$NON-NLS-1$
				SimpleAfirmaMessages.getString("PreferencesPanel.128") + //$NON-NLS-1$
				"</html>"); //$NON-NLS-1$
		proxyLabel.setLabelFor(proxyConfigButton);

		final JButton trustedCertificatesButton = new JButton(
				SimpleAfirmaMessages.getString("PreferencesPanel.194") //$NON-NLS-1$
			);

		trustedCertificatesButton.setMnemonic('t');
		trustedCertificatesButton.addActionListener(
			ae -> trustedCertificatesDlg(this)
		);
		trustedCertificatesButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.195") //$NON-NLS-1$
		);

		final GridBagConstraints netConstraints = new GridBagConstraints();
		netConstraints.insets = new Insets(0, 0, 4, 7);
		netConstraints.gridx = 0;
		netConstraints.gridy = 0;
		netConstraints.gridwidth = 2;
		netConstraints.anchor = GridBagConstraints.LINE_START;
		netConfigInnerPanel.add(this.secureConnections, netConstraints);
		netConstraints.gridx++;
		netConstraints.insets = new Insets(0, 25, 0, 0);
		netConfigInnerPanel.add(secureDomainsBtn, netConstraints);
		netConstraints.insets = new Insets(0, 0, 4, 7);
		netConstraints.anchor = GridBagConstraints.LINE_START;
		netConstraints.gridx = 0;
		netConstraints.gridwidth = 1;
		netConstraints.gridy++;
		netConfigInnerPanel.add(proxyLabel, netConstraints);
		netConstraints.gridx++;
		netConstraints.anchor = GridBagConstraints.LINE_END;
		netConfigInnerPanel.add(proxyConfigButton, netConstraints);
		netConstraints.insets = new Insets(0, 0, 0, 0);
		netConstraints.anchor = GridBagConstraints.LINE_START;
		netConstraints.gridy++;
		netConstraints.gridx = 0;
		netConfigInnerPanel.add(trustedCertificatesButton, netConstraints);

		gbc.gridy++;
		mainPanel.add(signGeneralPanel, gbc);
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

    	final ProxyPanel proxyPanel = new ProxyPanel();
    	proxyPanel.getAccessibleContext().setAccessibleDescription(
    			SimpleAfirmaMessages.getString("ProxyDialog.15")); //$NON-NLS-1$

    	// Cursor por defecto
    	container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

    	if(AOUIFactory.showConfirmDialog(
				container,
				proxyPanel,
				SimpleAfirmaMessages.getString("ProxyDialog.0"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION,
				JOptionPane.DEFAULT_OPTION
		) == JOptionPane.OK_OPTION) {

    		try {
    			proxyPanel.saveData();
    		}
    		catch (final ConfigurationException e) {
    			AOUIFactory.showErrorMessage(
					e.getMessage(),
					SimpleAfirmaMessages.getString("ProxyDialog.2"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e
				);
    			changeProxyDlg(container);
    			return;
			}
			// Aplicamos los valores tanto si el checkbox esta marcado o no, en un caso lo establecera y en en otro lo
			// eliminara
    		try {
    			ProxyUtil.setProxySettings();
    		}
    		catch (final Throwable e) {
    			LOGGER.log(Level.SEVERE, "Error al aplicar la configuracion de proxy", e); //$NON-NLS-1$
    			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("SimpleAfirma.11"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.WARNING_MESSAGE, e); //$NON-NLS-1$
    		}
    	}
    }

	/** Di&aacute;logo para cambiar la configuraci&oacute;n del listado de dominios seguros.
	 * @param container Contenedor en el que se define el di&aacute;logo. */
    public static void changeSecureDomainsDlg(final Container container) {

    	// Cursor en espera
    	container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    	final SecureDomainsPanel secureDomainsPanel = new SecureDomainsPanel();
    	secureDomainsPanel.getAccessibleContext().setAccessibleDescription(
    			SimpleAfirmaMessages.getString("SecureDomainsDialog.2")); //$NON-NLS-1$

    	// Cursor por defecto
    	container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

    	if(AOUIFactory.showConfirmDialog(
				container,
				secureDomainsPanel,
				SimpleAfirmaMessages.getString("SecureDomainsDialog.0"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION,
				JOptionPane.DEFAULT_OPTION
		) == JOptionPane.OK_OPTION) {

    		try {
    			secureDomainsPanel.saveData();
    		}
    		catch (final ConfigurationException e) {
    			AOUIFactory.showErrorMessage(
					e.getMessage(),
					SimpleAfirmaMessages.getString("SecureDomainsDialog.1"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e
				);
    			changeSecureDomainsDlg(container);
    			return;
			}
    	}
    }

	/**
	 * Di&aacute;logo para configurar los certificados de confianza.
	 * @param container Contenedor en el que se define el di&aacute;logo.
	 */
    public static void trustedCertificatesDlg(final Container container) {

    	final TrustedCertificatesPanel trustedCertPanel = new TrustedCertificatesPanel();

    	JOptionPane.showMessageDialog(
    			container,
				trustedCertPanel,
				SimpleAfirmaMessages.getString("TrustedCertificatesDialog.0"), //$NON-NLS-1$
				JOptionPane.PLAIN_MESSAGE);
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
    	formatPanel.getAccessibleContext().setAccessibleDescription(
    			SimpleAfirmaMessages.getString("DefaultFormatPanel.1")); //$NON-NLS-1$

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
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_CONFIRMTOSIGN, this.confirmToSign.isSelected());
		if (SimpleAfirma.isUpdatesEnabled()) {
			PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_UPDATECHECK, this.checkForUpdates.isSelected());
		}
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_ALLOW_INVALID_SIGNATURES, this.allowSignInvalidSignatures.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_ENABLED_JMULTICARD, this.enableJMulticard.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_VDI_OPTIMIZATION, this.optimizedForVdi.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_MASSIVE_OVERWRITE, this.massiveOverwrite.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_SECURE_CONNECTIONS, this.secureConnections.isSelected());

		// Segun lo configurado establecemos el uso de conexiones de seguras
		HttpManager.setSecureConnections(this.secureConnections.isSelected());
	}

	void loadPreferences() {

		this.signatureAlgorithms.setSelectedItem(
			PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_SIGNATURE_ALGORITHM)
		);
		// Si el valor establecido no es valido, establecemos el valor por defecto
		if (this.signatureAlgorithms.getSelectedItem() == null) {
			this.signatureAlgorithms.setSelectedItem(
					PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_SIGNATURE_ALGORITHM, PreferencesSource.DEFAULT));
		}
		this.avoidAskForClose.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_OMIT_ASKONCLOSE));
		this.confirmToSign.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_CONFIRMTOSIGN));

		if (!SimpleAfirma.isUpdatesEnabled()) {
			this.checkForUpdates.setSelected(false);
			this.checkForUpdates.setEnabled(false);
		}
		else {
			this.checkForUpdates.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_UPDATECHECK));
		}

		this.allowSignInvalidSignatures.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_ALLOW_INVALID_SIGNATURES));

		if (JMulticardUtilities.isJMulticardConfigurateBySystem()) {
			this.enableJMulticard.setSelected(false);
			this.enableJMulticard.setEnabled(false);
		}
		else {
			this.enableJMulticard.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_ENABLED_JMULTICARD));
		}

		this.optimizedForVdi.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_VDI_OPTIMIZATION));

		this.massiveOverwrite.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_MASSIVE_OVERWRITE));

		this.secureConnections.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_SECURE_CONNECTIONS));
	}

	/** Carga las opciones de configuraci&oacute;n por defecto del panel general
	 * desde un fichero externo de preferencias. */
	void loadDefaultPreferences() {

		try {
			PreferencesManager.clearAll();
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo restaurar la configuracion general de la aplicacion: " + e); //$NON-NLS-1$
		}
		try {
			KeyStorePreferencesManager.clearKeyStorePrefs();
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo restaurar la configuracion de almacenes de la aplicacion: " + e); //$NON-NLS-1$
		}
		loadPreferences();
		getDisposableInterface().disposeInterface();
	}

}