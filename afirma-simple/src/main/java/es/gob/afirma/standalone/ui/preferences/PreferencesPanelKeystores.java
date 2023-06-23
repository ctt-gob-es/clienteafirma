/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS;

import java.awt.Container;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemListener;
import java.awt.event.KeyListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.keystores.KeyStorePreferencesManager;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.filters.CertificateFilter;
import es.gob.afirma.keystores.filters.MultipleCertificateFilter;
import es.gob.afirma.keystores.filters.PseudonymFilter;
import es.gob.afirma.keystores.filters.SkipAuthDNIeFilter;
import es.gob.afirma.keystores.filters.rfc.KeyUsageFilter;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.SimpleKeyStoreManager;
import es.gob.afirma.ui.core.jse.certificateselection.CertificateSelectionDialog;

/** Pesta&ntilde;a de configuraci&oacute;n de las preferencias de certificados.
 * @author Jos&eacute; Montero. */
final class PreferencesPanelKeystores extends JScrollPane {

	private static final long serialVersionUID = 3491050093362323228L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static JComboBox<RegisteredKeystore> keystores;

	static JComboBox<RegisteredKeystore> smartCards;

	private final JPanel panelKeystores = new JPanel();

	private final JButton showContentButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.2")); //$NON-NLS-1$

	final JButton connectButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.10")); //$NON-NLS-1$

	final JButton addCardButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.11")); //$NON-NLS-1$

	final JButton modifyCardButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.23")); //$NON-NLS-1$

	final JButton deleteCardButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.24")); //$NON-NLS-1$

	private final JCheckBox callsFromNavigator = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.3")); //$NON-NLS-1$

	private final JCheckBox hideDniStartScreen = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.81")); //$NON-NLS-1$

	private final JCheckBox showExpiredCerts = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.6")); //$NON-NLS-1$

	private final JCheckBox onlySignature = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.7")); //$NON-NLS-1$

	private final JCheckBox onlyAlias = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.8")); //$NON-NLS-1$

	private final JCheckBox skipAuthCertDnie = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.32")); //$NON-NLS-1$

	static final String[] EXTS_PKCS12 = new String[] { "pfx", "p12" }; //$NON-NLS-1$ //$NON-NLS-2$

	private static final String EXTS_DESC_PKCS12 = " (*.p12, *.pfx)"; //$NON-NLS-1$

	String localKeystoreSelectedPath;

	/**
	 * Atributo que permite gestionar el bloqueo de preferencias.
	 */
	private boolean blocked = true;

	PreferencesPanelKeystores(final KeyListener keyListener,
			  final ItemListener modificationListener, final boolean blocked) {

		setBlocked(blocked);
		createUI(keyListener, modificationListener);
	}

	void createUI(final KeyListener keyListener,
			  final ItemListener modificationListener) {

		final JPanel mainPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;

		final Platform.OS os = Platform.getOS();
		final List<RegisteredKeystore> stores = new ArrayList<RegisteredKeystore>();

		if (Platform.OS.WINDOWS.equals(os)) {
			stores.add(new RegisteredKeystore(AOKeyStore.WINDOWS));
			if (SimpleKeyStoreManager.isFirefoxAvailable()) {
				stores.add(new RegisteredKeystore(AOKeyStore.MOZ_UNI));
			}
		}
		else if (Platform.OS.MACOSX.equals(os)) {
			stores.add(new RegisteredKeystore(AOKeyStore.APPLE));
			if (SimpleKeyStoreManager.isFirefoxAvailable()) {
				stores.add(new RegisteredKeystore(AOKeyStore.MOZ_UNI));
			}
		}
		else {
			stores.add(new RegisteredKeystore(AOKeyStore.SHARED_NSS));
		}

		stores.add(new RegisteredKeystore(AOKeyStore.PKCS12));

		stores.add(new RegisteredKeystore(AOKeyStore.DNIEJAVA));

		PreferencesPanelKeystores.setKeystores(new JComboBox<RegisteredKeystore>(stores.toArray(new RegisteredKeystore[0])));

		keystores.addActionListener (new ActionListener () {
		    @Override
			public void actionPerformed(final ActionEvent e) {
		       final RegisteredKeystore ks = (RegisteredKeystore) PreferencesPanelKeystores.getKeystores().getSelectedItem();
		       if (ks.getName().equals(AOKeyStore.PKCS12.getName()) && e.getModifiers() != 0) {
			   		final File[] ksFile;
			   		try {
						ksFile = AOUIFactory.getLoadFiles(
							SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.12"), //$NON-NLS-1$
							null,
							null,
							EXTS_PKCS12,
							SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.13") + EXTS_DESC_PKCS12, //$NON-NLS-1$
							false,
							false,
							null,
							this
						);
						PreferencesPanelKeystores.this.localKeystoreSelectedPath = ksFile[0].getAbsolutePath();
			   		} catch (final AOCancelledOperationException acoe) {
			   			PreferencesPanelKeystores.getKeystores().setSelectedIndex(0);
						return;
			   		}
		       } else if (ks.getProviderName().equals(AOKeyStore.PKCS11.getProviderName()) && e.getModifiers() != 0) {

		    	   PreferencesPanelKeystores.this.localKeystoreSelectedPath = ks.getLib();

		       } else if (ks.getName().equals(AOKeyStore.DNIEJAVA.getName()) && e.getModifiers() != 0) {
			    	final AOKeyStoreManager ksm = new AOKeyStoreManager();
			   		try {
						// Proporcionamos el componente padre como parametro
						ksm.init(
							AOKeyStore.DNIEJAVA,
							null,
							null,
							new Object[] { this },
							true
						);
					}
					catch (final AOCancelledOperationException aoce) {
						PreferencesPanelKeystores.getKeystores().setSelectedIndex(0);
						throw aoce;
					}
			   		catch (final Exception exc) {
			   			PreferencesPanelKeystores.getKeystores().setSelectedIndex(0);
			   			LOGGER.log(Level.WARNING,"No se ha podido cargar el DNIe: " + exc, exc); //$NON-NLS-1$
			   			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.14"), //$NON-NLS-1$
		    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, exc); //$NON-NLS-1$
					}
		       }
		    }
		});

        this.panelKeystores.setBorder(
    		BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.1") //$NON-NLS-1$
			)
		);

        this.panelKeystores.setLayout(new GridBagLayout());

        final GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.fill = GridBagConstraints.BOTH;
        gbc1.gridy = 0;
        gbc1.weightx = 1.0;
        gbc1.gridx = 0;

		keystores.addItemListener(modificationListener);
		keystores.addKeyListener(keyListener);

        this.panelKeystores.add(keystores, gbc1);

        gbc1.gridx++;

        this.showContentButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
				final AOKeyStoreManager ksm;
				AOKeyStore ks = AOKeyStore.getKeyStore(((RegisteredKeystore) PreferencesPanelKeystores.getKeystores().getSelectedItem()).getName());
				try {
					String lib = null;
					if (ks != null && (AOKeyStore.PKCS12.equals(ks) || AOKeyStore.PKCS11.getName().equals(ks.getName()))) {
						lib = PreferencesPanelKeystores.this.localKeystoreSelectedPath;
						if (lib == null) {
							lib = PreferencesManager.get(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH);
						}
					} else if (ks == null) {
				        final Map<String, String> regResult = KeyStorePreferencesManager.getSmartCardsRegistered();
						for (final String key : regResult.keySet()) {
							if (((RegisteredKeystore) PreferencesPanelKeystores.getKeystores().getSelectedItem()).getName().equals(key)){
								ks = AOKeyStore.PKCS11;
								ks.setName(key);
								lib = regResult.get(key);
							}
						}
					}

					ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
						ks,
						lib,
						"default", //$NON-NLS-1$
						ks.getStorePasswordCallback(this),
						this
					);

					String libName = null;
					if (lib != null) {
						final File file = new File(lib);
						libName = file.getName();
					}

					final CertificateSelectionDialog csd = new CertificateSelectionDialog(
						PreferencesPanelKeystores.this,
						new AOKeyStoreDialog(
							ksm,
				        	this,
		                	true,
		                	PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SHOWEXPIREDCERTS),
		                 	false,
		                 	getCertFilters(),
		                 	false,
		                    libName),
						SimpleAfirmaMessages.getString(
							"PreferencesPanelKeyStores.18", //$NON-NLS-1$
							ks.toString()
						),
						SimpleAfirmaMessages.getString(
							"PreferencesPanelKeyStores.19", //$NON-NLS-1$
							ks.toString()
						),
						false,
						true
					);
					csd.showDialog();
				} catch (final AOCancelledOperationException acoe) {
						return;
				} catch (final Exception kse) {
						AOUIFactory.showErrorMessage(
						SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.20"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.18", ks.toString()), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						kse
						);
						Logger.getLogger("es.gob.afirma").warning("Error al recuperar el almacen por defecto seleccionado: " + e); //$NON-NLS-1$ //$NON-NLS-2$
				}
            }
        });

        this.panelKeystores.add(this.showContentButton, gbc1);

        gbc1.gridy++;
        gbc1.gridx = 0;

		this.callsFromNavigator.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.callsFromNavigator.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.17") //$NON-NLS-1$
		);
		this.callsFromNavigator.setMnemonic('N');
		this.callsFromNavigator.addItemListener(modificationListener);
		this.callsFromNavigator.addKeyListener(keyListener);

        this.panelKeystores.add(this.callsFromNavigator, gbc1);

        mainPanel.add(this.panelKeystores, gbc);

        final JPanel certsFiltersPanel = new JPanel();
        certsFiltersPanel.setBorder(
    		BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.4") //$NON-NLS-1$
			)
		);
        certsFiltersPanel.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(5, 0, 0, 0);
        c.weightx = 1.0;
        c.gridy = 0;
        c.gridy++;

		this.hideDniStartScreen.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.hideDniStartScreen.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.82") //$NON-NLS-1$
		);
		this.hideDniStartScreen.setMnemonic('D');
		this.hideDniStartScreen.addItemListener(modificationListener);
		this.hideDniStartScreen.addKeyListener(keyListener);

        certsFiltersPanel.add(this.hideDniStartScreen, c);
        c.gridy++;
		c.gridy++;

		this.showExpiredCerts.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.showExpiredCerts.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.177")); //$NON-NLS-1$
		this.showExpiredCerts.setMnemonic('M');
		this.showExpiredCerts.addItemListener(modificationListener);
		this.showExpiredCerts.addKeyListener(keyListener);

        certsFiltersPanel.add(this.showExpiredCerts, c);
        c.gridy++;

		this.onlySignature.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.onlySignature.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.2")); //$NON-NLS-1$
		this.onlySignature.setMnemonic('E');
		this.onlySignature.addItemListener(modificationListener);
		this.onlySignature.addKeyListener(keyListener);

        certsFiltersPanel.add(this.onlySignature, c);
        c.gridy++;

		this.onlyAlias.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.onlyAlias.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.5")); //$NON-NLS-1$
		this.onlyAlias.setMnemonic('S');
		this.onlyAlias.addItemListener(modificationListener);
		this.onlyAlias.addKeyListener(keyListener);
        certsFiltersPanel.add(this.onlyAlias, c);

        c.gridy++;

		this.skipAuthCertDnie.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.skipAuthCertDnie.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.32")); //$NON-NLS-1$
		this.skipAuthCertDnie.setMnemonic('C');
		this.skipAuthCertDnie.addItemListener(modificationListener);
		this.skipAuthCertDnie.addKeyListener(keyListener);
        certsFiltersPanel.add(this.skipAuthCertDnie, c);

        gbc.gridy++;
        mainPanel.add(certsFiltersPanel, gbc);

        final JPanel inteligentCardsPanel = new JPanel();
        inteligentCardsPanel.setBorder(
    		BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.9") //$NON-NLS-1$
			)
		);
        inteligentCardsPanel.setLayout(new GridBagLayout());

        final GridBagConstraints icpConstraints = new GridBagConstraints();
        icpConstraints.fill = GridBagConstraints.HORIZONTAL;
        icpConstraints.gridy = 0;
        icpConstraints.gridx = 0;
        icpConstraints.weightx = 5.0;
        icpConstraints.gridwidth = 5;

        final JLabel smartCardDescLbl = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.33")); //$NON-NLS-1$

        inteligentCardsPanel.add(smartCardDescLbl, icpConstraints);

        smartCards = new JComboBox<RegisteredKeystore>();

        final RegisteredKeystore emptyItem = new RegisteredKeystore();
        emptyItem.setName(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.31")); //$NON-NLS-1$
        smartCards.addItem(emptyItem);

        final Map<String, String> regResult = KeyStorePreferencesManager.getSmartCardsRegistered();

        if (!regResult.isEmpty()) {
			for (final String smartCardName : regResult.keySet()) {
				final RegisteredKeystore newPKCS11 = new RegisteredKeystore(AOKeyStore.PKCS11);
				newPKCS11.setName(smartCardName);
				final String lib = regResult.get(smartCardName);
				newPKCS11.setLib(lib);
				smartCards.addItem(newPKCS11);
				keystores.addItem(newPKCS11);
			}
        }

		this.connectButton.setEnabled(false);
		this.modifyCardButton.setEnabled(false);
		this.deleteCardButton.setEnabled(false);

		smartCards.repaint();
		keystores.repaint();

		smartCards.addActionListener (new ActionListener () {
		    @Override
			public void actionPerformed(final ActionEvent e) {
		    	if (e.getModifiers() != 0 && smartCards.getSelectedIndex() == 0) {
		    		PreferencesPanelKeystores.this.connectButton.setEnabled(false);
					PreferencesPanelKeystores.this.modifyCardButton.setEnabled(false);
					PreferencesPanelKeystores.this.deleteCardButton.setEnabled(false);
		    	} else {
		    		PreferencesPanelKeystores.this.connectButton.setEnabled(true);
					PreferencesPanelKeystores.this.modifyCardButton.setEnabled(true);
					PreferencesPanelKeystores.this.deleteCardButton.setEnabled(true);
		    	}
		    }
		});

		loadPreferences();

		icpConstraints.gridy++;

		icpConstraints.weightx = 1.0;
		icpConstraints.gridwidth = 1;

        inteligentCardsPanel.add(PreferencesPanelKeystores.smartCards, icpConstraints);

        icpConstraints.gridx++;

        this.connectButton.addActionListener(
        		ae -> connectSmartCard(this)
		);

        inteligentCardsPanel.add(this.connectButton, icpConstraints);

        icpConstraints.gridx++;

        this.addCardButton.addActionListener(
        		ae -> addSmartCardDlg(this)
		);

        inteligentCardsPanel.add(this.addCardButton, icpConstraints);

        icpConstraints.gridx++;

        this.modifyCardButton.addActionListener(
        		ae -> modifySmartCardDlg(this)
		);

        inteligentCardsPanel.add(this.modifyCardButton, icpConstraints);

        icpConstraints.gridx++;

        this.deleteCardButton.addActionListener(
        		ae -> deleteSmartCardDlg(this, ((RegisteredKeystore) smartCards.getSelectedItem()).getName())
		);

        inteligentCardsPanel.add(this.deleteCardButton, icpConstraints);

        gbc.gridy++;
        mainPanel.add(inteligentCardsPanel, gbc);

        gbc.gridy++;
        gbc.weighty = 1.0;
        mainPanel.add(new JPanel(), gbc);

		// Panel para el boton de restaurar la configuracion
		final JPanel panelGeneral = new JPanel(new FlowLayout(FlowLayout.TRAILING));

		final JButton restoreConfigButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.147") //$NON-NLS-1$
		);

		restoreConfigButton.setMnemonic('R');
		restoreConfigButton.addActionListener(ae -> {
			if (AOUIFactory.showConfirmDialog(getParent(), SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.15"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("PreferencesPanel.139"), //$NON-NLS-1$
					JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {

				loadDefaultPreferences();

			}
		});
		restoreConfigButton.getAccessibleContext()
				.setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.136") //$NON-NLS-1$
		);

		gbc.gridy++;
		gbc.weighty = 0.0;
		panelGeneral.add(restoreConfigButton, gbc);

		gbc.gridy++;

		mainPanel.add(panelGeneral, gbc);

		setViewportView(mainPanel);
	}

	/**
	 * M&eacute;todo para probar las conexiones hacia el almac&eaucte;n de tarjeta inteligente
	 * @param preferencesPanelKeystores panel padre donde mostrar el di&aacute;logo
	 */
	 static void connectSmartCard(final Container container) {

		final AOKeyStore ks = AOKeyStore.PKCS11;

    	// Cursor en espera
    	container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

		try {
			final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
					ks,
					((RegisteredKeystore) smartCards.getSelectedItem()).getLib(),
					((RegisteredKeystore) smartCards.getSelectedItem()).getName(),
					ks.getStorePasswordCallback(container),
					container
				);
			if (ksm != null) {
				AOUIFactory.showMessageDialog(container, SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.29"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.30"), AOUIFactory.INFORMATION_MESSAGE); //$NON-NLS-1$
			}
		} catch (final Exception kse) {
			AOUIFactory.showErrorMessage(
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.28"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.18", ks.toString()), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				kse
			);
			Logger.getLogger("es.gob.afirma").warning("Error al conectar tarjeta inteligente: " + kse); //$NON-NLS-1$ //$NON-NLS-2$
		}

    	// Cursor por defecto
    	container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	}

	/** Di&aacute;logo para a&ntilde;adir tarjeta inteligente.
	 * @param container Contenedor en el que se define el di&aacute;logo. */
    public void addSmartCardDlg(final Container container) {

    	// Cursor en espera
    	container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    	final SmartCardPanel smartCardPanel = new SmartCardPanel();
    	smartCardPanel.getAccessibleContext().setAccessibleDescription(
    			SimpleAfirmaMessages.getString("SmartCardDialog.0")); //$NON-NLS-1$

    	// Cursor por defecto
    	container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

    	if(AOUIFactory.showConfirmDialog(
				container,
				smartCardPanel,
				SimpleAfirmaMessages.getString("SmartCardDialog.0"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION,
				JOptionPane.DEFAULT_OPTION
		) == JOptionPane.OK_OPTION) {

    		if (smartCardPanel.getCardNameTxt().getText().isEmpty()
    			|| smartCardPanel.getControllerNameTxt().getText().isEmpty() ) {
    			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.27"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    					AOUIFactory.ERROR_MESSAGE,
    					new Exception(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.27")) //$NON-NLS-1$
    					);

    			addSmartCardDlg(container);

    		} else {

    			final boolean regAdded = KeyStorePreferencesManager.addSmartCardToRec(smartCardPanel.getCardNameTxt().getText() ,
    																					smartCardPanel.getControllerNameTxt().getText());

    			if (regAdded) {
    				final RegisteredKeystore ks = new RegisteredKeystore();
    				ks.setName(smartCardPanel.getCardNameTxt().getText());
    				ks.setLib(smartCardPanel.getControllerNameTxt().getText());
    				ks.setProviderName(AOKeyStore.PKCS11.getProviderName());
    				smartCards.addItem(ks);
    				smartCards.setSelectedItem(ks);
    				smartCards.repaint();
    				keystores.addItem(ks);
    				keystores.repaint();
    			}

    			if (smartCards.getItemCount() > 1) {
    				this.connectButton.setEnabled(true);
    				this.modifyCardButton.setEnabled(true);
    				this.deleteCardButton.setEnabled(true);
    			}

    		}
    	}
    }

	/** Di&aacute;logo para modificar tarjeta inteligente.
	 * @param container Contenedor en el que se define el di&aacute;logo. */
    public static void modifySmartCardDlg(final Container container) {

    	// Cursor en espera
    	container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    	final SmartCardPanel smartCardPanel = new SmartCardPanel();
    	smartCardPanel.getAccessibleContext().setAccessibleDescription(
    			SimpleAfirmaMessages.getString("SmartCardDialog.9")); //$NON-NLS-1$
    	final String oldCardName = ((RegisteredKeystore) smartCards.getSelectedItem()).getName();
    	final String oldLibName = ((RegisteredKeystore) smartCards.getSelectedItem()).getLib();
    	smartCardPanel.getCardNameTxt().setText(oldCardName);
    	smartCardPanel.getControllerNameTxt().setText(oldLibName);
    	smartCardPanel.repaint();

    	// Cursor por defecto
    	container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

    	if(AOUIFactory.showConfirmDialog(
				container,
				smartCardPanel,
				SimpleAfirmaMessages.getString("SmartCardDialog.9"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION,
				JOptionPane.DEFAULT_OPTION
		) == JOptionPane.OK_OPTION) {

    		if (smartCardPanel.getCardNameTxt().getText().isEmpty()
        			|| smartCardPanel.getControllerNameTxt().getText().isEmpty() ) {
    			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.27"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    					AOUIFactory.ERROR_MESSAGE,
    					new Exception(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.27")) //$NON-NLS-1$
    					);

    			modifySmartCardDlg(container);
        	} else {
    			final boolean regDeleted = KeyStorePreferencesManager.deleteSmartCardRec(oldCardName);

    			if (regDeleted) {
    				for (int i = 0 ; i < smartCards.getItemCount() ; i++) {
    					System.out.println(smartCards.getItemAt(i).getName());
    					if (smartCards.getItemAt(i).getName().equals(oldCardName)) {
    						smartCards.removeItemAt(i);
    					}
    				}
    				for (int i = 0 ; i < keystores.getItemCount() ; i++) {
    					if (keystores.getItemAt(i).getName().equals(oldCardName)) {
    						keystores.removeItemAt(i);
    					}
    				}
    			}

    			keystores.repaint();
    			smartCards.repaint();

    			final boolean regAdded = KeyStorePreferencesManager.addSmartCardToRec(smartCardPanel.getCardNameTxt().getText() , smartCardPanel.getControllerNameTxt().getText());

    			if (regAdded) {
    				final RegisteredKeystore ks = new RegisteredKeystore();
    				ks.setName(smartCardPanel.getCardNameTxt().getText());
    				ks.setLib(smartCardPanel.getControllerNameTxt().getText());
    				ks.setProviderName(AOKeyStore.PKCS11.getProviderName());
    				smartCards.addItem(ks);
    				smartCards.setSelectedItem(ks);
    				smartCards.repaint();
    				keystores.addItem(ks);
    				keystores.repaint();
    			}
        	}
    	}
    }

	/** Di&aacute;logo para eliminar tarjeta inteligente.
	 * @param container Contenedor en el que se define el di&aacute;logo.
	 * @param name nombre de la clave que contiene la tarjeta para borrar.
	 * */
    public void deleteSmartCardDlg(final Container container, final String name) {

    	// Cursor en espera
    	container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    	// Cursor por defecto
    	container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

    	if(AOUIFactory.showConfirmDialog(
				container,
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.25"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("SmartCardDialog.10"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION,
				JOptionPane.DEFAULT_OPTION
		) == JOptionPane.OK_OPTION) {

			final boolean regDeleted = KeyStorePreferencesManager.deleteSmartCardRec(name);

			if (regDeleted) {
				for (int i = 0 ; i < keystores.getItemCount() ; i++) {
					if (keystores.getItemAt(i).getName().equals(name)) {
						keystores.removeItemAt(i);
					}
				}
				for (int i = 0 ; i < smartCards.getItemCount() ; i++) {
					if (smartCards.getItemAt(i).getName().equals(name)) {
						smartCards.removeItemAt(i);
					}
				}
			}

			keystores.repaint();
			smartCards.repaint();

			if (smartCards.getItemCount() == 1 || smartCards.getSelectedIndex() == 0) {
				this.connectButton.setEnabled(false);
				this.modifyCardButton.setEnabled(false);
				this.deleteCardButton.setEnabled(false);
			}
    	}
    }

	/** Guarda las preferencias. */
	void savePreferences() {

		if (this.localKeystoreSelectedPath != null) {
			PreferencesManager.put(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH, this.localKeystoreSelectedPath);
			KeyStorePreferencesManager.setLastSelectedKeystoreLib(this.localKeystoreSelectedPath);
		}

		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_USE_DEFAULT_STORE_IN_BROWSER_CALLS, this.callsFromNavigator.isSelected());
		final RegisteredKeystore rks = getDefaultStore();
		AOKeyStore aoks = AOKeyStore.getKeyStore(rks.getName());

		if (aoks == null && AOKeyStore.PKCS11.getProviderName().equals(rks.getProviderName())) {
			aoks = AOKeyStore.PKCS11;
			aoks.setName(rks.getName());
		}

		if (aoks != null && AOKeyStore.PKCS11.getProviderName().equals(aoks.getProviderName()) && rks.getLib() != null) {
			PreferencesManager.put(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH, rks.getLib());
		}

		if (AOKeyStore.PKCS12.getName().equals(rks.getName())) {
			aoks = AOKeyStore.PKCS12;
		}

		if (aoks != null) {
			PreferencesManager.put(
					PREFERENCE_KEYSTORE_DEFAULT_STORE,
					aoks.getName()
			);
			KeyStorePreferencesManager.setLastSelectedKeystore(aoks.getName());
		}

		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SHOWEXPIREDCERTS, this.showExpiredCerts.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN, this.hideDniStartScreen.isSelected());
		PreferencesManager.putBoolean(PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS, this.onlySignature.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS, this.onlyAlias.isSelected());
		KeyStorePreferencesManager.setSkipAuthCertDNIe(this.skipAuthCertDnie.isSelected());

	}

	void loadPreferences() {

		final String ks = PreferencesManager.get(PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE);
		final AOKeyStore aoks = AOKeyStore.getKeyStore(ks);
		RegisteredKeystore rks = null;

		if (aoks == null) {
			final Map<String, String> regResult = KeyStorePreferencesManager.getSmartCardsRegistered();
			if (!regResult.isEmpty()) {
				for (final String smartCardName : regResult.keySet()) {
					if (ks.equals(smartCardName)) {
						rks = new RegisteredKeystore(AOKeyStore.PKCS11);
						rks.setName(smartCardName);
					}
				}
			}
		} else {
			rks = new RegisteredKeystore(aoks);
		}

		if (rks != null) {
			for (int i = 0; i < PreferencesPanelKeystores.getKeystores().getItemCount() ; i++) {
				if (rks.getName().equals(getKeystores().getItemAt(i).getName())) {
					getKeystores().setSelectedIndex(i);
				}
			}
		}
		PreferencesPanelKeystores.getKeystores().repaint();
		this.callsFromNavigator.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_USE_DEFAULT_STORE_IN_BROWSER_CALLS));
		this.showExpiredCerts.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SHOWEXPIREDCERTS));
		this.hideDniStartScreen.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN));
		this.onlySignature.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS));
		this.onlyAlias.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS));
		this.skipAuthCertDnie.setSelected(KeyStorePreferencesManager.getSkipAuthCertDNIe());

        revalidate();
        repaint();

	}

	void loadDefaultPreferences() {
		PreferencesPanelKeystores.getKeystores().setSelectedIndex(0);
		this.callsFromNavigator.setSelected(false);
		this.showExpiredCerts.setSelected(false);
		this.hideDniStartScreen.setSelected(false);
		this.onlySignature.setSelected(false);
		this.onlyAlias.setSelected(false);
		this.skipAuthCertDnie.setSelected(false);

        revalidate();
        repaint();

	}

    static List<? extends CertificateFilter> getCertFilters() {

    	final List<CertificateFilter> filters = new ArrayList<>();

    	if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS)) {
    		filters.add(new KeyUsageFilter(KeyUsageFilter.SIGN_CERT_USAGE));
    	}
    	if (KeyStorePreferencesManager.getSkipAuthCertDNIe()) {
    		filters.add(new SkipAuthDNIeFilter());
    	}
    	if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS)) {
    		filters.add(new PseudonymFilter());
    	}
    	if (filters.size() > 1) {
    		return Arrays.asList(
				new MultipleCertificateFilter(filters.toArray(new CertificateFilter[0]))
			);
    	}
		else if (filters.size() == 1) {
    		return filters;
    	}
    	return null;
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

	static RegisteredKeystore getDefaultStore() {
		return PreferencesPanelKeystores.getKeystores().getItemAt(PreferencesPanelKeystores.getKeystores().getSelectedIndex());
	}

	public static JComboBox<RegisteredKeystore> getKeystores() {
		return keystores;
	}

	public static void setKeystores(final JComboBox<RegisteredKeystore> keystores) {
		PreferencesPanelKeystores.keystores = keystores;
	}
}
