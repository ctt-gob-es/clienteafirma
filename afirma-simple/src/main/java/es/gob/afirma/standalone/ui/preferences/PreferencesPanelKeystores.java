/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS;

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
import java.util.prefs.BackingStoreException;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.prefs.KeyStorePreferencesManager;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.CertificateFilter;
import es.gob.afirma.keystores.MultipleCertificateFilter;
import es.gob.afirma.keystores.filters.PseudonymFilter;
import es.gob.afirma.keystores.filters.SkipAuthDNIeFilter;
import es.gob.afirma.keystores.filters.rfc.KeyUsageFilter;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.SimpleKeyStoreManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.ui.core.jse.certificateselection.CertificateSelectionDialog;

/** Pesta&ntilde;a de configuraci&oacute;n de las preferencias de certificados.
 * @author Jos&eacute; Montero. */
final class PreferencesPanelKeystores extends JScrollPane {

	private static final long serialVersionUID = 3491050093362323228L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	JComboBox<RegisteredKeystore> keystores;

	JComboBox<RegisteredKeystore> smartCards;

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

	static final String[] EXTS_PKCS12 = { "pfx", "p12" }; //$NON-NLS-1$ //$NON-NLS-2$

	private static final String EXTS_DESC_PKCS12 = " (*.p12, *.pfx)"; //$NON-NLS-1$

	private static final String DEFAULT_VALUE = "default"; //$NON-NLS-1$

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

		getVerticalScrollBar().setUnitIncrement(16);

		final JPanel mainPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;

        // Creamos el listado de almacenes (luego se poblara)
		this.keystores = new JComboBox<>();

		this.keystores.addActionListener (new ActionListener () {
		    @Override
		    public void actionPerformed(final ActionEvent e) {

		    	final Object selectedItem = PreferencesPanelKeystores.this.keystores.getSelectedItem();
		    	if (selectedItem == null) {
		    		return;
		    	}

		    	final RegisteredKeystore ks = (RegisteredKeystore) PreferencesPanelKeystores.this.keystores.getSelectedItem();
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
		    			PreferencesPanelKeystores.this.keystores.setSelectedIndex(0);
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
		    			PreferencesPanelKeystores.this.keystores.setSelectedIndex(0);
		    			throw aoce;
		    		}
		    		catch (final Exception exc) {
		    			PreferencesPanelKeystores.this.keystores.setSelectedIndex(0);
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
        gbc1.fill = GridBagConstraints.HORIZONTAL;
        gbc1.weightx = 1.0;
        gbc1.gridx = 0;
        gbc1.gridy = 0;

		this.keystores.addItemListener(modificationListener);
		this.keystores.addKeyListener(keyListener);

        this.panelKeystores.add(this.keystores, gbc1);

        gbc1.gridwidth = 2;
        gbc1.gridx++;

        this.showContentButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
				final AOKeyStoreManager ksm;
				String lib = null;
				AOKeyStore ks = SimpleKeyStoreManager.getKeyStore(((RegisteredKeystore) PreferencesPanelKeystores.this.keystores.getSelectedItem()).getName());
				if (ks == null) {
			        final Map<String, String> userRegResult = KeyStorePreferencesManager.getUserSmartCardsRegistered();
					for (final String key : userRegResult.keySet()) {
						if (((RegisteredKeystore) PreferencesPanelKeystores.this.keystores.getSelectedItem()).getName().equals(key)){
							ks = AOKeyStore.PKCS11;
							ks.setName(key);
							lib = userRegResult.get(key);
						}
					}
			        final Map<String, String> systemRegResult = KeyStorePreferencesManager.getSystemSmartCardsRegistered();
					for (final String key : systemRegResult.keySet()) {
						if (((RegisteredKeystore) PreferencesPanelKeystores.this.keystores.getSelectedItem()).getName().equals(key)){
							ks = AOKeyStore.PKCS11;
							ks.setName(key);
							lib = userRegResult.get(key);
						}
					}
				} else if (AOKeyStore.PKCS12.equals(ks) || AOKeyStore.PKCS11.getName().equals(ks.getName())) {
					lib = PreferencesPanelKeystores.this.localKeystoreSelectedPath;
					if (lib == null) {
						lib = PreferencesManager.get(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH);
					}
				}

				// Si no se identifico el tipo de almacen, no intentamos mostrar ningun contenido
				if (ks == null) {
					AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.36"), //$NON-NLS-1$
	    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, null); //$NON-NLS-1$
					return;
				}

				try {
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
					csd.showDialog(false);
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

        this.showContentButton.setMnemonic('V');
        this.panelKeystores.add(this.showContentButton, gbc1);

        gbc1.gridx = 0;
        gbc1.gridy++;
        gbc1.insets = new Insets(5, 0, 0, 0);

        this.callsFromNavigator.setEnabled(!this.blocked);
		this.callsFromNavigator.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.callsFromNavigator.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.17") //$NON-NLS-1$
		);
		this.callsFromNavigator.addItemListener(modificationListener);
		this.callsFromNavigator.addKeyListener(keyListener);

        this.panelKeystores.add(this.callsFromNavigator, gbc1);


		this.hideDniStartScreen.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.hideDniStartScreen.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.82") //$NON-NLS-1$
		);
		this.hideDniStartScreen.addItemListener(modificationListener);
		this.hideDniStartScreen.addKeyListener(keyListener);

		gbc1.gridy++;

		this.panelKeystores.add(this.hideDniStartScreen, gbc1);

        mainPanel.add(this.panelKeystores, gbc);

        final JPanel certsFiltersPanel = new JPanel();
        certsFiltersPanel.setBorder(
    		BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.4") //$NON-NLS-1$
			)
		);
        certsFiltersPanel.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(5, 0, 0, 0);
        c.weightx = 1.0;
        c.gridy = 0;

        this.showExpiredCerts.setEnabled(!this.blocked);
		this.showExpiredCerts.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.showExpiredCerts.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.177")); //$NON-NLS-1$
		this.showExpiredCerts.addItemListener(modificationListener);
		this.showExpiredCerts.addKeyListener(keyListener);

        certsFiltersPanel.add(this.showExpiredCerts, c);
        c.gridy++;

		this.onlySignature.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.onlySignature.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.2")); //$NON-NLS-1$
		this.onlySignature.addItemListener(modificationListener);
		this.onlySignature.addKeyListener(keyListener);

        certsFiltersPanel.add(this.onlySignature, c);
        c.gridy++;

		this.onlyAlias.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.onlyAlias.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.5")); //$NON-NLS-1$
		this.onlyAlias.addItemListener(modificationListener);
		this.onlyAlias.addKeyListener(keyListener);
        certsFiltersPanel.add(this.onlyAlias, c);

        c.gridy++;

		this.skipAuthCertDnie.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.skipAuthCertDnie.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.32")); //$NON-NLS-1$
		this.skipAuthCertDnie.addItemListener(modificationListener);
		this.skipAuthCertDnie.addKeyListener(keyListener);
        certsFiltersPanel.add(this.skipAuthCertDnie, c);

        gbc.gridy++;
        mainPanel.add(certsFiltersPanel, gbc);

        final JPanel smartCardsPanel = new JPanel();
        smartCardsPanel.setBorder(
    		BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.9") //$NON-NLS-1$
			)
		);
        smartCardsPanel.setLayout(new GridBagLayout());

        final GridBagConstraints scpConstraints = new GridBagConstraints();
        scpConstraints.fill = GridBagConstraints.HORIZONTAL;
        scpConstraints.gridy = 0;
        scpConstraints.gridx = 0;
        scpConstraints.weightx = 5.0;
        scpConstraints.gridwidth = 5;

        final JLabel smartCardDescLbl = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.33")); //$NON-NLS-1$

        smartCardsPanel.add(smartCardDescLbl, scpConstraints);

        // Creamos el combo de tarjetas
        this.smartCards = new JComboBox<>();

        // Cargamos los elementos del listado de tarjetas y de los almacenes
        loadSmartCardsComboBox(this.smartCards);
        loadKeyStoresComboBox(this.keystores, this.smartCards);
		this.smartCards.repaint();
		this.keystores.repaint();

		this.connectButton.setEnabled(false);
		this.modifyCardButton.setEnabled(false);
		this.deleteCardButton.setEnabled(false);

		this.smartCards.addActionListener (e -> {
			if (e.getModifiers() != 0 && this.smartCards.getSelectedIndex() == 0) {
				PreferencesPanelKeystores.this.connectButton.setEnabled(false);
				PreferencesPanelKeystores.this.modifyCardButton.setEnabled(false);
				PreferencesPanelKeystores.this.deleteCardButton.setEnabled(false);
			} else if (e.getModifiers() != 0 && ((RegisteredKeystore) this.smartCards.getSelectedItem()).isSystemSmartCard()){
				PreferencesPanelKeystores.this.connectButton.setEnabled(true);
				PreferencesPanelKeystores.this.modifyCardButton.setEnabled(false);
				PreferencesPanelKeystores.this.deleteCardButton.setEnabled(false);
			} else {
				PreferencesPanelKeystores.this.connectButton.setEnabled(true);
				PreferencesPanelKeystores.this.modifyCardButton.setEnabled(true);
				PreferencesPanelKeystores.this.deleteCardButton.setEnabled(true);
			}
		});

		loadPreferences();

		scpConstraints.gridy++;

		scpConstraints.weightx = 1.0;
		scpConstraints.gridwidth = 1;

        smartCardsPanel.add(this.smartCards, scpConstraints);

        scpConstraints.gridx++;

        this.connectButton.addActionListener(
        		ae -> connectSmartCard(this)
		);

        this.connectButton.setMnemonic('T');
        smartCardsPanel.add(this.connectButton, scpConstraints);

        scpConstraints.gridx++;

        this.addCardButton.setMnemonic('J');
        this.addCardButton.addActionListener(
        		ae -> addSmartCardDlg(this)
		);

        smartCardsPanel.add(this.addCardButton, scpConstraints);

        scpConstraints.gridx++;

        this.modifyCardButton.setMnemonic('I');
        this.modifyCardButton.addActionListener(
        		ae -> modifySmartCardDlg(this)
		);

        smartCardsPanel.add(this.modifyCardButton, scpConstraints);

        scpConstraints.gridx++;

        this.deleteCardButton.setMnemonic('M');
        this.deleteCardButton.addActionListener(
        		ae -> deleteSmartCardDlg(this, ((RegisteredKeystore) this.smartCards.getSelectedItem()).getName())
		);

        smartCardsPanel.add(this.deleteCardButton, scpConstraints);

        gbc.gridy++;
        mainPanel.add(smartCardsPanel, gbc);

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

				restorePreferences();

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

	private static void loadKeyStoresComboBox(final JComboBox<RegisteredKeystore> ksComboBox, final JComboBox<RegisteredKeystore> smartCardsComboBox) {

		final Platform.OS os = Platform.getOS();

		// Agregamos los almacenes dependientes de sistema
		if (Platform.OS.WINDOWS.equals(os)) {
			ksComboBox.addItem(new RegisteredKeystore(AOKeyStore.WINDOWS));
			if (SimpleKeyStoreManager.isFirefoxAvailable()) {
				ksComboBox.addItem(new RegisteredKeystore(AOKeyStore.MOZ_UNI));
			}
		}
		else if (Platform.OS.MACOSX.equals(os)) {
			ksComboBox.addItem(new RegisteredKeystore(AOKeyStore.APPLE));
			if (SimpleKeyStoreManager.isFirefoxAvailable()) {
				ksComboBox.addItem(new RegisteredKeystore(AOKeyStore.MOZ_UNI));
			}
		}
		else {
			ksComboBox.addItem(new RegisteredKeystore(AOKeyStore.SHARED_NSS));
		}

		// Agregamos los almacenes comunes
		ksComboBox.addItem(new RegisteredKeystore(AOKeyStore.PKCS12));
		ksComboBox.addItem(new RegisteredKeystore(AOKeyStore.DNIEJAVA));

		// Agregamos los almacenes en tarjeta
        if (smartCardsComboBox != null && smartCardsComboBox.getItemCount() > 1) {
        	for (int i = 1; i < smartCardsComboBox.getItemCount(); i++) {
        		ksComboBox.addItem(smartCardsComboBox.getItemAt(i));
        	}
        }
	}

	private static void loadSmartCardsComboBox(final JComboBox<RegisteredKeystore> smartCardsCb) {

        final RegisteredKeystore emptyItem = new RegisteredKeystore();
        emptyItem.setName(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.31")); //$NON-NLS-1$
        smartCardsCb.addItem(emptyItem);

        final Map<String, String> userRegResult = KeyStorePreferencesManager.getUserSmartCardsRegistered();

        if (!userRegResult.isEmpty()) {
			for (final String smartCardName : userRegResult.keySet()) {
				final RegisteredKeystore newPKCS11 = new RegisteredKeystore(AOKeyStore.PKCS11);
				newPKCS11.setName(smartCardName);
				final String lib = userRegResult.get(smartCardName);
				newPKCS11.setLib(lib);
				smartCardsCb.addItem(newPKCS11);
			}
        }

        final Map<String, String> systemRegResult = KeyStorePreferencesManager.getSystemSmartCardsRegistered();

        if (!systemRegResult.isEmpty()) {
			for (final String smartCardName : systemRegResult.keySet()) {
				final RegisteredKeystore newPKCS11 = new RegisteredKeystore(AOKeyStore.PKCS11, true);
				newPKCS11.setName(smartCardName);
				final String lib = systemRegResult.get(smartCardName);
				newPKCS11.setLib(lib);
				smartCardsCb.addItem(newPKCS11);
			}
        }
	}

	/**
	 * M&eacute;todo para probar las conexiones hacia el almac&eaucte;n de tarjeta inteligente
	 * @param preferencesPanelKeystores panel padre donde mostrar el di&aacute;logo
	 */
	 void connectSmartCard(final Container container) {

		final AOKeyStore ks = AOKeyStore.PKCS11;
		ks.setName(((RegisteredKeystore) this.smartCards.getSelectedItem()).getName());

    	// Cursor en espera
    	container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

		try {
			final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
					ks,
					((RegisteredKeystore) this.smartCards.getSelectedItem()).getLib(),
					((RegisteredKeystore) this.smartCards.getSelectedItem()).getName(),
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

    	final SmartCardPanel smartCardPanel = new SmartCardPanel();
    	smartCardPanel.getAccessibleContext().setAccessibleDescription(
    			SimpleAfirmaMessages.getString("SmartCardDialog.0")); //$NON-NLS-1$

    	if(AOUIFactory.showConfirmDialog(
				container,
				smartCardPanel,
				SimpleAfirmaMessages.getString("SmartCardDialog.0"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION,
				JOptionPane.DEFAULT_OPTION
		) == JOptionPane.OK_OPTION) {

        	final String smartCardName = smartCardPanel.getCardNameTxt().getText();
        	final String smartCardP11Path = smartCardPanel.getControllerPathTxt().getText();

    		if (smartCardName.isEmpty()
    			|| smartCardP11Path.isEmpty() ) {
    			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.27"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    					AOUIFactory.ERROR_MESSAGE,
    					new Exception(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.27")) //$NON-NLS-1$
    					);

    			addSmartCardDlg(container);

    		} else if (checkDuplicatedName(smartCardName)) {

    			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.34"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    					AOUIFactory.ERROR_MESSAGE,
    					new Exception(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.34")) //$NON-NLS-1$
    					);

    			addSmartCardDlg(container);

    		} else if (checkDuplicatedLib(smartCardP11Path)) {

    			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.35"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    					AOUIFactory.ERROR_MESSAGE,
    					new Exception(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.35")) //$NON-NLS-1$
    					);

    			addSmartCardDlg(container);

    		} else {

            	// Cursor en espera
            	container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    			LOGGER.info("Creamos almacen para la tarjeta inteligente: " + LoggerUtil.getTrimStr(smartCardName)); //$NON-NLS-1$

    			final boolean regAdded = KeyStorePreferencesManager.addSmartCardToUserRec(
    					smartCardName, smartCardP11Path);

    			if (regAdded) {
    				final RegisteredKeystore ks = new RegisteredKeystore();
    				ks.setName(smartCardName);
    				ks.setLib(smartCardP11Path);
    				ks.setProviderName(AOKeyStore.PKCS11.getProviderName());
    				this.smartCards.addItem(ks);
    				this.smartCards.setSelectedItem(ks);
    				this.smartCards.repaint();
    				this.keystores.addItem(ks);
    				this.keystores.repaint();
    			}

    			if (this.smartCards.getItemCount() > 1) {
    				this.connectButton.setEnabled(true);
    				this.modifyCardButton.setEnabled(true);
    				this.deleteCardButton.setEnabled(true);
    			}

    	    	// Cursor por defecto
    	    	container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    		}
    	}
    }

	/** Di&aacute;logo para modificar tarjeta inteligente.
	 * @param container Contenedor en el que se define el di&aacute;logo. */
    public void modifySmartCardDlg(final Container container) {

    	// Cursor en espera
    	container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    	final SmartCardPanel smartCardPanel = new SmartCardPanel();
    	smartCardPanel.getAccessibleContext().setAccessibleDescription(
    			SimpleAfirmaMessages.getString("SmartCardDialog.9")); //$NON-NLS-1$
    	final String oldCardName = ((RegisteredKeystore) this.smartCards.getSelectedItem()).getName();
    	final String oldLibName = ((RegisteredKeystore) this.smartCards.getSelectedItem()).getLib();
    	smartCardPanel.getCardNameTxt().setText(oldCardName);
    	smartCardPanel.getControllerPathTxt().setText(oldLibName);
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
        			|| smartCardPanel.getControllerPathTxt().getText().isEmpty() ) {
    			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.27"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    					AOUIFactory.ERROR_MESSAGE,
    					new Exception(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.27")) //$NON-NLS-1$
    					);

    			modifySmartCardDlg(container);
        	} else if (checkDuplicatedName(smartCardPanel.getCardNameTxt().getText())) {

    			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.34"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    					AOUIFactory.ERROR_MESSAGE,
    					new Exception(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.34")) //$NON-NLS-1$
    					);

    			modifySmartCardDlg(container);

    		} else if (checkDuplicatedLib(smartCardPanel.getControllerPathTxt().getText())) {

    			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.35"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    					AOUIFactory.ERROR_MESSAGE,
    					new Exception(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.35")) //$NON-NLS-1$
    					);

    			modifySmartCardDlg(container);

    		} else {
    			final boolean regDeleted = KeyStorePreferencesManager.deleteSmartCardRec(oldCardName);

    			if (regDeleted) {
    				for (int i = 0 ; i < this.smartCards.getItemCount() ; i++) {
    					System.out.println(this.smartCards.getItemAt(i).getName());
    					if (this.smartCards.getItemAt(i).getName().equals(oldCardName)) {
    						this.smartCards.removeItemAt(i);
    					}
    				}
    				for (int i = 0 ; i < this.keystores.getItemCount() ; i++) {
    					if (this.keystores.getItemAt(i).getName().equals(oldCardName)) {
    						this.keystores.removeItemAt(i);
    					}
    				}
    			}

    			this.keystores.repaint();
    			this.smartCards.repaint();

    			final boolean regAdded = KeyStorePreferencesManager.addSmartCardToUserRec(smartCardPanel.getCardNameTxt().getText() , smartCardPanel.getControllerPathTxt().getText());

    			if (regAdded) {
    				final RegisteredKeystore ks = new RegisteredKeystore();
    				ks.setName(smartCardPanel.getCardNameTxt().getText());
    				ks.setLib(smartCardPanel.getControllerPathTxt().getText());
    				ks.setProviderName(AOKeyStore.PKCS11.getProviderName());
    				this.smartCards.addItem(ks);
    				this.smartCards.setSelectedItem(ks);
    				this.smartCards.repaint();
    				this.keystores.addItem(ks);
    				this.keystores.repaint();
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


			LOGGER.info("Eliminamos el almacen de la tarjeta inteligente " //$NON-NLS-1$
					+ LoggerUtil.getTrimStr(name));

			final boolean regDeleted = KeyStorePreferencesManager.deleteSmartCardRec(name);

			if (regDeleted) {
				for (int i = 0 ; i < this.keystores.getItemCount() ; i++) {
					if (this.keystores.getItemAt(i).getName().equals(name)) {
						this.keystores.removeItemAt(i);
					}
				}
				for (int i = 0 ; i < this.smartCards.getItemCount() ; i++) {
					if (this.smartCards.getItemAt(i).getName().equals(name)) {
						this.smartCards.removeItemAt(i);
					}
				}
			}

			this.keystores.repaint();
			this.smartCards.repaint();

			if (this.smartCards.getItemCount() == 1 || this.smartCards.getSelectedIndex() == 0) {
				this.connectButton.setEnabled(false);
				this.modifyCardButton.setEnabled(false);
				this.deleteCardButton.setEnabled(false);
			}
    	}
    }

    /**
     * Comprueba si ya hay una tarjeta inteligente registrada con el mismo nombre.
     * @param newName Nombre de la nueva tarjeta a registrar.
     * @return True en caso de que ya se encuentre el nombre registrado, false en caso contrario.
     */
    private boolean checkDuplicatedName(final String newName) {
    	for (int i = 0 ; i < this.smartCards.getItemCount() ; i++) {
    		final RegisteredKeystore rks = this.smartCards.getItemAt(i);
    		if (newName.equals(rks.getName())) {
    			return true;
    		}
    	}
    	return false;
    }

    /**
     * Comprueba si ya hay una tarjeta inteligente con el mismo controlador registrada con el mismo nombre.
     * @param newName Nombre del nuevo controlador.
     * @return True en caso de que ya se encuentre el controlador registrado, false en caso contrario.
     */
    private boolean checkDuplicatedLib(final String newLib) {
    	for (int i = 0 ; i < this.smartCards.getItemCount() ; i++) {
    		final RegisteredKeystore rks = this.smartCards.getItemAt(i);
    		if (newLib.equals(rks.getLib())) {
    			return true;
    		}
    	}
    	return false;
    }

	/** Guarda las preferencias. */
	void savePreferences() {

		if (this.localKeystoreSelectedPath != null) {
			PreferencesManager.put(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH, this.localKeystoreSelectedPath);
			KeyStorePreferencesManager.setLastSelectedKeystoreLib(this.localKeystoreSelectedPath);
		}

		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_USE_DEFAULT_STORE_IN_BROWSER_CALLS, this.callsFromNavigator.isSelected());

		final RegisteredKeystore rks = (RegisteredKeystore) this.keystores.getSelectedItem();
		AOKeyStore aoks = SimpleKeyStoreManager.getKeyStore(rks.getName());

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
			String ksName = aoks.getName();
			if (this.keystores.getSelectedIndex() == 0) {
				ksName = PreferencesManager.VALUE_KEYSTORE_DEFAULT;
			}
			PreferencesManager.put(
					PREFERENCE_KEYSTORE_DEFAULT_STORE,
					ksName
			);
			KeyStorePreferencesManager.setLastSelectedKeystore(aoks.getName());
		}

		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SHOWEXPIREDCERTS, this.showExpiredCerts.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN, this.hideDniStartScreen.isSelected());
		PreferencesManager.putBoolean(PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS, this.onlySignature.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS, this.onlyAlias.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SKIP_AUTH_CERT_DNIE, this.skipAuthCertDnie.isSelected());
	}

	void loadPreferences() {

		final String ks = PreferencesManager.get(PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE);

		if (DEFAULT_VALUE.equals(ks)) {
			this.keystores.setSelectedIndex(0);
		} else {

			final AOKeyStore aoks = SimpleKeyStoreManager.getKeyStore(ks);
			RegisteredKeystore rks = null;

			if (ks != null) {
				if (aoks == null) {
					final Map<String, String> userRegResult = KeyStorePreferencesManager.getUserSmartCardsRegistered();
					if (!userRegResult.isEmpty()) {
						for (final String smartCardName : userRegResult.keySet()) {
							if (ks.equals(smartCardName)) {
								rks = new RegisteredKeystore(AOKeyStore.PKCS11, false);
								rks.setName(smartCardName);
							}
						}
					}
					final Map<String, String> systemRegResult = KeyStorePreferencesManager.getSystemSmartCardsRegistered();
					if (!systemRegResult.isEmpty()) {
						for (final String smartCardName : systemRegResult.keySet()) {
							if (ks.equals(smartCardName)) {
								rks = new RegisteredKeystore(AOKeyStore.PKCS11, true);
								rks.setName(smartCardName);
							}
						}
					}
				} else {
					rks = new RegisteredKeystore(aoks, false);
				}
			}

			if (rks != null) {
				for (int i = 0; i < this.keystores.getItemCount() ; i++) {
					if (rks.getName().equals(this.keystores.getItemAt(i).getName())) {
						this.keystores.setSelectedIndex(i);
					}
				}
			}
		}

		this.keystores.repaint();
		this.callsFromNavigator.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_USE_DEFAULT_STORE_IN_BROWSER_CALLS));
		this.hideDniStartScreen.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN));
		this.showExpiredCerts.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SHOWEXPIREDCERTS));
		this.onlySignature.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS));
		this.onlyAlias.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS));
		this.skipAuthCertDnie.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SKIP_AUTH_CERT_DNIE));

        revalidate();
        repaint();

	}

	void restorePreferences() {

		// Eliminamos las tarjetas dadas de alta por el usuario
		final Map<String, String> smartCardsMap  = KeyStorePreferencesManager.getUserSmartCardsRegistered();
		for (final String smartCardName : smartCardsMap.keySet().toArray(new String[0])) {
			KeyStorePreferencesManager.deleteSmartCardRec(smartCardName);
		}

		// Actualizamos el listado de tarjetas
		this.smartCards.removeAllItems();
		loadSmartCardsComboBox(this.smartCards);
		this.smartCards.repaint();

		// Actualizamos el listado de tarjetas
		this.keystores.removeAllItems();
		loadKeyStoresComboBox(this.keystores, this.smartCards);
		this.keystores.repaint();

		// Eliminamos la configuracion de los almacenes
		try {
			KeyStorePreferencesManager.clearKeyStorePrefs();
		} catch (final BackingStoreException e) {
			LOGGER.log(Level.WARNING, "No se puede borrar la configuracion relativa a las tarjetas", e); //$NON-NLS-1$
		}

		// Eliminamos el resto de las opciones de configuracion
		PreferencesManager.remove(PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE);
		PreferencesManager.remove(PreferencesManager.PREFERENCE_USE_DEFAULT_STORE_IN_BROWSER_CALLS);
		PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN);
		PreferencesManager.remove(PreferencesManager.PREFERENCE_KEYSTORE_SHOWEXPIREDCERTS);
		PreferencesManager.remove(PreferencesManager.PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS);
		PreferencesManager.remove(PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS);
		PreferencesManager.remove(PreferencesManager.PREFERENCE_KEYSTORE_SKIP_AUTH_CERT_DNIE);

		try {
			PreferencesManager.flush();
		} catch (final BackingStoreException e) {
			LOGGER.log(Level.WARNING, "Error al restaurar la configuracion de almacenes", e); //$NON-NLS-1$
		}

		// Cargamos las preferencias, que seran las del sistema o las por defecto
		loadPreferences();
	}

    static List<? extends CertificateFilter> getCertFilters() {

    	final List<CertificateFilter> filters = new ArrayList<>();

    	if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS)) {
    		filters.add(new KeyUsageFilter(KeyUsageFilter.SIGN_CERT_USAGE));
    	}
    	if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SKIP_AUTH_CERT_DNIE)) {
    		filters.add(new SkipAuthDNIeFilter());
    	}
    	if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS)) {
    		filters.add(new PseudonymFilter(PseudonymFilter.VALUE_ONLY));
    	}
    	if (filters.size() > 1) {
    		return Arrays.asList(
				new MultipleCertificateFilter(filters.toArray(new CertificateFilter[0]))
			);
    	}
		if (filters.size() == 1) {
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
}