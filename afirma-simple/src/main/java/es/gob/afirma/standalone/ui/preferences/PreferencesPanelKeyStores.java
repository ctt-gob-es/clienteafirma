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
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.SimpleKeyStoreManager;
import es.gob.afirma.ui.core.jse.certificateselection.CertificateSelectionDialog;

/** Pesta&ntilde;a de configuraci&oacute;n de las preferencias de certificados.
 * @author Jos&eacute; Montero. */
final class PreferencesPanelKeystores extends JScrollPane {

	private static final long serialVersionUID = 3491050093362323228L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static JComboBox<RegisteredKeystore> keystores;

	private static JComboBox<RegisteredKeystore> smartCards;

	private final JPanel panelKeystores = new JPanel();

	private final JButton showContentButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.2")); //$NON-NLS-1$

	private final JButton connectButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.10")); //$NON-NLS-1$

	private final JButton addCardButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.11")); //$NON-NLS-1$

	private final JButton modifyCardButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.23")); //$NON-NLS-1$

	private final JButton deleteCardButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.24")); //$NON-NLS-1$

	private final JCheckBox callsFromNavigator = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.3")); //$NON-NLS-1$

	private final JCheckBox hideDniStartScreen = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.81")); //$NON-NLS-1$

	private final JCheckBox showExpiredCerts = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.6")); //$NON-NLS-1$

	private final JCheckBox onlySignature = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.7")); //$NON-NLS-1$

	private final JCheckBox onlyAlias = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.8")); //$NON-NLS-1$

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

		stores.add(new RegisteredKeystore(AOKeyStore.PKCS12_NO_PASS));

		stores.add(new RegisteredKeystore(AOKeyStore.DNIEJAVA));

		PreferencesPanelKeystores.setKeystores(new JComboBox<RegisteredKeystore>(stores.toArray(new RegisteredKeystore[0])));

		PreferencesPanelKeystores.getKeystores().addActionListener (new ActionListener () {
		    @Override
			public void actionPerformed(final ActionEvent e) {
		       final RegisteredKeystore ks = (RegisteredKeystore) PreferencesPanelKeystores.getKeystores().getSelectedItem();
		       if (ks.getName().equals(AOKeyStore.PKCS12_NO_PASS.getName()) && e.getModifiers() != 0) {
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
			   		} catch(final AOCancelledOperationException acoe) {
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

		PreferencesPanelKeystores.getKeystores().addItemListener(modificationListener);
		PreferencesPanelKeystores.getKeystores().addKeyListener(keyListener);

        this.panelKeystores.add(PreferencesPanelKeystores.getKeystores(), gbc1);

        gbc1.gridx++;

        this.showContentButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
				final AOKeyStoreManager ksm;
				AOKeyStore ks = AOKeyStore.getKeyStore(((RegisteredKeystore) PreferencesPanelKeystores.getKeystores().getSelectedItem()).getName());
				try {
					String lib = null;
					if (ks != null && (AOKeyStore.PKCS12.equals(ks) || AOKeyStore.PKCS11.getProviderName().equals(ks.getProviderName()))) {
						lib = PreferencesPanelKeystores.this.localKeystoreSelectedPath;
						if (lib == null) {
							lib = PreferencesManager.get(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH);
						}
					} else if (ks == null) {
				        final List<String> regResult = getSmartCardsRegistered();
						for (int k = 0 ; k < regResult.size() ; k = k + 9) {
							final String smartCardName = regResult.get(k + 5);
							if (((RegisteredKeystore) PreferencesPanelKeystores.getKeystores().getSelectedItem()).getName().equals(smartCardName)){
								ks = AOKeyStore.PKCS11;
								ks.setName(smartCardName);
								lib = regResult.get(k + 8);
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
							true,
							false,
							libName
						),
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
        icpConstraints.fill = GridBagConstraints.BOTH;
        icpConstraints.gridy = 0;
        icpConstraints.weightx = 1.0;
        icpConstraints.gridx = 0;

        final List<String> regResult = getSmartCardsRegistered();

        smartCards = new JComboBox<RegisteredKeystore>();

        if (regResult!= null && !regResult.get(0).isEmpty()) {
			for (int k = 0 ; k < regResult.size() ; k = k + 9) {
				final String smartCardName = regResult.get(k + 5);
				final RegisteredKeystore newPKCS11 = new RegisteredKeystore(AOKeyStore.PKCS11);
				newPKCS11.setName(smartCardName);
				final String lib = regResult.get(k + 8);
				newPKCS11.setLib(lib);
				smartCards.addItem(newPKCS11);
				keystores.addItem(newPKCS11);
			}
        }

		if (smartCards.getItemCount() == 0) {
			this.modifyCardButton.setEnabled(false);
			this.deleteCardButton.setEnabled(false);
		}

		smartCards.repaint();
		keystores.repaint();

		loadPreferences();

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
    			AOUIFactory.showMessageDialog(PreferencesPanelKeystores.class, SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.27"), //$NON-NLS-1$
    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE); //$NON-NLS-1$
    		} else {

    			final boolean regAdded = addSmartCardToReg(smartCardPanel);

    			if (regAdded) {
    				final RegisteredKeystore ks = new RegisteredKeystore();
    				ks.setName(smartCardPanel.getCardNameTxt().getText());
    				ks.setLib(smartCardPanel.getControllerNameTxt().getText());
    				ks.setProviderName(AOKeyStore.PKCS11.getProviderName());
    				smartCards.addItem(ks);
    				smartCards.repaint();
    				keystores.addItem(ks);
    				keystores.repaint();
    			}

    			if (smartCards.getItemCount() > 0) {
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
        			AOUIFactory.showMessageDialog(PreferencesPanelKeystores.class, SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.27"), //$NON-NLS-1$
        					SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE); //$NON-NLS-1$
        	} else {
    			final boolean regDeleted = deleteSmartCardReg(oldCardName);

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

    			final boolean regAdded = addSmartCardToReg(smartCardPanel);

    			if (regAdded) {
    				final RegisteredKeystore ks = new RegisteredKeystore();
    				ks.setName(smartCardPanel.getCardNameTxt().getText());
    				ks.setLib(smartCardPanel.getControllerNameTxt().getText());
    				ks.setProviderName(AOKeyStore.PKCS11.getProviderName());
    				smartCards.addItem(ks);
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
				SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.24"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION,
				JOptionPane.DEFAULT_OPTION
		) == JOptionPane.OK_OPTION) {

			final boolean regDeleted = deleteSmartCardReg(name);

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

			if (smartCards.getItemCount() == 0) {
				this.modifyCardButton.setEnabled(false);
				this.deleteCardButton.setEnabled(false);
			}

			keystores.repaint();
			smartCards.repaint();
    	}
    }

	/** Guarda las preferencias. */
	void savePreferences() {

		if (this.localKeystoreSelectedPath != null) {
			PreferencesManager.put(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH, this.localKeystoreSelectedPath);
		}

		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_USE_DEFAULT_STORE_IN_BROWSER_CALLS, this.callsFromNavigator.isSelected());
		final RegisteredKeystore rks = getDefaultStore();
		AOKeyStore aoks = AOKeyStore.getKeyStore(rks.getName());

		if (aoks == null && rks.getProviderName().equals(AOKeyStore.PKCS11.getProviderName())) {
			aoks = AOKeyStore.PKCS11;
			aoks.setName(rks.getName());
		}

		if (aoks != null && aoks.getProviderName().equals(AOKeyStore.PKCS11.getProviderName())) {
			PreferencesManager.put(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH, rks.getLib());
		}

		if (rks.getName().equals(AOKeyStore.PKCS12_NO_PASS.getName())) {
			aoks = AOKeyStore.PKCS12;
		}

		if (aoks != null) {
			PreferencesManager.put(
					PREFERENCE_KEYSTORE_DEFAULT_STORE,
					aoks.getName()
			);
		}

		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SHOWEXPIREDCERTS, this.showExpiredCerts.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN, this.hideDniStartScreen.isSelected());
		PreferencesManager.putBoolean(PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS, this.onlySignature.isSelected());
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS, this.onlyAlias.isSelected());

	}

	void loadPreferences() {

		String ks = PreferencesManager.get(PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE);
		if (ks.equals(AOKeyStore.PKCS12.name())) {
			ks = AOKeyStore.PKCS12_NO_PASS.name();
		}
		final AOKeyStore aoks = AOKeyStore.getKeyStore(ks);
		RegisteredKeystore rks = null;

		if (aoks == null) {
			final List<String> regResult = getSmartCardsRegistered();
			if (regResult != null && !regResult.get(0).isEmpty()) {
				for (int k = 0 ; k < regResult.size() ; k = k + 9) {
					final String smartCardName = regResult.get(k + 5);
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

        revalidate();
        repaint();

	}

	void loadDefaultPreferences() {
		PreferencesPanelKeystores.getKeystores().setSelectedItem(Integer.valueOf(0));
		this.callsFromNavigator.setSelected(false);
		this.showExpiredCerts.setSelected(false);
		this.hideDniStartScreen.setSelected(false);
		this.onlySignature.setSelected(false);
		this.onlyAlias.setSelected(false);

        revalidate();
        repaint();

	}

	/**
	 * Agrega una tarjeta inteligente al registro del sistema
	 * @param smartCardPanel panel con la informaci&oacute;n de la tarjeta a agregar
	 * @return devuelve true si se ha agregado correctamente
	 */
	private static boolean addSmartCardToReg(final SmartCardPanel smartCardPanel) {

		boolean regAdded = false;

		try {

			boolean noExistReg = true;

			int cont = 1;

			while (noExistReg) {

				final Process p = new ProcessBuilder(
						"reg", //$NON-NLS-1$
						"QUERY", //$NON-NLS-1$
						"HKCU\\Software\\JavaSoft\\Prefs\\es\\gob\\afirma\\standalone\\keystores\\" + cont, //$NON-NLS-1$
						"/s"  //$NON-NLS-1$
					).start();

				final String regResult;

				try (InputStream inputStream = p.getInputStream()) {
					regResult = new String(AOUtil.getDataFromInputStream(inputStream)).trim();
				}

				if (regResult.isEmpty()) {

					new ProcessBuilder(
						"reg", //$NON-NLS-1$
						"ADD", //$NON-NLS-1$
						"HKCU\\Software\\JavaSoft\\Prefs\\es\\gob\\afirma\\standalone\\keystores\\" + cont //$NON-NLS-1$
					).start();

					new ProcessBuilder(
						"reg", //$NON-NLS-1$
						"ADD", //$NON-NLS-1$
						"HKCU\\Software\\JavaSoft\\Prefs\\es\\gob\\afirma\\standalone\\keystores\\" + cont, //$NON-NLS-1$
						"/v",  //$NON-NLS-1$
						"name", "/t", "REG_SZ", "/d", smartCardPanel.getCardNameTxt().getText() //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					).start();

					new ProcessBuilder(
						"reg", //$NON-NLS-1$
						"ADD", //$NON-NLS-1$
						"HKCU\\Software\\JavaSoft\\Prefs\\es\\gob\\afirma\\standalone\\keystores\\" + cont, //$NON-NLS-1$
						"/v",  //$NON-NLS-1$
						"lib", "/t", "REG_SZ", "/d", smartCardPanel.getControllerNameTxt().getText() //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					).start();

					noExistReg = false;

					regAdded = true;

				}

				cont++;
			}

		} catch (final IOException e) {
   			LOGGER.log(Level.WARNING,"Error al escribir en el registro la tarjeta inteligente: " + e, e); //$NON-NLS-1$
   			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.21"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e); //$NON-NLS-1$
		}

		return regAdded;
	}

	/**
	 * Elimina del registro el almac&eacute;n indicado
	 * @param name Nombre del almac&eacute;n de la tarjeta a eliminar
	 * @return devuelve true en caso de que se haya eliminado correctamente
	 */
	private static boolean deleteSmartCardReg(final String name) {

		boolean deleteReg = false;

		try {

			int cont = 1;

			while (!deleteReg) {

				final Process p = new ProcessBuilder(
						"reg", //$NON-NLS-1$
						"QUERY", //$NON-NLS-1$
						"HKCU\\Software\\JavaSoft\\Prefs\\es\\gob\\afirma\\standalone\\keystores\\" + cont, //$NON-NLS-1$
						"/v",  //$NON-NLS-1$
						"name"  //$NON-NLS-1$
					).start();

				final String regResult;

				try (InputStream inputStream = p.getInputStream()) {
					regResult = new String(AOUtil.getDataFromInputStream(inputStream)).trim();
				}

				if (!regResult.isEmpty()) {
					final String [] regs = regResult.split("    "); //$NON-NLS-1$

					if (regs[3].equals(name)) {
						new ProcessBuilder(
								"reg", //$NON-NLS-1$
								"DELETE", //$NON-NLS-1$
								"HKCU\\Software\\JavaSoft\\Prefs\\es\\gob\\afirma\\standalone\\keystores\\" + cont, //$NON-NLS-1$
								"/f"  //$NON-NLS-1$
						).start();
						deleteReg = true;
					}
				}

				cont++;
			}

		} catch (final IOException e) {
   			LOGGER.log(Level.WARNING,"Error al eliminar el registro la tarjeta inteligente: " + e, e); //$NON-NLS-1$
   			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.26"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e); //$NON-NLS-1$
		}

		return deleteReg;
	}

	/**
	 * Se obtienen todos los registros de tarjetas inteligentes que hay en el sistema.
	 * @return lista con los registros
	 */
	static List<String> getSmartCardsRegistered(){
		final List<String> finalRegResult = new ArrayList<String>();
		try {
			final Process p = new ProcessBuilder(
					"reg", //$NON-NLS-1$
					"QUERY", //$NON-NLS-1$
					"HKCU\\Software\\JavaSoft\\Prefs\\es\\gob\\afirma\\standalone\\keystores", //$NON-NLS-1$
					"/s"  //$NON-NLS-1$
				).start();

			final String regResult;

			try (InputStream inputStream = p.getInputStream()) {
				regResult = new String(AOUtil.getDataFromInputStream(inputStream)).trim();
			}

			final String [] regs = regResult.split("    "); //$NON-NLS-1$
			for (int i = 0 ; i < regs.length ; i++) {
				if (regs[i].contains("\r\n")) { //$NON-NLS-1$
					final String [] splReg = regs[i].split("\r\n"); //$NON-NLS-1$
					for (int j = 0 ; j < splReg.length ; j++) {
						if (!splReg[j].isEmpty()) {
							finalRegResult.add(splReg[j]);
						}
					}
				} else {
					finalRegResult.add(regs[i]);
				}
			}

		} catch (final IOException e) {
   			LOGGER.log(Level.WARNING,"Error al obtener los registros de tarjetas inteligentes: " + e, e); //$NON-NLS-1$
   			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.22"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, e); //$NON-NLS-1$
		}
		return finalRegResult;
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
