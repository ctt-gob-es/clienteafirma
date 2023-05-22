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
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.SimpleKeyStoreManager;
import es.gob.afirma.ui.core.jse.certificateselection.ShowCertificatesDialog;

/** Pesta&ntilde;a de configuraci&oacute;n de las preferencias de certificados.
 * @author Jos&eacute; Montero. */
final class PreferencesPanelCertificates extends JScrollPane {

	private static final long serialVersionUID = 3491050093362323228L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	JComboBox<AOKeyStore> keystores;

	private final JComboBox<Object> intelligentCards = new JComboBox<>();

	private final JPanel panelKeystores = new JPanel();

	private final JButton showContentButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelCertificates.2")); //$NON-NLS-1$

	private final JButton connectButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelCertificates.10")); //$NON-NLS-1$

	private final JButton addCardButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanelCertificates.11")); //$NON-NLS-1$

	private final JCheckBox callsFromNavigator = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelCertificates.3")); //$NON-NLS-1$

	private final JCheckBox hideDniStartScreen = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.81")); //$NON-NLS-1$

	private final JCheckBox showExpiredCerts = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelCertificates.6")); //$NON-NLS-1$

	private final JCheckBox onlySignature = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelCertificates.7")); //$NON-NLS-1$

	private final JCheckBox onlyAlias = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelCertificates.8")); //$NON-NLS-1$

	static final String[] EXTS = new String[] { "pfx", "p12" }; //$NON-NLS-1$ //$NON-NLS-2$

	private static final String EXTS_DESC = " (*.p12, *.pfx)"; //$NON-NLS-1$

	String localKeystoreSelectedPath;

	/**
	 * Atributo que permite gestionar el bloqueo de preferencias.
	 */
	private boolean blocked = true;

	PreferencesPanelCertificates(final KeyListener keyListener,
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
		final List<AOKeyStore> stores = new ArrayList<>();

		if (Platform.OS.WINDOWS.equals(os)) {
			stores.add(AOKeyStore.WINDOWS);
			if (SimpleKeyStoreManager.isFirefoxAvailable()) {
				stores.add(AOKeyStore.MOZ_UNI);
			}
		}
		else if (Platform.OS.MACOSX.equals(os)) {
			stores.add(AOKeyStore.APPLE);
			if (SimpleKeyStoreManager.isFirefoxAvailable()) {
				stores.add(AOKeyStore.MOZ_UNI);
			}
		}
		else {
			stores.add(AOKeyStore.SHARED_NSS);
		}

		stores.add(AOKeyStore.PKCS12_NO_PASS);

		stores.add(AOKeyStore.DNIEJAVA);

		this.keystores = new JComboBox<>(stores.toArray(new AOKeyStore[0]));

		this.keystores.addActionListener (new ActionListener () {
		    @Override
			public void actionPerformed(final ActionEvent e) {
		       if (PreferencesPanelCertificates.this.keystores.getSelectedItem().equals(AOKeyStore.PKCS12_NO_PASS) && e.getModifiers() != 0) {
			   		final File[] ksFile;
			   		try {
						ksFile = AOUIFactory.getLoadFiles(
							SimpleAfirmaMessages.getString("PreferencesPanelCertificates.12"), //$NON-NLS-1$
							null,
							null,
							EXTS,
							SimpleAfirmaMessages.getString("PreferencesPanelCertificates.13") + EXTS_DESC, //$NON-NLS-1$
							false,
							false,
							null,
							this
						);
						PreferencesPanelCertificates.this.localKeystoreSelectedPath = ksFile[0].getAbsolutePath();
			   		} catch(final AOCancelledOperationException acoe) {
			   			PreferencesPanelCertificates.this.keystores.setSelectedIndex(0);
						return;
			   		}
		       } else if (PreferencesPanelCertificates.this.keystores.getSelectedItem().equals(AOKeyStore.DNIEJAVA) && e.getModifiers() != 0) {
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
						PreferencesPanelCertificates.this.keystores.setSelectedIndex(0);
						throw aoce;
					}
			   		catch (final Exception exc) {
			   			PreferencesPanelCertificates.this.keystores.setSelectedIndex(0);
			   			LOGGER.log(Level.WARNING,"No se ha podido cargar el DNIe: " + exc, exc); //$NON-NLS-1$
			   			AOUIFactory.showErrorMessage(SimpleAfirmaMessages.getString("PreferencesPanelCertificates.14"), //$NON-NLS-1$
		    					SimpleAfirmaMessages.getString("SimpleAfirma.7"), AOUIFactory.ERROR_MESSAGE, exc); //$NON-NLS-1$
					}
		       }
		    }
		});

        loadPreferences();

        this.panelKeystores.setBorder(
    		BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanelCertificates.1") //$NON-NLS-1$
			)
		);

        this.panelKeystores.setLayout(new GridBagLayout());

        final GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.fill = GridBagConstraints.BOTH;
        gbc1.gridy = 0;
        gbc1.weightx = 1.0;
        gbc1.gridx = 0;

		this.keystores.addItemListener(modificationListener);
		this.keystores.addKeyListener(keyListener);

        this.panelKeystores.add(this.keystores, gbc1);

        gbc1.gridx++;

        this.showContentButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
				final AOKeyStoreManager ksm;
				final AOKeyStore ks = AOKeyStore.getKeyStore(((AOKeyStore) PreferencesPanelCertificates.this.keystores.getSelectedItem()).getName());
				try {
					String lib = null;
					if (ks.equals(AOKeyStore.PKCS12)) {
						lib = PreferencesPanelCertificates.this.localKeystoreSelectedPath;
						if (lib == null) {
							lib = PreferencesManager.get(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH);
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

					final ShowCertificatesDialog csd = new ShowCertificatesDialog(
						PreferencesPanelCertificates.this,
						new AOKeyStoreDialog(
							ksm,
							this,
							true,
							true,
							false,
							libName
						),
						SimpleAfirmaMessages.getString(
							"PreferencesPanelKeyStores.10", //$NON-NLS-1$
							ks.toString()
						),
						SimpleAfirmaMessages.getString(
							"PreferencesPanelKeyStores.15", //$NON-NLS-1$
							ks.toString()
						),
						true,
						true
					);
					csd.showDialog();
				} 	catch (final Exception kse) {
						AOUIFactory.showErrorMessage(
						SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.11"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.10", ks.toString()), //$NON-NLS-1$
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
				SimpleAfirmaMessages.getString("") //$NON-NLS-1$
		);
		this.callsFromNavigator.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("") //$NON-NLS-1$
		);
		this.callsFromNavigator.setMnemonic('N');
		this.callsFromNavigator.addItemListener(modificationListener);
		this.callsFromNavigator.addKeyListener(keyListener);

        this.panelKeystores.add(this.callsFromNavigator, gbc1);

        mainPanel.add(this.panelKeystores, gbc);

        final JPanel certsFiltersPanel = new JPanel();
        certsFiltersPanel.setBorder(
    		BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanelCertificates.4") //$NON-NLS-1$
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
				SimpleAfirmaMessages.getString("") //$NON-NLS-1$
		);
		this.showExpiredCerts.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("")); //$NON-NLS-1$
		this.showExpiredCerts.setMnemonic('M');
		this.showExpiredCerts.addItemListener(modificationListener);
		this.showExpiredCerts.addKeyListener(keyListener);

        certsFiltersPanel.add(this.showExpiredCerts, c);
        c.gridy++;

		this.onlySignature.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("") //$NON-NLS-1$
		);
		this.onlySignature.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("")); //$NON-NLS-1$
		this.onlySignature.setMnemonic('E');
		this.onlySignature.addItemListener(modificationListener);
		this.onlySignature.addKeyListener(keyListener);

        certsFiltersPanel.add(this.onlySignature, c);
        c.gridy++;

		this.onlyAlias.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("") //$NON-NLS-1$
		);
		this.onlyAlias.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("")); //$NON-NLS-1$
		this.onlyAlias.setMnemonic('S');
		this.onlyAlias.addItemListener(modificationListener);
		this.onlyAlias.addKeyListener(keyListener);
        certsFiltersPanel.add(this.onlyAlias, c);

        gbc.gridy++;
        mainPanel.add(certsFiltersPanel, gbc);

        final JPanel inteligentCardsPanel = new JPanel();
        inteligentCardsPanel.setBorder(
    		BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanelCertificates.9") //$NON-NLS-1$
			)
		);
        inteligentCardsPanel.setLayout(new GridBagLayout());

        final GridBagConstraints icpConstraints = new GridBagConstraints();
        icpConstraints.fill = GridBagConstraints.BOTH;
        icpConstraints.gridy = 0;
        icpConstraints.weightx = 1.0;
        icpConstraints.gridx = 0;

        inteligentCardsPanel.add(this.intelligentCards, icpConstraints);

        icpConstraints.gridx++;

        inteligentCardsPanel.add(this.connectButton, icpConstraints);

        icpConstraints.gridx++;

        inteligentCardsPanel.add(this.addCardButton, icpConstraints);

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
			if (AOUIFactory.showConfirmDialog(getParent(), SimpleAfirmaMessages.getString("PreferencesPanelCertificates.15"), //$NON-NLS-1$
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

	/** Guarda las preferencias. */
	void savePreferences() {

		if (this.localKeystoreSelectedPath != null) {
			PreferencesManager.put(PreferencesManager.PREFERENCE_LOCAL_KEYSTORE_PATH, this.localKeystoreSelectedPath);
		}
		PreferencesManager.putBoolean(PreferencesManager.PREFERENCE_USE_DEFAULT_STORE_IN_BROWSER_CALLS, this.callsFromNavigator.isSelected());
		AOKeyStore aoks = getDefaultStore();
		if (aoks.equals(AOKeyStore.PKCS12_NO_PASS)) {
			aoks = AOKeyStore.PKCS12;
		}
		PreferencesManager.put(
				PREFERENCE_KEYSTORE_DEFAULT_STORE,
				aoks.name()
		);
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
		this.keystores.setSelectedItem(AOKeyStore.getKeyStore(ks));
		this.callsFromNavigator.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_USE_DEFAULT_STORE_IN_BROWSER_CALLS));
		this.showExpiredCerts.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SHOWEXPIREDCERTS));
		this.hideDniStartScreen.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN));
		this.onlySignature.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS));
		this.onlyAlias.setSelected(PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS));

        this.panelKeystores.removeAll();

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;
        revalidate();
        repaint();

	}

	void loadDefaultPreferences() {
		this.keystores.setSelectedItem(Integer.valueOf(0));
		this.callsFromNavigator.setSelected(false);
		this.showExpiredCerts.setSelected(false);
		this.hideDniStartScreen.setSelected(false);
		this.onlySignature.setSelected(false);
		this.onlyAlias.setSelected(false);

        revalidate();
        repaint();

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

	AOKeyStore getDefaultStore() {
		return this.keystores.getItemAt(this.keystores.getSelectedIndex());
	}
}
