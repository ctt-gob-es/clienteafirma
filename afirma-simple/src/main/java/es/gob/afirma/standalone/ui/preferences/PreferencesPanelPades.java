/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_FORMAT;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_POLICY_HASH;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_POLICY_HASH_ALGORITHM;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_SIGNER_CONTACT;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_SIGN_PRODUCTION_CITY;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_SIGN_REASON;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_VISIBLE;

import java.awt.Color;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import org.ietf.jgss.GSSException;
import org.ietf.jgss.Oid;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.preferences.PolicyPanel.PolicyItem;
import es.gob.afirma.standalone.ui.preferences.PreferencesPanel.ValueTextPair;

final class PreferencesPanelPades extends JScrollPane {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long serialVersionUID = 4299378019540627483L;

	private PolicyPanel padesPolicyDlg;

	/**
	 * Atributo que representa la etiqueta de la pol&iacute;tica seleccionada en
	 * el di&aacute;logo.
	 */
	private JLabel currentPolicyValue;

	private static final AdESPolicy POLICY_CADES_PADES_AGE_1_9 = new AdESPolicy(
		"2.16.724.1.3.1.1.2.1.9", //$NON-NLS-1$
		"G7roucf600+f03r/o0bAOQ6WAs0=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"https://sede.060.gob.es/politica_de_firma_anexo_1.pdf" //$NON-NLS-1$
	);

	/**
	 * Atributo para gestionar el bloqueo de propiedades.
	 */
	private boolean blocked = true;

	private final JComboBox<Object> padesBasicFormat = new JComboBox<>();
	JComboBox<Object> getBasicPadesFormat() {
		return this.padesBasicFormat;
	}

	private final JTextField padesSignReason = new JTextField();

	private final JTextField padesSignProductionCity = new JTextField();

	private final JTextField padesSignerContact = new JTextField();

	private final JCheckBox visiblePdfSignature = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.79")); //$NON-NLS-1$

	private static final String PADES_FORMAT_BASIC_TEXT = SimpleAfirmaMessages.getString("PreferencesPanel.71"); //$NON-NLS-1$
	private static final String PADES_FORMAT_BES_TEXT = SimpleAfirmaMessages.getString("PreferencesPanel.72"); //$NON-NLS-1$

	private static final String SIGN_FORMAT_PADES = "PAdES"; //$NON-NLS-1$

	PreferencesPanelPades(final KeyListener keyListener,
						  final ModificationListener modificationListener,
						  final boolean blocked) {

		setBlocked(blocked);
		createUI(keyListener, modificationListener);
	}

	void createUI(final KeyListener keyListener,
				  final ModificationListener modificationListener) {

		final JPanel mainPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;

		this.padesBasicFormat.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.70") //$NON-NLS-1$
		);
		final DefaultComboBoxModel<Object> padesFormatModel = new DefaultComboBoxModel<>(
			new Object[] {
				new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BES, PADES_FORMAT_BES_TEXT),
				new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BASIC, PADES_FORMAT_BASIC_TEXT)
			}
		);

		this.padesBasicFormat.setModel(padesFormatModel);
		this.padesBasicFormat.addItemListener(modificationListener);
		this.padesBasicFormat.addKeyListener(keyListener);
		this.padesBasicFormat.setEnabled(!isBlocked());

		this.visiblePdfSignature.setMnemonic('i');
		this.visiblePdfSignature.addItemListener(modificationListener);
		this.visiblePdfSignature.addKeyListener(keyListener);

		// Una vez creados todos los componentes, cargamos la configuracion
		loadPreferences();

        loadPadesPolicy();

        // Si hay establecida una politica de firma el formato de firma estara bloqueado a PAdES-BES
        if(this.padesPolicyDlg.getSelectedPolicy() != null) {
	        this.padesBasicFormat.removeAllItems();
			this.padesBasicFormat.addItem(new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BES, PADES_FORMAT_BES_TEXT));
			this.padesBasicFormat.addItem(new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BASIC, PADES_FORMAT_BASIC_TEXT));
			this.padesBasicFormat.setSelectedIndex(0);
			this.padesBasicFormat.setEnabled(false);
		}

    	this.padesPolicyDlg.setModificationListener(modificationListener);
    	this.padesPolicyDlg.setKeyListener(keyListener);

        ///////////// Panel Policy ////////////////

        final JPanel policyConfigPanel = createPolicyPanel();
		policyConfigPanel.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.153") //$NON-NLS-1$
			)
		);
        ///////////// Fin Panel Policy ////////////////

        gbc.gridy++;
        mainPanel.add(policyConfigPanel, gbc);

        ///////////// Inicio Panel Opciones de firma ////////////////
	    final JPanel signatureOptionsPanel = createSignatureOptionsPanel();
	    signatureOptionsPanel.setBorder(
	    	BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.69") //$NON-NLS-1$
			)
		);
	    ///////////// Fin Panel Opciones de firma ////////////////

	    gbc.gridy++;
		mainPanel.add(signatureOptionsPanel, gbc);

		///////////// Inicio Panel Metadatos ////////////////
	    final JPanel metadataPanel = new JPanel();
	    metadataPanel.setBorder(
    		BorderFactory.createTitledBorder(
    			SimpleAfirmaMessages.getString("PreferencesPanel.19") //$NON-NLS-1$
			)
		);
        metadataPanel.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;

	    final JLabel padesSignReasonLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.20")); //$NON-NLS-1$
	    padesSignReasonLabel.setLabelFor(this.padesSignReason);
	    metadataPanel.add(padesSignReasonLabel, c);

	    c.gridy++;

	    this.padesSignReason.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.63")); //$NON-NLS-1$
	    this.padesSignReason.addKeyListener(modificationListener);
	    this.padesSignReason.addKeyListener(keyListener);
	    metadataPanel.add(this.padesSignReason, c);

	    c.gridy++;

	    final JLabel padesSignProductionCityLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.21")); //$NON-NLS-1$
	    padesSignProductionCityLabel.setLabelFor(this.padesSignProductionCity);
	    metadataPanel.add(padesSignProductionCityLabel, c);

	    c.gridy++;

	    this.padesSignProductionCity.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.64")); //$NON-NLS-1$
	    this.padesSignProductionCity.addKeyListener(modificationListener);
	    this.padesSignProductionCity.addKeyListener(keyListener);
	    metadataPanel.add(this.padesSignProductionCity, c);

	    c.gridy++;

	    final JLabel padesSignerContactLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.22")); //$NON-NLS-1$
	    padesSignerContactLabel.setLabelFor(this.padesSignerContact);
	    metadataPanel.add(padesSignerContactLabel, c);

	    c.gridy++;

	    this.padesSignerContact.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.65")); //$NON-NLS-1$
	    this.padesSignerContact.addKeyListener(modificationListener);
	    this.padesSignerContact.addKeyListener(keyListener);
	    metadataPanel.add(this.padesSignerContact, c);

	    c.gridy++;
	    c.weighty = 1.0;
	    metadataPanel.add(new JPanel(), c);

	    gbc.gridy++;
	    mainPanel.add(metadataPanel, gbc);

	    ///////////// Fin Panel Metadatos ////////////////

	    gbc.gridy++;
	    gbc.weighty = 1.0;
	    mainPanel.add(new JPanel(), gbc); // Panel de relleno

	    // Panel para el boton de restaurar la configuracion
	 	final JPanel panelGeneral = new JPanel(new FlowLayout(FlowLayout.TRAILING));

	 	final JButton restoreConfigButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.147")); //$NON-NLS-1$

	 	restoreConfigButton.setMnemonic('R');
	 	restoreConfigButton.addActionListener(ae -> {
			if (AOUIFactory.showConfirmDialog(getParent(), SimpleAfirmaMessages.getString("PreferencesPanel.155"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("PreferencesPanel.139"), //$NON-NLS-1$
					JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {

				loadDefaultPreferences();
			}
		});
	 	restoreConfigButton.getAccessibleContext().setAccessibleDescription(
	 			SimpleAfirmaMessages.getString("PreferencesPanel.136")); //$NON-NLS-1$

	 	gbc.gridy++;
	 	gbc.weighty = 0.0;
		panelGeneral.add(restoreConfigButton, gbc);

	   	gbc.gridy++;
		mainPanel.add(panelGeneral, gbc);

		setViewportView(mainPanel);
	}

	private JPanel createPolicyPanel() {

		final JLabel currentPolicyLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.171")); //$NON-NLS-1$
		this.currentPolicyValue = new JLabel(this.padesPolicyDlg.getSelectedPolicyName());
		this.currentPolicyValue.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));
		this.currentPolicyValue.setFocusable(true);
		this.currentPolicyValue.addFocusListener(new FocusListener() {
			@Override
			public void focusLost(FocusEvent evt) {
				((JComponent) evt.getSource()).setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));
			}
			@Override
			public void focusGained(FocusEvent evt) {
				((JComponent) evt.getSource()).setBorder(BorderFactory.createLineBorder(Color.black, 1));
			}
		});
		currentPolicyLabel.setLabelFor(this.currentPolicyValue);

		final JButton policyConfigButton = new JButton(
				SimpleAfirmaMessages.getString("PreferencesPanel.150") //$NON-NLS-1$
			);

		policyConfigButton.setMnemonic('P');
		policyConfigButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					changePadesPolicyDlg(getParent());
				}
			}
		);
		policyConfigButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.151") //$NON-NLS-1$
		);

		policyConfigButton.setEnabled(!isBlocked());

		final JPanel policyConfigPanel = new JPanel(new FlowLayout(FlowLayout.LEADING));
		final JPanel innerPanel = new JPanel(new GridBagLayout());

		final GridBagConstraints c = new GridBagConstraints();
		c.anchor = GridBagConstraints.LINE_START;
		c.insets = new Insets(0,  7,  4,  7);
		c.gridx = 0;
		innerPanel.add(currentPolicyLabel, c);
		c.gridx = 1;
		innerPanel.add(this.currentPolicyValue, c);
		c.gridx = 2;
		innerPanel.add(policyConfigButton, c);

		policyConfigPanel.add(innerPanel);

		return policyConfigPanel;
	}

	private JPanel createSignatureOptionsPanel() {

		// Creamos un panel interior con los componentes locales
		final JPanel innerPanel = new JPanel(new GridBagLayout());

		final JLabel fileFormatLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanel.115") //$NON-NLS-1$
		);
		fileFormatLabel.setLabelFor(this.padesBasicFormat);

		// Colocamos los elementos
		final GridBagConstraints c = new GridBagConstraints();
		c.anchor = GridBagConstraints.LINE_START;
		c.insets = new Insets(0, 7, 4, 7);
		c.gridx = 0;
		c.gridy = 0;
		innerPanel.add(fileFormatLabel, c);
		c.gridx = 1;
		innerPanel.add(this.padesBasicFormat, c);
		c.gridx = 0;
		c.gridy++;
		c.gridwidth = 2;
		innerPanel.add(this.visiblePdfSignature, c);

		final JPanel signatureOptionsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		signatureOptionsPanel.add(innerPanel);

		return signatureOptionsPanel;
	}

	void checkPreferences() throws AOException {

		loadPadesPolicy();

		final AdESPolicy p = this.padesPolicyDlg.getSelectedPolicy();
		if (p != null) {
			// No nos interesa el resultado, solo si construye sin excepciones
			try {
				new Oid(p.getPolicyIdentifier().replace("urn:oid:", "")); //$NON-NLS-1$ //$NON-NLS-2$
			}
			catch (final GSSException e) {
				throw new AOException("El identificador debe ser un OID", e); //$NON-NLS-1$
			}
		}
	}

	void savePreferences() {
		// Firma PDF visible
		PreferencesManager.put(PREFERENCE_PADES_VISIBLE, Boolean.toString(this.visiblePdfSignature.isSelected()));

		if ("".equals(this.padesSignerContact.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_PADES_SIGNER_CONTACT);
		}
		else {
			PreferencesManager.put(PREFERENCE_PADES_SIGNER_CONTACT, this.padesSignerContact.getText());
		}
		if ("".equals(this.padesSignProductionCity.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_PADES_SIGN_PRODUCTION_CITY);
		}
		else {
			PreferencesManager.put(PREFERENCE_PADES_SIGN_PRODUCTION_CITY, this.padesSignProductionCity.getText());
		}
		if ("".equals(this.padesSignReason.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_PADES_SIGN_REASON);
		}
		else {
			PreferencesManager.put(PREFERENCE_PADES_SIGN_REASON, this.padesSignReason.getText());
		}

		final ComboBoxModel<Object> m = this.padesBasicFormat.getModel();
		final Object o = m.getElementAt(this.padesBasicFormat.getSelectedIndex());
		PreferencesManager.put(PREFERENCE_PADES_FORMAT, ((ValueTextPair) o).getValue());

		final AdESPolicy padesPolicy = this.padesPolicyDlg.getSelectedPolicy();
		if (padesPolicy != null) {
			PreferencesManager.put(PREFERENCE_PADES_POLICY_IDENTIFIER, padesPolicy.getPolicyIdentifier());
			PreferencesManager.put(PREFERENCE_PADES_POLICY_HASH, padesPolicy.getPolicyIdentifierHash());
			PreferencesManager.put(PREFERENCE_PADES_POLICY_HASH_ALGORITHM, padesPolicy.getPolicyIdentifierHashAlgorithm());
			if (padesPolicy.getPolicyQualifier() != null) {
				PreferencesManager.put(PREFERENCE_PADES_POLICY_QUALIFIER, padesPolicy.getPolicyQualifier().toString());
			}
			else {
				PreferencesManager.remove(PREFERENCE_PADES_POLICY_QUALIFIER);
			}
		}
		else {
			PreferencesManager.remove(PREFERENCE_PADES_POLICY_IDENTIFIER);
			PreferencesManager.remove(PREFERENCE_PADES_POLICY_HASH);
			PreferencesManager.remove(PREFERENCE_PADES_POLICY_HASH_ALGORITHM);
			PreferencesManager.remove(PREFERENCE_PADES_POLICY_QUALIFIER);
		}
		this.padesPolicyDlg.saveCurrentPolicy();
	}

	void loadPreferences() {
		this.padesSignReason.setText(PreferencesManager.get(PREFERENCE_PADES_SIGN_REASON));
		this.padesSignProductionCity.setText(PreferencesManager.get(PREFERENCE_PADES_SIGN_PRODUCTION_CITY));
		this.padesSignerContact.setText(PreferencesManager.get(PREFERENCE_PADES_SIGNER_CONTACT));
		this.visiblePdfSignature.setSelected(PreferencesManager.getBoolean(PREFERENCE_PADES_VISIBLE));

        final ComboBoxModel<Object> padesFormatModel = this.padesBasicFormat.getModel();
        final String selectedValue = PreferencesManager.get(PREFERENCE_PADES_FORMAT);
		for (int i = 0; i < padesFormatModel.getSize(); i++) {
			if (padesFormatModel.getElementAt(i).equals(selectedValue)) {
				this.padesBasicFormat.setSelectedIndex(i);
				break;
			}
		}

		final List<PolicyPanel.PolicyItem> padesPolicies = new ArrayList<>();
        padesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_CADES_PADES_AGE_1_9
    		)
		);
//        this.panelPolicies.removeAll();
        this.padesPolicyDlg = new PolicyPanel(
    		SIGN_FORMAT_PADES,
    		padesPolicies,
    		getPadesPreferedPolicy(),
    		isBlocked()
		);

//        final GridBagConstraints c = new GridBagConstraints();
//        c.fill = GridBagConstraints.BOTH;
//        c.weightx = 1.0;
//        c.gridy = 0;
//        this.panelPolicies.add(this.padesPolicyDlg, c);
        revalidate();
        repaint();
	}

	/** Carga las opciones de configuraci&oacute;n por defecto del panel de
	 * firmas PAdES desde un fichero externo de preferencias. */
	void loadDefaultPreferences() {

		this.padesSignReason.setText(PreferencesManager.getDefaultPreference(PREFERENCE_PADES_SIGN_REASON));
		this.padesSignProductionCity.setText(PreferencesManager.getDefaultPreference(PREFERENCE_PADES_SIGN_PRODUCTION_CITY));
		this.padesSignerContact.setText(PreferencesManager.getDefaultPreference(PREFERENCE_PADES_SIGNER_CONTACT));
		this.visiblePdfSignature.setSelected(PreferencesManager.getBooleanDefaultPreference(PREFERENCE_PADES_VISIBLE));

        if (this.padesBasicFormat.getItemCount() > 0) {
			this.padesBasicFormat.setSelectedIndex(0);
		}

        // Solo se reestablece el valor al por defecto, si no se ha bloqueado la edicion de la interfaz
		if (!isBlocked()) {
			final String selectedValue = PreferencesManager.getDefaultPreference(PREFERENCE_PADES_FORMAT);
			final ComboBoxModel<Object> padesFormatModel = this.padesBasicFormat.getModel();
			for (int i = 0; i < padesFormatModel.getSize(); i++) {
				if (padesFormatModel.getElementAt(i).equals(selectedValue)) {
					this.padesBasicFormat.setSelectedIndex(i);
					break;
				}
			}
		}

		this.padesBasicFormat.setEnabled(!isBlocked());

		final List<PolicyPanel.PolicyItem> padesPolicies = new ArrayList<>();
        padesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_CADES_PADES_AGE_1_9
    		)
		);

        this.padesPolicyDlg = new PolicyPanel(
    		SIGN_FORMAT_PADES,
    		padesPolicies,
    		getPadesDefaultPolicy(),
    		isBlocked()
		);

		this.currentPolicyValue.setText(this.padesPolicyDlg.getSelectedPolicyName());

        revalidate();
        repaint();
	}

	/** Obtiene la configuraci&oacute;n de pol&iacute;tica de firma PAdES establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada. */
	private static AdESPolicy getPadesPreferedPolicy() {

		if (PreferencesManager.get(PREFERENCE_PADES_POLICY_IDENTIFIER) == null ||
				PreferencesManager.get(PREFERENCE_PADES_POLICY_IDENTIFIER).isEmpty()) {
			return null;
		}
		try {
			return new AdESPolicy(
				PreferencesManager.get(PREFERENCE_PADES_POLICY_IDENTIFIER),
				PreferencesManager.get(PREFERENCE_PADES_POLICY_HASH),
				PreferencesManager.get(PREFERENCE_PADES_POLICY_HASH_ALGORITHM),
				PreferencesManager.get(PREFERENCE_PADES_POLICY_QUALIFIER)
			);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al recuperar la politica PAdES guardada en preferencias: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
	}

	/** Obtiene la configuraci&oacute;n de pol&iacute;tica de firma PAdES por
	 * defecto desde el fichero de preferencias.
	 * @return Pol&iacute;tica de firma configurada. */
	private AdESPolicy getPadesDefaultPolicy() {

		AdESPolicy adesPolicy = null;

		loadPadesPolicy();

		// Si la interfaz esta bloqueada, establecemos el valor que estuviese definido
		if (isBlocked()) {
			adesPolicy = this.padesPolicyDlg.getSelectedPolicy();
		}
		// Si no, devolvemos la configuracion por defecto
		else {
			try {
				if (PreferencesManager.getDefaultPreference(PREFERENCE_PADES_POLICY_IDENTIFIER) == null
						|| PreferencesManager.getDefaultPreference(PREFERENCE_PADES_POLICY_IDENTIFIER).isEmpty()) {
					this.padesPolicyDlg.loadPolicy(null);
				}
				else {
					this.padesPolicyDlg.loadPolicy(
						new AdESPolicy(PreferencesManager.getDefaultPreference(PREFERENCE_PADES_POLICY_IDENTIFIER),
							PreferencesManager.getDefaultPreference(PREFERENCE_PADES_POLICY_HASH),
							PreferencesManager.getDefaultPreference(PREFERENCE_PADES_POLICY_HASH_ALGORITHM),
							PreferencesManager.getDefaultPreference(PREFERENCE_PADES_POLICY_QUALIFIER)
						)
					);
				}

			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma") //$NON-NLS-1$
					.severe("Error al recuperar la politica PAdES guardada en fichero de preferencias: " + e); //$NON-NLS-1$

			}
		}

		return adesPolicy;
	}

	/** Indica si el panel permite o no la edici&oacute;n de sus valores.
	 * @return {@code true} si est&aacute; bloqueado y no permite la edici&oacute;n,
	 * {@code false} en caso contrario. */
	public boolean isBlocked() {
		return this.blocked;
	}

	/** Establece si deben bloquearse las opciones de configuraci&oacute;n del panel.
	 * @param blocked {@code true} si las opciones de configuraci&oacute;n deben bloquearse,
	 * {@code false} en caso contrario. */
	public void setBlocked(final boolean blocked) {
		this.blocked = blocked;
	}

	/** Carga el panel de pol&iacute;tica con las preferencias guardadas. */
	private void loadPadesPolicy() {
		// Si el panel no esta cargado lo obtengo de las preferencias guardadas
		if (this.padesPolicyDlg == null) {
			final List<PolicyPanel.PolicyItem> padesPolicies = new ArrayList<>();
			padesPolicies.add(
				new PolicyItem(SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
				POLICY_CADES_PADES_AGE_1_9)
			);
			this.padesPolicyDlg = new PolicyPanel(SIGN_FORMAT_PADES, padesPolicies, getPadesPreferedPolicy(), isBlocked());
		}
	}

	/** Di&aacute;logo para cambiar la configuraci&oacute;n de la pol&iacute;tica.
	 * @param container Contenedor en el que se define el di&aacute;logo. */
	public void changePadesPolicyDlg(final Container container) {

		// Cursor en espera
		container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

		// Cursor por defecto
		container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

		loadPadesPolicy();

		final int confirmDialog = AOUIFactory.showConfirmDialog(container, this.padesPolicyDlg,
				SimpleAfirmaMessages.getString("PolicyDialog.0"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION, JOptionPane.DEFAULT_OPTION);

		if (confirmDialog == JOptionPane.OK_OPTION) {

			try {
				checkPreferences();

				this.currentPolicyValue.setText(this.padesPolicyDlg.getSelectedPolicyName());
				final AdESPolicy padesPolicy = this.padesPolicyDlg.getSelectedPolicy();

				if (padesPolicy != null) {
					PreferencesManager.put(PREFERENCE_PADES_POLICY_IDENTIFIER, padesPolicy.getPolicyIdentifier());
					PreferencesManager.put(PREFERENCE_PADES_POLICY_HASH,
							padesPolicy.getPolicyIdentifierHash());
					PreferencesManager.put(PREFERENCE_PADES_POLICY_HASH_ALGORITHM,
							padesPolicy.getPolicyIdentifierHashAlgorithm());
					if (padesPolicy.getPolicyQualifier() != null) {
						PreferencesManager.put(PREFERENCE_PADES_POLICY_QUALIFIER,
								padesPolicy.getPolicyQualifier().toString());
					} else {
						PreferencesManager.remove(PREFERENCE_PADES_POLICY_QUALIFIER);
					}
					// Para cualquier politica definida se usa PAdES-BES como formato de firma

					this.padesBasicFormat.removeAllItems();
					this.padesBasicFormat.addItem(new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BES, PADES_FORMAT_BES_TEXT));
					this.padesBasicFormat.addItem(new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BASIC, PADES_FORMAT_BASIC_TEXT));
					this.padesBasicFormat.setSelectedIndex(0);
					this.padesBasicFormat.setEnabled(false);

				} else {
					PreferencesManager.remove(PREFERENCE_PADES_POLICY_IDENTIFIER);
					PreferencesManager.remove(PREFERENCE_PADES_POLICY_HASH);
					PreferencesManager.remove(PREFERENCE_PADES_POLICY_HASH_ALGORITHM);
					PreferencesManager.remove(PREFERENCE_PADES_POLICY_QUALIFIER);

					this.padesBasicFormat.setEnabled(true);
				}

				this.padesPolicyDlg.saveCurrentPolicy();

			} catch (final Exception e) {

				AOUIFactory.showErrorMessage(null,
						"<html><p>" + SimpleAfirmaMessages.getString("PreferencesPanel.7") + ":<br>" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								+ e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE);
				changePadesPolicyDlg(container);

			}

		}

		// Siempre, tras cualquier operación limpio el panel
		this.padesPolicyDlg = null;
	}

}
