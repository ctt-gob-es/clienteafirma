/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.GridLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.standalone.Messages;

final class PreferencesPanel extends JPanel {

    final JButton applyButton = new JButton(Messages.getString("PreferencesPanel.0")); //$NON-NLS-1$

    private final Window window;
    Window getParentWindow() {
        return this.window;
    }

    private final ModificationListener modificationListener;

	private static final String PREFERENCE_SIGNATURE_ALGORITHM = "signatureAlgorithm"; //$NON-NLS-1$
	private static final String PREFERENCE_POLICY_IDENTIFIER = "policyIdentifier"; //$NON-NLS-1$
	private static final String PREFERENCE_POLICY_IDENTIFIER_HASH = "policyIdentifierHash"; //$NON-NLS-1$
	private static final String PREFERENCE_POLICY_IDENTIFIER_HASH_ALGORITHM = "policyIdentifierHashAlgorithm"; //$NON-NLS-1$
	private static final String PREFERENCE_POLICY_QUALIFIER = "policyQualifier"; //$NON-NLS-1$

	private static final String PREFERENCE_CADES_IMPLICIT = "cadesImplicitMode"; //$NON-NLS-1$

	private static final String PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY = "xadesSignatureProductionCity"; //$NON-NLS-1$
	private static final String PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE = "xadesSignatureProductionProvince"; //$NON-NLS-1$
	private static final String PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE = "xadesSignatureProductionPostalCode"; //$NON-NLS-1$
	private static final String PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY = "xadesSignatureProductionCountry"; //$NON-NLS-1$
	private static final String PREFERENCE_XADES_SIGNER_CLAIMED_ROLE = "xadesSignerClaimedRole"; //$NON-NLS-1$
	private static final String PREFERENCE_XADES_SIGNER_CERTIFIED_ROLE = "xadesSignerCertifiedRole"; //$NON-NLS-1$
	private static final String PREFERENCE_XADES_SIGN_FORMAT = "xadesSignFormat"; //$NON-NLS-1$

	private static final String PREFERENCE_PADES_SIGN_REASON = "padesSignReason"; //$NON-NLS-1$
	private static final String PREFERENCE_PADES_SIGN_PRODUCTION_CITY = "padesSignProductionCity"; //$NON-NLS-1$
	private static final String PREFERENCE_PADES_SIGNER_CONTACT = "padesSignerContact"; //$NON-NLS-1$

	private static final long serialVersionUID = -3168095095548385291L;

	private static final Preferences PREFERENCES = Preferences.userRoot();

	//TODO: Poner los datos de la politica AGE
	private static final AdESPolicy POLICY_AGE = new AdESPolicy(
		"http://politica/id", //$NON-NLS-1$
		"AAAA", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"http://politica/qualifier" //$NON-NLS-1$
	);

	private static final int POLICY_INDEX_AGE = 1;

	private static final int POLICY_INDEX_NONE = 0;

	private static final AdESPolicy POLICY_CUSTOM;
	static {
		AdESPolicy customPolicy = null;
		try {
			customPolicy = new AdESPolicy(
				PREFERENCES.get(PREFERENCE_POLICY_IDENTIFIER, null),
				PREFERENCES.get(PREFERENCE_POLICY_IDENTIFIER_HASH, null),
				PREFERENCES.get(PREFERENCE_POLICY_IDENTIFIER_HASH_ALGORITHM, null),
				PREFERENCES.get(PREFERENCE_POLICY_QUALIFIER, null)
			);
		}
		catch(final Exception e) {
			// Se ignora
		}
		POLICY_CUSTOM = customPolicy;
	}

	private static final List<AdESPolicy> PRELOADED_POLICIES = new ArrayList<AdESPolicy>(1);
	static {
		PRELOADED_POLICIES.add(
			POLICY_INDEX_NONE, // Por posicion, en el 0 ninguna politica
			null
		);
		PRELOADED_POLICIES.add(
			POLICY_INDEX_AGE, // Por posicion, en el 1 la politica de la AGE
			POLICY_AGE
		);
		PRELOADED_POLICIES.add(
			2, // Por posicion, en el ultimo lugar una politica a medida
			POLICY_CUSTOM
		);
	}
	static List<AdESPolicy> getPreloadedPolicies() {
		return PRELOADED_POLICIES;
	}

	private final JComboBox signarureAlgorithms = new JComboBox();

	private final JComboBox policies = new JComboBox();
	JComboBox getPolicies() {
		return this.policies;
	}

	private final JTextField policyIdentifier = new JTextField();
	JTextField getPolicyIdentifier() {
		return this.policyIdentifier;
	}

	private final JTextField policyIdentifierHash = new JTextField();
	JTextField getPolicyIdentifierHash() {
		return this.policyIdentifierHash;
	}

	private final JComboBox policyIdentifierHashAlgorithm = new JComboBox(new String[] {
		"SHA1", //$NON-NLS-1$
		"SHA-512", //$NON-NLS-1$
		"SHA-384", //$NON-NLS-1$
		"SHA-256" //$NON-NLS-1$
	});
	JComboBox getPolicyIdentifierHashAlgorithm() {
		return this.policyIdentifierHashAlgorithm;
	}

	private final JTextField policyQualifier =new JTextField();
	JTextField getPolicyQualifier() {
		return this.policyQualifier;
	}

	private final JTextField padesSignReason = new JTextField(
		PREFERENCES.get(PREFERENCE_PADES_SIGN_REASON, "") //$NON-NLS-1$
	);
	private final JTextField padesSignProductionCity = new JTextField(
		PREFERENCES.get(PREFERENCE_PADES_SIGN_PRODUCTION_CITY, "") //$NON-NLS-1$
	);
	private final JTextField padesSignerContact = new JTextField(
		PREFERENCES.get(PREFERENCE_PADES_SIGNER_CONTACT, "") //$NON-NLS-1$
	);

	private final JCheckBox cadesImplicit = new JCheckBox(
		Messages.getString("PreferencesPanel.1"), //$NON-NLS-1$
		Boolean.parseBoolean(PREFERENCES.get(PREFERENCE_CADES_IMPLICIT, "true")) //$NON-NLS-1$
	);

	private final JTextField xadesSignatureProductionCity = new JTextField(
		PREFERENCES.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignatureProductionProvince = new JTextField(
		PREFERENCES.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignatureProductionPostalCode = new JTextField(
		PREFERENCES.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignatureProductionCountry = new JTextField(
		PREFERENCES.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignerClaimedRole = new JTextField(
		PREFERENCES.get(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignerCertifiedRole = new JTextField(
		PREFERENCES.get(PREFERENCE_XADES_SIGNER_CERTIFIED_ROLE, "") //$NON-NLS-1$
	);
	private final JComboBox xadesSignFormat = new JComboBox(new String[] {
	  AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
      AOSignConstants.SIGN_FORMAT_XADES_DETACHED,
      AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED
	});

	void createUI() {
		final JTabbedPane tabbedPane = new JTabbedPane();
		tabbedPane.addTab(Messages.getString("PreferencesPanel.2"), createGeneralPanel()); //$NON-NLS-1$
		tabbedPane.addTab(Messages.getString("PreferencesPanel.3"), createPadesPanel()); //$NON-NLS-1$
		tabbedPane.addTab(Messages.getString("PreferencesPanel.4"), createCadesPanel()); //$NON-NLS-1$
		tabbedPane.addTab(Messages.getString("PreferencesPanel.5"), createXadesPanel()); //$NON-NLS-1$

		add(tabbedPane);
		add(createButtonsPanel());
	}

	@SuppressWarnings("unused")
    boolean savePreferences() {
		// Lo primero que hay que guardar es la politica, porque puede dar error

		//****************************************************************************
		//**** PREFERENCIAS GENERALES ************************************************
		//****************************************************************************
		if (this.policies.getSelectedIndex() != POLICY_INDEX_NONE) {
			try {
				// Construimos el objeto para ver si los datos son correctos, ya que se comprueban en el constructor
				new AdESPolicy(
					PreferencesPanel.this.getPolicyIdentifier().getText(),
					PreferencesPanel.this.getPolicyIdentifierHash().getText(),
					PreferencesPanel.this.getPolicyIdentifierHashAlgorithm().getSelectedItem().toString(),
					PreferencesPanel.this.getPolicyQualifier().getText()
				);
			}
			catch(final Exception e) {
				JOptionPane.showMessageDialog(
					this,
					"<html><p>" + Messages.getString("PreferencesPanel.6") + ":<br>" + e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					Messages.getString("PreferencesPanel.7"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
				return false;
			}
			PreferencesPanel.PREFERENCES.put(PREFERENCE_POLICY_IDENTIFIER, this.policyIdentifier.getText());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_POLICY_IDENTIFIER_HASH, this.policyIdentifierHash.getText());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_POLICY_IDENTIFIER_HASH_ALGORITHM, this.policyIdentifierHashAlgorithm.getSelectedItem().toString());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_POLICY_QUALIFIER, this.policyQualifier.getText());
		}
		else {
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_POLICY_IDENTIFIER);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_POLICY_IDENTIFIER_HASH);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_POLICY_QUALIFIER);
		}

		PreferencesPanel.PREFERENCES.put(PREFERENCE_SIGNATURE_ALGORITHM, this.signarureAlgorithms.getSelectedItem().toString());

		//****************************************************************************
		//**** PREFERENCIAS CADES ****************************************************
		//****************************************************************************
		PreferencesPanel.PREFERENCES.put(PREFERENCE_CADES_IMPLICIT, Boolean.valueOf(this.cadesImplicit.isSelected()).toString());

		//****************************************************************************
		//**** PREFERENCIAS PADES ****************************************************
		//****************************************************************************
		if ("".equals(this.padesSignerContact.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_SIGNER_CONTACT);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_SIGNER_CONTACT, this.padesSignerContact.getText());
		}
		if ("".equals(this.padesSignProductionCity.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_SIGN_PRODUCTION_CITY);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_SIGN_PRODUCTION_CITY, this.padesSignProductionCity.getText());
		}
		if ("".equals(this.padesSignReason.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_SIGN_REASON);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_SIGN_REASON, this.padesSignReason.getText());
		}

		//****************************************************************************
		//**** PREFERENCIAS XADES ****************************************************
		//****************************************************************************
		PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGN_FORMAT, this.xadesSignFormat.getSelectedItem().toString());
		if ("".equals(this.xadesSignatureProductionCity.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, this.xadesSignatureProductionCity.getText());
		}
		if ("".equals(this.xadesSignatureProductionCountry.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, this.xadesSignatureProductionCountry.getText());
		}
		if ("".equals(this.xadesSignatureProductionPostalCode.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, this.xadesSignatureProductionPostalCode.getText());
		}
		if ("".equals(this.xadesSignatureProductionProvince.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, this.xadesSignatureProductionProvince.getText());
		}
		if ("".equals(this.xadesSignerCertifiedRole.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_SIGNER_CERTIFIED_ROLE);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGNER_CERTIFIED_ROLE, this.xadesSignerCertifiedRole.getText());
		}
		if ("".equals(this.xadesSignerClaimedRole.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, this.xadesSignerClaimedRole.getText());
		}

		try {
			PreferencesPanel.PREFERENCES.flush();
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al guardar las preferencias de firma: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}

	    return true;

	}

	private JPanel createXadesPanel() {
        final JPanel panel = new JPanel();

        final JPanel metadata = new JPanel();
        metadata.setBorder(BorderFactory.createTitledBorder(Messages.getString("PreferencesPanel.8"))); //$NON-NLS-1$
        metadata.setLayout(new GridLayout(0,1));

        final JLabel xadesSignatureProductionProvinceLabel = new JLabel(Messages.getString("PreferencesPanel.9")); //$NON-NLS-1$
        xadesSignatureProductionProvinceLabel.setLabelFor(this.xadesSignatureProductionProvince);
        metadata.add(xadesSignatureProductionProvinceLabel);
        this.xadesSignatureProductionProvince.addKeyListener(this.modificationListener);
        metadata.add(this.xadesSignatureProductionProvince);

        final JLabel xadesSignatureProductionPostalCodeLabel = new JLabel(Messages.getString("PreferencesPanel.10")); //$NON-NLS-1$
        xadesSignatureProductionPostalCodeLabel.setLabelFor(this.xadesSignatureProductionPostalCode);
        metadata.add(xadesSignatureProductionPostalCodeLabel);
        this.xadesSignatureProductionPostalCode.addKeyListener(this.modificationListener);
        metadata.add(this.xadesSignatureProductionPostalCode);

        final JLabel xadesSignatureProductionCityLabel = new JLabel(Messages.getString("PreferencesPanel.11")); //$NON-NLS-1$
        xadesSignatureProductionCityLabel.setLabelFor(this.xadesSignatureProductionCity);
        metadata.add(xadesSignatureProductionCityLabel);
        this.xadesSignatureProductionCity.addKeyListener(this.modificationListener);
        metadata.add(this.xadesSignatureProductionCity);

        final JLabel xadesSignatureProductionCountryLabel = new JLabel(Messages.getString("PreferencesPanel.12")); //$NON-NLS-1$
        xadesSignatureProductionCountryLabel.setLabelFor(this.xadesSignatureProductionCountry);
        metadata.add(xadesSignatureProductionCountryLabel);
        this.xadesSignatureProductionCountry.addKeyListener(this.modificationListener);
        metadata.add(this.xadesSignatureProductionCountry);

        final JLabel xadesSignerClaimedRoleLabel = new JLabel(Messages.getString("PreferencesPanel.13")); //$NON-NLS-1$
        xadesSignerClaimedRoleLabel.setLabelFor(this.xadesSignerClaimedRole);
        metadata.add(xadesSignerClaimedRoleLabel);
        this.xadesSignerClaimedRole.addKeyListener(this.modificationListener);
        metadata.add(this.xadesSignerClaimedRole);

        final JLabel xadesSignerCertifiedRoleLabel = new JLabel(Messages.getString("PreferencesPanel.14")); //$NON-NLS-1$
        xadesSignerCertifiedRoleLabel.setLabelFor(this.xadesSignerCertifiedRole);
        metadata.add(xadesSignerCertifiedRoleLabel);
        this.xadesSignerCertifiedRole.addKeyListener(this.modificationListener);
        metadata.add(this.xadesSignerCertifiedRole);

        final JPanel format = new JPanel();
        format.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), Messages.getString("PreferencesPanel.15"))); //$NON-NLS-1$
        this.xadesSignFormat.setSelectedItem(
    		PREFERENCES.get(PREFERENCE_XADES_SIGN_FORMAT, AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)
		);
        this.xadesSignFormat.addItemListener(this.modificationListener);
        format.add(this.xadesSignFormat);

        panel.add(metadata);
        panel.add(format);
        return panel;
	}

	private JPanel createCadesPanel() {
	    final JPanel panel = new JPanel();
	    final JPanel signatureMode = new JPanel();
	    signatureMode.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), Messages.getString("PreferencesPanel.16"))); //$NON-NLS-1$
	    this.cadesImplicit.addItemListener(this.modificationListener);
	    signatureMode.add(this.cadesImplicit);
	    panel.add(signatureMode);
	    return panel;
	}

	private JPanel createGeneralPanel() {
		final JPanel panel = new JPanel();
		panel.setBorder(BorderFactory.createTitledBorder(Messages.getString("PreferencesPanel.17"))); //$NON-NLS-1$
		final JPanel signatureAgorithmPanel = new JPanel();
		signatureAgorithmPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), Messages.getString("PreferencesPanel.18"))); //$NON-NLS-1$
		this.signarureAlgorithms.addItemListener(this.modificationListener);
		this.signarureAlgorithms.setModel(new DefaultComboBoxModel(new String[] {
			"SHA1withRSA", //$NON-NLS-1$
			"SHA512withRSA", //$NON-NLS-1$
			"SHA384withRSA", //$NON-NLS-1$
			"SHA256withRSA" //$NON-NLS-1$
		}));
		this.signarureAlgorithms.setSelectedItem(PreferencesPanel.PREFERENCES.get(PREFERENCE_SIGNATURE_ALGORITHM, "SHA1withRSA")); //$NON-NLS-1$
		signatureAgorithmPanel.add(this.signarureAlgorithms);
		panel.add(signatureAgorithmPanel);
		panel.add(createPolicyPanel());
		return panel;
	}

	private JPanel createPadesPanel() {
	    final JPanel panel = new JPanel();

        panel.setLayout(new GridLayout(0,1));

	    panel.setBorder(BorderFactory.createTitledBorder(Messages.getString("PreferencesPanel.19"))); //$NON-NLS-1$

	    final JLabel padesSignReasonLabel = new JLabel(Messages.getString("PreferencesPanel.20")); //$NON-NLS-1$
	    padesSignReasonLabel.setLabelFor(this.padesSignReason);
	    panel.add(padesSignReasonLabel);
	    this.padesSignReason.addKeyListener(this.modificationListener);
	    panel.add(this.padesSignReason);

	    final JLabel padesSignProductionCityLabel = new JLabel(Messages.getString("PreferencesPanel.21")); //$NON-NLS-1$
	    padesSignProductionCityLabel.setLabelFor(this.padesSignProductionCity);
	    panel.add(padesSignProductionCityLabel);
	    this.padesSignProductionCity.addKeyListener(this.modificationListener);
	    panel.add(this.padesSignProductionCity);

	    final JLabel padesSignerContactLabel = new JLabel(Messages.getString("PreferencesPanel.22")); //$NON-NLS-1$
	    padesSignerContactLabel.setLabelFor(this.padesSignerContact);
	    panel.add(padesSignerContactLabel);
	    this.padesSignerContact.addKeyListener(this.modificationListener);
	    panel.add(this.padesSignerContact);

	    return panel;
	}

	private JPanel createPolicyPanel() {
		final JPanel panel = new JPanel();
		panel.setLayout(new GridLayout(0,1));
		panel.setBorder(BorderFactory.createTitledBorder(Messages.getString("PreferencesPanel.23"))); //$NON-NLS-1$

		// Los elementos del menu desplegable se identifican por su orden
		this.policies.setModel(new DefaultComboBoxModel(new String[] {
			Messages.getString("PreferencesPanel.24"),	// Ninguna politica, debe ser el primer elemento //$NON-NLS-1$
			Messages.getString("PreferencesPanel.25"),	// Politica de la AGE, debe ser el segundo elemento //$NON-NLS-1$
			Messages.getString("PreferencesPanel.26")	// Politica a medida, debe ser el ultimo elemento //$NON-NLS-1$
		}));
		panel.add(this.policies);
		this.policies.addItemListener(this.modificationListener);
		this.policies.addItemListener(new ItemListener() {
			/** {@inheritDoc} */
			@Override
			public void itemStateChanged(final ItemEvent ie) {

				final boolean enabled = PreferencesPanel.this.getPolicies().getSelectedIndex() == (PreferencesPanel.this.getPolicies().getItemCount()-1);
				PreferencesPanel.this.getPolicyIdentifier().setEnabled(enabled);
				PreferencesPanel.this.getPolicyIdentifierHash().setEnabled(enabled);
				PreferencesPanel.this.getPolicyIdentifierHashAlgorithm().setEnabled(enabled);
				PreferencesPanel.this.getPolicyQualifier().setEnabled(enabled);

				loadPolicy(getPreloadedPolicies().get(PreferencesPanel.this.getPolicies().getSelectedIndex()));
			}
		});

		final boolean enableTextFields = this.policies.getSelectedIndex() == (this.policies.getItemCount()-1);

		this.policyIdentifier.setEnabled(enableTextFields);
		this.policyIdentifier.addKeyListener(this.modificationListener);
		final JLabel policyIdentifierLabel = new JLabel(Messages.getString("PreferencesPanel.27")); //$NON-NLS-1$
		policyIdentifierLabel.setLabelFor(this.policyIdentifier);
		panel.add(policyIdentifierLabel);
		panel.add(this.policyIdentifier);

		this.policyIdentifierHash.setEnabled(enableTextFields);
		this.policyIdentifierHash.addKeyListener(this.modificationListener);
		final JLabel policyIdentifierHashLabel = new JLabel(Messages.getString("PreferencesPanel.28")); //$NON-NLS-1$
		policyIdentifierHashLabel.setLabelFor(this.policyIdentifierHash);
		panel.add(policyIdentifierHashLabel);
		panel.add(this.policyIdentifierHash);

		this.policyIdentifierHashAlgorithm.setEnabled(enableTextFields);
		this.policyIdentifierHashAlgorithm.addItemListener(this.modificationListener);
		final JLabel policyIdentifierHashAlgorithmLabel = new JLabel(Messages.getString("PreferencesPanel.29")); //$NON-NLS-1$
		policyIdentifierHashAlgorithmLabel.setLabelFor(this.policyIdentifierHashAlgorithm);
		panel.add(policyIdentifierHashAlgorithmLabel);
		panel.add(this.policyIdentifierHashAlgorithm);

		this.policyQualifier.setEnabled(enableTextFields);
		this.policyQualifier.addKeyListener(this.modificationListener);
		final JLabel policyQualifierLabel = new JLabel(Messages.getString("PreferencesPanel.30")); //$NON-NLS-1$
		policyQualifierLabel.setLabelFor(this.policyQualifier);
		panel.add(policyQualifierLabel);
		panel.add(this.policyQualifier);

		// Cargamos la politica de las preferencias cambiado el Combo si es preciso
		final AdESPolicy savedPolicy = PRELOADED_POLICIES.get(this.policies.getItemCount()-1);
		if (savedPolicy != null) {
			if (POLICY_AGE.equals(savedPolicy)) {
				this.policies.setSelectedIndex(POLICY_INDEX_AGE);
			}
			else {
				this.policies.setSelectedIndex(this.policies.getItemCount()-1);
			}
		}

		return panel;
	}

	private JPanel createButtonsPanel() {
		final JPanel panel = new JPanel();
		final JButton cancelButton = new JButton(Messages.getString("PreferencesPanel.31")); //$NON-NLS-1$
		cancelButton.setMnemonic('C');
		cancelButton.getAccessibleContext().setAccessibleDescription(
			Messages.getString("PreferencesPanel.32") //$NON-NLS-1$
		);
		cancelButton.addActionListener(new ActionListener() {
		    /** {@inheritDoc} */
            @Override
            public void actionPerformed(final ActionEvent ae) {
                PreferencesPanel.this.getParentWindow().dispose();
            }
        });
		panel.add(cancelButton);
		final JButton acceptButton = new JButton(Messages.getString("PreferencesPanel.33")); //$NON-NLS-1$
		acceptButton.setMnemonic('A');
		acceptButton.getAccessibleContext().setAccessibleDescription(
			Messages.getString("PreferencesPanel.34") //$NON-NLS-1$
		);
		acceptButton.addActionListener(new ActionListener() {
			/** {@inheritDoc} */
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (savePreferences()) {
				    PreferencesPanel.this.getParentWindow().dispose();
				}
			}
		});
		panel.add(acceptButton);
		this.applyButton.setMnemonic('p');
		this.applyButton.getAccessibleContext().setAccessibleDescription(
			Messages.getString("PreferencesPanel.35") //$NON-NLS-1$
		);
		this.applyButton.addActionListener(new ActionListener() {
			/** {@inheritDoc} */
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (savePreferences()) {
				    setModified(false);
				}
			}
		});
		this.applyButton.setEnabled(false);
		panel.add(this.applyButton);
		return panel;
	}

	PreferencesPanel(final Window w) {
	    if (w==null) {
	        throw new IllegalArgumentException("Es necesario proporcionar una referencia a la ventana contenedora"); //$NON-NLS-1$
	    }
	    this.window = w;
	    this.modificationListener = new ModificationListener(this);
	    createUI();
	}

	/** Indica si se ha modificado algun dato desde el &ucuate;ltimo guardado, y por lo tanto si hay algo nuevo para guardar. */
	void setModified(final boolean mod) {
	    this.applyButton.setEnabled(mod);
	}

	void loadPolicy(final AdESPolicy policy) {
		if (policy != null) {
			PreferencesPanel.this.getPolicyIdentifier().setText(policy.getPolicyIdentifier());
			PreferencesPanel.this.getPolicyIdentifierHash().setText(policy.getPolicyIdentifierHash());
			PreferencesPanel.this.getPolicyIdentifierHashAlgorithm().setSelectedItem(policy.getPolicyIdentifierHashAlgorithm());
			PreferencesPanel.this.getPolicyQualifier().setText(policy.getPolicyQualifier().toString());
		}
		else {
			PreferencesPanel.this.getPolicyIdentifier().setText(""); //$NON-NLS-1$
			PreferencesPanel.this.getPolicyIdentifierHash().setText(""); //$NON-NLS-1$
			PreferencesPanel.this.getPolicyQualifier().setText(""); //$NON-NLS-1$
		}
	}

	private static final class ModificationListener extends KeyAdapter implements ItemListener {

	    private final PreferencesPanel prefPanel;

        ModificationListener(final PreferencesPanel pp) {
            if (pp == null) {
                throw new IllegalArgumentException(
            		"Se necesita un ModificationListener para indicar que ha habido modificaciones, no puede ser nulo" //$NON-NLS-1$
        		);
            }
            this.prefPanel = pp;
        }

        /** {@inheritDoc} */
        @Override
        public void keyReleased(final KeyEvent arg0) {
            this.prefPanel.setModified(true);
        }

        /** {@inheritDoc} */
		@Override
		public void itemStateChanged(final ItemEvent arg0) {
			this.prefPanel.setModified(true);
		}

	}

}