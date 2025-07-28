/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_XADES_POLICY_HASH;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_XADES_POLICY_HASH_ALGORITHM;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_XADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_XADES_SIGNER_CLAIMED_ROLE;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_XADES_SIGN_FORMAT;

import java.awt.Color;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyListener;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;

public final class PreferencesPanelXades extends JScrollPane {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long serialVersionUID = 1424468022677956783L;

	private static final String SIGN_FORMAT_XADES = "XAdES"; //$NON-NLS-1$

	public static final AdESPolicy POLICY_XADES_AGE_1_9;

	private static final String POLICY_BUNDLE_NAME = "policy"; //$NON-NLS-1$

	static {

		final ResourceBundle policyBundle = ResourceBundle
				.getBundle(POLICY_BUNDLE_NAME, Locale.getDefault());

		POLICY_XADES_AGE_1_9  = new AdESPolicy(
				policyBundle.getString("FirmaAGE19.policyIdentifier"), //$NON-NLS-1$
				policyBundle.getString("FirmaAGE19.policyIdentifierHash.XAdES"), //$NON-NLS-1$
				"SHA1", //$NON-NLS-1$
				policyBundle.getString("FirmaAGE19.policyQualifier") //$NON-NLS-1$
			);
	}

	private final JTextField xadesSignatureProductionCity = new JTextField();
	private final JTextField xadesSignatureProductionProvince = new JTextField();
	private final JTextField xadesSignatureProductionPostalCode = new JTextField();
	private final JTextField xadesSignatureProductionCountry = new JTextField();
	private final JTextField xadesSignerClaimedRole = new JTextField();
	private final JComboBox<Object> xadesSignFormat = new JComboBox<>(
		new Object[] {
			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED,
			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
			AOSignConstants.SIGN_FORMAT_XADES_DETACHED
		}
	);

	private final JRadioButton optionCoSign = new JRadioButton(SimpleAfirmaMessages.getString("PreferencesPanel.168")); //$NON-NLS-1$
	private final JRadioButton optionCounterSignLeafs = new JRadioButton(SimpleAfirmaMessages.getString("PreferencesPanel.169")); //$NON-NLS-1$
	private final JRadioButton optionCounterSignTree = new JRadioButton(SimpleAfirmaMessages.getString("PreferencesPanel.170")); //$NON-NLS-1$

	private final JPanel panelPolicies = new JPanel();

	private PolicyPanel xadesPolicyDlg;

	/**
	 * Atributo que representa la etiqueta de la pol&iacute;tica seleccionada en
	 * el di&aacute;logo
	 */
	private JLabel currentPolicyValue;

	/**
	 * Atributo que permite gestionar el bloqueo de preferencias.
	 */
	private boolean blocked = true;

	PreferencesPanelXades(final KeyListener keyListener,
						  final ModificationListener modificationListener,
						  final boolean blocked) {

		setBlocked(blocked);
		createUI(keyListener, modificationListener);
	}

	void createUI(final KeyListener keyListener,
				  final ModificationListener modificationListener
				  ) {

		getVerticalScrollBar().setUnitIncrement(16);

		final JPanel mainPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;

        loadXadesPolicy();

        loadPreferences();

    	this.xadesPolicyDlg.setModificationListener(modificationListener);
    	this.xadesPolicyDlg.setKeyListener(keyListener);

        ///////////// Inicio Panel Policy ////////////////

        final JPanel policyConfigPanel = createPolicyPanel();
		policyConfigPanel.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.153") //$NON-NLS-1$
			)
		);

        ///////////// Fin Panel Policy ////////////////

        ///////////// Inicio Panel Opciones de firma ////////////////

	    final JPanel signOptions = new JPanel(new FlowLayout(FlowLayout.LEADING));
        signOptions.setBorder(
        	BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.69") //$NON-NLS-1$
			)
		);

        final JPanel signOptionsInnerPanel = new JPanel(new GridBagLayout());

        this.xadesSignFormat.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("PreferencesPanel.183")); //$NON-NLS-1$
        this.xadesSignFormat.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.53")); //$NON-NLS-1$
        this.xadesSignFormat.addItemListener(modificationListener);
        this.xadesSignFormat.addKeyListener(keyListener);
        this.xadesSignFormat.setEnabled(!isBlocked());

        final JLabel xadesFormatLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanel.15") //$NON-NLS-1$
		);
        xadesFormatLabel.addKeyListener(keyListener);
        xadesFormatLabel.setLabelFor(this.xadesSignFormat);

        final GridBagConstraints cf = new GridBagConstraints();
        cf.anchor = GridBagConstraints.LINE_START;
        cf.insets = new Insets(0, 7, 4, 7);
        cf.gridx = 0;
        cf.gridx = 0;

        signOptionsInnerPanel.add(xadesFormatLabel, cf);
        cf.gridx = 1;
        signOptionsInnerPanel.add(this.xadesSignFormat, cf);
        signOptions.add(signOptionsInnerPanel);

        ///////////// Fin Panel Opciones de firma ////////////////

        ///////////// Inicio Panel Multisign ////////////////

        final JPanel multisignConfigPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints mcpc = new GridBagConstraints();
        mcpc.fill = GridBagConstraints.BOTH;
        mcpc.weightx = 1.0;
        mcpc.gridy = 0;

        multisignConfigPanel.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.167") //$NON-NLS-1$
			)
		);

        final ButtonGroup group = new ButtonGroup();
        group.add(this.optionCoSign);
        group.add(this.optionCounterSignLeafs);
        group.add(this.optionCounterSignTree);

        this.optionCoSign.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("PreferencesPanel.184")); //$NON-NLS-1$
        this.optionCoSign.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.168")); //$NON-NLS-1$
        this.optionCoSign.setEnabled(!isBlocked());
    	this.optionCoSign.addItemListener(modificationListener);
    	this.optionCoSign.addKeyListener(keyListener);

        this.optionCounterSignLeafs.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("PreferencesPanel.184")); //$NON-NLS-1$
        this.optionCounterSignLeafs.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.169")); //$NON-NLS-1$
        this.optionCounterSignLeafs.setEnabled(!isBlocked());
        this.optionCounterSignLeafs.addItemListener(modificationListener);
        this.optionCounterSignLeafs.addKeyListener(keyListener);

        this.optionCounterSignTree.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("PreferencesPanel.184")); //$NON-NLS-1$
        this.optionCounterSignTree.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.170")); //$NON-NLS-1$
        this.optionCounterSignTree.setEnabled(!isBlocked());
        this.optionCounterSignTree.addItemListener(modificationListener);
        this.optionCounterSignTree.addKeyListener(keyListener);

        multisignConfigPanel.add(this.optionCoSign,mcpc);

        mcpc.gridy++;
        multisignConfigPanel.add(this.optionCounterSignLeafs,mcpc);

        mcpc.gridy++;
        multisignConfigPanel.add(this.optionCounterSignTree,mcpc);

        ///////////// Fin Panel Multisign ////////////////

        ///////////// Inicio Panel Metadatos ////////////////

        final JPanel metadata = new JPanel();
        metadata.setBorder(
        		BorderFactory.createTitledBorder(
        				SimpleAfirmaMessages.getString("PreferencesPanel.8") //$NON-NLS-1$
        		)
        );
        metadata.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;

        final JLabel xadesSignatureProductionCityLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.11") //$NON-NLS-1$
		);
        xadesSignatureProductionCityLabel.setLabelFor(this.xadesSignatureProductionCity);
        c.gridy++;
        metadata.add(xadesSignatureProductionCityLabel, c);
        this.xadesSignatureProductionCity.getAccessibleContext().setAccessibleName(
    		SimpleAfirmaMessages.getString("PreferencesPanel.181") //$NON-NLS-1$
		);
        this.xadesSignatureProductionCity.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.66") //$NON-NLS-1$
		);
        this.xadesSignatureProductionCity.addKeyListener(modificationListener);
        this.xadesSignatureProductionCity.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignatureProductionCity, c);

        final JLabel xadesSignatureProductionProvinceLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.14") //$NON-NLS-1$
		);
        xadesSignatureProductionProvinceLabel.setLabelFor(this.xadesSignatureProductionProvince);
        c.gridy++;
        metadata.add(xadesSignatureProductionProvinceLabel, c);
        this.xadesSignatureProductionProvince.getAccessibleContext().setAccessibleName(
    		SimpleAfirmaMessages.getString("PreferencesPanel.181") //$NON-NLS-1$
		);
        this.xadesSignatureProductionProvince.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.14") //$NON-NLS-1$
		);
        this.xadesSignatureProductionProvince.addKeyListener(modificationListener);
        this.xadesSignatureProductionProvince.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignatureProductionProvince, c);

        final JLabel xadesSignatureProductionPostalCodeLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.102") //$NON-NLS-1$
		);
        xadesSignatureProductionPostalCodeLabel.setLabelFor(this.xadesSignatureProductionPostalCode);
        c.gridy++;
        metadata.add(xadesSignatureProductionPostalCodeLabel, c);
        this.xadesSignatureProductionPostalCode.getAccessibleContext().setAccessibleName(
    		SimpleAfirmaMessages.getString("PreferencesPanel.181") //$NON-NLS-1$
		);
        this.xadesSignatureProductionPostalCode.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.102") //$NON-NLS-1$
		);
        this.xadesSignatureProductionPostalCode.addKeyListener(modificationListener);
        this.xadesSignatureProductionPostalCode.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignatureProductionPostalCode, c);

        final JLabel xadesSignatureProductionCountryLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.12")); //$NON-NLS-1$
        xadesSignatureProductionCountryLabel.setLabelFor(this.xadesSignatureProductionCountry);
        c.gridy++;
        metadata.add(xadesSignatureProductionCountryLabel, c);
        this.xadesSignatureProductionCountry.getAccessibleContext().setAccessibleName(
    		SimpleAfirmaMessages.getString("PreferencesPanel.181") //$NON-NLS-1$
		);
        this.xadesSignatureProductionCountry.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.12") //$NON-NLS-1$
		);
        this.xadesSignatureProductionCountry.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.67")); //$NON-NLS-1$
        this.xadesSignatureProductionCountry.addKeyListener(modificationListener);
        this.xadesSignatureProductionCountry.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignatureProductionCountry, c);

        final JLabel xadesSignerCertifiedRoleLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanelXades.0")); //$NON-NLS-1$
        xadesSignerCertifiedRoleLabel.setLabelFor(this.xadesSignerClaimedRole);
        c.gridy++;
        metadata.add(xadesSignerCertifiedRoleLabel, c);
        this.xadesSignerClaimedRole.getAccessibleContext().setAccessibleName(
    		SimpleAfirmaMessages.getString("PreferencesPanel.181") //$NON-NLS-1$
		);
        this.xadesSignerClaimedRole.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanelXades.0") //$NON-NLS-1$
		);
        this.xadesSignerClaimedRole.addKeyListener(modificationListener);
        this.xadesSignerClaimedRole.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignerClaimedRole, c);

        ///////////// Fin Panel Metadatos ////////////////


		gbc.gridy++;
        mainPanel.add(policyConfigPanel, gbc);

        gbc.gridy++;
        mainPanel.add(signOptions, gbc);

		gbc.gridy++;
        mainPanel.add(multisignConfigPanel, gbc);

        gbc.gridy++;
        mainPanel.add(metadata, gbc);

        gbc.gridy++;
        gbc.weighty = 1.0;
        mainPanel.add(new JPanel(), gbc);

		// Panel para el boton de restaurar la configuracion
		final JPanel panelGeneral = new JPanel(new FlowLayout(FlowLayout.TRAILING));

		final JButton restoreConfigButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.147") //$NON-NLS-1$
		);

		restoreConfigButton.setMnemonic('R');
		restoreConfigButton.addActionListener(ae -> {
			if (AOUIFactory.showConfirmDialog(getParent(), SimpleAfirmaMessages.getString("PreferencesPanel.156"), //$NON-NLS-1$
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

	private JPanel createPolicyPanel() {

		final JLabel currentPolicyLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.171")); //$NON-NLS-1$
		this.currentPolicyValue = new JLabel(this.xadesPolicyDlg.getSelectedPolicyName());
		this.currentPolicyValue.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));
		this.currentPolicyValue.setFocusable(true);
		this.currentPolicyValue.addFocusListener(new FocusListener() {
			@Override
			public void focusLost(final FocusEvent evt) {
				((JComponent) evt.getSource()).setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));
			}
			@Override
			public void focusGained(final FocusEvent evt) {
				((JComponent) evt.getSource()).setBorder(BorderFactory.createLineBorder(Color.black, 1));
			}
		});
		currentPolicyLabel.setLabelFor(this.currentPolicyValue);

		final JButton policyConfigButton = new JButton(
				SimpleAfirmaMessages.getString("PreferencesPanel.150") //$NON-NLS-1$
			);


		policyConfigButton.setMnemonic('P');
		policyConfigButton.addActionListener(
			ae -> changeXadesPolicyDlg(getParent())
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

	void savePreferences() {
		PreferencesManager.put(PREFERENCE_XADES_SIGN_FORMAT, this.xadesSignFormat.getSelectedItem().toString());

		PreferencesManager.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, this.xadesSignatureProductionCity.getText());
		PreferencesManager.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, this.xadesSignatureProductionProvince.getText());
		PreferencesManager.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, this.xadesSignatureProductionPostalCode.getText());
		PreferencesManager.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, this.xadesSignatureProductionCountry.getText());
		PreferencesManager.put(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, this.xadesSignerClaimedRole.getText());

		final AdESPolicy xadesPolicy = this.xadesPolicyDlg.getSelectedPolicy();
		if (xadesPolicy != null) {
			PreferencesManager.put(PREFERENCE_XADES_POLICY_IDENTIFIER, xadesPolicy.getPolicyIdentifier());
			PreferencesManager.put(PREFERENCE_XADES_POLICY_HASH, xadesPolicy.getPolicyIdentifierHash());
			PreferencesManager.put(PREFERENCE_XADES_POLICY_HASH_ALGORITHM, xadesPolicy.getPolicyIdentifierHashAlgorithm());
			if (xadesPolicy.getPolicyQualifier() != null) {
				PreferencesManager.put(PREFERENCE_XADES_POLICY_QUALIFIER, xadesPolicy.getPolicyQualifier().toString());
			}
			else {
				PreferencesManager.remove(PREFERENCE_XADES_POLICY_QUALIFIER);
			}
		}
		else {
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_IDENTIFIER);
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_HASH);
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_HASH_ALGORITHM);
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_QUALIFIER);
		}

		String multiSignValue;
		if (this.optionCoSign.isSelected()) {
			multiSignValue = PreferencesManager.VALUE_MULTISIGN_COSIGN;
		} else if (this.optionCounterSignLeafs.isSelected()) {
			multiSignValue = PreferencesManager.VALUE_MULTISIGN_COUNTERSIGN_LEAFS;
		} else {
			multiSignValue = PreferencesManager.VALUE_MULTISIGN_COUNTERSIGN_TREE;
		}
		PreferencesManager.put(PreferencesManager.PREFERENCE_XADES_MULTISIGN, multiSignValue);

		this.xadesPolicyDlg.saveCurrentPolicy();

	}

	void loadPreferences() {
		this.xadesSignatureProductionCity.setText(PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY));
		this.xadesSignatureProductionProvince.setText(
			PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE)
		);
		this.xadesSignatureProductionPostalCode.setText(
			PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE)
		);
		this.xadesSignatureProductionCountry.setText(
			PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY)
		);
		this.xadesSignerClaimedRole.setText(PreferencesManager.get(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE));

		final List<PolicyItem> xadesPolicies = new ArrayList<>();
        xadesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_XADES_AGE_1_9
    		)
		);

        this.panelPolicies.removeAll();

        final AdESPolicy currentPolicy = getXAdESPreferedPolicy();
        this.xadesPolicyDlg = new PolicyPanel(
    		SIGN_FORMAT_XADES,
    		xadesPolicies,
    		currentPolicy,
    		isBlocked()
		);

		final String previousSubFormat = PreferencesManager.get(PREFERENCE_XADES_SIGN_FORMAT);

		// Si la politica de firma es la de la AGE, eliminamos el formato Enveloping del listado,
		// ya que no esta soportado, y evitamos que se establezca este si era el que estaba configurado
        if (currentPolicy != null && AgePolicy.isAGEPolicy(currentPolicy.getPolicyIdentifier(), AOSignConstants.SIGN_FORMAT_XADES)) {
			this.xadesSignFormat.removeAllItems();
			this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED);
			this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
			if (AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(previousSubFormat) ||
					AOSignConstants.SIGN_FORMAT_XADES_DETACHED.equals(previousSubFormat)) {
				this.xadesSignFormat.setSelectedItem(previousSubFormat);
			}
		}
        else {
        	this.xadesSignFormat.setSelectedItem(previousSubFormat);
        }

        final String multiSign = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_MULTISIGN);
        if (multiSign != null) {
        	this.optionCoSign.setSelected(PreferencesManager.VALUE_MULTISIGN_COSIGN.equals(multiSign));
        	this.optionCounterSignLeafs.setSelected(PreferencesManager.VALUE_MULTISIGN_COUNTERSIGN_LEAFS.equals(multiSign));
        	this.optionCounterSignTree.setSelected(PreferencesManager.VALUE_MULTISIGN_COUNTERSIGN_TREE.equals(multiSign));
        }

        revalidate();
        repaint();
	}

	void restorePreferences() {

		// Eliminamos la configuracion actual
		PreferencesManager.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY);
		PreferencesManager.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE);
		PreferencesManager.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE);
		PreferencesManager.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY);
		PreferencesManager.remove(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE);

		this.xadesSignatureProductionCity.setText(
				PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY));
		this.xadesSignatureProductionProvince.setText(
				PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE));
		this.xadesSignatureProductionPostalCode.setText(
				PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE));
		this.xadesSignatureProductionCountry.setText(
				PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY));
		this.xadesSignerClaimedRole.setText(
				PreferencesManager.get(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE));

		// Solo establecemos la opcion por defecto si la interfaz no esta bloqueada
		if (!isBlocked()) {

			final List<PolicyItem> xadesPolicies = new ArrayList<>();
			xadesPolicies.add(
					new PolicyItem(
							SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
							POLICY_XADES_AGE_1_9
							)
					);

			this.xadesPolicyDlg = new PolicyPanel(
					SIGN_FORMAT_XADES,
					xadesPolicies,
					restorePolicy(),
					isBlocked()
					);
			this.currentPolicyValue.setText(this.xadesPolicyDlg.getSelectedPolicyName());

			PreferencesManager.remove(PREFERENCE_XADES_SIGN_FORMAT);
			final String subFormat = PreferencesManager.get(PREFERENCE_XADES_SIGN_FORMAT);

			final String policyId = PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER);

			if (AgePolicy.isAGEPolicy(policyId, AOSignConstants.SIGN_FORMAT_XADES)) {
				this.xadesSignFormat.removeAllItems();
				this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED);
				this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
				if (AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(subFormat)) {
					this.xadesSignFormat.setSelectedItem(subFormat);
				} else {
					this.xadesSignFormat.setSelectedItem(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
				}
			}
			else {
				this.xadesSignFormat.removeAllItems();
				this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED);
				this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING);
				this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
				this.xadesSignFormat.setSelectedItem(subFormat);
			}

			if (AgePolicy.isAGEPolicy(policyId, AOSignConstants.SIGN_FORMAT_XADES)) {
				this.xadesSignFormat.setSelectedItem(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
			}

			PreferencesManager.remove(PreferencesManager.PREFERENCE_XADES_MULTISIGN);
			final String multiSign = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_MULTISIGN);
			this.optionCoSign.setSelected(PreferencesManager.VALUE_MULTISIGN_COSIGN.equals(multiSign));
			this.optionCounterSignLeafs.setSelected(PreferencesManager.VALUE_MULTISIGN_COUNTERSIGN_LEAFS.equals(multiSign));
			this.optionCounterSignTree.setSelected(PreferencesManager.VALUE_MULTISIGN_COUNTERSIGN_TREE.equals(multiSign));
		}

        revalidate();
        repaint();
	}


	/** Obtiene la configuraci&oacute;n de politica de firma XAdES establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada. */
	private static AdESPolicy getXAdESPreferedPolicy() {

		AdESPolicy adesPolicy = null;

		final String policyIdentifier = PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER);
		if (policyIdentifier != null && !policyIdentifier.isEmpty()) {
			try {
				adesPolicy = new AdESPolicy(
						policyIdentifier,
						PreferencesManager.get(PREFERENCE_XADES_POLICY_HASH),
						PreferencesManager.get(PREFERENCE_XADES_POLICY_HASH_ALGORITHM),
						PreferencesManager.get(PREFERENCE_XADES_POLICY_QUALIFIER)
						);
			}
			catch (final Exception e) {
				LOGGER.severe("Error al recuperar la politica XAdES guardada en preferencias: " + e); //$NON-NLS-1$
			}
		}
		return adesPolicy;
	}

	/**
	 * Elimina la pol&iacute;tica establecida por el usuario y establece la definida a nivel de
	 * sistema o, si no se defini&oacute;, la por defecto.
	 * @return Pol&iacute;tica de firma configurada.
	 */
	private AdESPolicy restorePolicy() {

		AdESPolicy adesPolicy = null;

		loadXadesPolicy();

		// Si la interfaz esta bloqueada, devolvemos lo que ya esta seleccionado
		if (isBlocked()) {
			adesPolicy = this.xadesPolicyDlg.getSelectedPolicy();
		}
		// Si no, eliminamos la configuracion del usuario y obtenemos la del sistema o por defecto
		else {
			//
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_IDENTIFIER);
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_HASH);
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_HASH_ALGORITHM);
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_QUALIFIER);

			try {

				final String policyId = PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER);
				if (policyId != null && !policyId.isEmpty()) {
					adesPolicy = new AdESPolicy(policyId,
							PreferencesManager.get(PREFERENCE_XADES_POLICY_HASH),
							PreferencesManager.get(PREFERENCE_XADES_POLICY_HASH_ALGORITHM),
							PreferencesManager.get(PREFERENCE_XADES_POLICY_QUALIFIER));
				}
				this.xadesPolicyDlg.loadPolicy(adesPolicy);
			}
			catch (final Exception e) {
				LOGGER.severe("Error al recuperar la politica XAdES guardada en preferencias: " + e); //$NON-NLS-1$
			}
		}

		return adesPolicy;
	}

	void checkPreferences() throws AOException {

		loadXadesPolicy();

		final AdESPolicy p = this.xadesPolicyDlg.getSelectedPolicy();
		if (p != null) {
			// No nos interesa el resultado, solo si construye sin excepciones
			try {
				new URI(p.getPolicyIdentifier());
			}
			catch (final URISyntaxException e) {
				throw new AOException("El identificador debe ser una URI", e); //$NON-NLS-1$
			}
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


	/**
	 * Carga el panel de pol&iacute;tica con las preferencias guardadas
	 */
	private void loadXadesPolicy() {
		// Si el panel no esta cargado lo obtengo de las preferencias guardadas
		if (this.xadesPolicyDlg == null) {
			final List<PolicyItem> xadesPolicies = new ArrayList<>();
			xadesPolicies.add(
					new PolicyItem(SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
							POLICY_XADES_AGE_1_9));

			this.xadesPolicyDlg = new PolicyPanel(SIGN_FORMAT_XADES, xadesPolicies, getXAdESPreferedPolicy(), isBlocked());
		}
	}

	/**
	 * Di&aacute;logo para cambair la configuracion de la pol&iacute;tica
	 *
	 * @param container
	 *            Contenedor en el que se define el di&aacute;logo.
	 */
	public void changeXadesPolicyDlg(final Container container) {

		// Cargamos el dialogo
		loadXadesPolicy();

		final int confirmDialog = AOUIFactory.showConfirmDialog(container, this.xadesPolicyDlg,
				SimpleAfirmaMessages.getString("PolicyDialog.0"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION, JOptionPane.DEFAULT_OPTION);

		if (confirmDialog == JOptionPane.OK_OPTION) {

			try {
				checkPreferences();

				this.currentPolicyValue.setText(this.xadesPolicyDlg.getSelectedPolicyName());
				final AdESPolicy xadesPolicy = this.xadesPolicyDlg.getSelectedPolicy();
				if (xadesPolicy != null) {
					PreferencesManager.put(PREFERENCE_XADES_POLICY_IDENTIFIER, xadesPolicy.getPolicyIdentifier());
					PreferencesManager.put(PREFERENCE_XADES_POLICY_HASH, xadesPolicy.getPolicyIdentifierHash());
					PreferencesManager.put(PREFERENCE_XADES_POLICY_HASH_ALGORITHM, xadesPolicy.getPolicyIdentifierHashAlgorithm());
					if (xadesPolicy.getPolicyQualifier() != null) {
						PreferencesManager.put(PREFERENCE_XADES_POLICY_QUALIFIER, xadesPolicy.getPolicyQualifier().toString());
					} else {
						PreferencesManager.remove(PREFERENCE_XADES_POLICY_QUALIFIER);
					}

					// Si se ha establecido alguna de las politicas de firmas de la AGE, se
					// elimina el formato Enveloping del listado soportado
					if (AgePolicy.isAGEPolicy(xadesPolicy.getPolicyIdentifier(), AOSignConstants.SIGN_FORMAT_XADES)) {
						final String previousSubFormat = (String) this.xadesSignFormat.getSelectedItem();
						this.xadesSignFormat.removeAllItems();
						this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED);
						this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
						if (AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(previousSubFormat) ||
								AOSignConstants.SIGN_FORMAT_XADES_DETACHED.equals(previousSubFormat)) {
							this.xadesSignFormat.setSelectedItem(previousSubFormat);
						}
					}
					else {
						final String previousSubFormat = (String) this.xadesSignFormat.getSelectedItem();
						this.xadesSignFormat.removeAllItems();
						this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED);
						this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING);
						this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
						this.xadesSignFormat.setSelectedItem(previousSubFormat);
					}

				} else {
					PreferencesManager.remove(PREFERENCE_XADES_POLICY_IDENTIFIER);
					PreferencesManager.remove(PREFERENCE_XADES_POLICY_HASH);
					PreferencesManager.remove(PREFERENCE_XADES_POLICY_HASH_ALGORITHM);
					PreferencesManager.remove(PREFERENCE_XADES_POLICY_QUALIFIER);

					final String previousSubFormat = (String) this.xadesSignFormat.getSelectedItem();
					this.xadesSignFormat.removeAllItems();
					this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED);
					this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING);
					this.xadesSignFormat.addItem(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
					this.xadesSignFormat.setSelectedItem(previousSubFormat);
				}

				this.xadesPolicyDlg.saveCurrentPolicy();

			} catch (final Exception e) {

				AOUIFactory.showErrorMessage(
						"<p>" + SimpleAfirmaMessages.getString("PreferencesPanel.6") + "</p>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE, e);
				changeXadesPolicyDlg(container);

			}

		}

		// Siempre, tras cualquier operación limpio el panel
		this.xadesPolicyDlg = null;
	}

}
