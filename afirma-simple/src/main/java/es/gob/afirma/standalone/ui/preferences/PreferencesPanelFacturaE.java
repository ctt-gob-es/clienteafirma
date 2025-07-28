/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_FACTURAE_POLICY;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_FACTURAE_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH_ALGORITHM;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_FACTURAE_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_CITY;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_COUNTRY;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_POSTAL_CODE;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_PROVINCE;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_FACTURAE_SIGNER_ROLE;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
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
import es.gob.afirma.standalone.configurator.common.PreferencesManager;

/** Pesta&ntilde;a de configuraci&oacute;n de las preferencias de facturaE.
 * @author Mariano Mart&iacute;nez. */
final class PreferencesPanelFacturaE extends JScrollPane {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long serialVersionUID = 4299378019540627483L;

	private static final String FACTURAE_ROL_EMISOR = "Emisor"; //$NON-NLS-1$
	private static final String FACTURAE_ROL_RECEPTOR = "Receptor"; //$NON-NLS-1$
	private static final String FACTURAE_ROL_TERCERO = "Tercero"; //$NON-NLS-1$

	private static final String SIGN_FORMAT_FACTURAE = AOSignConstants.SIGN_FORMAT_FACTURAE;

	private static final String POLICY_FACTURAE_31_NAME = "3.1"; //$NON-NLS-1$
	private static final AdESPolicy POLICY_FACTURAE_31 = new AdESPolicy(
		"http://www.facturae.es/politica_de_firma_formato_facturae/politica_de_firma_formato_facturae_v3_1.pdf", //$NON-NLS-1$
		"Ohixl6upD6av8N7pEvDABhEL6hM=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		null
	);

	private static final String POLICY_FACTURAE_30_NAME = "3.0"; //$NON-NLS-1$
	private static final AdESPolicy POLICY_FACTURAE_30 = new AdESPolicy(
		"http://www.facturae.es/politica de firma formato facturae/politica de firma formato facturae v3_0.pdf", //$NON-NLS-1$
		"xmfh8D/Ec/hHeE1IB4zPd61zHIY=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		null
	);

	private final JTextField facturaeSignatureProductionCity = new JTextField();
	private final JTextField facturaeSignatureProductionProvince = new JTextField();
	private final JTextField facturaeSignatureProductionPostalCode = new JTextField();
	private final JTextField facturaeSignatureProductionCountry = new JTextField();

	private final JComboBox<Object> facturaeRol = new JComboBox<>(
		new Object[] {
			FACTURAE_ROL_EMISOR,
			FACTURAE_ROL_RECEPTOR,
			FACTURAE_ROL_TERCERO
		}
	);

	private final JPanel panelPolicies = new JPanel();
	private PolicyPanel facturaePolicyPanel;

	/**
	 * Atributo que permite gestionar el bloqueo de preferencias.
	 */
	private boolean blocked = true;

	PreferencesPanelFacturaE(final KeyListener keyListener,
							 final ModificationListener modificationListener,
							 final boolean blocked) {

		setBlocked(blocked);
		createUI(keyListener, modificationListener);
	}

	void createUI(final KeyListener keyListener,
				  final ModificationListener modificationListener) {

		getVerticalScrollBar().setUnitIncrement(16);

		final JPanel mainPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;

        loadPreferences();

        this.panelPolicies.setLayout(new GridBagLayout());
        this.panelPolicies.add(this.facturaePolicyPanel, gbc);

        gbc.gridy++;

        mainPanel.add(this.panelPolicies, gbc);

        this.facturaePolicyPanel.setModificationListener(modificationListener);
        this.facturaePolicyPanel.setKeyListener(keyListener);

        final JPanel metadata = new JPanel();
        metadata.setBorder(
    		BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.4") //$NON-NLS-1$
			)
		);
        metadata.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;

        final JLabel facturaeSignatureProductionCityLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.11") //$NON-NLS-1$
		);
        facturaeSignatureProductionCityLabel.setLabelFor(this.facturaeSignatureProductionCity);
        c.gridy++;
        metadata.add(facturaeSignatureProductionCityLabel, c);
        this.facturaeSignatureProductionCity.getAccessibleContext().setAccessibleName(
    		SimpleAfirmaMessages.getString("PreferencesPanel.181") //$NON-NLS-1$
		);
        this.facturaeSignatureProductionCity.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.66") //$NON-NLS-1$
		);
        this.facturaeSignatureProductionCity.addKeyListener(modificationListener);
        this.facturaeSignatureProductionCity.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.facturaeSignatureProductionCity, c);

        final JLabel facturaeSignatureProductionProvinceLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.14") //$NON-NLS-1$
		);
        facturaeSignatureProductionProvinceLabel.setLabelFor(this.facturaeSignatureProductionProvince);
        c.gridy++;
        metadata.add(facturaeSignatureProductionProvinceLabel, c);
        this.facturaeSignatureProductionProvince.getAccessibleContext().setAccessibleName(
    		SimpleAfirmaMessages.getString("PreferencesPanel.181") //$NON-NLS-1$
		);
        this.facturaeSignatureProductionProvince.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.68") //$NON-NLS-1$
		);
        this.facturaeSignatureProductionProvince.addKeyListener(modificationListener);
        this.facturaeSignatureProductionProvince.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.facturaeSignatureProductionProvince, c);

        final JLabel facturaeSignatureProductionPostalCodeLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.102") //$NON-NLS-1$
		);
        facturaeSignatureProductionPostalCodeLabel.setLabelFor(this.facturaeSignatureProductionPostalCode);
        c.gridy++;
        metadata.add(facturaeSignatureProductionPostalCodeLabel, c);
        this.facturaeSignatureProductionPostalCode.getAccessibleContext().setAccessibleName(
    		SimpleAfirmaMessages.getString("PreferencesPanel.181") //$NON-NLS-1$
		);
        this.facturaeSignatureProductionPostalCode.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.102") //$NON-NLS-1$
		);
        this.facturaeSignatureProductionPostalCode.addKeyListener(modificationListener);
        this.facturaeSignatureProductionPostalCode.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.facturaeSignatureProductionPostalCode, c);

        final JLabel facturaeSignatureProductionCountryLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.12") //$NON-NLS-1$
		);
        facturaeSignatureProductionCountryLabel.setLabelFor(this.facturaeSignatureProductionCountry);
        c.gridy++;
        metadata.add(facturaeSignatureProductionCountryLabel, c);
        this.facturaeSignatureProductionCountry.getAccessibleContext().setAccessibleName(
    		SimpleAfirmaMessages.getString("PreferencesPanel.181") //$NON-NLS-1$
		);
        this.facturaeSignatureProductionCountry.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.67") //$NON-NLS-1$
		);
        this.facturaeSignatureProductionCountry.addKeyListener(modificationListener);
        this.facturaeSignatureProductionCountry.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.facturaeSignatureProductionCountry, c);

	    final JPanel signOptions = new JPanel(new FlowLayout(FlowLayout.LEADING));
        signOptions.setBorder(BorderFactory.createTitledBorder(
			SimpleAfirmaMessages.getString("PreferencesPanel.69")) //$NON-NLS-1$
		);

        final JPanel signOptionsInnerPanel = new JPanel(new GridBagLayout());

        this.facturaeRol.getAccessibleContext().setAccessibleName(
    		SimpleAfirmaMessages.getString("PreferencesPanel.183") //$NON-NLS-1$
		);
        this.facturaeRol.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.53") //$NON-NLS-1$
		);
        this.facturaeRol.addItemListener(modificationListener);

        final JLabel facturaeRolLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.3") //$NON-NLS-1$
		);
        facturaeRolLabel.setLabelFor(this.facturaeRol);


        final GridBagConstraints cf = new GridBagConstraints();
        cf.anchor = GridBagConstraints.LINE_START;
        cf.insets = new Insets(0, 7, 4, 7);
        cf.gridx = 0;
        cf.gridy = 0;
        signOptionsInnerPanel.add(facturaeRolLabel, cf);
        cf.gridx++;
        signOptionsInnerPanel.add(this.facturaeRol, cf);

        signOptions.add(signOptionsInnerPanel);

        gbc.gridy++;
        mainPanel.add(signOptions, gbc);

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
			if (AOUIFactory.showConfirmDialog(getParent(), SimpleAfirmaMessages.getString("PreferencesPanel.158"), //$NON-NLS-1$
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

	/** Guarda las preferencias de FacturaE. */
	void savePreferences() {

		PreferencesManager.put(PREFERENCE_FACTURAE_SIGNER_ROLE, this.facturaeRol.getSelectedItem().toString());

		PreferencesManager.put(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_CITY, this.facturaeSignatureProductionCity.getText());
		PreferencesManager.put(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_PROVINCE, this.facturaeSignatureProductionProvince.getText());
		PreferencesManager.put(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_POSTAL_CODE, this.facturaeSignatureProductionPostalCode.getText());
		PreferencesManager.put(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_COUNTRY, this.facturaeSignatureProductionCountry.getText());

		final AdESPolicy facturaePolicy = this.facturaePolicyPanel.getSelectedPolicy();
		if (facturaePolicy != null) {
			if (this.facturaePolicyPanel.getCurrentPolicyItem().toString().equals(SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.1"))) { //$NON-NLS-1$
					PreferencesManager.put(PreferencesManager.PREFERENCE_FACTURAE_POLICY, POLICY_FACTURAE_30_NAME);
			}
			else if (this.facturaePolicyPanel.getCurrentPolicyItem().toString().equals(SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.2"))) { //$NON-NLS-1$
				PreferencesManager.put(PreferencesManager.PREFERENCE_FACTURAE_POLICY, POLICY_FACTURAE_31_NAME);
			}
			PreferencesManager.put(PREFERENCE_FACTURAE_POLICY_IDENTIFIER, facturaePolicy.getPolicyIdentifier());
			PreferencesManager.put(PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH, facturaePolicy.getPolicyIdentifierHash());
			PreferencesManager.put(PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH_ALGORITHM, facturaePolicy.getPolicyIdentifierHashAlgorithm());
			if (facturaePolicy.getPolicyQualifier() != null) {
				PreferencesManager.put(PREFERENCE_FACTURAE_POLICY_QUALIFIER, facturaePolicy.getPolicyQualifier().toString());
			}
			else {
				PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_QUALIFIER);
			}
		}
		else {
			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_IDENTIFIER);
			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH);
			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_QUALIFIER);
		}
		this.facturaePolicyPanel.saveCurrentPolicy();
	}

	void loadPreferences() {
		this.facturaeRol.setSelectedItem(
			PreferencesManager.get(PREFERENCE_FACTURAE_SIGNER_ROLE)
		);

		this.facturaeSignatureProductionCity.setText(
			PreferencesManager.get(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_CITY)
		);

		this.facturaeSignatureProductionProvince.setText(
			PreferencesManager.get(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_PROVINCE)
		);

		this.facturaeSignatureProductionPostalCode.setText(
			PreferencesManager.get(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_POSTAL_CODE)
		);

		this.facturaeSignatureProductionCountry.setText(
			PreferencesManager.get(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_COUNTRY)
		);
		final List<PolicyItem> facturaePolicies = new ArrayList<>();

		facturaePolicies.add(
    		new PolicyItem(
    			SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.2"), //$NON-NLS-1$
        		POLICY_FACTURAE_31
    		)
		);

		facturaePolicies.add(
    		new PolicyItem(
    			SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.1"), //$NON-NLS-1$
        		POLICY_FACTURAE_30
    		)
		);

        this.panelPolicies.removeAll();
        this.facturaePolicyPanel = new PolicyPanel(
    		SIGN_FORMAT_FACTURAE,
    		facturaePolicies,
    		getFacturaEPreferedPolicy(),
    		false,
    		false,
    		false,
    		isBlocked()
		);

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;
        this.panelPolicies.add(this.facturaePolicyPanel, c);
        revalidate();
        repaint();

	}

	void restorePreferences() {

		// Eliminamos la configuracion actual
		PreferencesManager.remove(PREFERENCE_FACTURAE_SIGNER_ROLE);
		PreferencesManager.remove(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_CITY);
		PreferencesManager.remove(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_PROVINCE);
		PreferencesManager.remove(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_POSTAL_CODE);
		PreferencesManager.remove(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_COUNTRY);

		// Establecemos la configuracion (que sera la del sistema o la por defecto)

		this.facturaeRol.setSelectedItem(
			PreferencesManager.get(PREFERENCE_FACTURAE_SIGNER_ROLE)
		);

		this.facturaeSignatureProductionCity.setText(
			PreferencesManager.get(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_CITY)
		);

		this.facturaeSignatureProductionProvince.setText(
			PreferencesManager.get(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_PROVINCE)
		);

		this.facturaeSignatureProductionPostalCode.setText(
			PreferencesManager.get(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_POSTAL_CODE)
		);

		this.facturaeSignatureProductionCountry.setText(
			PreferencesManager.get(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_COUNTRY)
		);

		if (!isBlocked()) {

			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY);
			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_IDENTIFIER);
			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH);
			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_QUALIFIER);

			final List<PolicyItem> facturaePolicies = new ArrayList<>();
			facturaePolicies.add(
					new PolicyItem(
							SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.2"), //$NON-NLS-1$
							POLICY_FACTURAE_31
							)
					);
			facturaePolicies.add(
					new PolicyItem(
							SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.1"), //$NON-NLS-1$
							POLICY_FACTURAE_30
							)
					);

			this.panelPolicies.removeAll();
			this.facturaePolicyPanel = new PolicyPanel(
					SIGN_FORMAT_FACTURAE,
					facturaePolicies,
					getFacturaEDefaultPolicy(),
					false,
					false,
					false,
					isBlocked()
					);

			final GridBagConstraints c = new GridBagConstraints();
			c.fill = GridBagConstraints.BOTH;
			c.weightx = 1.0;
			c.gridy = 0;
			this.panelPolicies.add(this.facturaePolicyPanel, c);
		}

        revalidate();
        repaint();

	}

	/** Obtiene la configuraci&oacute;n de politica de firma FacturaE establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada. */
	private static AdESPolicy getFacturaEPreferedPolicy() {

		final String policy = PreferencesManager.get(PreferencesManager.PREFERENCE_FACTURAE_POLICY);
		if (policy.equals(POLICY_FACTURAE_30_NAME)) {
			return POLICY_FACTURAE_30;
		}
		else if (policy.equals(POLICY_FACTURAE_31_NAME)) {
			return POLICY_FACTURAE_31;
		}
		return null;
	}

	/** Obtiene la configuraci&oacute;n de politica de firma FacturaE establecida por defecto.
	 * @return Pol&iacute;tica de firma configurada. */
	private AdESPolicy getFacturaEDefaultPolicy() {

		AdESPolicy adesPolicy = null;

		// Si la interfaz esta bloqueada, establecemos el valor que estuviese definido
		if (isBlocked()) {
			adesPolicy = this.facturaePolicyPanel.getSelectedPolicy();
		}
		// Si no, establecemos la configuracion por defecto
		else {
			final String policy = PreferencesManager.get(PreferencesManager.PREFERENCE_FACTURAE_POLICY);
			if (policy.equals(POLICY_FACTURAE_30_NAME)) {
				adesPolicy = POLICY_FACTURAE_30;
			}
			else if (policy.equals(POLICY_FACTURAE_31_NAME)) {
				adesPolicy = POLICY_FACTURAE_31;
			}
		}

		return adesPolicy;
	}

	void checkPreferences() throws AOException {
		final AdESPolicy p = this.facturaePolicyPanel.getSelectedPolicy();
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
