/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_POLICY_HASH;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_POLICY_HASH_ALGORITHM;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGNER_CLAIMED_ROLE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGN_FORMAT;

import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyListener;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.preferences.PolicyPanel.PolicyItem;

final class PreferencesPanelXades extends JPanel {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long serialVersionUID = 1424468022677956783L;

	private static final String SIGN_FORMAT_XADES = "XAdES"; //$NON-NLS-1$

	private static final AdESPolicy POLICY_XADES_AGE_1_9 = new AdESPolicy(
		"urn:oid:2.16.724.1.3.1.1.2.1.9", //$NON-NLS-1$
		"G7roucf600+f03r/o0bAOQ6WAs0=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"https://sede.060.gob.es/politica_de_firma_anexo_1.pdf" //$NON-NLS-1$
	);

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

	private final JPanel panelPolicies = new JPanel();

	private PolicyPanel xadesPolicyDlg;

	/**
	 * Atributo que representa la etiqueta de la pol&iacute;tica seleccionada en
	 * el di&aacute;logo
	 */
	private JLabel policyLabel;

	/**
	 * Atributo que permite gestionar el bloqueo de preferencias.
	 */
	private boolean blocked = true;

	PreferencesPanelXades(final KeyListener keyListener,
						  final ModificationListener modificationListener,
						  final boolean blocked) {

		this.blocked = blocked;
		createUI(keyListener, modificationListener);
	}

	void createUI(final KeyListener keyListener,
				  final ModificationListener modificationListener
				  ) {

        setLayout(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;

        loadXadesPolicy();

        loadPreferences();

    	this.xadesPolicyDlg.setModificationListener(modificationListener);
    	this.xadesPolicyDlg.setKeyListener(keyListener);

        ///////////// Panel Policy ////////////////

        final JPanel policyConfigPanel = new JPanel(new FlowLayout(FlowLayout.LEADING));
		policyConfigPanel.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanel.153")) //$NON-NLS-1$
			)
		);

		final JButton policyConfigButton = new JButton(
			SimpleAfirmaMessages.getString("PreferencesPanel.150") //$NON-NLS-1$
		);

		this.policyLabel = new JLabel(this.xadesPolicyDlg.getSelectedPolicyName());
		this.policyLabel.setLabelFor(policyConfigButton);

		policyConfigButton.setMnemonic('P');
		policyConfigButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					changeXadesPolicyDlg(getParent());
				}
			}
		);
		policyConfigButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.151") //$NON-NLS-1$
		);

		policyConfigButton.setEnabled(!this.blocked);
		policyConfigPanel.add(this.policyLabel);
		policyConfigPanel.add(policyConfigButton);

        ///////////// Fin Panel Policy ////////////////

		gbc.gridy++;

        add(policyConfigPanel, gbc);

        final JPanel metadata = new JPanel();
        metadata.setBorder(BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanel.8"))); //$NON-NLS-1$
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
        this.xadesSignatureProductionPostalCode.addKeyListener(modificationListener);
        this.xadesSignatureProductionPostalCode.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignatureProductionPostalCode, c);

        final JLabel xadesSignatureProductionCountryLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.12")); //$NON-NLS-1$
        xadesSignatureProductionCountryLabel.setLabelFor(this.xadesSignatureProductionCountry);
        c.gridy++;
        metadata.add(xadesSignatureProductionCountryLabel, c);
        this.xadesSignatureProductionCountry.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.67")); //$NON-NLS-1$
        this.xadesSignatureProductionCountry.addKeyListener(modificationListener);
        this.xadesSignatureProductionCountry.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignatureProductionCountry, c);

        final JLabel xadesSignerCertifiedRoleLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanelXades.0")); //$NON-NLS-1$
        xadesSignerCertifiedRoleLabel.setLabelFor(this.xadesSignerClaimedRole);
        c.gridy++;
        metadata.add(xadesSignerCertifiedRoleLabel, c);
        this.xadesSignerClaimedRole.addKeyListener(modificationListener);
        this.xadesSignerClaimedRole.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignerClaimedRole, c);

        final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
	    final JPanel signOptions = new JPanel(fLayout);
        signOptions.setBorder(BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.69")) //$NON-NLS-1$
			)
		);

        final JPanel panelFirm = new JPanel();
		panelFirm.setBorder(
    		BorderFactory.createEmptyBorder()
		);
		panelFirm.setLayout(new GridBagLayout());

        final GridBagConstraints cf = new GridBagConstraints();
        cf.fill = GridBagConstraints.HORIZONTAL;
        cf.weightx = 1.0;

        this.xadesSignFormat.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.53")); //$NON-NLS-1$
        this.xadesSignFormat.addItemListener(modificationListener);
        this.xadesSignFormat.addKeyListener(keyListener);
        this.xadesSignFormat.setEnabled(!this.blocked);

        final JLabel xadesFormatLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanel.15") //$NON-NLS-1$
		);
        xadesFormatLabel.addKeyListener(keyListener);
        xadesFormatLabel.setLabelFor(this.xadesSignFormat);

        panelFirm.add(xadesFormatLabel, cf);
        cf.gridy++;
        cf.gridy++;
        panelFirm.add(this.xadesSignFormat, cf);
        signOptions.add(panelFirm);

        gbc.gridy++;
        add(metadata, gbc);

        gbc.gridy++;
        add(signOptions, gbc);

        gbc.gridy++;
        gbc.weighty = 1.0;
        add(new JPanel(), gbc);

		// Panel para el boton de restaurar la configuracion
		final JPanel panelGeneral = new JPanel(new FlowLayout(FlowLayout.TRAILING));

		final JButton restoreConfigButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.147") //$NON-NLS-1$
		);

		restoreConfigButton.setMnemonic('R');
		restoreConfigButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (AOUIFactory.showConfirmDialog(getParent(), SimpleAfirmaMessages.getString("PreferencesPanel.156"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PreferencesPanel.139"), //$NON-NLS-1$
						JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {

					loadDefaultPreferences();

				}
			}
		});
		restoreConfigButton.getAccessibleContext()
				.setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.136") //$NON-NLS-1$
		);

		gbc.gridy++;
		gbc.weighty = 0.0;
		panelGeneral.add(restoreConfigButton, gbc);

		gbc.gridy++;

		add(panelGeneral, gbc);

	}

	void savePreferences() {
		PreferencesManager.put(PREFERENCE_XADES_SIGN_FORMAT, this.xadesSignFormat.getSelectedItem().toString());

		if ("".equals(this.xadesSignatureProductionCity.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY);
		}
		else {
			PreferencesManager.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, this.xadesSignatureProductionCity.getText());
		}
		if ("".equals(this.xadesSignatureProductionCountry.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY);
		}
		else {
			PreferencesManager.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, this.xadesSignatureProductionCountry.getText());
		}
		if ("".equals(this.xadesSignatureProductionPostalCode.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE);
		}
		else {
			PreferencesManager.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, this.xadesSignatureProductionPostalCode.getText());
		}
		if ("".equals(this.xadesSignatureProductionProvince.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE);
		}
		else {
			PreferencesManager.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, this.xadesSignatureProductionProvince.getText());
		}
		if ("".equals(this.xadesSignerClaimedRole.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE);
		}
		else {
			PreferencesManager.put(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, this.xadesSignerClaimedRole.getText());
		}
		if ("".equals(this.xadesSignerClaimedRole.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE);
		}
		else {
			PreferencesManager.put(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, this.xadesSignerClaimedRole.getText());
		}

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

		final List<PolicyPanel.PolicyItem> xadesPolicies = new ArrayList<>();
        xadesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_XADES_AGE_1_9
    		)
		);

        this.panelPolicies.removeAll();

        final AdESPolicy currentPolicy = getXadesPreferedPolicy();
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

        revalidate();
        repaint();
	}

	void loadDefaultPreferences() {
		this.xadesSignatureProductionCity.setText(PreferencesManager.getDefaultPreference(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY));
		this.xadesSignatureProductionProvince.setText(
			PreferencesManager.getDefaultPreference(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE)
		);
		this.xadesSignatureProductionPostalCode.setText(
			PreferencesManager.getDefaultPreference(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE)
		);
		this.xadesSignatureProductionCountry.setText(
			PreferencesManager.getDefaultPreference(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY)
		);
		this.xadesSignerClaimedRole.setText(PreferencesManager.getDefaultPreference(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE));

		// unprotected: true -> no puedo modificarla, cargo la que estaba
		if (isBlocked()) {
			this.xadesSignFormat.setSelectedItem(PreferencesManager.getDefaultPreference(PREFERENCE_XADES_SIGN_FORMAT));
		}

		final List<PolicyPanel.PolicyItem> xadesPolicies = new ArrayList<>();
        xadesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_XADES_AGE_1_9
    		)
		);

        this.xadesPolicyDlg = new PolicyPanel(
    		SIGN_FORMAT_XADES,
    		xadesPolicies,
    		getXadesDefaultPolicy(),
    		isBlocked()
		);

        this.xadesSignFormat.setSelectedItem(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
        this.xadesSignFormat.setEnabled(!this.blocked);

		this.policyLabel.setText(this.xadesPolicyDlg.getSelectedPolicyName());

        revalidate();
        repaint();
	}


	/** Obtiene la configuraci&oacute;n de politica de firma XAdES establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada. */
	private static AdESPolicy getXadesPreferedPolicy() {

		if (PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER) == null ||
				PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER).isEmpty()) {
			return null;
		}
		try {
			return new AdESPolicy(
				PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER),
				PreferencesManager.get(PREFERENCE_XADES_POLICY_HASH),
				PreferencesManager.get(PREFERENCE_XADES_POLICY_HASH_ALGORITHM),
				PreferencesManager.get(PREFERENCE_XADES_POLICY_QUALIFIER)
				);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al recuperar la politica XAdES guardada en preferencias: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
	}

	/** Obtiene la configuraci&oacute;n de politica de firma XAdES establecida por defecto.
	 * @return Pol&iacute;tica de firma configurada. */
	private AdESPolicy getXadesDefaultPolicy() {

		AdESPolicy adesPolicy = null;

		loadXadesPolicy();

		if (isBlocked()) {

			// unprotected = true, luego no pueden alterarse las
			// propiedades:
			// devolvemos las preferencias almacenadas actualmente

			adesPolicy = this.xadesPolicyDlg.getSelectedPolicy();

		} else {

			// unprotected = false, luego se pueden alterar las propiedades:
			// devolvemos las preferencias por defecto
			try {

				if (PreferencesManager.getDefaultPreference(PREFERENCE_XADES_POLICY_IDENTIFIER) == null
						|| PreferencesManager.getDefaultPreference(PREFERENCE_XADES_POLICY_IDENTIFIER).isEmpty()) {
					this.xadesPolicyDlg.loadPolicy(null);
				} else {

					this.xadesPolicyDlg
							.loadPolicy(new AdESPolicy(PreferencesManager.getDefaultPreference(PREFERENCE_XADES_POLICY_IDENTIFIER),
									PreferencesManager.getDefaultPreference(PREFERENCE_XADES_POLICY_HASH),
									PreferencesManager.getDefaultPreference(PREFERENCE_XADES_POLICY_HASH_ALGORITHM),
									PreferencesManager.getDefaultPreference(PREFERENCE_XADES_POLICY_QUALIFIER)));
				}
			} catch (final Exception e) {
				Logger.getLogger("es.gob.afirma") //$NON-NLS-1$
						.severe("Error al recuperar la politica XAdES guardada en preferencias: " + e); //$NON-NLS-1$

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
	public void setBlocked(boolean blocked) {
		this.blocked = blocked;
	}


	/**
	 * Carga el panel de pol&iacute;tica con las preferencias guardadas
	 */
	private void loadXadesPolicy() {
		// Si el panel no esta cargado lo obtengo de las preferencias guardadas
		if (this.xadesPolicyDlg == null) {
			final List<PolicyPanel.PolicyItem> xadesPolicies = new ArrayList<>();
			xadesPolicies.add(new PolicyItem(SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
					POLICY_XADES_AGE_1_9));

			this.xadesPolicyDlg = new PolicyPanel(SIGN_FORMAT_XADES, xadesPolicies, getXadesPreferedPolicy(), isBlocked());
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

		if (AOUIFactory.showConfirmDialog(container, this.xadesPolicyDlg,
				SimpleAfirmaMessages.getString("PolicyDialog.0"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION, JOptionPane.DEFAULT_OPTION) == JOptionPane.OK_OPTION) {

			try {
				checkPreferences();

				this.policyLabel.setText(this.xadesPolicyDlg.getSelectedPolicyName());
				final AdESPolicy xadesPolicy = this.xadesPolicyDlg.getSelectedPolicy();

				if (xadesPolicy != null) {
					PreferencesManager.put(PREFERENCE_XADES_POLICY_IDENTIFIER, xadesPolicy.getPolicyIdentifier());
					PreferencesManager.put(PREFERENCE_XADES_POLICY_HASH,
							xadesPolicy.getPolicyIdentifierHash());
					PreferencesManager.put(PREFERENCE_XADES_POLICY_HASH_ALGORITHM,
							xadesPolicy.getPolicyIdentifierHashAlgorithm());
					if (xadesPolicy.getPolicyQualifier() != null) {
						PreferencesManager.put(PREFERENCE_XADES_POLICY_QUALIFIER,
								xadesPolicy.getPolicyQualifier().toString());
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

				AOUIFactory.showErrorMessage(this,
						"<html><p>" + SimpleAfirmaMessages.getString("PreferencesPanel.6") + ":<br>" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								+ e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE);
				changeXadesPolicyDlg(container);

			}

		}

		// Siempre, tras cualquier operación limpio el panel
		this.xadesPolicyDlg = null;
	}

}
