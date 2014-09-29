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

import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_CADES_IMPLICIT;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_CADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_CADES_POLICY_IDENTIFIER_HASH;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_CADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_OMIT_ASKONCLOSE;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_FORMAT;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_SIGNER_CONTACT;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_SIGN_PRODUCTION_CITY;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_SIGN_REASON;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_SIGNATURE_ALGORITHM;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGNER_CERTIFIED_ROLE;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGNER_CLAIMED_ROLE;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGN_FORMAT;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
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

import org.ietf.jgss.GSSException;
import org.ietf.jgss.Oid;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.PolicyPanel.PolicyItem;

final class PreferencesPanel extends JPanel implements KeyListener {

	private static final long serialVersionUID = -3168095095548385291L;

	private static final Preferences PREFERENCES = Preferences.userRoot();

	private static final String SIGN_FORMAT_CADES = "CAdES"; //$NON-NLS-1$
	private static final String SIGN_FORMAT_XADES = "XAdES"; //$NON-NLS-1$
	private static final String SIGN_FORMAT_PADES = "PAdES"; //$NON-NLS-1$

	private static final AdESPolicy POLICY_CADES_AGE_1_8 = new AdESPolicy(
		"2.16.724.1.3.1.1.2.1.8", //$NON-NLS-1$
		"7SxX3erFuH31TvAw9LZ70N7p1vA=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf" //$NON-NLS-1$
	);

	private static final AdESPolicy POLICY_XADES_AGE_1_8 = new AdESPolicy(
		"urn:oid:2.16.724.1.3.1.1.2.1.8", //$NON-NLS-1$
		"V8lVVNGDCPen6VELRD1Ja8HARFk=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf" //$NON-NLS-1$
	);

	private static final AdESPolicy POLICY_CADES_PADES_AGE_1_9 = new AdESPolicy(
		"2.16.724.1.3.1.1.2.1.9", //$NON-NLS-1$
		"G7roucf600+f03r/o0bAOQ6WAs0=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"https://sede.060.gob.es/politica_de_firma_anexo_1.pdf" //$NON-NLS-1$
	);

	private static final AdESPolicy POLICY_XADES_AGE_1_9 = new AdESPolicy(
		"urn:oid:2.16.724.1.3.1.1.2.1.9", //$NON-NLS-1$
		"G7roucf600+f03r/o0bAOQ6WAs0=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"https://sede.060.gob.es/politica_de_firma_anexo_1.pdf" //$NON-NLS-1$
	);

	private static final String PADES_FORMAT_BASIC_TEXT = SimpleAfirmaMessages.getString("PreferencesPanel.71"); //$NON-NLS-1$
	private static final String PADES_FORMAT_BES_TEXT = SimpleAfirmaMessages.getString("PreferencesPanel.72"); //$NON-NLS-1$

	private final JComboBox signarureAlgorithms = new JComboBox();

	private final JButton applyButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.0")); //$NON-NLS-1$

	private final ModificationListener modificationListener;

	private PolicyPanel cadesPolicyPanel;
	private PolicyPanel xadesPolicyPanel;
	private PolicyPanel padesPolicyPanel;

	private final JCheckBox avoidAskForClose = new JCheckBox(
		SimpleAfirmaMessages.getString("PreferencesPanel.36"), //$NON-NLS-1$
		PREFERENCES.getBoolean(PREFERENCE_OMIT_ASKONCLOSE, false)
	);

	private final JTextField padesSignReason = new JTextField(
		PREFERENCES.get(PREFERENCE_PADES_SIGN_REASON, "") //$NON-NLS-1$
	);
	private final JTextField padesSignProductionCity = new JTextField(
		PREFERENCES.get(PREFERENCE_PADES_SIGN_PRODUCTION_CITY, "") //$NON-NLS-1$
	);
	private final JTextField padesSignerContact = new JTextField(
		PREFERENCES.get(PREFERENCE_PADES_SIGNER_CONTACT, "") //$NON-NLS-1$
	);

    private final Window window;
    Window getParentWindow() {
        return this.window;
    }

	private final JComboBox padesBasicFormat = new JComboBox();
	JComboBox getBasicPadesFormat() {
		return this.padesBasicFormat;
	}

	private final JCheckBox cadesImplicit = new JCheckBox(
		SimpleAfirmaMessages.getString("PreferencesPanel.1"), //$NON-NLS-1$
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
	private final JComboBox xadesSignFormat = new JComboBox(
		new String[] {
	      AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED,
		  AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
	      AOSignConstants.SIGN_FORMAT_XADES_DETACHED
		}
	);

	private final JTabbedPane tabbedPane = new JTabbedPane();

	void createUI() {
		this.tabbedPane.addKeyListener(this);
		this.tabbedPane.addTab(SimpleAfirmaMessages.getString("PreferencesPanel.2"), null, createGeneralPanel(), SimpleAfirmaMessages.getString("PreferencesPanel.40")); //$NON-NLS-1$ //$NON-NLS-2$
		this.tabbedPane.addTab(SimpleAfirmaMessages.getString("PreferencesPanel.3"), null, createPadesPanel(), SimpleAfirmaMessages.getString("PreferencesPanel.41")); //$NON-NLS-1$ //$NON-NLS-2$
		this.tabbedPane.addTab(SimpleAfirmaMessages.getString("PreferencesPanel.4"), null, createCadesPanel(), SimpleAfirmaMessages.getString("PreferencesPanel.42")); //$NON-NLS-1$ //$NON-NLS-2$
		this.tabbedPane.addTab(SimpleAfirmaMessages.getString("PreferencesPanel.5"), null, createXadesPanel(), SimpleAfirmaMessages.getString("PreferencesPanel.43")); //$NON-NLS-1$ //$NON-NLS-2$

		this.setLayout(new GridBagLayout());

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.gridy = 0;

		add(this.tabbedPane, c);
		c.gridy++;
		c.weighty = 1.0;
		add(new JPanel(), c); // Relleno en blanco
		c.gridy++;
		c.weighty = 0.0;
		c.ipady = 11;
		add(createButtonsPanel(), c);
	}

    boolean savePreferences() {

		if (!checkPreferences()) {
			return false;
		}

		//****************************************************************************
		//**** PREFERENCIAS GENERALES ************************************************
		//****************************************************************************
		PreferencesPanel.PREFERENCES.put(PREFERENCE_SIGNATURE_ALGORITHM, this.signarureAlgorithms.getSelectedItem().toString());
		PreferencesPanel.PREFERENCES.putBoolean(PREFERENCE_OMIT_ASKONCLOSE, this.avoidAskForClose.isSelected());

		//****************************************************************************
		//**** PREFERENCIAS CADES ****************************************************
		//****************************************************************************
		PreferencesPanel.PREFERENCES.put(PREFERENCE_CADES_IMPLICIT, Boolean.valueOf(this.cadesImplicit.isSelected()).toString());
		final AdESPolicy cadesPolicy = this.cadesPolicyPanel.getCurrentPolicy();
		if (cadesPolicy != null) {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_CADES_POLICY_IDENTIFIER, cadesPolicy.getPolicyIdentifier());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_CADES_POLICY_IDENTIFIER_HASH, cadesPolicy.getPolicyIdentifierHash());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM, cadesPolicy.getPolicyIdentifierHashAlgorithm());
			if (cadesPolicy.getPolicyQualifier() != null) {
				PreferencesPanel.PREFERENCES.put(PREFERENCE_CADES_POLICY_QUALIFIER, cadesPolicy.getPolicyQualifier().toString());
			}
			else {
				PreferencesPanel.PREFERENCES.remove(PREFERENCE_CADES_POLICY_QUALIFIER);
			}
		}
		else {
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_CADES_POLICY_IDENTIFIER);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_CADES_POLICY_IDENTIFIER_HASH);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_CADES_POLICY_QUALIFIER);
		}
		this.cadesPolicyPanel.saveCurrentPolicy();

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

		PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_FORMAT, ((ValueTextPair) this.padesBasicFormat.getSelectedItem()).getValue());

		final AdESPolicy padesPolicy = this.padesPolicyPanel.getCurrentPolicy();
		if (padesPolicy != null) {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_POLICY_IDENTIFIER, padesPolicy.getPolicyIdentifier());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH, padesPolicy.getPolicyIdentifierHash());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM, padesPolicy.getPolicyIdentifierHashAlgorithm());
			if (padesPolicy.getPolicyQualifier() != null) {
				PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_POLICY_QUALIFIER, padesPolicy.getPolicyQualifier().toString());
			}
			else {
				PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_POLICY_QUALIFIER);
			}
		}
		else {
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_POLICY_IDENTIFIER);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_POLICY_QUALIFIER);
		}
		this.padesPolicyPanel.saveCurrentPolicy();

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

		final AdESPolicy xadesPolicy = this.xadesPolicyPanel.getCurrentPolicy();
		if (xadesPolicy != null) {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_POLICY_IDENTIFIER, xadesPolicy.getPolicyIdentifier());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, xadesPolicy.getPolicyIdentifierHash());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, xadesPolicy.getPolicyIdentifierHashAlgorithm());
			if (xadesPolicy.getPolicyQualifier() != null) {
				PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_POLICY_QUALIFIER, xadesPolicy.getPolicyQualifier().toString());
			}
			else {
				PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_POLICY_QUALIFIER);
			}
		}
		else {
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_POLICY_IDENTIFIER);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_POLICY_QUALIFIER);
		}
		this.xadesPolicyPanel.saveCurrentPolicy();

	    return true;

	}

	/** Comprueba que los datos configurados sean v&aacute;lidos.
	 * @return {@code true} cuando los datos son v&aacute;lidos, {@code false} en caso
	 * contrario. */
	@SuppressWarnings("unused")
	private boolean checkPreferences() {
		try {
			this.xadesPolicyPanel.getCurrentPolicy();
		}
		catch(final Exception e) {
			AOUIFactory.showErrorMessage(
				this,
				"<html><p>" + SimpleAfirmaMessages.getString("PreferencesPanel.6") + ":<br>" + e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			this.tabbedPane.setSelectedIndex(3);
			return false;
		}

		try {
			final AdESPolicy p = this.padesPolicyPanel.getCurrentPolicy();
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
		catch(final Exception e) {
			AOUIFactory.showErrorMessage(
				this,
				"<html><p>" + SimpleAfirmaMessages.getString("PreferencesPanel.7") + ":<br>" + e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			this.tabbedPane.setSelectedIndex(1);
			return false;
		}

		try {
			final AdESPolicy p = this.cadesPolicyPanel.getCurrentPolicy();
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
		catch(final Exception e) {
			AOUIFactory.showErrorMessage(
				this,
				"<html><p>" + SimpleAfirmaMessages.getString("PreferencesPanel.38") + ":<br>" + e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			this.tabbedPane.setSelectedIndex(2);
			return false;
		}

		return true;
	}

	private JPanel createXadesPanel() {
        final JPanel panel = new JPanel();
        panel.setLayout(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;

        final List<PolicyPanel.PolicyItem> xadesPolicies = new ArrayList<PolicyPanel.PolicyItem>();
        xadesPolicies.add(
    		new PolicyPanel.PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.25"), //$NON-NLS-1$
        		POLICY_XADES_AGE_1_8
    		)
		);
        xadesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_XADES_AGE_1_9
    		)
		);

        this.xadesPolicyPanel = new PolicyPanel(
    		SIGN_FORMAT_XADES,
    		xadesPolicies,
    		getXadesPreferedPolicy(),
    		this.xadesSignFormat
		);
        this.xadesPolicyPanel.setModificationListener(this.modificationListener);
        this.xadesPolicyPanel.setKeyListener(this);
        panel.add(this.xadesPolicyPanel, gbc);

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
        this.xadesSignatureProductionCity.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.66")); //$NON-NLS-1$
        this.xadesSignatureProductionCity.addKeyListener(this.modificationListener);
        this.xadesSignatureProductionCity.addKeyListener(this);
        c.gridy++;
        metadata.add(this.xadesSignatureProductionCity, c);

        final JLabel xadesSignatureProductionCountryLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.12")); //$NON-NLS-1$
        xadesSignatureProductionCountryLabel.setLabelFor(this.xadesSignatureProductionCountry);
        c.gridy++;
        metadata.add(xadesSignatureProductionCountryLabel, c);
        this.xadesSignatureProductionCountry.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.67")); //$NON-NLS-1$
        this.xadesSignatureProductionCountry.addKeyListener(this.modificationListener);
        this.xadesSignatureProductionCountry.addKeyListener(this);
        c.gridy++;
        metadata.add(this.xadesSignatureProductionCountry, c);

        final JLabel xadesSignerCertifiedRoleLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.14")); //$NON-NLS-1$
        xadesSignerCertifiedRoleLabel.setLabelFor(this.xadesSignerCertifiedRole);
        c.gridy++;
        metadata.add(xadesSignerCertifiedRoleLabel, c);
        this.xadesSignerCertifiedRole.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.68")); //$NON-NLS-1$
        this.xadesSignerCertifiedRole.addKeyListener(this.modificationListener);
        this.xadesSignerCertifiedRole.addKeyListener(this);
        c.gridy++;
        metadata.add(this.xadesSignerCertifiedRole, c);

        final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
	    final JPanel format = new JPanel(fLayout);
        format.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), SimpleAfirmaMessages.getString("PreferencesPanel.15"))); //$NON-NLS-1$
        this.xadesSignFormat.setSelectedItem(
    		PREFERENCES.get(PREFERENCE_XADES_SIGN_FORMAT, AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)
		);
        this.xadesSignFormat.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.53")); //$NON-NLS-1$
        this.xadesSignFormat.addItemListener(this.modificationListener);
        this.xadesSignFormat.addKeyListener(this);
        format.add(this.xadesSignFormat);

        gbc.gridy++;
        panel.add(metadata, gbc);

        gbc.gridy++;
        panel.add(format, gbc);

        gbc.gridy++;
        gbc.weighty = 1.0;
        panel.add(new JPanel(), gbc);

        return panel;
	}

	private JPanel createCadesPanel() {
	    final JPanel panel = new JPanel();
        panel.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;

        final List<PolicyPanel.PolicyItem> cadesPolicies = new ArrayList<PolicyPanel.PolicyItem>();
        cadesPolicies.add(new PolicyPanel.PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.25"), //$NON-NLS-1$
        		POLICY_CADES_AGE_1_8));
        cadesPolicies.add(new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_CADES_PADES_AGE_1_9));

        this.cadesPolicyPanel = new PolicyPanel(SIGN_FORMAT_CADES, cadesPolicies, getCadesPreferedPolicy(), null);
        this.cadesPolicyPanel.setModificationListener(this.modificationListener);
        this.cadesPolicyPanel.setKeyListener(this);
        panel.add(this.cadesPolicyPanel, c);

	    final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
	    final JPanel signatureMode = new JPanel(fLayout);
	    signatureMode.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), SimpleAfirmaMessages.getString("PreferencesPanel.16"))); //$NON-NLS-1$
	    this.cadesImplicit.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.45")); //$NON-NLS-1$
	    this.cadesImplicit.addItemListener(this.modificationListener);
	    this.cadesImplicit.addKeyListener(this);
	    signatureMode.add(this.cadesImplicit);

	    c.gridy++;
	    panel.add(signatureMode, c);

	    c.gridy++;
	    c.weighty = 1.0;
	    panel.add(new JPanel(), c);

	    return panel;
	}

	private JPanel createGeneralPanel() {
		final JPanel panel = new JPanel();
		panel.setBorder(BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanel.17"))); //$NON-NLS-1$
		panel.setLayout(new GridBagLayout());

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.gridx = 0;
		c.gridy = 0;

		final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
		final JPanel signatureAgorithmPanel = new JPanel(fLayout);
		signatureAgorithmPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), SimpleAfirmaMessages.getString("PreferencesPanel.18"))); //$NON-NLS-1$
		this.signarureAlgorithms.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.46")); //$NON-NLS-1$
		this.signarureAlgorithms.addItemListener(this.modificationListener);
		this.signarureAlgorithms.addKeyListener(this);
		this.signarureAlgorithms.setModel(
			new DefaultComboBoxModel(
				new String[] {
					"SHA1withRSA", //$NON-NLS-1$
					"SHA512withRSA", //$NON-NLS-1$
					"SHA384withRSA", //$NON-NLS-1$
					"SHA256withRSA" //$NON-NLS-1$
				}
			)
		);
		this.signarureAlgorithms.setSelectedItem(PreferencesPanel.PREFERENCES.get(PREFERENCE_SIGNATURE_ALGORITHM, "SHA1withRSA")); //$NON-NLS-1$
		signatureAgorithmPanel.add(this.signarureAlgorithms);

		panel.add(signatureAgorithmPanel, c);

		final JPanel generalPreferencesPanel = new JPanel(fLayout);
		generalPreferencesPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), SimpleAfirmaMessages.getString("PreferencesPanel.37"))); //$NON-NLS-1$
		this.avoidAskForClose.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.44")); //$NON-NLS-1$
		this.avoidAskForClose.addItemListener(this.modificationListener);
		this.avoidAskForClose.addKeyListener(this);
		generalPreferencesPanel.add(this.avoidAskForClose);

		c.gridy++;
		panel.add(generalPreferencesPanel, c);

		c.weighty = 1.0;
		c.gridy++;
		panel.add(new JPanel(), c);

		return panel;
	}

	private JPanel createPadesPanel() {

		final JPanel panel = new JPanel();

		panel.setLayout(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;

        final List<PolicyPanel.PolicyItem> padesPolicies = new ArrayList<PolicyPanel.PolicyItem>();
        padesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_CADES_PADES_AGE_1_9
    		)
		);

        this.padesPolicyPanel = new PolicyPanel(
    		SIGN_FORMAT_PADES,
    		padesPolicies,
    		getPadesPreferedPolicy(),
    		this.padesBasicFormat
		);
        this.padesPolicyPanel.setModificationListener(this.modificationListener);
        this.padesPolicyPanel.setKeyListener(this);
        panel.add(this.padesPolicyPanel, gbc);

	    final JPanel metadataPanel = new JPanel();
        metadataPanel.setBorder(BorderFactory.createTitledBorder(
    		SimpleAfirmaMessages.getString("PreferencesPanel.19")) //$NON-NLS-1$
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
	    this.padesSignReason.addKeyListener(this.modificationListener);
	    this.padesSignReason.addKeyListener(this);
	    metadataPanel.add(this.padesSignReason, c);

	    c.gridy++;

	    final JLabel padesSignProductionCityLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.21")); //$NON-NLS-1$
	    padesSignProductionCityLabel.setLabelFor(this.padesSignProductionCity);
	    metadataPanel.add(padesSignProductionCityLabel, c);

	    c.gridy++;

	    this.padesSignProductionCity.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.64")); //$NON-NLS-1$
	    this.padesSignProductionCity.addKeyListener(this.modificationListener);
	    this.padesSignProductionCity.addKeyListener(this);
	    metadataPanel.add(this.padesSignProductionCity, c);

	    c.gridy++;

	    final JLabel padesSignerContactLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.22")); //$NON-NLS-1$
	    padesSignerContactLabel.setLabelFor(this.padesSignerContact);
	    metadataPanel.add(padesSignerContactLabel, c);

	    c.gridy++;

	    this.padesSignerContact.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.65")); //$NON-NLS-1$
	    this.padesSignerContact.addKeyListener(this.modificationListener);
	    this.padesSignerContact.addKeyListener(this);
	    metadataPanel.add(this.padesSignerContact, c);

	    c.gridy++;
	    c.weighty = 1.0;
	    metadataPanel.add(new JPanel(), c);

	    gbc.gridy++;

	    panel.add(metadataPanel, gbc);

		final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
		final JPanel padesPreferencesPanel = new JPanel(fLayout);
		padesPreferencesPanel.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createEmptyBorder(),
				SimpleAfirmaMessages.getString("PreferencesPanel.69") //$NON-NLS-1$
			)
		);
		this.padesBasicFormat.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.70") //$NON-NLS-1$
		);
		this.padesBasicFormat.addItemListener(this.modificationListener);
		this.padesBasicFormat.addKeyListener(this);

		final DefaultComboBoxModel padesFormatModel = new DefaultComboBoxModel(
			new ValueTextPair[] {
				new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BES, PADES_FORMAT_BES_TEXT),
				new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BASIC, PADES_FORMAT_BASIC_TEXT)
			}
		);
		this.padesBasicFormat.setModel(padesFormatModel);
		final String selectedValue = PreferencesPanel.PREFERENCES.get(
			PREFERENCE_PADES_FORMAT,
			AOSignConstants.PADES_SUBFILTER_BASIC
		);
		for (int i = 0; i < padesFormatModel.getSize(); i++) {
			if (padesFormatModel.getElementAt(i).equals(selectedValue)) {
				this.padesBasicFormat.setSelectedIndex(i);
				break;
			}
		}
		padesPreferencesPanel.add(this.padesBasicFormat);

		gbc.gridy++;
		panel.add(padesPreferencesPanel, gbc);

	    gbc.gridy++;
	    gbc.weighty = 1.0;
	    panel.add(new JPanel(), gbc); // Panel de relleno

	    return panel;
	}

	private JPanel createButtonsPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		final JButton cancelButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.31")); //$NON-NLS-1$
		cancelButton.setMnemonic('C');
		cancelButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.32") //$NON-NLS-1$
		);
		cancelButton.addKeyListener(this);
		cancelButton.addActionListener(
			new ActionListener() {
			    /** {@inheritDoc} */
	            @Override
	            public void actionPerformed(final ActionEvent ae) {
	                PreferencesPanel.this.getParentWindow().dispose();
	            }
	        }
		);

		final JButton acceptButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.33")); //$NON-NLS-1$
		acceptButton.setMnemonic('A');
		acceptButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.34") //$NON-NLS-1$
		);
		acceptButton.addKeyListener(this);
		acceptButton.addActionListener(new ActionListener() {
			/** {@inheritDoc} */
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (savePreferences()) {
				    PreferencesPanel.this.getParentWindow().dispose();
				}
			}
		});

		this.applyButton.setMnemonic('p');
		this.applyButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.35") //$NON-NLS-1$
		);
		this.applyButton.addKeyListener(this);
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

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(cancelButton);
			panel.add(this.applyButton);
			panel.add(acceptButton);
		}
		else {
			panel.add(this.applyButton);
			panel.add(acceptButton);
			panel.add(cancelButton);
		}
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

	/** Indica si se ha modificado alg&uacute;n dato desde el &uacute;ltimo guardado,
	 * y por lo tanto si hay algo nuevo para guardar.
	 * @param mod <code>true</code> si se ha modificado alg&uacute;n dato, <code>false</code>
	 *            en caso contrario. */
	void setModified(final boolean mod) {
	    this.applyButton.setEnabled(mod);
	}

	/** Obtiene la configuraci&oacute;n de politica de firma XAdES establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada. */
	private static AdESPolicy getXadesPreferedPolicy() {

		if (PREFERENCES.get(PREFERENCE_XADES_POLICY_IDENTIFIER, null) == null) {
			return null;
		}
		try {
			return new AdESPolicy(
				PREFERENCES.get(PREFERENCE_XADES_POLICY_IDENTIFIER, null),
				PREFERENCES.get(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, null),
				PREFERENCES.get(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, null),
				PREFERENCES.get(PREFERENCE_XADES_POLICY_QUALIFIER, null)
				);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al recuperar la politica XAdES guardada en preferencias: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
	}

	/**
	 * Obtiene la configuraci&oacute;n de politica de firma PAdES establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada.
	 */
	private static AdESPolicy getPadesPreferedPolicy() {

		if (PREFERENCES.get(PREFERENCE_PADES_POLICY_IDENTIFIER, null) == null) {
			return null;
		}
		try {
			return new AdESPolicy(
					PREFERENCES.get(PREFERENCE_PADES_POLICY_IDENTIFIER, null),
					PREFERENCES.get(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH, null),
					PREFERENCES.get(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM, null),
					PREFERENCES.get(PREFERENCE_PADES_POLICY_QUALIFIER, null)
					);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al recuperar la politica PAdES guardada en preferencias: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
	}

	/**
	 * Obtiene la configuraci&oacute;n de politica de firma CAdES establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada.
	 */
	private static AdESPolicy getCadesPreferedPolicy() {

		if (PREFERENCES.get(PREFERENCE_CADES_POLICY_IDENTIFIER, null) == null) {
			return null;
		}
		try {
			return new AdESPolicy(
					PREFERENCES.get(PREFERENCE_CADES_POLICY_IDENTIFIER, null),
					PREFERENCES.get(PREFERENCE_CADES_POLICY_IDENTIFIER_HASH, null),
					PREFERENCES.get(PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM, null),
					PREFERENCES.get(PREFERENCE_CADES_POLICY_QUALIFIER, null)
					);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al recuperar la politica CAdES guardada en preferencias: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
	}


	/** {@inheritDoc} */
	@Override
	public void keyPressed(final KeyEvent e) { /* Vacio */ }

	/** {@inheritDoc} */
	@Override
	public void keyReleased(final KeyEvent ke) {
		// En Mac no cerramos los dialogos con Escape
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE && !Platform.OS.MACOSX.equals(Platform.getOS())) {
			PreferencesPanel.this.getParentWindow().dispose();
		}
	}

	/** {@inheritDoc} */
	@Override
	public void keyTyped(final KeyEvent e) { /* Vacio */ }

	/**
	 * Par de cadenas para su uso en ComboBox. Una cadena es el valor del elemento seleccionado y
	 * la otra el texto que se debe mostrar.
	 */
	private class ValueTextPair {

		private final String value;
		private final String text;

		public ValueTextPair(final String value, final String text) {
			this.value = value;
			this.text = text;
		}

		public String getValue() {
			return this.value;
		}

		@Override
		public boolean equals(final Object obj) {
			if (obj == null) {
				return false;
			}
			if (obj instanceof ValueTextPair) {
				return this.value.equals(((ValueTextPair) obj).value);
			}
			return this.value.equals(obj.toString());
		}

		@Override
		public String toString() {
			return this.text;
		}

		@Override
		public int hashCode() {
			// Funciona aleatoria para calcular el hashcode
			return 5 * this.text.length() + 7 * this.value.length();
		}
	}
}