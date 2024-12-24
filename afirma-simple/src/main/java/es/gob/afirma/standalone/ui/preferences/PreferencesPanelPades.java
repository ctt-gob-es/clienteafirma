/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_CHECK_ALLOW_CERTIFIED_PDF;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_CHECK_SHADOW_ATTACK;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_DEFAULT_CERTIFICATION_LEVEL;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_FORMAT;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_OBFUSCATE_CERT_INFO;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_POLICY_HASH;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_POLICY_HASH_ALGORITHM;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_SIGNER_CONTACT;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_SIGN_PRODUCTION_CITY;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_SIGN_REASON;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_STAMP;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_PADES_VISIBLE;

import java.awt.Color;
import java.awt.Container;
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
import java.util.Locale;
import java.util.ResourceBundle;
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
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.ui.preferences.PreferencesPanel.ValueTextPair;

final class PreferencesPanelPades extends JScrollPane {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long serialVersionUID = 4299378019540627483L;

	private static final AdESPolicy POLICY_PADES_AGE_1_9;

	private static final String POLICY_BUNDLE_NAME = "policy"; //$NON-NLS-1$

	static {

		final ResourceBundle policyBundle = ResourceBundle
				.getBundle(POLICY_BUNDLE_NAME, Locale.getDefault());

		POLICY_PADES_AGE_1_9  = new AdESPolicy(
				policyBundle.getString("FirmaAGE19.policyIdentifier"), //$NON-NLS-1$
				policyBundle.getString("FirmaAGE19.policyIdentifierHash.PAdES"), //$NON-NLS-1$
				"SHA1", //$NON-NLS-1$
				policyBundle.getString("FirmaAGE19.policyQualifier") //$NON-NLS-1$
			);
	}

	private PolicyPanel padesPolicyDlg;

	/**
	 * Atributo que representa la etiqueta de la pol&iacute;tica seleccionada en
	 * el di&aacute;logo.
	 */
	private JLabel currentPolicyValue;

	/**
	 * Atributo para gestionar el bloqueo de propiedades.
	 */
	private boolean blocked = true;

	private final JComboBox<Object> padesBasicFormat = new JComboBox<>();

	private final JComboBox<Object> pdfSignCertified = new JComboBox<>();

	private final JTextField padesSignReason = new JTextField();

	private final JTextField padesSignProductionCity = new JTextField();

	private final JTextField padesSignerContact = new JTextField();

	private final JCheckBox visiblePdfSignature = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.79")); //$NON-NLS-1$
	private final JCheckBox obfuscateCertificateInfo = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.175")); //$NON-NLS-1$
	private final JCheckBox visiblePdfStamp = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.172")); //$NON-NLS-1$
	private final JCheckBox checkShadowAttack = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.187")); //$NON-NLS-1$
	private final JCheckBox checkAllowCertifiedPdf = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.204")); //$NON-NLS-1$

	private static final String PADES_FORMAT_BASIC_TEXT = SimpleAfirmaMessages.getString("PreferencesPanel.71"); //$NON-NLS-1$
	private static final String PADES_FORMAT_BES_TEXT = SimpleAfirmaMessages.getString("PreferencesPanel.72"); //$NON-NLS-1$

	private static final String PDF_CERT_TYPE_0_TEXT = SimpleAfirmaMessages.getString("PreferencesPanel.206"); //$NON-NLS-1$
	private static final String PDF_CERT_TYPE_1_TEXT = SimpleAfirmaMessages.getString("PreferencesPanel.207"); //$NON-NLS-1$
	private static final String PDF_CERT_TYPE_2_TEXT = SimpleAfirmaMessages.getString("PreferencesPanel.208"); //$NON-NLS-1$
	private static final String PDF_CERT_TYPE_3_TEXT = SimpleAfirmaMessages.getString("PreferencesPanel.209"); //$NON-NLS-1$

	private static final String SIGN_FORMAT_PADES = "PAdES"; //$NON-NLS-1$

	PreferencesPanelPades(final KeyListener keyListener,
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

		this.padesBasicFormat.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.183") //$NON-NLS-1$
		);
		this.padesBasicFormat.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.70") //$NON-NLS-1$
		);

		this.pdfSignCertified.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.183") //$NON-NLS-1$
		);
		this.pdfSignCertified.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.204") //$NON-NLS-1$
		);

		final DefaultComboBoxModel<Object> padesFormatModel = new DefaultComboBoxModel<>(
			new Object[] {
				new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BES, PADES_FORMAT_BES_TEXT),
				new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BASIC, PADES_FORMAT_BASIC_TEXT)
			}
		);

		final DefaultComboBoxModel<Object> pdfCertifiedFormatModel = new DefaultComboBoxModel<>(
			new Object[] {
				new ValueTextPair(PdfExtraParams.CERTIFICATION_LEVEL_VALUE_TYPE_0, PDF_CERT_TYPE_0_TEXT),
				new ValueTextPair(PdfExtraParams.CERTIFICATION_LEVEL_VALUE_TYPE_1, PDF_CERT_TYPE_1_TEXT),
				new ValueTextPair(PdfExtraParams.CERTIFICATION_LEVEL_VALUE_TYPE_2, PDF_CERT_TYPE_2_TEXT),
				new ValueTextPair(PdfExtraParams.CERTIFICATION_LEVEL_VALUE_TYPE_3, PDF_CERT_TYPE_3_TEXT)
			}
		);

		this.padesBasicFormat.setModel(padesFormatModel);
		this.padesBasicFormat.addItemListener(modificationListener);
		this.padesBasicFormat.addKeyListener(keyListener);
		this.padesBasicFormat.setEnabled(!isBlocked());

		this.pdfSignCertified.setModel(pdfCertifiedFormatModel);
		this.pdfSignCertified.addItemListener(modificationListener);
		this.pdfSignCertified.addKeyListener(keyListener);
		this.pdfSignCertified.setEnabled(isBlocked());

		this.visiblePdfSignature.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.visiblePdfSignature.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.79") //$NON-NLS-1$
		);
		this.visiblePdfSignature.addItemListener(modificationListener);
		this.visiblePdfSignature.addKeyListener(keyListener);
		this.obfuscateCertificateInfo.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.obfuscateCertificateInfo.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.175") //$NON-NLS-1$
		);
		this.obfuscateCertificateInfo.addItemListener(modificationListener);
		this.obfuscateCertificateInfo.addKeyListener(keyListener);
		this.visiblePdfStamp.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.182") //$NON-NLS-1$
		);
		this.visiblePdfStamp.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.172") //$NON-NLS-1$
		);
		this.visiblePdfStamp.addItemListener(modificationListener);
    	this.visiblePdfStamp.addKeyListener(keyListener);
    	this.checkShadowAttack.setEnabled(!this.blocked);
		this.checkShadowAttack.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.188") //$NON-NLS-1$
		);
		this.checkShadowAttack.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.187") //$NON-NLS-1$
		);
		this.checkShadowAttack.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				final JCheckBox checkPSACheckBox = (JCheckBox) e.getSource();
                if (checkPSACheckBox.isSelected()) {
                	AOUIFactory.showMessageDialog(this, SimpleAfirmaMessages.getString("PreferencesPanel.189"), //$NON-NLS-1$
                			SimpleAfirmaMessages.getString("SimpleAfirma.48"), AOUIFactory.WARNING_MESSAGE); //$NON-NLS-1$
                }
			}
		});
		this.checkShadowAttack.addItemListener(modificationListener);
    	this.checkShadowAttack.addKeyListener(keyListener);

    	this.checkAllowCertifiedPdf.getAccessibleContext().setAccessibleName(
				SimpleAfirmaMessages.getString("PreferencesPanel.204") //$NON-NLS-1$
		);
		this.checkAllowCertifiedPdf.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.205") //$NON-NLS-1$
		);
		this.checkAllowCertifiedPdf.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				final JCheckBox checkPSACheckBox = (JCheckBox) e.getSource();
               	PreferencesPanelPades.this.pdfSignCertified.setEnabled(checkPSACheckBox.isSelected());
			}
		});
		this.checkAllowCertifiedPdf.addItemListener(modificationListener);
    	this.checkAllowCertifiedPdf.addKeyListener(keyListener);

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

	    this.padesSignReason.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("PreferencesPanel.181")); //$NON-NLS-1$
	    this.padesSignReason.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.63")); //$NON-NLS-1$
	    this.padesSignReason.addKeyListener(modificationListener);
	    this.padesSignReason.addKeyListener(keyListener);
	    metadataPanel.add(this.padesSignReason, c);

	    c.gridy++;

	    final JLabel padesSignProductionCityLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.21")); //$NON-NLS-1$
	    padesSignProductionCityLabel.setLabelFor(this.padesSignProductionCity);
	    metadataPanel.add(padesSignProductionCityLabel, c);

	    c.gridy++;

	    this.padesSignProductionCity.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("PreferencesPanel.181")); //$NON-NLS-1$
	    this.padesSignProductionCity.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.64")); //$NON-NLS-1$
	    this.padesSignProductionCity.addKeyListener(modificationListener);
	    this.padesSignProductionCity.addKeyListener(keyListener);
	    metadataPanel.add(this.padesSignProductionCity, c);

	    c.gridy++;

	    final JLabel padesSignerContactLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.22")); //$NON-NLS-1$
	    padesSignerContactLabel.setLabelFor(this.padesSignerContact);
	    metadataPanel.add(padesSignerContactLabel, c);

	    c.gridy++;

	    this.padesSignerContact.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("PreferencesPanel.181")); //$NON-NLS-1$
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

				restorePreferences();
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
			ae -> changePadesPolicyDlg(getParent())
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

		final JLabel certifiedFormatLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanel.203") //$NON-NLS-1$
		);
		certifiedFormatLabel.setLabelFor(this.pdfSignCertified);

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
		c.gridy++;
		innerPanel.add(this.obfuscateCertificateInfo, c);
		c.gridy++;
		innerPanel.add(this.visiblePdfStamp, c);
		c.gridy++;
		innerPanel.add(this.checkShadowAttack, c);
		c.gridy++;
		final JPanel certifiedOptionsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		certifiedOptionsPanel.add(this.checkAllowCertifiedPdf);
		certifiedOptionsPanel.add(certifiedFormatLabel);
		certifiedOptionsPanel.add(this.pdfSignCertified);
		c.insets = new Insets(-5, 2, 0, 0);
		c.gridx = 0;
		innerPanel.add(certifiedOptionsPanel,c);


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
		PreferencesManager.put(PREFERENCE_PADES_OBFUSCATE_CERT_INFO, Boolean.toString(this.obfuscateCertificateInfo.isSelected()));
		PreferencesManager.put(PREFERENCE_PADES_STAMP, Boolean.toString(this.visiblePdfStamp.isSelected()));
		PreferencesManager.put(PREFERENCE_PADES_CHECK_SHADOW_ATTACK, Boolean.toString(this.checkShadowAttack.isSelected()));
		PreferencesManager.put(PREFERENCE_PADES_CHECK_ALLOW_CERTIFIED_PDF, Boolean.toString(this.checkAllowCertifiedPdf.isSelected()));

		PreferencesManager.put(PREFERENCE_PADES_SIGNER_CONTACT, this.padesSignerContact.getText());
		PreferencesManager.put(PREFERENCE_PADES_SIGN_PRODUCTION_CITY, this.padesSignProductionCity.getText());
		PreferencesManager.put(PREFERENCE_PADES_SIGN_REASON, this.padesSignReason.getText());

		final ComboBoxModel<Object> m = this.padesBasicFormat.getModel();
		final Object o = m.getElementAt(this.padesBasicFormat.getSelectedIndex());
		PreferencesManager.put(PREFERENCE_PADES_FORMAT, ((ValueTextPair) o).getValue());

		final ComboBoxModel<Object> mdl = this.pdfSignCertified.getModel();
		final Object obj = mdl.getElementAt(this.pdfSignCertified.getSelectedIndex());
		PreferencesManager.put(PreferencesManager.PREFERENCE_PADES_DEFAULT_CERTIFICATION_LEVEL, ((ValueTextPair) obj).getValue());

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
		this.obfuscateCertificateInfo.setSelected(PreferencesManager.getBoolean(PREFERENCE_PADES_OBFUSCATE_CERT_INFO));
		this.visiblePdfStamp.setSelected(PreferencesManager.getBoolean(PREFERENCE_PADES_STAMP));
		this.checkShadowAttack.setSelected(PreferencesManager.getBoolean(PREFERENCE_PADES_CHECK_SHADOW_ATTACK));
		this.checkAllowCertifiedPdf.setSelected(PreferencesManager.getBoolean(PREFERENCE_PADES_CHECK_ALLOW_CERTIFIED_PDF));

        final ComboBoxModel<Object> padesFormatModel = this.padesBasicFormat.getModel();
        final String selectedValue = PreferencesManager.get(PREFERENCE_PADES_FORMAT);
		for (int i = 0; i < padesFormatModel.getSize(); i++) {
			if (padesFormatModel.getElementAt(i).equals(selectedValue)) {
				this.padesBasicFormat.setSelectedIndex(i);
				break;
			}
		}

		if (this.checkAllowCertifiedPdf.isSelected()) {
			this.pdfSignCertified.setEnabled(true);
	        final ComboBoxModel<Object> pdfSignCertifiedModel = this.pdfSignCertified.getModel();
	        final String selectedCertificationLevel = PreferencesManager.get(PREFERENCE_PADES_DEFAULT_CERTIFICATION_LEVEL);
			for (int i = 0; i < pdfSignCertifiedModel.getSize(); i++) {
				if (pdfSignCertifiedModel.getElementAt(i).equals(selectedCertificationLevel)) {
					this.pdfSignCertified.setSelectedIndex(i);
					break;
				}
			}
		}


    	if (this.pdfSignCertified.getSelectedIndex() == -1 && this.pdfSignCertified.getItemCount() > 0) {
    		this.pdfSignCertified.setSelectedIndex(0);
    	}

		final List<PolicyItem> padesPolicies = new ArrayList<>();
        padesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_PADES_AGE_1_9
    		)
		);

        this.padesPolicyDlg = new PolicyPanel(
    		SIGN_FORMAT_PADES,
    		padesPolicies,
    		getPadesPreferedPolicy(),
    		isBlocked()
		);

        revalidate();
        repaint();
	}

	/** Carga las opciones de configuraci&oacute;n por defecto del panel de
	 * firmas PAdES desde un fichero externo de preferencias. */
	void restorePreferences() {

		// Eliminamos la configuracion actual
		PreferencesManager.remove(PREFERENCE_PADES_SIGN_REASON);
		PreferencesManager.remove(PREFERENCE_PADES_SIGN_PRODUCTION_CITY);
		PreferencesManager.remove(PREFERENCE_PADES_SIGNER_CONTACT);
		PreferencesManager.remove(PREFERENCE_PADES_VISIBLE);
		PreferencesManager.remove(PREFERENCE_PADES_OBFUSCATE_CERT_INFO);
		PreferencesManager.remove(PREFERENCE_PADES_STAMP);

		// Establecemos la configuracion (que sera la del sistema o la por defecto)

		this.padesSignReason.setText(PreferencesManager.get(PREFERENCE_PADES_SIGN_REASON));
		this.padesSignProductionCity.setText(PreferencesManager.get(PREFERENCE_PADES_SIGN_PRODUCTION_CITY));
		this.padesSignerContact.setText(PreferencesManager.get(PREFERENCE_PADES_SIGNER_CONTACT));
		this.visiblePdfSignature.setSelected(PreferencesManager.getBoolean(PREFERENCE_PADES_VISIBLE));
		this.obfuscateCertificateInfo.setSelected(PreferencesManager.getBoolean(PREFERENCE_PADES_OBFUSCATE_CERT_INFO));
		this.visiblePdfStamp.setSelected(PreferencesManager.getBoolean(PREFERENCE_PADES_STAMP));

        // No se modifican las propiedades bloqueadas
        if (!isBlocked()) {

        	PreferencesManager.remove(PREFERENCE_PADES_FORMAT);
        	final String selectedValue = PreferencesManager.get(PREFERENCE_PADES_FORMAT);
        	final ComboBoxModel<Object> padesFormatModel = this.padesBasicFormat.getModel();
        	for (int i = 0; i < padesFormatModel.getSize(); i++) {
        		if (padesFormatModel.getElementAt(i).equals(selectedValue)) {
        			this.padesBasicFormat.setSelectedIndex(i);
        			break;
        		}
        	}

        	if (this.padesBasicFormat.getSelectedIndex() == -1 && this.padesBasicFormat.getItemCount() > 0) {
        		this.padesBasicFormat.setSelectedIndex(0);
        	}

    		PreferencesManager.remove(PREFERENCE_PADES_POLICY_IDENTIFIER);
    		PreferencesManager.remove(PREFERENCE_PADES_POLICY_HASH);
    		PreferencesManager.remove(PREFERENCE_PADES_POLICY_HASH_ALGORITHM);
    		PreferencesManager.remove(PREFERENCE_PADES_POLICY_QUALIFIER);

        	final List<PolicyItem> padesPolicies = new ArrayList<>();
        	padesPolicies.add(
        			new PolicyItem(
        					SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        					POLICY_PADES_AGE_1_9
        					)
        			);

        	this.padesPolicyDlg = new PolicyPanel(
        			SIGN_FORMAT_PADES,
        			padesPolicies,
        			getPadesDefaultPolicy(),
        			isBlocked()
        			);

        	this.currentPolicyValue.setText(this.padesPolicyDlg.getSelectedPolicyName());

        	PreferencesManager.remove(PREFERENCE_PADES_CHECK_SHADOW_ATTACK);
        	this.checkShadowAttack.setSelected(PreferencesManager.getBoolean(PREFERENCE_PADES_CHECK_SHADOW_ATTACK));


    		PreferencesManager.remove(PREFERENCE_PADES_CHECK_ALLOW_CERTIFIED_PDF);
    		PreferencesManager.remove(PREFERENCE_PADES_DEFAULT_CERTIFICATION_LEVEL);

    		this.checkAllowCertifiedPdf.setSelected(PreferencesManager.getBoolean(PREFERENCE_PADES_CHECK_ALLOW_CERTIFIED_PDF));
        	this.pdfSignCertified.setEnabled(this.checkAllowCertifiedPdf.isSelected());

        	final String certificationLevel = PreferencesManager.get(PREFERENCE_PADES_DEFAULT_CERTIFICATION_LEVEL);
        	final ComboBoxModel<Object> pdfSignCertifiedModel = this.pdfSignCertified.getModel();
        	for (int i = 0; i < pdfSignCertifiedModel.getSize(); i++) {
        		if (pdfSignCertifiedModel.getElementAt(i).equals(certificationLevel)) {
        			this.pdfSignCertified.setSelectedIndex(i);
        			break;
        		}
        	}

        	if (this.pdfSignCertified.getSelectedIndex() == -1 && this.pdfSignCertified.getItemCount() > 0) {
        		this.pdfSignCertified.setSelectedIndex(0);
        	}

        }

        revalidate();
        repaint();
	}

	/** Obtiene la configuraci&oacute;n de pol&iacute;tica de firma PAdES establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada. */
	private static AdESPolicy getPadesPreferedPolicy() {

		AdESPolicy adesPolicy = null;

		final String policyIdentifier = PreferencesManager.get(PREFERENCE_PADES_POLICY_IDENTIFIER);
		if (policyIdentifier != null && !policyIdentifier.isEmpty()) {
			try {
				adesPolicy = new AdESPolicy(
						policyIdentifier,
						PreferencesManager.get(PREFERENCE_PADES_POLICY_HASH),
						PreferencesManager.get(PREFERENCE_PADES_POLICY_HASH_ALGORITHM),
						PreferencesManager.get(PREFERENCE_PADES_POLICY_QUALIFIER)
						);
			}
			catch (final Exception e) {
				LOGGER.severe("Error al recuperar la politica PAdES guardada en preferencias: " + e); //$NON-NLS-1$
			}
		}
		return adesPolicy;
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
		// Si no, establecemos la configuracion por defecto
		else {
			try {
				final String policyIdentifier = PreferencesManager.get(PREFERENCE_PADES_POLICY_IDENTIFIER);
				if (policyIdentifier != null && !policyIdentifier.isEmpty()) {
					adesPolicy = new AdESPolicy(
							policyIdentifier,
							PreferencesManager.get(PREFERENCE_PADES_POLICY_HASH),
							PreferencesManager.get(PREFERENCE_PADES_POLICY_HASH_ALGORITHM),
							PreferencesManager.get(PREFERENCE_PADES_POLICY_QUALIFIER));
				}
				this.padesPolicyDlg.loadPolicy(adesPolicy);
			}
			catch (final Exception e) {
				LOGGER.severe("Error al recuperar la politica PAdES guardada en preferencias: " + e); //$NON-NLS-1$
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
			final List<PolicyItem> padesPolicies = new ArrayList<>();
			padesPolicies.add(
					new PolicyItem(SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
							POLICY_PADES_AGE_1_9));

			this.padesPolicyDlg = new PolicyPanel(SIGN_FORMAT_PADES, padesPolicies, getPadesPreferedPolicy(), isBlocked());
		}
	}

	/** Di&aacute;logo para cambiar la configuraci&oacute;n de la pol&iacute;tica.
	 * @param container Contenedor en el que se define el di&aacute;logo. */
	public void changePadesPolicyDlg(final Container container) {

		// Cargamos el dialogo
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
					PreferencesManager.put(PREFERENCE_PADES_POLICY_HASH, padesPolicy.getPolicyIdentifierHash());
					PreferencesManager.put(PREFERENCE_PADES_POLICY_HASH_ALGORITHM, padesPolicy.getPolicyIdentifierHashAlgorithm());
					if (padesPolicy.getPolicyQualifier() != null) {
						PreferencesManager.put(PREFERENCE_PADES_POLICY_QUALIFIER, padesPolicy.getPolicyQualifier().toString());
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

				AOUIFactory.showErrorMessage(
						"<p>" + SimpleAfirmaMessages.getString("PreferencesPanel.7") + "</p>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE, e);
				changePadesPolicyDlg(container);

			}

		}

		// Siempre, tras cualquier operación limpio el panel
		this.padesPolicyDlg = null;
	}

}
