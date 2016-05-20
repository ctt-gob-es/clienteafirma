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

import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_XADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_XADES_SIGNER_CLAIMED_ROLE;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_XADES_SIGN_FORMAT;

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

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.PreferencesManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.PolicyPanel.PolicyItem;

final class PreferencesPanel extends JPanel implements KeyListener {

	private static final long serialVersionUID = -3168095095548385291L;

	private static final String SIGN_FORMAT_XADES = "XAdES"; //$NON-NLS-1$

	static final AdESPolicy POLICY_CADES_PADES_AGE_1_9 = new AdESPolicy(
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

	private final JButton applyButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.0")); //$NON-NLS-1$

	private final ModificationListener modificationListener;

	private PolicyPanel xadesPolicyPanel;

    private final Window window;
    Window getParentWindow() {
        return this.window;
    }

	private final JTextField xadesSignatureProductionCity = new JTextField(
		PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignatureProductionProvince = new JTextField(
		PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignatureProductionPostalCode = new JTextField(
		PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignatureProductionCountry = new JTextField(
		PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignerClaimedRole = new JTextField(
		PreferencesManager.get(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, "") //$NON-NLS-1$
	);
	private final JComboBox<ValueTextPair> xadesSignFormat = new JComboBox<>(
		new ValueTextPair[] {
			new ValueTextPair(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED, AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED),
			new ValueTextPair(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING),
			new ValueTextPair(AOSignConstants.SIGN_FORMAT_XADES_DETACHED, AOSignConstants.SIGN_FORMAT_XADES_DETACHED)
		}
	);

	private PreferencesPanelGeneral preferencesPanelGeneral;
	private PreferencesPanelCades preferencesPanelCades;
	private PreferencesPanelPades preferencesPanelPades;
	private PreferencesPanelKeyStores preferencesPanelKeyStores;


	private final JTabbedPane tabbedPane = new JTabbedPane();

	void createUI() {

		this.preferencesPanelGeneral = new PreferencesPanelGeneral(this, this.modificationListener);
		this.preferencesPanelCades = new PreferencesPanelCades(this, this.modificationListener);
		this.preferencesPanelPades = new PreferencesPanelPades(this, this.modificationListener);
		this.preferencesPanelKeyStores = new PreferencesPanelKeyStores(this, this.modificationListener);

		this.tabbedPane.addKeyListener(this);

		int count = this.tabbedPane.getTabCount();
		this.tabbedPane.addTab(
			SimpleAfirmaMessages.getString("PreferencesPanel.2"), //$NON-NLS-1$
			null,
			this.preferencesPanelGeneral,
			SimpleAfirmaMessages.getString("PreferencesPanel.40") //$NON-NLS-1$
		);
		this.tabbedPane.setMnemonicAt(count, KeyEvent.VK_G);

		count = this.tabbedPane.getTabCount();
		this.tabbedPane.addTab(
			SimpleAfirmaMessages.getString("PreferencesPanel.3"), //$NON-NLS-1$
			null,
			this.preferencesPanelPades,
			SimpleAfirmaMessages.getString("PreferencesPanel.41") //$NON-NLS-1$
		);
		this.tabbedPane.setMnemonicAt(count, KeyEvent.VK_F);

		count = this.tabbedPane.getTabCount();
		this.tabbedPane.addTab(
			SimpleAfirmaMessages.getString("PreferencesPanel.4"), //$NON-NLS-1$
			null,
			this.preferencesPanelCades,
			SimpleAfirmaMessages.getString("PreferencesPanel.42") //$NON-NLS-1$
		);
		this.tabbedPane.setMnemonicAt(count, KeyEvent.VK_E);

		count = this.tabbedPane.getTabCount();
		this.tabbedPane.addTab(
			SimpleAfirmaMessages.getString("PreferencesPanel.5"), //$NON-NLS-1$
			null,
			createXadesPanel(),
			SimpleAfirmaMessages.getString("PreferencesPanel.43") //$NON-NLS-1$
		);
		this.tabbedPane.setMnemonicAt(count, KeyEvent.VK_X);

//		count = this.tabbedPane.getTabCount();
//		this.tabbedPane.addTab(
//			SimpleAfirmaMessages.getString("PreferencesPanel.84"), //$NON-NLS-1$
//			null,
//			this.preferencesPanelKeyStores,
//			SimpleAfirmaMessages.getString("PreferencesPanel.85") //$NON-NLS-1$
//		);
//		this.tabbedPane.setMnemonicAt(count, KeyEvent.VK_V);

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
		this.preferencesPanelGeneral.savePreferences();

		//****************************************************************************
		//**** PREFERENCIAS CADES ****************************************************
		//****************************************************************************
		this.preferencesPanelCades.savePreferences();

		//****************************************************************************
		//**** PREFERENCIAS PADES ****************************************************
		//****************************************************************************
		this.preferencesPanelPades.savePreferences();

		//****************************************************************************
		//**** PREFERENCIAS XADES ****************************************************
		//****************************************************************************
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

		try {
			PreferencesManager.flush();
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al guardar las preferencias de firma: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}

		final AdESPolicy xadesPolicy = this.xadesPolicyPanel.getCurrentPolicy();
		if (xadesPolicy != null) {
			PreferencesManager.put(PREFERENCE_XADES_POLICY_IDENTIFIER, xadesPolicy.getPolicyIdentifier());
			PreferencesManager.put(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, xadesPolicy.getPolicyIdentifierHash());
			PreferencesManager.put(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, xadesPolicy.getPolicyIdentifierHashAlgorithm());
			if (xadesPolicy.getPolicyQualifier() != null) {
				PreferencesManager.put(PREFERENCE_XADES_POLICY_QUALIFIER, xadesPolicy.getPolicyQualifier().toString());
			}
			else {
				PreferencesManager.remove(PREFERENCE_XADES_POLICY_QUALIFIER);
			}
		}
		else {
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_IDENTIFIER);
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH);
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_QUALIFIER);
		}
		this.xadesPolicyPanel.saveCurrentPolicy();

	    return true;

	}

	/** Comprueba que los datos configurados sean v&aacute;lidos.
	 * @return {@code true} cuando los datos son v&aacute;lidos, {@code false} en caso
	 * contrario. */
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
			this.preferencesPanelPades.checkPreferences();
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
			this.preferencesPanelCades.checkPreferences();
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

        final List<PolicyPanel.PolicyItem> xadesPolicies = new ArrayList<>();
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
        xadesSignerCertifiedRoleLabel.setLabelFor(this.xadesSignerClaimedRole);
        c.gridy++;
        metadata.add(xadesSignerCertifiedRoleLabel, c);
        this.xadesSignerClaimedRole.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.68")); //$NON-NLS-1$
        this.xadesSignerClaimedRole.addKeyListener(this.modificationListener);
        this.xadesSignerClaimedRole.addKeyListener(this);
        c.gridy++;
        metadata.add(this.xadesSignerClaimedRole, c);

        final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
	    final JPanel format = new JPanel(fLayout);
        format.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), SimpleAfirmaMessages.getString("PreferencesPanel.15"))); //$NON-NLS-1$
        this.xadesSignFormat.setSelectedItem(
    		PreferencesManager.get(PREFERENCE_XADES_SIGN_FORMAT, AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)
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

		if (PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER, null) == null) {
			return null;
		}
		try {
			return new AdESPolicy(
				PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER, null),
				PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, null),
				PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, null),
				PreferencesManager.get(PREFERENCE_XADES_POLICY_QUALIFIER, null)
				);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al recuperar la politica XAdES guardada en preferencias: " + e); //$NON-NLS-1$ //$NON-NLS-2$
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

	/** Par de cadenas para su uso en ComboBox. Una cadena es el valor del elemento seleccionado y
	 * la otra el texto que se debe mostrar. */
	static final class ValueTextPair {

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