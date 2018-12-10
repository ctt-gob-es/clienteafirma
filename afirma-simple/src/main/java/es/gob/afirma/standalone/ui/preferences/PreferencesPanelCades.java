/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_IMPLICIT;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_MULTISIGN_COSIGN;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_MULTISIGN_COUNTERSIGN_LEAFS;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_MULTISIGN_COUNTERSIGN_TREE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_POLICY_HASH;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_POLICY_HASH_ALGORITHM;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_POLICY_QUALIFIER;

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
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;

import org.ietf.jgss.GSSException;
import org.ietf.jgss.Oid;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.preferences.PolicyPanel.PolicyItem;

final class PreferencesPanelCades extends JScrollPane {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long serialVersionUID = -2410844527428138817L;

	private static final String SIGN_FORMAT_CADES = "CAdES"; //$NON-NLS-1$

	/**
	 * Atributo que permite gestionar el bloqueo de preferencias.
	 */
	private boolean blocked = true;

	private static final AdESPolicy POLICY_CADES_PADES_AGE_1_9 = new AdESPolicy(
		"2.16.724.1.3.1.1.2.1.9", //$NON-NLS-1$
		"G7roucf600+f03r/o0bAOQ6WAs0=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"https://sede.060.gob.es/politica_de_firma_anexo_1.pdf" //$NON-NLS-1$
	);

	private PolicyPanel cadesPolicyDlg;

	/**
	 * Atributo que representa la etiqueta de la pol&iacute;tica seleccionada en
	 * el di&aacute;logo
	 */
	private JLabel currentPolicyValue;

	private final JCheckBox signatureMode = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.1")); //$NON-NLS-1$

	private final JRadioButton optionCoSign = new JRadioButton(SimpleAfirmaMessages.getString("PreferencesPanel.168")); //$NON-NLS-1$
	private final JRadioButton optionCounterSignLeafs = new JRadioButton(SimpleAfirmaMessages.getString("PreferencesPanel.169")); //$NON-NLS-1$
	private final JRadioButton optionCounterSignTree = new JRadioButton(SimpleAfirmaMessages.getString("PreferencesPanel.170")); //$NON-NLS-1$

	PreferencesPanelCades(final KeyListener keyListener,
						  final ModificationListener modificationListener,
						  final boolean blocked) {

		setBlocked(blocked);
		createUI(keyListener, modificationListener);
	}

	void createUI(final KeyListener keyListener,
				  final ModificationListener modificationListener) {

		final JPanel mainPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;

        loadPreferences();

        loadCadesPolicy();

    	this.cadesPolicyDlg.setModificationListener(modificationListener);
    	this.cadesPolicyDlg.setKeyListener(keyListener);

        ///////////// Panel Policy ////////////////

        final JPanel policyConfigPanel = createPolicyPanel();
		policyConfigPanel.setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.153") //$NON-NLS-1$
			)
		);

        ///////////// Fin Panel Policy ////////////////

		c.gridy++;
		mainPanel.add(policyConfigPanel, c);

		///////////// Inicio Panel Opciones de firma ////////////////

	    final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
	    final JPanel signatureOptionsPanel = new JPanel(fLayout);
	    signatureOptionsPanel.setBorder(
	    	BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.69") //$NON-NLS-1$
			)
		);
	    this.signatureMode.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.45") //$NON-NLS-1$
		);
	    this.signatureMode.setMnemonic('i');
	    this.signatureMode.addItemListener(modificationListener);
	    this.signatureMode.addKeyListener(keyListener);
	    this.signatureMode.setEnabled(!isBlocked());
	    signatureOptionsPanel.add(this.signatureMode);

	    ///////////// Fin Panel Opciones de firma ////////////////

	    c.gridy++;
	    mainPanel.add(signatureOptionsPanel, c);

        ///////////// Panel Multisign ////////////////

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

        this.optionCoSign.setEnabled(!isBlocked());
    	this.optionCoSign.addItemListener(modificationListener);
    	this.optionCoSign.addKeyListener(keyListener);

        this.optionCounterSignLeafs.setEnabled(!isBlocked());
        this.optionCounterSignLeafs.addItemListener(modificationListener);
        this.optionCounterSignLeafs.addKeyListener(keyListener);

        this.optionCounterSignTree.setEnabled(!isBlocked());
        this.optionCounterSignTree.addItemListener(modificationListener);
        this.optionCounterSignTree.addKeyListener(keyListener);

        multisignConfigPanel.add(this.optionCoSign, mcpc);

        mcpc.gridy++;
        multisignConfigPanel.add(this.optionCounterSignLeafs, mcpc);

        mcpc.gridy++;
        multisignConfigPanel.add(this.optionCounterSignTree, mcpc);

        ///////////// Fin Panel Multisign ////////////////

		c.gridy++;
		mainPanel.add(multisignConfigPanel, c);

	    c.gridy++;
	    c.weighty = 1.0;
	    mainPanel.add(new JPanel(), c);

		// Panel para el boton de restaurar la configuracion
		final JPanel panelGeneral = new JPanel(new FlowLayout(FlowLayout.TRAILING));

		final JButton restoreConfigButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.147") //$NON-NLS-1$
		);

		restoreConfigButton.setMnemonic('R');
		restoreConfigButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (AOUIFactory.showConfirmDialog(getParent(), SimpleAfirmaMessages.getString("PreferencesPanel.157"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PreferencesPanel.139"), //$NON-NLS-1$
						JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {

					loadDefaultPreferences();

				}
			}
		});
		restoreConfigButton.getAccessibleContext()
				.setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.136") //$NON-NLS-1$
		);

	    c.gridy++;
	    c.weighty = 0.0;
		panelGeneral.add(restoreConfigButton, c);

		c.gridy++;

		mainPanel.add(panelGeneral, c);

		setViewportView(mainPanel);
	}

	private JPanel createPolicyPanel() {

		final JLabel currentPolicyLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.171")); //$NON-NLS-1$
		this.currentPolicyValue = new JLabel(this.cadesPolicyDlg.getSelectedPolicyName());
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
					changeCadesPolicyDlg(getParent());
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

	void checkPreferences() throws AOException {

		loadCadesPolicy();

		final AdESPolicy p = this.cadesPolicyDlg.getSelectedPolicy();
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
		PreferencesManager.putBoolean(PREFERENCE_CADES_IMPLICIT, this.signatureMode.isSelected());
		final AdESPolicy cadesPolicy = this.cadesPolicyDlg.getSelectedPolicy();
		if (cadesPolicy != null) {
			PreferencesManager.put(PREFERENCE_CADES_POLICY_IDENTIFIER, cadesPolicy.getPolicyIdentifier());
			PreferencesManager.put(PREFERENCE_CADES_POLICY_HASH, cadesPolicy.getPolicyIdentifierHash());
			PreferencesManager.put(PREFERENCE_CADES_POLICY_HASH_ALGORITHM, cadesPolicy.getPolicyIdentifierHashAlgorithm());
			if (cadesPolicy.getPolicyQualifier() != null) {
				PreferencesManager.put(PREFERENCE_CADES_POLICY_QUALIFIER, cadesPolicy.getPolicyQualifier().toString());
			}
			else {
				PreferencesManager.remove(PREFERENCE_CADES_POLICY_QUALIFIER);
			}
		}
		else {
			PreferencesManager.remove(PREFERENCE_CADES_POLICY_IDENTIFIER);
			PreferencesManager.remove(PREFERENCE_CADES_POLICY_HASH);
			PreferencesManager.remove(PREFERENCE_CADES_POLICY_HASH_ALGORITHM);
			PreferencesManager.remove(PREFERENCE_CADES_POLICY_QUALIFIER);
		}

		PreferencesManager.putBoolean(PREFERENCE_CADES_MULTISIGN_COSIGN, this.optionCoSign.isSelected());
		PreferencesManager.putBoolean(PREFERENCE_CADES_MULTISIGN_COUNTERSIGN_LEAFS, this.optionCounterSignLeafs.isSelected());
		PreferencesManager.putBoolean(PREFERENCE_CADES_MULTISIGN_COUNTERSIGN_TREE, this.optionCounterSignTree.isSelected());

		this.cadesPolicyDlg.saveCurrentPolicy();
	}

	void loadPreferences() {
		this.signatureMode.setSelected(PreferencesManager.getBoolean(PREFERENCE_CADES_IMPLICIT));

        final List<PolicyPanel.PolicyItem> cadesPolicies = new ArrayList<>();
        cadesPolicies.add(
    		new PolicyItem(
				SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
				POLICY_CADES_PADES_AGE_1_9
			)
		);

        this.cadesPolicyDlg = new PolicyPanel(
    		SIGN_FORMAT_CADES,
    		cadesPolicies,
    		getCadesPreferedPolicy(),
    		isBlocked()
        );

		this.optionCoSign.setSelected(PreferencesManager.getBoolean(PREFERENCE_CADES_MULTISIGN_COSIGN));
		this.optionCounterSignLeafs.setSelected(PreferencesManager.getBoolean(PREFERENCE_CADES_MULTISIGN_COUNTERSIGN_LEAFS));
		this.optionCounterSignTree.setSelected(PreferencesManager.getBoolean(PREFERENCE_CADES_MULTISIGN_COUNTERSIGN_TREE));

        revalidate();
        repaint();
	}

	void loadDefaultPreferences() {

		this.signatureMode.setSelected(PreferencesManager.getBooleanDefaultPreference(PREFERENCE_CADES_IMPLICIT));

		final List<PolicyPanel.PolicyItem> cadesPolicies = new ArrayList<>();
        cadesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_CADES_PADES_AGE_1_9
    		)
		);

        this.cadesPolicyDlg = new PolicyPanel(
        		SIGN_FORMAT_CADES,
        		cadesPolicies,
        		getCadesDefaultPolicy(),
        		isBlocked()
    		);

		this.currentPolicyValue.setText(this.cadesPolicyDlg.getSelectedPolicyName());

		this.optionCoSign.setSelected(PreferencesManager.getBooleanDefaultPreference(PREFERENCE_CADES_MULTISIGN_COSIGN));
		this.optionCounterSignLeafs.setSelected(PreferencesManager.getBooleanDefaultPreference(PREFERENCE_CADES_MULTISIGN_COUNTERSIGN_LEAFS));
		this.optionCounterSignTree.setSelected(PreferencesManager.getBooleanDefaultPreference(PREFERENCE_CADES_MULTISIGN_COUNTERSIGN_TREE));

        revalidate();
        repaint();
	}

	/** Obtiene la configuraci&oacute;n de politica de firma CAdES establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada. */
	private static AdESPolicy getCadesPreferedPolicy() {

		if (PreferencesManager.get(PREFERENCE_CADES_POLICY_IDENTIFIER) == null ||
				PreferencesManager.get(PREFERENCE_CADES_POLICY_IDENTIFIER).isEmpty()) {
			return null;
		}
		try {
			return new AdESPolicy(
					PreferencesManager.get(PREFERENCE_CADES_POLICY_IDENTIFIER),
					PreferencesManager.get(PREFERENCE_CADES_POLICY_HASH),
					PreferencesManager.get(PREFERENCE_CADES_POLICY_HASH_ALGORITHM),
					PreferencesManager.get(PREFERENCE_CADES_POLICY_QUALIFIER)
					);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al recuperar la politica CAdES guardada en preferencias: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
	}

	/** Obtiene la configuraci&oacute;n de politica de firma CAdES por defecto.
	 * @return Pol&iacute;tica de firma configurada. */
	private AdESPolicy getCadesDefaultPolicy() {

		AdESPolicy adesPolicy = null;

		loadCadesPolicy();

		// Si la interfaz esta bloqueada, establecemos el valor que estuviese definido
		if (isBlocked()) {
			adesPolicy = this.cadesPolicyDlg.getSelectedPolicy();
		}
		// Si no, establecemos la configuracion por defecto
		else {
			try {
				// Si, por defecto, no debe haber ninguna politica configurada, hacemos eso
				if (PreferencesManager.getDefaultPreference(PREFERENCE_CADES_POLICY_IDENTIFIER) == null
						|| PreferencesManager.getDefaultPreference(PREFERENCE_CADES_POLICY_IDENTIFIER).isEmpty()) {
					this.cadesPolicyDlg.loadPolicy(null);
				} else {

					this.cadesPolicyDlg
							.loadPolicy(new AdESPolicy(PreferencesManager.getDefaultPreference(PREFERENCE_CADES_POLICY_IDENTIFIER),
									PreferencesManager.getDefaultPreference(PREFERENCE_CADES_POLICY_HASH),
									PreferencesManager.getDefaultPreference(PREFERENCE_CADES_POLICY_HASH_ALGORITHM),
									PreferencesManager.getDefaultPreference(PREFERENCE_CADES_POLICY_QUALIFIER)));
				}
			} catch (final Exception e) {
				Logger.getLogger("es.gob.afirma") //$NON-NLS-1$
						.severe("Error al recuperar la politica CAdES guardada en preferencias: " + e); //$NON-NLS-1$

			}
		}

		return adesPolicy;

	}


	/**
	 * Carga el panel de pol&iacute;tica con las preferencias guardadas
	 */
	private void loadCadesPolicy() {
		// Si el panel no está cargado lo obtengo de las preferencias guardadas
		if (this.cadesPolicyDlg == null) {
			final List<PolicyPanel.PolicyItem> cadesPolicies = new ArrayList<>();
			cadesPolicies.add(new PolicyItem(SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
					POLICY_CADES_PADES_AGE_1_9));

			this.cadesPolicyDlg = new PolicyPanel(SIGN_FORMAT_CADES, cadesPolicies, getCadesPreferedPolicy(), isBlocked());
		}
	}

	/**
	 * Di&aacute;logo para cambair la configuracion de la pol&iacute;tica
	 *
	 * @param container
	 *            Contenedor en el que se define el di&aacute;logo.
	 */
	public void changeCadesPolicyDlg(final Container container) {

		// Cursor en espera
		container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

		// Cursor por defecto
		container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

		loadCadesPolicy();

		if (AOUIFactory.showConfirmDialog(container, this.cadesPolicyDlg,
				SimpleAfirmaMessages.getString("PolicyDialog.0"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION, JOptionPane.DEFAULT_OPTION) == JOptionPane.OK_OPTION) {

			try {
				checkPreferences();

				this.currentPolicyValue.setText(this.cadesPolicyDlg.getSelectedPolicyName());

				final AdESPolicy cadesPolicy = this.cadesPolicyDlg.getSelectedPolicy();
				if (cadesPolicy != null) {
					PreferencesManager.put(PREFERENCE_CADES_POLICY_IDENTIFIER, cadesPolicy.getPolicyIdentifier());
					PreferencesManager.put(PREFERENCE_CADES_POLICY_HASH, cadesPolicy.getPolicyIdentifierHash());
					PreferencesManager.put(PREFERENCE_CADES_POLICY_HASH_ALGORITHM,
							cadesPolicy.getPolicyIdentifierHashAlgorithm());
					if (cadesPolicy.getPolicyQualifier() != null) {
						PreferencesManager.put(PREFERENCE_CADES_POLICY_QUALIFIER,
								cadesPolicy.getPolicyQualifier().toString());
					} else {
						PreferencesManager.remove(PREFERENCE_CADES_POLICY_QUALIFIER);
					}
				} else {
					PreferencesManager.remove(PREFERENCE_CADES_POLICY_IDENTIFIER);
					PreferencesManager.remove(PREFERENCE_CADES_POLICY_HASH);
					PreferencesManager.remove(PREFERENCE_CADES_POLICY_HASH_ALGORITHM);
					PreferencesManager.remove(PREFERENCE_CADES_POLICY_QUALIFIER);
				}

				this.cadesPolicyDlg.saveCurrentPolicy();

			} catch (final Exception e) {

				AOUIFactory.showErrorMessage(this,
						"<html><p>" + SimpleAfirmaMessages.getString("PreferencesPanel.38") + ":<br>" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								+ e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$
						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE);
				changeCadesPolicyDlg(container);

			}

		}

		this.cadesPolicyDlg = null;

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
}
