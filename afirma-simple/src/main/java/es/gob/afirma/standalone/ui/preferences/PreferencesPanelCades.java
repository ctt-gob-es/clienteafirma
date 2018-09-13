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
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_POLICY_HASH;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_POLICY_HASH_ALGORITHM;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_MULTISIGN_COSIGN;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_MULTISIGN_COUNTERSIGN_LEAFS;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_MULTISIGN_COUNTERSIGN_TREE;

import java.awt.Container;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import org.ietf.jgss.GSSException;
import org.ietf.jgss.Oid;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.preferences.PolicyPanel.PolicyItem;

final class PreferencesPanelCades extends JPanel {

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
	private JLabel policyLabel;

	private final JCheckBox cadesImplicit = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.1")); //$NON-NLS-1$
	
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

        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;

        loadPreferences();

        loadCadesPolicy();

    	this.cadesPolicyDlg.setModificationListener(modificationListener);
    	this.cadesPolicyDlg.setKeyListener(keyListener);

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

		this.policyLabel = new JLabel(this.cadesPolicyDlg.getSelectedPolicyName());
		this.policyLabel.setLabelFor(policyConfigButton);

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
		policyConfigPanel.add(this.policyLabel);
		policyConfigPanel.add(policyConfigButton);

        ///////////// Fin Panel Policy ////////////////

		c.gridy++;
        add(policyConfigPanel, c);
        
        ///////////// Panel Multisign ////////////////

        final JPanel multisignConfigPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints mcpc = new GridBagConstraints();
        mcpc.fill = GridBagConstraints.BOTH;
        mcpc.weightx = 1.0;
        mcpc.gridy = 0;

        multisignConfigPanel.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanel.167")) //$NON-NLS-1$
			)
		);
        
        ButtonGroup grupo = new ButtonGroup();
        grupo.add(optionCoSign);
        grupo.add(optionCounterSignLeafs);
        grupo.add(optionCounterSignTree);
        
        optionCoSign.setEnabled(!isBlocked());
    	optionCoSign.addItemListener(modificationListener);
    	optionCoSign.addKeyListener(keyListener);

        optionCounterSignLeafs.setEnabled(!isBlocked());
        optionCounterSignLeafs.addItemListener(modificationListener);
        optionCounterSignLeafs.addKeyListener(keyListener);

        optionCounterSignTree.setEnabled(!isBlocked());
        optionCounterSignTree.addItemListener(modificationListener);
        optionCounterSignTree.addKeyListener(keyListener);

        multisignConfigPanel.add(optionCoSign,mcpc);
        
        mcpc.gridy++;
        multisignConfigPanel.add(optionCounterSignLeafs,mcpc);

        mcpc.gridy++;
        multisignConfigPanel.add(optionCounterSignTree,mcpc);

        ///////////// Fin Panel Multisign ////////////////

		c.gridy++;
        add(multisignConfigPanel, c);

	    final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
	    final JPanel signatureMode = new JPanel(fLayout);
	    signatureMode.setBorder(BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.69")) //$NON-NLS-1$
			)
		);
	    this.cadesImplicit.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.45") //$NON-NLS-1$
		);
	    this.cadesImplicit.setMnemonic('i');
	    this.cadesImplicit.addItemListener(modificationListener);
	    this.cadesImplicit.addKeyListener(keyListener);
	    this.cadesImplicit.setEnabled(!isBlocked());
	    signatureMode.add(this.cadesImplicit);

	    c.gridy++;
	    add(signatureMode, c);

	    c.gridy++;
	    c.weighty = 1.0;
	    add(new JPanel(), c);

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

		add(panelGeneral, c);

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
		PreferencesManager.putBoolean(PREFERENCE_CADES_IMPLICIT, this.cadesImplicit.isSelected());
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
		this.cadesImplicit.setSelected(PreferencesManager.getBoolean(PREFERENCE_CADES_IMPLICIT));

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

		this.cadesImplicit.setSelected(PreferencesManager.getBooleanDefaultPreference(PREFERENCE_CADES_IMPLICIT));

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

		this.policyLabel.setText(this.cadesPolicyDlg.getSelectedPolicyName());

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

				this.policyLabel.setText(this.cadesPolicyDlg.getSelectedPolicyName());

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
