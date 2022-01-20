/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.ietf.jgss.Oid;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Panel con los componentes para la configuracion de una pol&iacute;tica de firma. */
final class PolicyPanel extends JPanel implements ItemListener {

	/** Serial Id. */
	private static final long serialVersionUID = 4804298622744399269L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int POLICY_INDEX_NONE = 0;

	/** Algoritmos de huella digital admitidos para las pol&iacute;ticas de firma. */
	private static final String[] POLICY_HASH_ALGORITHMS = new String[] {
		"SHA1", //$NON-NLS-1$
		"SHA-512", //$NON-NLS-1$
		"SHA-384", //$NON-NLS-1$
		"SHA-256" //$NON-NLS-1$
	};

	private JComboBox<PolicyItem> policiesCombo;
	private final JTextField identifierField = new JTextField();
	private final JTextField hashField = new JTextField();
	private final JComboBox<String> hashAlgorithmField = new JComboBox<>(POLICY_HASH_ALGORITHMS);
	private final JTextField qualifierField = new JTextField();

	private final String signatureFormat;

	private AdESPolicy currentPolicy;

	private final List<PolicyItem> policies;

	/** Crea el panel de configuracion de pol&iacute;ticas de firma.
	 * @param signFormat Formato de firma.
	 * @param policies Listado de pol&iacute;ticas prefijadas.
	 * @param currentPolicy Pol&iacute;tica actualmente configurada.
	 * @param blocked Indica si debe bloquearse la configuraci&oacute;n del panel.
	 */
	PolicyPanel(final String signFormat,
			    final List<PolicyItem> policies,
			    final AdESPolicy currentPolicy,
			    final boolean blocked) {
		this(signFormat, policies, currentPolicy, true, true, true, blocked);
	}

	final boolean allowNoPolicy;
	final boolean allowCustomPolicy;

	/** Crea el panel de configuracion de pol&iacute;ticas de firma.
	 * @param signFormat Formato de firma.
	 * @param policies Listado de pol&iacute;ticas prefijadas.
	 * @param currentPolicy Pol&iacute;tica actualmente configurada.
	 * @param noPolicyOp Indica si debe permitirse seleccionar ninguna pol&iacute;tica.
	 * @param customPolicyOp Indica si debe permitirse seleccionar una pol&iacute;tica a medida.
	 * @param showFields Indica si han de mostrarse o no los campos con los datos de la
	 *                   pol&iacute;tica.
	 * @param blocked Indica si debe bloquearse la configuraci&oacute;n del panel.
	 */
	PolicyPanel(final String signFormat,
			    final List<PolicyItem> policies,
			    final AdESPolicy currentPolicy,
			    final boolean noPolicyOp,
			    final boolean customPolicyOp,
			    final boolean showFields,
			    final boolean blocked) {

		this.signatureFormat = signFormat;

		this.allowCustomPolicy = customPolicyOp;
		this.allowNoPolicy = noPolicyOp;

		// La politica actual sera la personalizada siempre que no sea una de las prefijadas
		AdESPolicy customPolicy = currentPolicy;
		if (currentPolicy != null) {
			for (final PolicyItem item : policies) {
				if (item.equals(customPolicy)) {
					customPolicy = null;
				}
			}
		}

		// Las politicas seran:
		// 0: Ninguna politica (si se permite)
		// De 1 a N: Politicas prefijadas
		// Ultima: Politica personalizada (si se permite)
		this.policies = new ArrayList<>();

		if (this.allowNoPolicy) {
			this.policies.add(
				new PolicyItem(
					SimpleAfirmaMessages.getString("PreferencesPanel.24"), //$NON-NLS-1$
					null
				)
			);
		}
		for (final PolicyItem item : policies) {
			this.policies.add(item);
		}
		if (this.allowCustomPolicy) {
			this.policies.add(
				new PolicyItem(
					SimpleAfirmaMessages.getString("PreferencesPanel.26"), //$NON-NLS-1$
					customPolicy
				)
			);
		}
		this.currentPolicy = currentPolicy;

		createUI(signFormat, showFields, blocked);
	}

	/** Crea la interfaz gr&aacute;fica del panel.
	 * @param signFormat Formato de firma.
	 * @param showFields Indica si han de mostrarse o no los campos con los datos de la
	 *                   pol&iacute;tica.
	 * @param blocked Indica si debe bloquearse la configuraci&oacute;n del panel.
	 */
	private void createUI(final String signFormat, final boolean showFields, final boolean blocked) {

		if (AOSignConstants.SIGN_FORMAT_FACTURAE.equals(this.signatureFormat)) {
			setBorder(BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanel.153") //$NON-NLS-1$
			));
		} else {
			setBorder(BorderFactory.createEmptyBorder());
		}

		setLayout(new GridBagLayout());

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.gridy = 0;

		// Los elementos del menu desplegable se identifican por su orden
		this.policiesCombo = new JComboBox<>();
		this.policiesCombo.setModel(
			new DefaultComboBoxModel<>(
				this.policies.toArray(new PolicyItem[this.policies.size()])
			)
		);

		final JLabel policyComboLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.23")); //$NON-NLS-1$
		policyComboLabel.setLabelFor(this.policiesCombo);

		// El panel de FacturaE se muestra con otro estilo
		if (AOSignConstants.SIGN_FORMAT_FACTURAE.equals(this.signatureFormat)) {
			final JPanel panel = createFacturaEPolicyPanel(policyComboLabel, this.policiesCombo);
			c.gridy++;
			add(panel, c);
		}
		else {
			c.gridy++;
			add(policyComboLabel, c);
			c.gridy++;
			add(this.policiesCombo, c);
		}

		this.policiesCombo.getAccessibleContext().setAccessibleName(
			SimpleAfirmaMessages.getString("PreferencesPanel.183") //$NON-NLS-1$
		);
		this.policiesCombo.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.47") //$NON-NLS-1$
		);

		this.policiesCombo.setEnabled(!blocked);

		if (showFields) {

			final boolean enableTextFields = this.policiesCombo.getSelectedIndex() > getNoPolicyIndex();
			final boolean editableTextFields = this.policiesCombo.getSelectedIndex() == getCustomPolicyIndex();

			this.identifierField.setEnabled(enableTextFields);
			this.identifierField.setEditable(editableTextFields);
			this.identifierField.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.54") //$NON-NLS-1$
			);
			this.identifierField.addFocusListener(
				new FocusListener() {

					/** Ajusta la introducci&oacute;n del prefijo "urn:oid" de forma autom&aacute;tica
					 * para evitar confusiones por parte del usuario. */
					@Override
					public void focusLost(final FocusEvent e) {
						if (!"XAdES".equals(signFormat)) { //$NON-NLS-1$
							PolicyPanel.this.getIdentifierField().setText(
								PolicyPanel.this.getIdentifierField().getText().replace("urn:oid:", "") //$NON-NLS-1$ //$NON-NLS-2$
							);
						}
						else {
							try {
								new Oid(PolicyPanel.this.getIdentifierField().getText());
							}
							catch(final Exception ex) {
								return;
							}
							// Es un OID, lo pasamos a URN de tipo OID
							PolicyPanel.this.getIdentifierField().setText(
								"urn:oid:" + PolicyPanel.this.getIdentifierField().getText() //$NON-NLS-1$
							);
						}
					}

					@Override
					public void focusGained(final FocusEvent e) { /* Vacio */ }
				}
			);

			final JLabel policyIdentifierLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanel." + signFormat + ".27") //$NON-NLS-1$ //$NON-NLS-2$
			);
			policyIdentifierLabel.setLabelFor(this.identifierField);
			c.gridy++;
			add(policyIdentifierLabel, c);
			c.gridy++;
			add(this.identifierField, c);

			this.hashField.setEnabled(enableTextFields);
			this.hashField.setEditable(editableTextFields);
			this.hashField.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.55") //$NON-NLS-1$
			);

			final JLabel policyIdentifierHashLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanel.28") //$NON-NLS-1$
			);
			policyIdentifierHashLabel.setLabelFor(this.hashField);
			c.gridy++;
			add(policyIdentifierHashLabel, c);
			c.gridy++;
			add(this.hashField, c);

			this.hashAlgorithmField.setEnabled(editableTextFields);
			this.hashAlgorithmField.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.50") //$NON-NLS-1$
			);

			final JLabel policyIdentifierHashAlgorithmLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanel.29") //$NON-NLS-1$
			);
			policyIdentifierHashAlgorithmLabel.setLabelFor(this.hashAlgorithmField);
			c.gridy++;
			add(policyIdentifierHashAlgorithmLabel, c);
			c.gridy++;
			add(this.hashAlgorithmField, c);

			this.qualifierField.setEnabled(enableTextFields);
			this.qualifierField.setEditable(editableTextFields);
			this.qualifierField.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanel.56") //$NON-NLS-1$
			);

			final JLabel policyQualifierLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanel.30") //$NON-NLS-1$
			);
			policyQualifierLabel.setLabelFor(this.qualifierField);
			c.gridy++;
			add(policyQualifierLabel, c);
			c.gridy++;
			add(this.qualifierField, c);

		}

		// Agregamos el listener ahora para que se configuren los parametros de la politica
		// cuando la seleccionemos a continuacion en el ComboBox
		this.policiesCombo.addItemListener(this);

		// Seleccionamos la politica actualmente configurada si la hay
		if (this.currentPolicy != null) {
			for (int i = 0; i < this.policiesCombo.getItemCount(); i++) {
				if (this.policiesCombo.getItemAt(i).equals(this.currentPolicy)) {
					this.policiesCombo.setSelectedIndex(i);
				}
			}
		}

		// Cargamos la configuracion de politica que corresponde
		loadPolicy(((PolicyItem) this.policiesCombo.getSelectedItem()).getPolicy());
	}

	private static JPanel createFacturaEPolicyPanel(final Component label, final Component combo) {
		final JPanel outerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		final JPanel innerPanel = new JPanel(new GridBagLayout());
		final GridBagConstraints feConstraints = new GridBagConstraints();
		feConstraints.anchor = GridBagConstraints.LINE_START;
		feConstraints.insets = new Insets(0, 7, 4, 7);
		feConstraints.gridx = 0;
		innerPanel.add(label, feConstraints);
		feConstraints.gridx = 1;
		innerPanel.add(combo, feConstraints);
		outerPanel.add(innerPanel);

		return outerPanel;
	}

	/** Obtiene el &iacute;ndice de la pol&iacute;tica personalizada en el listado de pol&iacute;ticas.
	 * @return &Iacute;ndice. */
	private int getCustomPolicyIndex() {
		if (!this.allowCustomPolicy) {
			return Integer.MAX_VALUE;
		}
		return this.policiesCombo.getItemCount() - 1;
	}

	/** Obtiene el &iacute;ndice de "Sin pol&iacute;tica" en el listado de pol&iacute;ticas.
	 * @return &Iacute;ndice. */
	private int getNoPolicyIndex() {
		if (!this.allowNoPolicy) {
			return -1;
		}
		return POLICY_INDEX_NONE;
	}

	/** Establece el listener que atiende a los cambios realizados en el panel de
	 * configuraci&oacute;n de pol&iacute;ticas.
	 * @param modListener Listener al que se notifican los eventos de cambio. */
	void setModificationListener(final ModificationListener modListener) {
		this.policiesCombo.addItemListener(modListener);
		this.identifierField.addKeyListener(modListener);
		this.hashField.addKeyListener(modListener);
		this.hashAlgorithmField.addItemListener(modListener);
		this.qualifierField.addKeyListener(modListener);
	}

	/** Establece el listener que atiende a los cambios realizados en el panel de
	 * configuraci&oacute;n de pol&iacute;ticas.
	 * @param keyListener Listener al que se notifican los eventos de cambio. */
	void setKeyListener(final KeyListener keyListener) {

		this.identifierField.addKeyListener(keyListener);
		this.hashField.addKeyListener(keyListener);
		this.hashAlgorithmField.addKeyListener(keyListener);
		this.qualifierField.addKeyListener(keyListener);
	}

	/** {@inheritDoc} */
	@Override
	public void itemStateChanged(final ItemEvent ie) {

		if (ie != null && ie.getStateChange() == ItemEvent.DESELECTED){
			return;
		}

		// Cambiamos las selecciones necesarias
		final boolean enabled = this.policiesCombo.getSelectedIndex() > getNoPolicyIndex();
		final boolean editable = this.policiesCombo.getSelectedIndex() == getCustomPolicyIndex();

		this.identifierField.setEnabled(enabled);
		this.hashField.setEnabled(enabled);
		this.qualifierField.setEnabled(enabled);

		this.identifierField.setEditable(editable);
		this.hashField.setEditable(editable);
		this.qualifierField.setEditable(editable);

		// El combo de algoritmos se podra seleccionar cuando este activado
		this.hashAlgorithmField.setEnabled(editable);

		// Mostramos la configuracion de politica que corresponde
		loadPolicy(((PolicyItem) this.policiesCombo.getSelectedItem()).getPolicy());
	}

	/** Carga los datos de una pol&iacute;tica en el panel.
	 * @param policy Pol&iacute;tica a cargar. */
	void loadPolicy(final AdESPolicy policy) {
		if (policy != null) {
			this.identifierField.setText(policy.getPolicyIdentifier());
			this.hashField.setText(policy.getPolicyIdentifierHash());
			this.hashAlgorithmField.setSelectedItem(policy.getPolicyIdentifierHashAlgorithm());
			this.qualifierField.setText(
				policy.getPolicyQualifier() != null ?
					policy.getPolicyQualifier().toString() :
						"" //$NON-NLS-1$
			);
		}
		else {
			this.identifierField.setText(""); //$NON-NLS-1$
			this.hashField.setText(""); //$NON-NLS-1$
			this.qualifierField.setText(""); //$NON-NLS-1$
		}
	}

	/** Guarda la configuraci&oacute;n actual dentro el contexto.
	 * <b>No guarda en las preferencias</b>. */
	void saveCurrentPolicy() {
		final AdESPolicy policy = getSelectedPolicy();
		if (this.policiesCombo.getSelectedIndex() == getCustomPolicyIndex()) {
			((PolicyItem) this.policiesCombo.getSelectedItem()).setPolicy(policy);
		}
		else if (this.policiesCombo.getItemAt(getCustomPolicyIndex()) != null) {
			this.policiesCombo.getItemAt(getCustomPolicyIndex()).setPolicy(null);
		}
		this.currentPolicy = policy;
	}

	/** Crea un objeto de configuracion con los valores actuales.
	 * @return Configuraci&oacute;n de pol&iacute;tica actualmente establecida o
	 * {@code null} en caso de no especificarse ninguna.
	 * @throws IllegalArgumentException Cuando la configuraci&oacute;n actual no
	 * es v&aacute;lida para una pol&iacute;tica. */
	AdESPolicy getSelectedPolicy() {
		if (this.policiesCombo.getSelectedIndex() <= getNoPolicyIndex()) {
			return null;
		}
		return new AdESPolicy(
			this.identifierField.getText(),
			this.hashField.getText(),
			this.hashAlgorithmField.getSelectedItem().toString(),
			this.qualifierField.getText()
		);
	}


	/**
	 * Devuelve la política seleccionada en el combo de pol&iacute;ticas
	 * @return La pol&iacute;tica seleccionada
	 */
	public String getSelectedPolicyName() {

		return ((PolicyItem)this.policiesCombo.getSelectedItem()).getName();

	}

	public PolicyItem getCurrentPolicyItem() {
		return (PolicyItem) this.policiesCombo.getSelectedItem();
	}

	JTextField getIdentifierField() {
		return this.identifierField;
	}

}
