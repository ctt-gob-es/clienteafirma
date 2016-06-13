package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGN_FORMAT;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.List;

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

	private static final int POLICY_INDEX_NONE = 0;

	/** Algoritmos de huella digital admitidos para las pol&iacute;ticas de firma. */
	private static final String[] POLICY_HASH_ALGORITHMS = new String[] {
		"SHA1", //$NON-NLS-1$
		"SHA-512", //$NON-NLS-1$
		"SHA-384", //$NON-NLS-1$
		"SHA-256" //$NON-NLS-1$
	};

	private JComboBox<PolicyItem> policiesCombo;
	public PolicyItem getCurrentPolicyItem() {
		return (PolicyItem) this.policiesCombo.getSelectedItem();
	}

	private final JTextField identifierField = new JTextField();
	JTextField getIdentifierField() {
		return this.identifierField;
	}

	private final JTextField hashField = new JTextField();
	private final JComboBox<String> hashAlgorithmField = new JComboBox<>(POLICY_HASH_ALGORITHMS);
	private final JTextField qualifierField = new JTextField();

	/** Men&uacute; desplegable con la selecci&oacute;n de sub-formato de firma.
	 * <p>
	 *  En el caso de PAdES, si hay una pol&iacute;tica establecida, sea la que sea,
	 *  es necesario modificarlo para que refleje PAdES-BES y que no se pueda cambiar
	 *  a PAdES-B&aacute;sico.
	 * </p>
	 * <p>
	 *  En el caso de XAdES, si la pol&iacute;tica de firma es la de la AGE, hay que
	 *  restringir al tipo <i>Enveloped</i>.
	 * </p> */
	private final JComboBox<Object> subFormatCombo;

	private final String signatureFormat;

	private AdESPolicy currentPolicy;

	private final List<PolicyItem> policies;

	/** Crea el panel de configuracion de pol&iacute;ticas de firma.
	 * @param signFormat Formato de firma.
	 * @param policies Listado de pol&iacute;ticas prefijadas.
	 * @param currentPolicy Pol&iacute;tica actualmente configurada.
	 * @param adesSubFormat Men&uacute; desplegable con la selecci&oacute;n de
	 *                      sub-formato de firma.
	 * <p>
	 *  En el caso de PAdES, si hay una pol&iacute;tica establecida, sea la que sea,
	 *  es necesario modificarlo para que refleje PAdES-BES y que no se pueda cambiar
	 *  a PAdES-B&aacute;sico.
	 * </p>
	 * <p>
	 *  En el caso de XAdES, si la pol&iacute;tica de firma es la de la AGE, hay que
	 *  restringir al tipo <i>Enveloped</i>, dej&aacute;ndolo libre en caso de pol&iacute;tica
	 *  a medida o sin pol&iacute;tica.
	 * </p>
	 * @param unprotected <code>true</code> para permitir al usuario realizar cualquier modificaci&oacute;n en las preferencias,
	 *                    <code>false</code> para limitar las preferencias que puede modificar. */
	PolicyPanel(final String signFormat,
			    final List<PolicyItem> policies,
			    final AdESPolicy currentPolicy,
			    final JComboBox<Object> adesSubFormat,
			    final boolean unprotected) {
		this(signFormat, policies, currentPolicy, adesSubFormat, true, true, true, unprotected);
	}

	final boolean allowNoPolicy;
	final boolean allowCustomPolicy;

	/** Crea el panel de configuracion de pol&iacute;ticas de firma.
	 * @param signFormat Formato de firma.
	 * @param policies Listado de pol&iacute;ticas prefijadas.
	 * @param currentPolicy Pol&iacute;tica actualmente configurada.
	 * @param adesSubFormat Men&uacute; desplegable con la selecci&oacute;n de
	 *                      sub-formato de firma.
	 * <p>
	 *  En el caso de PAdES, si hay una pol&iacute;tica establecida, sea la que sea,
	 *  es necesario modificarlo para que refleje PAdES-BES y que no se pueda cambiar
	 *  a PAdES-B&aacute;sico.
	 * </p>
	 * <p>
	 *  En el caso de XAdES, si la pol&iacute;tica de firma es la de la AGE, hay que
	 *  restringir al tipo <i>Enveloped</i>, dej&aacute;ndolo libre en caso de pol&iacute;tica
	 *  a medida o sin pol&iacute;tica.
	 * </p>
	 * @param noPolicyOp Indica si debe permitirse seleccionar ninguna pol&iacute;tica.
	 * @param customPolicyOp Indica si debe permitirse seleccionar una pol&iacute;tica a medida.
	 * @param showFields Indica si han de mostrarse o no los campos con los datos de la
	 *                   pol&iacute;tica.
	 * @param unprotected <code>true</code> para permitir al usuario realizar cualquier modificaci&oacute;n en las preferencias,
	 *                    <code>false</code> para limitar las preferencias que puede modificar. */
	PolicyPanel(final String signFormat,
			    final List<PolicyItem> policies,
			    final AdESPolicy currentPolicy,
			    final JComboBox<Object> adesSubFormat,
			    final boolean noPolicyOp,
			    final boolean customPolicyOp,
			    final boolean showFields,
			    final boolean unprotected) {

		this.subFormatCombo = adesSubFormat;
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

		createUI(signFormat, showFields, unprotected);
	}

	/** Crea la interfaz gr&aacute;fica del panel.
	 * @param signFormat Formato de firma.
	 * @param showFields Indica si han de mostrarse o no los campos con los datos de la
	 *                   pol&iacute;tica.
	 * @param unprotected <code>true</code> para permitir al usuario realizar cualquier modificaci&oacute;n en las preferencias,
	 *                    <code>false</code> para limitar las preferencias que puede modificar. */
	private void createUI(final String signFormat, final boolean showFields, final boolean unprotected) {
		setBorder(
			BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.23") //$NON-NLS-1$
			)
		);
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
		this.policiesCombo.setEnabled(unprotected);

		add(this.policiesCombo, c);
		this.policiesCombo.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.47") //$NON-NLS-1$
		);

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
					@SuppressWarnings("unused")
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

			if (enableTextFields && !unprotected) {
				this.identifierField.setEnabled(unprotected);
				this.hashField.setEnabled(unprotected);
				this.hashAlgorithmField.setEnabled(unprotected);
				this.qualifierField.setEnabled(unprotected);
			}

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

		// Si es PAdES hay que seleccionar PAdES-BES si hay politica, tanto si es AGE como
		// si es a medida.
		// Si es XAdES y la politica es AGE, hay que seleccionar Enveloped.
		if (this.subFormatCombo != null) {
			if (enabled && !editable && AOSignConstants.SIGN_FORMAT_XADES.equals(this.signatureFormat)) {
				this.subFormatCombo.setSelectedItem(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
		        this.subFormatCombo.removeItemAt(1);
				this.subFormatCombo.setEnabled(true);
			}
			else if (enabled && !editable || editable && AOSignConstants.SIGN_FORMAT_PADES.equalsIgnoreCase(this.signatureFormat)) {
				if (this.subFormatCombo.getItemCount() > 0) {
					this.subFormatCombo.setSelectedIndex(0);
				}
				this.subFormatCombo.setEnabled(false);
			}
			else {
				this.subFormatCombo.setSelectedItem(
					PreferencesManager.get(PREFERENCE_XADES_SIGN_FORMAT, AOSignConstants.SIGN_FORMAT_XADES_DETACHED)
				);
		        this.subFormatCombo.insertItemAt(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, 1);
				this.subFormatCombo.setEnabled(true);
			}
		}

	}

	/** Carga los datos de una pol&iacute;tica en el panel.
	 * @param policy Pol&iacute;tica a cargar. */
	private void loadPolicy(final AdESPolicy policy) {
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

	/** Pol&iacute;tica de firma definida por los valores que deben establecerse para la misma y un nombre
	 * que la define de cara al usuario. */
	static final class PolicyItem {

		private final String name;
		private AdESPolicy policy;

		/** Construye el elemento con nombre y configuraci&oacute;n de pol&iacute;tica.
		 * @param name Nombre de la configuraci&oacute;n.
		 * @param policy Configuraci&oacute;n. */
		PolicyItem(final String name, final AdESPolicy policy) {
			this.name = name;
			this.policy = policy;
		}

		/** Recupera la configuraci&oacute;n de pol&iacute;tica.
		 * @return Configuraci&oacute;n. */
		AdESPolicy getPolicy() {
			return this.policy;
		}

		/** Establece la configuraci&oacute;n de pol&iacute;tica.
		 * @param policy Configuraci&oacute;n. */
		void setPolicy(final AdESPolicy policy) {
			this.policy = policy;
		}

		@Override
		public boolean equals(final Object obj) {

			if (obj instanceof AdESPolicy) {
				return this.policy != null && this.policy.equals(obj);
			}
			else if (obj instanceof PolicyItem) {
				final PolicyItem item = (PolicyItem) obj;
				return this.name.equals(item.name) &&
						(this.policy == null && item.getPolicy() == null ||
								this.policy.equals(item.getPolicy()));
			}
			return false;
		}

		@Override
		public String toString() {
			return this.name;
		}

		@Override
		public int hashCode() {
			return super.hashCode();
		}
	}

	/** Guarda la configuraci&oacute;n actual dentro el contexto.
	 * <b>No guarda en las preferencias</b>. */
	void saveCurrentPolicy() {
		final AdESPolicy policy = getCurrentPolicy();
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
	AdESPolicy getCurrentPolicy() {
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
}
