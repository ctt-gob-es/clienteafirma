package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_KEYSTORE_ACCEPTED_POLICIES_ONLY_CERTS;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.logging.Logger;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.ietf.jgss.GSSException;
import org.ietf.jgss.Oid;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

final class CertPoliciesDialog extends JDialog {

	private static final long serialVersionUID = 4938188524529369282L;

	private static final String OID_SEPARATOR = ";"; //$NON-NLS-1$

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final JButton removeButton = new JButton(
		SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.7") //$NON-NLS-1$
	);

	private final JButton acceptButton = new JButton(
		SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.9") //$NON-NLS-1$
	);

	private final JCheckBox certPoliciesCheckBox = new JCheckBox(
		SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.12") //$NON-NLS-1$
	);
	boolean isCertPoliciesCheckBoxSelected() {
		return this.certPoliciesCheckBox.isSelected();
	}

	private final JTextField oidTextField = new JTextField();
	String getOidTextFieldText() {
		return this.oidTextField.getText().trim();
	}
	void setOidTextFieldText(final String text) {
		this.oidTextField.setText(text);
	}
	void clearOidTextFieldText() {
		this.oidTextField.setText(""); //$NON-NLS-1$
	}

	private final DefaultListModel<CustomOid> listModel = new DefaultListModel<>();
	void addOid(final CustomOid oid) {
		this.listModel.removeElement(oid);
		this.listModel.addElement(oid);
		this.removeButton.setEnabled(true);
	}
	void removeOid(final CustomOid oid) {
		this.listModel.removeElement(oid);
		if (!hasOidsAdded()) {
			this.removeButton.setEnabled(false);
		}
	}
	boolean hasOidsAdded() {
		return this.listModel.getSize() > 0;
	}

	private final JList<CustomOid> oidList = new JList<>(this.listModel);
	CustomOid getSelectedOid() {
		return this.oidList.getSelectedValue();
	}
	void setSelectedOid(final CustomOid oid) {
		if (oid != null) {
			this.oidList.setSelectedValue(oid, true);
		}
	}

	private final JButton addButton = new JButton(
		SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.4") //$NON-NLS-1$
	);
	void setEnabledAddButton(final boolean enabled) {
		this.addButton.setEnabled(enabled);
	}

	private void loadConfiguration() {

		this.certPoliciesCheckBox.setSelected(
			PreferencesManager.getBoolean(PREFERENCE_KEYSTORE_ACCEPTED_POLICIES_ONLY_CERTS, false)
		);

		final String oidString = PreferencesManager.get(
			PreferencesManager.PREFERENCE_KEYSTORE_ACCEPTED_POLICIES_LIST,
			"" //$NON-NLS-1$
		);

		if (oidString == null || oidString.isEmpty()) {
			this.removeButton.setEnabled(false);
		}
		else {
			final String[] oidStrings = oidString.split(OID_SEPARATOR);
			for (final String oidStr : oidStrings) {
				final CustomOid oid;
				try {
					oid = new CustomOid(oidStr);
				}
				catch(final Exception e) {
					LOGGER.severe(
						"La configuracion contenia un OID invalido (" + oidStr + "), se ignorara: " + e //$NON-NLS-1$ //$NON-NLS-2$
					);
					continue;
				}
				addOid(oid);
			}
			if (!hasOidsAdded()) {
				this.removeButton.setSelected(false);
			}
			else {
				this.oidList.setSelectedIndex(0);
			}
		}
		enableFields(isCertPoliciesCheckBoxSelected());
	}

	void saveConfiguration() {
		PreferencesManager.putBoolean(PREFERENCE_KEYSTORE_ACCEPTED_POLICIES_ONLY_CERTS, isCertPoliciesCheckBoxSelected());

		final StringBuilder sb = new StringBuilder();
		for (int i=0;i<this.listModel.getSize();i++) {
			sb.append(this.listModel.getElementAt(i).toCanonicalizedString());
			sb.append(OID_SEPARATOR);
		}
		final String ret = sb.toString();
		if (!ret.isEmpty()) {
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_KEYSTORE_ACCEPTED_POLICIES_LIST,
				ret.substring(0, ret.length()-1)
			);
		}
	}

	void enableFields(final boolean enable) {
		this.oidTextField.setEnabled(enable);
		this.removeButton.setEnabled(enable);
		this.oidList.setEnabled(enable);
	}

	/** Inicia el proceso de creaci&oacute;n de di&aacute;logo de pol&iacute;ticas de certificaci&oacute;n.
	 * @param parent Componente padre para la modalidad. */
	static void startCertPoliciesDialog(final Frame parent) {
		final CertPoliciesDialog cpd = new CertPoliciesDialog(parent);
		cpd.loadConfiguration();
		cpd.setSize(600, 320);
		cpd.setResizable(false);
		cpd.setLocationRelativeTo(parent);
		cpd.setVisible(true);
	}



	private CertPoliciesDialog(final Frame parentFrame) {
		super(parentFrame);
		setTitle(SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.0")); //$NON-NLS-1$
		setModalityType(ModalityType.APPLICATION_MODAL);
		SwingUtilities.invokeLater(
			new Runnable() {
				@Override
				public void run() {
					createUI();
				}
			}
		);
	}

	void createUI() {

		final Container c = getContentPane();
		final GridBagLayout gbl = new GridBagLayout();
		c.setLayout(gbl);
		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;
        gbc.insets = new Insets(10,10,10,10);
		setIconImage(
			AutoFirmaUtil.getDefaultDialogsIcon()
		);
		getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.1") //$NON-NLS-1$
		);

		this.certPoliciesCheckBox.setMnemonic('m');
		this.certPoliciesCheckBox.addChangeListener(
			new ChangeListener() {
				@Override
				public void stateChanged(final ChangeEvent e) {
					if (isCertPoliciesCheckBoxSelected()) {
						enableFields(true);
					}
					else {
						enableFields(false);
					}
				}
			}
		);

		final JLabel infoLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.2")); //$NON-NLS-1$
		infoLabel.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.2") //$NON-NLS-1$
		);

		final JLabel textFieldLabel = new JLabel(
			SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.3") //$NON-NLS-1$
		);
		textFieldLabel.setLabelFor(this.oidTextField);
		this.oidTextField.setColumns(10);
		this.oidTextField.getDocument().addDocumentListener(
			new DocumentListener() {

				@SuppressWarnings("unused")
				private void checkAdd() {
					if (getOidTextFieldText() != null && !getOidTextFieldText().trim().isEmpty()) {
						String oid = getOidTextFieldText().trim();
						if (getOidTextFieldText().startsWith("urn:oid:")) { //$NON-NLS-1$
							oid = getOidTextFieldText().replace("urn:oid:", ""); //$NON-NLS-1$ //$NON-NLS-2$
						}
						try {
							new Oid(oid);
							setEnabledAddButton(true);
						}
						catch(final GSSException e) {
							setEnabledAddButton(false);
						}
					}
					else {
						setEnabledAddButton(false);
					}
				}

				@Override
				public void insertUpdate(final DocumentEvent e) {
					checkAdd();
				}

				@Override
				public void removeUpdate(final DocumentEvent e) {
					checkAdd();
				}

				@Override
				public void changedUpdate(final DocumentEvent e) {
					checkAdd();
				}
			}
		);

		this.addButton.setEnabled(false);
		this.addButton.setMnemonic('g');
		this.addButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					String oidText = getOidTextFieldText().trim();
					if (getOidTextFieldText().startsWith("urn:oid:")) { //$NON-NLS-1$
						oidText = getOidTextFieldText().substring("urn:oid:".length()).trim(); //$NON-NLS-1$
					}
					clearOidTextFieldText();
					final CustomOid oid;
					try {
						oid = new CustomOid(
							oidText
						);
					}
					catch (final Exception e) {
						LOGGER.severe("Error creando el OID ('" + oidText + "'): " + e); //$NON-NLS-1$ //$NON-NLS-2$
						return;
					}
					addOid(oid);
					setSelectedOid(oid);
				}
			}
		);
		this.addButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.5") //$NON-NLS-1$
		);

		final JLabel oidListLabel = new JLabel(
			SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.6") //$NON-NLS-1$
		);
		oidListLabel.setLabelFor(this.oidList);

		final JScrollPane scrollPane = new JScrollPane(
			this.oidList,
			ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
			ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED
		);
		scrollPane.setPreferredSize(new Dimension(80, 100));

		this.removeButton.setMnemonic('Q');
		this.removeButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					removeOid(getSelectedOid());
				}
			}
		);
		this.removeButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.8") //$NON-NLS-1$
		);

		this.acceptButton.setMnemonic('A');
		this.acceptButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					// Comprobamos si ha activado el filtro, para estar seguros de que en
					// esta caso haya al menos una politica
					if (!hasOidsAdded() && isCertPoliciesCheckBoxSelected()) {
						AOUIFactory.showErrorMessage(
							CertPoliciesDialog.this,
							SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.14"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.13"), //$NON-NLS-1$
							JOptionPane.WARNING_MESSAGE
						);
					}
					else {
						saveConfiguration();
						CertPoliciesDialog.this.setVisible(false);
						CertPoliciesDialog.this.dispose();
					}
				}
			}
		);
		this.acceptButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.10") //$NON-NLS-1$
		);

		final JButton closeButton = new JButton(
			SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.11") //$NON-NLS-1$
		);

		closeButton.setMnemonic('C');
		closeButton.addActionListener(
			new ActionListener () {
				@Override
				public void actionPerformed(final ActionEvent e) {
					CertPoliciesDialog.this.setVisible(false);
					CertPoliciesDialog.this.dispose();
				}
			}
		);
		closeButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanelTrustedSites.12") //$NON-NLS-1$
		);

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(closeButton);
			panel.add(this.acceptButton);
		}
		else {
			panel.add(this.acceptButton);
			panel.add(closeButton);
		}

		add(this.certPoliciesCheckBox, gbc);
		gbc.gridy++;
		gbc.gridwidth = 2;
		gbc.insets = new Insets(10,40,10,10);
		add(infoLabel, gbc);
		gbc.gridwidth = 1;
		gbc.gridy++;
		gbc.insets = new Insets(5,40,0,10);
		add(textFieldLabel, gbc);
		gbc.gridy++;
		add(this.oidTextField, gbc);
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		add(this.addButton, gbc);
		gbc.insets = new Insets(10,40,0,10);
		gbc.gridy++;
		gbc.fill = GridBagConstraints.BOTH;
		add(oidListLabel, gbc);
		gbc.insets = new Insets(5,40,0,10);
		gbc.gridy++;
		add(scrollPane, gbc);
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.PAGE_START;
		add(this.removeButton, gbc);
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(10,40,0,10);
		gbc.gridy++;
		gbc.gridwidth = 2;
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		add(panel, gbc);
		pack();
	}

	private static final class CustomOid extends Oid {

		private static final Dictionary<String, String> KNOWN_OIDS = new Hashtable<>();
		static {
			KNOWN_OIDS.put("1.3.6.1.4.1.5734.3.10.1"    , "Certificado de identidad de persona f\u00EDsica para ciudadano de FNMT-RCM"); //$NON-NLS-1$ //$NON-NLS-2$
			KNOWN_OIDS.put("1.3.6.1.4.1.5734.3.5"       , "Certificado de identidad de persona f\u00EDsica de Clase 2 de FNMT-RCM"); //$NON-NLS-1$ //$NON-NLS-2$
			KNOWN_OIDS.put("1.3.6.1.4.1.5734.3.7"       , "Certificado de persona jur\u00EDdica de Clase 2 de FNMT-RCM"); //$NON-NLS-1$ //$NON-NLS-2$
			KNOWN_OIDS.put("1.3.6.1.4.1.5734.3.3.4.4.1" , "Certificado FNMT-RCM en tarjeta para empleado p\u00FAblico"); //$NON-NLS-1$ //$NON-NLS-2$
			KNOWN_OIDS.put("1.3.6.1.4.1.5734.3.3.4.4.2" , "Certificado FNMT-RCM en formato software para empleado p\u00FAblico"); //$NON-NLS-1$ //$NON-NLS-2$
			KNOWN_OIDS.put("2.16.724.1.2.2.2.3"         , "Certificado de firma del Documento Nacional de Identidad Electr\u00F3nico (DNIe)"); //$NON-NLS-1$ //$NON-NLS-2$
			KNOWN_OIDS.put("2.16.724.1.2.2.2.4"         , "Certificado de autenticaci\u00F3n del Documento Nacional de Identidad Electr\u00F3nico (DNIe)"); //$NON-NLS-1$ //$NON-NLS-2$
			KNOWN_OIDS.put("1.3.6.1.4.1.27781.2.4.4.1.3", "Certificado de firma de empleado p\u00FAblico (MEYSS)"); //$NON-NLS-1$ //$NON-NLS-2$
			KNOWN_OIDS.put("1.3.6.1.4.1.27781.2.4.4.2.3", "Certificado de autenticaci\u00F3n de empleado p\u00FAblico (MEYSS)"); //$NON-NLS-1$ //$NON-NLS-2$
			KNOWN_OIDS.put("2.16.724.1.1.1.1.3.2"       , "Certificado reconocido de firma electr\u00F3nica para persona f\u00EDsica expedido por el Ministerio de Defensa de Espa\u00F1a"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		CustomOid(final String strOid) throws GSSException {
			super(strOid);
		}

		String toCanonicalizedString() {
			return super.toString();
		}

		@Override
		public String toString() {
			final String suffix = KNOWN_OIDS.get(toCanonicalizedString());
			if (suffix == null) {
				return toCanonicalizedString();
			}
			return toCanonicalizedString() + " (" + suffix + ")"; //$NON-NLS-1$ //$NON-NLS-2$
		}

	}

}
