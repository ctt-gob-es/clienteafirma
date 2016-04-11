package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_TIMESTAMP_CERT_REQUIRED;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_TIMESTAMP_CONFIGURE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_TIMESTAMP_OID_CRITICAL;

import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.ietf.jgss.GSSException;
import org.ietf.jgss.Oid;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

final class TimeStampPadesDialog extends JDialog  {

	private static final long serialVersionUID = -6509727689395731686L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final JButton acceptButton = new JButton(
		SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.2") //$NON-NLS-1$
	);

	private final JCheckBox timeStampCheckBox = new JCheckBox(
		SimpleAfirmaMessages.getString("PreferencesPanel.122") //$NON-NLS-1$
	);
	void setTimeStampCheckBoxSelected(final boolean selected) {
		this.timeStampCheckBox.setSelected(selected);
	}
	boolean isTimeStampCheckBoxSelected() {
		return this.timeStampCheckBox.isSelected();
	}

	private final JComboBox<TimeStampPdfTypeResource> tsType = new JComboBox<>(
			TimeStampPdfTypeResource.getAllTimeStampPdfTypeResources()
	);
	JComboBox<TimeStampPdfTypeResource> getTsType() {
		return this.tsType;
	}

	private static final String[] HASH_ALGOS = new String[] {
		"SHA-1", //$NON-NLS-1$
		"SHA-256", //$NON-NLS-1$
		"SHA-384", //$NON-NLS-1$
		"SHA-512" //$NON-NLS-1$
	};

	private final JComboBox<String> tsaHashAlgorithms = new JComboBox<>(HASH_ALGOS);
	String getSelectedHashAlgorithm() {
		return this.tsaHashAlgorithms.getSelectedItem().toString();
	}

	private final JTextField tsaUrlTextField = new JTextField();
	String getTsaUrlText() {
		return this.tsaUrlTextField.getText().trim();
	}

	private final JTextField tsaPolicyTextField = new JTextField();
	String getTsaPolicyText() {
		return this.tsaPolicyTextField.getText().trim();
	}

	private final JCheckBox certRequiredCheckBox = new JCheckBox(
		SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.7") //$NON-NLS-1$
	);
	boolean isCertRequiredCheckBoxSelected() {
		return this.certRequiredCheckBox.isSelected();
	}

	private final JTextField tsaUsrTextField = new JTextField();
	String getTsaUsrText() {
		return this.tsaUsrTextField.getText().trim();
	}

	private final JPasswordField tsaPwdField = new JPasswordField(10);

	JPasswordField getPasswordField() {
		return this.tsaPwdField;
	}

	String getPassword() {
		return String.valueOf(this.tsaPwdField.getPassword());
	}

	private final JTextField tsaExtensionOidField = new JTextField();
	String getTsaExtensionOid() {
		return this.tsaExtensionOidField.getText().trim();
	}

	private final JTextField tsaExtensionValueBase64Field = new JTextField();
	String getTsaExtensionValueBase64Field() {
		return this.tsaExtensionValueBase64Field.getText().trim();
	}

	private final JCheckBox extensionCriticalCheckBox = new JCheckBox(
		SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.12") //$NON-NLS-1$
	);
	boolean isExtensionCriticalCheckBoxSelected() {
		return this.extensionCriticalCheckBox.isSelected();
	}

	/** Inicia el proceso de creaci&oacute;n de di&aacute;logo de pol&iacute;ticas de certificaci&oacute;n.
	 * @param parent Componente padre para la modalidad. */
	static void startTimeStampPadesDialog(final Frame parent) {
		final TimeStampPadesDialog tsd = new TimeStampPadesDialog(parent);
		tsd.loadConfiguration();
		tsd.setSize(720, 650);
		tsd.setResizable(false);
		tsd.setLocationRelativeTo(parent);
		tsd.setVisible(true);
	}

	private void loadConfiguration() {
		setTimeStampCheckBoxSelected(
			PreferencesManager.getBoolean(
				PREFERENCE_PADES_TIMESTAMP_CONFIGURE,
				this.timeStampCheckBox.isSelected()
			)
		);

		final TimeStampPdfTypeResource type = TimeStampPdfTypeResource.getName(
			Integer.parseInt(
				PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_TIMESTAMP_STAMP_TYPE,
					Integer.toString(TimeStampPdfTypeResource.SIGN.getIndex())
				)
			)
		);
		this.tsType.setSelectedItem(type);

		final String hash = PreferencesManager.get(
			PreferencesManager.PREFERENCE_PADES_TIMESTAMP_HASHALGORITHM,
			"" //$NON-NLS-1$
		);
		if (hash != null && !hash.isEmpty()) {
			this.tsaHashAlgorithms.setSelectedItem(hash);
		}

		final String url = PreferencesManager.get(
			PreferencesManager.PREFERENCE_PADES_TIMESTAMP_TSA_URL,
			"" //$NON-NLS-1$
		);
		if (url != null && !url.isEmpty()) {
			this.tsaUrlTextField.setText(url);
		}

		final String policy = PreferencesManager.get(
			PreferencesManager.PREFERENCE_PADES_TIMESTAMP_STAMP_POLICY,
			"" //$NON-NLS-1$
		);
		if (policy != null && !policy.isEmpty()) {
			this.tsaPolicyTextField.setText(policy);
		}

		this.certRequiredCheckBox.setSelected(
			PreferencesManager.getBoolean(PREFERENCE_PADES_TIMESTAMP_CERT_REQUIRED, true)
		);

		final String usr = PreferencesManager.get(
			PreferencesManager.PREFERENCE_PADES_TIMESTAMP_TSA_USR,
			"" //$NON-NLS-1$
		);
		if (usr != null && !usr.isEmpty()) {
			this.tsaUsrTextField.setText(usr);
		}

		final String pwd = PreferencesManager.get(
			PreferencesManager.PREFERENCE_PADES_TIMESTAMP_TSA_PWD,
			"" //$NON-NLS-1$
		);
		if (pwd != null && !pwd.isEmpty()) {
			this.tsaPwdField.setText(pwd);
		}

		final String oid = PreferencesManager.get(
			PreferencesManager.PREFERENCE_PADES_TIMESTAMP_EXTENSION_OID,
			"" //$NON-NLS-1$
		);
		if (oid != null && !oid.isEmpty()) {
			this.tsaExtensionOidField.setText(oid);
		}

		final String extValue = PreferencesManager.get(
			PreferencesManager.PREFERENCE_PADES_TIMESTAMP_EXTENSION_VALUE,
			"" //$NON-NLS-1$
		);
		if (extValue != null && !extValue.isEmpty()) {
			this.tsaExtensionValueBase64Field.setText(extValue);
		}

		this.extensionCriticalCheckBox.setSelected(
			PreferencesManager.getBoolean(PREFERENCE_PADES_TIMESTAMP_OID_CRITICAL, false)
		);
		enableFields(isTimeStampCheckBoxSelected());
	}

	void SaveConfiguration() {
		PreferencesManager.putBoolean(PREFERENCE_PADES_TIMESTAMP_CONFIGURE, this.timeStampCheckBox.isSelected());

		PreferencesManager.put(
			PreferencesManager.PREFERENCE_PADES_TIMESTAMP_STAMP_TYPE,
			Integer.toString(
					((TimeStampPdfTypeResource)getTsType().getSelectedItem()).getIndex()
			)
		);
		PreferencesManager.put(
			PreferencesManager.PREFERENCE_PADES_TIMESTAMP_HASHALGORITHM,
			getSelectedHashAlgorithm()
		);

		if (getTsaUrlText() != null && !getTsaUrlText().isEmpty()) {
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PADES_TIMESTAMP_TSA_URL,
				getTsaUrlText()
			);
		}
		else {
			PreferencesManager.remove(PreferencesManager.PREFERENCE_PADES_TIMESTAMP_TSA_URL);
		}

		if (getTsaPolicyText() != null && !getTsaPolicyText().isEmpty()) {
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PADES_TIMESTAMP_STAMP_POLICY,
				getTsaPolicyText()
			);
		}
		else {
			PreferencesManager.remove(PreferencesManager.PREFERENCE_PADES_TIMESTAMP_STAMP_POLICY);
		}

		PreferencesManager.putBoolean(
			PreferencesManager.PREFERENCE_PADES_TIMESTAMP_CERT_REQUIRED,
			isCertRequiredCheckBoxSelected()
		);

		if (getTsaUsrText() != null && !getTsaUsrText().isEmpty()) {
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PADES_TIMESTAMP_TSA_USR,
				getTsaUsrText()
			);
		}
		else {
			PreferencesManager.remove(PreferencesManager.PREFERENCE_PADES_TIMESTAMP_TSA_USR);
		}

		if (getPassword() != null && !getPassword().isEmpty()) {
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PADES_TIMESTAMP_TSA_PWD,
				getPassword()
			);
		}
		else {
			PreferencesManager.remove(PreferencesManager.PREFERENCE_PADES_TIMESTAMP_TSA_PWD);
		}

		if (getTsaExtensionOid() != null && !getTsaExtensionOid().isEmpty()) {
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PADES_TIMESTAMP_EXTENSION_OID,
				getTsaExtensionOid()
			);
		}
		else {
			PreferencesManager.remove(PreferencesManager.PREFERENCE_PADES_TIMESTAMP_EXTENSION_OID);
		}

		if (getTsaExtensionValueBase64Field() != null && !getTsaExtensionValueBase64Field().isEmpty()) {
			PreferencesManager.put(
				PreferencesManager.PREFERENCE_PADES_TIMESTAMP_EXTENSION_VALUE,
				getTsaExtensionValueBase64Field()
			);
		}
		else {
			PreferencesManager.remove(PreferencesManager.PREFERENCE_PADES_TIMESTAMP_EXTENSION_VALUE);
		}

		PreferencesManager.putBoolean(
			PreferencesManager.PREFERENCE_PADES_TIMESTAMP_OID_CRITICAL,
			isExtensionCriticalCheckBoxSelected()
		);
	}

	void enableFields(final boolean enable) {
		this.tsType.setEnabled(enable);
		this.tsaHashAlgorithms.setEnabled(enable);
		this.tsaUrlTextField.setEnabled(enable);
		this.tsaPolicyTextField.setEnabled(enable);
		this.tsaUsrTextField.setEnabled(enable);
		this.tsaPwdField.setEnabled(enable);
		this.tsaExtensionOidField.setEnabled(enable);
		this.tsaExtensionValueBase64Field.setEnabled(enable);
		this.certRequiredCheckBox.setEnabled(enable);
		this.extensionCriticalCheckBox.setEnabled(enable);
		if (getTsaUrlText() != null && !getTsaUsrText().isEmpty() && enable) {
			this.tsaPwdField.setEnabled(true);
			this.tsaExtensionValueBase64Field.setEnabled(true);
			this.extensionCriticalCheckBox.setEnabled(true);
		}
		else {
			this.tsaPwdField.setText(""); //$NON-NLS-1$
			this.tsaPwdField.setEnabled(false);
			this.tsaExtensionValueBase64Field.setText(""); //$NON-NLS-1$
			this.tsaExtensionValueBase64Field.setEnabled(false);
			this.extensionCriticalCheckBox.setEnabled(false);
		}
	}

	@SuppressWarnings("unused")
	void checkOid() throws GSSException {
		if (getTsaExtensionOid() != null && !getTsaExtensionOid().trim().isEmpty()) {
			String oid = getTsaExtensionOid().trim();
			if (getTsaExtensionOid().startsWith("urn:oid:")) { //$NON-NLS-1$
				oid = getTsaExtensionOid().replace("urn:oid:", ""); //$NON-NLS-1$ //$NON-NLS-2$
			}
			new Oid(oid);
		}
	}

	@SuppressWarnings("unused")
	boolean isValidUrl() {
		if (getTsaUrlText() != null && !getTsaUrlText().trim().isEmpty()) {
			try {
				new URL(getTsaUrlText());
				return true;
			}
			catch (final MalformedURLException e ){
				Logger.getLogger("es.gob.afirma").info("URL erronea: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		return false;
	}

	private TimeStampPadesDialog(final Frame parentFrame) {
		super(parentFrame);
		setTitle(SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.0")); //$NON-NLS-1$
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

		this.timeStampCheckBox.setMnemonic('s');
		this.timeStampCheckBox.addItemListener(
			new ItemListener() {
				@Override
				public void itemStateChanged(final ItemEvent e) {
					if (e.getStateChange() == ItemEvent.SELECTED) {
						enableFields(true);
					}
					else {
						enableFields(false);
					}
				}
			}
		);

		final Container c = getContentPane();
		final GridBagLayout gbl = new GridBagLayout();
		c.setLayout(gbl);
		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;
        gbc.insets = new Insets(10, 10, 5, 10);
		setIconImage(
			AutoFirmaUtil.getDefaultDialogsIcon()
		);
		getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.0") //$NON-NLS-1$
		);


		final JLabel tsTypesLabel = new JLabel(
			SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.3") //$NON-NLS-1$
		);
		tsTypesLabel.setLabelFor(this.tsType);

		final JLabel tsaUrlLabel = new JLabel(
			SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.4") //$NON-NLS-1$
		);
		tsaUrlLabel.setLabelFor(this.tsaUrlTextField);

		final JLabel tsaPolicyLabel = new JLabel(
			SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.5") //$NON-NLS-1$
		);
		tsaPolicyLabel.setLabelFor(this.tsaPolicyTextField);

		final JLabel tsaHashAlgorithmsLabel = new JLabel(
			SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.6") //$NON-NLS-1$
		);
		tsaHashAlgorithmsLabel.setLabelFor(this.tsaHashAlgorithms);

		this.certRequiredCheckBox.setMnemonic('r');

		final JLabel tsaUsrLabel = new JLabel(
			SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.8") //$NON-NLS-1$
		);
		tsaUsrLabel.setLabelFor(this.tsaUsrTextField);
		this.tsaUsrTextField.getDocument().addDocumentListener(
			new DocumentListener() {
				@Override
				public void insertUpdate(final DocumentEvent e) {
					enableFields(true);
				}

				@Override
				public void removeUpdate(final DocumentEvent e) {
					enableFields(true);
				}

				@Override
				public void changedUpdate(final DocumentEvent e) {
					enableFields(true);
				}
			}
		);

		final JLabel tsaPwdLabel = new JLabel(
			SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.9") //$NON-NLS-1$
		);
		tsaPwdLabel.setLabelFor(this.tsaPwdField);

		final JLabel tsaExtensionOidLabel = new JLabel(
			SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.10") //$NON-NLS-1$
		);
		tsaPwdLabel.setLabelFor(this.tsaExtensionOidField);

		final JLabel tsaExtensionValueBase64Label = new JLabel(
			SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.11") //$NON-NLS-1$
		);
		tsaPwdLabel.setLabelFor(this.tsaExtensionValueBase64Field);

		this.extensionCriticalCheckBox.setMnemonic('x');

		this.acceptButton.setEnabled(true);
		this.acceptButton.setMnemonic('A');
		this.acceptButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					if (isValidUrl()) {
						try {
							checkOid();
							SaveConfiguration();
							TimeStampPadesDialog.this.setVisible(false);
							TimeStampPadesDialog.this.dispose();
						}
						catch(final GSSException eGss) {
							Logger.getLogger("es.gob.afirma").info("OID erroneo: " + eGss); //$NON-NLS-1$ //$NON-NLS-2$
							AOUIFactory.showMessageDialog(
									TimeStampPadesDialog.this,
									SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.15"),  //$NON-NLS-1$
									SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.16"), //$NON-NLS-1$
									JOptionPane.ERROR_MESSAGE
							);
						}
					}
					else {
						AOUIFactory.showMessageDialog(
								TimeStampPadesDialog.this,
								SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.13"),  //$NON-NLS-1$
								SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.14"), //$NON-NLS-1$
								JOptionPane.ERROR_MESSAGE
						);
					}
				}
			}
		);
		this.acceptButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.10") //$NON-NLS-1$
		);

		final JButton closeButton = new JButton(
			SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.17") //$NON-NLS-1$
		);
		closeButton.setMnemonic('C');
		closeButton.addActionListener(
			new ActionListener () {
				@Override
				public void actionPerformed(final ActionEvent e) {
					if (getTsaUrlText() == null || getTsaUrlText().isEmpty()) {
						PreferencesManager.remove(PreferencesManager.PREFERENCE_PADES_TIMESTAMP_TSA_URL);
					}
					TimeStampPadesDialog.this.setVisible(false);
					TimeStampPadesDialog.this.dispose();
				}
			}
		);
		closeButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanelTimeStamps.18") //$NON-NLS-1$
		);

		final JPanel buttonsPanel = new JPanel();
		buttonsPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			buttonsPanel.add(closeButton);
			buttonsPanel.add(this.acceptButton);
		}
		else {
			buttonsPanel.add(this.acceptButton);
			buttonsPanel.add(closeButton);
		}

		add(this.timeStampCheckBox, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(10, 40, 5, 10);
		add(tsTypesLabel, gbc);
		gbc.gridy++;
        gbc.insets = new Insets(0, 35, 5, 10);
		add(this.tsType, gbc);
		gbc.gridy++;
        gbc.insets = new Insets(5, 40, 5, 10);
		add(tsaHashAlgorithmsLabel, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(0, 35, 5, 10);
		add(this.tsaHashAlgorithms, gbc);
		gbc.gridy++;
        gbc.insets = new Insets(5, 40, 5, 10);
		add(tsaUrlLabel, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(0, 35, 5, 10);
		add(this.tsaUrlTextField, gbc);
		gbc.gridy++;
        gbc.insets = new Insets(5, 40, 5, 10);
		add(tsaPolicyLabel, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(0, 35, 5, 10);
		add(this.tsaPolicyTextField, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(5, 40, 5, 10);
		add(tsaUsrLabel, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(0, 35, 5, 10);
		add(this.tsaUsrTextField, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(5, 40, 5, 10);
		add(tsaPwdLabel, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(0, 35, 5, 10);
		add(this.tsaPwdField, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(5, 40, 5, 10);
		add(tsaExtensionOidLabel, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(0, 35, 5, 10);
		add(this.tsaExtensionOidField, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(5, 40, 5, 10);
		add(tsaExtensionValueBase64Label, gbc);
		gbc.gridy++;
		gbc.insets = new Insets(0, 35, 10, 10);
		add(this.tsaExtensionValueBase64Field, gbc);
		gbc.gridy++;
		add(this.certRequiredCheckBox, gbc);
		gbc.gridy++;
		add(this.extensionCriticalCheckBox, gbc);
		gbc.gridy++;
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		c.add(buttonsPanel, gbc);

	}
}
